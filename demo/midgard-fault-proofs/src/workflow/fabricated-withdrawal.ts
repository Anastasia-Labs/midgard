import {
  FabricatedWithdrawalStep02Datum,
  FabricatedWithdrawalStep03Datum,
  FabricatedWithdrawalStep04Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import {
  type FabricatedWithdrawalContracts,
  parseSubmitFabricatedWithdrawalInclusion,
  submitFabricatedWithdrawalStep01,
} from "../submit-fabricated-withdrawal-step-01.js";
import { submitFabricatedWithdrawalStep02 } from "../submit-fabricated-withdrawal-step-02.js";
import { submitFabricatedWithdrawalStep03 } from "../submit-fabricated-withdrawal-step-03.js";
import { submitFabricatedWithdrawalStep04 } from "../submit-fabricated-withdrawal-step-04.js";
import { submitInit } from "../submit-init.js";
import { createFabricatedWithdrawalCompleteCanonicalReplay } from "./complete-replay.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  createFabricatedWithdrawalEvidenceAuthority,
  type FabricatedWithdrawalEvidenceAuthority,
  requireFabricatedWithdrawalArtifact,
} from "./fabricated-withdrawal-evidence.js";
import {
  defineLinearFamily,
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

const WITNESS_ROLES = [
  "stateQueueSpend",
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;

type AssemblyContext = LinearFamilyAssemblyContext<
  "fabricatedWithdrawal",
  (typeof WITNESS_ROLES)[number],
  false
>;

export type FabricatedWithdrawalWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "fabricatedWithdrawal",
    (typeof WITNESS_ROLES)[number],
    false
  >;

type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"fabricatedWithdrawal">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: FabricatedWithdrawalContracts;
  references: FabricatedWithdrawalWorkflowReferenceScripts;
  evidence: FabricatedWithdrawalEvidenceAuthority;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "fabricated-withdrawal workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "fabricatedWithdrawal" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("fabricated-withdrawal workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`fabricated-withdrawal workflow action omitted ${field}`);
  }
  return value;
};

const captureRemoval = async (
  config: BoundConfig,
  input: Readonly<Record<string, unknown>>,
) => {
  let mutationLease: StateQueueMutationLease | undefined;
  const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => {
      const acquired =
        await config.stateQueueMutationLeaseCoordinator.acquire();
      mutationLease = acquired;
      return acquired;
    },
  };
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  const transaction = await captureLocallyEvaluatedTransaction(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        fraudCategory: "fabricatedWithdrawal",
        fraudulentHeaderHash: config.binding.definition.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "fabricated-withdrawal removal changed authenticated inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease === undefined ? {} : { mutationLease }),
  });
};

const transactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"fabricatedWithdrawal"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "fabricatedWithdrawal",
  prepare: async ({ evidence, classification }) => {
    if (
      classification.headerHash !== evidence.headerHash ||
      classification.selected.position < 0n ||
      classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
    ) {
      throw new Error(
        "fabricated-withdrawal classification changed the canonical evidence identity",
      );
    }
    return await config.evidence.prepare(
      evidence,
      config.signer.paymentKeyHash,
      Number(classification.selected.position),
    );
  },
  capture: async ({ action, artifact }) => {
    const input = actionInput(action);
    const retained = ["step_03", "step_04", "remove"].includes(
      stringField(input, "stage"),
    );
    const admitted = requireFabricatedWithdrawalArtifact(
      retained
        ? config.evidence.readmitRetained(artifact)
        : await config.evidence.readmit(artifact),
      config.signer.paymentKeyHash,
      config.binding.definition.headerHash,
    );
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "fabricatedWithdrawal",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const inclusion = parseSubmitFabricatedWithdrawalInclusion(
        admitted.withdrawalInclusion,
      );
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitFabricatedWithdrawalStep01({
              lucid: config.lucid,
              contracts: config.contracts,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              withdrawalInclusion: inclusion,
              referenceScriptUtxo: config.references.steps[0],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const evidence =
        admitted.l1Evidence.kind === "absent_identity"
          ? ({ kind: "absent_identity" } as const)
          : ({
              kind: "present_event",
              eventOutRef: admitted.l1Evidence.historyOutRef,
            } as const);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitFabricatedWithdrawalStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              evidence,
              expectedOpeningCbor: admitted.authenticContent.openingCbor,
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitFabricatedWithdrawalStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              ...(admitted.authenticContent.openingCbor === null
                ? {}
                : {
                    openingCbor: admitted.authenticContent.openingCbor,
                  }),
              referenceScriptUtxo: config.references.steps[2],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      const mutationLease =
        await config.stateQueueMutationLeaseCoordinator.acquire();
      try {
        await mutationLease.renew();
        const transaction = await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitFabricatedWithdrawalStep04({
              lucid: config.lucid,
              contracts: config.contracts,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceScriptUtxo: config.references.steps[3],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary: async (built) => {
                await mutationLease.renew();
                await preSubmitBoundary(built);
              },
              awaitConfirmation: false,
            });
          },
        );
        return Object.freeze({ transaction, mutationLease });
      } catch (error) {
        await mutationLease.fail(
          `Terminal proof capture failed: ${String(error)}`,
        );
        throw error;
      }
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `fabricated-withdrawal workflow cannot execute ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundFabricatedWithdrawalWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "fabricatedWithdrawal",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type ManifestBoundFabricatedWithdrawalWorkflow =
  ManifestBoundLinearFamilyWorkflow<"fabricatedWithdrawal", false>;

const contracts = (context: AssemblyContext): FabricatedWithdrawalContracts => {
  const resolved = context.binding.resolvedContracts;
  const chain = resolved.contracts.fabricatedWithdrawal;
  const stateQueuePolicyId = resolved.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error(
      "fabricated-withdrawal manifest omitted required contracts",
    );
  }
  return Object.freeze({
    steps: chain.steps,
    history: chain.history,
    computationThread: resolved.contracts.computationThread,
    fraudProof: {
      policyId: resolved.contracts.fraudProof.policyId,
      mintingScript: resolved.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        resolved.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId,
    categoryId: resolved.category.categoryId,
  });
};

/**
 * The public L1 event authority both the transaction port and the replayer
 * read. It is a stateless view over the bound hub oracle; each caller may
 * hold its own instance.
 */
const evidenceAuthority = (context: AssemblyContext) => {
  const chain =
    context.binding.resolvedContracts.contracts.fabricatedWithdrawal;
  if (chain === undefined)
    throw new Error("Missing applied history proof family");
  return createFabricatedWithdrawalEvidenceAuthority({
    history: chain.history,
    lucid: context.lucid,
    network: context.binding.network,
    hubOraclePolicyId: context.binding.resolvedContracts.hubOraclePolicyId,
    minimumConfirmationDepth: 1,
  });
};

export const FABRICATED_WITHDRAWAL_FAMILY_DEFINITION = defineLinearFamily({
  category: "fabricatedWithdrawal",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    FabricatedWithdrawalStep02Datum,
    FabricatedWithdrawalStep03Datum,
    FabricatedWithdrawalStep04Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: (context) =>
    createFabricatedWithdrawalCompleteCanonicalReplay({
      authority: evidenceAuthority(context),
      owner: context.signer.paymentKeyHash,
    }),
  adapter: {
    kind: "linear",
    transactionPort: (context) =>
      transactionPort({
        binding: context.binding,
        lucid: context.lucid,
        signer: context.signer,
        contracts: contracts(context),
        references: context.references,
        evidence: evidenceAuthority(context),
        stateQueueMutationLeaseCoordinator:
          context.stateQueueMutationLeaseCoordinator,
      }),
  },
});

export const createManifestBoundFabricatedWithdrawalWorkflow = (
  config: ManifestBoundFabricatedWithdrawalWorkflowConfig,
): Promise<ManifestBoundFabricatedWithdrawalWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    FABRICATED_WITHDRAWAL_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundFabricatedWithdrawalWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
