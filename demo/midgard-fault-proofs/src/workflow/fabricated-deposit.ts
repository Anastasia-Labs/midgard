import {
  FabricatedDepositStep02Datum,
  FabricatedDepositStep03Datum,
  FabricatedDepositStep04Datum,
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
  type FabricatedDepositContracts,
  parseSubmitFabricatedDepositInclusion,
  submitFabricatedDepositStep01,
} from "../submit-fabricated-deposit-step-01.js";
import { submitFabricatedDepositStep02 } from "../submit-fabricated-deposit-step-02.js";
import { submitFabricatedDepositStep03 } from "../submit-fabricated-deposit-step-03.js";
import { submitFabricatedDepositStep04 } from "../submit-fabricated-deposit-step-04.js";
import { submitInit } from "../submit-init.js";
import { createFabricatedDepositCompleteCanonicalReplay } from "./complete-replay.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  createFabricatedDepositEvidenceAuthority,
  type FabricatedDepositEvidenceAuthority,
  requireFabricatedDepositArtifact,
} from "./fabricated-deposit-evidence.js";
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
  "fabricatedDeposit",
  (typeof WITNESS_ROLES)[number],
  false
>;

export type FabricatedDepositWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "fabricatedDeposit",
    (typeof WITNESS_ROLES)[number],
    false
  >;

type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"fabricatedDeposit">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: FabricatedDepositContracts;
  references: FabricatedDepositWorkflowReferenceScripts;
  evidence: FabricatedDepositEvidenceAuthority;
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
  const input = record(action.input, "fabricated-deposit workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "fabricatedDeposit" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("fabricated-deposit workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`fabricated-deposit workflow action omitted ${field}`);
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
        fraudCategory: "fabricatedDeposit",
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
              "fabricated-deposit removal changed authenticated inputs",
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
): LinearFamilyTransactionPort<"fabricatedDeposit"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "fabricatedDeposit",
  prepare: async ({ evidence, classification }) => {
    if (
      classification.headerHash !== evidence.headerHash ||
      classification.selected.position < 0n ||
      classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
    ) {
      throw new Error(
        "fabricated-deposit classification changed the canonical evidence identity",
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
    const admitted = requireFabricatedDepositArtifact(
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
              fraudCategory: "fabricatedDeposit",
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
      const inclusion = parseSubmitFabricatedDepositInclusion(
        admitted.depositInclusion,
      );
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitFabricatedDepositStep01({
              lucid: config.lucid,
              contracts: config.contracts,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              depositInclusion: inclusion,
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
            await submitFabricatedDepositStep02({
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
            await submitFabricatedDepositStep03({
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
            await submitFabricatedDepositStep04({
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
      `fabricated-deposit workflow cannot execute ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundFabricatedDepositWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "fabricatedDeposit",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type ManifestBoundFabricatedDepositWorkflow =
  ManifestBoundLinearFamilyWorkflow<"fabricatedDeposit", false>;

const contracts = (context: AssemblyContext): FabricatedDepositContracts => {
  const resolved = context.binding.resolvedContracts;
  const chain = resolved.contracts.fabricatedDeposit;
  const stateQueuePolicyId = resolved.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error("fabricated-deposit manifest omitted required contracts");
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
  const chain = context.binding.resolvedContracts.contracts.fabricatedDeposit;
  if (chain === undefined)
    throw new Error("Missing applied history proof family");
  return createFabricatedDepositEvidenceAuthority({
    history: chain.history,
    lucid: context.lucid,
    network: context.binding.network,
    hubOraclePolicyId: context.binding.resolvedContracts.hubOraclePolicyId,
    minimumConfirmationDepth: 1,
  });
};

export const FABRICATED_DEPOSIT_FAMILY_DEFINITION = defineLinearFamily({
  category: "fabricatedDeposit",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    FabricatedDepositStep02Datum,
    FabricatedDepositStep03Datum,
    FabricatedDepositStep04Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: (context) =>
    createFabricatedDepositCompleteCanonicalReplay({
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

export const createManifestBoundFabricatedDepositWorkflow = (
  config: ManifestBoundFabricatedDepositWorkflowConfig,
): Promise<ManifestBoundFabricatedDepositWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    FABRICATED_DEPOSIT_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundFabricatedDepositWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
