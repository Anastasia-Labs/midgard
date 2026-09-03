import {
  FabricatedDepositStep02Datum,
  FabricatedDepositStep03Datum,
  FabricatedDepositStep04Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

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
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { createFabricatedDepositCompleteCanonicalReplay } from "./complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import {
  createFabricatedDepositEvidenceAuthority,
  type FabricatedDepositEvidenceAuthority,
  requireFabricatedDepositArtifact,
} from "./fabricated-deposit-evidence.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export type FabricatedDepositWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
  };
}>;

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
    const admitted = requireFabricatedDepositArtifact(
      await config.evidence.readmit(artifact),
      config.signer.paymentKeyHash,
      config.binding.definition.headerHash,
    );
    const input = actionInput(action);
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
              eventOutRef: admitted.l1Evidence.eventOutRef,
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
              ...(admitted.authenticContent.eventDatumCbor === null
                ? {}
                : {
                    eventDatumCbor: admitted.authenticContent.eventDatumCbor,
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
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitFabricatedDepositStep04({
              lucid: config.lucid,
              contracts: config.contracts,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceScriptUtxo: config.references.steps[3],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `fabricated-deposit workflow cannot execute ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundFabricatedDepositWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: FabricatedDepositWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundFabricatedDepositWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"fabricatedDeposit">;
  l1: FraudProofFamilyL1ObservationPort<"fabricatedDeposit">;
  transactions: LinearFamilyTransactionPort<"fabricatedDeposit">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  replayer: ReturnType<typeof createFabricatedDepositCompleteCanonicalReplay>;
}>;

export const createManifestBoundFabricatedDepositWorkflow = async (
  config: ManifestBoundFabricatedDepositWorkflowConfig,
): Promise<ManifestBoundFabricatedDepositWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "fabricatedDeposit",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      FabricatedDepositStep02Datum,
      FabricatedDepositStep03Datum,
      FabricatedDepositStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.fabricatedDeposit;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error("fabricated-deposit manifest omitted required contracts");
  }
  const stepNames = [
    "fraudProofFabricatedDeposit",
    "fraudProofFabricatedDepositStep02",
    "fraudProofFabricatedDepositStep03",
    "fraudProofFabricatedDepositStep04",
  ] as const;
  const steps = stepNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as FabricatedDepositWorkflowReferenceScripts["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScripts>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name]!,
    });
  const references: FabricatedDepositWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze(steps),
    witnesses: Object.freeze({
      computationThreadMint: witness(
        "computationThreadMint",
        "computationThreadMint",
      ),
      fraudProofMint: witness("fraudProofMint", "fraudProofMint"),
    }),
  });
  const contracts: FabricatedDepositContracts = Object.freeze({
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    categoryId: binding.resolvedContracts.category.categoryId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined || l1.publications === undefined) {
    throw new Error(
      "fabricated-deposit requires authenticated raw L1 and publication authorities",
    );
  }
  const evidence = createFabricatedDepositEvidenceAuthority({
    lucid: config.lucid,
    network: binding.network,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    minimumConfirmationDepth: binding.releaseFinality.policy.confirmationDepth,
  });
  const transactions = transactionPort({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    evidence,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const adapter = createLinearFamilyWorkflowAdapter({
    category: "fabricatedDeposit",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    replayer: createFabricatedDepositCompleteCanonicalReplay({
      authority: evidence,
      owner: config.signer.paymentKeyHash,
    }),
  });
};

export const runOrResumeManifestBoundFabricatedDepositWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundFabricatedDepositWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await workflow.l1.observeHeader({
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: workflow.replayer,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["fabricatedDeposit"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
