import {
  DA_HASH_PREIMAGE_VIOLATION_ID_V1,
  DaHashPreimageStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  DA_HASH_PREIMAGE_EVIDENCE_V1_SCHEMA_VERSION,
  prepareDaHashPreimageFromCommittedLeavesV1,
  type PreparedDaHashPreimageOutput,
} from "../prepare-da-hash-preimage.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  parseSubmitDaHashPreimageTxInclusion,
  submitDaHashPreimageStep01,
} from "../submit-da-hash-preimage-step-01.js";
import { submitDaHashPreimageStep02 } from "../submit-da-hash-preimage-step-02.js";
import { submitInit } from "../submit-init.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStoreV1,
  type JournalJsonObjectV1,
  normalizeJournalJsonV1,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runDaHashPreimageWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyCapturedActionV1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_DA_HASH_PREIMAGE_ARTIFACT_V1 =
  "midgard-production-da-hash-preimage-artifact-v1" as const;

type DaHashPreimageArtifactEntryV1 = readonly [string, string];

export type ProductionDaHashPreimageArtifactV1 = JournalJsonObjectV1 & {
  readonly schemaVersion: typeof PRODUCTION_DA_HASH_PREIMAGE_ARTIFACT_V1;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: number;
  readonly committedTxId: string;
  readonly entries: readonly DaHashPreimageArtifactEntryV1[];
};

const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;

const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  expected: readonly string[],
  label: string,
): void => {
  const actual = Object.keys(value).sort();
  const canonical = [...expected].sort();
  if (
    actual.length !== canonical.length ||
    actual.some((key, index) => key !== canonical[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
};

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const artifactFields = [
  "schemaVersion",
  "headerHash",
  "committedTransactionsRoot",
  "l2TransactionCount",
  "committedTxId",
  "entries",
] as const;

const artifactInput = (
  value: unknown,
): {
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly committedTxId: string;
  readonly entries: readonly DaHashPreimageArtifactEntryV1[];
} => {
  const candidate = record(value, "da-hash-preimage workflow artifact");
  exactKeys(candidate, artifactFields, "da-hash-preimage workflow artifact");
  if (candidate.schemaVersion !== PRODUCTION_DA_HASH_PREIMAGE_ARTIFACT_V1) {
    throw new Error("da-hash-preimage workflow artifact version changed");
  }
  if (
    !Number.isSafeInteger(candidate.l2TransactionCount) ||
    (candidate.l2TransactionCount as number) < 0
  ) {
    throw new Error(
      "da-hash-preimage workflow artifact count is not a non-negative safe integer",
    );
  }
  if (!Array.isArray(candidate.entries) || candidate.entries.length === 0) {
    throw new Error(
      "da-hash-preimage workflow artifact has no committed leaves",
    );
  }
  const entries = candidate.entries.map((entry, index) => {
    if (!Array.isArray(entry) || entry.length !== 2) {
      throw new Error(
        `da-hash-preimage workflow leaf ${index.toString()} is malformed`,
      );
    }
    return Object.freeze([
      canonicalHex(entry[0], HEX_32, `committed leaf ${index.toString()} key`),
      canonicalHex(
        entry[1],
        /^(?:[0-9a-f]{2})+$/u,
        `committed leaf ${index.toString()} value`,
      ),
    ] as const);
  });
  return {
    headerHash: canonicalHex(
      candidate.headerHash,
      HEX_28,
      "da-hash-preimage artifact header",
    ),
    committedTransactionsRoot: canonicalHex(
      candidate.committedTransactionsRoot,
      HEX_32,
      "da-hash-preimage committed transactions root",
    ),
    l2TransactionCount: BigInt(candidate.l2TransactionCount as number),
    committedTxId: canonicalHex(
      candidate.committedTxId,
      HEX_32,
      "da-hash-preimage violating committed key",
    ),
    entries: Object.freeze(entries),
  };
};

/**
 * Reopens the counted transactions root and re-runs Q44 from the journaled raw
 * leaves. No verdict, proof, or decoded transaction claim is trusted from the
 * durable artifact.
 */
export const admitProductionDaHashPreimageArtifactV1 = async (
  value: unknown,
): Promise<PreparedDaHashPreimageOutput> => {
  const admitted = artifactInput(value);
  return await prepareDaHashPreimageFromCommittedLeavesV1({
    headerHash: admitted.headerHash,
    committedTransactionsRoot: admitted.committedTransactionsRoot,
    l2TransactionCount: admitted.l2TransactionCount,
    entries: admitted.entries,
    committedTxId: admitted.committedTxId,
  });
};

const sameJson = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left) === JSON.stringify(right);

/** Creates the minimal raw-leaf artifact from the independently routed plan. */
export const productionDaHashPreimageArtifactV1 = async (
  plan: PreparedDaHashPreimageOutput,
): Promise<ProductionDaHashPreimageArtifactV1> => {
  if (
    plan.schemaVersion !== DA_HASH_PREIMAGE_EVIDENCE_V1_SCHEMA_VERSION ||
    plan.violationId !== DA_HASH_PREIMAGE_VIOLATION_ID_V1 ||
    plan.files !== undefined
  ) {
    throw new Error(
      "da-hash-preimage production plan is not an in-memory authenticated raw-leaf plan",
    );
  }
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_DA_HASH_PREIMAGE_ARTIFACT_V1,
    headerHash: plan.headerHash,
    committedTransactionsRoot: plan.committedTransactionsRoot,
    l2TransactionCount: plan.l2TransactionCount,
    committedTxId: plan.violation.committedTxId,
    entries: plan.leaves.map(
      (leaf) => [leaf.committedTxId, leaf.committedLeafValueCbor] as const,
    ),
  }) as ProductionDaHashPreimageArtifactV1;
  const rederived = await admitProductionDaHashPreimageArtifactV1(artifact);
  if (
    rederived.violationId !== plan.violationId ||
    rederived.headerHash !== plan.headerHash ||
    rederived.committedTransactionsRoot !== plan.committedTransactionsRoot ||
    rederived.l2TransactionCount !== plan.l2TransactionCount ||
    !sameJson(rederived.txInclusion, plan.txInclusion) ||
    !sameJson(rederived.step02State, plan.step02State)
  ) {
    throw new Error(
      "da-hash-preimage production plan differs from raw-leaf re-derivation",
    );
  }
  return Object.freeze(artifact);
};

export type DaHashPreimageWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

type BoundDaHashPreimageTransactionsConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"daHashPreimage">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: DaHashPreimageWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

type DaHashPreimageBuilderSetV1 = Readonly<{
  init: typeof submitInit;
  step01: typeof submitDaHashPreimageStep01;
  step02: typeof submitDaHashPreimageStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

const productionBuilders: DaHashPreimageBuilderSetV1 = Object.freeze({
  init: submitInit,
  step01: submitDaHashPreimageStep01,
  step02: submitDaHashPreimageStep02,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "da-hash-preimage workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "daHashPreimage" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("da-hash-preimage workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string") {
    throw new Error(`da-hash-preimage workflow action omitted ${name}`);
  }
  return value;
};

const createBoundDaHashPreimageTransactionPortV1 = ({
  config,
  builders,
}: {
  readonly config: BoundDaHashPreimageTransactionsConfigV1;
  readonly builders: DaHashPreimageBuilderSetV1;
}): ProductionLinearFamilyTransactionPortV1<"daHashPreimage"> => {
  const capture = async ({
    action,
    artifact,
  }: {
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }): Promise<ProductionLinearFamilyCapturedActionV1> => {
    const plan = await admitProductionDaHashPreimageArtifactV1(artifact);
    if (plan.headerHash !== config.headerHash) {
      throw new Error(
        "da-hash-preimage artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await builders.init({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "daHashPreimage",
            fraudulentBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            fraudulentHeaderHash: config.headerHash,
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_01") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await builders.step01({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            txInclusion: parseSubmitDaHashPreimageTxInclusion(plan.txInclusion),
            referenceScriptUtxo: config.referenceScripts.steps[0],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_02") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await builders.step02({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            referenceScriptUtxo: config.referenceScripts.steps[1],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "remove") {
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
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (boundary) => {
          await builders.remove({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "daHashPreimage",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
            preSubmitBoundary: async (transaction) => {
              if (
                !workflowTransactionInputOutRefsV1(transaction.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "da-hash-preimage removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefsV1(
                  transaction.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "da-hash-preimage removal does not reference the authenticated retained proof token",
                );
              }
              await boundary(transaction);
            },
          });
        },
      );
      return Object.freeze({
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
    }
    throw new Error(
      `da-hash-preimage workflow action has unsupported stage ${String(input.stage)}`,
    );
  };
  return Object.freeze({
    portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
    category: "daHashPreimage",
    prepare: async () => {
      throw new Error(
        "da-hash-preimage requires the authenticated raw-source-leaf evidence route",
      );
    },
    capture,
  });
};

export type ManifestBoundDaHashPreimageWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: DaHashPreimageWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundDaHashPreimageWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"daHashPreimage">;
  l1: FraudProofFamilyL1ObservationPortV1<"daHashPreimage">;
  transactions: ProductionLinearFamilyTransactionPortV1<"daHashPreimage">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

/**
 * Q44 manifest-bound construction. It is deliberately not an admitted runner
 * yet: the generic canonical classifier cannot route a raw source-leaf defect.
 * Readiness remains missing until the dedicated evidence route enters the
 * shared durable workflow loop without manufacturing canonical evidence.
 */
export const createManifestBoundDaHashPreimageWorkflowV1 = async (
  config: ManifestBoundDaHashPreimageWorkflowConfigV1,
): Promise<ManifestBoundDaHashPreimageWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "daHashPreimage",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      DaHashPreimageStep02Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const references: DaHashPreimageWorkflowReferenceScriptsV1 = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofDaHashPreimage",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofDaHashPreimageStep02",
        utxo: config.referenceScripts.steps[1],
      }),
    ] as const),
    witnesses: Object.freeze({
      computationThreadMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "computationThreadMint",
        utxo: config.referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMint",
        utxo: config.referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
    }),
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const transactions = createBoundDaHashPreimageTransactionPortV1({
    config: {
      lucid: config.lucid,
      blueprint: binding.blueprint,
      deploymentInfo: binding.deploymentInfo,
      network: binding.network,
      signer: config.signer,
      headerHash: binding.definition.headerHash,
      referenceScripts: references,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: BigInt(
        binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
    },
    builders: productionBuilders,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter: createProductionLinearFamilyWorkflowAdapterV1({
      category: "daHashPreimage",
      l1,
      transactions,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    }),
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  });
};

/** Exact public-DA Q44 route into the shared durable lifecycle. */
export const runOrResumeManifestBoundDaHashPreimageWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundDaHashPreimageWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runDaHashPreimageWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["daHashPreimage"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

/** Narrow builder-injection seam for focused tests; never admit this as ready. */
export const unsafeCreateDaHashPreimageTransactionPortForTest = (input: {
  readonly config: BoundDaHashPreimageTransactionsConfigV1;
  readonly builders: DaHashPreimageBuilderSetV1;
}): ProductionLinearFamilyTransactionPortV1<"daHashPreimage"> =>
  createBoundDaHashPreimageTransactionPortV1(input);
