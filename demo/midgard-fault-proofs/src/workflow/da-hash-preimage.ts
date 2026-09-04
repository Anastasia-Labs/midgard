import {
  DA_HASH_PREIMAGE_VIOLATION_ID,
  DaHashPreimageStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  DA_HASH_PREIMAGE_EVIDENCE_SCHEMA_VERSION,
  prepareDaHashPreimageFromCommittedLeaves,
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
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyCapturedAction,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runDaHashPreimageWorkflowFromRetainedDa,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const DA_HASH_PREIMAGE_ARTIFACT =
  "midgard-production-da-hash-preimage-artifact-v1" as const;

type DaHashPreimageArtifactEntry = readonly [string, string];

export type DaHashPreimageArtifact = JournalJsonObject & {
  readonly schemaVersion: typeof DA_HASH_PREIMAGE_ARTIFACT;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: number;
  readonly committedTxId: string;
  readonly entries: readonly DaHashPreimageArtifactEntry[];
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
  readonly entries: readonly DaHashPreimageArtifactEntry[];
} => {
  const candidate = record(value, "da-hash-preimage workflow artifact");
  exactKeys(candidate, artifactFields, "da-hash-preimage workflow artifact");
  if (candidate.schemaVersion !== DA_HASH_PREIMAGE_ARTIFACT) {
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
export const admitDaHashPreimageArtifact = async (
  value: unknown,
): Promise<PreparedDaHashPreimageOutput> => {
  const admitted = artifactInput(value);
  return await prepareDaHashPreimageFromCommittedLeaves({
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
export const daHashPreimageArtifact = async (
  plan: PreparedDaHashPreimageOutput,
): Promise<DaHashPreimageArtifact> => {
  if (
    plan.schemaVersion !== DA_HASH_PREIMAGE_EVIDENCE_SCHEMA_VERSION ||
    plan.violationId !== DA_HASH_PREIMAGE_VIOLATION_ID ||
    plan.files !== undefined
  ) {
    throw new Error(
      "da-hash-preimage production plan is not an in-memory authenticated raw-leaf plan",
    );
  }
  const artifact = normalizeJournalJson({
    schemaVersion: DA_HASH_PREIMAGE_ARTIFACT,
    headerHash: plan.headerHash,
    committedTransactionsRoot: plan.committedTransactionsRoot,
    l2TransactionCount: plan.l2TransactionCount,
    committedTxId: plan.violation.committedTxId,
    entries: plan.leaves.map(
      (leaf) => [leaf.committedTxId, leaf.committedLeafValueCbor] as const,
    ),
  }) as DaHashPreimageArtifact;
  const rederived = await admitDaHashPreimageArtifact(artifact);
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

export type DaHashPreimageWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

type BoundDaHashPreimageTransactionsConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"daHashPreimage">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: DaHashPreimageWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

type DaHashPreimageBuilderSet = Readonly<{
  init: typeof submitInit;
  step01: typeof submitDaHashPreimageStep01;
  step02: typeof submitDaHashPreimageStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

const productionBuilders: DaHashPreimageBuilderSet = Object.freeze({
  init: submitInit,
  step01: submitDaHashPreimageStep01,
  step02: submitDaHashPreimageStep02,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowAction,
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

const createBoundDaHashPreimageTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundDaHashPreimageTransactionsConfig;
  readonly builders: DaHashPreimageBuilderSet;
}): LinearFamilyTransactionPort<"daHashPreimage"> => {
  const capture = async ({
    action,
    artifact,
  }: {
    readonly action: FraudProofWorkflowAction;
    readonly artifact: JournalJsonObject;
  }): Promise<LinearFamilyCapturedAction> => {
    const plan = await admitDaHashPreimageArtifact(artifact);
    if (plan.headerHash !== config.headerHash) {
      throw new Error(
        "da-hash-preimage artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransaction(
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
      const transaction = await captureLocallyEvaluatedTransaction(
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
      const transaction = await captureLocallyEvaluatedTransaction(
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
      const transaction = await captureLocallyEvaluatedTransaction(
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
                !workflowTransactionInputOutRefs(transaction.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "da-hash-preimage removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefs(
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
    portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
    category: "daHashPreimage",
    prepare: async () => {
      throw new Error(
        "da-hash-preimage requires the authenticated raw-source-leaf evidence route",
      );
    },
    capture,
  });
};

export type ManifestBoundDaHashPreimageWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: DaHashPreimageWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundDaHashPreimageWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"daHashPreimage">;
  l1: FraudProofFamilyL1ObservationPort<"daHashPreimage">;
  transactions: LinearFamilyTransactionPort<"daHashPreimage">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

/**
 * Q44 manifest-bound construction. It is deliberately not an admitted runner
 * yet: the generic canonical classifier cannot route a raw source-leaf defect.
 * Readiness remains missing until the dedicated evidence route enters the
 * shared durable workflow loop without manufacturing canonical evidence.
 */
export const createManifestBoundDaHashPreimageWorkflow = async (
  config: ManifestBoundDaHashPreimageWorkflowConfig,
): Promise<ManifestBoundDaHashPreimageWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const references: DaHashPreimageWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofDaHashPreimage",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofDaHashPreimageStep02",
        utxo: config.referenceScripts.steps[1],
      }),
    ] as const),
    witnesses: Object.freeze({
      computationThreadMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "computationThreadMint",
        utxo: config.referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMint",
        utxo: config.referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
    }),
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const transactions = createBoundDaHashPreimageTransactionPort({
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
    adapter: createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1,
      transactions,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    }),
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

/** Exact public-DA Q44 route into the shared durable lifecycle. */
export const runOrResumeManifestBoundDaHashPreimageWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundDaHashPreimageWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runDaHashPreimageWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    registry: createFraudProofWorkflowRegistry({
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
  readonly config: BoundDaHashPreimageTransactionsConfig;
  readonly builders: DaHashPreimageBuilderSet;
}): LinearFamilyTransactionPort<"daHashPreimage"> =>
  createBoundDaHashPreimageTransactionPort(input);
