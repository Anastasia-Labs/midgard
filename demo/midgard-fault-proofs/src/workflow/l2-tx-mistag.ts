import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  FraudProofComputationThreadStepDatum,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import type { L2TxMistagContracts } from "../l2-tx-mistag/contracts.js";
import { prepareL2TxMistagFromTransactions } from "../l2-tx-mistag/prepare-l2-tx-mistag.js";
import { L2TxMistagStep02Datum } from "../l2-tx-mistag/schemas.js";
import { submitL2TxMistagInit } from "../l2-tx-mistag/submit-l2-tx-mistag-init.js";
import { submitL2TxMistagStep01 } from "../l2-tx-mistag/submit-l2-tx-mistag-step-01.js";
import { submitL2TxMistagStep02 } from "../l2-tx-mistag/submit-l2-tx-mistag-step-02.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
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
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "./proof-chunk-prerequisite.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const L2_TX_MISTAG_ARTIFACT =
  "midgard-production-l2-tx-mistag-artifact-v1" as const;

export type L2TxMistagArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof L2_TX_MISTAG_ARTIFACT;
    headerHash: string;
    detectionId: string;
    transactionIndex: number;
    transactionCount: number;
    committedTransactionsRoot: string;
    transactionsPhasRoot: string;
    nativeTxId: string;
    nativeTxCompactCbor: string;
    l2TransactionSourceCbor: string;
    txMembershipProofCbor: string;
  }>;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
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

const natural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

const proofSteps = (
  proof: ReturnType<typeof parseSubmitStep01TxInclusion>["txMembershipProof"],
) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

const parseArtifact = (value: unknown): L2TxMistagArtifact => {
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "transactionIndex",
      "transactionCount",
      "committedTransactionsRoot",
      "transactionsPhasRoot",
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "txMembershipProofCbor",
    ],
    "l2-tx-mistag artifact",
  );
  if (
    parsed.schemaVersion !== L2_TX_MISTAG_ARTIFACT ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("l2-tx-mistag artifact identity changed");
  }
  return Object.freeze({
    schemaVersion: L2_TX_MISTAG_ARTIFACT,
    headerHash: canonicalHex(parsed.headerHash, HEX_28, "artifact header"),
    detectionId: parsed.detectionId,
    transactionIndex: natural(parsed.transactionIndex, "transaction index"),
    transactionCount: natural(parsed.transactionCount, "transaction count"),
    committedTransactionsRoot: canonicalHex(
      parsed.committedTransactionsRoot,
      HEX_32,
      "committed transactions root",
    ),
    transactionsPhasRoot: canonicalHex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "transactions PHAS root",
    ),
    nativeTxId: canonicalHex(parsed.nativeTxId, HEX_32, "native tx id"),
    nativeTxCompactCbor: canonicalHex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      "native compact CBOR",
    ),
    l2TransactionSourceCbor: canonicalHex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      "transaction source CBOR",
    ),
    txMembershipProofCbor: canonicalHex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      "transaction membership proof",
    ),
  });
};

export const admitL2TxMistagArtifact = async (
  value: unknown,
): Promise<
  Readonly<{
    artifact: L2TxMistagArtifact;
    inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  }>
> => {
  const artifact = parseArtifact(value);
  if (artifact.transactionCount === 0) {
    throw new Error("l2-tx-mistag artifact has no committed transactions");
  }
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompact(
        Buffer.from(artifact.nativeTxCompactCbor, "hex"),
      ),
    ),
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
    transactionsPhasRoot: artifact.transactionsPhasRoot,
    txMembershipProofCbor: artifact.txMembershipProofCbor,
  });
  if (inclusion.nativeTx.validity_code !== 1n) {
    throw new Error("l2-tx-mistag artifact is not a code-1 normal leaf");
  }
  const expectedDetection = `l2-tx-mistag:${artifact.transactionIndex.toString()}:${artifact.nativeTxId}:1`;
  if (artifact.detectionId !== expectedDetection) {
    throw new Error("l2-tx-mistag artifact changed its detection identity");
  }
  let openedRoot: Buffer | null;
  try {
    openedRoot = MpfProof.fromJSON(
      Buffer.from(artifact.nativeTxId, "hex"),
      Buffer.from(artifact.l2TransactionSourceCbor, "hex"),
      proofSteps(inclusion.txMembershipProof),
    ).verify(true);
  } catch {
    throw new Error("l2-tx-mistag membership proof cannot be replayed");
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error("l2-tx-mistag proof does not open its PHAS root");
  }
  const countedRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: artifact.transactionsPhasRoot,
      count: BigInt(artifact.transactionCount),
    }),
  );
  if (countedRoot !== artifact.committedTransactionsRoot) {
    throw new Error("l2-tx-mistag PHAS root does not open the counted root");
  }
  return Object.freeze({ artifact, inclusion });
};

export const prepareL2TxMistagArtifact = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "l2TxMistag" };
}): Promise<L2TxMistagArtifact> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "l2-tx-mistag classification differs from canonical evidence",
    );
  }
  const transactionIndex = Number(classification.selected.position);
  const selected = admitted.transactions[transactionIndex];
  if (selected === undefined) {
    throw new Error("l2-tx-mistag selected transaction is absent");
  }
  const expectedDetection = `l2-tx-mistag:${transactionIndex.toString()}:${selected.nodeTxId}:1`;
  if (
    classification.selected.violationId !== "l2-tx-mistag" ||
    classification.selected.detectionId !== expectedDetection
  ) {
    throw new Error("l2-tx-mistag classification changed selected leaf");
  }
  const prepared = await prepareL2TxMistagFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    txId: selected.nodeTxId,
  });
  const inclusion = prepared.tx.txInclusion;
  const artifact = normalizeJournalJson({
    schemaVersion: L2_TX_MISTAG_ARTIFACT,
    headerHash: admitted.headerHash,
    detectionId: expectedDetection,
    transactionIndex,
    transactionCount: prepared.txCount,
    committedTransactionsRoot: prepared.committedTransactionsRoot,
    transactionsPhasRoot: prepared.transactionsPhasRoot,
    nativeTxId: inclusion.nativeTxId,
    nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
    l2TransactionSourceCbor: inclusion.l2TransactionSourceCbor,
    txMembershipProofCbor: inclusion.txMembershipProofCbor,
  }) as L2TxMistagArtifact;
  await admitL2TxMistagArtifact(artifact);
  return Object.freeze(artifact);
};

export type L2TxMistagWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
}>;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"l2TxMistag">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: L2TxMistagContracts;
  category: FraudProofWorkflowDeploymentBinding<"l2TxMistag">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"l2TxMistag">["catalogue"];
  referenceScripts: L2TxMistagWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "l2-tx-mistag workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "l2TxMistag" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("l2-tx-mistag workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`l2-tx-mistag workflow action omitted ${field}`);
  }
  return value;
};

const captureRemoval = async ({
  config,
  input,
}: {
  readonly config: BoundConfig;
  readonly input: Readonly<Record<string, unknown>>;
}) => {
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
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: "l2TxMistag",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            )
          ) {
            throw new Error(
              "l2-tx-mistag removal changed its authenticated queue input",
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "l2-tx-mistag removal did not reference the retained proof token",
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

const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"l2TxMistag"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "l2TxMistag",
  prepare: async ({ evidence, classification }) =>
    await prepareL2TxMistagArtifact({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitL2TxMistagArtifact(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("l2-tx-mistag artifact changed workflow header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitL2TxMistagInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              category: config.category,
              catalogue: config.catalogue,
              signer: config.signer,
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitL2TxMistagStep01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.inclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitL2TxMistagStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval({ config, input });
    }
    throw new Error(
      `l2-tx-mistag workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundL2TxMistagWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: L2TxMistagWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundL2TxMistagWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"l2TxMistag">;
  l1: FraudProofFamilyL1ObservationPort<"l2TxMistag">;
  transactions: LinearFamilyTransactionPort<"l2TxMistag">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundL2TxMistagWorkflow = async (
  config: ManifestBoundL2TxMistagWorkflowConfig,
): Promise<ManifestBoundL2TxMistagWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "l2TxMistag",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      L2TxMistagStep02Datum,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.l2TxMistag;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error("l2-tx-mistag manifest omitted required contracts");
  }
  const references: L2TxMistagWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofL2TxMistag",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofL2TxMistagStep02",
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
      chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "chunkedVerifyWithdraw",
        utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
      }),
    }),
  });
  const contracts: L2TxMistagContracts = Object.freeze({
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
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const transactions = createTransactionPort({
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    headerHash: binding.definition.headerHash,
    contracts,
    category: binding.resolvedContracts.category,
    catalogue: binding.catalogue,
    referenceScripts: references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  const linear = createLinearFamilyWorkflowAdapter({
    category: "l2TxMistag",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const prerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "l2TxMistag",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const admitted = parseArtifact(artifact);
      return action.input.stage === "step_01"
        ? admitted.txMembershipProofCbor
        : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter: withProofChunkPrerequisite({
      category: "l2TxMistag",
      base: linear,
      prerequisite,
    }),
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const runOrResumeManifestBoundL2TxMistagWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundL2TxMistagWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["l2TxMistag"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
