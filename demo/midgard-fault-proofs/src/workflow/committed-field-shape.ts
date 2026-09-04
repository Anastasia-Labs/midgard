import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import {
  COMMITTED_FIELD_SHAPE_VIOLATION_ID,
  CommittedFieldShapeStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { CommittedFieldShapeContracts } from "../committed-field-shape/contracts.js";
import {
  prepareCommittedFieldShapeFromCanonicalTx,
  type PreparedCommittedFieldShape,
} from "../committed-field-shape/prepare-committed-field-shape.js";
import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import { submitCommittedFieldShapeStep01 } from "../committed-field-shape/submit-committed-field-shape-step-01.js";
import { submitCommittedFieldShapeStep02 } from "../committed-field-shape/submit-committed-field-shape-step-02.js";
import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
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
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const COMMITTED_FIELD_SHAPE_ARTIFACT =
  "midgard-production-committed-field-shape-artifact-v1" as const;

type ArtifactTransaction = Readonly<{
  nodeTxId: string;
  txCbor: string;
  l2TransactionSourceCbor: string;
}>;

export type CommittedFieldShapeArtifact = JournalJsonObject & {
  readonly schemaVersion: typeof COMMITTED_FIELD_SHAPE_ARTIFACT;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: number;
  readonly transactionsPhasRoot: string;
  readonly selectedTransactionIndex: number;
  readonly selectedFieldIndex: number;
  readonly txMembershipProofCbor: string;
  readonly transactions: readonly ArtifactTransaction[];
};

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
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

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
    throw new Error(`${label} is not a non-negative safe integer`);
  }
  return value as number;
};

const artifactFields = [
  "schemaVersion",
  "headerHash",
  "committedTransactionsRoot",
  "l2TransactionCount",
  "transactionsPhasRoot",
  "selectedTransactionIndex",
  "selectedFieldIndex",
  "txMembershipProofCbor",
  "transactions",
] as const;

const parseArtifact = (value: unknown): CommittedFieldShapeArtifact => {
  const artifact = record(value, "committed-field-shape artifact");
  exactKeys(artifact, artifactFields, "committed-field-shape artifact");
  if (artifact.schemaVersion !== COMMITTED_FIELD_SHAPE_ARTIFACT) {
    throw new Error("committed-field-shape artifact version changed");
  }
  if (
    !Array.isArray(artifact.transactions) ||
    artifact.transactions.length === 0
  ) {
    throw new Error("committed-field-shape artifact has no transactions");
  }
  const transactions = Object.freeze(
    artifact.transactions.map((value, index) => {
      const transaction = record(
        value,
        `committed-field-shape transaction ${index.toString()}`,
      );
      exactKeys(
        transaction,
        ["nodeTxId", "txCbor", "l2TransactionSourceCbor"],
        `committed-field-shape transaction ${index.toString()}`,
      );
      return Object.freeze({
        nodeTxId: canonicalHex(
          transaction.nodeTxId,
          HEX_32,
          `transaction ${index.toString()} id`,
        ),
        txCbor: canonicalHex(
          transaction.txCbor,
          EVEN_HEX,
          `transaction ${index.toString()} CBOR`,
        ),
        l2TransactionSourceCbor: canonicalHex(
          transaction.l2TransactionSourceCbor,
          EVEN_HEX,
          `transaction ${index.toString()} source CBOR`,
        ),
      });
    }),
  );
  const l2TransactionCount = natural(
    artifact.l2TransactionCount,
    "committed-field-shape transaction count",
  );
  if (l2TransactionCount !== transactions.length) {
    throw new Error(
      "committed-field-shape artifact transaction count differs from its leaves",
    );
  }
  return Object.freeze({
    schemaVersion: COMMITTED_FIELD_SHAPE_ARTIFACT,
    headerHash: canonicalHex(
      artifact.headerHash,
      HEX_28,
      "committed-field-shape header",
    ),
    committedTransactionsRoot: canonicalHex(
      artifact.committedTransactionsRoot,
      HEX_32,
      "committed transactions root",
    ),
    l2TransactionCount,
    transactionsPhasRoot: canonicalHex(
      artifact.transactionsPhasRoot,
      HEX_32,
      "transactions PHAS root",
    ),
    selectedTransactionIndex: natural(
      artifact.selectedTransactionIndex,
      "selected transaction index",
    ),
    selectedFieldIndex: natural(
      artifact.selectedFieldIndex,
      "selected field index",
    ),
    txMembershipProofCbor: canonicalHex(
      artifact.txMembershipProofCbor,
      EVEN_HEX,
      "transaction membership proof",
    ),
    transactions,
  });
};

type AdmittedCommittedFieldShapeArtifact = Readonly<{
  artifact: CommittedFieldShapeArtifact;
  prepared: PreparedCommittedFieldShape;
  txInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}>;

/** Strictly reopens every source leaf and reproduces the selected proof. */
export const admitCommittedFieldShapeArtifact = async (
  value: unknown,
): Promise<AdmittedCommittedFieldShapeArtifact> => {
  const artifact = parseArtifact(value);
  const decoded = await Promise.all(
    artifact.transactions.map((transaction) =>
      decodeTransactionMaterial(transaction),
    ),
  );
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  if (trie.root !== artifact.transactionsPhasRoot) {
    throw new Error(
      "committed-field-shape artifact transactions PHAS root changed",
    );
  }
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: artifact.committedTransactionsRoot,
    count: BigInt(artifact.l2TransactionCount),
  });
  const transaction = decoded[artifact.selectedTransactionIndex];
  if (transaction === undefined) {
    throw new Error("committed-field-shape selected transaction is absent");
  }
  const proof = requireProof(
    trie,
    Buffer.from(transaction.nodeTxId, "hex"),
    "committed-field-shape transaction",
  );
  if (proof !== artifact.txMembershipProofCbor) {
    throw new Error(
      "committed-field-shape transaction proof differs from leaf re-derivation",
    );
  }
  const canonical = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(
      artifact.transactions[artifact.selectedTransactionIndex]!.txCbor,
      "hex",
    ),
  );
  const prepared = prepareCommittedFieldShapeFromCanonicalTx({
    tx: canonical,
    fieldIndex: artifact.selectedFieldIndex,
  });
  if (prepared.evidence.badTxId !== transaction.nodeTxId) {
    throw new Error(
      "committed-field-shape selected transaction id changed on re-derivation",
    );
  }
  const txInclusion = parseSubmitStep01TxInclusion({
    nativeTxId: transaction.nodeTxId,
    nativeTx: transaction.nativeTxCompact,
    nativeTxCompactCbor: transaction.nativeCompactCbor,
    l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
    transactionsPhasRoot: trie.root,
    txMembershipProofCbor: proof,
  });
  return Object.freeze({ artifact, prepared, txInclusion });
};

const fieldIndexFromClassification = ({
  classification,
  transactionIndex,
  nodeTxId,
}: {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "committedFieldShape" };
  readonly transactionIndex: number;
  readonly nodeTxId: string;
}): number => {
  if (
    classification.selected.violationId !==
      COMMITTED_FIELD_SHAPE_VIOLATION_ID ||
    classification.selected.position !== BigInt(transactionIndex)
  ) {
    throw new Error(
      "committed-field-shape classification does not bind its transaction position",
    );
  }
  const prefix = `${COMMITTED_FIELD_SHAPE_VIOLATION_ID}:${transactionIndex.toString()}:${nodeTxId}:`;
  if (!classification.selected.detectionId.startsWith(prefix)) {
    throw new Error(
      "committed-field-shape classification does not bind its canonical transaction",
    );
  }
  const suffix = classification.selected.detectionId.slice(prefix.length);
  if (!/^(?:0|[1-9][0-9]*)$/u.test(suffix)) {
    throw new Error(
      "committed-field-shape classification has a malformed field index",
    );
  }
  return Number(suffix);
};

const prepareArtifactFromEvidence = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "committedFieldShape" };
}): Promise<CommittedFieldShapeArtifact> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "committed-field-shape classification differs from canonical evidence",
    );
  }
  const transactionIndex = Number(classification.selected.position);
  const transaction = admitted.transactions[transactionIndex];
  if (transaction === undefined) {
    throw new Error(
      "committed-field-shape classification selected an absent transaction",
    );
  }
  const fieldIndex = fieldIndexFromClassification({
    classification,
    transactionIndex,
    nodeTxId: transaction.nodeTxId,
  });
  const decoded = await Promise.all(
    admitted.transactions.map(decodeTransactionMaterial),
  );
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  if (trie.root !== evidence.reconstruction.rootData.transactions.phasRoot) {
    throw new Error(
      "committed-field-shape canonical source leaves differ from reconstructed DA",
    );
  }
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    count: BigInt(decoded.length),
  });
  const proof = requireProof(
    trie,
    Buffer.from(transaction.nodeTxId, "hex"),
    "committed-field-shape transaction",
  );
  const artifact = normalizeJournalJson({
    schemaVersion: COMMITTED_FIELD_SHAPE_ARTIFACT,
    headerHash: admitted.headerHash,
    committedTransactionsRoot: admitted.expectedTransactionsRoot,
    l2TransactionCount: decoded.length,
    transactionsPhasRoot: trie.root,
    selectedTransactionIndex: transactionIndex,
    selectedFieldIndex: fieldIndex,
    txMembershipProofCbor: proof,
    transactions: admitted.transactions.map((item) => ({
      nodeTxId: item.nodeTxId,
      txCbor: item.txCbor,
      l2TransactionSourceCbor: item.l2TransactionSourceCbor,
    })),
  }) as CommittedFieldShapeArtifact;
  await admitCommittedFieldShapeArtifact(artifact);
  return Object.freeze(artifact);
};

export type CommittedFieldShapeWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

type BoundCommittedFieldShapeTransactionsConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBinding<"committedFieldShape">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: CommittedFieldShapeContracts;
  category: FraudProofWorkflowDeploymentBinding<"committedFieldShape">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"committedFieldShape">["catalogue"];
  referenceScripts: CommittedFieldShapeWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

type CommittedFieldShapeBuilderSet = Readonly<{
  init: typeof submitCommittedFieldShapeInit;
  step01: typeof submitCommittedFieldShapeStep01;
  step02: typeof submitCommittedFieldShapeStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

const productionBuilders: CommittedFieldShapeBuilderSet = Object.freeze({
  init: submitCommittedFieldShapeInit,
  step01: submitCommittedFieldShapeStep01,
  step02: submitCommittedFieldShapeStep02,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "committed-field-shape workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "committedFieldShape" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("committed-field-shape workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string") {
    throw new Error(`committed-field-shape workflow action omitted ${name}`);
  }
  return value;
};

const createBoundTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundCommittedFieldShapeTransactionsConfig;
  readonly builders: CommittedFieldShapeBuilderSet;
}): LinearFamilyTransactionPort<"committedFieldShape"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "committedFieldShape",
  prepare: async ({ evidence, classification }) =>
    await prepareArtifactFromEvidence({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitCommittedFieldShapeArtifact(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "committed-field-shape artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.init({
            lucid: config.lucid,
            blueprint: config.blueprint,
            network: config.network,
            contracts: config.contracts,
            category: config.category,
            catalogue: config.catalogue,
            signer: config.signer,
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
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            txInclusion: admitted.txInclusion,
            prepared: admitted.prepared,
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
            fraudCategory: "committedFieldShape",
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
                  "committed-field-shape removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefs(
                  built.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "committed-field-shape removal does not reference the authenticated retained proof token",
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
    }
    throw new Error(
      `committed-field-shape workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundCommittedFieldShapeWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: CommittedFieldShapeWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundCommittedFieldShapeWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"committedFieldShape">;
  l1: FraudProofFamilyL1ObservationPort<"committedFieldShape">;
  transactions: LinearFamilyTransactionPort<"committedFieldShape">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundCommittedFieldShapeWorkflow = async (
  config: ManifestBoundCommittedFieldShapeWorkflowConfig,
): Promise<ManifestBoundCommittedFieldShapeWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "committedFieldShape",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      CommittedFieldShapeStep02Datum,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.committedFieldShape;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error(
      "committed-field-shape manifest binding omitted required contracts",
    );
  }
  const references: CommittedFieldShapeWorkflowReferenceScripts = Object.freeze(
    {
      steps: Object.freeze([
        requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "fraudProofCommittedFieldShape",
          utxo: config.referenceScripts.steps[0],
        }),
        requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName: "fraudProofCommittedFieldShapeStep02",
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
    },
  );
  const contracts: CommittedFieldShapeContracts = Object.freeze({
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
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const transactions = createBoundTransactionPort({
    config: {
      lucid: config.lucid,
      blueprint: binding.blueprint,
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
      deploymentInfo: binding.deploymentInfo,
    },
    builders: productionBuilders,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter: createLinearFamilyWorkflowAdapter({
      category: "committedFieldShape",
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

export const runOrResumeManifestBoundCommittedFieldShapeWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundCommittedFieldShapeWorkflow;
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
    replayer: COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["committedFieldShape"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export const unsafeCreateCommittedFieldShapeTransactionPortForTest = (input: {
  readonly config: BoundCommittedFieldShapeTransactionsConfig;
  readonly builders: CommittedFieldShapeBuilderSet;
}): LinearFamilyTransactionPort<"committedFieldShape"> =>
  createBoundTransactionPort(input);
