import { decodeMidgardNativeTxFullV1FromCanonicalCbor } from "@al-ft/midgard-core";
import {
  COMMITTED_FIELD_SHAPE_VIOLATION_ID_V1,
  CommittedFieldShapeStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { CommittedFieldShapeContractsV1 } from "../committed-field-shape/contracts-v1.js";
import {
  prepareCommittedFieldShapeFromCanonicalTxV1,
  type PreparedCommittedFieldShapeV1,
} from "../committed-field-shape/prepare-committed-field-shape-v1.js";
import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import { submitCommittedFieldShapeStep01 } from "../committed-field-shape/submit-committed-field-shape-step-01.js";
import { submitCommittedFieldShapeStep02 } from "../committed-field-shape/submit-committed-field-shape-step-02.js";
import {
  admitCanonicalEvidenceForProofBuildV1,
  type CanonicalEvidenceBuilderInputV1,
} from "../evidence/prepare-from-evidence-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  requireTransactionsRootMatchV1,
  transactionSourceTrieItemV1,
} from "../prepare-double-spend.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
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
  runFraudProofWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_COMMITTED_FIELD_SHAPE_ARTIFACT_V1 =
  "midgard-production-committed-field-shape-artifact-v1" as const;

type ArtifactTransactionV1 = Readonly<{
  nodeTxId: string;
  txCbor: string;
  l2TransactionSourceCbor: string;
}>;

export type ProductionCommittedFieldShapeArtifactV1 = JournalJsonObjectV1 & {
  readonly schemaVersion: typeof PRODUCTION_COMMITTED_FIELD_SHAPE_ARTIFACT_V1;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly l2TransactionCount: number;
  readonly transactionsPhasRoot: string;
  readonly selectedTransactionIndex: number;
  readonly selectedFieldIndex: number;
  readonly txMembershipProofCbor: string;
  readonly transactions: readonly ArtifactTransactionV1[];
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

const parseArtifact = (
  value: unknown,
): ProductionCommittedFieldShapeArtifactV1 => {
  const artifact = record(value, "committed-field-shape artifact");
  exactKeys(artifact, artifactFields, "committed-field-shape artifact");
  if (artifact.schemaVersion !== PRODUCTION_COMMITTED_FIELD_SHAPE_ARTIFACT_V1) {
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
    schemaVersion: PRODUCTION_COMMITTED_FIELD_SHAPE_ARTIFACT_V1,
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

type AdmittedCommittedFieldShapeArtifactV1 = Readonly<{
  artifact: ProductionCommittedFieldShapeArtifactV1;
  prepared: PreparedCommittedFieldShapeV1;
  txInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}>;

/** Strictly reopens every source leaf and reproduces the selected proof. */
export const admitProductionCommittedFieldShapeArtifactV1 = async (
  value: unknown,
): Promise<AdmittedCommittedFieldShapeArtifactV1> => {
  const artifact = parseArtifact(value);
  const decoded = await Promise.all(
    artifact.transactions.map((transaction) =>
      decodeTransactionMaterial(transaction),
    ),
  );
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
  if (trie.root !== artifact.transactionsPhasRoot) {
    throw new Error(
      "committed-field-shape artifact transactions PHAS root changed",
    );
  }
  await requireTransactionsRootMatchV1({
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
  const canonical = decodeMidgardNativeTxFullV1FromCanonicalCbor(
    Buffer.from(
      artifact.transactions[artifact.selectedTransactionIndex]!.txCbor,
      "hex",
    ),
  );
  const prepared = prepareCommittedFieldShapeFromCanonicalTxV1({
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
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "committedFieldShape" };
  readonly transactionIndex: number;
  readonly nodeTxId: string;
}): number => {
  if (
    classification.selected.violationId !==
      COMMITTED_FIELD_SHAPE_VIOLATION_ID_V1 ||
    classification.selected.position !== BigInt(transactionIndex)
  ) {
    throw new Error(
      "committed-field-shape classification does not bind its transaction position",
    );
  }
  const prefix = `${COMMITTED_FIELD_SHAPE_VIOLATION_ID_V1}:${transactionIndex.toString()}:${nodeTxId}:`;
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

const prepareArtifactFromEvidenceV1 = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "committedFieldShape" };
}): Promise<ProductionCommittedFieldShapeArtifactV1> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
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
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
  if (trie.root !== evidence.reconstruction.rootData.transactions.phasRoot) {
    throw new Error(
      "committed-field-shape canonical source leaves differ from reconstructed DA",
    );
  }
  await requireTransactionsRootMatchV1({
    sourceRoot: trie.root,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    count: BigInt(decoded.length),
  });
  const proof = requireProof(
    trie,
    Buffer.from(transaction.nodeTxId, "hex"),
    "committed-field-shape transaction",
  );
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_COMMITTED_FIELD_SHAPE_ARTIFACT_V1,
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
  }) as ProductionCommittedFieldShapeArtifactV1;
  await admitProductionCommittedFieldShapeArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type CommittedFieldShapeWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

type BoundCommittedFieldShapeTransactionsConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"committedFieldShape">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: CommittedFieldShapeContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"committedFieldShape">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"committedFieldShape">["catalogue"];
  referenceScripts: CommittedFieldShapeWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

type CommittedFieldShapeBuilderSetV1 = Readonly<{
  init: typeof submitCommittedFieldShapeInit;
  step01: typeof submitCommittedFieldShapeStep01;
  step02: typeof submitCommittedFieldShapeStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

const productionBuilders: CommittedFieldShapeBuilderSetV1 = Object.freeze({
  init: submitCommittedFieldShapeInit,
  step01: submitCommittedFieldShapeStep01,
  step02: submitCommittedFieldShapeStep02,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowActionV1,
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

const createBoundTransactionPortV1 = ({
  config,
  builders,
}: {
  readonly config: BoundCommittedFieldShapeTransactionsConfigV1;
  readonly builders: CommittedFieldShapeBuilderSetV1;
}): ProductionLinearFamilyTransactionPortV1<"committedFieldShape"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "committedFieldShape",
  prepare: async ({ evidence, classification }) =>
    await prepareArtifactFromEvidenceV1({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted =
      await admitProductionCommittedFieldShapeArtifactV1(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "committed-field-shape artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
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
      const transaction = await captureLocallyEvaluatedTransactionV1(
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
      const transaction = await captureLocallyEvaluatedTransactionV1(
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
      const transaction = await captureLocallyEvaluatedTransactionV1(
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
                !workflowTransactionInputOutRefsV1(built.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "committed-field-shape removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefsV1(
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

export type ManifestBoundCommittedFieldShapeWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: CommittedFieldShapeWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundCommittedFieldShapeWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"committedFieldShape">;
  l1: FraudProofFamilyL1ObservationPortV1<"committedFieldShape">;
  transactions: ProductionLinearFamilyTransactionPortV1<"committedFieldShape">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundCommittedFieldShapeWorkflowV1 = async (
  config: ManifestBoundCommittedFieldShapeWorkflowConfigV1,
): Promise<ManifestBoundCommittedFieldShapeWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
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
  assertManifestBoundWorkflowSignerV1({
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
  const references: CommittedFieldShapeWorkflowReferenceScriptsV1 =
    Object.freeze({
      steps: Object.freeze([
        requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "fraudProofCommittedFieldShape",
          utxo: config.referenceScripts.steps[0],
        }),
        requireManifestBoundReferenceScriptUtxoV1({
          binding,
          contractName: "fraudProofCommittedFieldShapeStep02",
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
  const contracts: CommittedFieldShapeContractsV1 = Object.freeze({
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
    claimRegistry: binding.claimRegistry,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const transactions = createBoundTransactionPortV1({
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
    adapter: createProductionLinearFamilyWorkflowAdapterV1({
      category: "committedFieldShape",
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

export const runOrResumeManifestBoundCommittedFieldShapeWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundCommittedFieldShapeWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["committedFieldShape"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export const unsafeCreateCommittedFieldShapeTransactionPortForTest = (input: {
  readonly config: BoundCommittedFieldShapeTransactionsConfigV1;
  readonly builders: CommittedFieldShapeBuilderSetV1;
}): ProductionLinearFamilyTransactionPortV1<"committedFieldShape"> =>
  createBoundTransactionPortV1(input);
