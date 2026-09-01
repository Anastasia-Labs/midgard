import {
  decodeMidgardNativeByteListPreimage,
  deriveMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  decodeAddressWitnessPreimage,
  FraudProofComputationThreadStepDatum,
  type MidgardAddressWitness,
  MISSING_SIGNATURE_VIOLATION_ID_V1,
  MissingSignatureStep02Datum,
  MissingSignatureStep03Datum,
  MissingSignatureStep04Datum,
  missingSignatureVkeyHashV1,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  admitCanonicalEvidenceForProofBuildV1,
  type CanonicalEvidenceBuilderInputV1,
} from "../evidence/prepare-from-evidence-v1.js";
import type { MissingSignatureContractsV1 } from "../missing-signature/contracts-v1.js";
import { submitMissingSignatureInit } from "../missing-signature/submit-missing-signature-init.js";
import { submitMissingSignatureStep01 } from "../missing-signature/submit-missing-signature-step-01.js";
import { submitMissingSignatureStep02 } from "../missing-signature/submit-missing-signature-step-02.js";
import { submitMissingSignatureStep03 } from "../missing-signature/submit-missing-signature-step-03.js";
import { submitMissingSignatureStep04 } from "../missing-signature/submit-missing-signature-step-04.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type PreparedTxInclusionJson,
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
import {
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
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
  createProductionMissingSignatureWorkflowAdapterV1,
  PRODUCTION_MISSING_SIGNATURE_TRANSACTION_PORT_V1,
  type ProductionMissingSignatureCapturedActionV1,
  type ProductionMissingSignatureTransactionPortV1,
} from "./production-missing-signature-adapter-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_MISSING_SIGNATURE_ARTIFACT_V1 =
  "midgard-production-missing-signature-artifact-v1" as const;

type MissingSignatureArtifactTransactionV1 = Readonly<{
  nodeTxId: string;
  txCbor: string;
  l2TransactionSourceCbor: string;
}>;

export type ProductionMissingSignatureArtifactV1 = JournalJsonObjectV1 & {
  readonly schemaVersion: typeof PRODUCTION_MISSING_SIGNATURE_ARTIFACT_V1;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly selectedTransactionIndex: number;
  readonly accusedRequiredSignerIndex: number;
  readonly accusedRequiredSignerHash: string;
  readonly resolvedVkey: string;
  readonly transactions: readonly MissingSignatureArtifactTransactionV1[];
};

export type AdmittedProductionMissingSignatureArtifactV1 = Readonly<{
  artifact: ProductionMissingSignatureArtifactV1;
  txInclusion: SubmitStep01TxInclusion;
  nativeTxCompactCbor: string;
  requiredSignerHashes: readonly string[];
  addrTxWits: readonly MidgardAddressWitness[];
  witnessSetCompact: NativeTxWitnessSetCompact;
  accusedRequiredSignerIndex: bigint;
  resolvedVkey: string;
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

const parseArtifact = (
  value: unknown,
): ProductionMissingSignatureArtifactV1 => {
  const artifact = record(value, "missing-signature artifact");
  exactKeys(
    artifact,
    [
      "schemaVersion",
      "headerHash",
      "committedTransactionsRoot",
      "selectedTransactionIndex",
      "accusedRequiredSignerIndex",
      "accusedRequiredSignerHash",
      "resolvedVkey",
      "transactions",
    ],
    "missing-signature artifact",
  );
  if (artifact.schemaVersion !== PRODUCTION_MISSING_SIGNATURE_ARTIFACT_V1) {
    throw new Error("missing-signature artifact version changed");
  }
  if (
    !Array.isArray(artifact.transactions) ||
    artifact.transactions.length === 0
  ) {
    throw new Error("missing-signature artifact has no committed transactions");
  }
  const transactions = Object.freeze(
    artifact.transactions.map((value, index) => {
      const transaction = record(
        value,
        `missing-signature transaction ${index.toString()}`,
      );
      exactKeys(
        transaction,
        ["nodeTxId", "txCbor", "l2TransactionSourceCbor"],
        `missing-signature transaction ${index.toString()}`,
      );
      return Object.freeze({
        nodeTxId: canonicalHex(
          transaction.nodeTxId,
          HEX_32,
          `missing-signature transaction ${index.toString()} id`,
        ),
        txCbor: canonicalHex(
          transaction.txCbor,
          EVEN_HEX,
          `missing-signature transaction ${index.toString()} CBOR`,
        ),
        l2TransactionSourceCbor: canonicalHex(
          transaction.l2TransactionSourceCbor,
          EVEN_HEX,
          `missing-signature transaction ${index.toString()} source`,
        ),
      });
    }),
  );
  return Object.freeze({
    schemaVersion: PRODUCTION_MISSING_SIGNATURE_ARTIFACT_V1,
    headerHash: canonicalHex(
      artifact.headerHash,
      HEX_28,
      "missing-signature header",
    ),
    committedTransactionsRoot: canonicalHex(
      artifact.committedTransactionsRoot,
      HEX_32,
      "missing-signature transactions root",
    ),
    selectedTransactionIndex: natural(
      artifact.selectedTransactionIndex,
      "missing-signature selected transaction index",
    ),
    accusedRequiredSignerIndex: natural(
      artifact.accusedRequiredSignerIndex,
      "missing-signature accused signer index",
    ),
    accusedRequiredSignerHash: canonicalHex(
      artifact.accusedRequiredSignerHash,
      HEX_28,
      "missing-signature accused signer hash",
    ),
    resolvedVkey: canonicalHex(
      artifact.resolvedVkey,
      HEX_32,
      "missing-signature resolved verification key",
    ),
    transactions,
  });
};

const signerHashes = (
  preimageCbor: Uint8Array,
  label: string,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, label).map(
    (bytes, index) => {
      if (bytes.length !== 28) {
        throw new Error(
          `${label}[${index.toString()}] is not a 28-byte signer hash`,
        );
      }
      return Buffer.from(bytes).toString("hex");
    },
  );

const witnessSetCompact = (
  witnessSet: Parameters<typeof deriveMidgardNativeTxWitnessSetCompactV1>[0],
): NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompactV1(witnessSet);
  return {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
};

const publicCommittedVkeyFor = ({
  hash,
  witnesses,
}: {
  readonly hash: string;
  readonly witnesses: readonly (readonly MidgardAddressWitness[])[];
}): string | undefined => {
  for (const list of witnesses) {
    for (const witness of list) {
      const verificationKey = witness.verification_key.toLowerCase();
      if (missingSignatureVkeyHashV1(verificationKey) === hash) {
        return verificationKey;
      }
    }
  }
  return undefined;
};

/**
 * Re-authenticates every durable byte, rebuilds the counted transaction root
 * and MPF proof, and recovers the vkey only from committed public L2 evidence.
 */
export const admitProductionMissingSignatureArtifactV1 = async (
  value: unknown,
): Promise<AdmittedProductionMissingSignatureArtifactV1> => {
  const artifact = parseArtifact(value);
  const decoded = await Promise.all(
    artifact.transactions.map(decodeTransactionMaterial),
  );
  const selected = decoded[artifact.selectedTransactionIndex];
  if (selected === undefined) {
    throw new Error("missing-signature artifact selected no transaction");
  }
  const requiredSignerHashes = signerHashes(
    selected.nativeTx.body.requiredSignersPreimageCbor,
    `transaction ${selected.nodeTxId} required_signers`,
  );
  const accused = requiredSignerHashes[artifact.accusedRequiredSignerIndex];
  if (accused !== artifact.accusedRequiredSignerHash) {
    throw new Error(
      "missing-signature artifact accused ordinal differs from the committed required-signer list",
    );
  }
  const allWitnesses = decoded.map((transaction) =>
    decodeAddressWitnessPreimage(
      transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
    ),
  );
  const addrTxWits = allWitnesses[artifact.selectedTransactionIndex]!;
  if (
    addrTxWits.some(
      (witness) =>
        missingSignatureVkeyHashV1(witness.verification_key) === accused,
    )
  ) {
    throw new Error(
      "missing-signature artifact accused key is present in the committed witness field",
    );
  }
  const resolvedVkey = publicCommittedVkeyFor({
    hash: accused,
    witnesses: allWitnesses,
  });
  if (resolvedVkey === undefined) {
    throw new Error(
      "missing-signature vkey preimage is absent from authenticated public evidence; route this case to validationTraceDispute",
    );
  }
  if (artifact.resolvedVkey !== resolvedVkey) {
    throw new Error(
      "missing-signature durable vkey is not the deterministic committed public preimage",
    );
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
  await requireTransactionsRootMatchV1({
    sourceRoot: trie.root,
    expectedTransactionsRoot: artifact.committedTransactionsRoot,
    count: BigInt(decoded.length),
  });
  const txInclusion: PreparedTxInclusionJson = Object.freeze({
    nativeTxId: selected.nodeTxId,
    nativeTx: selected.nativeTxCompact,
    nativeTxCompactCbor: selected.nativeCompactCbor,
    l2TransactionSourceCbor: selected.l2TransactionSourceCbor,
    transactionsPhasRoot: trie.root,
    txMembershipProofCbor: requireProof(
      trie,
      transactionSourceTrieItemV1(selected).key,
      "missing-signature transaction",
    ),
  });
  return Object.freeze({
    artifact,
    txInclusion: parseSubmitStep01TxInclusion(txInclusion),
    nativeTxCompactCbor: selected.nativeCompactCbor,
    requiredSignerHashes: Object.freeze([...requiredSignerHashes]),
    addrTxWits: Object.freeze([...addrTxWits]),
    witnessSetCompact: Object.freeze(
      witnessSetCompact(selected.nativeTx.witnessSet),
    ),
    accusedRequiredSignerIndex: BigInt(artifact.accusedRequiredSignerIndex),
    resolvedVkey,
  });
};

const selectedDetection = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingSignature" },
): Readonly<{
  transactionIndex: number;
  signerIndex: number;
  txId: string;
  signerHash: string;
}> => {
  const [violationId, transaction, signer, txId, signerHash, ...surplus] =
    classification.selected.detectionId.split(":");
  if (
    violationId !== MISSING_SIGNATURE_VIOLATION_ID_V1 ||
    surplus.length !== 0 ||
    !/^(?:0|[1-9][0-9]*)$/u.test(transaction ?? "") ||
    !/^(?:0|[1-9][0-9]*)$/u.test(signer ?? "") ||
    !HEX_32.test(txId ?? "") ||
    !HEX_28.test(signerHash ?? "")
  ) {
    throw new Error(
      "missing-signature classification has a malformed identity",
    );
  }
  const transactionIndex = Number(transaction);
  const signerIndex = Number(signer);
  if (
    !Number.isSafeInteger(transactionIndex) ||
    !Number.isSafeInteger(signerIndex) ||
    classification.selected.position !== BigInt(transactionIndex)
  ) {
    throw new Error("missing-signature classification has invalid ordinals");
  }
  return {
    transactionIndex,
    signerIndex,
    txId: txId!,
    signerHash: signerHash!,
  };
};

export const prepareProductionMissingSignatureArtifactV1 = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingSignature" };
}): Promise<ProductionMissingSignatureArtifactV1> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.violationId !== MISSING_SIGNATURE_VIOLATION_ID_V1
  ) {
    throw new Error(
      "missing-signature classification differs from canonical evidence",
    );
  }
  const selected = selectedDetection(classification);
  const transactions = admitted.transactions.map((transaction) => ({
    nodeTxId: transaction.nodeTxId,
    txCbor: transaction.txCbor,
    l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
  }));
  if (transactions[selected.transactionIndex]?.nodeTxId !== selected.txId) {
    throw new Error(
      "missing-signature classification transaction differs from committed evidence",
    );
  }
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const allWitnesses = decoded.map((transaction) =>
    decodeAddressWitnessPreimage(
      transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
    ),
  );
  const resolvedVkey = publicCommittedVkeyFor({
    hash: selected.signerHash,
    witnesses: allWitnesses,
  });
  if (resolvedVkey === undefined) {
    throw new Error(
      "missing-signature public evidence has no vkey preimage; the direct family must not accept operator input and this case requires validationTraceDispute",
    );
  }
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_MISSING_SIGNATURE_ARTIFACT_V1,
    headerHash: admitted.headerHash,
    committedTransactionsRoot: admitted.expectedTransactionsRoot,
    selectedTransactionIndex: selected.transactionIndex,
    accusedRequiredSignerIndex: selected.signerIndex,
    accusedRequiredSignerHash: selected.signerHash,
    resolvedVkey,
    transactions,
  }) as ProductionMissingSignatureArtifactV1;
  await admitProductionMissingSignatureArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type MissingSignatureWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
  fieldCertificates?: Readonly<{
    step02?: UTxO;
    step04?: UTxO;
  }>;
}>;

type BoundMissingSignatureTransactionsConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"missingSignature">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: MissingSignatureContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"missingSignature">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"missingSignature">["catalogue"];
  referenceScripts: MissingSignatureWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

type MissingSignatureBuilderSetV1 = Readonly<{
  init: typeof submitMissingSignatureInit;
  step01: typeof submitMissingSignatureStep01;
  step02: typeof submitMissingSignatureStep02;
  step03: typeof submitMissingSignatureStep03;
  step04: typeof submitMissingSignatureStep04;
  remove: typeof submitRemoveFraudulentBlock;
}>;

const productionBuilders: MissingSignatureBuilderSetV1 = Object.freeze({
  init: submitMissingSignatureInit,
  step01: submitMissingSignatureStep01,
  step02: submitMissingSignatureStep02,
  step03: submitMissingSignatureStep03,
  step04: submitMissingSignatureStep04,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "missing-signature workflow action");
  if (
    input.schemaVersion !== "midgard-production-missing-signature-action-v1" ||
    input.category !== "missingSignature" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("missing-signature workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string") {
    throw new Error(`missing-signature workflow action omitted ${name}`);
  }
  return value;
};

const createBoundTransactionPortV1 = ({
  config,
  builders,
}: {
  readonly config: BoundMissingSignatureTransactionsConfigV1;
  readonly builders: MissingSignatureBuilderSetV1;
}): ProductionMissingSignatureTransactionPortV1 => ({
  portVersion: PRODUCTION_MISSING_SIGNATURE_TRANSACTION_PORT_V1,
  category: "missingSignature",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionMissingSignatureArtifactV1({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitProductionMissingSignatureArtifactV1(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "missing-signature artifact targets a different manifest-bound header",
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
            network: config.network,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            txInclusion: admitted.txInclusion,
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
            requiredSignerHashes: admitted.requiredSignerHashes,
            nativeTxCompactCbor: admitted.nativeTxCompactCbor,
            badRequiredSignerHashIndex: admitted.accusedRequiredSignerIndex,
            certificateUtxo: config.referenceScripts.fieldCertificates?.step02,
            referenceScriptUtxo: config.referenceScripts.steps[1],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_03") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await builders.step03({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            missingRequiredSignerVkey: admitted.resolvedVkey,
            referenceScriptUtxo: config.referenceScripts.steps[2],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_04") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await builders.step04({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            addrTxWits: admitted.addrTxWits,
            nativeTxCompactCbor: admitted.nativeTxCompactCbor,
            witnessSetCompact: admitted.witnessSetCompact,
            certificateUtxo: config.referenceScripts.fieldCertificates?.step04,
            referenceScriptUtxo: config.referenceScripts.steps[3],
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
            fraudCategory: "missingSignature",
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
                  "missing-signature removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefsV1(
                  built.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "missing-signature removal does not reference the authenticated retained proof token",
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
      }) satisfies ProductionMissingSignatureCapturedActionV1;
    }
    throw new Error(
      `missing-signature workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundMissingSignatureWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingSignatureWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingSignatureWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"missingSignature">;
  l1: FraudProofFamilyL1ObservationPortV1<"missingSignature">;
  transactions: ProductionMissingSignatureTransactionPortV1;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundMissingSignatureWorkflowV1 = async (
  config: ManifestBoundMissingSignatureWorkflowConfigV1,
): Promise<ManifestBoundMissingSignatureWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "missingSignature",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      MissingSignatureStep02Datum,
      MissingSignatureStep03Datum,
      MissingSignatureStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.missingSignature;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    binding.fieldPreimageCertificate === null
  ) {
    throw new Error(
      "missing-signature manifest binding omitted required contracts",
    );
  }
  const references: MissingSignatureWorkflowReferenceScriptsV1 = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMissingSignature",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMissingSignatureStep02",
        utxo: config.referenceScripts.steps[1],
      }),
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMissingSignatureStep03",
        utxo: config.referenceScripts.steps[2],
      }),
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMissingSignatureStep04",
        utxo: config.referenceScripts.steps[3],
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
    ...(config.referenceScripts.fieldCertificates === undefined
      ? {}
      : { fieldCertificates: config.referenceScripts.fieldCertificates }),
  });
  const contracts: MissingSignatureContractsV1 = Object.freeze({
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
    fieldPreimageCertificatePolicyId: binding.fieldPreimageCertificate.policyId,
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
    adapter: createProductionMissingSignatureWorkflowAdapterV1({
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

export const runOrResumeManifestBoundMissingSignatureWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMissingSignatureWorkflowV1;
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
    replayer: MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["missingSignature"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export const unsafeCreateMissingSignatureTransactionPortForTest = (input: {
  readonly config: BoundMissingSignatureTransactionsConfigV1;
  readonly builders: MissingSignatureBuilderSetV1;
}): ProductionMissingSignatureTransactionPortV1 =>
  createBoundTransactionPortV1(input);
