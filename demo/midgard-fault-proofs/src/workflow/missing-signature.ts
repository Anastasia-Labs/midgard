import {
  decodeMidgardNativeByteListPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  MissingSignatureForcedSignerDatum,
  MissingSignatureForcedStepDatum,
  MissingSignatureForcedWitnessDatum,
} from "@al-ft/midgard-sdk";
import {
  decodeAddressWitnessPreimage,
  FraudProofComputationThreadStepDatum,
  type MidgardAddressWitness,
  MISSING_SIGNATURE_VIOLATION_ID,
  MissingSignatureStep02Datum,
  MissingSignatureStep03Datum,
  MissingSignatureStep04Datum,
  missingSignatureVkeyHash,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import type { MissingSignatureContracts } from "../missing-signature/contracts.js";
import {
  planMissingSignatureAddressWitnessesOpening,
  planMissingSignatureRequiredSignersOpening,
} from "../missing-signature/evidence.js";
import {
  admitMissingSignatureForcedArtifact,
  MISSING_SIGNATURE_FORCED_ARTIFACT,
  missingSignatureForcedArtifact,
} from "../missing-signature/forced-artifact.js";
import { submitMissingSignatureForcedAction } from "../missing-signature/submit-forced.js";
import { submitMissingSignatureInit } from "../missing-signature/submit-missing-signature-init.js";
import { submitMissingSignatureStep01 } from "../missing-signature/submit-missing-signature-step-01.js";
import { submitMissingSignatureStep02 } from "../missing-signature/submit-missing-signature-step-02.js";
import { submitMissingSignatureStep03 } from "../missing-signature/submit-missing-signature-step-03.js";
import { submitMissingSignatureStep04 } from "../missing-signature/submit-missing-signature-step-04.js";
import {
  MISSING_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID,
  prepareMissingSignatureWrongfulRejection,
} from "../missing-signature/wrongful-rejection.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type PreparedTxInclusionJson,
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
import {
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
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
import { observeFraudProofWorkflowHeader } from "./family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "./field-carriage-prerequisite.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import {
  createMissingSignatureWorkflowAdapter,
  MISSING_SIGNATURE_TRANSACTION_PORT,
  type MissingSignatureCapturedAction,
  type MissingSignatureTransactionPort,
} from "./missing-signature-adapter.js";
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

export const MISSING_SIGNATURE_ARTIFACT =
  "midgard-production-missing-signature-artifact-v1" as const;

type MissingSignatureArtifactTransaction = Readonly<{
  nodeTxId: string;
  txCbor: string;
  l2TransactionSourceCbor: string;
}>;

export type MissingSignatureArtifact = JournalJsonObject & {
  readonly schemaVersion: typeof MISSING_SIGNATURE_ARTIFACT;
  readonly headerHash: string;
  readonly committedTransactionsRoot: string;
  readonly selectedTransactionIndex: number;
  readonly accusedRequiredSignerIndex: number;
  readonly accusedRequiredSignerHash: string;
  readonly resolvedVkey: string;
  readonly transactions: readonly MissingSignatureArtifactTransaction[];
};

export type AdmittedMissingSignatureArtifact = Readonly<{
  artifact: MissingSignatureArtifact;
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

const parseArtifact = (value: unknown): MissingSignatureArtifact => {
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
  if (artifact.schemaVersion !== MISSING_SIGNATURE_ARTIFACT) {
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
    schemaVersion: MISSING_SIGNATURE_ARTIFACT,
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
  witnessSet: Parameters<typeof deriveMidgardNativeTxWitnessSetCompact>[0],
): NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(witnessSet);
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
      if (missingSignatureVkeyHash(verificationKey) === hash) {
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
export const admitMissingSignatureArtifact = async (
  value: unknown,
): Promise<AdmittedMissingSignatureArtifact> => {
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
        missingSignatureVkeyHash(witness.verification_key) === accused,
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
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  await requireTransactionsRootMatch({
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
      transactionSourceTrieItem(selected).key,
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
    CanonicalBlockClassification,
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
    violationId !== MISSING_SIGNATURE_VIOLATION_ID ||
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

export const prepareMissingSignatureArtifact = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingSignature" };
}): Promise<MissingSignatureArtifact> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.violationId !== MISSING_SIGNATURE_VIOLATION_ID
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
  const artifact = normalizeJournalJson({
    schemaVersion: MISSING_SIGNATURE_ARTIFACT,
    headerHash: admitted.headerHash,
    committedTransactionsRoot: admitted.expectedTransactionsRoot,
    selectedTransactionIndex: selected.transactionIndex,
    accusedRequiredSignerIndex: selected.signerIndex,
    accusedRequiredSignerHash: selected.signerHash,
    resolvedVkey,
    transactions,
  }) as MissingSignatureArtifact;
  await admitMissingSignatureArtifact(artifact);
  return Object.freeze(artifact);
};

export type MissingSignatureWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  fieldPreimageCertificateMint?: UTxO;
  forced?: Readonly<{ bind: UTxO; signer: UTxO; witness: UTxO }>;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
  fieldCertificates?: Readonly<{
    step02?: UTxO;
    step04?: UTxO;
    forcedSigner?: UTxO;
    forcedWitness?: UTxO;
  }>;
}>;

type BoundMissingSignatureTransactionsConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBinding<"missingSignature">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: MissingSignatureContracts;
  category: FraudProofWorkflowDeploymentBinding<"missingSignature">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"missingSignature">["catalogue"];
  referenceScripts: MissingSignatureWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

type MissingSignatureBuilderSet = Readonly<{
  init: typeof submitMissingSignatureInit;
  step01: typeof submitMissingSignatureStep01;
  step02: typeof submitMissingSignatureStep02;
  step03: typeof submitMissingSignatureStep03;
  step04: typeof submitMissingSignatureStep04;
  remove: typeof submitRemoveFraudulentBlock;
}>;

const productionBuilders: MissingSignatureBuilderSet = Object.freeze({
  init: submitMissingSignatureInit,
  step01: submitMissingSignatureStep01,
  step02: submitMissingSignatureStep02,
  step03: submitMissingSignatureStep03,
  step04: submitMissingSignatureStep04,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowAction,
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

export const prepareMissingSignatureWorkflowArtifact = async (
  input: Parameters<typeof prepareMissingSignatureArtifact>[0],
): Promise<JournalJsonObject> => {
  if (
    input.classification.selected.violationId !==
    MISSING_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID
  )
    return prepareMissingSignatureArtifact(input);
  admitCanonicalEvidenceForProofBuild(input.evidence);
  const prepared = await prepareMissingSignatureWrongfulRejection({
    block: input.evidence,
  });
  if (
    prepared.headerHash !== input.classification.headerHash ||
    prepared.detectionId !== input.classification.selected.detectionId
  )
    throw new Error(
      "missingSignature: forced classification differs from authenticated evidence",
    );
  const artifact = missingSignatureForcedArtifact(
    prepared,
    input.evidence.reconstruction.forcedTransactions[
      prepared.forcedIndex
    ]!.fullTransactionCbor.toString("hex"),
  );
  await admitMissingSignatureForcedArtifact(artifact);
  return artifact;
};

const createBoundTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundMissingSignatureTransactionsConfig;
  readonly builders: MissingSignatureBuilderSet;
}): MissingSignatureTransactionPort => ({
  portVersion: MISSING_SIGNATURE_TRANSACTION_PORT,
  category: "missingSignature",
  prepare: async ({ evidence, classification }) =>
    await prepareMissingSignatureWorkflowArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const forced =
      artifact.schemaVersion === MISSING_SIGNATURE_FORCED_ARTIFACT
        ? await admitMissingSignatureForcedArtifact(artifact)
        : undefined;
    const admitted =
      forced === undefined
        ? await admitMissingSignatureArtifact(artifact)
        : undefined;
    if (
      (forced?.headerHash ?? admitted?.artifact.headerHash) !==
      config.headerHash
    ) {
      throw new Error(
        "missing-signature artifact targets a different manifest-bound header",
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
    if (forced !== undefined && input.stage !== "remove") {
      const refs = config.referenceScripts.forced;
      if (refs === undefined)
        throw new Error(
          "missingSignature: forced reference scripts are not deployed",
        );
      const referenceScriptUtxo =
        input.stage === "step_01"
          ? config.referenceScripts.steps[0]
          : input.stage === "step_05"
            ? refs.bind
            : input.stage === "step_06"
              ? refs.signer
              : input.stage === "step_07"
                ? refs.witness
                : undefined;
      if (referenceScriptUtxo === undefined)
        throw new Error(
          "missingSignature: forced artifact entered an accepted stage",
        );
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await submitMissingSignatureForcedAction({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            prepared: forced,
            referenceScriptUtxo,
            witnessReferenceScripts: config.referenceScripts.witnesses,
            certificateUtxo:
              input.stage === "step_06"
                ? config.referenceScripts.fieldCertificates?.forcedSigner
                : config.referenceScripts.fieldCertificates?.forcedWitness,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_01") {
      if (admitted === undefined)
        throw new Error("missingSignature: accepted artifact missing");
      const transaction = await captureLocallyEvaluatedTransaction(
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
      if (admitted === undefined)
        throw new Error("missingSignature: accepted artifact missing");
      const transaction = await captureLocallyEvaluatedTransaction(
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
      if (admitted === undefined)
        throw new Error("missingSignature: accepted artifact missing");
      const transaction = await captureLocallyEvaluatedTransaction(
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
      if (admitted === undefined)
        throw new Error("missingSignature: accepted artifact missing");
      const transaction = await captureLocallyEvaluatedTransaction(
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
      const transaction = await captureLocallyEvaluatedTransaction(
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
                !workflowTransactionInputOutRefs(built.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "missing-signature removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefs(
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
      }) satisfies MissingSignatureCapturedAction;
    }
    throw new Error(
      `missing-signature workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundMissingSignatureWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingSignatureWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingSignatureWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingSignature">;
  l1: FraudProofFamilyL1ObservationPort<"missingSignature">;
  transactions: MissingSignatureTransactionPort;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundMissingSignatureWorkflow = async (
  config: ManifestBoundMissingSignatureWorkflowConfig,
): Promise<ManifestBoundMissingSignatureWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  assertManifestBoundWorkflowSigner({
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
  const references: MissingSignatureWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMissingSignature",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMissingSignatureStep02",
        utxo: config.referenceScripts.steps[1],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMissingSignatureStep03",
        utxo: config.referenceScripts.steps[2],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMissingSignatureStep04",
        utxo: config.referenceScripts.steps[3],
      }),
    ] as const),
    ...(config.referenceScripts.forced === undefined
      ? {}
      : {
          forced: Object.freeze({
            bind: requireManifestBoundReferenceScriptUtxo({
              binding,
              contractName: "fraudProofMissingSignatureForcedStep",
              utxo: config.referenceScripts.forced.bind,
            }),
            signer: requireManifestBoundReferenceScriptUtxo({
              binding,
              contractName: "fraudProofMissingSignatureForcedSigner",
              utxo: config.referenceScripts.forced.signer,
            }),
            witness: requireManifestBoundReferenceScriptUtxo({
              binding,
              contractName: "fraudProofMissingSignatureForcedWitness",
              utxo: config.referenceScripts.forced.witness,
            }),
          }),
        }),
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
    ...(config.referenceScripts.fieldPreimageCertificateMint === undefined
      ? {}
      : {
          fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo(
            {
              binding,
              contractName: "fieldPreimageCertificateMint",
              utxo: config.referenceScripts.fieldPreimageCertificateMint,
            },
          ),
        }),
    ...(config.referenceScripts.fieldCertificates === undefined
      ? {}
      : { fieldCertificates: config.referenceScripts.fieldCertificates }),
  });
  const contracts: MissingSignatureContracts = Object.freeze({
    steps: chain.steps,
    forcedStep: chain.forcedStep,
    forcedSigner: chain.forcedSigner,
    forcedWitness: chain.forcedWitness,
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: {
      ...binding.definition,
      computationThread: {
        ...binding.definition.computationThread,
        steps: [
          ...binding.definition.computationThread.steps,
          {
            role: "computation_thread_step_05",
            address: chain.forcedStep.spendingScriptAddress,
            datumSchema: MissingSignatureForcedStepDatum,
          },
          {
            role: "computation_thread_step_06",
            address: chain.forcedSigner.spendingScriptAddress,
            datumSchema: MissingSignatureForcedSignerDatum,
          },
          {
            role: "computation_thread_step_07",
            address: chain.forcedWitness.spendingScriptAddress,
            datumSchema: MissingSignatureForcedWitnessDatum,
          },
        ],
      },
    },
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
  if (l1.publications === undefined)
    throw new Error("missing-signature raw L1 omitted publication observer");
  const certificate = binding.fieldPreimageCertificate;
  let adapter = createMissingSignatureWorkflowAdapter({
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const prerequisite = createMissingSignatureForcedFieldPrerequisite({
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    certificate,
    certificateReference: references.fieldPreimageCertificateMint,
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "missingSignature",
    base: adapter,
    prerequisite,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const createMissingSignatureForcedFieldPrerequisite = ({
  lucid,
  network,
  signer,
  publications,
  certificate,
  certificateReference,
  transactionConfirmed,
}: {
  readonly lucid: LucidEvolution;
  readonly network: import("@lucid-evolution/lucid").Network;
  readonly signer: ResolvedProverSigner;
  readonly publications: import("./raw-l1-publication-observation.js").FraudProofAuthenticatedPublicationObserver;
  readonly certificate: {
    readonly policyId: string;
    readonly mintingScript: import("@lucid-evolution/lucid").MintingPolicy;
  };
  readonly certificateReference: UTxO | undefined;
  readonly transactionConfirmed: (input: {
    headerHash: string;
    txHash: string;
  }) => Promise<boolean>;
}) => {
  return createAuthenticatedFieldCarriagePrerequisitePort({
    category: "missingSignature",
    lucid: lucid,
    network: network,
    signer: signer,
    publications: publications,
    requirementForAction: async ({ action, artifact }) => {
      const input = record(
        action.input,
        "missing-signature prerequisite action",
      );
      if (
        artifact.schemaVersion !== MISSING_SIGNATURE_FORCED_ARTIFACT ||
        (input.stage !== "step_06" && input.stage !== "step_07")
      )
        return null;
      const prepared = await admitMissingSignatureForcedArtifact(artifact);
      if (input.stage === "step_07" && prepared.witnessIndex === -1n)
        return null;
      const planned =
        input.stage === "step_06"
          ? planMissingSignatureRequiredSignersOpening({
              anchorSourceKind: 1n,
              anchorTxId: prepared.transactionId,
              nativeTxCompactCbor: prepared.nativeTxCompactCbor,
              requiredSignerHashes: prepared.evidence.requiredSignerHashes,
              owner: signer.paymentKeyHash,
            })
          : planMissingSignatureAddressWitnessesOpening({
              anchorSourceKind: 1n,
              anchorTxId: prepared.transactionId,
              nativeTxCompactCbor: prepared.nativeTxCompactCbor,
              addrTxWits: prepared.evidence.addrTxWits,
              witnessSet: prepared.witnessSetCompact,
              anchorWitnessSetHash: prepared.verifiedWitnessSetHash,
              owner: signer.paymentKeyHash,
            });
      const referenceScriptUtxo = certificateReference;
      if (referenceScriptUtxo === undefined)
        throw new Error(
          "missing-signature installed forced path omitted certificate mint reference",
        );
      return {
        planned,
        compactCbor: prepared.nativeTxCompactCbor,
        witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact({
          addrTxWitsHash: Buffer.from(
            prepared.witnessSetCompact.addr_tx_wits_hash,
            "hex",
          ),
          scriptTxWitsHash: Buffer.from(
            prepared.witnessSetCompact.script_tx_wits_hash,
            "hex",
          ),
          redeemerTxWitsHash: Buffer.from(
            prepared.witnessSetCompact.redeemer_tx_wits_hash,
            "hex",
          ),
        }).toString("hex"),
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo,
        },
      };
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await transactionConfirmed({ headerHash, txHash }),
  });
};

export const runOrResumeManifestBoundMissingSignatureWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMissingSignatureWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["missingSignature"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export const unsafeCreateMissingSignatureTransactionPortForTest = (input: {
  readonly config: BoundMissingSignatureTransactionsConfig;
  readonly builders: MissingSignatureBuilderSet;
}): MissingSignatureTransactionPort => createBoundTransactionPort(input);
