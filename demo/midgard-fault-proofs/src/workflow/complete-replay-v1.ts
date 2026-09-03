import { createHash } from "node:crypto";

import {
  decodeMidgardAddressBytes,
  decodeMidgardAddressWitnessFieldPreimage,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxProofSource,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_DECODABILITY_VIOLATION_ID,
  canonicalDecodabilityEvidenceFromCommittedField,
  COMMITTED_FIELD_SHAPE_VIOLATION_ID,
  committedWithdrawalKeyBytes,
  decodeAddressWitnessPreimage,
  DOUBLE_WITHDRAW_VIOLATION_ID,
  EMPTY_MERKLE_TREE_ROOT,
  type EvidenceProvenance,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
  INPUT_NO_IDX_VIOLATION_ID,
  INVALID_SIGNATURE_VIOLATION_ID,
  invalidRangeViolationReason,
  isPayableWithdrawalLeaf,
  isWithdrawnInputViolation,
  MIN_ADA_VIOLATION_ID,
  MIN_FEE_VIOLATION_ID,
  minimumFeeFromProofSource,
  MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID,
  MISSING_SIGNATURE_VIOLATION_ID,
  missingNativeScriptIsAbsent,
  missingSignatureVkeyHash,
  NATIVE_SCRIPT_INVALID_VIOLATION_ID,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
  REFERENCE_INPUT_NO_IDX_VIOLATION_ID,
  type VerdictSubject,
  verifyAddressWitness,
  WITHDRAWN_INPUT_VIOLATION_ID,
  WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import { classifyCommittedFieldShapeFields } from "../committed-field-shape/prepare-committed-field-shape-v1.js";
import { detectDistinctAssetAccumulationCanonicalViolations } from "../distinct-asset-accumulation-limit/production-replay-v1.js";
import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../evidence/canonical-block-evidence-v1.js";
import { detectExecutionNativeScriptInvalidCanonicalViolations } from "../execution-native-script-invalid/production-replay-v1.js";
import { detectExecutionSourceScriptDecodingCanonicalViolations } from "../execution-source-script-decoding/production-replay-v1.js";
import { detectFieldItemWidthIllegalCompleteReplay } from "../field-item-width-illegal/production-workflow-v1.js";
import { detectFieldPreimageLengthCompleteReplay } from "../field-preimage-length-mismatch/production-evidence-v1.js";
import { detectInputSetUniquenessForcedReplay } from "../input-set-uniqueness/replay-v1.js";
import {
  INPUT_SET_UNIQUENESS_VIOLATION_ID,
  scanInputSetUniqueness,
} from "../input-set-uniqueness/scan-v1.js";
import { detectInvalidRangeForcedReplay } from "../invalid-range/replay-v1.js";
import { detectMintDeclaredAssetLimitForcedReplay } from "../mint-declared-asset-limit/replay-v1.js";
import { detectMissingRedeemerCanonicalViolations } from "../missing-redeemer/production-replay-v1.js";
import { detectMissingScriptSourceCanonicalViolations } from "../missing-script-source/production-replay-v1.js";
import { ledgerKeyBytesHex } from "../ne-submit-step-03.js";
import { findNetworkIdFaults } from "../network-id/evidence-v1.js";
import { detectObserverOrderInvalidCompleteReplay } from "../observer-order-invalid/replay-v1.js";
import { detectObserversForbiddenForcedReplay } from "../observers-forbidden-on-untagged-network/replay-v1.js";
import { detectOutputReferenceScriptDecodingCanonicalViolations } from "../output-reference-script-decoding/output-reference-script-decoding-v1.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.js";
import { detectInputNoIdxViolationsFromTransactions } from "../prepare-input-no-idx.js";
import { detectReferenceInputNoIdxViolationsFromTransactions } from "../prepare-reference-input-no-idx.js";
import { detectProtectedOutputSignerMissingCompleteReplay } from "../protected-output-signer-missing/protected-output-signer-missing-v1.js";
import { protectedOutputSignerEvidenceIdentity } from "../protected-output-signer-missing/workflow-v1.js";
import { detectReceivePurposeLanguageCanonicalViolations } from "../receive-purpose-language/production-replay-v1.js";
import { detectRedeemerCanonicityCompleteReplay } from "../redeemer-canonicity/production-workflow-v1.js";
import {
  deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus,
  detectResolvedOutputNonCanonicalCompleteReplay,
  resolvedOutputEvidenceIdentity,
} from "../resolved-output-non-canonical/resolved-output-non-canonical-v1.js";
import { detectScriptIntegrityHashMismatchCanonicalViolations } from "../script-integrity-hash-mismatch/production-replay-v1.js";
import { detectScriptIntegrityHashMissingFromCanonicalEvidence } from "../script-integrity-hash-missing/replay-v1.js";
import { detectSpendInputSignerMissingCompleteReplay } from "../spend-input-signer-missing/spend-input-signer-missing-v1.js";
import { spendInputSignerWorkflowEvidenceIdentity } from "../spend-input-signer-missing/workflow-v1.js";
import { detectTransactionOutputNonCanonicalCompleteReplay } from "../transaction-output-non-canonical/production-workflow-v1.js";
import { detectUnusedRedeemerCanonicalViolations } from "../unused-redeemer/production-replay-v1.js";
import { detectUnusedScriptWitnessCanonicalViolations } from "../unused-script-witness/production-replay-v1.js";
import { detectWitnessScriptDecodingCompleteReplay } from "../witness-script-decoding/production-workflow-v1.js";
import { detectZeroInputForcedReplay } from "../zero-input/replay-v1.js";
import {
  type CanonicalViolationDetection,
  DOUBLE_SPEND_VIOLATION_ID,
  NETWORK_ID_VIOLATION_ID,
} from "./classification-v1.js";
import {
  type FabricatedDepositEvidenceAuthority,
  requireFabricatedDepositEvidenceAuthority,
} from "./production-fabricated-deposit-evidence-v1.js";
import {
  type FabricatedWithdrawalEvidenceAuthority,
  requireFabricatedWithdrawalEvidenceAuthority,
} from "./production-fabricated-withdrawal-evidence-v1.js";
import { requireHistoricalNativeScriptCorpus } from "./production-historical-native-script-corpus-v1.js";
import {
  detectMinAdaUtxoFromHistoricalCorpus,
  detectMissingNativeScriptUtxoFromHistoricalCorpus,
  type HistoricalNativeScriptCorpus,
} from "./production-historical-native-script-corpus-v1.js";

export const COMPLETE_CANONICAL_REPLAY =
  "midgard-complete-canonical-replay-v1" as const;
export const COMPLETE_CANONICAL_REPLAY_PREDECESSOR =
  "midgard-complete-canonical-replay-predecessor-v1" as const;
export const COMPLETE_CANONICAL_REPLAY_HISTORICAL_CORPUS =
  "midgard-complete-canonical-replay-historical-corpus-v1" as const;

export type CompleteCanonicalReplayPredecessor = Readonly<{
  schemaVersion: typeof COMPLETE_CANONICAL_REPLAY_PREDECESSOR;
  challengedHeaderHash: string;
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
}>;

export type CompleteCanonicalReplayContext = Readonly<{
  /**
   * Exact public-DA/L1-authenticated predecessor. Required by ledger-relative
   * detectors unless the current header commits the empty genesis ledger.
   */
  predecessor?: CompleteCanonicalReplayPredecessor;
  /** Opaque authority for the complete retained-DA history of this header. */
  historicalCorpus?: CompleteCanonicalReplayHistoricalCorpus;
}>;

export type CompleteCanonicalReplayContextIdentity = Readonly<{
  predecessorHeaderHash?: string;
  predecessorPayloadEnvelopeSha256?: string;
  predecessorPayloadSha256?: string;
  historicalThroughHeaderHash?: string;
  historicalProviderRosterDigest?: string;
  historicalCheckpointDigest?: string;
  historicalCorpusDigest?: string;
}>;

export type CompleteCanonicalReplayHistoricalCorpus = Readonly<{
  schemaVersion: typeof COMPLETE_CANONICAL_REPLAY_HISTORICAL_CORPUS;
  challengedHeaderHash: string;
  throughHeaderHash: string;
  providerRosterDigest: string;
  checkpointDigest: string;
  corpusDigest: string;
}>;

export type CompleteCanonicalReplayDecision = {
  readonly replayVersion: typeof COMPLETE_CANONICAL_REPLAY;
  readonly launchScope: readonly FraudProofCatalogueCategoryName[];
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly context: CompleteCanonicalReplayContextIdentity | null;
  readonly detections: readonly CanonicalViolationDetection[];
};

export interface CompleteCanonicalReplay {
  readonly replayVersion: typeof COMPLETE_CANONICAL_REPLAY;
  readonly launchScope: readonly FraudProofCatalogueCategoryName[];
  replay(
    evidence: CanonicalBlockEvidence,
    context?: CompleteCanonicalReplayContext,
  ): Promise<CompleteCanonicalReplayDecision>;
}

const admittedReplayers = new WeakSet<object>();
const admittedDecisions = new WeakSet<object>();
const predecessorEvidenceByAuthority = new WeakMap<
  object,
  CanonicalBlockEvidence
>();
const historicalCorpusByAuthority = new WeakMap<
  object,
  HistoricalNativeScriptCorpus
>();

export const admitCompleteCanonicalReplayHistoricalCorpus = ({
  evidence,
  corpus,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly corpus: HistoricalNativeScriptCorpus;
}): CompleteCanonicalReplayHistoricalCorpus => {
  const admitted = requireHistoricalNativeScriptCorpus(corpus);
  if (
    admitted.currentEvidence !== evidence ||
    corpus.throughHeaderHash !== evidence.headerHash
  ) {
    throw new Error(
      "historical replay corpus belongs to another challenged header",
    );
  }
  const authority: CompleteCanonicalReplayHistoricalCorpus = Object.freeze({
    schemaVersion: COMPLETE_CANONICAL_REPLAY_HISTORICAL_CORPUS,
    challengedHeaderHash: evidence.headerHash,
    throughHeaderHash: corpus.throughHeaderHash,
    providerRosterDigest: corpus.providerRosterDigest,
    checkpointDigest: corpus.checkpointDigest,
    corpusDigest: corpus.corpusDigest,
  });
  historicalCorpusByAuthority.set(authority, corpus);
  return authority;
};

const requireReplayHistoricalCorpus = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): HistoricalNativeScriptCorpus => {
  const authority = context?.historicalCorpus;
  const corpus =
    authority === undefined
      ? undefined
      : historicalCorpusByAuthority.get(authority);
  if (
    authority === undefined ||
    corpus === undefined ||
    authority.schemaVersion !== COMPLETE_CANONICAL_REPLAY_HISTORICAL_CORPUS ||
    authority.challengedHeaderHash !== evidence.headerHash ||
    authority.throughHeaderHash !== corpus.throughHeaderHash ||
    authority.providerRosterDigest !== corpus.providerRosterDigest ||
    authority.checkpointDigest !== corpus.checkpointDigest ||
    authority.corpusDigest !== corpus.corpusDigest ||
    requireHistoricalNativeScriptCorpus(corpus).currentEvidence !== evidence
  ) {
    throw new Error(
      "complete replay historical corpus was not admitted for this challenged header",
    );
  }
  return corpus;
};

const replayContextIdentity = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CompleteCanonicalReplayContextIdentity | null => {
  const predecessor = requireReplayPredecessorEvidence({ evidence, context });
  const historical = context?.historicalCorpus;
  if (predecessor === undefined && historical === undefined) return null;
  if (historical !== undefined) {
    requireReplayHistoricalCorpus({ evidence, context });
  }
  return Object.freeze({
    ...(predecessor === undefined
      ? {}
      : {
          predecessorHeaderHash: predecessor.headerHash,
          predecessorPayloadEnvelopeSha256: predecessor.payloadEnvelopeSha256,
          predecessorPayloadSha256: predecessor.payloadSha256,
        }),
    ...(historical === undefined
      ? {}
      : {
          historicalThroughHeaderHash: historical.throughHeaderHash,
          historicalProviderRosterDigest: historical.providerRosterDigest,
          historicalCheckpointDigest: historical.checkpointDigest,
          historicalCorpusDigest: historical.corpusDigest,
        }),
  });
};

const predecessorRecord = (
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

/**
 * Re-admits exact untrusted predecessor bytes through the canonical L1/DA
 * evidence constructor and returns a non-revivable replay authority.
 */
export const admitCompleteCanonicalReplayPredecessor = async ({
  value,
  currentEvidence,
  minimumConfirmationDepth,
}: {
  readonly value: unknown;
  readonly currentEvidence: CanonicalBlockEvidence;
  readonly minimumConfirmationDepth: number;
}): Promise<CompleteCanonicalReplayPredecessor> => {
  const parsed = predecessorRecord(value, "raw predecessor context");
  if (
    Object.keys(parsed).sort().join(",") !==
    "daProvenance,observation,payloadEnvelopeCborHex"
  ) {
    throw new Error("raw predecessor context has missing or unknown fields");
  }
  if (
    typeof parsed.payloadEnvelopeCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(parsed.payloadEnvelopeCborHex)
  ) {
    throw new Error(
      "raw predecessor payload envelope must be canonical lowercase byte hex",
    );
  }
  const predecessor = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: parsed.observation as AuthenticatedStateQueueHeaderObservation,
    payloadEnvelopeCbor: Buffer.from(parsed.payloadEnvelopeCborHex, "hex"),
    daProvenance: parsed.daProvenance as EvidenceProvenance,
    minimumConfirmationDepth,
  });
  if (
    currentEvidence.header.prevHeaderHash !== predecessor.headerHash ||
    currentEvidence.header.prevUtxosRoot !== predecessor.header.utxosRoot
  ) {
    throw new Error(
      "raw predecessor does not match the challenged header's prev_header_hash and prev_utxos_root",
    );
  }
  const authority: CompleteCanonicalReplayPredecessor = Object.freeze({
    schemaVersion: COMPLETE_CANONICAL_REPLAY_PREDECESSOR,
    challengedHeaderHash: currentEvidence.headerHash,
    headerHash: predecessor.headerHash,
    payloadEnvelopeSha256: predecessor.payloadEnvelopeSha256,
    payloadSha256: predecessor.payloadSha256,
  });
  predecessorEvidenceByAuthority.set(authority, predecessor);
  return authority;
};

const requireReplayPredecessorEvidence = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CanonicalBlockEvidence | undefined => {
  if (context?.predecessor === undefined) return undefined;
  const predecessor = predecessorEvidenceByAuthority.get(context.predecessor);
  if (
    predecessor === undefined ||
    context.predecessor.schemaVersion !==
      COMPLETE_CANONICAL_REPLAY_PREDECESSOR ||
    context.predecessor.challengedHeaderHash !== evidence.headerHash
  ) {
    throw new Error(
      "complete replay predecessor was not admitted for this challenged header",
    );
  }
  return predecessor;
};

/**
 * Returns only the exact predecessor evidence retained behind an admitted
 * replay authority. Production family preparers use this instead of accepting
 * caller-supplied predecessor payloads or reconstructed ledger roots.
 */
export const completeCanonicalReplayPredecessorEvidence = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CanonicalBlockEvidence | undefined =>
  requireReplayPredecessorEvidence({ evidence, context });

type CanonicalReplayJson =
  | null
  | string
  | readonly CanonicalReplayJson[]
  | { readonly [key: string]: CanonicalReplayJson };

const canonicalizeReplayJson = (
  value: CanonicalReplayJson,
): CanonicalReplayJson => {
  if (Array.isArray(value)) return value.map(canonicalizeReplayJson);
  if (typeof value !== "object" || value === null) return value;
  return Object.freeze(
    Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
        .map(([key, child]) => [key, canonicalizeReplayJson(child)]),
    ),
  );
};

const replayDecisionJson = (
  replay: CompleteCanonicalReplayDecision,
): CanonicalReplayJson => ({
  replayVersion: replay.replayVersion,
  launchScope: replay.launchScope,
  headerHash: replay.headerHash,
  payloadEnvelopeSha256: replay.payloadEnvelopeSha256,
  payloadSha256: replay.payloadSha256,
  context: replay.context,
  detections: replay.detections.map((detection) => ({
    detectionId: detection.detectionId,
    headerHash: detection.headerHash,
    violationId: detection.violationId,
    position: detection.position.toString(),
    diagnostic: detection.diagnostic ?? null,
  })),
});

const outputReferenceKey = (input: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string => `${input.transactionId}#${input.outputIndex.toString()}`;

const detectDoubleSpends = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const firstSpendByInput = new Map<
    string,
    { readonly transactionIndex: number; readonly transactionId: string }
  >();
  const detections: CanonicalViolationDetection[] = [];
  for (const [transactionIndex, transaction] of transactions.entries()) {
    const seenInTransaction = new Set<string>();
    for (const [inputIndex, input] of transaction.inputs.entries()) {
      const inputKey = outputReferenceKey(input);
      if (seenInTransaction.has(inputKey)) continue;
      seenInTransaction.add(inputKey);
      const first = firstSpendByInput.get(inputKey);
      if (first === undefined) {
        firstSpendByInput.set(inputKey, {
          transactionIndex,
          transactionId: transaction.nodeTxId,
        });
        continue;
      }
      if (first.transactionId === transaction.nodeTxId) continue;
      detections.push({
        detectionId: [
          DOUBLE_SPEND_VIOLATION_ID,
          first.transactionIndex.toString(),
          transactionIndex.toString(),
          inputIndex.toString(),
          inputKey,
        ].join(":"),
        headerHash: evidence.headerHash,
        violationId: DOUBLE_SPEND_VIOLATION_ID,
        position: BigInt(transactionIndex),
        diagnostic: `transactions ${first.transactionId} and ${transaction.nodeTxId} spend ${inputKey}`,
      });
    }
  }
  return detections;
};

const PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID =
  "authenticated-predecessor-context-unavailable" as const;

const predecessorLedgerKeys = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): ReadonlySet<string> | null => {
  if (evidence.header.prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT) {
    if (context?.predecessor !== undefined) {
      throw new Error(
        "genesis-ledger replay received a predecessor that the challenged header does not commit",
      );
    }
    return new Set<string>();
  }
  const predecessor = requireReplayPredecessorEvidence({
    evidence,
    context,
  });
  if (predecessor === undefined) return null;
  if (
    evidence.header.prevHeaderHash !== predecessor.headerHash ||
    evidence.header.prevUtxosRoot !== predecessor.header.utxosRoot
  ) {
    throw new Error(
      "canonical replay predecessor differs from the challenged prev_header_hash or prev_utxos_root",
    );
  }
  return new Set(
    predecessor.reconstruction.utxos.map((entry) =>
      Buffer.from(entry.key).toString("hex"),
    ),
  );
};

const detectLedgerRelativeMissingInputs = async ({
  evidence,
  context,
  kind,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
  readonly kind: "spend" | "reference";
}): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const currentTransactionIds = new Set(
    transactions.map((transaction) => transaction.nodeTxId),
  );
  const predecessorLedger = predecessorLedgerKeys({ evidence, context });
  const violationId =
    kind === "spend" ? "non-existent-input" : "no-reference-input";
  const detections: CanonicalViolationDetection[] = [];
  for (const [transactionIndex, transaction] of transactions.entries()) {
    if (transaction.nativeTxCompact.validity_code !== 0n) continue;
    const inputs =
      kind === "spend" ? transaction.inputs : transaction.referenceInputs;
    for (const [inputIndex, input] of inputs.entries()) {
      if (currentTransactionIds.has(input.transactionId)) continue;
      const inputKey = ledgerKeyBytesHex({
        tx_id: input.transactionId,
        output_index: input.outputIndex,
      });
      if (predecessorLedger === null) {
        detections.push({
          detectionId: `${PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID}:${kind}:${transactionIndex.toString()}:${inputIndex.toString()}:${transaction.nodeTxId}:${inputKey}`,
          headerHash: evidence.headerHash,
          violationId: PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID,
          position: BigInt(transactionIndex),
          diagnostic: `${kind} input ${inputIndex.toString()} requires the exact authenticated predecessor ledger before classification`,
        });
      } else if (!predecessorLedger.has(inputKey)) {
        detections.push({
          detectionId: `${violationId}:${transactionIndex.toString()}:${inputIndex.toString()}:${transaction.nodeTxId}:${inputKey}`,
          headerHash: evidence.headerHash,
          violationId,
          position: BigInt(transactionIndex),
          diagnostic: `accepted transaction ${transaction.nodeTxId} ${kind} input ${inputIndex.toString()} is absent from both the current transaction set and predecessor ledger`,
        });
      }
    }
  }
  return detections;
};

const detectNetworkIds = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  evidence.transactions.flatMap((transaction, transactionIndex) =>
    findNetworkIdFaults({
      evidence: {
        source: "retained-da",
        evidenceSourceId: evidence.provenance.da.sourceId,
        nativeTxCanonicalCbor: transaction.txCbor,
      },
      expectedNetworkId: evidence.header.expectedNetworkId,
    }).map((fault) => ({
      detectionId:
        fault.kind === "transaction-network"
          ? `${NETWORK_ID_VIOLATION_ID}:${transactionIndex.toString()}:transaction`
          : `${NETWORK_ID_VIOLATION_ID}:${transactionIndex.toString()}:output:${fault.outputIndex.toString()}`,
      headerHash: evidence.headerHash,
      violationId: NETWORK_ID_VIOLATION_ID,
      position: BigInt(transactionIndex),
      diagnostic:
        fault.kind === "transaction-network"
          ? `transaction ${transaction.nodeTxId} carries the wrong explicit network id`
          : `transaction ${transaction.nodeTxId} output ${fault.outputIndex.toString()} carries the wrong network id`,
    })),
  );

const detectInvalidRanges = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const accepted = transactions.flatMap((transaction, transactionIndex) => {
    const normalizedRange = normalizeNativeTxValidityRange(
      transaction.nativeTxCompact.body,
    );
    const reason = invalidRangeViolationReason({
      blockSlot: evidence.header.blockSlot,
      normalizedRange,
    });
    return reason === null
      ? []
      : [
          {
            detectionId: `invalid-range:${transactionIndex.toString()}:${transaction.nodeTxId}:${reason}`,
            headerHash: evidence.headerHash,
            violationId: "invalid-range",
            position: BigInt(transactionIndex),
            diagnostic: `transaction ${transaction.nodeTxId} excludes the committed block slot: ${reason}`,
          },
        ];
  });
  const forced = detectInvalidRangeForcedReplay(evidence).map((detection) => ({
    detectionId: detection.detectionId,
    headerHash: detection.headerHash,
    violationId: detection.violationId,
    position: detection.position,
    diagnostic: `forced transaction at index ${detection.forcedIndex.toString()} was rejected for a typed invalid-range reason despite its authenticated validity range contradicting that rejection`,
  }));
  return [...accepted, ...forced];
};

const detectZeroInputs = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const accepted = transactions.flatMap((transaction, transactionIndex) =>
    nativeTxBodyHasZeroInputViolation({
      txBody: transaction.nativeTxCompact.body,
    })
      ? [
          {
            detectionId: `zero-input:${transactionIndex.toString()}:${transaction.nodeTxId}`,
            headerHash: evidence.headerHash,
            violationId: "zero-input",
            position: BigInt(transactionIndex),
            diagnostic: `transaction ${transaction.nodeTxId} has no spending inputs`,
          },
        ]
      : [],
  );
  const forced = detectZeroInputForcedReplay(evidence).map((detection) => ({
    detectionId: detection.detectionId,
    headerHash: detection.headerHash,
    violationId: detection.violationId,
    position: detection.position,
    diagnostic: `forced transaction ${detection.transactionId} was rejected for EmptyInputs despite carrying authenticated spending inputs`,
  }));
  return [...accepted, ...forced];
};

const detectL2TxMistags = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) =>
    transaction.nativeTxCompact.validity_code === 1n
      ? [
          {
            detectionId: `l2-tx-mistag:${transactionIndex.toString()}:${transaction.nodeTxId}:1`,
            headerHash: evidence.headerHash,
            violationId: "l2-tx-mistag",
            position: BigInt(transactionIndex),
            diagnostic: `normal transactions-root leaf ${transaction.nodeTxId} is mistagged with validity code 1`,
          },
        ]
      : [],
  );
};

const detectMinFees = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    const fee = transaction.nativeTx.body.fee;
    const { minimumFee } = minimumFeeFromProofSource({
      source: deriveMidgardNativeTxProofSource(transaction.nativeTx),
      minFeeA: evidence.header.minFeeA,
      minFeeB: evidence.header.minFeeB,
    });
    return fee < minimumFee
      ? [
          {
            detectionId: `${MIN_FEE_VIOLATION_ID}:${transactionIndex.toString()}:${transaction.nodeTxId}:${fee.toString()}:${minimumFee.toString()}`,
            headerHash: evidence.headerHash,
            violationId: MIN_FEE_VIOLATION_ID,
            position: BigInt(transactionIndex),
            diagnostic: `transaction ${transaction.nodeTxId} pays ${fee.toString()} below exact minimum ${minimumFee.toString()}`,
          },
        ]
      : [];
  });
};

const detectCommittedFieldShape = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  evidence.transactions.flatMap((transaction, transactionIndex) => {
    const canonical = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(transaction.txCbor, "hex"),
    );
    return classifyCommittedFieldShapeFields(canonical)
      .filter(({ evidence: fieldEvidence }) => fieldEvidence.isViolation)
      .map(({ fieldIndex, evidence: fieldEvidence }) => {
        if (fieldEvidence.badTxId !== transaction.nodeTxId) {
          throw new Error(
            "committed-field-shape replay transaction id differs from canonical evidence",
          );
        }
        return {
          detectionId: `${COMMITTED_FIELD_SHAPE_VIOLATION_ID}:${transactionIndex.toString()}:${transaction.nodeTxId}:${fieldIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: COMMITTED_FIELD_SHAPE_VIOLATION_ID,
          position: BigInt(transactionIndex),
          diagnostic: `transaction ${transaction.nodeTxId} committed malformed field ${fieldIndex.toString()}`,
        };
      });
  });

const detectCanonicalDecodability = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  evidence.transactions.flatMap((transaction, transactionIndex) => {
    const canonical = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(transaction.txCbor, "hex"),
    );
    return classifyCommittedFieldShapeFields(canonical).flatMap(
      ({ fieldIndex, preimage }) => {
        const fieldEvidence = canonicalDecodabilityEvidenceFromCommittedField({
          badTxId: transaction.nodeTxId,
          fieldIndex,
          committedPreimage: preimage,
        });
        return fieldEvidence.isViolation
          ? [
              {
                detectionId: `${CANONICAL_DECODABILITY_VIOLATION_ID}:${transactionIndex.toString()}:${transaction.nodeTxId}:${fieldIndex.toString()}:${fieldEvidence.verdict.toString()}`,
                headerHash: evidence.headerHash,
                violationId: CANONICAL_DECODABILITY_VIOLATION_ID,
                position: BigInt(transactionIndex),
                diagnostic: `transaction ${transaction.nodeTxId} committed non-canonical field ${fieldIndex.toString()} with verdict ${fieldEvidence.verdict.toString()}`,
              },
            ]
          : [];
      },
    );
  });

const canonicalSignerHashes = (
  preimageCbor: Uint8Array,
  label: string,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, label).map(
    (bytes, index) => {
      if (bytes.length !== 28) {
        throw new Error(
          `${label}[${index.toString()}] is ${bytes.length.toString()} bytes, expected 28`,
        );
      }
      return Buffer.from(bytes).toString("hex");
    },
  );

/**
 * Complete Phase-A required-signer scan over every normal transaction leaf.
 *
 * The normal `transactions_root` contains the transactions the operator
 * accepted into the block. Forced transactions have their own root and their
 * adjudicated validity is handled by the forced/validation-trace surface. A
 * required key is present only when field 7 contains that exact verification
 * key hash; signature validity itself belongs to `invalidSignature`.
 */
const detectMissingSignatures = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    const requiredSignerHashes = canonicalSignerHashes(
      transaction.nativeTx.body.requiredSignersPreimageCbor,
      `transaction ${transaction.nodeTxId} required_signers`,
    );
    const witnessSignerHashes = new Set(
      decodeAddressWitnessPreimage(
        transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
      ).map((witness) => missingSignatureVkeyHash(witness.verification_key)),
    );
    return requiredSignerHashes.flatMap((requiredSignerHash, signerIndex) =>
      witnessSignerHashes.has(requiredSignerHash)
        ? []
        : [
            {
              detectionId: `${MISSING_SIGNATURE_VIOLATION_ID}:${transactionIndex.toString()}:${signerIndex.toString()}:${transaction.nodeTxId}:${requiredSignerHash}`,
              headerHash: evidence.headerHash,
              violationId: MISSING_SIGNATURE_VIOLATION_ID,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} is missing required signer ${requiredSignerHash} at ordinal ${signerIndex.toString()}`,
            },
          ],
    );
  });
};

/**
 * Complete same-block accepted-input scan for a missing script witness.
 *
 * The proof family authenticates both the spending and producing transaction
 * through the accused header's transactions root, so only same-block
 * producers are executable by this family. The native-script preimage is
 * deliberately not accepted here: the production transaction port resolves
 * it from authenticated finalized L1 history after this detector identifies
 * the exact credential hash.
 */
const detectMissingNativeScriptTransactions = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const producerById = new Map(
    transactions.map((transaction, transactionIndex) => [
      transaction.nodeTxId,
      { transaction, transactionIndex },
    ]),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    const scriptWitnessItems = decodeMidgardNativeByteListPreimage(
      transaction.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
      `transaction ${transaction.nodeTxId} script witnesses`,
    );
    return transaction.inputs.flatMap((input, inputIndex) => {
      const producer = producerById.get(input.transactionId);
      if (
        producer === undefined ||
        producer.transaction.nativeTxCompact.validity_code !== 0n
      ) {
        return [];
      }
      const outputItems = decodeMidgardNativeByteListPreimage(
        producer.transaction.nativeTx.body.outputsPreimageCbor,
        `transaction ${producer.transaction.nodeTxId} outputs`,
      );
      if (
        input.outputIndex < 0n ||
        input.outputIndex > BigInt(Number.MAX_SAFE_INTEGER)
      ) {
        return [];
      }
      const outputItem = outputItems[Number(input.outputIndex)];
      if (outputItem === undefined) return [];
      const credential = decodeMidgardAddressBytes(
        decodeMidgardTxOutput(outputItem).address,
      ).paymentCredential;
      if (credential.kind !== "Script") return [];
      const expectedScriptHash = credential.hash.toString("hex");
      if (
        !missingNativeScriptIsAbsent({
          scriptTxWitsItems: scriptWitnessItems,
          expectedMissingScriptHash: expectedScriptHash,
        })
      ) {
        return [];
      }
      return [
        {
          detectionId: `${MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID}:${transactionIndex.toString()}:${inputIndex.toString()}:${producer.transactionIndex.toString()}:${input.outputIndex.toString()}:${transaction.nodeTxId}:${producer.transaction.nodeTxId}:${expectedScriptHash}`,
          headerHash: evidence.headerHash,
          violationId: MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID,
          position: BigInt(transactionIndex),
          diagnostic: `accepted transaction ${transaction.nodeTxId} input ${inputIndex.toString()} spends same-block script output ${producer.transaction.nodeTxId}#${input.outputIndex.toString()} without witness ${expectedScriptHash}`,
        },
      ];
    });
  });
};

/** Complete positional scan of every committed address witness. */
const detectInvalidSignatures = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) =>
    decodeAddressWitnessPreimage(
      transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
    ).flatMap((witness, witnessIndex) =>
      verifyAddressWitness({ txId: transaction.nodeTxId, witness })
        ? []
        : [
            {
              detectionId: `${INVALID_SIGNATURE_VIOLATION_ID}:${transactionIndex.toString()}:${witnessIndex.toString()}:${transaction.nodeTxId}:${witness.verification_key}`,
              headerHash: evidence.headerHash,
              violationId: INVALID_SIGNATURE_VIOLATION_ID,
              position: BigInt(transactionIndex),
              diagnostic: `transaction ${transaction.nodeTxId} carries invalid address witness ${witnessIndex.toString()} for verification key ${witness.verification_key}`,
            },
          ],
    ),
  );
};

/** Complete evaluation of every well-formed native witness in accepted txs. */
const detectNativeScriptInvalid = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    const signers = new Set(
      decodeMidgardAddressWitnessFieldPreimage(
        transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
      ).map((witness) =>
        missingSignatureVkeyHash(
          Buffer.from(witness.verificationKey).toString("hex"),
        ),
      ),
    );
    const start = transaction.nativeTx.body.validityIntervalStart;
    const end = transaction.nativeTx.body.validityIntervalEnd;
    return decodeMidgardFieldPreimage(
      transaction.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
    ).flatMap((item, scriptIndex) => {
      const script = decodeMidgardVersionedScript(item);
      if (
        script.language !== "NativeCardano" ||
        verifyMidgardNativeScript(script.nativeScript, {
          validityIntervalStart:
            start === MIDGARD_POSIX_TIME_NONE ? undefined : start,
          validityIntervalEnd:
            end === MIDGARD_POSIX_TIME_NONE ? undefined : end,
          witnessSigners: signers,
        })
      ) {
        return [];
      }
      return [
        {
          detectionId: `${NATIVE_SCRIPT_INVALID_VIOLATION_ID}:${transaction.nodeTxId}:${scriptIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: NATIVE_SCRIPT_INVALID_VIOLATION_ID,
          position: BigInt(transactionIndex),
          diagnostic: `accepted transaction ${transaction.nodeTxId} carries false native witness ${scriptIndex.toString()}`,
        },
      ];
    });
  });
};

const descriptorIsBelowMinAda = (descriptorCbor: Uint8Array): boolean => {
  const descriptor = decodeMidgardLedgerOutputCommitment(descriptorCbor);
  return !outputMeetsMinAda(
    MIDGARD_COINS_PER_UTXO_BYTE,
    BigInt(descriptor.totalLength),
    descriptor.lovelace,
  );
};

/** Complete MIN-ADA-TX and introducing-transition MIN-ADA-UTXO scan. */
const detectMinAda = async (
  evidence: CanonicalBlockEvidence,
  context: CompleteCanonicalReplayContext | undefined,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const txDetections = transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    return decodeMidgardFieldPreimage(
      transaction.nativeTx.body.outputsPreimageCbor,
    ).flatMap((output, outputIndex) => {
      const descriptor = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex,
        outputCbor: output,
      }).descriptorCbor;
      return descriptorIsBelowMinAda(descriptor)
        ? [
            {
              detectionId: `${MIN_ADA_VIOLATION_ID}:tx:${transaction.nodeTxId}:${outputIndex.toString()}`,
              headerHash: evidence.headerHash,
              violationId: MIN_ADA_VIOLATION_ID,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} output ${outputIndex.toString()} is below the exact min-Ada floor`,
            },
          ]
        : [];
    });
  });
  const predecessor = requireReplayPredecessorEvidence({
    evidence,
    context,
  });
  if (
    predecessor === undefined &&
    evidence.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT
  ) {
    return [
      ...txDetections,
      {
        detectionId: `${PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID}:min-ada-utxo:${evidence.headerHash}`,
        headerHash: evidence.headerHash,
        violationId: PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID,
        position: BigInt(transactions.length),
        diagnostic:
          "MIN-ADA-UTXO classification requires the exact authenticated predecessor ledger",
      },
    ];
  }
  const predecessorKeys = new Set(
    (predecessor?.reconstruction.utxos ?? []).map((entry) =>
      Buffer.from(entry.key).toString("hex"),
    ),
  );
  const utxoDetections = evidence.reconstruction.utxos.flatMap(
    (entry, index) => {
      const key = Buffer.from(entry.key).toString("hex");
      if (predecessorKeys.has(key)) return [];
      const material = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: entry.key,
        outputCbor: entry.value,
      });
      if (!descriptorIsBelowMinAda(material.descriptorCbor)) return [];
      const outRef = decodeMidgardSpendInputItem(entry.key);
      const transactionId = Buffer.from(outRef.txId).toString("hex");
      return [
        {
          detectionId: `${MIN_ADA_VIOLATION_ID}:utxo:${transactionId}:${outRef.outputIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: MIN_ADA_VIOLATION_ID,
          position: BigInt(transactions.length + index),
          diagnostic: `post-state UTxO ${transactionId}#${outRef.outputIndex.toString()} was introduced below the exact min-Ada floor`,
        },
      ];
    },
  );
  return [...txDetections, ...utxoDetections];
};

const detectInputNoIdxViolations = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> =>
  (await detectInputNoIdxViolationsFromTransactions(evidence.transactions)).map(
    (detection) => ({
      detectionId: `${INPUT_NO_IDX_VIOLATION_ID}:${detection.badTxIndex.toString()}:${detection.badInputsIndex.toString()}:${detection.badTxId}:${detection.producingTxId}:${detection.badInputOutputIndex.toString()}:${detection.producingTxOutputCount.toString()}`,
      headerHash: evidence.headerHash,
      violationId: INPUT_NO_IDX_VIOLATION_ID,
      position: BigInt(detection.badTxIndex),
      diagnostic: `transaction ${detection.badTxId} input ${detection.badInputsIndex.toString()} names output ${detection.badInputOutputIndex.toString()} beyond same-block producer ${detection.producingTxId}'s ${detection.producingTxOutputCount.toString()} outputs`,
    }),
  );

const detectInputSetUniqueness = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const accepted = transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    const spendInputItemCbors = decodeMidgardNativeByteListPreimage(
      transaction.nativeTx.body.spendInputsPreimageCbor,
      `transaction ${transaction.nodeTxId} spend inputs`,
    ).map((item) => Buffer.from(item).toString("hex"));
    const referenceInputItemCbors = decodeMidgardNativeByteListPreimage(
      transaction.nativeTx.body.referenceInputsPreimageCbor,
      `transaction ${transaction.nodeTxId} reference inputs`,
    ).map((item) => Buffer.from(item).toString("hex"));
    const [claim] = scanInputSetUniqueness({
      spendInputItemCbors,
      referenceInputItemCbors,
    });
    if (claim === undefined) return [];
    const identity =
      claim.kind === "spendReferenceOverlap"
        ? `${claim.kind}:${claim.spendIndex.toString()}:${claim.referenceIndex.toString()}`
        : `${claim.kind}:${claim.firstIndex.toString()}:${claim.secondIndex.toString()}`;
    return [
      {
        detectionId: `${INPUT_SET_UNIQUENESS_VIOLATION_ID}:${transactionIndex.toString()}:${transaction.nodeTxId}:${identity}`,
        headerHash: evidence.headerHash,
        violationId: INPUT_SET_UNIQUENESS_VIOLATION_ID,
        position: BigInt(transactionIndex),
        diagnostic: `accepted transaction ${transaction.nodeTxId} violates input-set uniqueness via ${identity}`,
      },
    ];
  });
  const forced = detectInputSetUniquenessForcedReplay(evidence).map(
    (detection) => ({
      detectionId: detection.detectionId,
      headerHash: detection.headerHash,
      violationId: detection.violationId,
      position: detection.position,
      diagnostic: `forced transaction ${detection.transactionId} was rejected for DuplicateInput despite a complete authenticated strictly increasing input union`,
    }),
  );
  return [...accepted, ...forced];
};

/**
 * Complete same-block scan of every accepted spend input against every
 * authenticated withdrawal leaf. Both roots are reconstructed from the one
 * retained-DA payload before this detector runs.
 */
const detectWithdrawnInputs = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    return transaction.inputs.flatMap((input, inputIndex) =>
      evidence.reconstruction.withdrawals.flatMap(
        (withdrawal, withdrawalIndex) => {
          if (
            !isWithdrawnInputViolation({
              input: {
                tx_id: input.transactionId,
                output_index: input.outputIndex,
              },
              withdrawal: withdrawal.value,
            })
          ) {
            return [];
          }
          return [
            {
              detectionId: `${WITHDRAWN_INPUT_VIOLATION_ID}:${transactionIndex.toString()}:${inputIndex.toString()}:${withdrawalIndex.toString()}:${transaction.nodeTxId}:${committedWithdrawalKeyBytes(withdrawal.key)}`,
              headerHash: evidence.headerHash,
              violationId: WITHDRAWN_INPUT_VIOLATION_ID,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} spend input ${inputIndex.toString()} consumes the valid withdrawal leaf at ordinal ${withdrawalIndex.toString()}`,
            },
          ];
        },
      ),
    );
  });
};

/** Complete same-block accepted reference-input/withdrawal intersection. */
const detectWithdrawnReferenceInputs = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    if (transaction.nativeTxCompact.validity_code !== 0n) return [];
    return transaction.referenceInputs.flatMap((input, inputIndex) =>
      evidence.reconstruction.withdrawals.flatMap(
        (withdrawal, withdrawalIndex) => {
          const outRef = withdrawal.value.body.l2_outref;
          if (
            withdrawal.value.validity !== "WithdrawalIsValid" ||
            input.transactionId !== outRef.transactionId ||
            input.outputIndex !== outRef.outputIndex
          ) {
            return [];
          }
          return [
            {
              detectionId: `${WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID}:${transactionIndex.toString()}:${inputIndex.toString()}:${withdrawalIndex.toString()}:${transaction.nodeTxId}:${committedWithdrawalKeyBytes(withdrawal.key)}`,
              headerHash: evidence.headerHash,
              violationId: WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} reference input ${inputIndex.toString()} names the valid withdrawal leaf at ordinal ${withdrawalIndex.toString()}`,
            },
          ];
        },
      ),
    );
  });
};

const detectReferenceInputNoIdxViolations = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  detectReferenceInputNoIdxViolationsFromTransactions(
    evidence.transactions,
  ).map((detection) => ({
    detectionId: `${REFERENCE_INPUT_NO_IDX_VIOLATION_ID}:${detection.badTxIndex.toString()}:${detection.badReferenceInputIndex.toString()}:${detection.badTxId}:${detection.producingTxId}:${detection.badReferenceInputOutputIndex.toString()}:${detection.producingTxOutputCount.toString()}`,
    headerHash: evidence.headerHash,
    violationId: REFERENCE_INPUT_NO_IDX_VIOLATION_ID,
    position: BigInt(detection.badTxIndex),
    diagnostic: `transaction ${detection.badTxId} reference input ${detection.badReferenceInputIndex.toString()} names output ${detection.badReferenceInputOutputIndex.toString()} beyond same-block producer ${detection.producingTxId}'s ${detection.producingTxOutputCount.toString()} outputs`,
  }));

const sameOutputReference = (
  left: {
    readonly transactionId: string;
    readonly outputIndex: bigint;
  },
  right: {
    readonly transactionId: string;
    readonly outputIndex: bigint;
  },
): boolean =>
  left.transactionId.toLowerCase() === right.transactionId.toLowerCase() &&
  left.outputIndex === right.outputIndex;

/**
 * Complete same-block scan for two distinct payable withdrawal leaves which
 * drain the same L2 output. The reconstruction has already re-admitted every
 * leaf from public DA against the L1-committed counted withdrawals root.
 */
const detectDoubleWithdraws = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const withdrawals = evidence.reconstruction.withdrawals;
  const detections: CanonicalViolationDetection[] = [];
  for (let firstIndex = 0; firstIndex < withdrawals.length; firstIndex += 1) {
    const first = withdrawals[firstIndex]!;
    if (!isPayableWithdrawalLeaf(first.value)) continue;
    for (
      let secondIndex = firstIndex + 1;
      secondIndex < withdrawals.length;
      secondIndex += 1
    ) {
      const second = withdrawals[secondIndex]!;
      if (
        !isPayableWithdrawalLeaf(second.value) ||
        sameOutputReference(first.key, second.key) ||
        !sameOutputReference(
          first.value.body.l2_outref,
          second.value.body.l2_outref,
        )
      ) {
        continue;
      }
      const firstKey = committedWithdrawalKeyBytes(first.key);
      const secondKey = committedWithdrawalKeyBytes(second.key);
      detections.push({
        detectionId: `${DOUBLE_WITHDRAW_VIOLATION_ID}:${firstIndex.toString()}:${secondIndex.toString()}:${firstKey}:${secondKey}`,
        headerHash: evidence.headerHash,
        violationId: DOUBLE_WITHDRAW_VIOLATION_ID,
        position: BigInt(secondIndex),
        diagnostic: `withdrawal leaves ${firstIndex.toString()} and ${secondIndex.toString()} are both payable for the same L2 output`,
      });
    }
  }
  return detections;
};

const completeReplayer = (
  launchScope: readonly FraudProofCatalogueCategoryName[],
  replay: (
    evidence: CanonicalBlockEvidence,
    context: CompleteCanonicalReplayContext | undefined,
  ) => Promise<readonly CanonicalViolationDetection[]>,
): CompleteCanonicalReplay => {
  const frozenScope = Object.freeze([...launchScope]);
  const replayer: CompleteCanonicalReplay = Object.freeze({
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    launchScope: frozenScope,
    replay: async (
      evidence: CanonicalBlockEvidence,
      context?: CompleteCanonicalReplayContext,
    ) => {
      const contextIdentity = replayContextIdentity({ evidence, context });
      const detections = Object.freeze(
        (await replay(evidence, context)).map((detection) =>
          Object.freeze({ ...detection }),
        ),
      );
      const decision: CompleteCanonicalReplayDecision = Object.freeze({
        replayVersion: COMPLETE_CANONICAL_REPLAY,
        launchScope: frozenScope,
        headerHash: evidence.headerHash,
        payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
        payloadSha256: evidence.payloadSha256,
        context: contextIdentity,
        detections,
      });
      admittedDecisions.add(decision);
      return decision;
    },
  });
  admittedReplayers.add(replayer);
  return replayer;
};

/** Runtime admission for application-installed replay bundles. */
export const requireCompleteCanonicalReplayBundle = (
  replayer: CompleteCanonicalReplay,
): readonly FraudProofCatalogueCategoryName[] => {
  if (
    !admittedReplayers.has(replayer) ||
    replayer.replayVersion !== COMPLETE_CANONICAL_REPLAY
  ) {
    throw new Error(
      "production workflow requires a closed canonical replay bundle",
    );
  }
  return replayer.launchScope;
};

/** Complete replay for the constrained double-spend family surface. */
export const DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["doubleSpend"],
  detectDoubleSpends,
);

/** Complete accepted spend-input scan against current and predecessor state. */
export const NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["nonExistentInput"],
  async (evidence, context) =>
    await detectLedgerRelativeMissingInputs({
      evidence,
      context,
      kind: "spend",
    }),
);

/** Complete replay for every transaction/output covered by the Q35 family. */
export const NETWORK_ID_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["networkId"],
  async (evidence) => detectNetworkIds(evidence),
);

/** Complete replay for the two-step invalid-range family. */
export const INVALID_RANGE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["invalidRange"],
  detectInvalidRanges,
);

/** Complete replay for the two-step zero-input family. */
export const ZERO_INPUT_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["zeroInput"],
  detectZeroInputs,
);

/** Complete accepted reference-input scan against current and predecessor state. */
export const NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["noReferenceInput"],
  async (evidence, context) =>
    await detectLedgerRelativeMissingInputs({
      evidence,
      context,
      kind: "reference",
    }),
);

/**
 * Complete Q44 absence decision after canonical reconstruction succeeds. A
 * malformed source leaf is routed before this replay by the authenticated raw
 * source-leaf branch; reaching canonical evidence proves every source leaf was
 * canonical and key/body-id bound, so the closed detector result is empty.
 */
export const DA_HASH_PREIMAGE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["daHashPreimage"],
  async () => [],
);

/** Complete replay for all nine committed native-transaction field slots. */
export const COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["committedFieldShape"],
  async (evidence) => detectCommittedFieldShape(evidence),
);

/** Complete total-envelope scan over all nine fields of every transaction. */
export const CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["canonicalDecodability"], async (evidence) =>
    detectCanonicalDecodability(evidence),
  );

/** Complete required-signer scan of every committed accepted transaction. */
export const MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["missingSignature"],
  detectMissingSignatures,
);

/** Complete same-block missing-script-witness scan for every accepted input. */
export const MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(
    ["missingNativeScriptTx"],
    detectMissingNativeScriptTransactions,
  );

/**
 * Q33 is history-relative: a script credential alone cannot prove that the
 * preimage is native. This factory admits only the complete retained-history
 * capability derived for the exact challenged block.
 */
export const createMissingNativeScriptUtxoCompleteCanonicalReplay = (
  corpus: HistoricalNativeScriptCorpus,
): CompleteCanonicalReplay =>
  completeReplayer(
    ["missingNativeScriptUtxo"],
    async (evidence) =>
      await detectMissingNativeScriptUtxoFromHistoricalCorpus({
        evidence,
        corpus,
      }),
  );

/** Complete Ed25519 verification of every committed address witness. */
export const INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["invalidSignature"],
  detectInvalidSignatures,
);

/**
 * Complete committed-deposit scan with each candidate classified against the
 * concrete public L1 authority. The returned replayer is opaque-admitted like
 * every fixed replay bundle; structural evidence-authority copies are refused.
 */
export const createFabricatedDepositCompleteCanonicalReplay = ({
  authority,
  owner,
}: {
  readonly authority: FabricatedDepositEvidenceAuthority;
  readonly owner: string;
}): CompleteCanonicalReplay => {
  const admitted = requireFabricatedDepositEvidenceAuthority(authority);
  return completeReplayer(["fabricatedDeposit"], async (evidence) =>
    (await admitted.detect(evidence, owner)).map(({ detection }) => detection),
  );
};

/** Complete committed-withdrawal scan against the concrete public L1 authority. */
export const createFabricatedWithdrawalCompleteCanonicalReplay = ({
  authority,
  owner,
}: {
  readonly authority: FabricatedWithdrawalEvidenceAuthority;
  readonly owner: string;
}): CompleteCanonicalReplay => {
  const admitted = requireFabricatedWithdrawalEvidenceAuthority(authority);
  return completeReplayer(["fabricatedWithdrawal"], async (evidence) =>
    (await admitted.detect(evidence, owner)).map(({ detection }) => detection),
  );
};

/** Complete evaluation of all accepted native script witnesses. */
export const NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["nativeScriptInvalid"],
  detectNativeScriptInvalid,
);

/** Complete transaction-output and introducing post-state min-Ada scan. */
export const MIN_ADA_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["minAda"],
  detectMinAda,
);

/** Complete Q27 replay backed by the same checkpointed predecessor authority as Q33. */
export const createMinAdaCompleteCanonicalReplayFromHistoricalCorpus = (
  corpus: HistoricalNativeScriptCorpus,
): CompleteCanonicalReplay =>
  completeReplayer(["minAda"], async (evidence) => [
    ...(await detectMinAda(evidence, undefined)).filter(
      (detection) => detection.violationId === MIN_ADA_VIOLATION_ID,
    ),
    ...detectMinAdaUtxoFromHistoricalCorpus({ evidence, corpus }),
  ]);

/** Complete same-block input/producer output-count scan. */
export const INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["nonExistentInputNoIndex"],
  detectInputNoIdxViolations,
);

/** Complete same-block reference-input/producer output-count scan. */
export const REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["referenceInputNoIdx"], async (evidence) =>
    detectReferenceInputNoIdxViolations(evidence),
  );

/** Complete scan of every committed withdrawal pair in the accused block. */
export const DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["doubleWithdraw"],
  async (evidence) => detectDoubleWithdraws(evidence),
);

/** Complete scan of every normal transactions-root leaf for code-1 mistags. */
export const L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["l2TxMistag"],
  detectL2TxMistags,
);

/** Complete exact-size/header-schedule fee scan of every transaction leaf. */
export const MIN_FEE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["minFee"],
  detectMinFees,
);

/** Complete input-set scan for every accepted transaction leaf. */
export const INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["inputSetUniqueness"],
  detectInputSetUniqueness,
);

/** Complete scan of forced field-length wrongful-rejection contradictions. */
export const FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["fieldPreimageLengthMismatch"], async (evidence) =>
    detectFieldPreimageLengthCompleteReplay(evidence),
  );

/** Complete scan of every output and mint item for illegal committed width. */
export const FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["fieldItemWidthIllegal"], async (evidence) =>
    detectFieldItemWidthIllegalCompleteReplay(evidence),
  );

/** Complete accepted and forced scan for malformed field-6 native scripts. */
export const WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["witnessScriptDecoding"], async (evidence) =>
    detectWitnessScriptDecodingCompleteReplay(evidence),
  );

/** Complete accepted and forced scan for missing required integrity hashes. */
export const SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["scriptIntegrityHashMissing"], async (evidence) =>
    detectScriptIntegrityHashMissingFromCanonicalEvidence(evidence),
  );

/** Complete accepted and forced scan for non-canonical transaction outputs. */
export const TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["transactionOutputNonCanonical"], async (evidence) =>
    detectTransactionOutputNonCanonicalCompleteReplay(evidence),
  );

/** Complete resolved-input scan backed by opaque authenticated retained history. */
export const RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(
    ["resolvedOutputNonCanonical"],
    async (evidence, context) => {
      const corpus = requireReplayHistoricalCorpus({ evidence, context });
      const priorLedger =
        await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
          block: evidence,
          corpus,
        });
      return detectResolvedOutputNonCanonicalCompleteReplay({
        block: evidence,
        priorLedger,
      }).map((finding) => ({
        detectionId: `resolved-output-non-canonical:${resolvedOutputEvidenceIdentity(finding)}`,
        headerHash: evidence.headerHash,
        violationId: "resolved-output-non-canonical",
        position: verdictSubjectReplayPosition(evidence, finding.subject),
        diagnostic: "authenticated prior ledger output is non-canonical",
      }));
    },
  );

/** Complete exact forced-rejection scan; accepted crossings use the raw route. */
export const MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["mintDeclaredAssetLimit"], async (evidence) =>
    detectMintDeclaredAssetLimitForcedReplay(evidence),
  );

const verdictSubjectReplayPosition = (
  evidence: CanonicalBlockEvidence,
  subject: VerdictSubject,
): bigint => {
  const index =
    subject.source_kind === 0n
      ? evidence.transactions.findIndex(
          (transaction) => transaction.nodeTxId === subject.transaction_id,
        )
      : evidence.reconstruction.forcedTransactions.findIndex(
          (transaction) => transaction.value.tx_id === subject.transaction_id,
        );
  if (index < 0) {
    throw new Error(
      "signer replay finding does not belong to its authenticated transaction frontier",
    );
  }
  return BigInt(index);
};

/** Complete spend-signature scan backed by opaque authenticated retained history. */
export const SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["spendInputSignerMissing"], async (evidence, context) => {
    const corpus = requireReplayHistoricalCorpus({ evidence, context });
    const priorLedger =
      await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
        block: evidence,
        corpus,
      });
    return detectSpendInputSignerMissingCompleteReplay({
      block: evidence,
      priorLedger,
    }).map((finding) => ({
      detectionId: `spend-input-signer-missing:${spendInputSignerWorkflowEvidenceIdentity(finding)}`,
      headerHash: evidence.headerHash,
      violationId: "spend-input-signer-missing",
      position: verdictSubjectReplayPosition(evidence, finding.subject),
      diagnostic: "authenticated spend input has no valid matching key witness",
    }));
  });

/** Complete protected-output signature scan over accepted and exact forced subjects. */
export const PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["protectedOutputSignerMissing"], async (evidence) =>
    detectProtectedOutputSignerMissingCompleteReplay(evidence).map(
      (finding) => ({
        detectionId: `protected-output-signer-missing:${protectedOutputSignerEvidenceIdentity(finding)}`,
        headerHash: evidence.headerHash,
        violationId: "protected-output-signer-missing",
        position: verdictSubjectReplayPosition(evidence, finding.subject),
        diagnostic:
          "authenticated protected output has no valid matching key witness",
      }),
    ),
  );

/** Forced half of the observer rule; accepted crossings use the raw route. */
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["observersForbiddenOnUntaggedNetwork"], async (evidence) =>
    detectObserversForbiddenForcedReplay(evidence),
  );

/** Complete accepted and forced scan for malformed output reference scripts. */
export const OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["outputReferenceScriptDecoding"], async (evidence) =>
    detectOutputReferenceScriptDecodingCanonicalViolations(evidence),
  );

/** Complete accepted and forced scan for malformed execution-source scripts. */
export const EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["executionSourceScriptDecoding"], async (evidence) =>
    detectExecutionSourceScriptDecodingCanonicalViolations(evidence),
  );

/** Complete accepted-false and forced-true native execution replay. */
export const EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(
    ["executionNativeScriptInvalid"],
    async (evidence, context) =>
      detectExecutionNativeScriptInvalidCanonicalViolations({
        block: evidence,
        corpus: requireReplayHistoricalCorpus({ evidence, context }),
      }),
  );

export const OBSERVER_ORDER_INVALID_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["observerOrderInvalid"], async (evidence) =>
    detectObserverOrderInvalidCompleteReplay(evidence),
  );

export const REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["redeemerCanonicity"],
  async (evidence) =>
    detectRedeemerCanonicityCompleteReplay(evidence).map((detection) => ({
      detectionId: detection.detectionId,
      headerHash: detection.headerHash,
      violationId: "redeemer-malformed",
      position: detection.position,
      diagnostic: "authenticated redeemer item is not canonical Plutus Data",
    })),
);

export const RECEIVE_PURPOSE_LANGUAGE_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["receivePurposeLanguage"], async (evidence) =>
    detectReceivePurposeLanguageCanonicalViolations(evidence),
  );

export const UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["unusedScriptWitness"],
  async (evidence) => detectUnusedScriptWitnessCanonicalViolations(evidence),
);

export const MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["missingScriptSource"],
  async (evidence) => detectMissingScriptSourceCanonicalViolations(evidence),
);

/** Complete retained-stage-10 scan for accepted absence and forced presence. */
export const MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["missingRedeemer"],
  async (evidence) => detectMissingRedeemerCanonicalViolations(evidence),
);

/** Complete retained-stage-10 reverse match for every redeemer purpose kind. */
export const UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["unusedRedeemer"],
  async (evidence) => detectUnusedRedeemerCanonicalViolations(evidence),
);

/** Complete accepted-mismatch and forced-equality ScriptIntegrity replay. */
export const SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["scriptIntegrityHashMismatch"], async (evidence) =>
    detectScriptIntegrityHashMismatchCanonicalViolations(evidence),
  );

/** Complete typed input/output/mint distinct-asset accumulation replay. */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["distinctAssetAccumulationLimit"], async (evidence) =>
    detectDistinctAssetAccumulationCanonicalViolations(evidence),
  );

/** Complete accepted-spend/withdrawal intersection scan for one block. */
export const WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["withdrawnInput"],
  detectWithdrawnInputs,
);

/** Complete accepted-reference/withdrawal intersection scan for one block. */
export const WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["withdrawnReferenceInput"], detectWithdrawnReferenceInputs);

/** Closed union used once both family adapters are launch-scope complete. */
export const DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["doubleSpend", "networkId"], async (evidence) => [
    ...(await detectDoubleSpends(evidence)),
    ...detectNetworkIds(evidence),
  ]);

/**
 * Builds one closed replay bundle from already-admitted complete family
 * replayers. The application installs this object, rather than a list of
 * detector callbacks, so no caller can omit a family or inject a partial scan
 * after deployment composition. Categories must be disjoint and in the
 * append-only catalogue order.
 */
export const createCompleteCanonicalReplayUnion = (
  members: readonly CompleteCanonicalReplay[],
): CompleteCanonicalReplay => {
  if (members.length === 0) {
    throw new Error("complete replay union must contain at least one member");
  }
  const categories: FraudProofCatalogueCategoryName[] = [];
  const seen = new Set<FraudProofCatalogueCategoryName>();
  for (const member of members) {
    const memberScope = requireCompleteCanonicalReplayBundle(member);
    for (const category of memberScope) {
      if (seen.has(category)) {
        throw new Error(`complete replay union duplicates ${category}`);
      }
      seen.add(category);
      categories.push(category);
    }
  }
  const canonical = [...categories].sort(
    (left, right) =>
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(left) -
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(right),
  );
  if (canonical.some((category, index) => category !== categories[index])) {
    throw new Error(
      "complete replay union members are not in canonical catalogue order",
    );
  }
  return completeReplayer(
    Object.freeze(categories),
    async (evidence, context) => {
      const detections: CanonicalViolationDetection[] = [];
      for (const member of members) {
        const decision = await member.replay(evidence, context);
        detections.push(
          ...requireCompleteCanonicalReplayDecision({
            evidence,
            replayer: member,
            decision,
            ...(context === undefined ? {} : { context }),
          }),
        );
      }
      return detections;
    },
  );
};

/** Rejects caller-authored or partial-detector decisions at runtime. */
export const requireCompleteCanonicalReplayDecision = ({
  evidence,
  replayer,
  decision,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly replayer: CompleteCanonicalReplay;
  readonly decision: CompleteCanonicalReplayDecision;
  readonly context?: CompleteCanonicalReplayContext;
}): readonly CanonicalViolationDetection[] => {
  requireCompleteCanonicalReplayBundle(replayer);
  if (!admittedDecisions.has(decision)) {
    throw new Error(
      "canonical replay decision was not produced by the closed replay bundle",
    );
  }
  const expectedContext = replayContextIdentity({ evidence, context });
  if (
    decision.replayVersion !== COMPLETE_CANONICAL_REPLAY ||
    decision.launchScope !== replayer.launchScope ||
    decision.headerHash !== evidence.headerHash ||
    decision.payloadEnvelopeSha256 !== evidence.payloadEnvelopeSha256 ||
    decision.payloadSha256 !== evidence.payloadSha256 ||
    JSON.stringify(decision.context) !== JSON.stringify(expectedContext)
  ) {
    throw new Error(
      "canonical replay decision does not bind its bundle and fetched evidence",
    );
  }
  return decision.detections;
};

/**
 * Digest of a module-admitted complete replay decision. The admission check is
 * part of this operation so a structural/caller-authored decision can never
 * mint the digest used by production capture artifacts.
 */
export const completeCanonicalReplayDecisionDigest = ({
  evidence,
  replayer,
  decision,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly replayer: CompleteCanonicalReplay;
  readonly decision: CompleteCanonicalReplayDecision;
  readonly context?: CompleteCanonicalReplayContext;
}): string => {
  requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision,
    ...(context === undefined ? {} : { context }),
  });
  return createHash("sha256")
    .update(
      JSON.stringify(canonicalizeReplayJson(replayDecisionJson(decision))),
    )
    .digest("hex");
};
