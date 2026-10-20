import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  CANONICAL_DECODABILITY_VIOLATION_ID,
  canonicalDecodabilityEvidenceFromCommittedField,
  COMMITTED_FIELD_SHAPE_VIOLATION_ID,
  decodeAddressWitnessPreimage,
  EMPTY_MERKLE_TREE_ROOT,
  invalidRangeViolationReason,
  MIN_FEE_VIOLATION_ID,
  minimumFeeFromProofSource,
  MISSING_SIGNATURE_VIOLATION_ID,
  missingSignatureVkeyHash,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
} from "@al-ft/midgard-sdk";

import { classifyCommittedFieldShapeFields } from "../committed-field-shape/prepare-committed-field-shape.js";
import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  EventLedgerReplayRefusal,
  ledgerBeforeTransactionEvent,
} from "../input-existence/event-ledger.js";
import { detectInvalidRangeForcedReplay } from "../invalid-range/replay.js";
import { type CheckedLedger } from "../ledger-at.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.js";
import {
  eventRunsPlutus,
  type PlutusExecutionReplay,
  replayPlutusExecution,
} from "../validation-trace-dispute/replay.js";
import { detectZeroInputForcedReplay } from "../zero-input/replay.js";
import { type CanonicalViolationDetection } from "./classification.js";
import {
  EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID,
  PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID,
} from "./complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  type CompleteCanonicalReplayContext,
  requireReplayPredecessorLedger,
} from "./complete-replay.replay-context-identity.js";
import {
  acceptedTransactionSubject,
  sourceEventSubject,
  subjectOf,
} from "./detection-subject.js";
import {
  completeReplayFindings,
  type ReplayPrerequisiteFailure,
  replayPrerequisiteFailure,
} from "./replay-prerequisite.js";

export const detectInvalidRanges = async (
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
            ...acceptedTransactionSubject(transaction.nodeTxId),
            detectionId: `invalid-range:${transactionIndex.toString()}:${transaction.nodeTxId}:${reason}`,
            headerHash: evidence.headerHash,
            violationId: "invalid-range",
            position: BigInt(transactionIndex),
            diagnostic: `transaction ${transaction.nodeTxId} excludes the committed block slot: ${reason}`,
          },
        ];
  });
  const forced = detectInvalidRangeForcedReplay(evidence).map((detection) => ({
    ...subjectOf(detection),
    detectionId: detection.detectionId,
    headerHash: detection.headerHash,
    violationId: detection.violationId,
    position: detection.position,
    diagnostic: `forced transaction at index ${detection.forcedIndex.toString()} was rejected for a typed invalid-range reason despite its authenticated validity range contradicting that rejection`,
  }));
  return [...accepted, ...forced];
};

export const detectZeroInputs = async (
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
            ...acceptedTransactionSubject(transaction.nodeTxId),
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
    ...subjectOf(detection),
    detectionId: detection.detectionId,
    headerHash: detection.headerHash,
    violationId: detection.violationId,
    position: detection.position,
    diagnostic: `forced transaction ${detection.transactionId} was rejected for EmptyInputs despite carrying authenticated spending inputs`,
  }));
  return [...accepted, ...forced];
};

export const detectL2TxMistags = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) =>
    transaction.nativeTxCompact.validity_code === 1n
      ? [
          {
            ...acceptedTransactionSubject(transaction.nodeTxId),
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

export const detectMinFees = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  return transactions.flatMap((transaction, transactionIndex) => {
    const fee = transaction.nativeTx.body.fee;
    const { minimumFee } = minimumFeeFromProofSource({
      sourceKind: "normal",
      source: deriveMidgardNativeTxProofSource(transaction.nativeTx),
      minFeeA: evidence.header.minFeeA,
      minFeeB: evidence.header.minFeeB,
    });
    return fee < minimumFee
      ? [
          {
            ...acceptedTransactionSubject(transaction.nodeTxId),
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

export const detectCommittedFieldShape = (
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
          ...acceptedTransactionSubject(transaction.nodeTxId),
          detectionId: `${COMMITTED_FIELD_SHAPE_VIOLATION_ID}:${transactionIndex.toString()}:${transaction.nodeTxId}:${fieldIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: COMMITTED_FIELD_SHAPE_VIOLATION_ID,
          position: BigInt(transactionIndex),
          diagnostic: `transaction ${transaction.nodeTxId} committed malformed field ${fieldIndex.toString()}`,
        };
      });
  });

export const detectCanonicalDecodability = (
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
                ...acceptedTransactionSubject(transaction.nodeTxId),
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

/**
 * Complete Phase-A required-signer scan over every normal transaction leaf.
 *
 * The normal `transactions_root` contains the transactions the operator
 * accepted into the block. Forced transactions have their own root and their
 * adjudicated validity is handled by the forced/validation-trace surface. A
 * required key is present only when field 7 contains that exact verification
 * key hash; signature validity itself belongs to `invalidSignature`.
 */
export const detectMissingSignatures = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const prerequisites: ReplayPrerequisiteFailure[] = [];
  const detections = transactions.flatMap((transaction, transactionIndex) => {
    const signers = decodeMidgardNativeByteListPreimage(
      transaction.nativeTx.body.requiredSignersPreimageCbor,
      `transaction ${transaction.nodeTxId} required_signers`,
    );
    if (signers.some((signer) => signer.length !== 28)) {
      prerequisites.push(
        ...replayPrerequisiteFailure(
          evidence.headerHash,
          { L2TransactionEventKey: { tx_id: transaction.nodeTxId } },
          "representable_field_shape",
        ).failures,
      );
      return [];
    }
    const requiredSignerHashes = signers.map((signer) =>
      Buffer.from(signer).toString("hex"),
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
              ...acceptedTransactionSubject(transaction.nodeTxId),
              detectionId: `${MISSING_SIGNATURE_VIOLATION_ID}:${transactionIndex.toString()}:${signerIndex.toString()}:${transaction.nodeTxId}:${requiredSignerHash}`,
              headerHash: evidence.headerHash,
              violationId: MISSING_SIGNATURE_VIOLATION_ID,
              position: BigInt(transactionIndex),
              diagnostic: `accepted transaction ${transaction.nodeTxId} is missing required signer ${requiredSignerHash} at ordinal ${signerIndex.toString()}`,
            },
          ],
    );
  });
  return completeReplayFindings(detections, prerequisites);
};

export const VALIDATION_TRACE_VIOLATION_ID = "validation-trace" as const;
/** The verdict replay saw a failed script run that the trace replay did not
 * reproduce; no claim is built on it. Unregistered, so it classifies as a gap. */
export const VALIDATION_TRACE_REPLAY_DISAGREEMENT_VIOLATION_ID =
  "validation-trace-replay-disagreement" as const;
/** The script re-execution ended in an error rather than a verdict or a
 * trace, such as an evaluator with no verdict. Only that event is held:
 * unregistered, so it classifies as a gap, and the block still decides. */
export const VALIDATION_TRACE_REPLAY_UNAVAILABLE_VIOLATION_ID =
  "validation-trace-replay-unavailable" as const;

/**
 * Accepted normal transactions with redeemers (rule R-DESC) whose honest
 * re-execution from DA replay ends Terminal Rejected with
 * `PlutusExecutionFailed`. Each detection names the event and the honest
 * trace's step count. Forced transactions are excluded, as R-DESC excludes
 * them on chain.
 */
export const detectPlutusExecutionFailures = async ({
  evidence,
  context,
  l1SlotConfig,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
  readonly l1SlotConfig: L1SlotConfig;
}): Promise<readonly CanonicalViolationDetection[]> => {
  const predecessor = requireReplayPredecessorLedger({ evidence, context });
  const detections: CanonicalViolationDetection[] = [];
  let position = -1;
  for (const event of evidence.reconstruction.sourceEvents) {
    if (event.phase !== "L2Transaction") continue;
    position += 1;
    if (!eventRunsPlutus(event)) continue;
    const found = (violationId: string, detail: string): void => {
      detections.push({
        ...sourceEventSubject(event),
        detectionId: `${violationId}:${position.toString()}:${event.entry.txId}`,
        headerHash: evidence.headerHash,
        violationId,
        position: BigInt(position),
        diagnostic: detail,
      });
    };
    if (
      predecessor === undefined &&
      evidence.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT
    ) {
      found(
        PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID,
        "script re-execution requires the authenticated predecessor ledger",
      );
      continue;
    }
    const opening = evidence.reconstruction.eventToStepByFingerprint.get(
      event.fingerprint,
    );
    if (opening === undefined || opening.value.phase !== event.phase) {
      found(
        EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID,
        "script re-execution requires the exact committed event-to-step opening",
      );
      continue;
    }
    let ledger: CheckedLedger;
    try {
      ledger = await ledgerBeforeTransactionEvent({
        block: evidence,
        eventKey: event.eventKey,
        stepIndex: opening.value.step_index,
        predecessor,
      });
    } catch (cause) {
      if (!(cause instanceof EventLedgerReplayRefusal)) throw cause;
      found(
        EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID,
        `script re-execution event-pre-state hold: ${cause.reason}`,
      );
      continue;
    }
    let replay: PlutusExecutionReplay;
    try {
      replay = await replayPlutusExecution({
        evidence,
        event,
        ledger,
        l1SlotConfig,
      });
    } catch (cause) {
      found(
        VALIDATION_TRACE_REPLAY_UNAVAILABLE_VIOLATION_ID,
        `script re-execution of accepted transaction ${event.entry.txId} has no result: ${cause instanceof Error ? cause.message : String(cause)}`,
      );
      continue;
    }
    if (replay.kind === "fails")
      found(
        VALIDATION_TRACE_VIOLATION_ID,
        `accepted transaction ${event.entry.txId} fails Plutus execution; its honest trace has ${replay.stepCount.toString()} steps`,
      );
    else if (replay.kind === "trace_disagrees")
      found(
        VALIDATION_TRACE_REPLAY_DISAGREEMENT_VIOLATION_ID,
        `accepted transaction ${event.entry.txId}: ${replay.detail}`,
      );
  }
  return detections;
};
