import {
  committedWithdrawalKeyBytes,
  DOUBLE_WITHDRAW_VIOLATION_ID,
  type FraudProofCatalogueCategoryName,
  isPayableWithdrawalLeaf,
  REFERENCE_INPUT_NO_IDX_VIOLATION_ID,
  WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { detectMissingSignatureWrongfulRejections } from "../missing-signature/wrongful-rejection.js";
import { detectNoReferenceInputWrongfulRejections } from "../no-reference-input/wrongful-rejection.js";
import { detectNonExistentInputWrongfulRejections } from "../non-existent-input/wrongful-rejection.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.js";
import { detectReferenceInputNoIdxViolationsFromTransactions } from "../prepare-reference-input-no-idx.js";
import { detectValidationTraceReplay } from "../validation-dispute/replay.js";
import { type CanonicalViolationDetection } from "./classification.js";
import {
  completeCanonicalReplayPredecessorEvidence,
  detectDoubleSpends,
  detectLedgerRelativeMissingInputs,
  detectNetworkIds,
} from "./complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  detectCanonicalDecodability,
  detectCommittedFieldShape,
  detectInvalidRanges,
  detectMissingNativeScriptTransactions,
  detectMissingSignatures,
  detectZeroInputs,
} from "./complete-replay.detect-missing-native-script-transactions.js";
import {
  admittedDecisions,
  admittedReplayers,
  COMPLETE_CANONICAL_REPLAY,
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
  type CompleteCanonicalReplayDecision,
  replayContextIdentity,
} from "./complete-replay.replay-context-identity.js";
import {
  acceptedTransactionSubject,
  withdrawalSubject,
} from "./detection-subject.js";
import {
  detectMissingNativeScriptUtxoFromHistoricalCorpus,
  type HistoricalNativeScriptCorpus,
} from "./historical-native-script-corpus.js";

/** Complete same-block accepted reference-input/withdrawal intersection. */
export const detectWithdrawnReferenceInputs = async (
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
              ...acceptedTransactionSubject(transaction.nodeTxId),
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

export const detectReferenceInputNoIdxViolations = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  detectReferenceInputNoIdxViolationsFromTransactions(
    evidence.transactions,
  ).map((detection) => ({
    ...acceptedTransactionSubject(detection.badTxId),
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
export const detectDoubleWithdraws = (
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
        ...withdrawalSubject(first.key, second.key),
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

export const completeReplayer = (
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
  async (evidence, context) => [
    ...(await (evidence.transactions.length === 0
      ? []
      : detectLedgerRelativeMissingInputs({
          evidence,
          context,
          kind: "spend",
        }))),
    ...(await detectNonExistentInputWrongfulRejections({
      block: evidence,
      predecessor: completeCanonicalReplayPredecessorEvidence({
        evidence,
        context,
      }),
    })),
  ],
);

/** Complete replay for every transaction/output covered by the Q35 family. */
export const NETWORK_ID_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["networkId"],
  async (evidence) => detectNetworkIds(evidence),
);

export const VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["validationTraceDispute"], async (evidence, context) => {
    if (context?.validationTraceReplay === undefined)
      throw new Error("validation trace replay requires its admitted context");
    return detectValidationTraceReplay({
      evidence,
      context: context.validationTraceReplay,
      predecessor: context.predecessor,
      transitionTraceEvents: context.transitionTraceEvents,
    });
  });

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

/** Complete accepted and wrongful-rejected reference-input scan. */
export const NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["noReferenceInput"],
  async (evidence, context) => [
    ...(await (evidence.transactions.length === 0
      ? []
      : detectLedgerRelativeMissingInputs({
          evidence,
          context,
          kind: "reference",
        }))),
    ...(await detectNoReferenceInputWrongfulRejections({
      block: evidence,
      predecessor: completeCanonicalReplayPredecessorEvidence({
        evidence,
        context,
      }),
    })),
  ],
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
  async (evidence) => [
    ...(await detectMissingSignatures(evidence)),
    ...detectMissingSignatureWrongfulRejections({ block: evidence }),
  ],
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
  corpus: HistoricalNativeScriptCorpus | (() => HistoricalNativeScriptCorpus),
): CompleteCanonicalReplay =>
  completeReplayer(
    ["missingNativeScriptUtxo"],
    async (evidence) =>
      await detectMissingNativeScriptUtxoFromHistoricalCorpus({
        evidence,
        corpus: typeof corpus === "function" ? corpus() : corpus,
      }),
  );
