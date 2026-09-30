import {
  decodeMidgardAddressWitnessFieldPreimage,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardSpendInputItem,
  decodeMidgardVersionedScript,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import {
  committedWithdrawalKeyBytes,
  decodeAddressWitnessPreimage,
  EMPTY_MERKLE_TREE_ROOT,
  INPUT_NO_IDX_VIOLATION_ID,
  INVALID_SIGNATURE_VIOLATION_ID,
  isWithdrawnInputViolation,
  MIN_ADA_VIOLATION_ID,
  missingSignatureVkeyHash,
  NATIVE_SCRIPT_INVALID_VIOLATION_ID,
  verifyAddressWitness,
  WITHDRAWN_INPUT_VIOLATION_ID,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { detectInputSetUniquenessForcedReplay } from "../input-set-uniqueness/replay.js";
import {
  INPUT_SET_UNIQUENESS_VIOLATION_ID,
  scanInputSetUniqueness,
} from "../input-set-uniqueness/scan.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.js";
import { detectInputNoIdxViolationsFromTransactions } from "../prepare-input-no-idx.js";
import { type CanonicalViolationDetection } from "./classification.js";
import { PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID } from "./complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  type CompleteCanonicalReplayContext,
  requireReplayPredecessorEvidence,
} from "./complete-replay.replay-context-identity.js";

/** Complete positional scan of every committed address witness. */
export const detectInvalidSignatures = async (
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
export const detectNativeScriptInvalid = async (
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
export const detectMinAda = async (
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

export const detectInputNoIdxViolations = async (
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

export const detectInputSetUniqueness = async (
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
export const detectWithdrawnInputs = async (
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
