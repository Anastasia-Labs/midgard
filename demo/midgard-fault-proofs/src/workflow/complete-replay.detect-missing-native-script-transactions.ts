import {
  decodeMidgardAddressBytes,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import {
  CANONICAL_DECODABILITY_VIOLATION_ID,
  canonicalDecodabilityEvidenceFromCommittedField,
  COMMITTED_FIELD_SHAPE_VIOLATION_ID,
  decodeAddressWitnessPreimage,
  invalidRangeViolationReason,
  MIN_FEE_VIOLATION_ID,
  minimumFeeFromProofSource,
  MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID,
  MISSING_SIGNATURE_VIOLATION_ID,
  missingNativeScriptIsAbsent,
  missingSignatureVkeyHash,
  nativeTxBodyHasZeroInputViolation,
  normalizeNativeTxValidityRange,
} from "@al-ft/midgard-sdk";

import { classifyCommittedFieldShapeFields } from "../committed-field-shape/prepare-committed-field-shape.js";
import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { detectInvalidRangeForcedReplay } from "../invalid-range/replay.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.js";
import { detectZeroInputForcedReplay } from "../zero-input/replay.js";
import { type CanonicalViolationDetection } from "./classification.js";
import { acceptedTransactionSubject, subjectOf } from "./detection-subject.js";
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
export const detectMissingNativeScriptTransactions = async (
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
          ...acceptedTransactionSubject(transaction.nodeTxId),
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
