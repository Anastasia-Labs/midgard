import {
  decodeMidgardAddressWitnessFieldPreimageV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardVersionedScript,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import { missingSignatureVkeyHashV1 } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
import {
  type ProductionHistoricalNativeScriptCorpusV1,
  requireProductionHistoricalNativeScriptCorpusV1,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import { reconstructExecutionNativeScriptPurposesV1 } from "./canonical-reconstruction-v1.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1 =
  "execution-native-script-invalid" as const;

export type ExecutionNativeScriptInvalidReplayDetectionV1 =
  CanonicalViolationDetectionV1 &
    Readonly<{
      transactionId: string;
      executionIndex: number;
      source: "accepted" | "forced";
      direction: "wrongfulAcceptance" | "wrongfulRejection";
      forcedIndex?: number;
    }>;

const priorOutputs = ({
  block,
  corpus,
}: {
  block: CanonicalBlockEvidenceV1;
  corpus: ProductionHistoricalNativeScriptCorpusV1;
}): ReadonlyMap<string, Uint8Array> => {
  const history = requireProductionHistoricalNativeScriptCorpusV1(corpus);
  if (
    history.currentEvidence !== block ||
    corpus.throughHeaderHash !== block.headerHash
  )
    throw new Error(
      "executionNativeScriptInvalid historical authority changed challenged header",
    );
  const predecessor = history.reconstructions.at(-2);
  if (predecessor === undefined) return new Map();
  if (
    predecessor.headerHash !== block.header.prevHeaderHash ||
    predecessor.header.utxosRoot !== block.header.prevUtxosRoot
  )
    throw new Error(
      "executionNativeScriptInvalid predecessor differs from challenged header",
    );
  return new Map(
    predecessor.utxos.map(({ key, value }) => [
      Buffer.from(key).toString("hex"),
      Buffer.from(value),
    ]),
  );
};

const nativeResult = ({
  txCbor,
  executionIndex,
  resolvedOutputs,
}: {
  txCbor: Uint8Array;
  executionIndex: number;
  resolvedOutputs: ReadonlyMap<string, Uint8Array>;
}): boolean | null => {
  const transaction = decodeMidgardNativeTxFullV1FromCanonicalCbor(txCbor);
  const purpose = reconstructExecutionNativeScriptPurposesV1({
    canonicalTransactionCbor: txCbor,
    resolvedOutputsByOutRef: resolvedOutputs,
  }).purposes[executionIndex];
  if (purpose === undefined || purpose.source.languageTag !== 0) return null;
  const script = decodeMidgardVersionedScript(
    Buffer.from(purpose.source.versionedItemCbor, "hex"),
  );
  if (script.language !== "NativeCardano") return null;
  const signers = new Set(
    decodeMidgardAddressWitnessFieldPreimageV1(
      transaction.witnessSet.addrTxWitsPreimageCbor,
    ).map(({ verificationKey }) =>
      missingSignatureVkeyHashV1(Buffer.from(verificationKey).toString("hex")),
    ),
  );
  const start = transaction.body.validityIntervalStart;
  const end = transaction.body.validityIntervalEnd;
  return verifyMidgardNativeScript(script.nativeScript, {
    validityIntervalStart:
      start === MIDGARD_POSIX_TIME_NONE ? undefined : start,
    validityIntervalEnd: end === MIDGARD_POSIX_TIME_NONE ? undefined : end,
    witnessSigners: signers,
  });
};

/**
 * Complete ID32 replay from the challenged payload plus its admitted,
 * contiguous predecessor ledger. Every execution coordinate is reconstructed
 * from canonical purposes; witness position is never treated as authority.
 */
export const detectExecutionNativeScriptInvalidCanonicalViolationsV1 = ({
  block,
  corpus,
}: {
  block: CanonicalBlockEvidenceV1;
  corpus: ProductionHistoricalNativeScriptCorpusV1;
}): readonly ExecutionNativeScriptInvalidReplayDetectionV1[] => {
  const resolvedOutputs = priorOutputs({ block, corpus });
  const detections: ExecutionNativeScriptInvalidReplayDetectionV1[] = [];
  block.transactions.forEach((transaction, position) => {
    const txCbor = Buffer.from(transaction.txCbor, "hex");
    const decoded = decodeMidgardNativeTxFullV1FromCanonicalCbor(txCbor);
    if (decoded.validity !== "TxIsValid") return;
    let reconstruction;
    try {
      reconstruction = reconstructExecutionNativeScriptPurposesV1({
        canonicalTransactionCbor: txCbor,
        resolvedOutputsByOutRef: resolvedOutputs,
      });
    } catch {
      // Missing or malformed canonical source authority belongs to an earlier
      // family. ID32 never guesses a source to keep scanning.
      return;
    }
    reconstruction.purposes.forEach(({ executionIndex }) => {
      let result: boolean | null;
      try {
        result = nativeResult({ txCbor, executionIndex, resolvedOutputs });
      } catch {
        return;
      }
      if (result !== false) return;
      detections.push({
        detectionId: `${EXECUTION_NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1}:accepted:${position.toString()}:${transaction.nodeTxId}:${executionIndex.toString()}`,
        headerHash: block.headerHash,
        violationId: EXECUTION_NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1,
        position: BigInt(position),
        diagnostic: `accepted transaction ${transaction.nodeTxId} has false native execution ${executionIndex.toString()}`,
        transactionId: transaction.nodeTxId,
        executionIndex,
        source: "accepted",
        direction: "wrongfulAcceptance",
      });
    });
  });
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") return;
      const reason = verdict.ForcedTxInvalid.reason;
      if (
        typeof reason === "string" ||
        !("ExecutionNativeScriptFalse" in reason)
      )
        return;
      const coordinate = reason.ExecutionNativeScriptFalse.execution_index;
      const executionIndex = Number(coordinate);
      if (!Number.isSafeInteger(executionIndex) || executionIndex < 0)
        throw new Error(
          "executionNativeScriptInvalid forced coordinate changed",
        );
      const txCbor = Buffer.from(transaction.fullTransactionCbor);
      let result: boolean | null;
      try {
        result = nativeResult({ txCbor, executionIndex, resolvedOutputs });
      } catch {
        return;
      }
      if (result !== true) return;
      detections.push({
        detectionId: `${EXECUTION_NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${executionIndex.toString()}`,
        headerHash: block.headerHash,
        violationId: EXECUTION_NATIVE_SCRIPT_INVALID_VIOLATION_ID_V1,
        position: BigInt(forcedIndex),
        diagnostic: `forced transaction ${transaction.value.tx_id} was rejected for a native execution that evaluates true at ${executionIndex.toString()}`,
        transactionId: transaction.value.tx_id,
        executionIndex,
        source: "forced",
        direction: "wrongfulRejection",
        forcedIndex,
      });
    },
  );
  return Object.freeze(detections.map((detection) => Object.freeze(detection)));
};
