import {
  CANONICAL_NATURAL,
  exactLiteral,
  exactRecord,
  exactString,
  fail,
  HEX_32,
  type JsonRecord,
} from "./durable-store.canonical-json.js";
import {
  parseConfirmation,
  parseDaProofInput,
  parseDeadline,
  parseDecision,
  parseFault,
  parseReconstructedState,
  parseRetry,
  parseSpentProtocolUtxo,
  parseSubmission,
} from "./durable-store.parse-confirmation.js";
import {
  parseChainPoint,
  parseL1Observation,
  parseProtocolUtxo,
  type RecordParser,
  type WatcherCorrectionResult,
  type WatcherDurableRecords,
} from "./durable-store.parse-l1-observation.js";

const parseCorrectionResult: RecordParser<WatcherCorrectionResult> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "correctionId",
    "faultId",
    "confirmationId",
    "outcome",
    "finalStateRoot",
    "slashLovelace",
    "rewardLovelace",
  ]);
  return {
    correctionId: exactString(
      record.correctionId,
      `${path}.correctionId`,
      HEX_32,
    ),
    faultId: exactString(record.faultId, `${path}.faultId`, HEX_32),
    confirmationId: exactString(
      record.confirmationId,
      `${path}.confirmationId`,
      HEX_32,
    ),
    outcome: exactLiteral(record.outcome, `${path}.outcome`, [
      "removed",
      "resolved",
      "removed_and_slashed",
      "removed_slashed_and_rewarded",
    ]),
    finalStateRoot: exactString(
      record.finalStateRoot,
      `${path}.finalStateRoot`,
      HEX_32,
    ),
    slashLovelace: exactString(
      record.slashLovelace,
      `${path}.slashLovelace`,
      CANONICAL_NATURAL,
    ),
    rewardLovelace: exactString(
      record.rewardLovelace,
      `${path}.rewardLovelace`,
      CANONICAL_NATURAL,
    ),
  };
};

export const parseSortedRecords = <T>(
  value: unknown,
  path: string,
  parser: RecordParser<T>,
  keyOf: (record: T) => string,
): readonly T[] => {
  if (!Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const members = value as readonly unknown[];
  const records = members.map((member, index) =>
    parser(member, `${path}[${index.toString()}]`),
  );
  for (let index = 1; index < records.length; index += 1) {
    const previous = keyOf(records[index - 1] as T);
    const current = keyOf(records[index] as T);
    if (current === previous) {
      fail("duplicate_key", `${path}[${index.toString()}]`);
    }
    if (current < previous) {
      fail("unsorted_records", path);
    }
  }
  return records;
};

const STORE_RECORD_KEYS = [
  "l1Observations",
  "chainPoints",
  "protocolUtxos",
  "spentProtocolUtxos",
  "daProofInputs",
  "reconstructedStates",
  "decisions",
  "faults",
  "submissions",
  "confirmations",
  "retries",
  "deadlines",
  "correctionResults",
] as const;

export const STORE_KEYS = [
  "schemaVersion",
  "migrationVersion",
  "migrationManifestSha256",
  "revision",
  "deploymentMarker",
  ...STORE_RECORD_KEYS,
  "caches",
] as const;

export const parseRecords = (record: JsonRecord): WatcherDurableRecords => ({
  l1Observations: parseSortedRecords(
    record.l1Observations,
    "$.l1Observations",
    parseL1Observation,
    (entry) => entry.observationId,
  ),
  chainPoints: parseSortedRecords(
    record.chainPoints,
    "$.chainPoints",
    parseChainPoint,
    (entry) => entry.chainPointId,
  ),
  protocolUtxos: parseSortedRecords(
    record.protocolUtxos,
    "$.protocolUtxos",
    parseProtocolUtxo,
    (entry) => entry.outRef,
  ),
  spentProtocolUtxos: parseSortedRecords(
    record.spentProtocolUtxos,
    "$.spentProtocolUtxos",
    parseSpentProtocolUtxo,
    (entry) => entry.outRef,
  ),
  daProofInputs: parseSortedRecords(
    record.daProofInputs,
    "$.daProofInputs",
    parseDaProofInput,
    (entry) => entry.inputId,
  ),
  reconstructedStates: parseSortedRecords(
    record.reconstructedStates,
    "$.reconstructedStates",
    parseReconstructedState,
    (entry) => entry.blockHash,
  ),
  decisions: parseSortedRecords(
    record.decisions,
    "$.decisions",
    parseDecision,
    (entry) => entry.blockHash,
  ),
  faults: parseSortedRecords(
    record.faults,
    "$.faults",
    parseFault,
    (entry) => entry.faultId,
  ),
  submissions: parseSortedRecords(
    record.submissions,
    "$.submissions",
    parseSubmission,
    (entry) => entry.submissionId,
  ),
  confirmations: parseSortedRecords(
    record.confirmations,
    "$.confirmations",
    parseConfirmation,
    (entry) => entry.confirmationId,
  ),
  retries: parseSortedRecords(
    record.retries,
    "$.retries",
    parseRetry,
    (entry) => entry.retryId,
  ),
  deadlines: parseSortedRecords(
    record.deadlines,
    "$.deadlines",
    parseDeadline,
    (entry) => entry.deadlineId,
  ),
  correctionResults: parseSortedRecords(
    record.correctionResults,
    "$.correctionResults",
    parseCorrectionResult,
    (entry) => entry.correctionId,
  ),
});
