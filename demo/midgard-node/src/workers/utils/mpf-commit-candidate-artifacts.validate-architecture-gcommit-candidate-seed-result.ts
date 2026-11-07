import {
  exactKeysRecord,
  positiveSafeInteger,
  sha256Digest,
} from "../../artifact-schema.js";
import { type ArchitectureGCommitCandidateSeedResult } from "./mpf-commit-candidate-artifacts.architecture-gcommit-candidate-input.js";

export const validateArchitectureGCommitCandidateSeedResult = ({
  value,
  expectedDatabaseName,
  expectedCorpusSliceSha256,
  expectedTransactionCount,
  expectedConfirmedLedgerCount,
}: {
  readonly value: unknown;
  readonly expectedDatabaseName: string;
  readonly expectedCorpusSliceSha256: string;
  readonly expectedTransactionCount: number;
  readonly expectedConfirmedLedgerCount: number;
}): ArchitectureGCommitCandidateSeedResult => {
  const result = exactKeysRecord(
    value,
    "Architecture G commit-candidate seed result",
    [
      "schemaVersion",
      "databaseName",
      "corpusSliceSha256",
      "mempoolTxCount",
      "fundingCount",
      "terminalLedgerCount",
      "deltaCount",
      "confirmedLedgerCount",
    ],
  );
  const expectedCount = positiveSafeInteger(
    expectedTransactionCount,
    "expectedTransactionCount",
  );
  const fundingCount = positiveSafeInteger(
    result.fundingCount,
    "seedResult.fundingCount",
  );
  const terminalLedgerCount = positiveSafeInteger(
    result.terminalLedgerCount,
    "seedResult.terminalLedgerCount",
  );
  if (
    result.schemaVersion !==
      "midgard-architecture-g-commit-candidate-seed-result-v1" ||
    typeof expectedDatabaseName !== "string" ||
    !/^midgard_phase3_arch_g_[a-z0-9_]+$/u.test(expectedDatabaseName) ||
    result.databaseName !== expectedDatabaseName ||
    result.corpusSliceSha256 !==
      sha256Digest(expectedCorpusSliceSha256, "expectedCorpusSliceSha256") ||
    result.mempoolTxCount !== expectedCount ||
    result.deltaCount !== expectedCount ||
    result.confirmedLedgerCount !==
      positiveSafeInteger(
        expectedConfirmedLedgerCount,
        "expectedConfirmedLedgerCount",
      ) ||
    result.fundingCount !== fundingCount ||
    result.terminalLedgerCount !== terminalLedgerCount
  ) {
    throw new Error("Architecture G commit-candidate seed result is invalid");
  }
  return result as ArchitectureGCommitCandidateSeedResult;
};
