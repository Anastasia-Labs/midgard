import {
  boundedNonEmptyString,
  canonicalAbsolutePath,
  exactKeysRecord,
  nonNegativeSafeInteger,
  positiveFiniteNumber,
  positiveSafeInteger,
  sha256Digest,
} from "../../artifact-schema.js";
import {
  type ArchitectureGCommitCandidateInput,
  decodeArchitectureGOwnerDiagnostics,
  type JsonRecord,
  sameJson,
} from "./mpf-commit-candidate-artifacts.architecture-gcommit-candidate-input.js";
import { decodeArchitectureGCommitCandidateInput } from "./mpf-commit-candidate-artifacts.decode-architecture-gcommit-candidate-input.js";

export const validateArchitectureGCommitCandidateProbeResult = ({
  value,
  expectedInput,
  expectedInputPath,
  expectedInputSha256,
  expectedProbePath,
  expectedProbeSha256,
  expectedCpuAffinity,
}: {
  readonly value: unknown;
  readonly expectedInput: ArchitectureGCommitCandidateInput;
  readonly expectedInputPath: string;
  readonly expectedInputSha256: string;
  readonly expectedProbePath: string;
  readonly expectedProbeSha256: string;
  readonly expectedCpuAffinity: string;
}): JsonRecord => {
  const input = decodeArchitectureGCommitCandidateInput(expectedInput);
  const result = exactKeysRecord(
    JSON.parse(JSON.stringify(value)) as unknown,
    "Architecture G commit-candidate probe result",
    [
      "schemaVersion",
      "probePath",
      "probeSha256",
      "inputPath",
      "inputSha256",
      "expectedTransactionCount",
      "corpusSha256",
      "corpusSliceSha256",
      "fundingMapSha256",
      "fixtureCreationSha256",
      "fixtureInitialUtxoCount",
      "baseUtxoPayloadAggregate",
      "binarySha256",
      "cpuAffinity",
      "durationMs",
      "confirmedLedgerFullScans",
      "userEventRows",
      "journalRowsBefore",
      "journalRowsAfter",
      "candidateConfig",
      "providerReads",
      "providerBoundaryAttempts",
      "submissionAttempts",
      "candidate",
      "ownerBefore",
      "ownerAfter",
    ],
  );
  const aggregate = exactKeysRecord(
    result.baseUtxoPayloadAggregate,
    "Commit-candidate base UTxO payload aggregate",
    ["entryCount", "encodedTupleBytes"],
  );
  const config = exactKeysRecord(
    result.candidateConfig,
    "Commit-candidate configuration evidence",
    [
      "mpfEngine",
      "scratchBuild",
      "payloadRootCheck",
      "parallelRoots",
      "costModel",
      "mempoolRetrievePageSize",
      "maxL2TxCount",
      "maxLedgerOpCount",
      "maxTransitionStepCount",
    ],
  );
  const candidate = exactKeysRecord(
    result.candidate,
    "Commit-candidate summary",
    ["endTimeMs", "l2TransactionCount", "roots"],
  );
  const rootKeys = [
    "utxos",
    "rawTransactions",
    "transactions",
    "transitionTrace",
    "eventToStep",
  ] as const;
  const userEventRows = exactKeysRecord(
    result.userEventRows,
    "Commit-candidate fixture user-event rows",
    ["deposits", "forcedTransactions", "withdrawals"],
  );
  const roots = exactKeysRecord(
    candidate.roots,
    "Commit-candidate roots",
    rootKeys,
  );
  const ownerBefore = decodeArchitectureGOwnerDiagnostics(
    result.ownerBefore,
    "Commit-candidate owner-before diagnostics",
  );
  const ownerAfter = decodeArchitectureGOwnerDiagnostics(
    result.ownerAfter,
    "Commit-candidate owner-after diagnostics",
  );
  const inputPath = canonicalAbsolutePath(
    expectedInputPath,
    "expectedInputPath",
  );
  const probePath = canonicalAbsolutePath(
    expectedProbePath,
    "expectedProbePath",
  );
  const inputSha256 = sha256Digest(expectedInputSha256, "expectedInputSha256");
  const probeSha256 = sha256Digest(expectedProbeSha256, "expectedProbeSha256");
  const cpuAffinity = boundedNonEmptyString(
    expectedCpuAffinity,
    "expectedCpuAffinity",
  );
  const transactionCount = input.expectedTransactionCount;
  if (
    result.schemaVersion !==
      "midgard-architecture-g-commit-candidate-probe-v1" ||
    result.probePath !== probePath ||
    result.probeSha256 !== probeSha256 ||
    result.inputPath !== inputPath ||
    result.inputSha256 !== inputSha256 ||
    result.expectedTransactionCount !== transactionCount ||
    result.corpusSha256 !== input.corpusSha256 ||
    result.corpusSliceSha256 !== input.corpusSliceSha256 ||
    result.fundingMapSha256 !== input.fundingMapSha256 ||
    result.fixtureCreationSha256 !== input.fixtureCreationSha256 ||
    result.fixtureInitialUtxoCount !== input.fixtureInitialUtxoCount ||
    !sameJson(aggregate, input.baseUtxoPayloadAggregate) ||
    result.binarySha256 !== input.binarySha256 ||
    result.cpuAffinity !== cpuAffinity ||
    // shouldHydrateCommitBaseEntries hydrates whenever candidateTxCount > 0.
    result.confirmedLedgerFullScans !== 1 ||
    result.journalRowsBefore !== 0 ||
    result.journalRowsAfter !== 0 ||
    result.providerBoundaryAttempts !== 0 ||
    result.submissionAttempts !== result.providerBoundaryAttempts
  ) {
    throw new Error(
      "Architecture G commit-candidate probe boundary identity is invalid",
    );
  }
  positiveFiniteNumber(result.durationMs, "candidateProbe.durationMs");
  if (
    config.mpfEngine !== "architecture_g" ||
    config.scratchBuild !== "fromlist" ||
    config.payloadRootCheck !== "off" ||
    config.parallelRoots !== true ||
    config.costModel !== "ewma" ||
    positiveSafeInteger(
      config.mempoolRetrievePageSize,
      "candidateConfig.mempoolRetrievePageSize",
    ) < transactionCount ||
    positiveSafeInteger(config.maxL2TxCount, "candidateConfig.maxL2TxCount") <
      transactionCount ||
    positiveSafeInteger(
      config.maxLedgerOpCount,
      "candidateConfig.maxLedgerOpCount",
    ) <
      transactionCount * 3 ||
    positiveSafeInteger(
      config.maxTransitionStepCount,
      "candidateConfig.maxTransitionStepCount",
    ) < transactionCount
  ) {
    throw new Error("Commit-candidate configuration evidence is invalid");
  }
  nonNegativeSafeInteger(result.providerReads, "candidateProbe.providerReads");
  positiveSafeInteger(candidate.endTimeMs, "candidate.endTimeMs");
  if (candidate.l2TransactionCount !== transactionCount) {
    throw new Error("Commit-candidate transaction count is invalid");
  }
  for (const field of rootKeys) {
    sha256Digest(roots[field], `candidate.roots.${field}`);
  }
  // The candidate omits the user-event roots, so the fixture must hold no
  // user events: their roots are then the empty tree.
  if (Object.values(userEventRows).some((count) => count !== 0)) {
    throw new Error(
      "Commit-candidate fixture must hold no deposits, forced transactions or withdrawals",
    );
  }
  if (
    ownerBefore.durableRoot !== input.baseUtxosRoot ||
    ownerBefore.durableRoot !== ownerAfter.durableRoot ||
    !sameJson(ownerBefore.ownerEpoch, ownerAfter.ownerEpoch) ||
    ownerBefore.childRestarts !== ownerAfter.childRestarts
  ) {
    throw new Error("Commit-candidate native owner identity drifted");
  }
  return result;
};
