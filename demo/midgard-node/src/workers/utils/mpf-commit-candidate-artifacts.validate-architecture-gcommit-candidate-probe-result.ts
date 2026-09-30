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
      "journalRowsBefore",
      "journalRowsAfter",
      "candidateConfig",
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
    [
      "candidateId",
      "baseHeaderHash",
      "endTimeMs",
      "builtAtMs",
      "buildDurationMs",
      "invalidationKey",
      "watermarks",
      "expectedUserEventCounts",
      "expectedL2TransactionCount",
      "roots",
    ],
  );
  const watermarks = exactKeysRecord(
    candidate.watermarks,
    "Commit-candidate barrier watermarks",
    ["depositMs", "withdrawalMs", "txOrderMs", "refreshedAtMs"],
  );
  const expectedUserEventCounts = exactKeysRecord(
    candidate.expectedUserEventCounts,
    "Commit-candidate expected user-event counts",
    ["deposits", "forcedTransactions", "withdrawals"],
  );
  const rootKeys = [
    "utxos",
    "rawTransactions",
    "transactions",
    "deposits",
    "forcedTransactions",
    "withdrawals",
    "transitionTrace",
    "eventToStep",
  ] as const;
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
    result.confirmedLedgerFullScans !== 0 ||
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
  const watermarkValues = Object.entries(watermarks).map(([field, value]) =>
    nonNegativeSafeInteger(value, `candidate.watermarks.${field}`),
  );
  for (const [field, count] of Object.entries(expectedUserEventCounts)) {
    nonNegativeSafeInteger(count, `candidate.expectedUserEventCounts.${field}`);
  }
  const endTimeMs = positiveSafeInteger(
    candidate.endTimeMs,
    "candidate.endTimeMs",
  );
  positiveSafeInteger(candidate.builtAtMs, "candidate.builtAtMs");
  positiveFiniteNumber(candidate.buildDurationMs, "candidate.buildDurationMs");
  const minimumWatermarkMs = Math.min(...watermarkValues);
  if (
    typeof candidate.candidateId !== "string" ||
    !/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      candidate.candidateId,
    ) ||
    candidate.baseHeaderHash !==
      input.workerInput.data.speculativeBuild.base.headerHash ||
    !sameJson(watermarks, input.workerInput.data.speculativeBuild.watermarks) ||
    candidate.expectedL2TransactionCount !== transactionCount ||
    candidate.invalidationKey !==
      `${candidate.baseHeaderHash as string}:${endTimeMs.toString()}:${minimumWatermarkMs.toString()}`
  ) {
    throw new Error("Commit-candidate identity or barrier evidence is invalid");
  }
  for (const field of rootKeys) {
    sha256Digest(roots[field], `candidate.roots.${field}`);
  }
  if (
    ownerBefore.durableRoot !==
      input.workerInput.data.speculativeBuild.base.utxosRoot ||
    ownerBefore.durableRoot !== ownerAfter.durableRoot ||
    !sameJson(ownerBefore.ownerEpoch, ownerAfter.ownerEpoch) ||
    ownerBefore.childRestarts !== ownerAfter.childRestarts
  ) {
    throw new Error("Commit-candidate native owner identity drifted");
  }
  return result;
};
