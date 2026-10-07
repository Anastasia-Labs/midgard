import { jsonEqual } from "./mpf-architecture-g-gate-config.capture-architecture-gphase1-formal-binding-identity.mjs";
import {
  isCanonicalAbsolutePath,
  isHash,
  isNonNegativeSafeInteger,
  isPositiveSafeInteger,
  requireExactObjectKeys,
} from "./mpf-architecture-g-gate-config.validate-architecture-gfixture-creation-evidence.mjs";
import { validateArchitectureGOwnerDiagnostics } from "./mpf-architecture-g-gate-config.validate-architecture-groot-gate-result-shape.mjs";

export const validateCommitCandidateProbeResult = ({
  result,
  transactions,
  cpuSet,
  fixtureSize,
  inputPath,
  inputSha256,
  probePath,
  probeSha256,
  binarySha256,
}) => {
  requireExactObjectKeys(
    result,
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
    "Architecture G commit-candidate probe result",
  );
  requireExactObjectKeys(
    result.baseUtxoPayloadAggregate,
    ["entryCount", "encodedTupleBytes"],
    "Commit-candidate base UTxO payload aggregate",
  );
  requireExactObjectKeys(
    result.candidateConfig,
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
    "Commit-candidate configuration evidence",
  );
  requireExactObjectKeys(
    result.candidate,
    ["endTimeMs", "l2TransactionCount", "roots"],
    "Commit-candidate summary",
  );
  requireExactObjectKeys(
    result.userEventRows,
    ["deposits", "forcedTransactions", "withdrawals"],
    "Commit-candidate fixture user-event rows",
  );
  const rootKeys = [
    "utxos",
    "rawTransactions",
    "transactions",
    "transitionTrace",
    "eventToStep",
  ];
  requireExactObjectKeys(
    result.candidate.roots,
    rootKeys,
    "Commit-candidate roots",
  );
  validateArchitectureGOwnerDiagnostics(
    result.ownerBefore,
    "Commit-candidate owner-before diagnostics",
  );
  validateArchitectureGOwnerDiagnostics(
    result.ownerAfter,
    "Commit-candidate owner-after diagnostics",
  );
  if (
    result?.schemaVersion !== "midgard-architecture-g-commit-candidate-probe-v1"
  ) {
    throw new Error("Unsupported commit-candidate probe result schema");
  }
  if (
    result.expectedTransactionCount !== transactions ||
    result.candidate?.l2TransactionCount !== transactions
  ) {
    throw new Error("Commit-candidate probe transaction count drifted");
  }
  if (result.cpuAffinity !== cpuSet) {
    throw new Error("Commit-candidate probe CPU affinity drifted");
  }
  if (
    result.inputPath !== inputPath ||
    !isCanonicalAbsolutePath(result.inputPath) ||
    !isHash(inputSha256) ||
    result.inputSha256 !== inputSha256
  ) {
    throw new Error("Commit-candidate probe input identity drifted");
  }
  if (
    result.probePath !== probePath ||
    !isCanonicalAbsolutePath(result.probePath) ||
    !isHash(probeSha256) ||
    result.probeSha256 !== probeSha256 ||
    !isHash(binarySha256) ||
    result.binarySha256 !== binarySha256
  ) {
    throw new Error("Commit-candidate executable identity drifted");
  }
  for (const field of [
    "corpusSha256",
    "corpusSliceSha256",
    "fundingMapSha256",
    "fixtureCreationSha256",
    "binarySha256",
  ]) {
    if (!/^[0-9a-f]{64}$/u.test(result[field] ?? "")) {
      throw new Error(`Commit-candidate probe ${field} is invalid`);
    }
  }
  if (
    result.fixtureInitialUtxoCount !== fixtureSize ||
    result.baseUtxoPayloadAggregate?.entryCount !== fixtureSize ||
    !Number.isSafeInteger(result.baseUtxoPayloadAggregate?.encodedTupleBytes) ||
    result.baseUtxoPayloadAggregate.encodedTupleBytes <= 0
  ) {
    throw new Error(
      "Commit-candidate probe fixture aggregate/cardinality drifted",
    );
  }
  const candidateConfig = result.candidateConfig;
  if (
    candidateConfig.mpfEngine !== "architecture_g" ||
    candidateConfig.scratchBuild !== "fromlist" ||
    candidateConfig.payloadRootCheck !== "off" ||
    candidateConfig.parallelRoots !== true ||
    candidateConfig.costModel !== "ewma" ||
    !isPositiveSafeInteger(candidateConfig.mempoolRetrievePageSize) ||
    candidateConfig.mempoolRetrievePageSize < transactions ||
    !isPositiveSafeInteger(candidateConfig.maxL2TxCount) ||
    candidateConfig.maxL2TxCount < transactions ||
    !isPositiveSafeInteger(candidateConfig.maxLedgerOpCount) ||
    candidateConfig.maxLedgerOpCount < transactions * 3 ||
    !isPositiveSafeInteger(candidateConfig.maxTransitionStepCount) ||
    candidateConfig.maxTransitionStepCount < transactions
  ) {
    throw new Error("Commit-candidate configuration evidence is invalid");
  }
  // shouldHydrateCommitBaseEntries hydrates whenever candidateTxCount > 0.
  if (result.confirmedLedgerFullScans !== 1) {
    throw new Error(
      "Commit-candidate probe must perform exactly one confirmed-ledger scan",
    );
  }
  // The candidate omits the user-event roots, so the fixture must hold no
  // user events: their roots are then the empty tree.
  if (Object.values(result.userEventRows).some((count) => count !== 0)) {
    throw new Error(
      "Commit-candidate fixture must hold no deposits, forced transactions or withdrawals",
    );
  }
  if (!isNonNegativeSafeInteger(result.providerReads)) {
    throw new Error("Commit-candidate probe provider read count is invalid");
  }
  if (
    result.providerBoundaryAttempts !== 0 ||
    result.submissionAttempts !== result.providerBoundaryAttempts
  ) {
    throw new Error(
      "Commit-candidate probe crossed the provider/submission boundary",
    );
  }
  if (!isPositiveSafeInteger(result.candidate.endTimeMs)) {
    throw new Error("Commit-candidate block end time is invalid");
  }
  if (result.journalRowsBefore !== 0 || result.journalRowsAfter !== 0) {
    throw new Error(
      "Commit-candidate probe requires a fresh empty pending journal and must leave it empty",
    );
  }
  if (!Number.isFinite(result.durationMs) || result.durationMs <= 0) {
    throw new Error("Commit-candidate probe duration is invalid");
  }
  if (
    result.ownerBefore.durableRoot !== result.ownerAfter.durableRoot ||
    !jsonEqual(result.ownerBefore.ownerEpoch, result.ownerAfter.ownerEpoch) ||
    result.ownerBefore.childRestarts !== result.ownerAfter.childRestarts
  ) {
    throw new Error("Commit-candidate native owner identity drifted");
  }
  const roots = result.candidate?.roots;
  if (rootKeys.some((field) => !isHash(roots[field]))) {
    throw new Error("Commit-candidate roots are invalid");
  }
  return roots;
};
