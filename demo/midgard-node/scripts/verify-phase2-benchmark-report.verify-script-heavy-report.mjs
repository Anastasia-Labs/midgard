import {
  atLeast,
  below,
  equal,
  fail,
  nonEmptyString,
  object,
  positiveSafeInteger,
  verifyEightPhysicalCores,
  verifyExactNodeContainerImage,
} from "./verify-phase2-benchmark-report.verify-stage-breport.mjs";

export const verifyScriptHeavyReport = (
  reportValue,
  { chunkSize, candidate = false },
) => {
  const report = object(reportValue, "report");
  equal(report.gateAsserted, true, "gateAsserted");
  equal(report.pinnedEightCore, true, "pinnedEightCore");
  equal(report.containerIdentityProved, true, "containerIdentityProved");
  equal(report.nodeVersion, "v22.22.2", "nodeVersion");
  verifyExactNodeContainerImage(report);
  equal(report.availableParallelism, 8, "availableParallelism");
  verifyEightPhysicalCores(report);
  equal(report.poolSize, 6, "poolSize");
  equal(report.batchSize, 256, "batchSize");
  equal(report.chunkSize, chunkSize, "chunkSize");
  equal(report.signatureVerifier, "node", "signatureVerifier");
  equal(
    report.everyTransactionHasPlutusSpend,
    true,
    "everyTransactionHasPlutusSpend",
  );
  equal(report.verdictMatchesInline, true, "verdictMatchesInline");
  equal(
    report.gateMode,
    candidate ? "chunk128_candidate" : "production_default_chunk64",
    "gateMode",
  );
  equal(report.everyTransactionIsPlutusV3, true, "everyTransactionIsPlutusV3");
  equal(report.statePatchMatchesInline, true, "statePatchMatchesInline");
  equal(report.rejected, 0, "rejected");
  atLeast(report.batches, 1, "batches");
  atLeast(report.accepted, 1, "accepted");
  equal(report.accepted, report.batchSize * report.batches, "accepted");
  atLeast(report.durationMsObserved, 300_000, "durationMsObserved");
  below(report.eventLoopDelayP99Ms, 50, "eventLoopDelayP99Ms");

  if (candidate) {
    atLeast(report.durationMsRequested, 300_000, "durationMsRequested");
    atLeast(
      report.durationMsObserved,
      report.durationMsRequested,
      "durationMsObserved against requested duration",
    );
    nonEmptyString(report.chunkAbExperimentId, "chunkAbExperimentId");
    nonEmptyString(report.corpusPath, "corpusPath");
    nonEmptyString(report.corpusManifestPath, "corpusManifestPath");
    const corpusSha256 = nonEmptyString(report.corpusSha256, "corpusSha256");
    if (!/^[0-9a-f]{64}$/u.test(corpusSha256)) {
      fail("corpusSha256 must be an exact lowercase SHA-256");
    }
    positiveSafeInteger(report.corpusRowCount, "corpusRowCount");
  }
  return report;
};
