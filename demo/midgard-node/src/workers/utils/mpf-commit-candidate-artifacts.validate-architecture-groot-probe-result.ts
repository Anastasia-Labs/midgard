import {
  boundedNonEmptyString,
  canonicalAbsolutePath,
  exactKeysRecord,
  nonNegativeFiniteNumber,
  nonNegativeSafeInteger,
  positiveFiniteNumber,
  positiveSafeInteger,
  sha256Digest,
} from "../../artifact-schema.js";
import {
  decodeArchitectureGOwnerDiagnostics,
  type JsonRecord,
  sameJson,
} from "./mpf-commit-candidate-artifacts.architecture-gcommit-candidate-input.js";

export const validateArchitectureGRootProbeResult = ({
  value,
  expectedTransactionCount,
  expectedInitialUtxoCount,
  expectedProbePath,
  expectedProbeSha256,
}: {
  readonly value: unknown;
  readonly expectedTransactionCount: number;
  readonly expectedInitialUtxoCount: number;
  readonly expectedProbePath: string;
  readonly expectedProbeSha256: string;
}): JsonRecord => {
  const result = exactKeysRecord(
    JSON.parse(JSON.stringify(value)) as unknown,
    "Architecture G root-probe result",
    [
      "engine",
      "transactionCount",
      "initialUtxoCount",
      "workloadSha256",
      "canonicalCorpusSlice",
      "canonicalFunding",
      "levelBackedInitialView",
      "reusedLevelFixture",
      "ledgerOpCount",
      "startupMs",
      "durationMs",
      "buildPlusCaptureMs",
      "phaseMs",
      "utxoRoot",
      "rawTxRoot",
      "txRoot",
      "transitionTraceRoot",
      "eventToStepRoot",
      "depositsRoot",
      "withdrawalsRoot",
      "forcedTransactionsRoot",
      "transitionRoots",
      "nativePhaseMs",
      "pathHydration",
      "confirmedLedgerFullScans",
      "binarySha256",
      "cpuAffinity",
      "ownerBefore",
      "ownerAfter",
      "probePath",
      "probeSha256",
    ],
  );
  const phaseMs = exactKeysRecord(
    result.phaseMs,
    "Architecture G root-probe phase timings",
    [
      "transactionSourceRoot",
      "transitionTraceBuild",
      "transactionMpfApply",
      "auxiliaryRoots",
    ],
  );
  const nativePhaseMs = exactKeysRecord(
    result.nativePhaseMs,
    "Architecture G root-probe native phase timings",
    [
      "validation",
      "eventLogEncode",
      "ownerApply",
      "ownerProofArena",
      "ownerMutation",
      "memberAssembly",
      "retainedRoots",
    ],
  );
  const hydrationKeys = [
    "prefetchMs",
    "uniquePaths",
    "nodesRequested",
    "hydrationHits",
    "hydrationMisses",
    "loadedNodes",
    "maxInFlight",
    "maxBatchKeys",
    "maxFrontierPaths",
    "retainedBytesEstimate",
    "chunkCount",
    "checkpointMs",
    "authenticationMs",
    "materializeMs",
    "collapseMs",
    "checkpointSerializedNodes",
    "checkpointSerializedBytes",
    "verifiedUpperNodes",
    "retainedUpperNodes",
    "collapsedNodes",
    "peakDecodedNodes",
  ] as const;
  const pathHydration = exactKeysRecord(
    result.pathHydration,
    "Architecture G root-probe path-hydration diagnostics",
    hydrationKeys,
  );
  const transactionCount = positiveSafeInteger(
    expectedTransactionCount,
    "expectedTransactionCount",
  );
  const initialUtxoCount = positiveSafeInteger(
    expectedInitialUtxoCount,
    "expectedInitialUtxoCount",
  );
  const probePath = canonicalAbsolutePath(
    expectedProbePath,
    "expectedProbePath",
  );
  const probeSha256 = sha256Digest(expectedProbeSha256, "expectedProbeSha256");
  if (
    result.engine !== "architecture_g" ||
    result.transactionCount !== transactionCount ||
    result.initialUtxoCount !== initialUtxoCount ||
    result.levelBackedInitialView !== true ||
    result.reusedLevelFixture !== true ||
    result.confirmedLedgerFullScans !== 0 ||
    result.probePath !== probePath ||
    result.probeSha256 !== probeSha256
  ) {
    throw new Error("Architecture G root-probe boundary identity is invalid");
  }
  sha256Digest(result.workloadSha256, "rootProbe.workloadSha256");
  sha256Digest(result.binarySha256, "rootProbe.binarySha256");
  boundedNonEmptyString(result.cpuAffinity, "rootProbe.cpuAffinity");
  positiveSafeInteger(result.ledgerOpCount, "rootProbe.ledgerOpCount");
  nonNegativeFiniteNumber(result.startupMs, "rootProbe.startupMs");
  const durationMs = positiveFiniteNumber(
    result.durationMs,
    "rootProbe.durationMs",
  );
  if (result.buildPlusCaptureMs !== durationMs) {
    throw new Error("Architecture G root-probe duration identity drifted");
  }
  for (const [field, timing] of Object.entries(phaseMs)) {
    nonNegativeFiniteNumber(timing, `rootProbe.phaseMs.${field}`);
  }
  for (const [field, timing] of Object.entries(nativePhaseMs)) {
    nonNegativeFiniteNumber(timing, `rootProbe.nativePhaseMs.${field}`);
  }
  const hydrationTimingFields = new Set([
    "prefetchMs",
    "checkpointMs",
    "authenticationMs",
    "materializeMs",
    "collapseMs",
  ]);
  for (const [field, metric] of Object.entries(pathHydration)) {
    if (hydrationTimingFields.has(field)) {
      nonNegativeFiniteNumber(metric, `rootProbe.pathHydration.${field}`);
    } else {
      nonNegativeSafeInteger(metric, `rootProbe.pathHydration.${field}`);
    }
  }
  for (const field of [
    "utxoRoot",
    "rawTxRoot",
    "txRoot",
    "transitionTraceRoot",
    "eventToStepRoot",
    "depositsRoot",
    "withdrawalsRoot",
    "forcedTransactionsRoot",
  ] as const) {
    sha256Digest(result[field], `rootProbe.${field}`);
  }
  if (
    result.canonicalCorpusSlice === null ||
    result.canonicalFunding === null
  ) {
    if (
      result.canonicalCorpusSlice !== null ||
      result.canonicalFunding !== null
    ) {
      throw new Error(
        "Architecture G root-probe corpus and funding identities must be present together",
      );
    }
  } else {
    const canonicalSlice = exactKeysRecord(
      result.canonicalCorpusSlice,
      "Architecture G root-probe corpus slice",
      ["path", "sha256", "rowCount"],
    );
    const canonicalFunding = exactKeysRecord(
      result.canonicalFunding,
      "Architecture G root-probe canonical funding",
      ["path", "sha256", "entryCount"],
    );
    canonicalAbsolutePath(
      canonicalSlice.path,
      "rootProbe.canonicalCorpusSlice.path",
    );
    sha256Digest(
      canonicalSlice.sha256,
      "rootProbe.canonicalCorpusSlice.sha256",
    );
    if (canonicalSlice.rowCount !== transactionCount) {
      throw new Error(
        "Architecture G root-probe corpus row count does not match the workload",
      );
    }
    canonicalAbsolutePath(
      canonicalFunding.path,
      "rootProbe.canonicalFunding.path",
    );
    sha256Digest(canonicalFunding.sha256, "rootProbe.canonicalFunding.sha256");
    positiveSafeInteger(
      canonicalFunding.entryCount,
      "rootProbe.canonicalFunding.entryCount",
    );
  }
  if (
    !Array.isArray(result.transitionRoots) ||
    result.transitionRoots.length !== transactionCount
  ) {
    throw new Error(
      "Architecture G root-probe transition-root count is invalid",
    );
  }
  const transitions = result.transitionRoots.map((value, index) => {
    const transition = exactKeysRecord(
      value,
      `Architecture G transition-root pair ${index.toString()}`,
      ["pre", "post"],
    );
    sha256Digest(transition.pre, `transitionRoots[${index.toString()}].pre`);
    sha256Digest(transition.post, `transitionRoots[${index.toString()}].post`);
    return transition;
  });
  const ownerBefore = decodeArchitectureGOwnerDiagnostics(
    result.ownerBefore,
    "Architecture G root-probe owner-before diagnostics",
  );
  const ownerAfter = decodeArchitectureGOwnerDiagnostics(
    result.ownerAfter,
    "Architecture G root-probe owner-after diagnostics",
  );
  if (
    transitions[0]?.pre !== ownerBefore.durableRoot ||
    transitions.at(-1)?.post !== result.utxoRoot ||
    ownerBefore.durableRoot !== ownerAfter.durableRoot ||
    !sameJson(ownerBefore.ownerEpoch, ownerAfter.ownerEpoch) ||
    ownerBefore.childRestarts !== ownerAfter.childRestarts
  ) {
    throw new Error("Architecture G root-probe owner/root identity drifted");
  }
  for (let index = 1; index < transitions.length; index += 1) {
    if (transitions[index]?.pre !== transitions[index - 1]?.post) {
      throw new Error(
        `Architecture G root-probe transition chain broke at ${index.toString()}`,
      );
    }
  }
  return result;
};
