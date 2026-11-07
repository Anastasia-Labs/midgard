import {
  isBoundedNonEmptyString,
  isNonEmptyString,
  jsonEqual,
} from "./mpf-architecture-g-gate-config.capture-architecture-gphase1-formal-binding-identity.mjs";
import {
  isHash,
  isNonNegativeFiniteNumber,
  isNonNegativeSafeInteger,
  requireExactObjectKeys,
} from "./mpf-architecture-g-gate-config.validate-architecture-gfixture-creation-evidence.mjs";

export const validateArchitectureGCorpusFundingV1 = ({
  artifact,
  expectedCorpusSha256,
  expectedSliceSha256,
  expectedFundingRoots,
}) => {
  requireExactObjectKeys(
    artifact,
    ["schemaVersion", "corpusSha256", "sliceSha256", "entries"],
    "Architecture G corpus funding",
  );
  if (
    artifact.schemaVersion !== "midgard-architecture-g-corpus-funding-v1" ||
    !isHash(expectedCorpusSha256) ||
    artifact.corpusSha256 !== expectedCorpusSha256 ||
    !isHash(expectedSliceSha256) ||
    artifact.sliceSha256 !== expectedSliceSha256 ||
    !Array.isArray(artifact.entries) ||
    artifact.entries.length === 0 ||
    !Array.isArray(expectedFundingRoots) ||
    expectedFundingRoots.length !== artifact.entries.length
  ) {
    throw new Error("Architecture G corpus funding identity is invalid");
  }
  const walletIds = new Set();
  const outrefs = new Set();
  const identities = artifact.entries.map((value, index) => {
    const entry = requireExactObjectKeys(
      value,
      ["walletId", "outref", "outputCbor"],
      `Architecture G funding entry ${index.toString()}`,
    );
    if (
      !isNonEmptyString(entry.walletId) ||
      !isNonEmptyString(entry.outref) ||
      !isBoundedNonEmptyString(entry.outputCbor, 1_048_576) ||
      walletIds.has(entry.walletId) ||
      outrefs.has(entry.outref) ||
      entry.outref !== entry.outref.toLowerCase() ||
      entry.outputCbor !== entry.outputCbor.toLowerCase() ||
      !/^[0-9a-f]{64}#(?:0|[1-9]\d*)$/u.test(entry.outref) ||
      entry.outputCbor.length % 2 !== 0 ||
      entry.outputCbor.length > 1_048_576 ||
      Buffer.from(entry.outputCbor, "hex").toString("hex") !== entry.outputCbor
    ) {
      throw new Error(
        `Architecture G funding entry ${index.toString()} is invalid or duplicated`,
      );
    }
    walletIds.add(entry.walletId);
    outrefs.add(entry.outref);
    return { walletId: entry.walletId, outref: entry.outref };
  });
  for (const [index, value] of expectedFundingRoots.entries()) {
    requireExactObjectKeys(
      value,
      ["walletId", "outref"],
      `Expected Architecture G funding root ${index.toString()}`,
    );
  }
  if (!jsonEqual(identities, expectedFundingRoots)) {
    throw new Error(
      "Architecture G corpus funding entries do not match the selected corpus roots",
    );
  }
  return artifact;
};

const ARCHITECTURE_G_OWNER_DIAGNOSTIC_KEYS = Object.freeze([
  "ownerEpoch",
  "durableRoot",
  "residentNodes",
  "residentEdges",
  "residentBytes",
  "activeGenerations",
  "generatedNodes",
  "generatedBytes",
  "rssBytes",
  "peakRssBytes",
  "childRestarts",
]);

export const validateArchitectureGOwnerDiagnostics = (owner, label) => {
  requireExactObjectKeys(owner, ARCHITECTURE_G_OWNER_DIAGNOSTIC_KEYS, label);
  requireExactObjectKeys(owner.ownerEpoch, ["type", "data"], `${label} epoch`);
  if (
    owner.ownerEpoch.type !== "Buffer" ||
    !Array.isArray(owner.ownerEpoch.data) ||
    owner.ownerEpoch.data.length !== 16 ||
    !owner.ownerEpoch.data.every(
      (byte) => Number.isInteger(byte) && byte >= 0 && byte <= 255,
    ) ||
    !isHash(owner.durableRoot) ||
    !ARCHITECTURE_G_OWNER_DIAGNOSTIC_KEYS.slice(2).every((field) =>
      isNonNegativeSafeInteger(owner[field]),
    )
  ) {
    throw new Error(`${label} is invalid`);
  }
  return owner;
};

export const validateArchitectureGRootGateResultShape = (result) => {
  requireExactObjectKeys(
    result,
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
    "Architecture G root-gate result",
  );
  for (const [value, keys, label] of [
    [
      result.phaseMs,
      [
        "transactionSourceRoot",
        "transitionTraceBuild",
        "transactionMpfApply",
        "auxiliaryRoots",
      ],
      "Architecture G result phase timings",
    ],
    [
      result.nativePhaseMs,
      [
        "validation",
        "eventLogEncode",
        "ownerApply",
        "ownerProofArena",
        "ownerMutation",
        "memberAssembly",
        "retainedRoots",
      ],
      "Architecture G result native phase timings",
    ],
    [
      result.pathHydration,
      [
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
      ],
      "Architecture G result path-hydration diagnostics",
    ],
  ]) {
    requireExactObjectKeys(value, keys, label);
  }
  for (const [value, keys, label] of [
    [
      result.canonicalCorpusSlice,
      ["path", "sha256", "rowCount"],
      "Architecture G result corpus slice",
    ],
    [
      result.canonicalFunding,
      ["path", "sha256", "entryCount"],
      "Architecture G result funding identity",
    ],
  ]) {
    if (value !== null) requireExactObjectKeys(value, keys, label);
  }
  for (const [owner, label] of [
    [result.ownerBefore, "Architecture G owner-before diagnostics"],
    [result.ownerAfter, "Architecture G owner-after diagnostics"],
  ]) {
    validateArchitectureGOwnerDiagnostics(owner, label);
  }
  const hydrationTimingFields = new Set([
    "prefetchMs",
    "checkpointMs",
    "authenticationMs",
    "materializeMs",
    "collapseMs",
  ]);
  if (
    Object.entries(result.pathHydration).some(([field, value]) =>
      hydrationTimingFields.has(field)
        ? !isNonNegativeFiniteNumber(value)
        : !isNonNegativeSafeInteger(value),
    )
  ) {
    throw new Error(
      "Architecture G path-hydration diagnostics contain an invalid value",
    );
  }
  if (!Array.isArray(result.transitionRoots)) {
    throw new Error("Architecture G transition roots must be an array");
  }
  for (const transition of result.transitionRoots) {
    requireExactObjectKeys(
      transition,
      ["pre", "post"],
      "Architecture G transition-root pair",
    );
  }
  return result;
};

export const percentile = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};
