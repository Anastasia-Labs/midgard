import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { decodeArchitectureGFixtureCreation } from "../src/workers/utils/mpf-commit-candidate-artifacts.js";

export const hash = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

export const submittedTxHash = hash(20);

export const fixtureRoot = hash(21);

export const currentBlockStartTimeMs = 1_700_000_000_000;

export const slotConfig = {
  zeroTime: 1_655_769_600_000,
  zeroSlot: 86_400,
  slotLength: 1_000,
};

export const slotConfigDocument = {
  schemaVersion: "midgard-node-slot-config-evidence-v1",
  capturedAtIso: "2026-07-28T00:00:00.000Z",
  network: "Preprod",
  source: {
    kind: "lucid_network_table",
    lucidVersion: "0.6.0",
  },
  slotConfig: { ...slotConfig },
};

export const evidenceDirectory = mkdtempSync(
  join(tmpdir(), "midgard-slot-config-artifact-"),
);

export const slotConfigArtifactPath = join(
  evidenceDirectory,
  "slot-config.json",
);

export const slotConfigArtifactBytes = Buffer.from(
  `${JSON.stringify(slotConfigDocument)}\n`,
);

const diagnostics = () => ({
  entries: 2,
  storePuts: 2,
  storeDels: 0,
  serialiseCalls: 1,
  serialiseMs: 0.5,
  deferredMaterializedEstimatedBytes: 0,
  deferredMaterializedActualBytes: 0,
  deferredLazyReads: 0,
  deferredLazySerialiseMs: 0,
  deferredLazySerialisedBytes: 0,
  arenaCheckpointCalls: 0,
  arenaCheckpointMs: 0,
  arenaCheckpointNodes: 0,
  arenaCheckpointBytes: 0,
  pathCacheEntries: 0,
  pathCacheBytes: 0,
  pathCacheHits: 0,
  liveArenaPrunedNodes: 0,
  liveArenaPromotedNodes: 0,
  liveArenaPromotedBytes: 0,
  retainedSnapshotAuthentications: 0,
  retainedSnapshotAuthenticationMs: 0,
  transientLiveNodes: 0,
  transientLiveBytes: 0,
  transientDirtyNodes: 0,
  transientSnapshotsCaptured: 0,
  eventAtomicFinalizations: 0,
  eventAtomicDirtyNodes: 0,
  eventAtomicMaxDirtyNodes: 0,
  levelGets: 0,
  levelGetManyCalls: 0,
  levelGetManyMaxKeys: 0,
  levelGetMs: 0,
  jsonCodecMs: 0,
  overlayHits: 0,
  readCacheHits: 0,
  levelBatchWrites: 1,
  bytesFlushed: 1024,
  overlayEntries: 0,
  overlayBytes: 0,
  overlaySpills: 0,
  overlaySpillMs: 0,
  flushMs: 0.25,
});

export const fixtureCreation = () => ({
  fixtureCreated: true,
  fixturePath: "/evidence/architecture-g-level",
  initialUtxoCount: 2,
  marker: fixtureRoot,
  durationMs: 12.5,
  diagnostics: diagnostics(),
  utxoPayloadAggregate: {
    entryCount: 2,
    encodedTupleBytes: 1024,
  },
  canonicalFunding: {
    path: "/evidence/canonical-corpus-funding.json",
    sha256: hash(13),
    entryCount: 1,
  },
});

export const ownerDiagnostics = (durableRoot: string) => ({
  ownerEpoch: { type: "Buffer", data: Array(16).fill(7) },
  durableRoot,
  residentNodes: 10,
  residentEdges: 9,
  residentBytes: 1_024,
  activeGenerations: 0,
  generatedNodes: 20,
  generatedBytes: 2_048,
  rssBytes: 4_096,
  peakRssBytes: 8_192,
  childRestarts: 0,
});

export const rootProbeResult = () => {
  const middleRoot = hash(60);
  const utxoRoot = hash(61);
  return {
    engine: "architecture_g",
    transactionCount: 2,
    initialUtxoCount: 100,
    workloadSha256: hash(62),
    canonicalCorpusSlice: {
      path: "/evidence/canonical-corpus-slice.ndjson",
      sha256: hash(63),
      rowCount: 2,
    },
    canonicalFunding: {
      path: "/evidence/canonical-corpus-funding.json",
      sha256: hash(64),
      entryCount: 1,
    },
    levelBackedInitialView: true,
    reusedLevelFixture: true,
    ledgerOpCount: 6,
    startupMs: 1,
    durationMs: 10,
    buildPlusCaptureMs: 10,
    phaseMs: {
      transactionSourceRoot: 1,
      transitionTraceBuild: 2,
      transactionMpfApply: 3,
      auxiliaryRoots: 4,
    },
    utxoRoot,
    rawTxRoot: hash(65),
    txRoot: hash(66),
    transitionTraceRoot: hash(67),
    eventToStepRoot: hash(68),
    depositsRoot: hash(69),
    withdrawalsRoot: hash(70),
    forcedTransactionsRoot: hash(71),
    transitionRoots: [
      { pre: fixtureRoot, post: middleRoot },
      { pre: middleRoot, post: utxoRoot },
    ],
    nativePhaseMs: {
      validation: 1,
      eventLogEncode: 2,
      ownerApply: 3,
      ownerProofArena: 4,
      ownerMutation: 5,
      memberAssembly: 6,
      retainedRoots: 7,
    },
    pathHydration: {
      prefetchMs: 0,
      uniquePaths: 2,
      nodesRequested: 2,
      hydrationHits: 2,
      hydrationMisses: 0,
      loadedNodes: 0,
      maxInFlight: 1,
      maxBatchKeys: 2,
      maxFrontierPaths: 2,
      retainedBytesEstimate: 128,
      chunkCount: 1,
      checkpointMs: 0,
      authenticationMs: 0,
      materializeMs: 0,
      collapseMs: 0,
      checkpointSerializedNodes: 0,
      checkpointSerializedBytes: 0,
      verifiedUpperNodes: 1,
      retainedUpperNodes: 1,
      collapsedNodes: 0,
      peakDecodedNodes: 2,
    },
    confirmedLedgerFullScans: 0,
    binarySha256: hash(72),
    cpuAffinity: "2-3",
    ownerBefore: ownerDiagnostics(fixtureRoot),
    ownerAfter: ownerDiagnostics(fixtureRoot),
    probePath: "/probes/mpf-engine-probe.js",
    probeSha256: hash(73),
  };
};

export const validateFixture = (value: unknown) =>
  decodeArchitectureGFixtureCreation({
    value,
    expectedFixturePath: "/evidence/architecture-g-level",
    expectedMarker: fixtureRoot,
    expectedUtxos: 2,
    expectedAggregate: {
      entryCount: 2,
      encodedTupleBytes: 1024,
    },
    expectedFundingMapSha256: hash(13),
  });
