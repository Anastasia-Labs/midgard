import { createHash } from "node:crypto";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  ogmiosEndpointIdentitySha256,
  parseOgmiosShelleyGenesisSlotConfig,
} from "../src/local-ledger-slot.js";
import {
  assertArchitectureGCandidateSlotRuntimeIdentity,
  decodeArchitectureGCommitCandidateInput,
  decodeArchitectureGFixtureCreation,
  validateArchitectureGCommitCandidateProbeResult,
  validateArchitectureGRootProbeResult,
} from "../src/workers/utils/mpf-commit-candidate-artifacts.js";

const hash = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

const submittedTxHash = hash(20);
const fixtureRoot = hash(21);
const currentBlockStartTimeMs = 1_700_000_000_000;
const slotConfig = {
  zeroTime: 1_655_769_600_000,
  zeroSlot: 86_400,
  slotLength: 1_000,
};
const slotConfigDocument = {
  schemaVersion: "midgard-node-slot-config-evidence-v1",
  capturedAtIso: "2026-07-28T00:00:00.000Z",
  network: "Preprod",
  source: {
    kind: "lucid_network_table",
    lucidVersion: "0.6.0",
  },
  slotConfig: { ...slotConfig },
};
const evidenceDirectory = mkdtempSync(
  join(tmpdir(), "midgard-slot-config-artifact-"),
);
const slotConfigArtifactPath = join(evidenceDirectory, "slot-config.json");
const slotConfigArtifactBytes = Buffer.from(
  `${JSON.stringify(slotConfigDocument)}\n`,
);
writeFileSync(slotConfigArtifactPath, slotConfigArtifactBytes);
const slotConfigArtifactSha256 = createHash("sha256")
  .update(slotConfigArtifactBytes)
  .digest("hex");
const customOgmiosUrl = "ws://127.0.0.1:1337/";
const customOgmiosPayload = {
  jsonrpc: "2.0",
  result: {
    startTime: "2022-06-21T00:00:00.000Z",
    slotLength: { milliseconds: 1_000 },
  },
  id: "midgard-custom-slot-config",
};
const customGenesis = parseOgmiosShelleyGenesisSlotConfig(customOgmiosPayload);
const customSlotConfigDocument = {
  schemaVersion: "midgard-node-slot-config-evidence-v1",
  capturedAtIso: "2026-07-28T00:00:00.000Z",
  network: "Custom",
  source: {
    kind: "local_ogmios_genesis",
    endpointIdentitySha256: ogmiosEndpointIdentitySha256(customOgmiosUrl),
    configurationSha256: customGenesis.configurationSha256,
  },
  slotConfig: {
    zeroTime: customGenesis.startTimeMs,
    zeroSlot: 0,
    slotLength: customGenesis.slotLengthMs,
  },
};
const customSlotConfigArtifactPath = join(
  evidenceDirectory,
  "custom-slot-config.json",
);
const customSlotConfigArtifactBytes = Buffer.from(
  `${JSON.stringify(customSlotConfigDocument)}\n`,
);
writeFileSync(customSlotConfigArtifactPath, customSlotConfigArtifactBytes);
const customSlotConfigArtifactSha256 = createHash("sha256")
  .update(customSlotConfigArtifactBytes)
  .digest("hex");
afterAll(() => rmSync(evidenceDirectory, { recursive: true, force: true }));

const candidateInput = () => ({
  schemaVersion: "midgard-architecture-g-commit-candidate-input-v1",
  phase1FormalBinding: {
    schemaVersion: "midgard-architecture-g-phase1-formal-binding-identity-v1",
    path: "/evidence/phase1-formal-binding.json",
    sha256: hash(1),
    deploymentManifestId: "deployment-manifest-id",
    nodeImageId: "sha256:node-image",
    nodeContainerId: "node-container-id",
    walletSetSha256: hash(2),
    fundingSetSha256: hash(3),
    corpus: {
      path: "/evidence/corpus.ndjson",
      indexPath: "/evidence/corpus.ndjson.index.ndjson",
      manifestPath: "/evidence/corpus.ndjson.manifest.json",
      sliceId: "phase1-live",
      corpusSha256: hash(4),
      indexSha256: hash(5),
      manifestSha256: hash(6),
    },
    generationResult: {
      path: "/evidence/generation-result.json",
      sha256: hash(7),
      schemaVersion: "midgard-stress-corpus-generation-v1",
    },
    harness: { scenarioId: hash(8), engineId: hash(9) },
  },
  runtimeIdentity: {
    schemaVersion: "midgard-architecture-g-runtime-identity-v1",
    version: "v22.22.2",
    execPath: "/opt/node-v22.22.2/bin/node",
    executableSha256: hash(10),
  },
  levelPath: "/evidence/architecture-g-level",
  binaryPath: "/evidence/mpf-native-owner",
  binarySha256: hash(11),
  sidecarPath: "/evidence/mpf-native-owner-sidecar.mjs",
  expectedTransactionCount: 2,
  corpusSha256: hash(4),
  corpusSliceSha256: hash(12),
  fundingMapSha256: hash(13),
  fixtureCreationPath: "/evidence/fixture-creation.json",
  fixtureCreationSha256: hash(14),
  fixtureInitialUtxoCount: 2,
  baseUtxoPayloadAggregate: {
    entryCount: 2,
    encodedTupleBytes: 1024,
  },
  forcedValidationSlotConfigArtifact: {
    path: slotConfigArtifactPath,
    sha256: slotConfigArtifactSha256,
    document: structuredClone(slotConfigDocument),
  },
  workerInput: {
    data: {
      availableConfirmedBlock: "",
      availableLocalFinalizationBlock: "",
      currentBlockStartTimeMs,
      forcedValidationSlotConfig: { ...slotConfig },
      localFinalizationPending: false,
      ledgerStoreLeaseOwner: "commit:12345678-1234-4123-8123-123456789abc",
      mempoolTxsCountSoFar: 0,
      sizeOfProcessedTxsSoFar: 0,
      baseSnapshotId: `architecture-g-candidate:${submittedTxHash}`,
      stateQueueHasUnmergedTail: true,
      speculativeBuild: {
        base: {
          headerHash: submittedTxHash.slice(0, 56),
          utxosRoot: fixtureRoot,
          blockEndTimeMs: currentBlockStartTimeMs,
          submittedTxHash,
        },
        watermarks: {
          depositMs: currentBlockStartTimeMs + 1,
          withdrawalMs: currentBlockStartTimeMs + 2,
          txOrderMs: currentBlockStartTimeMs + 3,
          refreshedAtMs: currentBlockStartTimeMs + 3,
        },
        excludedMempoolTxIds: [] as string[],
        excludedDepositEventIds: [] as string[],
        excludedForcedTransactionEventIds: [] as string[],
        excludedWithdrawalEventIds: [] as string[],
      },
    },
  },
});

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

const fixtureCreation = () => ({
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

const ownerDiagnostics = (durableRoot: string) => ({
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

const candidateProbeResult = () => {
  const input = candidateInput();
  const candidateWatermarks = structuredClone(
    input.workerInput.data.speculativeBuild.watermarks,
  );
  const candidateEndTimeMs = 2_000;
  // The commit worker derives the invalidation key from the candidate end time
  // and the minimum of the three barrier watermarks
  // (`minimumBarrierWatermarkMs`), so the fixture must derive it the same way
  // instead of pinning a literal that silently drifts from the watermarks.
  const candidateInvalidationKey = `${input.workerInput.data.speculativeBuild.base.headerHash}:${candidateEndTimeMs.toString()}:${Math.min(
    candidateWatermarks.depositMs,
    candidateWatermarks.withdrawalMs,
    candidateWatermarks.txOrderMs,
  ).toString()}`;
  return {
    schemaVersion: "midgard-architecture-g-commit-candidate-probe-v1",
    probePath: "/probes/mpf-commit-candidate-probe.js",
    probeSha256: hash(30),
    inputPath: "/evidence/candidate-input.json",
    inputSha256: hash(31),
    expectedTransactionCount: 2,
    corpusSha256: input.corpusSha256,
    corpusSliceSha256: input.corpusSliceSha256,
    fundingMapSha256: input.fundingMapSha256,
    fixtureCreationSha256: input.fixtureCreationSha256,
    fixtureInitialUtxoCount: input.fixtureInitialUtxoCount,
    baseUtxoPayloadAggregate: structuredClone(input.baseUtxoPayloadAggregate),
    binarySha256: input.binarySha256,
    cpuAffinity: "2-3",
    durationMs: 10,
    confirmedLedgerFullScans: 0,
    journalRowsBefore: 0,
    journalRowsAfter: 0,
    candidateConfig: {
      mpfEngine: "architecture_g",
      scratchBuild: "fromlist",
      payloadRootCheck: "off",
      parallelRoots: true,
      costModel: "ewma",
      mempoolRetrievePageSize: 2,
      maxL2TxCount: 2,
      maxLedgerOpCount: 6,
      maxTransitionStepCount: 2,
    },
    providerBoundaryAttempts: 0,
    submissionAttempts: 0,
    candidate: {
      candidateId: "123e4567-e89b-42d3-a456-426614174000",
      baseHeaderHash: input.workerInput.data.speculativeBuild.base.headerHash,
      endTimeMs: candidateEndTimeMs,
      builtAtMs: 2_001,
      buildDurationMs: 9,
      invalidationKey: candidateInvalidationKey,
      watermarks: candidateWatermarks,
      expectedUserEventCounts: {
        deposits: 0,
        forcedTransactions: 0,
        withdrawals: 0,
      },
      expectedL2TransactionCount: 2,
      roots: Object.fromEntries(
        [
          "utxos",
          "rawTransactions",
          "transactions",
          "deposits",
          "forcedTransactions",
          "withdrawals",
          "transitionTrace",
          "eventToStep",
        ].map((name, index) => [name, hash(40 + index)]),
      ),
    },
    ownerBefore: ownerDiagnostics(fixtureRoot),
    ownerAfter: ownerDiagnostics(fixtureRoot),
  };
};

const rootProbeResult = () => {
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

const validateFixture = (value: unknown) =>
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

describe("Architecture G commit-candidate probe V1 artifacts", () => {
  it("accepts the complete canonical candidate input", () => {
    const value = candidateInput();
    expect(decodeArchitectureGCommitCandidateInput(value)).toBe(value);
  });

  it("binds standard and Custom slot evidence to the runtime node identity", () => {
    const standard = decodeArchitectureGCommitCandidateInput(candidateInput());
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentity({
        input: standard,
        runtimeNetwork: "Preprod",
      }),
    ).not.toThrow();
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentity({
        input: standard,
        runtimeNetwork: "Mainnet",
      }),
    ).toThrow(/does not match NodeConfig\.NETWORK/u);

    const standardValue = candidateInput();
    const customValue = {
      ...standardValue,
      forcedValidationSlotConfigArtifact: {
        path: customSlotConfigArtifactPath,
        sha256: customSlotConfigArtifactSha256,
        document: structuredClone(customSlotConfigDocument),
      },
      workerInput: {
        data: {
          ...standardValue.workerInput.data,
          forcedValidationSlotConfig: {
            ...customSlotConfigDocument.slotConfig,
          },
        },
      },
    };
    const custom = decodeArchitectureGCommitCandidateInput(customValue);
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentity({
        input: custom,
        runtimeNetwork: "Custom",
        ogmiosUrl: customOgmiosUrl,
        customGenesis,
      }),
    ).not.toThrow();
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentity({
        input: custom,
        runtimeNetwork: "Custom",
        ogmiosUrl: customOgmiosUrl,
        customGenesis: {
          ...customGenesis,
          configurationSha256: hash(99),
        },
      }),
    ).toThrow(/does not match the live configured Ogmios genesis/u);
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentity({
        input: custom,
        runtimeNetwork: "Custom",
        ogmiosUrl: "ws://127.0.0.1:2337/",
        customGenesis,
      }),
    ).toThrow(/does not match the live configured Ogmios genesis/u);
  });

  it.each([
    (value: ReturnType<typeof candidateInput>) =>
      Object.assign(value, { unknown: true }),
    (value: ReturnType<typeof candidateInput>) =>
      Object.assign(value.workerInput.data, { unknown: true }),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.levelPath = "relative/level"),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.levelPath = "/evidence/\0architecture-g-level"),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.corpusSha256 = hash(30)),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.workerInput.data.ledgerStoreLeaseOwner = "commit:shared"),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.workerInput.data.baseSnapshotId = "candidate"),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.workerInput.data.speculativeBuild.base.headerHash = hash(
        31,
      ).slice(0, 56)),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.workerInput.data.speculativeBuild.base.blockEndTimeMs =
        currentBlockStartTimeMs - 1),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.workerInput.data.speculativeBuild.watermarks.depositMs =
        currentBlockStartTimeMs),
    (value: ReturnType<typeof candidateInput>) =>
      void value.workerInput.data.speculativeBuild.excludedMempoolTxIds.push(
        hash(32),
      ),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.baseUtxoPayloadAggregate.entryCount = 1),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.workerInput.data.forcedValidationSlotConfig.slotLength = 2_000),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.forcedValidationSlotConfigArtifact.document.slotConfig.slotLength = 2_000),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.forcedValidationSlotConfigArtifact.sha256 = "invalid"),
  ])("rejects extended, mismatched, or unsafe candidate input %#", (mutate) => {
    const value = candidateInput();
    mutate(value);
    expect(() => decodeArchitectureGCommitCandidateInput(value)).toThrow();
  });

  it("accepts the complete fixture artifact and binds canonical funding", () => {
    const value = fixtureCreation();
    expect(validateFixture(value)).toBe(value);
  });

  it("accepts a synthetic fixture only when canonical funding is explicitly absent", () => {
    const value = { ...fixtureCreation(), canonicalFunding: null };
    expect(
      decodeArchitectureGFixtureCreation({
        value,
        expectedFixturePath: "/evidence/architecture-g-level",
        expectedMarker: fixtureRoot,
        expectedUtxos: 2,
        expectedAggregate: {
          entryCount: 2,
          encodedTupleBytes: 1_024,
        },
        expectedFundingMapSha256: null,
      }),
    ).toBe(value);
  });

  it.each([
    (value: ReturnType<typeof fixtureCreation>) =>
      Object.assign(value, { unknown: true }),
    (value: ReturnType<typeof fixtureCreation>) =>
      Object.assign(value.diagnostics, { unknown: 0 }),
    (value: ReturnType<typeof fixtureCreation>) =>
      void (value.fixturePath = "/evidence/other-level"),
    (value: ReturnType<typeof fixtureCreation>) =>
      void (value.marker = hash(40)),
    (value: ReturnType<typeof fixtureCreation>) => void (value.durationMs = 0),
    (value: ReturnType<typeof fixtureCreation>) =>
      void (value.diagnostics.flushMs = Number.NaN),
    (value: ReturnType<typeof fixtureCreation>) =>
      void (value.diagnostics.entries = -1),
    (value: ReturnType<typeof fixtureCreation>) =>
      void (value.utxoPayloadAggregate.encodedTupleBytes = 1023),
    (value: ReturnType<typeof fixtureCreation>) =>
      Object.assign(value.canonicalFunding, { unknown: true }),
    (value: ReturnType<typeof fixtureCreation>) =>
      void (value.canonicalFunding.sha256 = hash(41)),
    (value: ReturnType<typeof fixtureCreation>) =>
      void (value.canonicalFunding.entryCount = 0),
    (value: ReturnType<typeof fixtureCreation>) =>
      void Object.assign(value, { canonicalFunding: null }),
  ])(
    "rejects extended, mismatched, or unsafe fixture evidence %#",
    (mutate) => {
      const value = fixtureCreation();
      mutate(value);
      expect(() => validateFixture(value)).toThrow();
    },
  );

  it("validates the exact candidate-probe artifact before emission", () => {
    const input = candidateInput();
    const valid = candidateProbeResult();
    const validate = (value: unknown) =>
      validateArchitectureGCommitCandidateProbeResult({
        value,
        expectedInput: decodeArchitectureGCommitCandidateInput(input),
        expectedInputPath: "/evidence/candidate-input.json",
        expectedInputSha256: hash(31),
        expectedProbePath: "/probes/mpf-commit-candidate-probe.js",
        expectedProbeSha256: hash(30),
        expectedCpuAffinity: "2-3",
      });
    expect(validate(valid)).toEqual(valid);
    const liveRuntimeShape = candidateProbeResult() as unknown as {
      readonly ownerBefore: Record<string, unknown>;
      readonly ownerAfter: Record<string, unknown>;
    };
    liveRuntimeShape.ownerBefore.ownerEpoch = Buffer.alloc(16, 7);
    liveRuntimeShape.ownerAfter.ownerEpoch = Buffer.alloc(16, 7);
    expect(validate(liveRuntimeShape)).toEqual(valid);
    for (const mutate of [
      (value: ReturnType<typeof candidateProbeResult>) =>
        Object.assign(value, { unknown: true }),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.schemaVersion = "candidate-probe-v2"),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.corpusSha256 = hash(80)),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.fixtureInitialUtxoCount = 1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        Object.assign(value.candidate, { unknown: true }),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidate.baseHeaderHash = hash(81).slice(0, 56)),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidate.watermarks.withdrawalMs = 1_001),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidate.invalidationKey = `${value.candidate.baseHeaderHash}:2000:1001`),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidate.endTimeMs = 2_001),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidate.roots.utxos = "bad"),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidateConfig.maxLedgerOpCount = 5),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.providerBoundaryAttempts = 1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.ownerAfter.durableRoot = hash(82)),
    ]) {
      const invalid = structuredClone(valid);
      mutate(invalid);
      expect(() => validate(invalid)).toThrow();
    }
  });

  it("validates the exact Architecture G root-probe artifact before emission", () => {
    const valid = rootProbeResult();
    const validate = (value: unknown) =>
      validateArchitectureGRootProbeResult({
        value,
        expectedTransactionCount: 2,
        expectedInitialUtxoCount: 100,
        expectedProbePath: "/probes/mpf-engine-probe.js",
        expectedProbeSha256: hash(73),
      });
    expect(validate(valid)).toEqual(valid);
    const liveRuntimeShape = rootProbeResult() as unknown as {
      readonly ownerBefore: Record<string, unknown>;
      readonly ownerAfter: Record<string, unknown>;
    };
    liveRuntimeShape.ownerBefore.ownerEpoch = Buffer.alloc(16, 7);
    liveRuntimeShape.ownerAfter.ownerEpoch = Buffer.alloc(16, 7);
    expect(validate(liveRuntimeShape)).toEqual(valid);
    for (const mutate of [
      (value: ReturnType<typeof rootProbeResult>) =>
        Object.assign(value, { unknown: true }),
      (value: ReturnType<typeof rootProbeResult>) =>
        void (value.engine = "overlay"),
      (value: ReturnType<typeof rootProbeResult>) =>
        void (value.canonicalCorpusSlice.rowCount = 1),
      (value: ReturnType<typeof rootProbeResult>) =>
        Object.assign(value.canonicalFunding, { unknown: true }),
      (value: ReturnType<typeof rootProbeResult>) =>
        Object.assign(value.pathHydration, { unknown: 0 }),
      (value: ReturnType<typeof rootProbeResult>) =>
        void (value.transitionRoots[1]!.pre = hash(83)),
      (value: ReturnType<typeof rootProbeResult>) =>
        void (value.ownerAfter.childRestarts = 1),
      (value: ReturnType<typeof rootProbeResult>) =>
        void (value.probeSha256 = hash(84)),
      (value: ReturnType<typeof rootProbeResult>) =>
        void (value.buildPlusCaptureMs = 11),
    ]) {
      const invalid = structuredClone(valid);
      mutate(invalid);
      expect(() => validate(invalid)).toThrow();
    }
  });
});
