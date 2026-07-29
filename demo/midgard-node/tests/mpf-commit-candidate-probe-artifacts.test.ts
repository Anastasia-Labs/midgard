import { createHash } from "node:crypto";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  ogmiosEndpointIdentitySha256,
  parseOgmiosShelleyGenesisSlotConfig,
} from "@/local-ledger-slot.js";
import {
  assertArchitectureGCandidateSlotRuntimeIdentityV1,
  decodeArchitectureGCommitCandidateInputV1,
  decodeArchitectureGFixtureCreationV1,
} from "@/workers/utils/mpf-commit-candidate-artifacts.js";

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

const validateFixture = (value: unknown) =>
  decodeArchitectureGFixtureCreationV1({
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
    expect(decodeArchitectureGCommitCandidateInputV1(value)).toBe(value);
  });

  it("binds standard and Custom slot evidence to the runtime node identity", () => {
    const standard =
      decodeArchitectureGCommitCandidateInputV1(candidateInput());
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentityV1({
        input: standard,
        runtimeNetwork: "Preprod",
      }),
    ).not.toThrow();
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentityV1({
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
    const custom = decodeArchitectureGCommitCandidateInputV1(customValue);
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentityV1({
        input: custom,
        runtimeNetwork: "Custom",
        ogmiosUrl: customOgmiosUrl,
        customGenesis,
      }),
    ).not.toThrow();
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentityV1({
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
      assertArchitectureGCandidateSlotRuntimeIdentityV1({
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
    expect(() => decodeArchitectureGCommitCandidateInputV1(value)).toThrow();
  });

  it("accepts the complete fixture artifact and binds canonical funding", () => {
    const value = fixtureCreation();
    expect(validateFixture(value)).toBe(value);
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
});
