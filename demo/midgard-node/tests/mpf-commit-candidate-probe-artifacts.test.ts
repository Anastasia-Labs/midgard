import "node:crypto";
import "node:fs";
import "node:os";
import "node:path";
import "vitest";
import "@al-ft/midgard-core/ogmios-slot";
import "../src/workers/utils/mpf-commit-candidate-artifacts.js";
import "./mpf-commit-candidate-probe-artifacts.root-probe-result.js";

import { createHash } from "node:crypto";
import { rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { parseOgmiosShelleyGenesisSlotConfig } from "@al-ft/midgard-core/ogmios-slot";
import { afterAll, describe, expect, it } from "vitest";

import {
  assertArchitectureGCandidateSlotRuntimeIdentity,
  decodeArchitectureGCommitCandidateInput,
  decodeArchitectureGFixtureCreation,
  validateArchitectureGCommitCandidateProbeResult,
  validateArchitectureGRootProbeResult,
} from "../src/workers/utils/mpf-commit-candidate-artifacts.js";
import {
  currentBlockStartTimeMs,
  evidenceDirectory,
  fixtureCreation,
  fixtureRoot,
  hash,
  ownerDiagnostics,
  rootProbeResult,
  slotConfig,
  slotConfigArtifactBytes,
  slotConfigArtifactPath,
  slotConfigDocument,
  submittedTxHash,
  validateFixture,
} from "./mpf-commit-candidate-probe-artifacts.root-probe-result.js";

writeFileSync(slotConfigArtifactPath, slotConfigArtifactBytes);

const slotConfigArtifactSha256 = createHash("sha256")
  .update(slotConfigArtifactBytes)
  .digest("hex");

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
    configurationSha256: customGenesis.configurationSha256,
  },
  slotConfig: {
    zeroTime: customGenesis.startTimeMs,
    zeroSlot: 0,
    slotLength: customGenesis.slotLengthMs,
  },
};

const customLedgerSlotConfig = {
  zeroTime: customGenesis.startTimeMs,
  zeroSlot: 0,
  slotLength: customGenesis.slotLengthMs,
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
  baseUtxosRoot: fixtureRoot,
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
    },
  },
});

const candidateProbeResult = () => {
  const input = candidateInput();
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
    confirmedLedgerFullScans: 1,
    userEventRows: { deposits: 0, forcedTransactions: 0, withdrawals: 0 },
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
    providerReads: 4,
    providerBoundaryAttempts: 0,
    submissionAttempts: 0,
    candidate: {
      endTimeMs: 2_000,
      l2TransactionCount: 2,
      roots: Object.fromEntries(
        [
          "utxos",
          "rawTransactions",
          "transactions",
          "transitionTrace",
          "eventToStep",
        ].map((name, index) => [name, hash(40 + index)]),
      ),
    },
    ownerBefore: ownerDiagnostics(fixtureRoot),
    ownerAfter: ownerDiagnostics(fixtureRoot),
  };
};

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
        ledgerSlotConfig: customLedgerSlotConfig,
      }),
    ).not.toThrow();
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentity({
        input: custom,
        runtimeNetwork: "Custom",
        ledgerSlotConfig: {
          ...customLedgerSlotConfig,
          zeroTime: customLedgerSlotConfig.zeroTime + 1_000,
        },
      }),
    ).toThrow(/does not match the local node's ledger slot mapping/u);
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentity({
        input: custom,
        runtimeNetwork: "Custom",
      }),
    ).toThrow(/does not match the local node's ledger slot mapping/u);
  });

  it("checks the Custom chain by its ledger slot mapping, never by an endpoint", () => {
    // The evidence carries no endpoint; the live check is the local node's
    // ledger slot mapping, so a chain with another start never matches.
    expect(customSlotConfigDocument.source).toStrictEqual({
      kind: "local_ogmios_genesis",
      configurationSha256: customGenesis.configurationSha256,
    });
    const withSource = (
      source: Record<string, unknown>,
      name: string,
    ): ReturnType<typeof candidateInput> => {
      const document = { ...customSlotConfigDocument, source };
      const bytes = Buffer.from(`${JSON.stringify(document)}\n`);
      const path = join(evidenceDirectory, name);
      writeFileSync(path, bytes);
      const standardValue = candidateInput();
      return {
        ...standardValue,
        forcedValidationSlotConfigArtifact: {
          path,
          sha256: createHash("sha256").update(bytes).digest("hex"),
          document: structuredClone(document),
        },
        workerInput: {
          data: {
            ...standardValue.workerInput.data,
            forcedValidationSlotConfig: { ...document.slotConfig },
          },
        },
      } as unknown as ReturnType<typeof candidateInput>;
    };
    expect(() =>
      assertArchitectureGCandidateSlotRuntimeIdentity({
        input: decodeArchitectureGCommitCandidateInput(
          withSource(customSlotConfigDocument.source, "custom-same.json"),
        ),
        runtimeNetwork: "Custom",
        ledgerSlotConfig: {
          ...customLedgerSlotConfig,
          zeroTime: customLedgerSlotConfig.zeroTime + 86_400_000,
        },
      }),
    ).toThrow(/does not match the local node's ledger slot mapping/u);
    // An artifact that still names an endpoint is not this schema.
    expect(() =>
      decodeArchitectureGCommitCandidateInput(
        withSource(
          {
            ...customSlotConfigDocument.source,
            endpointIdentitySha256: hash(98),
          },
          "custom-endpoint.json",
        ),
      ),
    ).toThrow();
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
      void (value.workerInput.data.baseSnapshotId = `architecture-g-candidate:${hash(31).toUpperCase()}`),
    (value: ReturnType<typeof candidateInput>) =>
      void (value.baseUtxosRoot = "bad"),
    (value: ReturnType<typeof candidateInput>) =>
      void delete (value as Partial<typeof value>).baseUtxosRoot,
    (value: ReturnType<typeof candidateInput>) =>
      Object.assign(value.workerInput.data, {
        speculativeBuild: { base: {} },
      }),
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
        Object.assign(value.candidate, { candidateId: "candidate-1" }),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidate.endTimeMs = 0),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidate.l2TransactionCount = 1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidate.roots.utxos = "bad"),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void delete (value.candidate.roots as Record<string, unknown>)
          .eventToStep,
      (value: ReturnType<typeof candidateProbeResult>) =>
        Object.assign(value.candidate.roots, { deposits: hash(84) }),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.userEventRows.deposits = 1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.userEventRows.forcedTransactions = 1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.userEventRows.withdrawals = 1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void delete (value as Partial<ReturnType<typeof candidateProbeResult>>)
          .userEventRows,
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.confirmedLedgerFullScans = 0),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.confirmedLedgerFullScans = 2),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.providerReads = -1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.journalRowsAfter = 1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.candidateConfig.maxLedgerOpCount = 5),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.providerBoundaryAttempts = 1),
      (value: ReturnType<typeof candidateProbeResult>) =>
        void (value.ownerAfter.durableRoot = hash(82)),
      (value: ReturnType<typeof candidateProbeResult>) => {
        value.ownerBefore.durableRoot = hash(83);
        value.ownerAfter.durableRoot = hash(83);
      },
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
