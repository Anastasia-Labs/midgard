import { createHash } from "node:crypto";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  appendTrendEntries,
  evaluateBenchmarkRegressions,
  extractTrendEntriesFromReport,
  loadCurrentEntries,
  median,
} from "../../../scripts/ci/check-benchmark-regression.mjs";
import {
  findDefectSignatures,
  tagReportWithDefects,
} from "../../../scripts/ci/tag-defect-signatures.mjs";
import {
  buildScenarioEnvironment,
  phase1FormalHarnessIds,
} from "../scripts/benchmark-scenario.mjs";
import { assertPhase1FormalBindingOutputAvailable } from "../scripts/create-phase1-formal-binding.mjs";
import {
  loadPhase1FormalBindingSync,
  parsePhase1FormalBindingDocument,
  extractStressCorpusEnvironment,
  PHASE1_FORMAL_BINDING_SCHEMA,
  PHASE1_FORMAL_CHAIN_COUNT,
  PHASE1_FORMAL_CHAIN_DEPTH,
  PHASE1_FORMAL_ROW_COUNT,
  sha256FileSync,
  validatePhase1FormalCorpus,
  verifyPhase1LivePreflight,
} from "../scripts/phase1-formal-identity.mjs";
import {
  loadCorpusIndex,
  openStreamingCorpusReader,
  parseCorpusManifest,
  parseCorpusRowLine,
  scanCorpusPrefixEvidence,
  validateCorpusSlice,
  verifyCorpusArtifactIdentity,
} from "../scripts/throughput-valid-stress-corpus.mjs";
import {
  summarizeL1Observation,
  summarizeHistogramDelta,
  deriveCalibratedClientCapacity,
  summarizeLatency,
  summarizeOpenLoopCheckpointProgress,
  summarizePhase1StageAWindowGate,
  summarizePhase1StarvationGate,
} from "../scripts/throughput-benchmark-utils.mjs";

describe("large benchmark sample summaries", () => {
  it("summarizes more samples than JavaScript argument spreading supports", () => {
    const values = Array.from({ length: 200_000 }, (_, index) => index % 100);
    expect(summarizeLatency(values)).toMatchObject({
      count: 200_000,
      min: 0,
      p99: 98,
      max: 99,
    });
  });
});

describe("calibrated live client capacity", () => {
  it("does not let a fast no-op endpoint throttle the slower bound workload", () => {
    expect(
      deriveCalibratedClientCapacity({
        observedMaxInFlight: 132,
        targetRateTps: 5_000,
        assumedAcceptanceLatencyMs: 819,
        activeChainCount: 4_096,
        httpPipelining: 1,
      }),
    ).toEqual({
      observedMaxInFlight: 132,
      workloadFloor: 4_095,
      submitConcurrency: 4_095,
      httpConnections: 4_095,
    });
  });

  it("fails when the corpus cannot sustain its assumed latency floor", () => {
    expect(() =>
      deriveCalibratedClientCapacity({
        observedMaxInFlight: 132,
        targetRateTps: 5_000,
        assumedAcceptanceLatencyMs: 820,
        activeChainCount: 4_096,
        httpPipelining: 1,
      }),
    ).toThrow(/requires 4100 in-flight chains/u);
  });
});

describe("corpus artifact report binding", () => {
  const canonicalManifest = ({
    corpusPath,
    indexPath,
    corpusSha256,
    indexSha256,
  }) => ({
    schemaVersion: "midgard-stress-corpus-manifest-v1",
    targetRateTps: 1,
    durationMs: 1_000,
    warmupCount: 0,
    cooldownCount: 0,
    safetyFactor: 1,
    assumedAcceptanceLatencyMs: 1_000,
    chainCount: 1,
    chainDepth: 1,
    corpusShape: "chain",
    corpusSliceIds: ["slice-a"],
    generatedAtIso: "2026-07-27T00:00:00.000Z",
    generatorGitSha: "test",
    lucidMidgardVersion: "test",
    feeParams: { minFeeA: "0", minFeeB: "0" },
    network: "Preprod",
    networkId: "0",
    maxSubmitTxCborBytes: 32_768,
    amountTemplate: {
      lovelace: "1",
      shape: "self-transfer-change-chain",
    },
    verification: {
      rebuildSampleRate: 1,
      rebuildSampleAlgorithm: "sha256-corpus-chain-id-order-v1",
    },
    fundingSummary: {
      walletCount: 1,
      perWalletFundingLovelace: "1",
      totalFundingLovelace: "1",
    },
    walletSetIdentity: {
      walletCount: 1,
      fundingRowCount: 1,
      uniqueFirstFundingOutrefCount: 1,
      walletSetHashAlgorithm: "sha256-wallet-id-l2-address-lines-v1",
      walletSetSha256: "00".repeat(32),
      fundingSetHashAlgorithm:
        "sha256-wallet-id-outref-output-cbor-sha256-lines-v1",
      fundingSetSha256: "11".repeat(32),
    },
    sliceSummary: [{ corpusSliceId: "slice-a", walletCount: 1, rowCount: 1 }],
    files: {
      corpus: { path: corpusPath, sha256: corpusSha256, rowCount: 1 },
      index: { path: indexPath, sha256: indexSha256, rowCount: 1 },
      shards: ["shard-0.ndjson"],
    },
  });

  it("hashes all three artifacts and rejects corpus drift from the manifest", async () => {
    const directory = fs.mkdtempSync(
      path.join(os.tmpdir(), "midgard-corpus-identity-"),
    );
    const corpusPath = path.join(directory, "corpus.ndjson");
    const indexPath = `${corpusPath}.index.ndjson`;
    const manifestPath = `${corpusPath}.manifest.json`;
    const corpusBytes = Buffer.from('{"row":1}\n');
    const indexBytes = Buffer.from('{"entry":1}\n');
    const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
    const manifest = canonicalManifest({
      corpusPath,
      indexPath,
      corpusSha256: sha256(corpusBytes),
      indexSha256: sha256(indexBytes),
    });
    fs.writeFileSync(corpusPath, corpusBytes);
    fs.writeFileSync(indexPath, indexBytes);
    fs.writeFileSync(manifestPath, `${JSON.stringify(manifest)}\n`);

    await expect(
      verifyCorpusArtifactIdentity({
        corpusPath,
        indexPath,
        manifestPath,
        manifest,
      }),
    ).resolves.toMatchObject({
      corpusSha256: sha256(corpusBytes),
      indexSha256: sha256(indexBytes),
      manifestSha256: sha256(fs.readFileSync(manifestPath)),
      manifestMatchesArtifacts: true,
    });

    await expect(
      verifyCorpusArtifactIdentity({
        corpusPath,
        indexPath,
        manifestPath,
        manifest: {
          ...manifest,
          generatedAtIso: "2026-07-27T00:00:01.000Z",
        },
      }),
    ).rejects.toThrow("does not match the persisted manifest bytes");

    fs.appendFileSync(corpusPath, "drift\n");
    await expect(
      verifyCorpusArtifactIdentity({
        corpusPath,
        indexPath,
        manifestPath,
        manifest,
      }),
    ).rejects.toThrow("does not match manifest");
  });

  it("rejects incomplete, extra-key, and wrong-version corpus manifests", () => {
    const manifest = canonicalManifest({
      corpusPath: "corpus.ndjson",
      indexPath: "corpus.ndjson.index.ndjson",
      corpusSha256: "22".repeat(32),
      indexSha256: "33".repeat(32),
    });
    expect(() =>
      parseCorpusManifest({ ...manifest, schemaVersion: "legacy-v2" }),
    ).toThrow("unsupported corpus manifest schemaVersion");
    const { files: _files, ...missing } = manifest;
    expect(() => parseCorpusManifest(missing)).toThrow("missing=[files]");
    expect(() =>
      parseCorpusManifest({ ...manifest, unexpected: true }),
    ).toThrow("extra=[unexpected]");
    const { sha256: _sha256, ...corpusWithoutSha256 } = manifest.files.corpus;
    expect(() =>
      parseCorpusManifest({
        ...manifest,
        files: {
          ...manifest.files,
          corpus: corpusWithoutSha256,
        },
      }),
    ).toThrow("missing=[sha256]");
    expect(() =>
      parseCorpusManifest({ ...manifest, network: "preprod" }),
    ).toThrow("network is unsupported");
    expect(() =>
      parseCorpusManifest({
        ...manifest,
        walletSetIdentity: {
          ...manifest.walletSetIdentity,
          walletSetHashAlgorithm: "legacy",
        },
      }),
    ).toThrow("hash algorithm is unsupported");
    expect(() =>
      parseCorpusManifest({ ...manifest, generatedAtIso: "2026-07-27" }),
    ).toThrow("canonical ISO-8601");
    expect(() => parseCorpusManifest({ ...manifest, networkId: "1" })).toThrow(
      "does not match network",
    );
    expect(() =>
      parseCorpusManifest({
        ...manifest,
        fundingSummary: {
          ...manifest.fundingSummary,
          totalFundingLovelace: "2",
        },
      }),
    ).toThrow("cardinality binding is inconsistent");
    expect(() =>
      parseCorpusManifest({
        ...manifest,
        files: {
          ...manifest.files,
          shards: [manifest.files.shards[0], manifest.files.shards[0]],
        },
      }),
    ).toThrow("must be non-empty and unique");
  });
});

describe("bounded corpus uniqueness validation", () => {
  const corpusRow = (index) => {
    const nativeTx = materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        fee: BigInt(index),
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
    const cbor = encodeMidgardNativeTxCanonical(nativeTx);
    return {
      txHash: computeMidgardNativeTxId(nativeTx).toString("hex"),
      canonicalCborHex: cbor.toString("hex"),
      canonicalCborSha256: createHash("sha256").update(cbor).digest("hex"),
      canonicalCborByteLength: cbor.length,
      senderWalletId: `wallet-${index}`,
      selectedInputOutref: `${createHash("sha256").update(`input-${index}`).digest("hex")}#0`,
      outputOutrefs: [],
      planShape: "chain",
      parentTxHash: null,
      corpusSliceId: "slice-a",
    };
  };

  const writeCorpus = (rows) => {
    const directory = fs.mkdtempSync(
      path.join(os.tmpdir(), "midgard-corpus-uniqueness-test-"),
    );
    const corpusPath = path.join(directory, "corpus.ndjson");
    const bytes = Buffer.from(
      `${rows.map((row) => JSON.stringify(row)).join("\n")}\n`,
    );
    fs.writeFileSync(corpusPath, bytes);
    return {
      corpusPath,
      indexEntries: [
        {
          corpusSliceId: "slice-a",
          planShape: "chain",
          chainId: "chain-a",
          startByteOffset: 0,
          endByteOffset: bytes.length,
          rowCount: rows.length,
        },
      ],
    };
  };

  it("rejects corpus rows, indices, and prefix evidence with non-exact keys", async () => {
    const row = corpusRow(0);
    const { parentTxHash: _parentTxHash, ...missingParent } = row;
    expect(() =>
      parseCorpusRowLine(JSON.stringify(missingParent), "missing parent"),
    ).toThrow("missing=[parentTxHash]");
    expect(() =>
      parseCorpusRowLine(
        JSON.stringify({ ...row, historicalExtension: true }),
        "extended row",
      ),
    ).toThrow("extra=[historicalExtension]");
    expect(() =>
      parseCorpusRowLine(
        JSON.stringify({ ...row, senderWalletId: " wallet-0" }),
        "spaced wallet",
      ),
    ).toThrow("exact non-empty string");
    expect(() =>
      parseCorpusRowLine(
        JSON.stringify({ ...row, txHash: "00".repeat(32) }),
        "mismatched hash",
      ),
    ).toThrow("does not bind canonicalCborHex");
    expect(() =>
      parseCorpusRowLine(
        JSON.stringify({ ...row, selectedInputOutref: "bad" }),
        "invalid input",
      ),
    ).toThrow("must be canonical");
    expect(() =>
      parseCorpusRowLine(
        JSON.stringify({ ...row, outputOutrefs: [`${row.txHash}#0`] }),
        "invalid outputs",
      ),
    ).toThrow("must exactly enumerate");

    const directory = fs.mkdtempSync(
      path.join(os.tmpdir(), "midgard-exact-index-"),
    );
    const indexPath = path.join(directory, "index.ndjson");
    fs.writeFileSync(
      indexPath,
      `${JSON.stringify({
        corpusSliceId: "slice-a",
        planShape: "chain",
        chainId: "chain-a",
        startByteOffset: 0,
        endByteOffset: 1,
        rowCount: 1,
        extension: "legacy",
      })}\n`,
    );
    await expect(loadCorpusIndex(indexPath)).rejects.toThrow(
      "extra=[extension]",
    );
    await expect(
      scanCorpusPrefixEvidence({
        corpusPath: "unused",
        fullIndex: [],
        selectedEntries: [],
        consumption: {
          schemaVersion: "midgard-stress-corpus-prefix-evidence-v1",
          rowCount: 0,
          chains: [],
          historicalBinding: true,
        },
        expectedCorpusSha256: "00".repeat(32),
      }),
    ).rejects.toThrow("extra=[historicalBinding]");
  });

  it("validates exact uniqueness across bounded sorted chunks", async () => {
    const fixture = writeCorpus([0, 1, 2, 3].map(corpusRow));
    await expect(
      validateCorpusSlice({
        ...fixture,
        uniquenessChunkEntries: 2,
      }),
    ).resolves.toEqual({
      rowCount: 4,
      uniqueTxHashes: 4,
      uniqueSelectedInputs: 4,
    });
  });

  it("rejects duplicate transaction hashes split across chunks", async () => {
    const rows = [0, 1, 2, 3].map(corpusRow);
    rows[2] = {
      ...rows[2],
      txHash: rows[0].txHash,
      canonicalCborHex: rows[0].canonicalCborHex,
      canonicalCborSha256: rows[0].canonicalCborSha256,
      canonicalCborByteLength: rows[0].canonicalCborByteLength,
      outputOutrefs: rows[0].outputOutrefs,
    };
    await expect(
      validateCorpusSlice({
        ...writeCorpus(rows),
        uniquenessChunkEntries: 2,
      }),
    ).rejects.toThrow(`duplicate txHash ${rows[0].txHash}`);
  });

  it("rejects duplicate selected inputs split across chunks", async () => {
    const rows = [0, 1, 2, 3].map(corpusRow);
    rows[2].selectedInputOutref = rows[0].selectedInputOutref;
    await expect(
      validateCorpusSlice({
        ...writeCorpus(rows),
        uniquenessChunkEntries: 2,
      }),
    ).rejects.toThrow(
      `duplicate selected input ${rows[0].selectedInputOutref}`,
    );
  });

  it("streams indexed cursors through one bounded positional reader", async () => {
    const rows = [0, 1, 2, 3].map(corpusRow);
    const lines = rows.map((row) => `${JSON.stringify(row)}\n`);
    const directory = fs.mkdtempSync(
      path.join(os.tmpdir(), "midgard-corpus-reader-test-"),
    );
    const corpusPath = path.join(directory, "corpus.ndjson");
    fs.writeFileSync(corpusPath, lines.join(""));
    const firstEnd = Buffer.byteLength(lines.slice(0, 2).join(""));
    const indexEntries = [
      {
        corpusSliceId: "slice-a",
        planShape: "chain",
        chainId: "chain-a",
        startByteOffset: 0,
        endByteOffset: firstEnd,
        rowCount: 2,
      },
      {
        corpusSliceId: "slice-a",
        planShape: "chain",
        chainId: "chain-b",
        startByteOffset: firstEnd,
        endByteOffset: Buffer.byteLength(lines.join("")),
        rowCount: 2,
      },
    ];
    const cursors = openStreamingCorpusReader({
      corpusPath,
      indexEntries,
      readAheadRows: 50,
    });
    try {
      expect((await cursors[0].takeNextTx()).txIdHex).toBe(rows[0].txHash);
      expect((await cursors[1].takeNextTx()).txIdHex).toBe(rows[2].txHash);
      expect((await cursors[0].takeNextTx()).txIdHex).toBe(rows[1].txHash);
      expect((await cursors[1].takeNextTx()).txIdHex).toBe(rows[3].txHash);
      expect(await cursors[0].takeNextTx()).toBeNull();
    } finally {
      await cursors.close();
    }
  });
});

describe("Phase 1 single-stream Stage-A window gate", () => {
  it("derives checkpoint misses from starts due instead of end-of-stage state", () => {
    expect(
      summarizeOpenLoopCheckpointProgress({
        targetRateTps: 5_000,
        durationSec: 300,
        dispatchedStarts: 1_485_000,
      }),
    ).toEqual({
      expectedStarts: 1_500_000,
      scheduledStarts: 1_485_000,
      missedStarts: 15_000,
    });
  });

  const latency = (p95, p99) => ({
    count: 1_500_000,
    min: 0,
    p50: 1,
    p95,
    p99,
    max: p99,
    mean: 1,
  });

  it("passes a five-minute 5k checkpoint from a continuous cursor stream", () => {
    const gate = summarizePhase1StageAWindowGate({
      checkpointAvailable: true,
      checkpointRequestedAfterMs: 300_000,
      checkpointObservedAfterMs: 300_050,
      measuredDurationSec: 300,
      targetRateTps: 5_000,
      durablyAdmitted: 1_500_000,
      acceptedDelta: 1_500_000,
      rejectedDelta: 0,
      duplicateSuccesses: 0,
      otherSuccesses: 0,
      submitErrors: 0,
      queueFullResponses: 0,
      submitLatencyMs: latency(8, 12),
      scheduleLagMs: latency(3, 5),
      scheduledStarts: 1_500_000,
      missedStarts: 0,
      missingRequiredMetrics: [],
      streamContinuity: { passed: true },
    });

    expect(gate).toMatchObject({
      passed: true,
      measuredDurationSec: 300,
      durablyAdmittedPerSec: 5_000,
      acceptedPerSec: 5_000,
    });
  });

  it("fails a late callback instead of substituting a longer window", () => {
    const gate = summarizePhase1StageAWindowGate({
      checkpointAvailable: true,
      checkpointRequestedAfterMs: 450_000,
      checkpointObservedAfterMs: 450_050,
      checkpointMaxJitterMs: 1_000,
      measuredDurationSec: 450.05,
      targetRateTps: 5_000,
      durablyAdmitted: 2_232_248,
      acceptedDelta: 2_232_248,
      rejectedDelta: 0,
      duplicateSuccesses: 0,
      otherSuccesses: 0,
      submitErrors: 0,
      queueFullResponses: 0,
      submitLatencyMs: latency(8, 12),
      scheduleLagMs: latency(3, 5),
      scheduledStarts: 2_232_248,
      missedStarts: 0,
      missingRequiredMetrics: [],
      streamContinuity: { passed: true },
    });

    expect(gate.passed).toBe(false);
    expect(gate.reasons).toEqual(
      expect.arrayContaining([
        expect.stringContaining("checkpoint_requested_after_ms"),
        expect.stringContaining("checkpoint_observed_after_ms"),
      ]),
    );
  });

  it("fails closed for a missing checkpoint or a reset/duplicate stream", () => {
    const gate = summarizePhase1StageAWindowGate({
      checkpointAvailable: false,
      checkpointError: "counter probe failed",
      measuredDurationSec: 299,
      targetRateTps: 5_000,
      durablyAdmitted: 1_495_000,
      acceptedDelta: 1_495_000,
      rejectedDelta: 0,
      duplicateSuccesses: 1,
      otherSuccesses: 0,
      submitErrors: 0,
      queueFullResponses: 0,
      submitLatencyMs: latency(8, 12),
      scheduleLagMs: latency(3, 5),
      scheduledStarts: 1_495_001,
      missedStarts: 0,
      missingRequiredMetrics: [],
      streamContinuity: { passed: false, reason: "cursor regressed" },
    });

    expect(gate.passed).toBe(false);
    expect(gate.reasons).toEqual(
      expect.arrayContaining([
        expect.stringContaining("checkpoint failed"),
        expect.stringContaining("measured_duration_sec"),
        "duplicate_successes=1",
        expect.stringContaining("stream_continuity_failed"),
      ]),
    );
  });
});

describe("Phase 1 starvation benchmark gate", () => {
  const sample = (timestampMs, commitBlock, oldestAgeMs) => ({
    timestampMs,
    counters: {
      commitBlock,
      mempoolOldestTxAgeMs: oldestAgeMs,
      metricNames: { mempoolOldestTxAgeMs: "mempool_oldest_tx_age_ms" },
    },
  });

  it("passes a ten-minute 2x overload window with bounded saw-tooth age", () => {
    const samples = [];
    for (let second = 0; second <= 600; second += 1) {
      samples.push(
        sample(second * 1_000, Math.floor(second / 10), (second % 10) * 1_000),
      );
    }
    const gate = summarizePhase1StarvationGate({
      samples,
      stageStartedAtMs: 0,
      stageEndedAtMs: 600_000,
      targetRateTps: 4_000,
      overloadBaselineTps: 2_000,
      commitTxDelta: 1_200_000,
      commitBlockDelta: 60,
    });

    expect(gate).toMatchObject({
      passed: true,
      measuredDurationSec: 600,
      observedDecrease: true,
      observedOverloadRatio: 2,
      maxAllowedOldestTxAgeMs: 30_000,
    });
  });

  it("fails closed when commit intervals are absent or age never decreases", () => {
    const gate = summarizePhase1StarvationGate({
      samples: [
        sample(0, 0, 0),
        sample(300_000, 0, 300_000),
        sample(600_000, 1, 600_000),
      ],
      stageStartedAtMs: 0,
      stageEndedAtMs: 600_000,
      targetRateTps: 5_000,
      overloadBaselineTps: 2_500,
      commitTxDelta: 10_000,
      commitBlockDelta: 1,
    });

    expect(gate.passed).toBe(false);
    expect(gate.reasons).toEqual(
      expect.arrayContaining([
        expect.stringContaining("successful_commit_interval_p95_ms missing"),
        expect.stringContaining("did not decrease"),
      ]),
    );
  });

  it("fails closed when the oldest-age metric is missing", () => {
    const samples = [sample(0, 0, 0), sample(600_000, 2, 0)];
    samples[1].counters.metricNames.mempoolOldestTxAgeMs = null;
    const gate = summarizePhase1StarvationGate({
      samples,
      stageStartedAtMs: 0,
      stageEndedAtMs: 600_000,
      targetRateTps: 5_000,
      overloadBaselineTps: 2_500,
      commitTxDelta: 20_000,
      commitBlockDelta: 2,
    });

    expect(gate.passed).toBe(false);
    expect(gate.missingOldestAgeSamples).toBe(1);
  });
});

describe("Class B L1 observation", () => {
  it("reports tip bounds and observed inter-block times", () => {
    const observation = summarizeL1Observation([
      { timestampMs: 0, counters: { l1TipSlot: 100 } },
      { timestampMs: 1_000, counters: { l1TipSlot: 100 } },
      { timestampMs: 2_000, counters: { l1TipSlot: 102 } },
      { timestampMs: 5_000, counters: { l1TipSlot: 105 } },
    ]);

    expect(observation).toMatchObject({
      startTipSlot: 100,
      endTipSlot: 105,
      observedPreprodBlockCount: 2,
      interBlockTimeMs: { min: 2_000, p50: 2_000, max: 3_000 },
    });
  });
});

describe("histogram delta reporting", () => {
  it("derives interval quantiles from cumulative Prometheus snapshots", () => {
    const summary = summarizeHistogramDelta(
      {
        count: 2,
        sum: 30,
        buckets: [
          { le: "10", value: 1 },
          { le: "20", value: 2 },
          { le: "+Inf", value: 2 },
        ],
      },
      {
        count: 6,
        sum: 130,
        buckets: [
          { le: "10", value: 2 },
          { le: "20", value: 4 },
          { le: "40", value: 5 },
          { le: "+Inf", value: 6 },
        ],
      },
    );

    expect(summary).toMatchObject({
      count: 4,
      sum: 100,
      mean: 25,
      p50: 20,
      p95: 40,
    });
  });
});

describe("Phase 1 formal scenario contracts", () => {
  it("ships the formal stress engine HTTP client as a runtime dependency", () => {
    const packageJson = JSON.parse(fs.readFileSync("package.json", "utf8"));
    expect(packageJson.dependencies?.undici).toBe("^7.25.0");
    expect(packageJson.devDependencies?.undici).toBeUndefined();
  });

  it("routes the client-capacity self-check through the configured no-op endpoint", () => {
    const source = fs.readFileSync(
      "scripts/throughput-valid-stress.mjs",
      "utf8",
    );
    const selfCheck = source.slice(
      source.indexOf("const runClientSelfCheck"),
      source.indexOf("const summarizeCursorContinuity"),
    );
    expect(selfCheck).toContain("`${noOpEndpoint}/submit`");
    expect(selfCheck).toContain(
      '"content-type": "application/vnd.midgard.v1+cbor"',
    );
    expect(selfCheck).toContain("body: Buffer.from([0])");
    expect(selfCheck).toContain(".request(endpoint, requestOptions)");
    expect(selfCheck).toContain("Math.min(httpConnections, submitConcurrency)");
    expect(selfCheck).toContain("warmupFailures > 0");
    expect(selfCheck).toContain("runDeadlineBatchedSchedule({");
    const initialHttpConnections = source.slice(
      source.indexOf("const httpConnectionsSetting"),
      source.indexOf("let httpConnections"),
    );
    expect(initialHttpConnections).toContain('"256"');
  });

  it("batches every due high-rate start after a coarse timer wake", () => {
    const source = fs.readFileSync(
      "scripts/throughput-valid-stress.mjs",
      "utf8",
    );
    const scheduler = source.slice(
      source.indexOf("const scheduledStartCountDue"),
      source.indexOf("const findAvailableCursor"),
    );
    expect(scheduler).toContain("Math.max(1, Math.ceil(waitMs))");
    expect(scheduler).toContain("nextStartIndex < dueStarts");
    expect(scheduler).toContain("inFlight.size < maxInFlight");
    expect(scheduler).toContain("missedStarts: totalStarts - nextStartIndex");
    expect(scheduler).not.toContain("await sleep(intervalMs)");
  });

  it("uses the same deadline scheduler for calibration and open-loop load", () => {
    const source = fs.readFileSync(
      "scripts/throughput-valid-stress.mjs",
      "utf8",
    );
    const openLoop = source.slice(
      source.indexOf("const runOpenLoopStage"),
      source.indexOf("const collectCalibrationRows"),
    );
    const calibration = source.slice(
      source.indexOf("const runNoOpCalibrationStage"),
      source.indexOf("const waitForStageDrain"),
    );
    expect(openLoop).toContain("runDeadlineBatchedSchedule({");
    expect(openLoop).toContain("stage.missedStarts += schedule.missedStarts");
    expect(openLoop).not.toContain("allowPostDeadlineCatchUp: true");
    expect(calibration).toContain("runDeadlineBatchedSchedule({");
    expect(calibration).toContain("startedAtPerfMs: startedPerfMs");
    expect(calibration).toContain("warmupRequestCount");
    expect(calibration).toContain("warmupFailures > 0");
    expect(calibration.indexOf("warmupResults")).toBeLessThan(
      calibration.indexOf("const startedPerfMs"),
    );
    expect(calibration).toContain("allowPostDeadlineCatchUp: true");
    expect(calibration).toContain("(1 - missedStartMaxRatio)");
    expect(source).toContain("readAheadRows: 1");
    expect(calibration).toContain("schedule.missedStarts === 0");
    expect(source).toContain('"calibration_capacity_selected"');
  });

  const sha = (character) => character.repeat(64);
  const makeFormalFixture = (overrides = {}) => {
    const directory = fs.mkdtempSync(
      path.join(os.tmpdir(), "midgard-phase1-formal-binding-"),
    );
    const corpusPath = path.join(directory, "corpus.ndjson");
    const indexPath = `${corpusPath}.index.ndjson`;
    const manifestPath = `${corpusPath}.manifest.json`;
    const generationResultPath = `${corpusPath}.generation-result.json`;
    const bindingPath = path.join(directory, "binding.json");
    const stressCorpusEnv = {
      STRESS_CORPUS_INDEX_PATH: indexPath,
      STRESS_CORPUS_MANIFEST_PATH: manifestPath,
      STRESS_CORPUS_PATH: corpusPath,
      STRESS_CORPUS_READAHEAD_ROWS: "50",
      STRESS_CORPUS_SHAPE: "chain",
      STRESS_CORPUS_SLICE_ID: "phase1-live",
    };
    const walletSetIdentity = {
      walletCount: PHASE1_FORMAL_CHAIN_COUNT,
      fundingRowCount: PHASE1_FORMAL_CHAIN_COUNT,
      uniqueFirstFundingOutrefCount: PHASE1_FORMAL_CHAIN_COUNT,
      walletSetHashAlgorithm: "sha256-wallet-id-l2-address-lines-v1",
      walletSetSha256: sha("a"),
      fundingSetHashAlgorithm:
        "sha256-wallet-id-outref-output-cbor-sha256-lines-v1",
      fundingSetSha256: sha("b"),
    };
    const selectedIndexEntries = Array.from(
      { length: PHASE1_FORMAL_CHAIN_COUNT },
      (_, index) => ({
        chainId: `wallet-${index.toString().padStart(4, "0")}`,
        corpusSliceId: "phase1-live",
        planShape: "chain",
        startByteOffset: index * 1_000,
        endByteOffset: (index + 1) * 1_000,
        rowCount: PHASE1_FORMAL_CHAIN_DEPTH,
      }),
    );
    const sampleIds = [...selectedIndexEntries]
      .sort((left, right) => {
        const key = (entry) =>
          createHash("sha256")
            .update(sha("c"))
            .update("\0")
            .update(entry.chainId)
            .update("\0")
            .update(String(entry.startByteOffset))
            .digest("hex");
        return key(left).localeCompare(key(right));
      })
      .slice(0, 5)
      .map((entry) => entry.chainId);
    const liveEntries = sampleIds.map((walletId, index) => ({
      walletId,
      l2Address: `addr_test1_${index.toString()}`,
      firstInputOutref: `${index.toString(16).padStart(64, "0")}#0`,
      outputCborSha256: sha(String.fromCharCode(97 + index)),
    }));
    const generationResultDocument = {
      schemaVersion: "midgard-stress-corpus-generation-v1",
      outDir: directory,
      corpusPath,
      indexPath,
      manifestPath,
      plan: {},
      walletSetIdentity,
      assembled: {},
      verified: {
        rowCount: PHASE1_FORMAL_ROW_COUNT,
        chainCount: PHASE1_FORMAL_CHAIN_COUNT,
        corpusSha256: sha("c"),
        indexSha256: sha("d"),
        walletSetIdentity,
        rebuildSample: {
          algorithm: "sha256-corpus-chain-id-order-v1",
          sampleRate: 0.001,
          checkedChainCount: 5,
          checkedRowCount: 5 * PHASE1_FORMAL_CHAIN_DEPTH,
          sampledChainIds: liveEntries.map((entry) => entry.walletId),
          livePreflightEntries: liveEntries,
        },
        verificationArtifact: {},
      },
    };
    fs.writeFileSync(
      generationResultPath,
      `${JSON.stringify(generationResultDocument)}\n`,
    );
    const bindingDocument = {
      schemaVersion: PHASE1_FORMAL_BINDING_SCHEMA,
      deploymentManifestId: "deployment-1",
      nodeImageId: "sha256:node-image",
      nodeContainerId: "node-container-1",
      walletSetSha256: walletSetIdentity.walletSetSha256,
      fundingSetSha256: walletSetIdentity.fundingSetSha256,
      corpus: {
        path: corpusPath,
        indexPath,
        manifestPath,
        sliceId: "phase1-live",
        corpusSha256: sha("c"),
        indexSha256: sha("d"),
        manifestSha256: sha("e"),
      },
      generationResult: {
        path: generationResultPath,
        sha256: sha256FileSync(generationResultPath),
      },
      livePreflight: {
        algorithm: "sha256-corpus-chain-id-order-v1",
        sampleSize: 5,
        entries: liveEntries,
      },
      harness: phase1FormalHarnessIds,
      stressCorpusEnv,
    };
    fs.writeFileSync(bindingPath, `${JSON.stringify(bindingDocument)}\n`);
    const baseEnv = {
      ...stressCorpusEnv,
      STRESS_PHASE1_BINDING_PATH: bindingPath,
      STRESS_PHASE1_DEPLOYMENT_MANIFEST_ID:
        bindingDocument.deploymentManifestId,
      STRESS_PHASE1_NODE_IMAGE_ID: bindingDocument.nodeImageId,
      STRESS_PHASE1_NODE_CONTAINER_ID: bindingDocument.nodeContainerId,
      ...overrides,
    };
    return {
      baseEnv,
      binding: loadPhase1FormalBindingSync(bindingPath),
      bindingDocument,
      generationResultDocument,
      walletSetIdentity,
      selectedIndexEntries,
    };
  };

  it("does not clobber an existing formal binding output", () => {
    const fixture = makeFormalFixture();
    const before = fs.readFileSync(fixture.binding.path);
    expect(() =>
      assertPhase1FormalBindingOutputAvailable(fixture.binding.path),
    ).toThrow(/Refusing to overwrite existing Phase 1 binding/);
    expect(fs.readFileSync(fixture.binding.path)).toEqual(before);
  });

  it("accepts only the exact canonical Phase 1 formal binding V1 language", () => {
    const fixture = makeFormalFixture();
    expect(
      parsePhase1FormalBindingDocument(
        fixture.bindingDocument,
        fixture.binding.path,
      ),
    ).toEqual(fixture.bindingDocument);
    const mutations = [
      (binding) => {
        binding.schemaVersion = "midgard-phase1-live-corpus-binding-v2";
      },
      (binding) => {
        binding.unknown = true;
      },
      (binding) => {
        binding.corpus.unknown = true;
      },
      (binding) => {
        binding.corpus.path = "./corpus.ndjson";
      },
      (binding) => {
        binding.walletSetSha256 = binding.walletSetSha256.toUpperCase();
      },
      (binding) => {
        binding.livePreflight.entries[1].walletId =
          binding.livePreflight.entries[0].walletId;
      },
      (binding) => {
        binding.livePreflight.entries[0].firstInputOutref = `${"0".repeat(64)}#00`;
      },
      (binding) => {
        binding.stressCorpusEnv.STRESS_CORPUS_WALLET_SEED_PHRASE =
          "must-not-be-accepted";
      },
    ];
    for (const mutate of mutations) {
      const binding = structuredClone(fixture.bindingDocument);
      mutate(binding);
      expect(() =>
        parsePhase1FormalBindingDocument(binding, fixture.binding.path),
      ).toThrow();
    }
  });

  it("pins the five-minute 5k admission gate", () => {
    const { scenario, env } = buildScenarioEnvironment({
      scenarioName: "phase1-admission-5000-5min",
      baseEnv: {},
      resultsRoot: "/tmp/results",
    });

    expect(scenario.scenarioClass).toBe("A");
    expect(env).toMatchObject({
      STRESS_TARGET_ACCEPTED_TPS: "5000",
      STRESS_OPEN_LOOP_RATE_TPS: "5000",
      STRESS_MEASURED_SEC: "300",
      STRESS_SUBMIT_LATENCY_P99_MAX_MS: "1000",
      STRESS_FORMAL_BENCHMARK: "true",
    });
  });

  it("pins the ten-minute commit-enabled 2x starvation gate", () => {
    const fixture = makeFormalFixture();
    const { scenario, env } = buildScenarioEnvironment({
      scenarioName: "phase1-starvation-2x-soak",
      baseEnv: fixture.baseEnv,
      resultsRoot: "/tmp/results",
    });

    expect(scenario.scenarioClass).toBe("B");
    expect(env).toMatchObject({
      STRESS_CORPUS_SHAPE: "chain",
      STRESS_OPEN_LOOP_RATE_TPS: "5000",
      STRESS_MEASURED_SEC: "600",
      STRESS_WARMUP_TXS: "0",
      STRESS_WARMUP_SEC: "0",
      STRESS_CLIENT_SELF_CHECK: "true",
      STRESS_CLIENT_SELF_CHECK_REQUIRED: "true",
      STRESS_CLIENT_SELF_CHECK_MULTIPLIER: "2",
      STRESS_REQUIRE_METRIC_PRESENCE: "true",
      STRESS_MAX_CHAINS: "auto",
      STRESS_WAIT_FOR_COMMIT: "true",
      STRESS_PHASE1_STAGE_A_WINDOW_GATE: "true",
      STRESS_PHASE1_STAGE_A_WINDOW_SEC: "300",
      STRESS_PHASE1_STAGE_A_CHECKPOINT_MAX_JITTER_MS: "1000",
      STRESS_PHASE1_STARVATION_GATE: "true",
      STRESS_PHASE1_STARVATION_BASELINE_TPS: "2500",
      STRESS_PHASE1_STARVATION_MIN_OVERLOAD_RATIO: "2",
      STRESS_PHASE1_STARVATION_MAX_AGE_MULTIPLIER: "3",
      STRESS_NODE_SATURATION_MIN_RATIO: "1",
      STRESS_FORMAL_BENCHMARK: "true",
      STRESS_PHASE1_SCENARIO_HARNESS_ID: phase1FormalHarnessIds.scenarioId,
      STRESS_PHASE1_ENGINE_HARNESS_ID: phase1FormalHarnessIds.engineId,
    });
  });

  it("rejects hostile overrides of formal scenario invariants", () => {
    const hostileOverrides = {
      STRESS_MEASURED_SEC: "1",
      STRESS_OPEN_LOOP_RATE_TPS: "1",
      STRESS_TARGET_ACCEPTED_TPS: "1",
      STRESS_CORPUS_SHAPE: "fanout",
      STRESS_NODE_SATURATION_MIN_RATIO: "1.2",
      STRESS_PHASE1_STAGE_A_WINDOW_GATE: "false",
      STRESS_PHASE1_STARVATION_GATE: "false",
      STRESS_FORMAL_BENCHMARK: "false",
      STRESS_WARMUP_TXS: "1",
      STRESS_WARMUP_SEC: "1",
      STRESS_CLIENT_SELF_CHECK: "false",
      STRESS_CLIENT_SELF_CHECK_REQUIRED: "false",
      STRESS_CLIENT_SELF_CHECK_MULTIPLIER: "1",
      STRESS_REQUIRE_METRIC_PRESENCE: "false",
      STRESS_MAX_CHAINS: "1",
    };
    for (const [name, value] of Object.entries(hostileOverrides)) {
      const fixture = makeFormalFixture({ [name]: value });
      expect(() =>
        buildScenarioEnvironment({
          scenarioName: "phase1-starvation-2x-soak",
          baseEnv: fixture.baseEnv,
          resultsRoot: "/tmp/results",
        }),
      ).toThrow(new RegExp(`formal scenario .* requires ${name}=`));
    }
  });

  it("fails closed when binding identity inputs or corpus env drift", () => {
    expect(() =>
      buildScenarioEnvironment({
        scenarioName: "phase1-starvation-2x-soak",
        baseEnv: {},
        resultsRoot: "/tmp/results",
      }),
    ).toThrow(/STRESS_PHASE1_BINDING_PATH/u);

    const wrongDeployment = makeFormalFixture({
      STRESS_PHASE1_DEPLOYMENT_MANIFEST_ID: "other-deployment",
    });
    expect(() =>
      buildScenarioEnvironment({
        scenarioName: "phase1-starvation-2x-soak",
        baseEnv: wrongDeployment.baseEnv,
        resultsRoot: "/tmp/results",
      }),
    ).toThrow(/deployment manifest ID/u);

    const extraCorpusEnv = makeFormalFixture({
      STRESS_CORPUS_UNBOUND_VALUE: "drift",
    });
    expect(() =>
      buildScenarioEnvironment({
        scenarioName: "phase1-starvation-2x-soak",
        baseEnv: extraCorpusEnv.baseEnv,
        resultsRoot: "/tmp/results",
      }),
    ).toThrow(/unsupported STRESS_CORPUS_\* environment keys/u);

    expect(() =>
      extractStressCorpusEnvironment({
        STRESS_CORPUS_PATH: "/corpus",
        STRESS_CORPUS_WALLET_SEED_PHRASE: "never copy this secret",
      }),
    ).toThrow(/secret-like and extraneous keys are forbidden/u);

    const nonCanonical = makeFormalFixture();
    const bindingDocument = {
      ...nonCanonical.bindingDocument,
      stressCorpusEnv: {
        ...nonCanonical.bindingDocument.stressCorpusEnv,
        STRESS_CORPUS_PATH: "./corpus.ndjson",
      },
    };
    fs.writeFileSync(
      nonCanonical.binding.path,
      `${JSON.stringify(bindingDocument)}\n`,
    );
    expect(() =>
      buildScenarioEnvironment({
        scenarioName: "phase1-starvation-2x-soak",
        baseEnv: {
          ...nonCanonical.baseEnv,
          STRESS_CORPUS_PATH: "./corpus.ndjson",
        },
        resultsRoot: "/tmp/results",
      }),
    ).toThrow(/canonical absolute corpus path/u);
  });

  it("requires canonical artifact hashes and exactly 4,096 selected chains", () => {
    const fixture = makeFormalFixture();
    const selectedIndexEntries = fixture.selectedIndexEntries;
    const corpusManifest = {
      targetRateTps: 5_000,
      durationMs: 600_000,
      warmupCount: 0,
      cooldownCount: 0,
      safetyFactor: 1.02,
      assumedAcceptanceLatencyMs: 819,
      chainCount: PHASE1_FORMAL_CHAIN_COUNT,
      chainDepth: PHASE1_FORMAL_CHAIN_DEPTH,
      corpusShape: "chain",
      corpusSliceIds: [fixture.bindingDocument.corpus.sliceId],
      sliceSummary: [
        {
          corpusSliceId: fixture.bindingDocument.corpus.sliceId,
          walletCount: PHASE1_FORMAL_CHAIN_COUNT,
          rowCount: PHASE1_FORMAL_ROW_COUNT,
        },
      ],
      network: "Preprod",
      networkId: "0",
      maxSubmitTxCborBytes: 32_768,
      feeParams: { minFeeA: "10", minFeeB: "10" },
      amountTemplate: { lovelace: "1", shape: "self-transfer-change-chain" },
      fundingSummary: {
        walletCount: PHASE1_FORMAL_CHAIN_COUNT,
        perWalletFundingLovelace: "11228229",
        totalFundingLovelace: "45990825984",
      },
      verification: {
        rebuildSampleRate: 0.001,
        rebuildSampleAlgorithm: "sha256-corpus-chain-id-order-v1",
      },
      files: { corpus: { rowCount: PHASE1_FORMAL_ROW_COUNT } },
      walletSetIdentity: fixture.walletSetIdentity,
    };
    const corpusArtifactIdentity = {
      corpusSha256: fixture.bindingDocument.corpus.corpusSha256,
      indexSha256: fixture.bindingDocument.corpus.indexSha256,
      manifestSha256: fixture.bindingDocument.corpus.manifestSha256,
    };

    expect(
      validatePhase1FormalCorpus({
        binding: fixture.binding,
        corpusManifest,
        corpusArtifactIdentity,
        selectedIndexEntries,
      }),
    ).toMatchObject({
      deploymentManifestId: fixture.bindingDocument.deploymentManifestId,
      selectedChainCount: PHASE1_FORMAL_CHAIN_COUNT,
      selectedRowCount: PHASE1_FORMAL_ROW_COUNT,
    });
    expect(() =>
      validatePhase1FormalCorpus({
        binding: fixture.binding,
        corpusManifest,
        corpusArtifactIdentity,
        selectedIndexEntries: selectedIndexEntries.slice(1),
      }),
    ).toThrow(/selected chain count/u);
    expect(() =>
      validatePhase1FormalCorpus({
        binding: fixture.binding,
        corpusManifest,
        corpusArtifactIdentity: {
          ...corpusArtifactIdentity,
          corpusSha256: sha("f"),
        },
        selectedIndexEntries,
      }),
    ).toThrow(/corpus SHA-256/u);
    expect(() =>
      validatePhase1FormalCorpus({
        binding: fixture.binding,
        corpusManifest: {
          ...corpusManifest,
          walletSetIdentity: {
            ...corpusManifest.walletSetIdentity,
            walletSetSha256: sha("f"),
          },
        },
        corpusArtifactIdentity,
        selectedIndexEntries,
      }),
    ).toThrow(/wallet-set SHA-256/u);

    for (const [label, mutate] of [
      ["manifest target rate", (value) => (value.targetRateTps = 4_999)],
      ["manifest duration", (value) => (value.durationMs = 599_999)],
      ["manifest safety factor", (value) => (value.safetyFactor = 1.01)],
      [
        "manifest assumed acceptance latency",
        (value) => (value.assumedAcceptanceLatencyMs = 820),
      ],
      ["manifest MIN_FEE_A", (value) => (value.feeParams.minFeeA = "11")],
      [
        "manifest transfer amount",
        (value) => (value.amountTemplate.lovelace = "2"),
      ],
      [
        "manifest per-wallet funding",
        (value) => (value.fundingSummary.perWalletFundingLovelace = "1"),
      ],
      [
        "manifest rebuild sample rate",
        (value) => (value.verification.rebuildSampleRate = 0.01),
      ],
    ]) {
      const hostile = structuredClone(corpusManifest);
      mutate(hostile);
      expect(() =>
        validatePhase1FormalCorpus({
          binding: fixture.binding,
          corpusManifest: hostile,
          corpusArtifactIdentity,
          selectedIndexEntries,
        }),
      ).toThrow(new RegExp(label));
    }

    fs.appendFileSync(fixture.bindingDocument.generationResult.path, " ");
    expect(() =>
      validatePhase1FormalCorpus({
        binding: fixture.binding,
        corpusManifest,
        corpusArtifactIdentity,
        selectedIndexEntries,
      }),
    ).toThrow(/generation result artifact SHA-256/u);
  });

  it("requires the deterministic live sample to contain exact outref and output bytes", async () => {
    const output = Buffer.from("8200", "hex");
    const outputCborSha256 = createHash("sha256").update(output).digest("hex");
    // §5.3 field-0/1 item form: `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`.
    const expectedOutrefCbor = `825820${"a".repeat(64)}190000`;
    const expected = {
      algorithm: "sha256-corpus-chain-id-order-v1",
      sampleSize: 1,
      entries: [
        {
          walletId: "stress-wallet-0001",
          l2Address: "addr_test1_sample",
          firstInputOutref: `${"a".repeat(64)}#0`,
          outputCborSha256,
        },
      ],
    };
    await expect(
      verifyPhase1LivePreflight({
        expected,
        fetchUtxos: async () => [],
      }),
    ).rejects.toThrow(/missing_first_input/u);
    await expect(
      verifyPhase1LivePreflight({
        expected,
        fetchUtxos: async () => [
          { outref: expectedOutrefCbor, outputCbor: "8201" },
        ],
      }),
    ).rejects.toThrow(/output_mismatch/u);
    await expect(
      verifyPhase1LivePreflight({
        expected,
        fetchUtxos: async () => [
          { outref: expectedOutrefCbor, outputCbor: "8200" },
        ],
      }),
    ).resolves.toMatchObject({ passed: true, sampleSize: 1 });
  });
});

describe("benchmark regression gate", () => {
  it("computes medians for odd and even windows", () => {
    expect(median([3, 1, 2])).toBe(2);
    expect(median([4, 1, 2, 3])).toBe(2.5);
  });

  it("keeps Class A scenarios in bootstrap until five historical entries exist", () => {
    const current = {
      scenario: "accept-2500-tps-gate",
      stage: "accepted",
      tps: 2000,
      classA: true,
    };
    const evaluations = evaluateBenchmarkRegressions({
      trendEntries: [
        { ...current, tps: 2500 },
        { ...current, tps: 2490 },
        { ...current, tps: 2480 },
        { ...current, tps: 2470 },
      ],
      currentEntries: [current],
      classAOnly: true,
    });

    expect(evaluations).toMatchObject([
      { status: "bootstrap", historicalCount: 4 },
    ]);
  });

  it("fails Class A drops greater than 10 percent versus the trailing median", () => {
    const current = {
      scenario: "accept-2500-tps-gate",
      stage: "accepted",
      tps: 2200,
      classA: true,
    };
    const evaluations = evaluateBenchmarkRegressions({
      trendEntries: [2500, 2510, 2490, 2505, 2495].map((tps) => ({
        ...current,
        tps,
      })),
      currentEntries: [current],
      classAOnly: true,
    });

    expect(evaluations[0].status).toBe("regression");
    expect(evaluations[0].trailingMedian).toBe(2500);
  });

  it("records Class B regressions as informational when class-a-only is set", () => {
    const current = {
      scenario: "soak-10min-at-max",
      stage: "committed",
      tps: 1,
      classA: false,
    };
    const evaluations = evaluateBenchmarkRegressions({
      trendEntries: [20, 21, 22, 23, 24].map((tps) => ({ ...current, tps })),
      currentEntries: [current],
      classAOnly: true,
    });

    expect(evaluations).toMatchObject([{ status: "informational_class_b" }]);
  });

  it("extracts per-stage entries from canonical engine reports", () => {
    const entries = extractTrendEntriesFromReport({
      scenario: "burst-2x-target",
      sha: "abc",
      generatedAtIso: "2026-07-09T00:00:00.000Z",
      stages: [
        {
          name: "burst",
          queuedSubmitSuccessPerSec: 5000,
          measuredAcceptedTps: 4990,
          measuredElapsedSec: 45,
          commitTxDelta: 90,
          mergeBlockDelta: 2,
        },
      ],
    });

    expect(entries).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ stage: "offered", tps: 5000, classA: true }),
        expect.objectContaining({
          stage: "accepted",
          tps: 4990,
          classA: true,
        }),
        expect.objectContaining({ stage: "committed", tps: 2, classA: false }),
      ]),
    );
  });

  it("appends and reloads trend entries from result artifacts", () => {
    const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "midgard-bench-"));
    const trendDir = path.join(tmp, "trends");
    const resultsDir = path.join(tmp, "results");
    fs.mkdirSync(resultsDir);
    fs.writeFileSync(
      path.join(resultsDir, "accept-2500-tps-gate.json"),
      `${JSON.stringify({
        scenario: "accept-2500-tps-gate",
        sha: "abc",
        summary: {
          queuedSubmitSuccessPerSec: 2500,
          avgAcceptedTps: 2490,
        },
      })}\n`,
    );

    const entries = loadCurrentEntries(resultsDir);
    appendTrendEntries({ trendDir, entries });

    expect(
      fs.readFileSync(
        path.join(trendDir, "accept-2500-tps-gate.ndjson"),
        "utf8",
      ),
    ).toContain('"stage":"accepted"');
  });
});

describe("benchmark defect signature tagging", () => {
  it("finds known defect signatures in captured logs", () => {
    expect(
      findDefectSignatures(
        "Worker failed with DatabaseInitializationError; state-queue mutation lease is busy",
      ),
    ).toEqual(["DEF-001", "DEF-003"]);
    expect(
      findDefectSignatures(
        '{"observedCommittedCount":0,"finalityTimedOutCount":500}',
      ),
    ).toEqual(["DEF-002"]);
  });

  it("adds defectSignaturesObserved to report JSON", () => {
    const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "midgard-defect-"));
    const reportPath = path.join(tmp, "report.json");
    const logPath = path.join(tmp, "node.log");
    fs.writeFileSync(reportPath, '{"scenario":"burst-2x-target"}\n');
    fs.writeFileSync(logPath, "DatabaseInitializationError\n");

    const observed = tagReportWithDefects({ reportPath, logPaths: [logPath] });
    const report = JSON.parse(fs.readFileSync(reportPath, "utf8"));

    expect(observed).toEqual(["DEF-001"]);
    expect(report.defectSignaturesObserved).toEqual(["DEF-001"]);
  });
});
