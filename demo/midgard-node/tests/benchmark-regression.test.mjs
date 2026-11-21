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
  runDeadlineBatchedSchedule,
  scheduledStartCountDue,
} from "../scripts/lib/deadline-batched-schedule.mjs";
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

/**
 * The open-loop start scheduler, exercised directly against a fake clock.
 *
 * This replaces three tests that asserted ~40 substrings of
 * `scripts/throughput-valid-stress.mjs` as proof of runtime scheduling: they
 * broke on any rename and never ran a single start. The contract they were
 * reaching for — one coarse timer wake dispatches every start that became due,
 * instead of `await sleep(intervalMs)` per start — is a behavioural claim and
 * is asserted as one here.
 */
describe("deadline-batched open-loop scheduler", () => {
  const makeFakeClock = (startMs = 0) => {
    let clockMs = startMs;
    const sleeps = [];
    return {
      now: () => clockMs,
      sleep: async (ms) => {
        sleeps.push(ms);
        clockMs += ms;
        await Promise.resolve();
      },
      sleeps,
      advance: (ms) => {
        clockMs += ms;
      },
      get clockMs() {
        return clockMs;
      },
    };
  };

  it("counts a start due at its own deadline and clamps to the schedule", () => {
    const schedule = {
      startedAtPerfMs: 100,
      intervalMs: 10,
      totalStarts: 5,
    };
    expect(
      [99, 100, 109, 110, 139, 140, 1_000].map((nowPerfMs) =>
        scheduledStartCountDue({ ...schedule, nowPerfMs }),
      ),
      // 139ms is 3.9 intervals after the start: four starts are due, not five.
    ).toEqual([0, 1, 1, 2, 4, 5, 5]);
  });

  it("dispatches every start that came due during one coarse wake", async () => {
    // 10k starts/s: a per-start `sleep(intervalMs)` cannot hold this rate,
    // because Node rounds every sub-millisecond timer up to 1ms.
    const totalStarts = 200;
    const intervalMs = 0.1;
    const clock = makeFakeClock();
    const dispatchClockMs = [];

    const result = await runDeadlineBatchedSchedule({
      totalStarts,
      startedAtPerfMs: 0,
      deadlinePerfMs: 10_000,
      intervalMs,
      maxInFlight: totalStarts,
      dispatchStart: ({ startIndex, scheduledAtPerfMs }) => {
        dispatchClockMs.push({
          startIndex,
          scheduledAtPerfMs,
          at: clock.now(),
        });
        return Promise.resolve();
      },
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.scheduledStarts).toBe(totalStarts);
    expect(result.missedStarts).toBe(0);
    expect(result.stoppedWithoutCapacity).toBe(false);
    // Starts are handed out in schedule order at their own scheduled times.
    expect(dispatchClockMs.map(({ startIndex }) => startIndex)).toEqual(
      Array.from({ length: totalStarts }, (_, index) => index),
    );
    expect(
      dispatchClockMs.map(({ scheduledAtPerfMs }) => scheduledAtPerfMs),
    ).toEqual(
      Array.from({ length: totalStarts }, (_, index) => index * intervalMs),
    );

    // Every timer wake is the 1ms floor, and one wake releases a whole batch:
    // 200 starts at 0.1ms spacing must cost ~20 sleeps, not 200.
    expect(new Set(clock.sleeps)).toEqual(new Set([1]));
    expect(clock.sleeps.length).toBeLessThanOrEqual(25);
    const batchSizes = new Map();
    for (const { at } of dispatchClockMs) {
      batchSizes.set(at, (batchSizes.get(at) ?? 0) + 1);
    }
    expect(Math.max(...batchSizes.values())).toBeGreaterThanOrEqual(10);
  });

  it("never exceeds the in-flight bound", async () => {
    const clock = makeFakeClock();
    let live = 0;
    let peakLive = 0;
    const result = await runDeadlineBatchedSchedule({
      totalStarts: 50,
      startedAtPerfMs: 0,
      deadlinePerfMs: 10_000,
      intervalMs: 0.1,
      maxInFlight: 4,
      dispatchStart: () => {
        live += 1;
        peakLive = Math.max(peakLive, live);
        return Promise.resolve().then(() => {
          live -= 1;
        });
      },
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.scheduledStarts).toBe(50);
    expect(peakLive).toBeLessThanOrEqual(4);
    expect(result.maxObservedInFlight).toBeLessThanOrEqual(4);
  });

  it("stops at a hard deadline but catches up when the stage allows it", async () => {
    const runPastDeadline = (allowPostDeadlineCatchUp) => {
      const clock = makeFakeClock(5_000);
      return runDeadlineBatchedSchedule({
        totalStarts: 10,
        startedAtPerfMs: 0,
        deadlinePerfMs: 1_000,
        intervalMs: 1,
        maxInFlight: 3,
        dispatchStart: () => Promise.resolve(),
        allowPostDeadlineCatchUp,
        now: clock.now,
        sleep: clock.sleep,
      });
    };

    const hardDeadline = await runPastDeadline(false);
    expect(hardDeadline.scheduledStarts).toBe(3);
    expect(hardDeadline.missedStarts).toBe(7);
    expect(hardDeadline.stoppedWithoutCapacity).toBe(false);

    const catchUp = await runPastDeadline(true);
    expect(catchUp.scheduledStarts).toBe(10);
    expect(catchUp.missedStarts).toBe(0);
  });

  it("reports a starved schedule rather than spinning when no start can be dispatched", async () => {
    const clock = makeFakeClock(5_000);
    const result = await runDeadlineBatchedSchedule({
      totalStarts: 5,
      startedAtPerfMs: 0,
      deadlinePerfMs: 1_000,
      intervalMs: 1,
      maxInFlight: 3,
      // No cursor is available: the stress script signals that with `null`.
      dispatchStart: () => null,
      allowPostDeadlineCatchUp: true,
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result).toMatchObject({
      scheduledStarts: 0,
      missedStarts: 5,
      stoppedWithoutCapacity: true,
      lastDispatchedAtPerfMs: null,
    });
  });
});
