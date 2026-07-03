import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { walletFromSeed } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { NodeUtxo } from "@/commands/command-utils.js";
import {
  E2E_L2_STRESS_SCHEMA_VERSION,
  parseE2EL2StressConfig,
  runE2EL2StressThroughput,
  type StressSubmitTransfer,
} from "@/commands/e2e-stress-l2-throughput.js";
import {
  buildStressMetrics,
  computeMetricWindow,
} from "@/commands/stress-stage-metrics.js";

const TEST_SEED =
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart";
const OTHER_TEST_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";
const THIRD_TEST_SEED =
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee";

const txHashForIndex = (index: number): string =>
  index.toString(16).padStart(64, "0");

let tempDirs: string[] = [];

const makeTempDir = async (): Promise<string> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-e2e-stress-"));
  tempDirs.push(dir);
  return dir;
};

afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(
    tempDirs.map((dir) => rm(dir, { recursive: true, force: true })),
  );
  tempDirs = [];
});

const responseJson = (body: unknown, status = 200): Response =>
  ({
    status,
    text: async () => JSON.stringify(body),
  }) as Response;

const makeClock = () => {
  let time = Date.parse("2026-01-01T00:00:00.000Z");
  return {
    now: () => {
      time += 1_000;
      return new Date(time);
    },
    sleep: async (ms: number) => {
      time += ms;
    },
  };
};

const fakeUtxo = {
  txHash: "11".repeat(32),
  outputIndex: 0,
  outrefCbor: Buffer.from("00", "hex"),
  outputCbor: Buffer.from("00", "hex"),
  address: walletFromSeed(TEST_SEED, { network: "Preprod" }).address,
  assets: { lovelace: 10_000_000n },
} satisfies NodeUtxo;

describe("e2e-stress-l2-throughput config", () => {
  it("rejects non-positive and unbounded parameters by default", () => {
    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        count: "0",
      }),
    ).toThrow("--count must be a safe positive integer");

    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        count: "501",
      }),
    ).toThrow("exceeds the default cap");
  });

  it("rejects unsafe shared-wallet concurrency", () => {
    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        count: "2",
        concurrency: "2",
      }),
    ).toThrow("--concurrency > 1 requires --mode parallel-fanout");

    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        mode: "parallel-fanout",
        count: "2",
        concurrency: "2",
        stressWalletSeedPhraseEnvs: ["STRESS_A"],
        env: {
          STRESS_A: OTHER_TEST_SEED,
        },
      }),
    ).toThrow("requires at least 2 independent");
  });

  it("accepts pre-funded independent wallet seeds for bounded fanout", () => {
    const config = parseE2EL2StressConfig({
      mode: "parallel-fanout",
      count: "4",
      concurrency: "2",
      stressWalletSeedPhraseEnvs: ["STRESS_A", "STRESS_B"],
      env: {
        STRESS_A: OTHER_TEST_SEED,
        STRESS_B: THIRD_TEST_SEED,
      },
    });

    expect(config.mode).toBe("parallel-fanout");
    expect(config.primaryWallet).toBeUndefined();
    expect(config.stressWallets).toHaveLength(2);
    expect(
      new Set(config.stressWallets.map((wallet) => wallet.address)).size,
    ).toBe(2);
  });
});

describe("e2e-stress-l2-throughput runner", () => {
  it("runs serial stress, polls tx-status, and writes all artifacts", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "2",
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(request.index + 1),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"22".repeat(32)}#${request.index.toString()}`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));
    const fetchImpl = vi.fn(async (input: RequestInfo | URL) => {
      expect(String(input)).toContain("/tx-status?tx_hash=");
      return responseJson({
        status: "committed",
      });
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: fetchImpl as typeof fetch,
      fetchUtxos: async () => [fakeUtxo],
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.schemaVersion).toBe(E2E_L2_STRESS_SCHEMA_VERSION);
    expect(result.summary.requestedCount).toBe(2);
    expect(result.summary.measurementPolicy).toMatchObject({
      advanceOn: "accepted",
      primaryStageMetric: "metrics.l2Admission.perSecond",
      finalityObservation: "post-submit-bounded",
      submissionWindowExcludesCommitDrain: true,
      fullFinalityRequiresDrainProof: true,
    });
    expect(result.summary.submittedCount).toBe(2);
    expect(result.summary.submissionFailedCount).toBe(0);
    expect(result.summary.observedCommittedCount).toBe(2);
    expect(result.summary.metrics.clientSubmission.count).toBe(2);
    expect(result.summary.metrics.l2Admission).toMatchObject({
      status: "complete",
      count: 2,
      precision: "observer_timestamp",
    });
    expect(result.summary.metrics.immutableObservation).toMatchObject({
      status: "complete",
      count: 2,
      precision: "observer_timestamp",
    });
    expect(result.summary.metrics.fullFinality).toMatchObject({
      status: "unavailable",
      count: 0,
    });
    expect(result.summary.rejectedCount).toBe(0);
    expect(submitTransfer).toHaveBeenCalledTimes(2);
    expect(
      submitTransfer.mock.calls[0]?.[0].config.submitRequestTimeoutMs,
    ).toBe(300_000);
    const configJson = await readFile(result.configJsonPath, "utf8");
    expect(configJson).not.toContain(TEST_SEED);
    expect(configJson).toContain('"advanceOn": "accepted"');
    expect(configJson).not.toContain("commitTimeoutMs");
    await expect(readFile(result.eventsNdjsonPath, "utf8")).resolves.toContain(
      "transfer_submitted",
    );
    const summaryJson = await readFile(result.summaryJsonPath, "utf8");
    const parsedSummary = JSON.parse(summaryJson) as Record<string, unknown>;
    expect(summaryJson).toContain('"observedCommittedCount": 2');
    expect(parsedSummary).toHaveProperty("metrics");
    expect(parsedSummary).not.toHaveProperty("throughput");
    expect(summaryJson).not.toContain("committedCount");
    expect(summaryJson).not.toContain("timedOutCount");
    expect(summaryJson).not.toContain("unknownCount");
    await expect(
      readFile(result.summaryMarkdownPath, "utf8"),
    ).resolves.toContain("# Midgard L2 Stress Summary");
  });

  it("submits the next chained tx after accepted without waiting for committed", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "2",
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    let releaseFirstCommit:
      | ((response: Response | PromiseLike<Response>) => void)
      | undefined;
    let secondSubmitSeen = false;
    let firstCommitReleased = false;
    const releaseFirstCommitIfReady = () => {
      if (releaseFirstCommit === undefined || firstCommitReleased) {
        return;
      }
      firstCommitReleased = true;
      releaseFirstCommit(
        responseJson({
          status: "committed",
        }),
      );
    };
    const firstTxHash = txHashForIndex(1);
    const secondTxHash = txHashForIndex(2);
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => {
      if (request.index === 1) {
        secondSubmitSeen = true;
        releaseFirstCommitIfReady();
      }
      return {
        txId: request.index === 0 ? firstTxHash : secondTxHash,
        status: "queued",
        senderAddress: request.walletAddress,
        destinationAddress: request.destinationAddress,
        selectedInputs: [`${"44".repeat(32)}#${request.index.toString()}`],
        requestedAssets: { lovelace: 1_000_000n },
        changeAssets: { lovelace: 9_000_000n },
        walletSeedSource: request.walletSeedSource,
        nodeEndpoint: request.config.nodeEndpoint,
      };
    });
    let firstTxStatusPolls = 0;
    const fetchImpl = vi.fn(async (input: RequestInfo | URL) => {
      const url = String(input);
      if (url.includes(firstTxHash)) {
        firstTxStatusPolls += 1;
        if (firstTxStatusPolls === 1) {
          return responseJson({
            status: "accepted",
          });
        }
        return new Promise<Response>((resolve) => {
          releaseFirstCommit = resolve;
          if (secondSubmitSeen) {
            releaseFirstCommitIfReady();
          }
        });
      }
      return responseJson({
        status: "committed",
      });
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: fetchImpl as typeof fetch,
      fetchUtxos: async () => [fakeUtxo],
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(submitTransfer).toHaveBeenCalledTimes(2);
    const secondSubmitOrder = submitTransfer.mock.invocationCallOrder[1]!;
    const finalFetchOrders = fetchImpl.mock.invocationCallOrder;
    const lastFetchOrder = finalFetchOrders[finalFetchOrders.length - 1]!;
    expect(secondSubmitOrder).toBeLessThan(lastFetchOrder);
    expect(firstCommitReleased).toBe(true);
    expect(result.summary.submittedCount).toBe(2);
    expect(result.summary.acceptedCount).toBe(2);
    expect(result.summary.observedCommittedCount).toBe(2);
    expect(result.summary.transactions[0]).toMatchObject({
      acceptance: { status: "accepted" },
      finality: { status: "committed" },
    });
  });

  it("does not perform per-transfer UTxO preflight in serial stress", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "2",
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(request.index + 20),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"77".repeat(32)}#${request.index.toString()}`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));
    const fetchUtxos = vi.fn(async () => {
      throw new Error("serial stress must not preflight per-transfer UTxOs");
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: (async () =>
        responseJson({
          status: "committed",
        })) as typeof fetch,
      fetchUtxos,
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.status).toBe("completed");
    expect(result.summary.acceptedCount).toBe(2);
    expect(fetchUtxos).not.toHaveBeenCalled();
  });

  it("caps post-submit finality observation concurrency", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      mode: "parallel-fanout",
      count: "3",
      concurrency: "3",
      stressWalletSeedPhraseEnvs: ["STRESS_A", "STRESS_B", "STRESS_C"],
      env: {
        STRESS_A: TEST_SEED,
        STRESS_B: OTHER_TEST_SEED,
        STRESS_C: THIRD_TEST_SEED,
      },
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "10000",
      finalityObserverMaxConcurrentRequests: "2",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(request.index + 30),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"88".repeat(32)}#${request.index.toString()}`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));
    const statusReadsByTx = new Map<string, number>();
    let activeFinalityReads = 0;
    let maxActiveFinalityReads = 0;
    const fetchImpl = vi.fn(async (input: RequestInfo | URL) => {
      const txHash = String(input).match(/tx_hash=([0-9a-f]+)/)?.[1] ?? "";
      const count = statusReadsByTx.get(txHash) ?? 0;
      statusReadsByTx.set(txHash, count + 1);
      if (count === 0) {
        return responseJson({ status: "accepted" });
      }
      activeFinalityReads += 1;
      maxActiveFinalityReads = Math.max(
        maxActiveFinalityReads,
        activeFinalityReads,
      );
      await Promise.resolve();
      activeFinalityReads -= 1;
      return responseJson({ status: "committed" });
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: fetchImpl as typeof fetch,
      fetchUtxos: async () => [fakeUtxo],
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.observedCommittedCount).toBe(3);
    expect(result.summary.finalityObserver.maxConcurrentRequests).toBe(2);
    expect(
      result.summary.finalityObserver.maxObservedConcurrentRequests,
    ).toBeLessThanOrEqual(2);
    expect(maxActiveFinalityReads).toBeLessThanOrEqual(2);
  });

  it("writes an interrupted partial summary when the runtime aborts", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const abortController = new AbortController();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "3",
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(request.index + 40),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"99".repeat(32)}#${request.index.toString()}`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));
    const fetchImpl = vi.fn(async () => {
      abortController.abort(new Error("test abort"));
      return responseJson({ status: "accepted" });
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: fetchImpl as typeof fetch,
      now: clock.now,
      sleep: clock.sleep,
      abortSignal: abortController.signal,
    });

    expect(result.summary).toMatchObject({
      status: "interrupted",
      requestedCount: 3,
      submittedCount: 1,
      acceptedCount: 1,
      notStartedCount: 2,
      unknownFinalityCount: 1,
    });
    await expect(readFile(result.summaryJsonPath, "utf8")).resolves.toContain(
      '"status": "interrupted"',
    );
  });

  it("classifies acceptance timeout without counting it as admitted", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "1",
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(7),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"55".repeat(32)}#0`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: (async () =>
        responseJson({
          status: "pending",
        })) as typeof fetch,
      fetchUtxos: async () => [fakeUtxo],
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.submittedCount).toBe(1);
    expect(result.summary.acceptedCount).toBe(0);
    expect(result.summary.acceptanceTimedOutCount).toBe(1);
    expect(result.summary.finalityTimedOutCount).toBe(0);
    expect(result.summary.transactions[0]).toMatchObject({
      txHash: txHashForIndex(7),
      submission: { status: "submitted" },
      acceptance: { status: "timeout" },
      finality: { status: "not_observed" },
    });
  });

  it("keeps finality observation timeout separate from L2 admission", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "1",
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(8),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"66".repeat(32)}#0`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));
    let txStatusPolls = 0;

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: (async () => {
        txStatusPolls += 1;
        return responseJson({
          status: txStatusPolls === 1 ? "accepted" : "pending_commit",
        });
      }) as typeof fetch,
      fetchUtxos: async () => [fakeUtxo],
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.submittedCount).toBe(1);
    expect(result.summary.acceptedCount).toBe(1);
    expect(result.summary.observedCommittedCount).toBe(0);
    expect(result.summary.finalityTimedOutCount).toBe(1);
    expect(result.summary.metrics.l2Admission.perSecond).toBeGreaterThan(0);
    expect(result.summary.metrics.fullFinality.status).toBe("unavailable");
    expect(result.summary.transactions[0]).toMatchObject({
      txHash: txHashForIndex(8),
      submission: { status: "submitted" },
      acceptance: { status: "accepted" },
      finality: { status: "timeout" },
    });
  });

  it("rejects parallel fanout wallets without enough lovelace during preflight", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      mode: "parallel-fanout",
      count: "1",
      concurrency: "1",
      lovelace: "1000",
      stressWalletSeedPhraseEnvs: ["STRESS_A"],
      env: {
        STRESS_A: OTHER_TEST_SEED,
      },
      outDir,
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async () => {
      throw new Error("underfunded preflight must fail before submission");
    });

    await expect(
      runE2EL2StressThroughput(config, {
        submitTransfer,
        fetch: (async () =>
          responseJson({ status: "committed" })) as typeof fetch,
        fetchUtxos: async () => [{ ...fakeUtxo, assets: { lovelace: 999n } }],
        now: clock.now,
        sleep: clock.sleep,
      }),
    ).rejects.toThrow("at least 1000 lovelace");
    expect(submitTransfer).not.toHaveBeenCalled();
  });

  it("records rejected stress transactions as failed evidence", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "1",
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(9),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"33".repeat(32)}#0`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: (async () =>
        responseJson({
          status: "rejected",
          reasonCode: "phase_b_rejected",
        })) as typeof fetch,
      fetchUtxos: async () => [fakeUtxo],
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.observedCommittedCount).toBe(0);
    expect(result.summary.rejectedCount).toBe(1);
    expect(result.summary.transactions[0]).toMatchObject({
      txHash: txHashForIndex(9),
      acceptance: { status: "rejected" },
      finality: { status: "rejected" },
    });
  });
});

describe("stress stage metrics", () => {
  it("uses null rate instead of infinity for a zero-duration window", () => {
    const metric = computeMetricWindow({
      count: 1,
      expectedCount: 1,
      startedAt: "2026-01-01T00:00:00.000Z",
      finishedAt: "2026-01-01T00:00:00.000Z",
      source: "test",
      precision: "artifact_timestamp",
    });

    expect(metric).toMatchObject({
      status: "complete",
      durationMs: 0,
      perSecond: null,
      notes: ["zero_duration_window"],
    });
  });

  it("marks missing DB admission rows as partial", () => {
    const firstTxHash = txHashForIndex(91);
    const secondTxHash = txHashForIndex(92);
    const metrics = buildStressMetrics({
      requestedCount: 2,
      submittedCount: 2,
      acceptedCount: 2,
      observedCommittedCount: 0,
      startedAt: "2026-01-01T00:00:00.000Z",
      submissionFinishedAt: "2026-01-01T00:00:02.000Z",
      finishedAt: "2026-01-01T00:00:03.000Z",
      transactions: [
        {
          txHash: firstTxHash,
          submission: {
            status: "submitted",
            submittedAt: "2026-01-01T00:00:01.000Z",
          },
          acceptance: {
            status: "accepted",
            acceptedAt: "2026-01-01T00:00:02.000Z",
          },
          finality: { status: "not_observed" },
        },
        {
          txHash: secondTxHash,
          submission: {
            status: "submitted",
            submittedAt: "2026-01-01T00:00:01.500Z",
          },
          acceptance: {
            status: "accepted",
            acceptedAt: "2026-01-01T00:00:02.500Z",
          },
          finality: { status: "not_observed" },
        },
      ],
      dbSources: {
        l2Admissions: [
          {
            txHash: firstTxHash,
            status: "accepted",
            firstSeenAt: "2026-01-01T00:00:01.000Z",
            terminalAt: "2026-01-01T00:00:02.000Z",
          },
        ],
        l1Commits: [],
        immutableObservations: [],
        residue: [],
      },
    });

    expect(metrics.l2Admission).toMatchObject({
      status: "partial",
      count: 1,
      missingCount: 1,
      precision: "db_timestamp",
    });
  });
});
