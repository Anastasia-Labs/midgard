import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";

import {
  computeMidgardNativeTxIdV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import { walletFromSeed } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { NodeUtxo } from "@/commands/command-utils.js";
import {
  E2E_L2_STRESS_CONFIG_SCHEMA_VERSION,
  E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
  parseE2EL2StressConfig,
  parseE2EL2StressConfigArtifactV1,
  parseE2EL2StressSummaryV1,
  runE2EL2StressThroughput,
  type StressSubmitTransfer,
} from "@/commands/e2e-stress-l2-throughput.js";
import {
  type OpenLoopCorpusRow,
  parseOpenLoopCorpusNdjson,
  planOpenLoopCorpus,
} from "@/commands/stress-open-loop.js";
import {
  buildStressMetrics,
  computeMetricWindow,
} from "@/commands/stress-stage-metrics.js";

import { createTrackedTempDirFactory } from "./helpers/temp-files.js";

const TEST_SEED =
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart";
const OTHER_TEST_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";
const THIRD_TEST_SEED =
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee";

const txHashForIndex = (index: number): string =>
  index.toString(16).padStart(64, "0");

const makeTempDir = createTrackedTempDirFactory("midgard-e2e-stress-");

afterEach(async () => {
  vi.restoreAllMocks();
});

const responseJson = (body: unknown, status = 200): Response =>
  ({
    status,
    text: async () => JSON.stringify(body),
    json: async () => body,
  }) as Response;

const sha256Hex = (bytes: Buffer): string =>
  createHash("sha256").update(bytes).digest("hex");

const jsonClone = <Value>(value: Value): Value =>
  JSON.parse(JSON.stringify(value)) as Value;

const corpusRow = (index: number): OpenLoopCorpusRow => {
  const nativeTransaction = materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor([
        encodeMidgardTxOutput({
          address: Buffer.concat([
            Buffer.from([0x60]),
            Buffer.alloc(28, index + 1),
          ]),
          value: { lovelace: 2_000_000n, assets: new Map() },
        }),
      ]),
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
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(nativeTransaction);
  const txHash = computeMidgardNativeTxIdV1(nativeTransaction).toString("hex");
  return {
    txHash,
    canonicalCborHex: canonicalCbor.toString("hex"),
    canonicalCborSha256: sha256Hex(canonicalCbor),
    canonicalCborByteLength: canonicalCbor.length,
    senderWalletId: `wallet-${index.toString()}`,
    selectedInputOutref: `${txHashForIndex(index + 200)}#0`,
    outputOutrefs: [`${txHash}#0`],
    planShape: "fanout",
    parentTxHash: null,
    corpusSliceId: "slice-a",
  };
};

const writeCorpus = async (
  outDir: string,
  rows: readonly OpenLoopCorpusRow[],
): Promise<string> => {
  const path = `${outDir}/tx-corpus.ndjson`;
  await writeFile(
    path,
    `${rows.map((row) => JSON.stringify(row)).join("\n")}\n`,
    "utf8",
  );
  return path;
};

const makeClock = (stepMs = 1_000) => {
  let time = Date.parse("2026-01-01T00:00:00.000Z");
  return {
    now: () => {
      time += stepMs;
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

  it("resolves unpadded stress wallet env names through canonical padded names", () => {
    const config = parseE2EL2StressConfig({
      mode: "parallel-fanout",
      count: "1",
      concurrency: "1",
      stressWalletSeedPhraseEnvs: ["STRESS_A_1"],
      env: {
        STRESS_A_0001: OTHER_TEST_SEED,
      },
    });

    expect(config.stressWallets[0]?.resolvedWalletSeedPhrase.resolvedFrom).toBe(
      "STRESS_A_0001",
    );
  });

  it("reports every unresolvable stress wallet env var at once", () => {
    expect(() =>
      parseE2EL2StressConfig({
        mode: "parallel-fanout",
        count: "2",
        concurrency: "2",
        stressWalletSeedPhraseEnvs: ["MISSING_A", "MISSING_B_01"],
        env: {},
      }),
    ).toThrow(
      /2\/2 stress wallet env vars are unresolvable:[\s\S]*MISSING_A[\s\S]*MISSING_B_01[\s\S]*MISSING_B_0001/,
    );
  });

  it("keeps duplicate detection when malformed names resolve to the same padded env", () => {
    expect(() =>
      parseE2EL2StressConfig({
        mode: "parallel-fanout",
        count: "2",
        concurrency: "2",
        stressWalletSeedPhraseEnvs: ["STRESS_A_1", "STRESS_A_01"],
        env: {
          STRESS_A_0001: OTHER_TEST_SEED,
        },
      }),
    ).toThrow("Duplicate stress wallet seed source STRESS_A_0001");
  });

  it("requires a prebuilt corpus for open-loop upper-bound runs", () => {
    expect(() =>
      parseE2EL2StressConfig({
        loadModel: "open-loop-upper-bound",
        targetRateTps: "2",
        openLoopDurationMs: "1000",
      }),
    ).toThrow("requires --tx-corpus");
  });
});

// Existing pollIntervalMs: "1" runner fixtures deliberately exercise the
// legacy fixed-interval override path; adaptive backoff has dedicated tests.
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

    expect(result.summary.schemaVersion).toBe(
      E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
    );
    expect(result.summary.loadModel).toBe("closed-loop-smoke");
    expect(result.summary.workloadProfile).toBe("production-end-user");
    expect(result.summary.classification).toBe("closed_loop_smoke");
    expect(result.summary.rateSemantics).toBe("burst_cycle_rate");
    expect(result.summary.burstCycleRatePerSecond).not.toBeNull();
    expect(result.summary.requestedCount).toBe(2);
    expect(result.summary.measurementPolicy).toMatchObject({
      loadModel: "closed-loop-smoke",
      workloadProfile: "production-end-user",
      syntheticVsProduction: "production_end_user_path",
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
    expect(result.summary.metrics.durableAdmission.status).toBe("unavailable");
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
    const parsedConfig = JSON.parse(configJson) as Record<string, unknown>;
    expect(parsedConfig.schemaVersion).toBe(
      E2E_L2_STRESS_CONFIG_SCHEMA_VERSION,
    );
    expect(parseE2EL2StressConfigArtifactV1(parsedConfig)).toEqual(
      parsedConfig,
    );
    const missingConfig = { ...parsedConfig };
    delete missingConfig.runId;
    expect(() => parseE2EL2StressConfigArtifactV1(missingConfig)).toThrow(
      "missing required field",
    );
    expect(() =>
      parseE2EL2StressConfigArtifactV1({
        ...parsedConfig,
        unexpected: true,
      }),
    ).toThrow("unknown field");
    expect(() =>
      parseE2EL2StressConfigArtifactV1({
        ...parsedConfig,
        schemaVersion: "wrong-config-version",
      }),
    ).toThrow(E2E_L2_STRESS_CONFIG_SCHEMA_VERSION);
    expect(configJson).not.toContain(TEST_SEED);
    expect(configJson).toContain('"advanceOn": "accepted"');
    expect(configJson).not.toContain("commitTimeoutMs");
    await expect(readFile(result.eventsNdjsonPath, "utf8")).resolves.toContain(
      "transfer_submitted",
    );
    const summaryJson = await readFile(result.summaryJsonPath, "utf8");
    const parsedSummary = JSON.parse(summaryJson) as Record<string, unknown>;
    expect(parsedSummary.schemaVersion).toBe(
      E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
    );
    expect(parseE2EL2StressSummaryV1(parsedSummary)).toEqual(parsedSummary);
    const missingSummary = { ...parsedSummary };
    delete missingSummary.runId;
    expect(() => parseE2EL2StressSummaryV1(missingSummary)).toThrow(
      "missing required field",
    );
    expect(() =>
      parseE2EL2StressSummaryV1({
        ...parsedSummary,
        unexpected: true,
      }),
    ).toThrow("unknown field");
    expect(() =>
      parseE2EL2StressSummaryV1({
        ...parsedSummary,
        schemaVersion: "wrong-summary-version",
      }),
    ).toThrow(E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION);
    const contradictoryCount = jsonClone(parsedSummary);
    contradictoryCount.submittedCount = 1;
    expect(() => parseE2EL2StressSummaryV1(contradictoryCount)).toThrow(
      "counts",
    );
    const contradictoryDuration = jsonClone(parsedSummary);
    contradictoryDuration.durationMs = 1;
    expect(() => parseE2EL2StressSummaryV1(contradictoryDuration)).toThrow(
      "chronology",
    );
    const contradictoryPolicy = jsonClone(parsedSummary);
    const policy = contradictoryPolicy.measurementPolicy as Record<
      string,
      unknown
    >;
    policy.advanceOn = "scheduled_submit";
    expect(() => parseE2EL2StressSummaryV1(contradictoryPolicy)).toThrow(
      "policy",
    );
    const contradictoryTransaction = jsonClone(parsedSummary);
    const transactions = contradictoryTransaction.transactions as Record<
      string,
      unknown
    >[];
    const firstTransaction = transactions[0]!;
    const submission = firstTransaction.submission as Record<string, unknown>;
    submission.submittedAt = null;
    expect(() => parseE2EL2StressSummaryV1(contradictoryTransaction)).toThrow(
      "transactions[0]",
    );
    const duplicateTransactionIdentity = jsonClone(parsedSummary);
    const duplicateTransactions =
      duplicateTransactionIdentity.transactions as Record<string, unknown>[];
    duplicateTransactions[1]!.txHash = duplicateTransactions[0]!.txHash;
    expect(() =>
      parseE2EL2StressSummaryV1(duplicateTransactionIdentity),
    ).toThrow("identities");
    const contradictoryMetric = jsonClone(parsedSummary);
    const metrics = contradictoryMetric.metrics as Record<string, unknown>;
    const clientSubmission = metrics.clientSubmission as Record<
      string,
      unknown
    >;
    clientSubmission.perSecond = 999;
    expect(() => parseE2EL2StressSummaryV1(contradictoryMetric)).toThrow(
      "rate",
    );
    const missingInterruptionReason = jsonClone(parsedSummary);
    missingInterruptionReason.status = "interrupted";
    expect(() => parseE2EL2StressSummaryV1(missingInterruptionReason)).toThrow(
      "status",
    );
    expect(() => parseE2EL2StressSummaryV1(parsedConfig)).toThrow();
    expect(summaryJson).toContain('"observedCommittedCount": 2');
    expect(parsedSummary).toHaveProperty("metrics");
    expect(parsedSummary).not.toHaveProperty("throughput");
    expect(summaryJson).not.toContain("committedCount");
    expect(summaryJson).not.toContain("timedOutCount");
    expect(summaryJson).not.toContain("unknownCount");
    const summaryMarkdown = await readFile(result.summaryMarkdownPath, "utf8");
    expect(summaryMarkdown).toContain("# Midgard L2 Stress Summary");
    expect(summaryMarkdown).toContain("burstCycleRatePerSecond");
    expect(summaryMarkdown).not.toMatch(/\bTPS\b/);
    expect(summaryMarkdown).not.toMatch(/\btx\/s\b/);
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
      feeHeadroomLovelace: "0",
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

  it("rejects parallel fanout wallets funded without fee headroom", async () => {
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
        fetchUtxos: async () => [{ ...fakeUtxo, assets: { lovelace: 1_000n } }],
        now: clock.now,
        sleep: clock.sleep,
      }),
    ).rejects.toThrow(
      "at least 501000 lovelace (transfer 1000 + fee headroom 500000)",
    );
    expect(submitTransfer).not.toHaveBeenCalled();
  });

  it("allows parallel fanout wallets that satisfy transfer plus fee headroom", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      mode: "parallel-fanout",
      count: "1",
      concurrency: "1",
      lovelace: "1000",
      feeHeadroomLovelace: "500",
      stressWalletSeedPhraseEnvs: ["STRESS_A"],
      env: {
        STRESS_A: OTHER_TEST_SEED,
      },
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(10),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"aa".repeat(32)}#0`],
      requestedAssets: { lovelace: 1_000n },
      changeAssets: { lovelace: 500n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: (async () =>
        responseJson({ status: "committed" })) as typeof fetch,
      fetchUtxos: async () => [{ ...fakeUtxo, assets: { lovelace: 1_500n } }],
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.status).toBe("completed");
    expect(result.summary.submittedCount).toBe(1);
    expect(submitTransfer).toHaveBeenCalledTimes(1);
  });

  it("interrupts after the first submission failure by default", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "3",
      outDir,
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async () => {
      throw new Error("builder ran out of lovelace");
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: (async () =>
        responseJson({ status: "committed" })) as typeof fetch,
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.status).toBe("interrupted");
    expect(result.summary.interruptedReason).toContain(
      "submission/build failure threshold exceeded",
    );
    expect(result.summary.submissionFailedCount).toBe(1);
    expect(result.summary.notStartedCount).toBe(2);
    expect(submitTransfer).toHaveBeenCalledTimes(1);
  });

  it("allows the configured number of submission failures before interrupting", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "5",
      maxSubmissionFailures: "2",
      outDir,
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async () => {
      throw new Error("submit endpoint unavailable");
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: (async () =>
        responseJson({ status: "committed" })) as typeof fetch,
      now: clock.now,
      sleep: clock.sleep,
    });

    expect(result.summary.status).toBe("interrupted");
    expect(result.summary.submissionFailedCount).toBe(3);
    expect(result.summary.notStartedCount).toBe(2);
    expect(submitTransfer).toHaveBeenCalledTimes(3);
  });

  it("does not count acceptance rejections toward submission failure aborts", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "3",
      maxSubmissionFailures: "0",
      outDir,
      pollIntervalMs: "1",
      acceptanceTimeoutMs: "1000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(request.index + 70),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"bb".repeat(32)}#${request.index.toString()}`],
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

    expect(result.summary.status).toBe("completed");
    expect(result.summary.submissionFailedCount).toBe(0);
    expect(result.summary.rejectedCount).toBe(3);
    expect(submitTransfer).toHaveBeenCalledTimes(3);
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

  it("uses adaptive poll backoff when no fixed poll interval is configured", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "1",
      outDir,
      pollInitialIntervalMs: "50",
      pollMaxIntervalMs: "200",
      acceptanceTimeoutMs: "100000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(80),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"cc".repeat(32)}#0`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));
    let polls = 0;
    const fetchImpl = vi.fn(async () => {
      polls += 1;
      return responseJson({
        status: polls <= 4 ? "pending" : "committed",
      });
    });
    const sleepSpy = vi.fn(async (ms: number) => {
      await clock.sleep(ms);
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: fetchImpl as typeof fetch,
      now: clock.now,
      sleep: sleepSpy,
    });

    expect(result.summary.acceptedCount).toBe(1);
    expect(sleepSpy.mock.calls.map(([ms]) => ms)).toEqual([50, 100, 200, 200]);
  });

  it("keeps the legacy fixed poll interval when explicitly configured", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock();
    const config = parseE2EL2StressConfig({
      walletSeedPhrase: TEST_SEED,
      count: "1",
      outDir,
      pollIntervalMs: "37",
      pollInitialIntervalMs: "50",
      pollMaxIntervalMs: "200",
      acceptanceTimeoutMs: "100000",
      commitObservationTimeoutMs: "1000",
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async (request) => ({
      txId: txHashForIndex(81),
      status: "queued",
      senderAddress: request.walletAddress,
      destinationAddress: request.destinationAddress,
      selectedInputs: [`${"cd".repeat(32)}#0`],
      requestedAssets: { lovelace: 1_000_000n },
      changeAssets: { lovelace: 9_000_000n },
      walletSeedSource: request.walletSeedSource,
      nodeEndpoint: request.config.nodeEndpoint,
    }));
    let polls = 0;
    const fetchImpl = vi.fn(async () => {
      polls += 1;
      return responseJson({
        status: polls <= 4 ? "pending" : "committed",
      });
    });
    const sleepSpy = vi.fn(async (ms: number) => {
      await clock.sleep(ms);
    });

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetch: fetchImpl as typeof fetch,
      now: clock.now,
      sleep: sleepSpy,
    });

    expect(result.summary.acceptedCount).toBe(1);
    expect(sleepSpy.mock.calls.map(([ms]) => ms)).toEqual([37, 37, 37, 37]);
  });

  it("runs open-loop upper-bound from a corpus without wallet or tx-status work", async () => {
    const outDir = await makeTempDir();
    const clock = makeClock(1);
    const rows = [corpusRow(1), corpusRow(2)];
    const corpusPath = await writeCorpus(outDir, rows);
    const config = parseE2EL2StressConfig({
      loadModel: "open-loop-upper-bound",
      corpusPath,
      corpusSliceId: "slice-a",
      targetRateTps: "2",
      openLoopDurationMs: "1000",
      openLoopMaxInFlight: "2",
      outDir,
    });
    const submitTransfer = vi.fn<StressSubmitTransfer>(async () => {
      throw new Error("open-loop must not call submit-l2-transfer");
    });
    const observedSamples: unknown[] = [];
    let submittedAtValues: number[] = [];

    const result = await runE2EL2StressThroughput(config, {
      submitTransfer,
      fetchUtxos: async () => {
        throw new Error("open-loop must not fetch /utxos");
      },
      runCanonicalEngine: async ({ paths }) => {
        submittedAtValues = rows.map(() => clock.now().getTime());
        await writeFile(
          paths.submitRecordsNdjson,
          rows
            .map((row, index) =>
              JSON.stringify({
                txHash: row.txHash,
                scheduledAtMs: submittedAtValues[index],
                submittedAtMs: submittedAtValues[index],
                scheduleSlipMs: 0,
                latencyMs: 1,
                statusCode: 202,
                responseTxId: row.txHash,
                error: null,
              }),
            )
            .join("\n") + "\n",
          "utf8",
        );
        await writeFile(
          paths.engineEventsNdjson,
          [
            {
              event: "engine_started",
              at: "2026-01-01T00:00:00.000Z",
            },
            {
              event: "stage_started",
              at: "2026-01-01T00:00:00.000Z",
              name: "measured-open",
              targetRateTps: 2,
            },
            {
              event: "stage_finished",
              at: "2026-01-01T00:00:01.000Z",
              name: "measured-open",
              submitted: 2,
              submitErrors: 0,
            },
          ]
            .map((event) => JSON.stringify(event))
            .join("\n") + "\n",
          "utf8",
        );
        await writeFile(
          paths.engineReportJson,
          JSON.stringify({ calibration: { noOp: null } }),
          "utf8",
        );
        await writeFile(paths.noopCalibrationJson, "{}\n", "utf8");
        return { exitCode: 0, signal: null };
      },
      collectAggregateObserverSample: async (sample) => {
        observedSamples.push(sample);
        return { ok: true };
      },
      collectStageMetricSources: async ({ txHashes }) => ({
        l2Admissions: txHashes.map((txHash, index) => ({
          txHash,
          status: "accepted",
          firstSeenAt: new Date(submittedAtValues[index]!).toISOString(),
          validationStartedAt: new Date(
            submittedAtValues[index]! + 1,
          ).toISOString(),
          terminalAt: new Date(submittedAtValues[index]! + 2).toISOString(),
        })),
        l1Commits: [],
        immutableObservations: [],
        residue: [],
      }),
      now: clock.now,
      sleep: async () => undefined,
    });

    expect(submitTransfer).not.toHaveBeenCalled();
    expect(observedSamples.length).toBeGreaterThanOrEqual(0);
    expect(result.summary.loadModel).toBe("open-loop-upper-bound");
    expect(result.summary.workloadProfile).toBe("synthetic-admission");
    expect(result.summary.rateSemantics).toBe("offered_tps_uncalibrated");
    expect(result.summary.burstCycleRatePerSecond).toBeNull();
    expect(result.summary.measurementPolicy).toMatchObject({
      advanceOn: "scheduled_submit",
      finalityObservation: "aggregate-window",
      primaryStageMetric: "metrics.durableAdmission.perSecond",
    });
    expect(result.summary.finalityObserver.pollRequestCount).toBe(0);
    expect(result.summary.metrics.durableAdmission.count).toBe(2);
    expect(result.summary.metrics.l2Admission.count).toBe(2);
    expect(result.summary.classification).toBe("ingress_ok_commit_failed");
    expect(result.summary.artifactPaths.engineReportJson).toContain(
      "engine-report.json",
    );
  });
});

describe("open-loop corpus planning", () => {
  it("rejects duplicate input outrefs within a corpus slice", () => {
    const rows = [
      corpusRow(1),
      {
        ...corpusRow(2),
        selectedInputOutref: corpusRow(1).selectedInputOutref,
      },
    ];
    const parsed = parseOpenLoopCorpusNdjson(
      rows.map((row) => JSON.stringify(row)).join("\n"),
    );

    expect(() =>
      planOpenLoopCorpus({
        rows: parsed,
        targetRateTps: 2,
        durationMs: 1000,
        warmupCount: 0,
        cooldownCount: 0,
        corpusShape: "fanout",
        corpusSliceId: "slice-a",
      }),
    ).toThrow("duplicate selected input");
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
            validationStartedAt: "2026-01-01T00:00:01.500Z",
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
