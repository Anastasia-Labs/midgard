#!/usr/bin/env node

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import {
  PerformanceObserver,
  monitorEventLoopDelay,
  performance,
} from "node:perf_hooks";
import { fileURLToPath } from "node:url";
import { encode as cborEncode } from "cborg";
import dotenv from "dotenv";
import { blake2b } from "@noble/hashes/blake2.js";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { Pool } from "undici";
import {
  BENCHMARK_WINDOWS_MS,
  acceptedStatuses,
  classifyLikelyBottleneckWithEvidence,
  counterDelta,
  createPhaseRecorder,
  gaugeSlopePerSec,
  isDrainComplete,
  rateBetweenCounters,
  summarizeCounterWindow,
  summarizeLatency,
  summarizeRollingRates,
  terminalStatuses,
} from "./throughput-benchmark-utils.mjs";

const MIDGARD_NATIVE_TX_VERSION = 1n;
const MIDGARD_POSIX_TIME_NONE = -1n;
const MIDGARD_NETWORK_ID_PREPROD = 0n;
const TX_IS_VALID_CODE = 0n;
const HASH32_LEN = 32;

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const pkgRoot = path.resolve(__dirname, "..");

const envPath = process.env.STRESS_ENV_FILE ?? path.join(pkgRoot, ".env");
const submitEndpoint =
  process.env.STRESS_SUBMIT_ENDPOINT ?? "http://127.0.0.1:3000";
const metricsEndpoint =
  process.env.STRESS_METRICS_ENDPOINT ?? "http://127.0.0.1:9464/metrics";
const chainLength = Number.parseInt(
  process.env.STRESS_CHAIN_LENGTH ?? "500",
  10,
);
const maxChains = Number.parseInt(process.env.STRESS_MAX_CHAINS ?? "8", 10);
const utxosPerWallet = Number.parseInt(
  process.env.STRESS_UTXOS_PER_WALLET ?? "3",
  10,
);
const minLovelace = BigInt(process.env.STRESS_MIN_LOVELACE ?? "0");
const fanoutEnabled =
  (process.env.STRESS_FANOUT_ENABLED ?? "true").trim().toLowerCase() !==
  "false";
const fanoutMaxOutputsPerTx = Number.parseInt(
  process.env.STRESS_FANOUT_MAX_OUTPUTS_PER_TX ?? "256",
  10,
);
const fanoutOutputLovelace =
  process.env.STRESS_FANOUT_OUTPUT_LOVELACE === undefined
    ? null
    : BigInt(process.env.STRESS_FANOUT_OUTPUT_LOVELACE);
const fanoutStatusTimeoutMs = Number.parseInt(
  process.env.STRESS_FANOUT_STATUS_TIMEOUT_MS ?? "30000",
  10,
);
const retry503 = Number.parseInt(process.env.STRESS_RETRY_503 ?? "3", 10);
const measuredRetry503 = Number.parseInt(
  process.env.STRESS_MEASURED_RETRY_503 ?? "0",
  10,
);
const retryDelayMs = Number.parseInt(
  process.env.STRESS_RETRY_DELAY_MS ?? "25",
  10,
);
const metricsPollMs = Number.parseInt(
  process.env.STRESS_METRICS_POLL_MS ?? "1000",
  10,
);
const observeAfterSubmitSec = Number.parseInt(
  process.env.STRESS_OBSERVE_AFTER_SUBMIT_SEC ?? "15",
  10,
);
const targetAcceptedTps = Number.parseFloat(
  process.env.STRESS_TARGET_ACCEPTED_TPS ?? "600",
);
const requireFreshChains =
  (process.env.STRESS_REQUIRE_FRESH_CHAINS ?? "true").trim().toLowerCase() !==
  "false";
const txStatusRetries = Number.parseInt(
  process.env.STRESS_TX_STATUS_RETRIES ?? "5",
  10,
);
const txStatusRetryDelayMs = Number.parseInt(
  process.env.STRESS_TX_STATUS_RETRY_DELAY_MS ?? "50",
  10,
);
const benchmarkMode = (process.env.STRESS_MODE ?? "closed")
  .trim()
  .toLowerCase();
const measuredSec = Number.parseFloat(process.env.STRESS_MEASURED_SEC ?? "30");
const warmupTxs = Number.parseInt(process.env.STRESS_WARMUP_TXS ?? "0", 10);
const warmupSec = Number.parseFloat(process.env.STRESS_WARMUP_SEC ?? "0");
const cooldownSec = Number.parseFloat(process.env.STRESS_COOLDOWN_SEC ?? "3");
const drainTimeoutSec = Number.parseFloat(
  process.env.STRESS_DRAIN_TIMEOUT_SEC ?? "60",
);
const waitForCommit =
  (process.env.STRESS_WAIT_FOR_COMMIT ?? "false").trim().toLowerCase() ===
  "true";
const waitForMerge =
  (process.env.STRESS_WAIT_FOR_MERGE ?? "false").trim().toLowerCase() ===
  "true";
const statusSampleSize = Number.parseInt(
  process.env.STRESS_STATUS_SAMPLE_SIZE ?? "100",
  10,
);
const submitConcurrency = Number.parseInt(
  process.env.STRESS_SUBMIT_CONCURRENCY ?? "512",
  10,
);
const httpConnections = Number.parseInt(
  process.env.STRESS_HTTP_CONNECTIONS ?? "128",
  10,
);
const httpPipelining = Number.parseInt(
  process.env.STRESS_HTTP_PIPELINING ?? "1",
  10,
);
const httpTimeoutMs = Number.parseInt(
  process.env.STRESS_HTTP_TIMEOUT_MS ?? "30000",
  10,
);
const openLoopRate = Number.parseFloat(
  process.env.STRESS_OPEN_LOOP_RATE_TPS ?? String(targetAcceptedTps),
);
const rampStartTps = Number.parseFloat(
  process.env.STRESS_RAMP_START_TPS ?? "100",
);
const rampStepTps = Number.parseFloat(
  process.env.STRESS_RAMP_STEP_TPS ?? "100",
);
const rampMaxTps = Number.parseFloat(
  process.env.STRESS_RAMP_MAX_TPS ??
    String(targetAcceptedTps > 0 ? targetAcceptedTps : 1000),
);
const rampStageSec = Number.parseFloat(
  process.env.STRESS_RAMP_STAGE_SEC ?? "15",
);
const rampMinAcceptedRatio = Number.parseFloat(
  process.env.STRESS_RAMP_MIN_ACCEPTED_RATIO ?? "0.99",
);
const offeredRateMinRatio = Number.parseFloat(
  process.env.STRESS_OFFERED_RATE_MIN_RATIO ?? "0.98",
);
const acceptedRateMinRatio = Number.parseFloat(
  process.env.STRESS_ACCEPTED_RATE_MIN_RATIO ?? "0.99",
);
const scheduleLagP95MaxMs = Number.parseFloat(
  process.env.STRESS_SCHEDULE_LAG_P95_MAX_MS ?? "100",
);
const scheduleLagP99MaxMs = Number.parseFloat(
  process.env.STRESS_SCHEDULE_LAG_P99_MAX_MS ?? "250",
);
const missedStartMaxRatio = Number.parseFloat(
  process.env.STRESS_MISSED_START_MAX_RATIO ?? "0.001",
);
const backlogSlopeMaxPerSec = Number.parseFloat(
  process.env.STRESS_BACKLOG_SLOPE_MAX_PER_SEC ?? "0.1",
);
const candidateCleanTimeoutSec = Number.parseFloat(
  process.env.STRESS_CANDIDATE_CLEAN_TIMEOUT_SEC ?? "30",
);
const requireIdleNode =
  (process.env.STRESS_REQUIRE_IDLE_NODE ?? "true").trim().toLowerCase() !==
  "false";
const idleProbeSec = Number.parseFloat(
  process.env.STRESS_IDLE_PROBE_SEC ?? "2",
);
const requireMetricPresence =
  (process.env.STRESS_REQUIRE_METRIC_PRESENCE ?? "true")
    .trim()
    .toLowerCase() !== "false";
const findMaxBinaryIterations = Number.parseInt(
  process.env.STRESS_FIND_MAX_BINARY_ITERATIONS ?? "6",
  10,
);
const findMaxConfirmationSec = Number.parseFloat(
  process.env.STRESS_FIND_MAX_CONFIRMATION_SEC ?? String(measuredSec),
);
const findMaxRepeats = Number.parseInt(
  process.env.STRESS_FIND_MAX_REPEATS ?? "2",
  10,
);
const findMaxMaxCandidates = Number.parseInt(
  process.env.STRESS_FIND_MAX_MAX_CANDIDATES ?? "32",
  10,
);
const clientSelfCheckEnabled =
  (process.env.STRESS_CLIENT_SELF_CHECK ?? "true").trim().toLowerCase() !==
  "false";
const clientSelfCheckRequired =
  (process.env.STRESS_CLIENT_SELF_CHECK_REQUIRED ?? "false")
    .trim()
    .toLowerCase() !== "false";
const clientSelfCheckMultiplier = Number.parseFloat(
  process.env.STRESS_CLIENT_SELF_CHECK_MULTIPLIER ?? "2",
);
const clientSelfCheckMinRatio = Number.parseFloat(
  process.env.STRESS_CLIENT_SELF_CHECK_MIN_RATIO ?? "0.95",
);
const clientSelfCheckDurationSec = Number.parseFloat(
  process.env.STRESS_CLIENT_SELF_CHECK_DURATION_SEC ?? "2",
);
const reportPath =
  process.env.STRESS_REPORT_PATH ??
  path.join(
    pkgRoot,
    "benchmark-results",
    `l2-throughput-${new Date().toISOString().replace(/[:.]/g, "-")}.json`,
  );
const pgStatStatementsEnabled =
  (process.env.STRESS_PG_STAT_STATEMENTS ?? "false").trim().toLowerCase() ===
  "true";
const profileMode =
  (process.env.STRESS_PROFILE_MODE ?? "false").trim().toLowerCase() === "true";
const pyroscopeEnabled =
  (process.env.STRESS_PYROSCOPE ?? "false").trim().toLowerCase() === "true";

const execFileAsync = promisify(execFile);

if (!Number.isFinite(chainLength) || chainLength <= 0) {
  throw new Error("STRESS_CHAIN_LENGTH must be a positive integer");
}
if (!Number.isFinite(maxChains) || maxChains <= 0) {
  throw new Error("STRESS_MAX_CHAINS must be a positive integer");
}
if (!Number.isFinite(utxosPerWallet) || utxosPerWallet <= 0) {
  throw new Error("STRESS_UTXOS_PER_WALLET must be a positive integer");
}
if (!Number.isFinite(fanoutMaxOutputsPerTx) || fanoutMaxOutputsPerTx <= 1) {
  throw new Error(
    "STRESS_FANOUT_MAX_OUTPUTS_PER_TX must be an integer greater than 1",
  );
}
if (!Number.isFinite(fanoutStatusTimeoutMs) || fanoutStatusTimeoutMs <= 0) {
  throw new Error("STRESS_FANOUT_STATUS_TIMEOUT_MS must be a positive integer");
}
if (!Number.isFinite(metricsPollMs) || metricsPollMs <= 0) {
  throw new Error("STRESS_METRICS_POLL_MS must be a positive integer");
}
if (!["closed", "open", "ramp", "find-max"].includes(benchmarkMode)) {
  throw new Error("STRESS_MODE must be one of: closed, open, ramp, find-max");
}
if (!Number.isFinite(measuredSec) || measuredSec <= 0) {
  throw new Error("STRESS_MEASURED_SEC must be a positive number");
}
if (!Number.isFinite(warmupTxs) || warmupTxs < 0) {
  throw new Error("STRESS_WARMUP_TXS must be a non-negative integer");
}
if (!Number.isFinite(warmupSec) || warmupSec < 0) {
  throw new Error("STRESS_WARMUP_SEC must be a non-negative number");
}
if (!Number.isFinite(cooldownSec) || cooldownSec < 0) {
  throw new Error("STRESS_COOLDOWN_SEC must be a non-negative number");
}
if (!Number.isFinite(drainTimeoutSec) || drainTimeoutSec <= 0) {
  throw new Error("STRESS_DRAIN_TIMEOUT_SEC must be a positive number");
}
if (!Number.isFinite(statusSampleSize) || statusSampleSize < 0) {
  throw new Error("STRESS_STATUS_SAMPLE_SIZE must be a non-negative integer");
}
if (!Number.isFinite(submitConcurrency) || submitConcurrency <= 0) {
  throw new Error("STRESS_SUBMIT_CONCURRENCY must be a positive integer");
}
if (!Number.isFinite(httpConnections) || httpConnections <= 0) {
  throw new Error("STRESS_HTTP_CONNECTIONS must be a positive integer");
}
if (!Number.isFinite(httpPipelining) || httpPipelining <= 0) {
  throw new Error("STRESS_HTTP_PIPELINING must be a positive integer");
}
if (!Number.isFinite(httpTimeoutMs) || httpTimeoutMs <= 0) {
  throw new Error("STRESS_HTTP_TIMEOUT_MS must be a positive integer");
}
if (!Number.isFinite(measuredRetry503) || measuredRetry503 < 0) {
  throw new Error("STRESS_MEASURED_RETRY_503 must be a non-negative integer");
}
for (const [name, value] of [
  ["STRESS_OFFERED_RATE_MIN_RATIO", offeredRateMinRatio],
  ["STRESS_ACCEPTED_RATE_MIN_RATIO", acceptedRateMinRatio],
  ["STRESS_RAMP_MIN_ACCEPTED_RATIO", rampMinAcceptedRatio],
]) {
  if (!Number.isFinite(value) || value <= 0 || value > 1) {
    throw new Error(`${name} must be in the range (0, 1]`);
  }
}
for (const [name, value] of [
  ["STRESS_SCHEDULE_LAG_P95_MAX_MS", scheduleLagP95MaxMs],
  ["STRESS_SCHEDULE_LAG_P99_MAX_MS", scheduleLagP99MaxMs],
  ["STRESS_BACKLOG_SLOPE_MAX_PER_SEC", backlogSlopeMaxPerSec],
  ["STRESS_CANDIDATE_CLEAN_TIMEOUT_SEC", candidateCleanTimeoutSec],
  ["STRESS_IDLE_PROBE_SEC", idleProbeSec],
  ["STRESS_FIND_MAX_CONFIRMATION_SEC", findMaxConfirmationSec],
]) {
  if (!Number.isFinite(value) || value < 0) {
    throw new Error(`${name} must be a non-negative number`);
  }
}
if (!Number.isFinite(missedStartMaxRatio) || missedStartMaxRatio < 0) {
  throw new Error("STRESS_MISSED_START_MAX_RATIO must be non-negative");
}
for (const [name, value] of [
  ["STRESS_FIND_MAX_BINARY_ITERATIONS", findMaxBinaryIterations],
  ["STRESS_FIND_MAX_REPEATS", findMaxRepeats],
  ["STRESS_FIND_MAX_MAX_CANDIDATES", findMaxMaxCandidates],
]) {
  if (!Number.isFinite(value) || value < 1) {
    throw new Error(`${name} must be a positive integer`);
  }
}

/** @typedef {{ outref: string; value: string }} NodeUtxo */
/** @typedef {{ txHex: string; txIdHex: string }} PrebuiltTx */

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

class BenchmarkHttpClient {
  constructor({ connections, pipelining, timeoutMs }) {
    this.connections = connections;
    this.pipelining = pipelining;
    this.timeoutMs = timeoutMs;
    this.pools = new Map();
  }

  poolFor(url) {
    const parsed = new URL(url);
    let pool = this.pools.get(parsed.origin);
    if (pool === undefined) {
      pool = new Pool(parsed.origin, {
        connections: this.connections,
        pipelining: this.pipelining,
        headersTimeout: this.timeoutMs,
        bodyTimeout: this.timeoutMs,
      });
      this.pools.set(parsed.origin, pool);
    }
    return pool;
  }

  async request(url, options = {}) {
    const parsed = new URL(url);
    const pool = this.poolFor(url);
    const startedAt = performance.now();
    const response = await pool.request({
      method: options.method ?? "GET",
      path: `${parsed.pathname}${parsed.search}`,
      headers: options.headers,
      body: options.body,
      headersTimeout: this.timeoutMs,
      bodyTimeout: this.timeoutMs,
    });
    const text = await response.body.text();
    const latencyMs = performance.now() - startedAt;
    return {
      status: response.statusCode,
      ok: response.statusCode >= 200 && response.statusCode < 300,
      body: text,
      latencyMs,
      json() {
        return text.length === 0 ? {} : JSON.parse(text);
      },
    };
  }

  async close() {
    await Promise.all(Array.from(this.pools.values(), (pool) => pool.close()));
  }
}

const httpClient = new BenchmarkHttpClient({
  connections: httpConnections,
  pipelining: httpPipelining,
  timeoutMs: httpTimeoutMs,
});

/**
 * Encodes a value into hexadecimal CBOR text.
 */
const encodeCbor = (value) => Buffer.from(cborEncode(value));
/**
 * Computes a 32-byte Blake2b hash.
 */
const hash32 = (value) => Buffer.from(blake2b(value, { dkLen: HASH32_LEN }));

/**
 * Encodes a byte-list preimage used by the native transaction fixtures.
 */
const encodeByteListPreimage = (items) =>
  encodeCbor(items.map((item) => Buffer.from(item)));

/**
 * Encodes a transaction output reference into CBOR.
 */
const toOutRefCbor = (txId, outputIndex) =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(txId),
      BigInt(outputIndex),
    ).to_cbor_bytes(),
  );

/**
 * Parses environment configuration for the valid-stress workload.
 */
const parseEnv = (filename) => {
  const raw = fs.readFileSync(filename, "utf8");
  return dotenv.parse(raw);
};

/**
 * Builds wallet descriptors from environment configuration.
 */
const makeWalletsFromEnv = (env) => {
  const keys = [
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_B",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_C",
  ];

  return keys
    .map((key) => {
      const seed = env[key];
      if (!seed || seed.trim().length === 0) {
        return null;
      }
      const wallet = walletFromSeed(seed.trim(), { network: "Preprod" });
      return {
        key,
        seed: seed.trim(),
        address: wallet.address,
        signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
      };
    })
    .filter((wallet) => wallet !== null);
};

/**
 * Fetches raw Prometheus metrics text from the node.
 */
const fetchMetricsText = async () => {
  const resp = await httpClient.request(metricsEndpoint);
  if (!resp.ok) {
    throw new Error(`metrics endpoint returned ${resp.status}`);
  }
  return resp.body;
};

/**
 * Escapes a string for literal use in a regular expression.
 */
const escapeRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

/**
 * Extracts a Prometheus sample value from metrics text.
 */
const extractMetricValue = (text, names) => {
  for (const name of names) {
    const pattern = `^${escapeRegex(name)}(?:\\{[^}]*\\})?\\s+([0-9]+(?:\\.[0-9]+)?)$`;
    const m = text.match(new RegExp(pattern, "m"));
    if (m !== null) {
      return { value: Number(m[1]), name };
    }
  }
  return { value: 0, name: null };
};

const metricSpecs = {
  submit: ["tx_count_total", "tx_count"],
  accept: ["validation_accept_count_total", "validation_accept_count"],
  reject: ["validation_reject_count_total", "validation_reject_count"],
  mempoolTx: ["mempool_tx_count"],
  validationQueueDepth: ["validation_queue_depth", "tx_queue_size"],
  validationBatchSize: ["validation_batch_size"],
  validationPhaseALatencyMs: ["validation_phase_a_latency_ms"],
  validationPhaseBLatencyMs: ["validation_phase_b_latency_ms"],
  validationBatchDurationCount: ["validation_batch_duration_count"],
  validationBatchDurationSum: ["validation_batch_duration_sum"],
  validationPhaseADurationCount: ["validation_phase_a_duration_count"],
  validationPhaseADurationSum: ["validation_phase_a_duration_sum"],
  validationPhaseBDurationCount: ["validation_phase_b_duration_count"],
  validationPhaseBDurationSum: ["validation_phase_b_duration_sum"],
  submitHandlerLatencyCount: ["submit_handler_latency_count"],
  submitHandlerLatencySum: ["submit_handler_latency_sum"],
  submitQueueOfferFailure: [
    "submit_queue_offer_failure_count_total",
    "submit_queue_offer_failure_count",
  ],
  commitBlock: ["commit_block_count_total", "commit_block_count"],
  commitBlockTx: ["commit_block_tx_count_total", "commit_block_tx_count"],
  commitWorkerDurationCount: ["commit_worker_duration_count"],
  commitWorkerDurationSum: ["commit_worker_duration_sum"],
  mergeBlock: ["merge_block_count_total", "merge_block_count"],
  mergeFailure: ["merge_failure_count_total", "merge_failure_count"],
  mergeDurationCount: ["merge_duration_count"],
  mergeDurationSum: ["merge_duration_sum"],
  blocksInQueue: ["blocks_in_queue"],
  processedUnsubmittedTxs: ["processed_unsubmitted_txs_count"],
  processedUnsubmittedTxsSizeBytes: ["processed_unsubmitted_txs_size_bytes"],
  unconfirmedSubmittedBlockPending: ["unconfirmed_submitted_block_pending"],
  unconfirmedSubmittedBlockAgeMs: ["unconfirmed_submitted_block_age_ms"],
};

const requiredStageMetricKeys = [
  "submit",
  "accept",
  "reject",
  "validationQueueDepth",
];

const metricMissingKeys = (counters, keys = requiredStageMetricKeys) =>
  keys.filter((key) => counters.metricNames?.[key] === null);

/**
 * Extracts Prometheus histogram series for machine-readable report artifacts.
 */
const extractHistogram = (text, baseName) => {
  const escaped = escapeRegex(baseName);
  const count = extractMetricValue(text, [`${baseName}_count`]);
  const sum = extractMetricValue(text, [`${baseName}_sum`]);
  const buckets = [];
  const re = new RegExp(
    `^${escaped}_bucket\\{([^}]*)\\}\\s+([0-9]+(?:\\.[0-9]+)?)$`,
    "gm",
  );
  let match = re.exec(text);
  while (match !== null) {
    const labels = match[1];
    const le = labels
      .split(",")
      .map((part) => part.trim())
      .find((part) => part.startsWith("le="));
    buckets.push({
      le: le === undefined ? null : le.slice(3).replace(/^"|"$/g, ""),
      value: Number(match[2]),
    });
    match = re.exec(text);
  }
  return {
    count: count.name === null ? null : count.value,
    sum: sum.name === null ? null : sum.value,
    buckets,
  };
};

/**
 * Fetches and parses the counters used by the workload monitor.
 */
const readCounters = async () => {
  const text = await fetchMetricsText();
  const counters = { metricNames: {}, missingMetrics: [], histograms: {} };
  for (const [key, names] of Object.entries(metricSpecs)) {
    const extracted = extractMetricValue(text, names);
    counters[key] = extracted.value;
    counters.metricNames[key] = extracted.name;
    if (extracted.name === null) {
      counters.missingMetrics.push({ key, names });
    }
  }
  counters.histograms.submitHandlerLatency = extractHistogram(
    text,
    "submit_handler_latency",
  );
  counters.histograms.validationBatchDuration = extractHistogram(
    text,
    "validation_batch_duration",
  );
  counters.histograms.validationPhaseADuration = extractHistogram(
    text,
    "validation_phase_a_duration",
  );
  counters.histograms.validationPhaseBDuration = extractHistogram(
    text,
    "validation_phase_b_duration",
  );
  counters.histograms.commitWorkerDuration = extractHistogram(
    text,
    "commit_worker_duration",
  );
  counters.histograms.mergeDuration = extractHistogram(text, "merge_duration");
  return counters;
};

/**
 * Fetches spendable UTxOs for a wallet address.
 */
const fetchUtxos = async (address) => {
  const resp = await httpClient.request(
    `${submitEndpoint}/utxos?address=${encodeURIComponent(address)}`,
  );
  if (!resp.ok) {
    throw new Error(`utxos endpoint returned ${resp.status} for ${address}`);
  }
  const body = resp.json();
  if (!Array.isArray(body.utxos)) {
    return [];
  }
  return /** @type {NodeUtxo[]} */ (body.utxos);
};

/**
 * Fetches transaction status information from the node.
 */
const fetchTxStatus = async (txIdHex) => {
  let attempt = 0;
  while (attempt <= txStatusRetries) {
    try {
      const resp = await httpClient.request(
        `${submitEndpoint}/tx-status?tx_hash=${encodeURIComponent(txIdHex)}`,
      );
      if (!resp.ok && resp.status !== 404) {
        if (
          (resp.status === 429 || resp.status === 503) &&
          attempt < txStatusRetries
        ) {
          attempt += 1;
          await sleep(txStatusRetryDelayMs);
          continue;
        }
        throw new Error(
          `tx-status endpoint returned ${resp.status} for ${txIdHex}`,
        );
      }
      let body = {};
      try {
        body = resp.json();
      } catch {
        // Keep fallback behavior below.
      }
      if (typeof body?.status === "string") {
        return body.status;
      }
      return resp.status === 404 ? "not_found" : "unknown";
    } catch (error) {
      if (attempt >= txStatusRetries) {
        throw error;
      }
      attempt += 1;
      await sleep(txStatusRetryDelayMs);
    }
  }
  return "unknown";
};

/**
 * Decodes a lovelace quantity from a UTxO payload.
 */
const decodeCoin = (outputHex) => {
  const output = CML.TransactionOutput.from_cbor_bytes(
    Buffer.from(outputHex, "hex"),
  );
  return output.amount().coin();
};

/**
 * Returns true when a transaction output carries non-ADA assets.
 */
const outputHasMultiAssets = (outputCbor) =>
  CML.TransactionOutput.from_cbor_bytes(outputCbor).amount().has_multiassets();

/**
 * Returns a transaction output equivalent to `outputCbor` with a reduced coin.
 */
const withOutputCoin = (outputCbor, coin) => {
  const output = CML.TransactionOutput.from_cbor_bytes(outputCbor);
  const currentAmount = output.amount();
  const nextAmount = currentAmount.has_multiassets()
    ? CML.Value.new(coin, currentAmount.multi_asset())
    : CML.Value.from_coin(coin);
  output.set_amount(nextAmount);
  return Buffer.from(output.to_cbor_bytes());
};

/**
 * Returns a transaction output with the same address as `templateOutputCbor`
 * and a lovelace-only value.
 */
const lovelaceOnlyOutputForTemplate = (templateOutputCbor, coin) => {
  const template = CML.TransactionOutput.from_cbor_bytes(templateOutputCbor);
  return Buffer.from(
    CML.TransactionOutput.new(
      template.address(),
      CML.Value.from_coin(coin),
    ).to_cbor_bytes(),
  );
};

/**
 * Splits a lovelace-only native UTxO into many same-address outputs for
 * high-concurrency benchmark setup.
 */
const buildNativeSignedSplitWithFee = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  outputCount,
  fee,
}) => {
  const input = CML.TransactionOutput.from_cbor_bytes(inputOutputCbor);
  if (input.amount().has_multiassets()) {
    throw new Error("fanout setup only supports lovelace-only source UTxOs");
  }
  const inputCoin = input.amount().coin();
  if (inputCoin <= fee) {
    throw new Error(
      `fanout source coin ${inputCoin.toString()} cannot cover fee ${fee.toString()}`,
    );
  }
  const available = inputCoin - fee;
  const outputCountBig = BigInt(outputCount);
  const baseCoin = available / outputCountBig;
  const remainder = available % outputCountBig;
  if (baseCoin <= 0n) {
    throw new Error(
      `fanout source coin ${inputCoin.toString()} too small for ${outputCount} outputs`,
    );
  }

  const outputs = Array.from({ length: outputCount }, (_, index) =>
    lovelaceOnlyOutputForTemplate(
      inputOutputCbor,
      baseCoin + (BigInt(index) < remainder ? 1n : 0n),
    ),
  );

  const spendInputsPreimageCbor = encodeByteListPreimage([spendOutRefCbor]);
  const referenceInputsPreimageCbor = EMPTY_CBOR_LIST;
  const outputsPreimageCbor = encodeByteListPreimage(outputs);
  const requiredObserversPreimageCbor = EMPTY_CBOR_LIST;
  const requiredSignersPreimageCbor = encodeByteListPreimage([
    Buffer.from(signer.to_public().hash().to_raw_bytes()),
  ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const scriptIntegrityHash = hash32(EMPTY_CBOR_NULL);
  const auxiliaryDataHash = hash32(EMPTY_CBOR_NULL);

  const bodyCompact = [
    hash32(spendInputsPreimageCbor),
    hash32(referenceInputsPreimageCbor),
    hash32(outputsPreimageCbor),
    fee,
    MIDGARD_POSIX_TIME_NONE,
    MIDGARD_POSIX_TIME_NONE,
    hash32(requiredObserversPreimageCbor),
    hash32(requiredSignersPreimageCbor),
    hash32(mintPreimageCbor),
    scriptIntegrityHash,
    auxiliaryDataHash,
    MIDGARD_NETWORK_ID_PREPROD,
  ];

  const bodyHash = hash32(encodeCbor(bodyCompact));
  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(bodyHash),
    signer,
  );

  const addrTxWitsPreimageCbor = encodeByteListPreimage([
    Buffer.from(witness.to_cbor_bytes()),
  ]);
  const scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const redeemerTxWitsPreimageCbor = EMPTY_CBOR_LIST;

  const witnessCompact = [
    hash32(addrTxWitsPreimageCbor),
    hash32(scriptTxWitsPreimageCbor),
    hash32(redeemerTxWitsPreimageCbor),
  ];

  const compact = [
    MIDGARD_NATIVE_TX_VERSION,
    bodyHash,
    hash32(encodeCbor(witnessCompact)),
    TX_IS_VALID_CODE,
  ];

  const bodyFull = [
    bodyCompact[0],
    spendInputsPreimageCbor,
    bodyCompact[1],
    referenceInputsPreimageCbor,
    bodyCompact[2],
    outputsPreimageCbor,
    bodyCompact[3],
    bodyCompact[4],
    bodyCompact[5],
    bodyCompact[6],
    requiredObserversPreimageCbor,
    bodyCompact[7],
    requiredSignersPreimageCbor,
    bodyCompact[8],
    mintPreimageCbor,
    bodyCompact[9],
    bodyCompact[10],
    bodyCompact[11],
  ];

  const witnessFull = [
    witnessCompact[0],
    addrTxWitsPreimageCbor,
    witnessCompact[1],
    scriptTxWitsPreimageCbor,
    witnessCompact[2],
    redeemerTxWitsPreimageCbor,
  ];

  const txCbor = encodeCbor([
    MIDGARD_NATIVE_TX_VERSION,
    compact,
    bodyFull,
    witnessFull,
  ]);

  return {
    txId: bodyHash,
    txHex: txCbor.toString("hex"),
    outputs: outputs.map((outputCbor, outputIndex) => ({
      outputCbor,
      spendOutRefCbor: toOutRefCbor(bodyHash, outputIndex),
      outRefHex: toOutRefCbor(bodyHash, outputIndex).toString("hex"),
      outputIndex,
    })),
    fee,
  };
};

/**
 * Builds a native split transaction with a converged linear min fee.
 */
const buildNativeSignedSplit = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  outputCount,
  minFeeA,
  minFeeB,
}) => {
  let fee = minFeeB;
  for (let iteration = 0; iteration < 12; iteration += 1) {
    const tx = buildNativeSignedSplitWithFee({
      spendOutRefCbor,
      inputOutputCbor,
      signer,
      outputCount,
      fee,
    });
    const requiredFee =
      minFeeA * BigInt(Buffer.from(tx.txHex, "hex").length) + minFeeB;
    if (requiredFee === fee) {
      return tx;
    }
    fee = requiredFee;
  }
  throw new Error("failed to converge native fanout transaction min fee");
};

/**
 * Builds and signs a native one-to-one transfer transaction for a fixed fee.
 */
const buildNativeSignedOneToOneWithFee = ({
  spendOutRefCbor,
  outputCbor,
  signer,
  fee,
}) => {
  const spendInputsPreimageCbor = encodeByteListPreimage([spendOutRefCbor]);
  const referenceInputsPreimageCbor = EMPTY_CBOR_LIST;
  const outputsPreimageCbor = encodeByteListPreimage([outputCbor]);
  const requiredObserversPreimageCbor = EMPTY_CBOR_LIST;
  const requiredSignersPreimageCbor = encodeByteListPreimage([
    Buffer.from(signer.to_public().hash().to_raw_bytes()),
  ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const scriptIntegrityHash = hash32(EMPTY_CBOR_NULL);
  const auxiliaryDataHash = hash32(EMPTY_CBOR_NULL);

  const bodyCompact = [
    hash32(spendInputsPreimageCbor),
    hash32(referenceInputsPreimageCbor),
    hash32(outputsPreimageCbor),
    fee,
    MIDGARD_POSIX_TIME_NONE,
    MIDGARD_POSIX_TIME_NONE,
    hash32(requiredObserversPreimageCbor),
    hash32(requiredSignersPreimageCbor),
    hash32(mintPreimageCbor),
    scriptIntegrityHash,
    auxiliaryDataHash,
    MIDGARD_NETWORK_ID_PREPROD,
  ];

  const bodyHash = hash32(encodeCbor(bodyCompact));
  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(bodyHash),
    signer,
  );

  const addrTxWitsPreimageCbor = encodeByteListPreimage([
    Buffer.from(witness.to_cbor_bytes()),
  ]);
  const scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const redeemerTxWitsPreimageCbor = EMPTY_CBOR_LIST;

  const witnessCompact = [
    hash32(addrTxWitsPreimageCbor),
    hash32(scriptTxWitsPreimageCbor),
    hash32(redeemerTxWitsPreimageCbor),
  ];

  const compact = [
    MIDGARD_NATIVE_TX_VERSION,
    bodyHash,
    hash32(encodeCbor(witnessCompact)),
    TX_IS_VALID_CODE,
  ];

  const bodyFull = [
    bodyCompact[0],
    spendInputsPreimageCbor,
    bodyCompact[1],
    referenceInputsPreimageCbor,
    bodyCompact[2],
    outputsPreimageCbor,
    bodyCompact[3],
    bodyCompact[4],
    bodyCompact[5],
    bodyCompact[6],
    requiredObserversPreimageCbor,
    bodyCompact[7],
    requiredSignersPreimageCbor,
    bodyCompact[8],
    mintPreimageCbor,
    bodyCompact[9],
    bodyCompact[10],
    bodyCompact[11],
  ];

  const witnessFull = [
    witnessCompact[0],
    addrTxWitsPreimageCbor,
    witnessCompact[1],
    scriptTxWitsPreimageCbor,
    witnessCompact[2],
    redeemerTxWitsPreimageCbor,
  ];

  const txCbor = encodeCbor([
    MIDGARD_NATIVE_TX_VERSION,
    compact,
    bodyFull,
    witnessFull,
  ]);

  const txId = bodyHash;

  return {
    txId,
    txHex: txCbor.toString("hex"),
    nextOutRef: toOutRefCbor(txId, 0),
    outputCbor,
    fee,
  };
};

/**
 * Builds and signs a native one-to-one transfer transaction with a converged
 * linear min fee.
 */
const buildNativeSignedOneToOne = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  minFeeA,
  minFeeB,
}) => {
  let fee = minFeeB;
  for (let iteration = 0; iteration < 12; iteration += 1) {
    const inputCoin = CML.TransactionOutput.from_cbor_bytes(inputOutputCbor)
      .amount()
      .coin();
    if (inputCoin <= fee) {
      throw new Error(
        `input coin ${inputCoin.toString()} cannot cover fee ${fee.toString()}`,
      );
    }
    const outputCbor = withOutputCoin(inputOutputCbor, inputCoin - fee);
    const tx = buildNativeSignedOneToOneWithFee({
      spendOutRefCbor,
      outputCbor,
      signer,
      fee,
    });
    const requiredFee =
      minFeeA * BigInt(Buffer.from(tx.txHex, "hex").length) + minFeeB;
    if (requiredFee === fee) {
      return tx;
    }
    fee = requiredFee;
  }
  throw new Error("failed to converge native stress transaction min fee");
};

/**
 * Prebuilds a dependent transaction chain for the stress workload.
 */
const prebuildChain = (chain, length, feeConfig) => {
  /** @type {PrebuiltTx[]} */
  const txs = [];
  let currentOutRef = chain.spendOutRefCbor;
  let currentOutputCbor = chain.outputCbor;
  for (let i = 0; i < length; i++) {
    const tx = buildNativeSignedOneToOne({
      spendOutRefCbor: currentOutRef,
      signer: chain.signer,
      inputOutputCbor: currentOutputCbor,
      minFeeA: feeConfig.minFeeA,
      minFeeB: feeConfig.minFeeB,
    });
    txs.push({
      txHex: tx.txHex,
      txIdHex: tx.txId.toString("hex"),
    });
    currentOutRef = tx.nextOutRef;
    currentOutputCbor = tx.outputCbor;
  }
  return txs;
};

/**
 * Submits a CBOR transaction hex payload to the node.
 */
const submitTxHex = async (txHex, { retryLimit = retry503 } = {}) => {
  let attempt = 0;
  const attempts = [];
  while (attempt <= retryLimit) {
    const resp = await httpClient.request(`${submitEndpoint}/submit`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ tx_cbor: txHex }),
    });
    attempts.push({
      status: resp.status,
      ok: resp.ok,
      latencyMs: resp.latencyMs,
    });

    if (resp.ok) {
      return {
        ok: true,
        status: resp.status,
        latencyMs: resp.latencyMs,
        attempts,
        physicalAttemptCount: attempts.length,
      };
    }

    const body = resp.body;
    if ((resp.status === 503 || resp.status === 429) && attempt < retryLimit) {
      attempt += 1;
      await sleep(retryDelayMs);
      continue;
    }

    return {
      ok: false,
      status: resp.status,
      body,
      latencyMs: resp.latencyMs,
      attempts,
      physicalAttemptCount: attempts.length,
    };
  }

  return {
    ok: false,
    status: 0,
    body: "retry loop exhausted",
    latencyMs: 0,
    attempts,
    physicalAttemptCount: attempts.length,
  };
};

/**
 * Waits until a transaction has passed validation or failed.
 */
const waitForAcceptedTx = async (txIdHex, label) => {
  const startedAt = Date.now();
  while (Date.now() - startedAt <= fanoutStatusTimeoutMs) {
    const status = await fetchTxStatus(txIdHex);
    if (
      status === "accepted" ||
      status === "pending_commit" ||
      status === "awaiting_local_recovery" ||
      status === "committed"
    ) {
      return status;
    }
    if (status === "rejected") {
      throw new Error(`${label} ${txIdHex} was rejected`);
    }
    await sleep(txStatusRetryDelayMs);
  }
  throw new Error(
    `${label} ${txIdHex} did not reach accepted status within ${fanoutStatusTimeoutMs}ms`,
  );
};

/**
 * Estimates a safe leaf value for generated fanout UTxOs.
 */
const defaultFanoutOutputLovelace = (minFeeA, minFeeB) => {
  const estimatedTxBytes = 900n;
  const estimatedFee = minFeeA * estimatedTxBytes + minFeeB;
  return estimatedFee * BigInt(chainLength + 2) + 1_000_000n;
};

/**
 * Expands available source UTxOs into enough independent benchmark chains.
 */
const ensureFanoutCandidates = async ({ candidates, minFeeA, minFeeB }) => {
  if (!fanoutEnabled || candidates.length >= maxChains) {
    return {
      candidates,
      fanoutTxCount: 0,
      fanoutOutputCount: 0,
    };
  }

  const leafLovelace =
    fanoutOutputLovelace ?? defaultFanoutOutputLovelace(minFeeA, minFeeB);
  const result = [...candidates];
  let fanoutTxCount = 0;
  let fanoutOutputCount = 0;

  result.sort((a, b) =>
    a.lovelace === b.lovelace ? 0 : a.lovelace > b.lovelace ? -1 : 1,
  );

  for (
    let sourceIndex = 0;
    result.length < maxChains && sourceIndex < result.length;
    sourceIndex += 1
  ) {
    const source = result[sourceIndex];
    if (outputHasMultiAssets(source.outputCbor)) {
      continue;
    }

    const remainingAfterSpendingSource = result.length - 1;
    const neededOutputs = maxChains - remainingAfterSpendingSource;
    const affordableOutputs = Number(source.lovelace / leafLovelace);
    const outputCount = Math.min(
      fanoutMaxOutputsPerTx,
      neededOutputs,
      affordableOutputs,
    );
    if (outputCount <= 1) {
      continue;
    }

    const split = buildNativeSignedSplit({
      spendOutRefCbor: source.spendOutRefCbor,
      inputOutputCbor: source.outputCbor,
      signer: source.signer,
      outputCount,
      minFeeA,
      minFeeB,
    });
    const txIdHex = split.txId.toString("hex");
    const submittedResult = await submitTxHex(split.txHex);
    if (!submittedResult.ok) {
      throw new Error(
        `fanout submit failed for ${source.outRefHex}: status=${submittedResult.status} body=${submittedResult.body ?? ""}`,
      );
    }
    const status = await waitForAcceptedTx(txIdHex, "fanout tx");
    console.log(
      `fanout tx accepted: tx=${txIdHex} status=${status} outputs=${split.outputs.length} source=${source.outRefHex}`,
    );

    result.splice(
      sourceIndex,
      1,
      ...split.outputs.map((output) => ({
        walletKey: source.walletKey,
        address: source.address,
        signer: source.signer,
        spendOutRefCbor: output.spendOutRefCbor,
        outputCbor: output.outputCbor,
        lovelace: CML.TransactionOutput.from_cbor_bytes(output.outputCbor)
          .amount()
          .coin(),
        outRefHex: output.outRefHex,
      })),
    );
    fanoutTxCount += 1;
    fanoutOutputCount += split.outputs.length;
    sourceIndex += split.outputs.length - 1;
  }

  return {
    candidates: result,
    fanoutTxCount,
    fanoutOutputCount,
  };
};

const makeChainCursors = (chains) =>
  chains.map((chain, chainIndex) => ({
    chain,
    chainIndex,
    nextIndex: 0,
    stopped: false,
  }));

const remainingTxCount = (cursors) =>
  cursors.reduce(
    (acc, cursor) =>
      acc + (cursor.stopped ? 0 : cursor.chain.txs.length - cursor.nextIndex),
    0,
  );

const takeNextTx = (cursor) => {
  if (cursor.stopped || cursor.nextIndex >= cursor.chain.txs.length) {
    return null;
  }
  const tx = cursor.chain.txs[cursor.nextIndex];
  const txIndex = cursor.nextIndex;
  cursor.nextIndex += 1;
  return {
    ...tx,
    chainIndex: cursor.chainIndex,
    txIndex,
  };
};

const createStageStats = ({ name, mode, targetRateTps = null }) => ({
  name,
  mode,
  targetRateTps,
  startedAtMs: Date.now(),
  endedAtMs: null,
  counterStart: null,
  counterEnd: null,
  drainCounters: null,
  drain: null,
  logicalSubmitAttempts: 0,
  physicalSubmitAttempts: 0,
  submitted: 0,
  submitErrors: 0,
  submitStatusCounts: {},
  physicalSubmitStatusCounts: {},
  queueFullResponses: 0,
  firstErrors: [],
  submitLatencyMs: [],
  submitAttemptLatencyMs: [],
  statusLatencyMs: [],
  scheduleLagMs: [],
  scheduledStarts: 0,
  sentStarts: 0,
  missedStarts: 0,
  inFlightHighWater: 0,
  bytesSent: 0,
  statusSampleTxIds: [],
  submittedAtByTxId: new Map(),
});

const recordSubmitResult = (stage, tx, result) => {
  stage.logicalSubmitAttempts += 1;
  stage.physicalSubmitAttempts += result.physicalAttemptCount ?? 1;
  stage.bytesSent += Buffer.byteLength(tx.txHex, "hex");
  const key = String(result.status);
  stage.submitStatusCounts[key] = (stage.submitStatusCounts[key] ?? 0) + 1;
  stage.submitLatencyMs.push(result.latencyMs ?? 0);
  for (const attempt of result.attempts ?? []) {
    const attemptKey = String(attempt.status);
    stage.physicalSubmitStatusCounts[attemptKey] =
      (stage.physicalSubmitStatusCounts[attemptKey] ?? 0) + 1;
    stage.submitAttemptLatencyMs.push(attempt.latencyMs ?? 0);
    if (attempt.status === 429 || attempt.status === 503) {
      stage.queueFullResponses += 1;
    }
  }
  if (result.ok) {
    stage.submitted += 1;
    stage.submittedAtByTxId.set(tx.txIdHex, Date.now());
    if (stage.statusSampleTxIds.length < statusSampleSize) {
      stage.statusSampleTxIds.push(tx.txIdHex);
    }
    return;
  }

  stage.submitErrors += 1;
  if (stage.firstErrors.length < 10) {
    stage.firstErrors.push(
      `chain=${tx.chainIndex} index=${tx.txIndex} status=${result.status} body=${result.body ?? ""}`,
    );
  }
};

const submitOne = async (
  cursor,
  stage,
  { scheduledAtPerfMs = null, retryLimit = retry503 } = {},
) => {
  const tx = takeNextTx(cursor);
  if (tx === null) {
    return false;
  }
  if (scheduledAtPerfMs !== null) {
    stage.scheduleLagMs.push(performance.now() - scheduledAtPerfMs);
  }
  stage.sentStarts += 1;
  const submittedResult = await submitTxHex(tx.txHex, { retryLimit });
  recordSubmitResult(stage, tx, submittedResult);
  if (!submittedResult.ok) {
    cursor.stopped = true;
  }
  return true;
};

const runClosedLoopStage = async ({
  name,
  cursors,
  durationSec,
  maxTxs = Number.POSITIVE_INFINITY,
  retryLimit = retry503,
}) => {
  const stage = createStageStats({ name, mode: "closed" });
  stage.counterStart = await readCounters();
  stage.startedAtMs = Date.now();
  const deadlineMs = stage.startedAtMs + durationSec * 1000;
  let started = 0;

  await Promise.all(
    cursors.map(async (cursor) => {
      while (Date.now() < deadlineMs && started < maxTxs) {
        if (remainingTxCount([cursor]) <= 0) {
          break;
        }
        started += 1;
        const hadTx = await submitOne(cursor, stage, { retryLimit });
        if (!hadTx || cursor.stopped) {
          break;
        }
      }
    }),
  );

  stage.endedAtMs = Date.now();
  stage.counterEnd = await readCounters();
  return stage;
};

const waitForAnyInFlight = async (inFlight) => {
  if (inFlight.size === 0) {
    return;
  }
  await Promise.race(inFlight);
};

const findAvailableCursor = (cursors, busy, startIndex) => {
  for (let offset = 0; offset < cursors.length; offset += 1) {
    const index = (startIndex + offset) % cursors.length;
    const cursor = cursors[index];
    if (
      !busy.has(cursor.chainIndex) &&
      !cursor.stopped &&
      cursor.nextIndex < cursor.chain.txs.length
    ) {
      return { cursor, nextIndex: (index + 1) % cursors.length };
    }
  }
  return null;
};

const runOpenLoopStage = async ({
  name,
  cursors,
  rateTps,
  durationSec,
  maxTxs = Number.POSITIVE_INFINITY,
  retryLimit = retry503,
}) => {
  if (!Number.isFinite(rateTps) || rateTps <= 0) {
    throw new Error("open-loop stages require a positive target rate");
  }
  const stage = createStageStats({
    name,
    mode: "open",
    targetRateTps: rateTps,
  });
  stage.counterStart = await readCounters();
  stage.startedAtMs = Date.now();
  const startedAt = performance.now();
  const deadline = startedAt + durationSec * 1000;
  const intervalMs = 1000 / rateTps;
  const inFlight = new Set();
  const busy = new Set();
  let nextCursorIndex = 0;
  let scheduled = 0;

  while (
    performance.now() < deadline &&
    scheduled < maxTxs &&
    remainingTxCount(cursors) > 0
  ) {
    while (inFlight.size >= submitConcurrency) {
      await waitForAnyInFlight(inFlight);
    }

    const selected = findAvailableCursor(cursors, busy, nextCursorIndex);
    if (selected === null) {
      if (inFlight.size === 0) {
        break;
      }
      await waitForAnyInFlight(inFlight);
      continue;
    }
    const { cursor } = selected;
    nextCursorIndex = selected.nextIndex;

    let dueAt = startedAt + scheduled * intervalMs;
    const waitMs = dueAt - performance.now();
    if (waitMs > 0) {
      await sleep(waitMs);
    } else if (-waitMs >= intervalMs) {
      const missed = Math.floor(-waitMs / intervalMs);
      stage.missedStarts += missed;
      scheduled += missed;
      dueAt = startedAt + scheduled * intervalMs;
    }

    stage.scheduledStarts += 1;
    busy.add(cursor.chainIndex);
    const promise = submitOne(cursor, stage, {
      scheduledAtPerfMs: dueAt,
      retryLimit,
    }).finally(() => {
      busy.delete(cursor.chainIndex);
      inFlight.delete(promise);
    });
    inFlight.add(promise);
    stage.inFlightHighWater = Math.max(stage.inFlightHighWater, inFlight.size);
    scheduled += 1;
  }

  await Promise.all(inFlight);
  stage.endedAtMs = Date.now();
  stage.counterEnd = await readCounters();
  return stage;
};

const waitForStageDrain = async (stage) => {
  const startedAt = Date.now();
  let lastCounters = stage.counterEnd ?? (await readCounters());
  while (Date.now() - startedAt <= drainTimeoutSec * 1000) {
    const acceptedDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "accept",
    );
    const rejectedDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "reject",
    );
    const commitTxDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "commitBlockTx",
    );
    const commitBlockDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "commitBlock",
    );
    const mergeBlockDelta = counterDelta(
      stage.counterStart,
      lastCounters,
      "mergeBlock",
    );
    const settled = isDrainComplete({
      submitted: stage.submitted,
      acceptedDelta,
      rejectedDelta,
    });
    const committed = !waitForCommit || commitTxDelta >= acceptedDelta;
    const merged =
      !waitForMerge || mergeBlockDelta >= Math.max(1, commitBlockDelta);

    if (settled && committed && merged) {
      stage.drainCounters = lastCounters;
      stage.drain = {
        completed: true,
        elapsedMs: Date.now() - startedAt,
        acceptedDelta,
        rejectedDelta,
        commitTxDelta,
        commitBlockDelta,
        mergeBlockDelta,
      };
      return stage.drain;
    }

    await sleep(metricsPollMs);
    lastCounters = await readCounters();
  }

  const acceptedDelta = counterDelta(
    stage.counterStart,
    lastCounters,
    "accept",
  );
  const rejectedDelta = counterDelta(
    stage.counterStart,
    lastCounters,
    "reject",
  );
  stage.drainCounters = lastCounters;
  stage.drain = {
    completed: false,
    elapsedMs: Date.now() - startedAt,
    acceptedDelta,
    rejectedDelta,
    commitTxDelta: counterDelta(
      stage.counterStart,
      lastCounters,
      "commitBlockTx",
    ),
    commitBlockDelta: counterDelta(
      stage.counterStart,
      lastCounters,
      "commitBlock",
    ),
    mergeBlockDelta: counterDelta(
      stage.counterStart,
      lastCounters,
      "mergeBlock",
    ),
  };
  return stage.drain;
};

const collectStatusLatencies = async (stage) => {
  if (stage.statusSampleTxIds.length === 0) {
    return;
  }
  await Promise.all(
    stage.statusSampleTxIds.map(async (txIdHex) => {
      const submittedAt = stage.submittedAtByTxId.get(txIdHex);
      if (submittedAt === undefined) {
        return;
      }
      const startedAt = Date.now();
      while (Date.now() - startedAt <= drainTimeoutSec * 1000) {
        const status = await fetchTxStatus(txIdHex);
        if (terminalStatuses.has(status)) {
          if (acceptedStatuses.has(status)) {
            stage.statusLatencyMs.push(Date.now() - submittedAt);
          }
          return;
        }
        await sleep(txStatusRetryDelayMs);
      }
    }),
  );
};

const startCounterMonitor = async (phaseNameRef) => {
  const samples = [];
  let stopped = false;
  const firstCounters = await readCounters();
  let prev = firstCounters;
  let prevTs = Date.now();
  samples.push({
    timestampMs: prevTs,
    phase: phaseNameRef.current,
    counters: firstCounters,
  });

  const loop = (async () => {
    while (!stopped) {
      await sleep(metricsPollMs);
      const now = await readCounters();
      const nowTs = Date.now();
      const dt = (nowTs - prevTs) / 1000;
      samples.push({
        timestampMs: nowTs,
        phase: phaseNameRef.current,
        counters: now,
      });
      if (dt > 0) {
        const submitRate = (now.submit - prev.submit) / dt;
        const acceptRate = (now.accept - prev.accept) / dt;
        const rejectRate = (now.reject - prev.reject) / dt;
        const commitTxRate = (now.commitBlockTx - prev.commitBlockTx) / dt;
        const mergeRate = (now.mergeBlock - prev.mergeBlock) / dt;
        console.log(
          `phase=${phaseNameRef.current} rate_submit=${submitRate.toFixed(2)} rate_accept=${acceptRate.toFixed(2)} rate_reject=${rejectRate.toFixed(2)} rate_commit_tx=${commitTxRate.toFixed(2)} rate_merge=${mergeRate.toFixed(2)} queue=${now.validationQueueDepth} mempool=${now.mempoolTx}`,
        );
      }
      prev = now;
      prevTs = nowTs;
    }
  })();

  return {
    samples,
    async stop() {
      stopped = true;
      await loop;
    },
  };
};

const createRuntimeSampler = () => {
  const eventLoopDelay = monitorEventLoopDelay({ resolution: 20 });
  const gcDurations = [];
  let observer = null;
  try {
    observer = new PerformanceObserver((list) => {
      for (const entry of list.getEntries()) {
        gcDurations.push(entry.duration);
      }
    });
    observer.observe({ entryTypes: ["gc"] });
  } catch {
    observer = null;
  }
  eventLoopDelay.enable();
  const startElu = performance.eventLoopUtilization();
  const startCpu = process.cpuUsage();
  const startMemory = process.memoryUsage();
  const startedAtMs = Date.now();

  return {
    stop() {
      const endElu = performance.eventLoopUtilization(startElu);
      const endCpu = process.cpuUsage(startCpu);
      const endMemory = process.memoryUsage();
      eventLoopDelay.disable();
      if (observer !== null) {
        observer.disconnect();
      }
      return {
        startedAtMs,
        endedAtMs: Date.now(),
        eventLoopUtilization: endElu.utilization,
        eventLoopDelayMs: {
          min: eventLoopDelay.min / 1e6,
          mean: eventLoopDelay.mean / 1e6,
          p50: eventLoopDelay.percentile(50) / 1e6,
          p95: eventLoopDelay.percentile(95) / 1e6,
          p99: eventLoopDelay.percentile(99) / 1e6,
          max: eventLoopDelay.max / 1e6,
        },
        cpuUsageMicros: endCpu,
        memoryStart: startMemory,
        memoryEnd: endMemory,
        gcPauseMs: summarizeLatency(gcDurations),
      };
    },
  };
};

const maybeStartPyroscope = async () => {
  if (!pyroscopeEnabled) {
    return { enabled: false };
  }
  try {
    const pyroscope = await import("@pyroscope/nodejs");
    const client = pyroscope.default ?? pyroscope;
    client.init({
      serverAddress:
        process.env.PYROSCOPE_SERVER_ADDRESS ?? "http://pyroscope:4040",
      appName:
        process.env.PYROSCOPE_APPLICATION_NAME ?? "midgard-node-benchmark",
      tags: {
        benchmark: "l2-throughput",
        mode: benchmarkMode,
      },
    });
    client.start();
    return { enabled: true, mode: "pyroscope" };
  } catch (error) {
    return {
      enabled: false,
      error: `Pyroscope requested but @pyroscope/nodejs could not be started: ${error instanceof Error ? error.message : String(error)}`,
    };
  }
};

const readPgStatStatements = async (env, label) => {
  if (!pgStatStatementsEnabled) {
    return { enabled: false };
  }
  const host = process.env.POSTGRES_HOST ?? env.POSTGRES_HOST ?? "localhost";
  const port = process.env.POSTGRES_PORT ?? env.POSTGRES_PORT ?? "5432";
  const user = process.env.POSTGRES_USER ?? env.POSTGRES_USER ?? "postgres";
  const database = process.env.POSTGRES_DB ?? env.POSTGRES_DB ?? "midgard";
  const password =
    process.env.POSTGRES_PASSWORD ?? env.POSTGRES_PASSWORD ?? "postgres";
  const query = `
    select queryid,calls,total_exec_time,mean_exec_time,rows,
           left(regexp_replace(query, '\\s+', ' ', 'g'), 240) as query
    from pg_stat_statements
    order by total_exec_time desc
    limit 20
  `;
  try {
    const { stdout } = await execFileAsync(
      "psql",
      [
        "-h",
        host,
        "-p",
        String(port),
        "-U",
        user,
        "-d",
        database,
        "-At",
        "-F",
        "\t",
        "-c",
        query,
      ],
      {
        env: {
          ...process.env,
          PGPASSWORD: password,
        },
        timeout: 10_000,
      },
    );
    return {
      enabled: true,
      label,
      rows: stdout
        .trim()
        .split("\n")
        .filter((line) => line.length > 0)
        .map((line) => {
          const [queryid, calls, totalExecTime, meanExecTime, rows, queryText] =
            line.split("\t");
          return {
            queryid,
            calls: Number(calls),
            totalExecTimeMs: Number(totalExecTime),
            meanExecTimeMs: Number(meanExecTime),
            rows: Number(rows),
            query: queryText,
          };
        }),
    };
  } catch (error) {
    return {
      enabled: true,
      label,
      error: error instanceof Error ? error.message : String(error),
    };
  }
};

const readGitMetadata = async () => {
  try {
    const [{ stdout: commitStdout }, { stdout: statusStdout }] =
      await Promise.all([
        execFileAsync("git", ["rev-parse", "HEAD"], { cwd: pkgRoot }),
        execFileAsync("git", ["status", "--short"], { cwd: pkgRoot }),
      ]);
    return {
      commit: commitStdout.trim(),
      dirty: statusStdout.trim().length > 0,
      statusShort: statusStdout.trim().split("\n").filter(Boolean),
    };
  } catch (error) {
    return {
      commit: null,
      dirty: null,
      error: error instanceof Error ? error.message : String(error),
    };
  }
};

const readRuntimeMetadata = async () => ({
  nodeVersion: process.version,
  platform: process.platform,
  arch: process.arch,
  hostname: os.hostname(),
  cpuModel: os.cpus()[0]?.model ?? null,
  cpuCount: os.cpus().length,
  totalMemoryBytes: os.totalmem(),
  freeMemoryBytes: os.freemem(),
  loadAverage: os.loadavg(),
  pid: process.pid,
  argv: process.argv,
  env: {
    NODE_ENV: process.env.NODE_ENV ?? null,
  },
});

const runClientSelfCheck = async () => {
  if (!clientSelfCheckEnabled) {
    return { enabled: false, required: false };
  }
  const baseRate =
    Number.isFinite(targetAcceptedTps) && targetAcceptedTps > 0
      ? targetAcceptedTps
      : Number.isFinite(openLoopRate) && openLoopRate > 0
        ? openLoopRate
        : 100;
  const targetRate = Math.max(1, baseRate * clientSelfCheckMultiplier);
  const intervalMs = 1000 / targetRate;
  const startedAt = performance.now();
  const deadline = startedAt + clientSelfCheckDurationSec * 1000;
  const inFlight = new Set();
  let scheduled = 0;
  let ok = 0;
  let failed = 0;
  const latencies = [];

  while (performance.now() < deadline) {
    while (inFlight.size >= submitConcurrency) {
      await Promise.race(inFlight);
    }
    const dueAt = startedAt + scheduled * intervalMs;
    const waitMs = dueAt - performance.now();
    if (waitMs > 0) {
      await sleep(waitMs);
    }
    const promise = httpClient
      .request(`${submitEndpoint}/readyz`)
      .then((resp) => {
        latencies.push(resp.latencyMs);
        if (resp.ok) {
          ok += 1;
        } else {
          failed += 1;
        }
      })
      .catch(() => {
        failed += 1;
      })
      .finally(() => {
        inFlight.delete(promise);
      });
    inFlight.add(promise);
    scheduled += 1;
  }

  await Promise.all(inFlight);
  const elapsedSec = (performance.now() - startedAt) / 1000;
  const achievedRate = ok / elapsedSec;
  const result = {
    enabled: true,
    required: clientSelfCheckRequired,
    endpoint: `${submitEndpoint}/readyz`,
    targetRate,
    minRequiredRate: targetRate * clientSelfCheckMinRatio,
    achievedRate,
    scheduled,
    ok,
    failed,
    elapsedSec,
    latencyMs: summarizeLatency(latencies),
  };
  if (
    clientSelfCheckRequired &&
    achievedRate < targetRate * clientSelfCheckMinRatio
  ) {
    throw new Error(
      `benchmark client self-check failed: achieved ${achievedRate.toFixed(2)} req/s < required ${(targetRate * clientSelfCheckMinRatio).toFixed(2)} req/s`,
    );
  }
  return result;
};

const buildStageReport = (stage) => {
  const measuredElapsedMs = Math.max(1, stage.endedAtMs - stage.startedAtMs);
  const endCounters = stage.counterEnd ?? stage.counterStart;
  const drainCounters = stage.drainCounters ?? endCounters;
  const acceptedDelta = counterDelta(stage.counterStart, endCounters, "accept");
  const rejectedDelta = counterDelta(stage.counterStart, endCounters, "reject");
  const commitTxDelta = counterDelta(
    stage.counterStart,
    endCounters,
    "commitBlockTx",
  );
  const mergeBlockDelta = counterDelta(
    stage.counterStart,
    endCounters,
    "mergeBlock",
  );
  const drainAcceptedDelta = counterDelta(
    stage.counterStart,
    drainCounters,
    "accept",
  );
  const drainRejectedDelta = counterDelta(
    stage.counterStart,
    drainCounters,
    "reject",
  );
  return {
    name: stage.name,
    mode: stage.mode,
    targetRateTps: stage.targetRateTps,
    startedAtIso: new Date(stage.startedAtMs).toISOString(),
    endedAtIso: new Date(stage.endedAtMs).toISOString(),
    measuredElapsedSec: measuredElapsedMs / 1000,
    logicalSubmitAttempts: stage.logicalSubmitAttempts,
    physicalSubmitAttempts: stage.physicalSubmitAttempts,
    submitted: stage.submitted,
    submitErrors: stage.submitErrors,
    submitStatusCounts: stage.submitStatusCounts,
    physicalSubmitStatusCounts: stage.physicalSubmitStatusCounts,
    queueFullResponses: stage.queueFullResponses,
    firstErrors: stage.firstErrors,
    scheduledStarts: stage.scheduledStarts,
    sentStarts: stage.sentStarts,
    missedStarts: stage.missedStarts,
    missedStartRatio:
      stage.scheduledStarts + stage.missedStarts > 0
        ? stage.missedStarts / (stage.scheduledStarts + stage.missedStarts)
        : 0,
    inFlightHighWater: stage.inFlightHighWater,
    bytesSent: stage.bytesSent,
    measuredWindow: summarizeCounterWindow({
      startCounters: stage.counterStart,
      endCounters,
      elapsedMs: measuredElapsedMs,
      counterKeys: [
        "submit",
        "accept",
        "reject",
        "commitBlockTx",
        "mergeBlock",
      ],
    }),
    measuredAcceptedTps: rateBetweenCounters(
      stage.counterStart,
      endCounters,
      "accept",
      measuredElapsedMs,
    ),
    measuredSubmittedTps: rateBetweenCounters(
      stage.counterStart,
      endCounters,
      "submit",
      measuredElapsedMs,
    ),
    physicalSubmitAttemptsPerSec:
      stage.physicalSubmitAttempts / (measuredElapsedMs / 1000),
    queuedSubmitSuccessPerSec: stage.submitted / (measuredElapsedMs / 1000),
    acceptedDelta,
    rejectedDelta,
    commitTxDelta,
    mergeBlockDelta,
    drainAcceptedDelta,
    drainRejectedDelta,
    missingRequiredMetrics: requireMetricPresence
      ? metricMissingKeys(endCounters)
      : [],
    drain: stage.drain,
    submitLatencyMs: summarizeLatency(stage.submitLatencyMs),
    submitAttemptLatencyMs: summarizeLatency(stage.submitAttemptLatencyMs),
    scheduleLagMs: summarizeLatency(stage.scheduleLagMs),
    scheduleLagSamplesMs: stage.scheduleLagMs,
    statusLatencyMs: summarizeLatency(stage.statusLatencyMs),
  };
};

const activeCursorCount = (cursors) =>
  cursors.filter(
    (cursor) => !cursor.stopped && cursor.nextIndex < cursor.chain.txs.length,
  ).length;

const assertCandidateCapacity = ({
  cursors,
  targetRateTps,
  durationSec,
  priorSubmitLatencyP99Ms,
}) => {
  const availableTxs = remainingTxCount(cursors);
  const requiredTxs = Math.ceil(targetRateTps * durationSec * 1.02);
  if (availableTxs < requiredTxs) {
    throw new Error(
      `candidate requires about ${requiredTxs} txs but only ${availableTxs} prebuilt txs remain`,
    );
  }
  const chainCount = activeCursorCount(cursors);
  if (chainCount <= 0) {
    throw new Error("candidate has no active chains");
  }
  if (priorSubmitLatencyP99Ms !== null && priorSubmitLatencyP99Ms > 0) {
    const chainCapacityTps = chainCount / (priorSubmitLatencyP99Ms / 1000);
    if (chainCapacityTps < targetRateTps * offeredRateMinRatio) {
      throw new Error(
        `candidate target ${targetRateTps} TPS exceeds one-in-flight chain capacity estimate ${chainCapacityTps.toFixed(2)} TPS (chains=${chainCount}, prior_submit_p99_ms=${priorSubmitLatencyP99Ms.toFixed(2)})`,
      );
    }
  }
};

const hasCounterActivity = (before, after) =>
  [
    "submit",
    "accept",
    "reject",
    "commitBlock",
    "commitBlockTx",
    "mergeBlock",
  ].some((key) => counterDelta(before, after, key) !== 0);

const waitForCandidateCleanliness = async (label) => {
  const startedAt = Date.now();
  let lastCounters = await readCounters();
  while (Date.now() - startedAt <= candidateCleanTimeoutSec * 1000) {
    const missing = requireMetricPresence
      ? metricMissingKeys(lastCounters, ["validationQueueDepth"])
      : [];
    if (missing.length > 0) {
      throw new Error(
        `required metrics missing before ${label}: ${missing.join(",")}`,
      );
    }
    const queueClean = Number(lastCounters.validationQueueDepth ?? 0) === 0;
    const localFinalizationClean =
      !waitForCommit ||
      Number(lastCounters.unconfirmedSubmittedBlockPending ?? 0) === 0;
    const mergeClean =
      !waitForMerge || Number(lastCounters.blocksInQueue ?? 0) === 0;
    if (queueClean && localFinalizationClean && mergeClean) {
      if (requireIdleNode && idleProbeSec > 0) {
        await sleep(idleProbeSec * 1000);
        const afterIdleProbe = await readCounters();
        if (hasCounterActivity(lastCounters, afterIdleProbe)) {
          throw new Error(
            `node was not idle before ${label}; global Prometheus counters changed during idle probe`,
          );
        }
        return afterIdleProbe;
      }
      return lastCounters;
    }
    await sleep(metricsPollMs);
    lastCounters = await readCounters();
  }
  throw new Error(
    `node did not reach a clean candidate window before ${label} within ${candidateCleanTimeoutSec}s`,
  );
};

const evaluateStagePass = ({ stage, stageReport, monitorSamples }) => {
  const reasons = [];
  const measuredSamples = monitorSamples.filter(
    (sample) =>
      sample.timestampMs >= stage.startedAtMs &&
      sample.timestampMs <= stage.endedAtMs,
  );
  const backlogSlope = gaugeSlopePerSec(
    measuredSamples.length >= 2 ? measuredSamples : monitorSamples,
    "validationQueueDepth",
  );
  const targetRate = Number(stageReport.targetRateTps ?? 0);
  const offeredRate = stageReport.queuedSubmitSuccessPerSec;
  const acceptedRate = stageReport.measuredAcceptedTps;
  if (stageReport.missingRequiredMetrics.length > 0) {
    reasons.push(
      `missing_required_metrics=${stageReport.missingRequiredMetrics.join(",")}`,
    );
  }
  if (targetRate > 0 && offeredRate < targetRate * offeredRateMinRatio) {
    reasons.push(
      `offered_rate ${offeredRate.toFixed(2)} < ${(targetRate * offeredRateMinRatio).toFixed(2)}`,
    );
  }
  if (targetRate > 0 && acceptedRate < targetRate * acceptedRateMinRatio) {
    reasons.push(
      `accepted_rate ${acceptedRate.toFixed(2)} < ${(targetRate * acceptedRateMinRatio).toFixed(2)}`,
    );
  }
  if (stageReport.submitErrors > 0) {
    reasons.push(`submit_errors=${stageReport.submitErrors}`);
  }
  if (stageReport.queueFullResponses > 0) {
    reasons.push(`queue_full_responses=${stageReport.queueFullResponses}`);
  }
  if (stageReport.rejectedDelta > 0 || stageReport.drainRejectedDelta > 0) {
    reasons.push(
      `unexpected_rejections measured=${stageReport.rejectedDelta} drain=${stageReport.drainRejectedDelta}`,
    );
  }
  if (stageReport.drain === null || stageReport.drain.completed !== true) {
    reasons.push("drain_not_completed");
  }
  if (
    stageReport.scheduleLagMs.p95 !== null &&
    stageReport.scheduleLagMs.p95 > scheduleLagP95MaxMs
  ) {
    reasons.push(
      `schedule_lag_p95_ms ${stageReport.scheduleLagMs.p95.toFixed(2)} > ${scheduleLagP95MaxMs}`,
    );
  }
  if (
    stageReport.scheduleLagMs.p99 !== null &&
    stageReport.scheduleLagMs.p99 > scheduleLagP99MaxMs
  ) {
    reasons.push(
      `schedule_lag_p99_ms ${stageReport.scheduleLagMs.p99.toFixed(2)} > ${scheduleLagP99MaxMs}`,
    );
  }
  if (stageReport.missedStartRatio > missedStartMaxRatio) {
    reasons.push(
      `missed_start_ratio ${stageReport.missedStartRatio.toFixed(6)} > ${missedStartMaxRatio}`,
    );
  }
  if (backlogSlope > backlogSlopeMaxPerSec) {
    reasons.push(
      `validation_queue_slope_per_sec ${backlogSlope.toFixed(4)} > ${backlogSlopeMaxPerSec}`,
    );
  }
  const bottleneck = classifyLikelyBottleneckWithEvidence({
    submitted: stageReport.submitted,
    submitErrors: stageReport.submitErrors,
    queueFullResponses: stageReport.queueFullResponses,
    acceptedDelta: stageReport.acceptedDelta,
    rejectedDelta: stageReport.rejectedDelta,
    commitTxDelta: stageReport.commitTxDelta,
    mergeBlockDelta: stageReport.mergeBlockDelta,
    targetAcceptedTps: targetRate,
    avgAcceptedTps: stageReport.measuredAcceptedTps,
    clientSelfCheck: null,
    endCounters: stage.counterEnd ?? {},
    waitForCommit,
    waitForMerge,
    scheduleLagMs: stageReport.scheduleLagMs,
    missedStarts: stageReport.missedStarts,
    inFlightHighWater: stageReport.inFlightHighWater,
    submitConcurrency,
    backlogSlopePerSec: backlogSlope,
    requiredMetricsMissing: stageReport.missingRequiredMetrics,
  });
  return {
    passed: reasons.length === 0,
    reasons,
    backlogSlopePerSec: backlogSlope,
    bottleneck,
  };
};

const writeReport = (report) => {
  fs.mkdirSync(path.dirname(reportPath), { recursive: true });
  fs.writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`);
};

/**
 * Runs the valid-stress throughput benchmark workload.
 */
const main = async () => {
  const runtimeSampler = createRuntimeSampler();
  const phaseNameRef = { current: "setup" };
  const phaseRecorder = createPhaseRecorder();
  const setPhase = (name) => {
    phaseNameRef.current = name;
    phaseRecorder.start(name);
  };

  setPhase("setup");
  let monitor = null;

  try {
    const env = parseEnv(envPath);
    const wallets = makeWalletsFromEnv(env);
    const minFeeA = BigInt(
      process.env.STRESS_MIN_FEE_A ?? env.MIN_FEE_A ?? "0",
    );
    const minFeeB = BigInt(
      process.env.STRESS_MIN_FEE_B ?? env.MIN_FEE_B ?? "0",
    );
    const pyroscope = await maybeStartPyroscope();
    const git = await readGitMetadata();
    const runtimeMetadata = await readRuntimeMetadata();

    if (wallets.length === 0) {
      throw new Error(`No genesis wallet seeds found in ${envPath}`);
    }

    const configForReport = {
      envPath,
      submitEndpoint,
      metricsEndpoint,
      benchmarkMode,
      workload: "valid-native-chain",
      chainLength,
      maxChains,
      utxosPerWallet,
      minLovelace: minLovelace.toString(),
      fanoutEnabled,
      fanoutMaxOutputsPerTx,
      fanoutOutputLovelace:
        fanoutOutputLovelace === null ? null : fanoutOutputLovelace.toString(),
      fanoutStatusTimeoutMs,
      retry503,
      measuredRetry503,
      retryDelayMs,
      metricsPollMs,
      observeAfterSubmitSec,
      targetAcceptedTps,
      requireFreshChains,
      measuredSec,
      warmupTxs,
      warmupSec,
      cooldownSec,
      drainTimeoutSec,
      waitForCommit,
      waitForMerge,
      statusSampleSize,
      submitConcurrency,
      httpConnections,
      httpPipelining,
      httpTimeoutMs,
      openLoopRate,
      rampStartTps,
      rampStepTps,
      rampMaxTps,
      rampStageSec,
      rampMinAcceptedRatio,
      offeredRateMinRatio,
      acceptedRateMinRatio,
      scheduleLagP95MaxMs,
      scheduleLagP99MaxMs,
      missedStartMaxRatio,
      backlogSlopeMaxPerSec,
      candidateCleanTimeoutSec,
      requireIdleNode,
      idleProbeSec,
      requireMetricPresence,
      findMaxBinaryIterations,
      findMaxConfirmationSec,
      findMaxRepeats,
      findMaxMaxCandidates,
      clientSelfCheckEnabled,
      clientSelfCheckRequired,
      clientSelfCheckMultiplier,
      clientSelfCheckMinRatio,
      clientSelfCheckDurationSec,
      profileMode,
      pyroscopeEnabled,
      pgStatStatementsEnabled,
      minFeeA: minFeeA.toString(),
      minFeeB: minFeeB.toString(),
      reportPath,
    };

    console.log("Starting Midgard L2 throughput benchmark with config:");
    console.log(JSON.stringify(configForReport, null, 2));

    setPhase("client-self-check");
    const clientSelfCheck = await runClientSelfCheck();
    if (clientSelfCheck.enabled) {
      console.log("Client self-check:");
      console.log(JSON.stringify(clientSelfCheck, null, 2));
    }

    setPhase("setup");
    const pgBefore = await readPgStatStatements(env, "before");
    const candidates = [];
    const seenOutRefs = new Set();
    for (const wallet of wallets) {
      const utxos = await fetchUtxos(wallet.address);
      let selected = 0;
      for (const utxo of utxos) {
        if (selected >= utxosPerWallet) {
          break;
        }
        const outRefHex = utxo.outref.toLowerCase();
        if (seenOutRefs.has(outRefHex)) {
          continue;
        }
        try {
          const coin = decodeCoin(utxo.value);
          if (coin < minLovelace) {
            continue;
          }
          candidates.push({
            walletKey: wallet.key,
            address: wallet.address,
            signer: wallet.signer,
            spendOutRefCbor: Buffer.from(utxo.outref, "hex"),
            outputCbor: Buffer.from(utxo.value, "hex"),
            lovelace: coin,
            outRefHex,
          });
          seenOutRefs.add(outRefHex);
          selected += 1;
        } catch {
          // Skip malformed or non-decodable UTxOs.
        }
      }
      console.log(
        `wallet ${wallet.key} address=${wallet.address} selected_utxos=${selected}`,
      );
    }

    if (candidates.length === 0) {
      throw new Error("No spendable UTxOs found for configured wallets");
    }

    setPhase("fanout");
    const fanout = await ensureFanoutCandidates({
      candidates,
      minFeeA,
      minFeeB,
    });
    const effectiveCandidates = fanout.candidates;
    console.log(
      `Available benchmark source UTxOs after fanout: ${effectiveCandidates.length} (fanout_txs=${fanout.fanoutTxCount}, fanout_outputs=${fanout.fanoutOutputCount})`,
    );

    setPhase("prebuild");
    /** @type {{ outRefHex: string; txs: PrebuiltTx[]; signer: any; outputCbor: Buffer; spendOutRefCbor: Buffer; walletKey: string; address: string; lovelace: bigint; }[]} */
    const selectedChains = [];
    const generatedTxIds = new Set();
    let replaySkipped = 0;
    let duplicateSkipped = 0;
    let chainBuildSkipped = 0;

    for (const candidate of effectiveCandidates) {
      if (selectedChains.length >= maxChains) {
        break;
      }
      let txs;
      try {
        txs = prebuildChain(candidate, chainLength, { minFeeA, minFeeB });
      } catch (error) {
        chainBuildSkipped += 1;
        console.log(
          `Skipping source UTxO that cannot fund requested chain: outref=${candidate.outRefHex} lovelace=${candidate.lovelace.toString()} reason=${error instanceof Error ? error.message : String(error)}`,
        );
        continue;
      }
      if (txs.length === 0) {
        continue;
      }
      if (requireFreshChains) {
        const firstStatus = await fetchTxStatus(txs[0].txIdHex);
        if (firstStatus !== "not_found") {
          replaySkipped += 1;
          continue;
        }
      }
      let hasDuplicate = false;
      for (const tx of txs) {
        if (generatedTxIds.has(tx.txIdHex)) {
          hasDuplicate = true;
          break;
        }
      }
      if (hasDuplicate) {
        duplicateSkipped += 1;
        continue;
      }
      for (const tx of txs) {
        generatedTxIds.add(tx.txIdHex);
      }
      selectedChains.push({
        ...candidate,
        txs,
      });
    }

    if (selectedChains.length === 0) {
      throw new Error(
        "No eligible fresh chains found. Increase STRESS_UTXOS_PER_WALLET or disable STRESS_REQUIRE_FRESH_CHAINS=false for diagnostics.",
      );
    }

    console.log(
      `Using ${selectedChains.length} independent prebuilt chains (replay_skipped=${replaySkipped}, duplicate_skipped=${duplicateSkipped}, chain_build_skipped=${chainBuildSkipped})`,
    );

    const cursors = makeChainCursors(selectedChains);
    const stageReports = [];
    monitor = await startCounterMonitor(phaseNameRef);

    if (warmupTxs > 0 || warmupSec > 0) {
      setPhase("warmup");
      const warmupStage = await runClosedLoopStage({
        name: "warmup",
        cursors,
        durationSec: warmupSec > 0 ? warmupSec : Number.MAX_SAFE_INTEGER / 1000,
        maxTxs: warmupTxs > 0 ? warmupTxs : Number.POSITIVE_INFINITY,
      });
      await waitForStageDrain(warmupStage);
      await collectStatusLatencies(warmupStage);
      stageReports.push(buildStageReport(warmupStage));
    }

    if (remainingTxCount(cursors) <= 0) {
      throw new Error("No benchmark transactions remain after warmup");
    }

    setPhase("measured");
    const measuredStages = [];
    const candidateEvaluations = [];
    let priorSubmitLatencyP99Ms = null;
    let findMaxResult = null;
    const runOpenCandidate = async ({ name, targetRate, durationSec }) => {
      await waitForCandidateCleanliness(name);
      assertCandidateCapacity({
        cursors,
        targetRateTps: targetRate,
        durationSec,
        priorSubmitLatencyP99Ms,
      });
      const stage = await runOpenLoopStage({
        name,
        cursors,
        rateTps: targetRate,
        durationSec,
        retryLimit: measuredRetry503,
      });
      await waitForStageDrain(stage);
      await collectStatusLatencies(stage);
      const stageReport = buildStageReport(stage);
      const evaluation = evaluateStagePass({
        stage,
        stageReport,
        monitorSamples: monitor.samples,
      });
      stageReport.evaluation = evaluation;
      priorSubmitLatencyP99Ms =
        stageReport.submitLatencyMs.p99 ?? priorSubmitLatencyP99Ms;
      measuredStages.push(stageReport);
      stageReports.push(stageReport);
      candidateEvaluations.push({
        name,
        targetRateTps: targetRate,
        durationSec,
        passed: evaluation.passed,
        reasons: evaluation.reasons,
        measuredAcceptedTps: stageReport.measuredAcceptedTps,
        queuedSubmitSuccessPerSec: stageReport.queuedSubmitSuccessPerSec,
        backlogSlopePerSec: evaluation.backlogSlopePerSec,
        bottleneck: evaluation.bottleneck,
      });
      return { stage, stageReport, evaluation };
    };
    const runSearchCandidate = async ({ name, targetRate, durationSec }) => {
      try {
        return await runOpenCandidate({ name, targetRate, durationSec });
      } catch (error) {
        const reason = `preflight_failed: ${error instanceof Error ? error.message : String(error)}`;
        const evaluation = {
          passed: false,
          reasons: [reason],
          backlogSlopePerSec: null,
          bottleneck: {
            label: "benchmark-client limited",
            rule: "candidate preflight failed before measured submission",
            evidence: {
              targetRateTps: targetRate,
              durationSec,
              reason,
            },
          },
        };
        const stageReport = {
          name,
          targetRateTps: targetRate,
          measuredAcceptedTps: 0,
          queuedSubmitSuccessPerSec: 0,
          evaluation,
        };
        candidateEvaluations.push({
          name,
          targetRateTps: targetRate,
          durationSec,
          passed: false,
          reasons: evaluation.reasons,
          measuredAcceptedTps: 0,
          queuedSubmitSuccessPerSec: 0,
          backlogSlopePerSec: null,
          bottleneck: evaluation.bottleneck,
        });
        return { stage: null, stageReport, evaluation };
      }
    };

    if (benchmarkMode === "ramp" || benchmarkMode === "find-max") {
      if (benchmarkMode === "ramp") {
        for (
          let targetRate = rampStartTps;
          targetRate <= rampMaxTps && remainingTxCount(cursors) > 0;
          targetRate += rampStepTps
        ) {
          setPhase(`measured-ramp-${targetRate}`);
          const { stageReport, evaluation } = await runSearchCandidate({
            name: `ramp-${targetRate}`,
            targetRate,
            durationSec: rampStageSec,
          });
          if (!evaluation.passed) {
            console.log(
              `Stopping ramp at target=${targetRate}: reasons=${evaluation.reasons.join("; ")} measured_accepted_tps=${stageReport.measuredAcceptedTps.toFixed(2)}`,
            );
            break;
          }
        }
      } else {
        const exploratoryPassed = [];
        let low = 0;
        let high = null;
        let candidateCount = 0;
        for (
          let targetRate = rampStartTps;
          targetRate <= rampMaxTps &&
          remainingTxCount(cursors) > 0 &&
          candidateCount < findMaxMaxCandidates;
          targetRate += rampStepTps
        ) {
          setPhase(`measured-find-max-ramp-${targetRate}`);
          const result = await runSearchCandidate({
            name: `find-max-ramp-${targetRate}`,
            targetRate,
            durationSec: rampStageSec,
          });
          candidateCount += 1;
          if (result.evaluation.passed) {
            low = targetRate;
            exploratoryPassed.push(targetRate);
          } else {
            high = targetRate;
            break;
          }
        }

        if (high !== null && low > 0) {
          for (
            let iteration = 0;
            iteration < findMaxBinaryIterations &&
            remainingTxCount(cursors) > 0 &&
            candidateCount < findMaxMaxCandidates;
            iteration += 1
          ) {
            const targetRate = Math.round(((low + high) / 2) * 100) / 100;
            if (targetRate <= low || targetRate >= high) {
              break;
            }
            setPhase(`measured-find-max-binary-${iteration}-${targetRate}`);
            const result = await runSearchCandidate({
              name: `find-max-binary-${iteration}-${targetRate}`,
              targetRate,
              durationSec: rampStageSec,
            });
            candidateCount += 1;
            if (result.evaluation.passed) {
              low = targetRate;
              exploratoryPassed.push(targetRate);
            } else {
              high = targetRate;
            }
          }
        }

        const confirmationTargets = [...new Set(exploratoryPassed)]
          .sort((a, b) => b - a)
          .slice(0, 3);
        for (const targetRate of confirmationTargets) {
          const confirmationReports = [];
          let confirmed = true;
          for (
            let repeat = 1;
            repeat <= findMaxRepeats &&
            remainingTxCount(cursors) > 0 &&
            candidateCount < findMaxMaxCandidates;
            repeat += 1
          ) {
            setPhase(`measured-find-max-confirm-${targetRate}-${repeat}`);
            const result = await runSearchCandidate({
              name: `find-max-confirm-${targetRate}-${repeat}`,
              targetRate,
              durationSec: findMaxConfirmationSec,
            });
            candidateCount += 1;
            confirmationReports.push(result.stageReport);
            if (!result.evaluation.passed) {
              confirmed = false;
              break;
            }
          }
          if (confirmed && confirmationReports.length === findMaxRepeats) {
            findMaxResult = {
              maxSustainableAcceptedTxPerSec: targetRate,
              confirmationRepeats: findMaxRepeats,
              confirmationDurationSec: findMaxConfirmationSec,
              confirmationStageNames: confirmationReports.map(
                (report) => report.name,
              ),
              confirmationAcceptedTps: confirmationReports.map(
                (report) => report.measuredAcceptedTps,
              ),
            };
            break;
          }
        }

        if (findMaxResult === null) {
          console.log(
            "find-max did not confirm a sustainable candidate; see candidate evaluations for failure reasons",
          );
        }
      }
    } else if (benchmarkMode === "open") {
      await runOpenCandidate({
        name: "measured-open",
        targetRate: openLoopRate,
        durationSec: measuredSec,
      });
    } else {
      await waitForCandidateCleanliness("measured-closed");
      const stage = await runClosedLoopStage({
        name: "measured-closed",
        cursors,
        durationSec: measuredSec,
        retryLimit: measuredRetry503,
      });
      await waitForStageDrain(stage);
      await collectStatusLatencies(stage);
      const stageReport = buildStageReport(stage);
      stageReport.evaluation = evaluateStagePass({
        stage,
        stageReport,
        monitorSamples: monitor.samples,
      });
      measuredStages.push(stageReport);
      stageReports.push(stageReport);
    }

    setPhase("cooldown");
    if (cooldownSec > 0) {
      await sleep(cooldownSec * 1000);
    }
    const monitorSamples = monitor.samples;
    await monitor.stop();
    monitor = null;
    const finalCounterSnapshot = await readCounters();

    const pgAfter = await readPgStatStatements(env, "after");
    phaseRecorder.end();
    const runtime = runtimeSampler.stop();

    const primaryMeasuredStages =
      benchmarkMode === "find-max" && findMaxResult !== null
        ? measuredStages.filter((stage) =>
            findMaxResult.confirmationStageNames.includes(stage.name),
          )
        : measuredStages;

    const measuredElapsedSec = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.measuredElapsedSec,
      0,
    );
    const measuredSubmitted = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.submitted,
      0,
    );
    const measuredPhysicalSubmitAttempts = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.physicalSubmitAttempts,
      0,
    );
    const measuredQueueFullResponses = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.queueFullResponses,
      0,
    );
    const measuredSubmitErrors = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.submitErrors,
      0,
    );
    const measuredAcceptedDelta = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.acceptedDelta,
      0,
    );
    const measuredRejectedDelta = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.rejectedDelta,
      0,
    );
    const measuredCommitTxDelta = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.commitTxDelta,
      0,
    );
    const measuredMergeBlockDelta = primaryMeasuredStages.reduce(
      (acc, stage) => acc + stage.mergeBlockDelta,
      0,
    );
    const measuredMonitorSamples = monitorSamples.filter((sample) =>
      String(sample.phase).startsWith("measured"),
    );
    const rollingRateSamples =
      measuredMonitorSamples.length >= 2
        ? measuredMonitorSamples
        : monitorSamples;
    const rollingRates = summarizeRollingRates(
      rollingRateSamples,
      ["submit", "accept", "reject", "commitBlockTx", "mergeBlock"],
      BENCHMARK_WINDOWS_MS,
    );
    const combinedScheduleLag = summarizeLatency(
      primaryMeasuredStages.flatMap(
        (stage) => stage.scheduleLagSamplesMs ?? [],
      ),
    );
    const missingRequiredMetrics = [
      ...new Set(
        primaryMeasuredStages.flatMap(
          (stage) => stage.missingRequiredMetrics ?? [],
        ),
      ),
    ];
    const measuredBacklogSlope = gaugeSlopePerSec(
      measuredMonitorSamples.length >= 2
        ? measuredMonitorSamples
        : monitorSamples,
      "validationQueueDepth",
    );
    const bottleneck = classifyLikelyBottleneckWithEvidence({
      submitted: measuredSubmitted,
      submitErrors: measuredSubmitErrors,
      queueFullResponses: measuredQueueFullResponses,
      acceptedDelta: measuredAcceptedDelta,
      rejectedDelta: measuredRejectedDelta,
      commitTxDelta: measuredCommitTxDelta,
      mergeBlockDelta: measuredMergeBlockDelta,
      targetAcceptedTps:
        benchmarkMode === "find-max" && findMaxResult !== null
          ? findMaxResult.maxSustainableAcceptedTxPerSec
          : targetAcceptedTps,
      avgAcceptedTps:
        measuredElapsedSec > 0 ? measuredAcceptedDelta / measuredElapsedSec : 0,
      clientSelfCheck,
      endCounters: finalCounterSnapshot,
      waitForCommit,
      waitForMerge,
      scheduleLagMs: combinedScheduleLag,
      submitConcurrency,
      backlogSlopePerSec: measuredBacklogSlope,
      requiredMetricsMissing: missingRequiredMetrics,
    });

    const summary = {
      chainCount: selectedChains.length,
      chainLength,
      attempted: selectedChains.reduce(
        (acc, chain) => acc + chain.txs.length,
        0,
      ),
      remaining: remainingTxCount(cursors),
      replaySkipped,
      duplicateSkipped,
      chainBuildSkipped,
      fanoutTxCount: fanout.fanoutTxCount,
      fanoutOutputCount: fanout.fanoutOutputCount,
      uniquePrebuiltTxIds: generatedTxIds.size,
      primaryStageNames: primaryMeasuredStages.map((stage) => stage.name),
      submitted: measuredSubmitted,
      physicalSubmitAttempts: measuredPhysicalSubmitAttempts,
      submitErrors: measuredSubmitErrors,
      queueFullResponses: measuredQueueFullResponses,
      acceptDelta: measuredAcceptedDelta,
      rejectDelta: measuredRejectedDelta,
      commitTxDelta: measuredCommitTxDelta,
      mergeBlockDelta: measuredMergeBlockDelta,
      physicalSubmitAttemptsPerSec:
        measuredElapsedSec > 0
          ? measuredPhysicalSubmitAttempts / measuredElapsedSec
          : 0,
      queuedSubmitSuccessPerSec:
        measuredElapsedSec > 0 ? measuredSubmitted / measuredElapsedSec : 0,
      avgAcceptedTps:
        measuredElapsedSec > 0 ? measuredAcceptedDelta / measuredElapsedSec : 0,
      committedTxPerSec:
        measuredElapsedSec > 0 ? measuredCommitTxDelta / measuredElapsedSec : 0,
      mergeBlocksPerSec:
        measuredElapsedSec > 0
          ? measuredMergeBlockDelta / measuredElapsedSec
          : 0,
      maxAcceptRate1s: rollingRates.accept?.["1s"] ?? 0,
      maxSubmitRate1s: rollingRates.submit?.["1s"] ?? 0,
      maxRejectRate1s: rollingRates.reject?.["1s"] ?? 0,
      maxCommitTxRate1s: rollingRates.commitBlockTx?.["1s"] ?? 0,
      maxMergeBlockRate1s: rollingRates.mergeBlock?.["1s"] ?? 0,
      targetAcceptedTps,
      maxSustainableAcceptedTxPerSec:
        findMaxResult?.maxSustainableAcceptedTxPerSec ?? null,
      measuredElapsedSec,
      bottleneck: bottleneck.label,
      bottleneckEvidence: bottleneck,
      missingRequiredMetrics,
      measuredBacklogSlopePerSec: measuredBacklogSlope,
      reportPath,
      firstErrors: primaryMeasuredStages
        .flatMap((stage) => stage.firstErrors)
        .slice(0, 10),
    };

    const report = {
      benchmark: "midgard-l2-throughput",
      version: 2,
      generatedAtIso: new Date().toISOString(),
      metadata: {
        git,
        runtime: runtimeMetadata,
      },
      config: configForReport,
      clientSelfCheck,
      sourceUtxos: {
        selectedBeforeFanout: candidates.length,
        selectedAfterFanout: effectiveCandidates.length,
        fanoutTxCount: fanout.fanoutTxCount,
        fanoutOutputCount: fanout.fanoutOutputCount,
      },
      workload: {
        name: "valid-native-chain",
        chainCount: selectedChains.length,
        chainLength,
        uniquePrebuiltTxIds: generatedTxIds.size,
      },
      phases: phaseRecorder.list(),
      stages: stageReports,
      candidateEvaluations,
      findMax: findMaxResult,
      rollingRateSampleScope:
        measuredMonitorSamples.length >= 2 ? "measured" : "all",
      rollingRates,
      runtime,
      profiling: {
        nodeProfileMode: profileMode,
        pyroscope,
      },
      postgres: {
        before: pgBefore,
        after: pgAfter,
      },
      summary,
    };

    writeReport(report);
    console.log("Benchmark summary:");
    console.log(JSON.stringify(summary, null, 2));

    const failedDrain = measuredStages.some(
      (stage) =>
        primaryMeasuredStages.includes(stage) &&
        (stage.drain === null || stage.drain.completed !== true),
    );
    if (
      measuredAcceptedDelta <= 0 ||
      measuredSubmitErrors > 0 ||
      failedDrain ||
      (benchmarkMode === "find-max" && findMaxResult === null)
    ) {
      process.exitCode = 1;
    }
  } finally {
    if (monitor !== null) {
      await monitor.stop();
    }
    await httpClient.close();
  }
};

main().catch((error) => {
  console.error("throughput-valid-stress failed:", error);
  process.exitCode = 1;
});
