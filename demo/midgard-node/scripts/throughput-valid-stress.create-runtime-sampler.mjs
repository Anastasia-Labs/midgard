import fs from "node:fs";
import path from "node:path";
import {
  monitorEventLoopDelay,
  performance,
  PerformanceObserver,
} from "node:perf_hooks";

import { buildNativeSignedOneToOneWithMinFee as buildNativeSignedOneToOne } from "./native-tx-workload-utils.mjs";
import { summarizeLatency } from "./throughput-benchmark-utils.mjs";

/** @typedef {{ outref: string; outputCbor: string }} NodeUtxo */
/** @typedef {{ txHex: string; txIdHex: string }} PrebuiltTx */

export const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

export const makeNdjsonWriter = (filePath) => {
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
  const stream = fs.createWriteStream(filePath, { flags: "w" });
  return {
    path: filePath,
    write(value) {
      stream.write(`${JSON.stringify(value)}\n`);
    },
    async close() {
      await new Promise((resolve, reject) => {
        stream.once("error", reject);
        stream.end(resolve);
      });
    },
  };
};

/**
 * Escapes a string for literal use in a regular expression.
 */
const escapeRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

/**
 * Extracts a Prometheus sample value from metrics text.
 */
export const extractMetricValue = (text, names) => {
  for (const name of names) {
    const pattern = `^${escapeRegex(name)}(?:\\{[^}]*\\})?\\s+([0-9]+(?:\\.[0-9]+)?)$`;
    const m = text.match(new RegExp(pattern, "m"));
    if (m !== null) {
      return { value: Number(m[1]), name };
    }
  }
  return { value: 0, name: null };
};

/**
 * Extracts Prometheus histogram series for machine-readable report artifacts.
 */
export const extractHistogram = (text, baseName) => {
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
 * Prebuilds a dependent transaction chain for the stress workload.
 */
export const prebuildChain = (chain, length, feeConfig) => {
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

export const makeChainCursors = (chains) =>
  chains.map((chain, chainIndex) => ({
    chain,
    chainIndex,
    nextIndex: 0,
    stopped: false,
    async takeNextTx() {
      if (this.stopped || this.nextIndex >= this.chain.txs.length) {
        return null;
      }
      const tx = this.chain.txs[this.nextIndex];
      const txIndex = this.nextIndex;
      this.nextIndex += 1;
      return {
        ...tx,
        chainIndex: this.chainIndex,
        txIndex,
      };
    },
  }));

export const remainingTxCount = (cursors) =>
  cursors.reduce(
    (acc, cursor) =>
      acc +
      (cursor.stopped
        ? 0
        : (cursor.entry?.rowCount ?? cursor.chain.txs.length) -
          cursor.nextIndex),
    0,
  );

export const takeNextTx = async (cursor) => await cursor.takeNextTx();

export const createStageStats = ({ name, mode, targetRateTps = null }) => ({
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
  cursorPositionsAtStart: null,
  cursorPositionsAtEnd: null,
  phase1StageACheckpoint: null,
});

export const snapshotCursorPositions = (cursors) =>
  cursors.map((cursor) => ({
    chainIndex: cursor.chainIndex,
    nextIndex: cursor.nextIndex,
  }));

export const findAvailableCursor = (cursors, busy, startIndex) => {
  for (let offset = 0; offset < cursors.length; offset += 1) {
    const index = (startIndex + offset) % cursors.length;
    const cursor = cursors[index];
    if (
      !busy.has(cursor.chainIndex) &&
      !cursor.stopped &&
      cursor.nextIndex < (cursor.entry?.rowCount ?? cursor.chain.txs.length)
    ) {
      return { cursor, nextIndex: (index + 1) % cursors.length };
    }
  }
  return null;
};

export const collectCalibrationRows = async (cursors, count) => {
  const rows = [];
  const busy = new Set();
  let nextCursorIndex = 0;
  while (rows.length < count) {
    const selected = findAvailableCursor(cursors, busy, nextCursorIndex);
    if (selected === null) {
      break;
    }
    nextCursorIndex = selected.nextIndex;
    const tx = await takeNextTx(selected.cursor);
    if (tx !== null) {
      // Calibration needs only the request body. Retaining parsed corpus rows
      // multiplies heap use by every metadata string and output array.
      rows.push(tx.txHex);
    }
  }
  if (rows.length < count) {
    throw new Error(
      `no-op calibration needs ${count} corpus rows but only ${rows.length} were available`,
    );
  }
  return rows;
};

export const summarizeScheduleSlip = (values) => {
  const summary = summarizeLatency(values);
  return {
    p50: summary.p50 ?? 0,
    p95: summary.p95 ?? 0,
    p99: summary.p99 ?? 0,
    max: summary.max ?? 0,
  };
};

export const createRuntimeSampler = () => {
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

export const updateFramedHash = (hash, relativePath, bytes) => {
  const pathBytes = Buffer.from(relativePath);
  const lengths = Buffer.allocUnsafe(12);
  lengths.writeUInt32LE(pathBytes.length, 0);
  lengths.writeBigUInt64LE(BigInt(bytes.length), 4);
  hash.update(lengths).update(pathBytes).update(bytes);
};
