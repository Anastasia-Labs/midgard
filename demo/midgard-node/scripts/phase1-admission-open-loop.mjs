#!/usr/bin/env node

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { createReadStream, readFileSync, writeFileSync } from "node:fs";
import { availableParallelism, cpus, hostname } from "node:os";
import { resolve } from "node:path";
import { performance } from "node:perf_hooks";
import { createInterface } from "node:readline";

import { Pool } from "undici";
import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";

const requireOptIn = () => {
  if (process.env.PHASE1_ADMISSION_OPERATOR !== "1") {
    throw new Error("PHASE1_ADMISSION_OPERATOR=1 is required");
  }
  const endpoint = new URL(process.env.PHASE1_ADMISSION_ENDPOINT ?? "");
  if (
    !["127.0.0.1", "localhost", "::1"].includes(endpoint.hostname) ||
    endpoint.protocol !== "http:"
  ) {
    throw new Error("PHASE1_ADMISSION_ENDPOINT must be loopback HTTP");
  }
  if (endpoint.port === "3000") {
    throw new Error("Refusing the live Midgard HTTP port 3000");
  }
  return endpoint;
};

const positiveNumber = (name, fallback) => {
  const value = Number(process.env[name] ?? fallback);
  if (!Number.isFinite(value) || value <= 0) {
    throw new Error(`${name} must be a positive number`);
  }
  return value;
};

const endpoint = requireOptIn();
const corpusPath = resolve(process.env.PHASE1_ADMISSION_CORPUS_PATH ?? "");
const manifestPath = resolve(
  process.env.PHASE1_ADMISSION_CORPUS_MANIFEST_PATH ??
    `${corpusPath}.manifest.json`,
);
const reportPath = resolve(
  process.env.PHASE1_ADMISSION_CLIENT_REPORT ??
    "tests/benchmarks/output/phase1-admission-client.json",
);
const rateTps = positiveNumber("PHASE1_ADMISSION_RATE_TPS", "5250");
const durationSec = positiveNumber("PHASE1_ADMISSION_DURATION_SEC", "300");
const warmupRateTps = positiveNumber(
  "PHASE1_ADMISSION_WARMUP_RATE_TPS",
  "1000",
);
const warmupSec = positiveNumber("PHASE1_ADMISSION_WARMUP_SEC", "5");
const connections = Math.floor(
  positiveNumber("PHASE1_ADMISSION_HTTP_CONNECTIONS", "256"),
);
const maxInFlight = Math.floor(
  positiveNumber("PHASE1_ADMISSION_MAX_IN_FLIGHT", "2048"),
);

const percentile = (samples, fraction) => {
  if (samples.length === 0) return null;
  const sorted = [...samples].sort((left, right) => left - right);
  return sorted[
    Math.min(sorted.length - 1, Math.floor(sorted.length * fraction))
  ];
};

const summarize = (samples) => ({
  count: samples.length,
  p50: percentile(samples, 0.5),
  p95: percentile(samples, 0.95),
  p99: percentile(samples, 0.99),
  max:
    samples.length === 0
      ? null
      : samples.reduce((maximum, sample) => Math.max(maximum, sample), 0),
});

const expandCpuList = (value) => {
  const ids = [];
  for (const part of value.split(",")) {
    const [startText, endText] = part.trim().split("-");
    const start = Number(startText);
    const end = endText === undefined ? start : Number(endText);
    for (let cpu = start; cpu <= end; cpu += 1) ids.push(cpu);
  }
  return ids;
};

const readTopology = () => {
  const status = readFileSync("/proc/self/status", "utf8");
  const allowedText = /^Cpus_allowed_list:\s*(.+)$/mu.exec(status)?.[1]?.trim();
  if (allowedText === undefined) {
    throw new Error("/proc/self/status has no Cpus_allowed_list");
  }
  const logicalCpuIds = expandCpuList(allowedText);
  const allowed = new Set(logicalCpuIds);
  const rows = execFileSync("lscpu", ["-p=CPU,CORE,SOCKET"], {
    encoding: "utf8",
  })
    .split(/\r?\n/u)
    .filter((line) => line.length > 0 && !line.startsWith("#"))
    .map((line) => line.split(","))
    .filter(([cpu]) => allowed.has(Number(cpu)));
  const physicalCoreIds = rows.map(([, core, socket]) => `${socket}:${core}`);
  return {
    logicalCpuIds,
    physicalCoreIds,
    distinctPhysicalCoreCount: new Set(physicalCoreIds).size,
  };
};

const sha256File = async (path) => {
  const hash = createHash("sha256");
  for await (const chunk of createReadStream(path)) hash.update(chunk);
  return hash.digest("hex");
};

class CorpusCursor {
  constructor(path) {
    this.input = createReadStream(path, { encoding: "utf8" });
    this.lines = createInterface({
      input: this.input,
      crlfDelay: Number.POSITIVE_INFINITY,
    });
    this.iterator = this.lines[Symbol.asyncIterator]();
    this.rowsRead = 0;
  }

  async next() {
    const next = await this.iterator.next();
    if (next.done) throw new Error("canonical corpus exhausted");
    const row = JSON.parse(next.value);
    const body = Buffer.from(row.canonicalCborHex, "hex");
    if (body.length !== row.canonicalCborByteLength) {
      throw new Error(`corpus byte-length mismatch for ${row.txHash}`);
    }
    const rowSha256 = createHash("sha256").update(body).digest("hex");
    if (rowSha256 !== row.canonicalCborSha256) {
      throw new Error(`corpus row SHA-256 mismatch for ${row.txHash}`);
    }
    this.rowsRead += 1;
    return body;
  }

  close() {
    this.lines.close();
    this.input.destroy();
  }
}

const sleep = (milliseconds) =>
  new Promise((resolveSleep) => setTimeout(resolveSleep, milliseconds));

const pool = new Pool(endpoint.origin, {
  connections,
  pipelining: 1,
  keepAliveTimeout: 30_000,
  keepAliveMaxTimeout: 60_000,
});
const cursor = new CorpusCursor(corpusPath);

const runStage = async ({ name, rate, seconds, retainSamples }) => {
  const requestCount = Math.floor(rate * seconds);
  const inFlight = new Set();
  const latencyMs = [];
  const scheduleLagMs = [];
  const statusCounts = new Map();
  let accepted202 = 0;
  let duplicate200 = 0;
  let rejectedOrFailed = 0;
  let completed = 0;
  let maxObservedInFlight = 0;
  let lastProgressAt = performance.now();
  const cpuBefore = process.cpuUsage();
  const rssBeforeBytes = process.memoryUsage().rss;
  let peakRssBytes = rssBeforeBytes;
  const memorySampler = setInterval(() => {
    peakRssBytes = Math.max(peakRssBytes, process.memoryUsage().rss);
  }, 100);
  const startedAt = performance.now();
  let lastCompletedAt = startedAt;

  const dispatch = (body, scheduledAt) => {
    const submissionBody = encodeMidgardProofSubmission({
      transactionCbor: body,
      programMaterial: [],
    });
    const dispatchedAt = performance.now();
    if (retainSamples)
      scheduleLagMs.push(Math.max(0, dispatchedAt - scheduledAt));
    const requestStartedAt = dispatchedAt;
    const task = pool
      .request({
        path: "/submit",
        method: "POST",
        headers: { "content-type": "application/vnd.midgard.v1+cbor" },
        body: submissionBody,
      })
      .then(async (response) => {
        await response.body.text();
        const latency = performance.now() - requestStartedAt;
        if (retainSamples) latencyMs.push(latency);
        const key = String(response.statusCode);
        statusCounts.set(key, (statusCounts.get(key) ?? 0) + 1);
        if (response.statusCode === 202) accepted202 += 1;
        else if (response.statusCode === 200) duplicate200 += 1;
        else rejectedOrFailed += 1;
      })
      .catch(() => {
        rejectedOrFailed += 1;
        statusCounts.set(
          "transport_error",
          (statusCounts.get("transport_error") ?? 0) + 1,
        );
      })
      .finally(() => {
        completed += 1;
        lastCompletedAt = performance.now();
        inFlight.delete(task);
      });
    inFlight.add(task);
    maxObservedInFlight = Math.max(maxObservedInFlight, inFlight.size);
  };

  for (let ordinal = 0; ordinal < requestCount; ordinal += 1) {
    const scheduledAt = startedAt + (ordinal / rate) * 1_000;
    while (performance.now() < scheduledAt) {
      const remaining = scheduledAt - performance.now();
      if (remaining > 1) await sleep(Math.min(remaining, 5));
      else
        await new Promise((resolveImmediate) => setImmediate(resolveImmediate));
    }
    while (inFlight.size >= maxInFlight) await Promise.race(inFlight);
    dispatch(await cursor.next(), scheduledAt);
    const now = performance.now();
    if (now - lastProgressAt >= 30_000) {
      process.stderr.write(
        `phase1_admission_client stage=${name} scheduled=${(ordinal + 1).toString()} completed=${completed.toString()} in_flight=${inFlight.size.toString()}\n`,
      );
      lastProgressAt = now;
    }
  }
  await Promise.all(inFlight);
  clearInterval(memorySampler);
  const measuredDurationMs = lastCompletedAt - startedAt;
  const cpu = process.cpuUsage(cpuBefore);
  const processCpuMs = (cpu.user + cpu.system) / 1_000;
  return {
    name,
    requestedRateTps: rate,
    requestedDurationSec: seconds,
    measuredDurationMs,
    offered: requestCount,
    completed,
    accepted202,
    duplicate200,
    rejectedOrFailed,
    acceptedTps: accepted202 / (measuredDurationMs / 1_000),
    statusCounts: Object.fromEntries([...statusCounts.entries()].sort()),
    latencyMs: summarize(latencyMs),
    scheduleLagMs: summarize(scheduleLagMs),
    maxObservedInFlight,
    resources: {
      processCpuMs,
      averageCpuCores: processCpuMs / measuredDurationMs,
      rssBeforeBytes,
      rssAfterBytes: process.memoryUsage().rss,
      peakRssBytes,
    },
  };
};

try {
  const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
  const shortProof = process.env.PHASE1_ADMISSION_SHORT_PROOF === "1";
  const corpusSha256 = shortProof
    ? manifest.files?.corpus?.sha256
    : await sha256File(corpusPath);
  if (corpusSha256 !== manifest.files?.corpus?.sha256) {
    throw new Error(
      `corpus SHA-256 mismatch: expected=${manifest.files?.corpus?.sha256},actual=${corpusSha256}`,
    );
  }
  const topology = readTopology();
  const warmup = await runStage({
    name: "warmup",
    rate: warmupRateTps,
    seconds: warmupSec,
    retainSamples: false,
  });
  const measured = await runStage({
    name: "measured",
    rate: rateTps,
    seconds: durationSec,
    retainSamples: true,
  });
  const report = {
    generatedAtIso: new Date().toISOString(),
    host: hostname(),
    nodeVersion: process.version,
    cpuModel: cpus()[0]?.model ?? "unknown",
    availableParallelism: availableParallelism(),
    endpoint: endpoint.origin,
    corpus: {
      path: corpusPath,
      manifestPath,
      sha256: corpusSha256,
      expectedSha256: manifest.files.corpus.sha256,
      manifestRowCount: manifest.files.corpus.rowCount,
      rowsRead: cursor.rowsRead,
      verificationMode: shortProof
        ? "short-proof-manifest-plus-per-row-sha256"
        : "full-file-sha256-plus-per-row-sha256",
    },
    topology,
    configuration: { connections, maxInFlight },
    warmup,
    measured,
  };
  writeFileSync(reportPath, `${JSON.stringify(report, null, 2)}\n`);
  process.stdout.write(`${JSON.stringify(report)}\n`);
} finally {
  cursor.close();
  await pool.close();
}
