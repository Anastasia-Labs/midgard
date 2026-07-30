import { createHash } from "node:crypto";
import { performance } from "node:perf_hooks";

import { encodeMidgardProofSubmissionV1 } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";

export type OpenLoopCorpusShape = "fanout" | "chain" | "mixed";

export type OpenLoopWorkloadProfile =
  | "synthetic-admission"
  | "production-end-user";

export type OpenLoopCorpusRow = {
  readonly txHash: string;
  readonly canonicalCborHex: string;
  readonly canonicalCborSha256: string;
  readonly canonicalCborByteLength: number;
  readonly senderWalletId: string;
  readonly selectedInputOutref: string;
  readonly outputOutrefs: readonly string[];
  readonly planShape: OpenLoopCorpusShape;
  readonly parentTxHash: string | null;
  readonly corpusSliceId: string;
};

export type OpenLoopCorpusPlan = {
  readonly rows: readonly OpenLoopCorpusRow[];
  readonly requiredTransactionCount: number;
  readonly selectedTransactionCount: number;
  readonly corpusShape: OpenLoopCorpusShape;
  readonly corpusSliceId: string;
};

export type OpenLoopPlacementProof = {
  readonly processPid: number;
  readonly cwd: string;
  readonly insideMidgardNodeProcess: boolean;
  readonly insideMidgardNodeContainer: boolean;
  readonly validForUpperBoundClaim: boolean;
  readonly notes: readonly string[];
};

export type OpenLoopSubmitRecord = {
  readonly txHash: string;
  readonly scheduledAtMs: number;
  readonly submittedAtMs: number;
  readonly scheduleSlipMs: number;
  readonly latencyMs: number;
  readonly statusCode: number | null;
  readonly responseTxId: string | null;
  readonly error: string | null;
};

export type OpenLoopSubmitSummary = {
  readonly offeredCount: number;
  readonly submittedCount: number;
  readonly failedCount: number;
  readonly targetRateTps: number;
  readonly maxInFlight: number;
  readonly maxObservedInFlight: number;
  readonly startedAtIso: string;
  readonly finishedAtIso: string;
  readonly durationMs: number;
  readonly achievedRateTps: number;
  readonly submittedOfferedRatio: number;
  readonly scheduleSlipMs: {
    readonly p50: number;
    readonly p95: number;
    readonly p99: number;
    readonly max: number;
  };
};

export type NoOpCalibrationSummary = OpenLoopSubmitSummary & {
  readonly endpoint: string;
  readonly minRequiredRateTps: number;
  readonly p95ScheduleSlipLimitMs: number;
  readonly p99ScheduleSlipLimitMs: number;
  readonly passed: boolean;
  readonly eventLoopUtilization?: number;
  readonly cpuUserMicros: number;
  readonly cpuSystemMicros: number;
  readonly notes: readonly string[];
};

const TX_HASH_PATTERN = /^[0-9a-f]{64}$/;
const SHA256_PATTERN = /^[0-9a-f]{64}$/;
const OUTREF_PATTERN = /^[0-9a-f]{64}#(0|[1-9][0-9]*)$/;
const CORPUS_ROW_KEYS = [
  "txHash",
  "canonicalCborHex",
  "canonicalCborSha256",
  "canonicalCborByteLength",
  "senderWalletId",
  "selectedInputOutref",
  "outputOutrefs",
  "planShape",
  "parentTxHash",
  "corpusSliceId",
] as const;

const normalizeHex = (value: string): string => value.trim().toLowerCase();

const sha256Hex = (bytes: Buffer): string =>
  createHash("sha256").update(bytes).digest("hex");

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const percentile = (values: readonly number[], quantile: number): number => {
  if (values.length === 0) {
    return 0;
  }
  const sorted = [...values].sort((left, right) => left - right);
  const index = Math.min(
    sorted.length - 1,
    Math.max(0, Math.ceil(sorted.length * quantile) - 1),
  );
  return sorted[index]!;
};

const parseStringField = (
  input: Record<string, unknown>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error(`corpus row ${name} must be a non-empty string`);
  }
  if (value !== value.trim()) {
    throw new Error(`corpus row ${name} must be an exact non-empty string`);
  }
  return value;
};

const parseOutputOutrefs = (
  input: Record<string, unknown>,
): readonly string[] => {
  const value = input.outputOutrefs;
  if (
    !Array.isArray(value) ||
    value.some(
      (entry) =>
        typeof entry !== "string" ||
        entry.length === 0 ||
        entry !== entry.trim(),
    )
  ) {
    throw new Error(
      "corpus row outputOutrefs must be an array of exact non-empty strings",
    );
  }
  return value;
};

const parsePlanShape = (value: string): OpenLoopCorpusShape => {
  if (value === "fanout" || value === "chain" || value === "mixed") {
    return value;
  }
  throw new Error(`unsupported corpus planShape ${value}`);
};

export const parseOpenLoopCorpusLine = (
  line: string,
  index: number,
): OpenLoopCorpusRow => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(line) as unknown;
  } catch (error) {
    throw new Error(
      `invalid JSON in corpus row ${index.toString()}: ${errorMessage(error)}`,
    );
  }
  if (typeof parsed !== "object" || parsed === null) {
    throw new Error(`corpus row ${index.toString()} must be an object`);
  }
  const input = parsed as Record<string, unknown>;
  const keys = Object.keys(input);
  const extra = keys.filter(
    (key) => !CORPUS_ROW_KEYS.includes(key as (typeof CORPUS_ROW_KEYS)[number]),
  );
  const missing = CORPUS_ROW_KEYS.filter((key) => !Object.hasOwn(input, key));
  if (missing.length > 0 || extra.length > 0) {
    throw new Error(
      `corpus row ${index.toString()} keys must be exact; missing=[${missing.join(",")}], extra=[${extra.join(",")}]`,
    );
  }
  for (const field of [
    "txHash",
    "canonicalCborHex",
    "canonicalCborSha256",
  ] as const) {
    const value = input[field];
    if (typeof value !== "string" || value !== value.trim().toLowerCase()) {
      throw new Error(
        `corpus row ${index.toString()} ${field} must use exact lowercase encoding`,
      );
    }
  }
  const txHash = normalizeHex(parseStringField(input, "txHash"));
  const canonicalCborHex = normalizeHex(
    parseStringField(input, "canonicalCborHex"),
  );
  const canonicalCborSha256 = normalizeHex(
    parseStringField(input, "canonicalCborSha256"),
  );
  const canonicalCborBytes = Buffer.from(canonicalCborHex, "hex");
  const byteLength = input.canonicalCborByteLength;
  if (!TX_HASH_PATTERN.test(txHash)) {
    throw new Error(
      `corpus row ${index.toString()} txHash must be 32-byte hex`,
    );
  }
  if (
    canonicalCborHex.length === 0 ||
    canonicalCborHex.length % 2 !== 0 ||
    canonicalCborBytes.toString("hex") !== canonicalCborHex
  ) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborHex must be hex`,
    );
  }
  if (!SHA256_PATTERN.test(canonicalCborSha256)) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborSha256 must be 32-byte hex`,
    );
  }
  if (sha256Hex(canonicalCborBytes) !== canonicalCborSha256) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborSha256 does not match canonicalCborHex`,
    );
  }
  if (
    typeof byteLength !== "number" ||
    !Number.isSafeInteger(byteLength) ||
    byteLength !== canonicalCborBytes.length
  ) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborByteLength does not match canonicalCborHex`,
    );
  }
  let outputCount: number;
  let computedTxHash: string;
  try {
    const nativeTx =
      decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCborBytes);
    computedTxHash = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
    outputCount = decodeMidgardNativeByteListPreimage(
      nativeTx.body.outputsPreimageCbor,
      `corpus row ${index.toString()} outputs`,
    ).length;
  } catch (cause) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborHex must be canonical Midgard native V1 transaction CBOR: ${errorMessage(cause)}`,
    );
  }
  if (computedTxHash !== txHash) {
    throw new Error(
      `corpus row ${index.toString()} txHash does not bind canonicalCborHex`,
    );
  }

  const rawParentTxHash = input.parentTxHash;
  if (rawParentTxHash !== null && typeof rawParentTxHash !== "string") {
    throw new Error(
      `corpus row ${index.toString()} parentTxHash must be null or 32-byte hex`,
    );
  }
  const parentTxHash =
    rawParentTxHash === null ? null : normalizeHex(rawParentTxHash);
  if (
    typeof rawParentTxHash === "string" &&
    rawParentTxHash !== rawParentTxHash.trim().toLowerCase()
  ) {
    throw new Error(
      `corpus row ${index.toString()} parentTxHash must use exact lowercase encoding`,
    );
  }
  if (parentTxHash !== null && !TX_HASH_PATTERN.test(parentTxHash)) {
    throw new Error(
      `corpus row ${index.toString()} parentTxHash must be null or 32-byte hex`,
    );
  }

  const senderWalletId = parseStringField(input, "senderWalletId");
  const selectedInputOutref = parseStringField(input, "selectedInputOutref");
  if (!OUTREF_PATTERN.test(selectedInputOutref)) {
    throw new Error(
      `corpus row ${index.toString()} selectedInputOutref must be canonical <64hex>#<index>`,
    );
  }
  const outputOutrefs = parseOutputOutrefs(input);
  if (
    outputOutrefs.length !== outputCount ||
    outputOutrefs.some(
      (outref, outputIndex) => outref !== `${txHash}#${outputIndex.toString()}`,
    )
  ) {
    throw new Error(
      `corpus row ${index.toString()} outputOutrefs must exactly enumerate canonicalCborHex outputs`,
    );
  }

  return {
    txHash,
    canonicalCborHex,
    canonicalCborSha256,
    canonicalCborByteLength: byteLength,
    senderWalletId,
    selectedInputOutref,
    outputOutrefs,
    planShape: parsePlanShape(parseStringField(input, "planShape")),
    parentTxHash,
    corpusSliceId: parseStringField(input, "corpusSliceId"),
  };
};

export const parseOpenLoopCorpusNdjson = (
  raw: string,
): readonly OpenLoopCorpusRow[] =>
  raw
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => parseOpenLoopCorpusLine(line, index + 1));

export const planOpenLoopCorpus = ({
  rows,
  targetRateTps,
  durationMs,
  warmupCount,
  cooldownCount,
  corpusShape,
  corpusSliceId,
}: {
  readonly rows: readonly OpenLoopCorpusRow[];
  readonly targetRateTps: number;
  readonly durationMs: number;
  readonly warmupCount: number;
  readonly cooldownCount: number;
  readonly corpusShape: OpenLoopCorpusShape;
  readonly corpusSliceId: string;
}): OpenLoopCorpusPlan => {
  if (!Number.isFinite(targetRateTps) || targetRateTps <= 0) {
    throw new Error("open-loop targetRateTps must be positive");
  }
  if (!Number.isSafeInteger(durationMs) || durationMs <= 0) {
    throw new Error("open-loop durationMs must be a positive safe integer");
  }
  const requiredTransactionCount =
    Math.ceil((targetRateTps * durationMs) / 1000) +
    Math.max(0, warmupCount) +
    Math.max(0, cooldownCount);
  const matchingRows = rows.filter(
    (row) =>
      row.planShape === corpusShape && row.corpusSliceId === corpusSliceId,
  );
  if (matchingRows.length < requiredTransactionCount) {
    throw new Error(
      `corpus slice ${corpusSliceId} has ${matchingRows.length.toString()} ${corpusShape} txs, need ${requiredTransactionCount.toString()}`,
    );
  }
  const selectedRows = matchingRows.slice(0, requiredTransactionCount);
  const seenInputs = new Map<string, string>();
  for (const row of selectedRows) {
    const existing = seenInputs.get(row.selectedInputOutref);
    if (existing !== undefined) {
      throw new Error(
        `duplicate selected input ${row.selectedInputOutref} in corpus slice ${corpusSliceId} (${existing}, ${row.txHash})`,
      );
    }
    seenInputs.set(row.selectedInputOutref, row.txHash);
  }
  return {
    rows: selectedRows,
    requiredTransactionCount,
    selectedTransactionCount: selectedRows.length,
    corpusShape,
    corpusSliceId,
  };
};

export const buildOpenLoopPlacementProof = ({
  cwd = process.cwd(),
  env = process.env,
}: {
  readonly cwd?: string;
  readonly env?: NodeJS.ProcessEnv;
} = {}): OpenLoopPlacementProof => {
  const insideMidgardNodeProcess =
    process.argv.some((arg) => arg.includes("listen")) ||
    process.argv.some((arg) => arg.includes("dist/index.js"));
  const insideMidgardNodeContainer =
    env.MIDGARD_NODE_CONTAINER === "1" ||
    env.HOSTNAME?.includes("midgard-node") === true;
  const notes = [
    ...(insideMidgardNodeProcess ? ["load_generator_inside_node_process"] : []),
    ...(insideMidgardNodeContainer
      ? ["load_generator_inside_midgard_node_container"]
      : []),
  ];
  return {
    processPid: process.pid,
    cwd,
    insideMidgardNodeProcess,
    insideMidgardNodeContainer,
    validForUpperBoundClaim: notes.length === 0,
    notes,
  };
};

export const summarizeOpenLoopSubmissions = ({
  records,
  offeredCount,
  targetRateTps,
  maxInFlight,
  maxObservedInFlight,
  startedAtMs,
  finishedAtMs,
  startedAtIso,
  finishedAtIso,
}: {
  readonly records: readonly OpenLoopSubmitRecord[];
  readonly offeredCount: number;
  readonly targetRateTps: number;
  readonly maxInFlight: number;
  readonly maxObservedInFlight: number;
  readonly startedAtMs: number;
  readonly finishedAtMs: number;
  readonly startedAtIso: string;
  readonly finishedAtIso: string;
}): OpenLoopSubmitSummary => {
  const submittedCount = records.filter(
    (record) => record.statusCode !== null && record.error === null,
  ).length;
  const failedCount = records.length - submittedCount;
  const durationMs = Math.max(1, finishedAtMs - startedAtMs);
  const scheduleSlips = records.map((record) => record.scheduleSlipMs);
  return {
    offeredCount,
    submittedCount,
    failedCount,
    targetRateTps,
    maxInFlight,
    maxObservedInFlight,
    startedAtIso,
    finishedAtIso,
    durationMs,
    achievedRateTps: submittedCount / (durationMs / 1000),
    submittedOfferedRatio:
      offeredCount === 0 ? 0 : submittedCount / offeredCount,
    scheduleSlipMs: {
      p50: percentile(scheduleSlips, 0.5),
      p95: percentile(scheduleSlips, 0.95),
      p99: percentile(scheduleSlips, 0.99),
      max: scheduleSlips.length === 0 ? 0 : Math.max(...scheduleSlips),
    },
  };
};

export const runOpenLoopSubmitter = async ({
  rows,
  endpoint,
  targetRateTps,
  maxInFlight,
  fetchImpl,
  now = () => new Date(),
  performanceNow = () => performance.now(),
  sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms)),
  signal,
}: {
  readonly rows: readonly OpenLoopCorpusRow[];
  readonly endpoint: string;
  readonly targetRateTps: number;
  readonly maxInFlight: number;
  readonly fetchImpl: typeof fetch;
  readonly now?: () => Date;
  readonly performanceNow?: () => number;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly signal?: AbortSignal;
}): Promise<{
  readonly records: readonly OpenLoopSubmitRecord[];
  readonly summary: OpenLoopSubmitSummary;
}> => {
  if (!Number.isFinite(targetRateTps) || targetRateTps <= 0) {
    throw new Error("open-loop targetRateTps must be positive");
  }
  const boundedMaxInFlight = Math.max(1, Math.floor(maxInFlight));
  const startedAtDate = now();
  const startedAtMs = startedAtDate.getTime();
  const startedPerfMs = performanceNow();
  const intervalMs = 1000 / targetRateTps;
  const inFlight = new Set<Promise<void>>();
  const records: OpenLoopSubmitRecord[] = [];
  let maxObservedInFlight = 0;

  const waitForInFlight = async (): Promise<void> => {
    if (inFlight.size === 0) {
      return;
    }
    await Promise.race(inFlight);
  };

  for (const [index, row] of rows.entries()) {
    if (signal?.aborted === true) {
      break;
    }
    while (inFlight.size >= boundedMaxInFlight) {
      await waitForInFlight();
    }
    const scheduledPerfMs = startedPerfMs + index * intervalMs;
    const waitMs = scheduledPerfMs - performanceNow();
    if (waitMs > 0) {
      await sleep(waitMs);
    }

    const promise = (async () => {
      const submittedPerfMs = performanceNow();
      const submittedAtMs = Date.now();
      const body = encodeMidgardProofSubmissionV1({
        transactionCbor: Buffer.from(row.canonicalCborHex, "hex"),
        programMaterial: [],
      });
      try {
        const response = await fetchImpl(endpoint, {
          method: "POST",
          headers: { "content-type": "application/vnd.midgard.v1+cbor" },
          body,
          signal,
        });
        let responseTxId: string | null = null;
        try {
          const parsed = (await response.json()) as unknown;
          if (typeof parsed === "object" && parsed !== null) {
            const txId = (parsed as { readonly txId?: unknown }).txId;
            responseTxId = typeof txId === "string" ? txId : null;
          }
        } catch {
          responseTxId = null;
        }
        if (responseTxId !== null && responseTxId !== row.txHash) {
          records.push({
            txHash: row.txHash,
            scheduledAtMs: startedAtMs + index * intervalMs,
            submittedAtMs,
            scheduleSlipMs: Math.max(0, submittedPerfMs - scheduledPerfMs),
            latencyMs: Math.max(0, performanceNow() - submittedPerfMs),
            statusCode: response.status,
            responseTxId,
            error: `response txId ${responseTxId} did not match corpus txHash ${row.txHash}`,
          });
          return;
        }
        records.push({
          txHash: row.txHash,
          scheduledAtMs: startedAtMs + index * intervalMs,
          submittedAtMs,
          scheduleSlipMs: Math.max(0, submittedPerfMs - scheduledPerfMs),
          latencyMs: Math.max(0, performanceNow() - submittedPerfMs),
          statusCode: response.status,
          responseTxId,
          error: null,
        });
      } catch (error) {
        records.push({
          txHash: row.txHash,
          scheduledAtMs: startedAtMs + index * intervalMs,
          submittedAtMs,
          scheduleSlipMs: Math.max(0, submittedPerfMs - scheduledPerfMs),
          latencyMs: Math.max(0, performanceNow() - submittedPerfMs),
          statusCode: null,
          responseTxId: null,
          error: errorMessage(error),
        });
      }
    })().finally(() => {
      inFlight.delete(promise);
    });
    inFlight.add(promise);
    maxObservedInFlight = Math.max(maxObservedInFlight, inFlight.size);
  }

  await Promise.all(inFlight);
  const finishedAtDate = now();
  const finishedAtMs = finishedAtDate.getTime();
  return {
    records,
    summary: summarizeOpenLoopSubmissions({
      records,
      offeredCount: rows.length,
      targetRateTps,
      maxInFlight: boundedMaxInFlight,
      maxObservedInFlight,
      startedAtMs,
      finishedAtMs,
      startedAtIso: startedAtDate.toISOString(),
      finishedAtIso: finishedAtDate.toISOString(),
    }),
  };
};

export const runNoOpSubmitCalibration = async ({
  endpoint,
  rows,
  targetRateTps,
  durationMs,
  maxInFlight,
  minRateMultiplier = 2,
  fetchImpl,
  signal,
}: {
  readonly endpoint: string;
  readonly rows: readonly OpenLoopCorpusRow[];
  readonly targetRateTps: number;
  readonly durationMs: number;
  readonly maxInFlight: number;
  readonly minRateMultiplier?: number;
  readonly fetchImpl: typeof fetch;
  readonly signal?: AbortSignal;
}): Promise<NoOpCalibrationSummary> => {
  const calibrationRate = targetRateTps * Math.max(1, minRateMultiplier);
  const count = Math.max(1, Math.ceil((calibrationRate * durationMs) / 1000));
  const calibrationRows = Array.from(
    { length: count },
    (_unused, index) => rows[index % rows.length]!,
  );
  const cpuBefore = process.cpuUsage();
  const eluBefore = performance.eventLoopUtilization?.();
  const result = await runOpenLoopSubmitter({
    rows: calibrationRows,
    endpoint,
    targetRateTps: calibrationRate,
    maxInFlight,
    fetchImpl,
    signal,
  });
  const cpuAfter = process.cpuUsage(cpuBefore);
  const eluAfter =
    eluBefore === undefined
      ? undefined
      : performance.eventLoopUtilization(eluBefore).utilization;
  const minRequiredRateTps = targetRateTps * minRateMultiplier;
  const p95ScheduleSlipLimitMs = 100;
  const p99ScheduleSlipLimitMs = 250;
  const passed =
    result.summary.achievedRateTps >= minRequiredRateTps &&
    result.summary.scheduleSlipMs.p95 <= p95ScheduleSlipLimitMs &&
    result.summary.scheduleSlipMs.p99 <= p99ScheduleSlipLimitMs;
  return {
    ...result.summary,
    endpoint,
    minRequiredRateTps,
    p95ScheduleSlipLimitMs,
    p99ScheduleSlipLimitMs,
    passed,
    ...(eluAfter === undefined ? {} : { eventLoopUtilization: eluAfter }),
    cpuUserMicros: cpuAfter.user,
    cpuSystemMicros: cpuAfter.system,
    notes: passed ? [] : ["no_op_calibration_gate_failed"],
  };
};
