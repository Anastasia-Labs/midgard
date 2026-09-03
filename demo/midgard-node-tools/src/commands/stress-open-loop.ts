import { performance } from "node:perf_hooks";

import { encodeMidgardProofSubmissionV1 } from "@al-ft/midgard-core/cek-proof";
import type {
  OpenLoopCorpusRow,
  OpenLoopCorpusShape,
} from "midgard-node/open-loop-corpus-format";
import { percentileOfUnsorted as percentile } from "midgard-node/percentile";

// The row wire format lives on the node side (the mpf-engine-probe worker and
// the stage-B benchmark read corpora too); re-export it so the rest of the
// tooling keeps one import site for corpus rows.
export {
  type OpenLoopCorpusRow,
  type OpenLoopCorpusShape,
  parseOpenLoopCorpusLine,
  parseOpenLoopCorpusNdjson,
} from "midgard-node/open-loop-corpus-format";

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

export type OpenLoopWorkloadProfile =
  | "synthetic-admission"
  | "production-end-user";

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

// The operator binary, as it appears in a process argv on either path
// convention. The load generator is "inside the node process" only when it
// runs from that binary (or as the `listen` command); this tooling package's
// own `dist/index.js` is a separate process by construction and must not
// disqualify an upper-bound claim.
const OPERATOR_BINARY_PATTERN =
  /(?:^|[\\/])midgard-node[\\/]dist[\\/]index\.js$/;

export const buildOpenLoopPlacementProof = ({
  cwd = process.cwd(),
  env = process.env,
  argv = process.argv,
}: {
  readonly cwd?: string;
  readonly env?: NodeJS.ProcessEnv;
  readonly argv?: readonly string[];
} = {}): OpenLoopPlacementProof => {
  const insideMidgardNodeProcess = argv.some(
    (arg) => arg.includes("listen") || OPERATOR_BINARY_PATTERN.test(arg),
  );
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
