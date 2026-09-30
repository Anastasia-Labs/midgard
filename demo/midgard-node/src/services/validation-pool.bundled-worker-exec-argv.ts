import { Worker } from "node:worker_threads";

import {
  type LocalScriptEvalResult,
  type PhaseAConfig,
  type PhaseAResult,
  type QueuedTx,
} from "@al-ft/midgard-validation";
import { Context, Data, Effect, Metric } from "effect";

import {
  type ValidationJobRequest,
  type ValidationWorkerResponse,
} from "../workers/utils/validation-pool.js";

export class ValidationWorkerError extends Data.TaggedError(
  "ValidationWorkerError",
)<{
  readonly message: string;
  readonly cause?: unknown;
}> {}

export type ValidationPoolService = {
  readonly poolSize: number;
  readonly consensusProfile: NonNullable<PhaseAConfig["consensusProfile"]>;
  readonly runPhaseAChunk: (
    txs: readonly QueuedTx[],
  ) => Effect.Effect<PhaseAResult, ValidationWorkerError>;
  readonly evaluateScript: (
    scriptBytes: Uint8Array,
    contextCbor: Uint8Array,
  ) => Effect.Effect<LocalScriptEvalResult, ValidationWorkerError>;
  readonly ready: Effect.Effect<void, ValidationWorkerError>;
  readonly stats: Effect.Effect<{
    readonly busyWorkers: number;
    readonly queueDepth: number;
    readonly oldestInFlightAgeMs: number;
    readonly liveWorkers: number;
    readonly restartingWorkers: number;
  }>;
};

export class ValidationPool extends Context.Tag("ValidationPool")<
  ValidationPool,
  ValidationPoolService
>() {}

export type PendingJob = {
  readonly request: ValidationJobRequest;
  readonly transferList: readonly ArrayBuffer[];
  readonly resolve: (response: ValidationWorkerResponse) => void;
  readonly reject: (error: ValidationWorkerError) => void;
};

export type WorkerSlot = {
  readonly index: number;
  worker: Worker;
  inFlight: PendingJob | null;
  timeout: NodeJS.Timeout | null;
  restartAttempt: number;
  restarting: boolean;
  inFlightStartedAt: number;
};

export const poolSizeGauge = Metric.gauge("validation_pool_size", {
  description: "Configured long-lived validation worker count",
});

export const busyWorkersGauge = Metric.gauge("validation_pool_busy_workers", {
  description: "Validation workers with an in-flight job",
});

export const queueDepthGauge = Metric.gauge("validation_pool_queue_depth", {
  description: "Validation jobs waiting for a worker",
});

export const phaseAJobDuration = Metric.timer(
  "validation_pool_phase_a_job_duration",
  "Duration of Phase A worker jobs",
);

export const uplcJobDuration = Metric.timer(
  "validation_pool_uplc_job_duration",
  "Duration of UPLC worker jobs",
);

export const serializeDuration = Metric.timer(
  "validation_pool_serialize_duration",
  "Duration of worker request serialization",
);

export const deserializeDuration = Metric.timer(
  "validation_pool_deserialize_duration",
  "Duration of worker result deserialization",
);

export const restartCounter = Metric.counter(
  "validation_worker_restart_count",
  {
    description: "Validation worker restarts after failure",
    bigint: true,
    incremental: true,
  },
);

export const timeoutCounter = Metric.counter(
  "validation_worker_job_timeout_count",
  {
    description: "Validation worker jobs terminated after timeout",
    bigint: true,
    incremental: true,
  },
);

/**
 * A bundled worker entry starts with no Node arguments of its own.
 *
 * A worker thread otherwise inherits `process.execArgv`, and the entry this
 * pool spawns is always a BUILT bundle beside `dist/` — never TypeScript.
 * Under Vitest the parent carries `--conditions midgard-source`, the workspace
 * condition that resolves `@al-ft/*` imports to sibling *source* so a stale
 * dist can never shape a test result. That is right for the modules Vite
 * transforms and wrong for this worker: plain Node loads the bundle, follows
 * its external `@al-ft/midgard-validation` import into `src/index.ts`, and
 * fails on the first `./cek-builtin.js` specifier, because rewriting a `.js`
 * specifier onto a `.ts` file is the bundler's job and no bundler is in the
 * loop.
 *
 * Passing the list explicitly rather than filtering the inherited one is what
 * `worker_threads` allows: it validates an explicit `execArgv` against the
 * per-thread-permitted flags and rejects V8 options such as the
 * `--max-old-space-size` the test pool sets, which a worker could not honour
 * anyway — the heap ceiling belongs to the process, not the thread. In
 * production `node dist/index.js` runs with no execArgv at all, so this is
 * exactly what the worker already inherited.
 */
export const BUNDLED_WORKER_EXEC_ARGV: readonly string[] = [];
