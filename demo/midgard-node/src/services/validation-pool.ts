import { Worker } from "node:worker_threads";

import {
  deserializePhaseACandidate,
  type LocalScriptEvalResult,
  type PhaseAConfig,
  type PhaseAResult,
  type QueuedTx,
  type RejectedTx,
} from "@al-ft/midgard-validation";
import { Context, Data, Duration, Effect, Layer, Metric } from "effect";

import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import {
  copyToTransferable,
  packPhaseAJob,
  type ValidationJobRequest,
  type ValidationWorkerInit,
  type ValidationWorkerResponse,
} from "../workers/utils/validation-pool.js";
import { NodeConfig } from "./config.js";
import { ContractDeploymentIdentity } from "./midgard-contracts.js";

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

type PendingJob = {
  readonly request: ValidationJobRequest;
  readonly transferList: readonly ArrayBuffer[];
  readonly resolve: (response: ValidationWorkerResponse) => void;
  readonly reject: (error: ValidationWorkerError) => void;
};

type WorkerSlot = {
  readonly index: number;
  worker: Worker;
  inFlight: PendingJob | null;
  timeout: NodeJS.Timeout | null;
  restartAttempt: number;
  restarting: boolean;
  inFlightStartedAt: number;
};

const poolSizeGauge = Metric.gauge("validation_pool_size", {
  description: "Configured long-lived validation worker count",
});
const busyWorkersGauge = Metric.gauge("validation_pool_busy_workers", {
  description: "Validation workers with an in-flight job",
});
const queueDepthGauge = Metric.gauge("validation_pool_queue_depth", {
  description: "Validation jobs waiting for a worker",
});
const phaseAJobDuration = Metric.timer(
  "validation_pool_phase_a_job_duration",
  "Duration of Phase A worker jobs",
);
const uplcJobDuration = Metric.timer(
  "validation_pool_uplc_job_duration",
  "Duration of UPLC worker jobs",
);
const serializeDuration = Metric.timer(
  "validation_pool_serialize_duration",
  "Duration of worker request serialization",
);
const deserializeDuration = Metric.timer(
  "validation_pool_deserialize_duration",
  "Duration of worker result deserialization",
);
const restartCounter = Metric.counter("validation_worker_restart_count", {
  description: "Validation worker restarts after failure",
  bigint: true,
  incremental: true,
});
const timeoutCounter = Metric.counter("validation_worker_job_timeout_count", {
  description: "Validation worker jobs terminated after timeout",
  bigint: true,
  incremental: true,
});

export class FixedValidationWorkerPool {
  readonly slots: WorkerSlot[] = [];
  readonly queue: PendingJob[] = [];
  readonly capacityWaiters: Array<() => void> = [];
  readonly restartTimers = new Set<NodeJS.Timeout>();
  nextJobId = 1;
  closed = false;
  started = false;
  startPromise: Promise<void> | null = null;
  readonly cpuProfileHandles = new Map<
    number,
    {
      readonly threadId: number;
      readonly stop: () => Promise<string>;
    }
  >();

  constructor(
    readonly size: number,
    readonly queueCapacity: number,
    readonly timeoutMs: number,
    readonly entry: URL,
    readonly init: ValidationWorkerInit,
  ) {}

  start(): Promise<void> {
    if (this.started) return Promise.resolve();
    if (this.startPromise !== null) return this.startPromise;
    this.startPromise = this.startOnce();
    return this.startPromise;
  }

  isClosed(): boolean {
    return this.closed;
  }

  async startCpuProfiles(): Promise<void> {
    if (!this.started) {
      throw new Error(
        "validation worker pool must be started before profiling",
      );
    }
    if (this.cpuProfileHandles.size > 0) {
      throw new Error("validation worker CPU profiling is already active");
    }
    await Promise.all(
      this.slots.map(async (slot) => {
        const profileWorker = slot.worker as Worker & {
          readonly startCpuProfile?: (name: string) => Promise<{
            readonly stop: () => Promise<string>;
          }>;
        };
        if (profileWorker.startCpuProfile === undefined) {
          throw new Error(
            "Node runtime does not expose Worker.startCpuProfile",
          );
        }
        const handle = await profileWorker.startCpuProfile(
          `validation-worker-${slot.index.toString()}`,
        );
        this.cpuProfileHandles.set(slot.index, {
          threadId: slot.worker.threadId,
          stop: handle.stop.bind(handle),
        });
      }),
    );
  }

  async stopCpuProfiles(): Promise<
    readonly {
      readonly workerIndex: number;
      readonly threadId: number;
      readonly profileJson: string;
    }[]
  > {
    const handles = [...this.cpuProfileHandles];
    this.cpuProfileHandles.clear();
    return await Promise.all(
      handles.map(async ([workerIndex, profile]) => ({
        workerIndex,
        threadId: profile.threadId,
        profileJson: await profile.stop(),
      })),
    );
  }

  private async startOnce(): Promise<void> {
    try {
      for (let index = 0; index < this.size; index += 1) {
        this.slots.push(this.spawnSlot(index));
      }
      await Promise.all(
        Array.from({ length: this.size }, () =>
          this.submit(packPhaseAJob(this.allocateJobId(), [])),
        ),
      );
      this.started = true;
    } catch (error) {
      await this.close();
      throw error;
    }
  }

  allocateJobId(): number {
    const id = this.nextJobId;
    this.nextJobId = this.nextJobId === Number.MAX_SAFE_INTEGER ? 1 : id + 1;
    return id;
  }

  stats(): {
    readonly busyWorkers: number;
    readonly queueDepth: number;
    readonly oldestInFlightAgeMs: number;
    readonly liveWorkers: number;
    readonly restartingWorkers: number;
  } {
    const now = Date.now();
    return {
      busyWorkers: this.slots.filter((slot) => slot.inFlight !== null).length,
      queueDepth: this.queue.length,
      oldestInFlightAgeMs: this.slots.reduce(
        (oldest, slot) =>
          slot.inFlight === null
            ? oldest
            : Math.max(oldest, now - slot.inFlightStartedAt),
        0,
      ),
      liveWorkers: this.slots.filter((slot) => !slot.restarting).length,
      restartingWorkers: this.slots.filter((slot) => slot.restarting).length,
    };
  }

  async workerMemoryStatistics(): Promise<
    readonly {
      readonly workerIndex: number;
      readonly threadId: number;
      readonly usedHeapBytes: number;
      readonly externalBytes: number;
      readonly comparableFootprintBytes: number;
    }[]
  > {
    if (!this.started || this.closed) {
      throw new ValidationWorkerError({
        message: "validation pool must be running to sample worker memory",
      });
    }
    return await Promise.all(
      this.slots.map(async (slot) => {
        if (slot.restarting || slot.worker.threadId < 0) {
          throw new ValidationWorkerError({
            message: `validation worker ${slot.index.toString()} is unavailable during memory sampling`,
          });
        }
        const threadId = slot.worker.threadId;
        const heap = await slot.worker.getHeapStatistics();
        return {
          workerIndex: slot.index,
          threadId,
          usedHeapBytes: heap.used_heap_size,
          externalBytes: heap.external_memory,
          comparableFootprintBytes: heap.used_heap_size + heap.external_memory,
        };
      }),
    );
  }

  terminateWorker(index: number): Promise<number> {
    const slot = this.slots[index];
    if (slot === undefined) {
      return Promise.reject(new Error(`validation worker ${index} not found`));
    }
    return slot.worker.terminate();
  }

  async submit(
    request: ValidationJobRequest,
  ): Promise<ValidationWorkerResponse> {
    while (!this.closed && this.queue.length >= this.queueCapacity) {
      await new Promise<void>((resolve) => this.capacityWaiters.push(resolve));
    }
    if (this.closed) {
      throw new ValidationWorkerError({ message: "validation pool is closed" });
    }
    const transferList =
      request.kind === "phase_a"
        ? [request.arena]
        : [request.scriptBytes, request.contextCbor];
    return new Promise<ValidationWorkerResponse>((resolve, reject) => {
      this.queue.push({ request, transferList, resolve, reject });
      this.dispatch();
    });
  }

  async close(): Promise<void> {
    if (this.closed) return;
    this.closed = true;
    if (this.cpuProfileHandles.size > 0) {
      await this.stopCpuProfiles();
    }
    for (const timer of this.restartTimers) clearTimeout(timer);
    this.restartTimers.clear();
    const error = new ValidationWorkerError({
      message: "validation pool shut down",
    });
    for (const job of this.queue.splice(0)) job.reject(error);
    for (const resolve of this.capacityWaiters.splice(0)) resolve();
    await Promise.all(
      this.slots.map(async (slot) => {
        if (slot.timeout !== null) clearTimeout(slot.timeout);
        slot.inFlight?.reject(error);
        slot.inFlight = null;
        await slot.worker.terminate();
      }),
    );
    this.slots.splice(0);
  }

  private spawnSlot(index: number): WorkerSlot {
    const worker = new Worker(this.entry, { workerData: this.init });
    const slot: WorkerSlot = {
      index,
      worker,
      inFlight: null,
      timeout: null,
      restartAttempt: 0,
      restarting: false,
      inFlightStartedAt: 0,
    };
    worker.on("message", (response: ValidationWorkerResponse) =>
      this.handleResponse(slot, response),
    );
    worker.on("error", (cause) => this.failSlot(slot, cause));
    worker.on("exit", (code) => {
      if (!this.closed && !slot.restarting) {
        this.failSlot(
          slot,
          new Error(`validation worker exited with code ${code}`),
        );
      }
      if (!this.closed) this.scheduleRespawn(slot);
    });
    return slot;
  }

  private dispatch(): void {
    for (const slot of this.slots) {
      if (slot.inFlight !== null || slot.restarting) continue;
      const job = this.queue.shift();
      if (job === undefined) break;
      this.capacityWaiters.shift()?.();
      slot.inFlight = job;
      slot.inFlightStartedAt = Date.now();
      slot.timeout = setTimeout(() => {
        void Metric.increment(timeoutCounter).pipe(Effect.runPromise);
        this.failSlot(
          slot,
          new Error(
            `validation job ${job.request.jobId} timed out after ${this.timeoutMs}ms`,
          ),
        );
        void slot.worker.terminate();
      }, this.timeoutMs);
      slot.worker.postMessage(job.request, [...job.transferList]);
    }
  }

  private handleResponse(
    slot: WorkerSlot,
    response: ValidationWorkerResponse,
  ): void {
    const job = slot.inFlight;
    if (job === null || response.jobId !== job.request.jobId) {
      this.failSlot(
        slot,
        new Error(
          `unexpected validation worker response jobId=${response.jobId}`,
        ),
      );
      void slot.worker.terminate();
      return;
    }
    if (response.kind === "job_failed") {
      this.failSlot(slot, new Error(response.error));
      void slot.worker.terminate();
      return;
    }
    this.finishSlot(slot);
    job.resolve(response);
  }

  private finishSlot(slot: WorkerSlot): void {
    if (slot.timeout !== null) clearTimeout(slot.timeout);
    slot.timeout = null;
    slot.inFlight = null;
    slot.inFlightStartedAt = 0;
    slot.restartAttempt = 0;
    this.dispatch();
  }

  private failSlot(slot: WorkerSlot, cause: unknown): void {
    if (slot.restarting || this.closed) return;
    slot.restarting = true;
    if (slot.timeout !== null) clearTimeout(slot.timeout);
    slot.timeout = null;
    slot.inFlight?.reject(
      new ValidationWorkerError({
        message: `validation worker ${slot.index} failed`,
        cause,
      }),
    );
    slot.inFlight = null;
    slot.inFlightStartedAt = 0;
  }

  private scheduleRespawn(slot: WorkerSlot): void {
    if (this.closed) return;
    const delay = Math.min(250 * 2 ** slot.restartAttempt, 5_000);
    slot.restartAttempt += 1;
    const timer = setTimeout(() => {
      this.restartTimers.delete(timer);
      if (this.closed) return;
      const replacement = this.spawnSlot(slot.index);
      replacement.restartAttempt = slot.restartAttempt;
      this.slots[slot.index] = replacement;
      void Metric.increment(restartCounter).pipe(Effect.runPromise);
      this.dispatch();
    }, delay);
    this.restartTimers.add(timer);
  }
}

const makeValidationPool = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const deploymentIdentity = yield* ContractDeploymentIdentity;
  const size = config.VALIDATION_WORKER_POOL_SIZE;
  if (size === 0) {
    return {
      poolSize: 0,
      consensusProfile: deploymentIdentity.consensusProfile,
      runPhaseAChunk: () =>
        Effect.fail(
          new ValidationWorkerError({ message: "validation pool is disabled" }),
        ),
      evaluateScript: () =>
        Effect.fail(
          new ValidationWorkerError({ message: "validation pool is disabled" }),
        ),
      ready: Effect.void,
      stats: Effect.succeed({
        busyWorkers: 0,
        queueDepth: 0,
        oldestInFlightAgeMs: 0,
        liveWorkers: 0,
        restartingWorkers: 0,
      }),
    } satisfies ValidationPoolService;
  }

  const entry = resolveWorkerEntry(import.meta.url, "validation.js");
  const pool = new FixedValidationWorkerPool(
    size,
    size * 4,
    config.VALIDATION_WORKER_JOB_TIMEOUT_MS,
    entry,
    {
      config: {
        expectedNetworkId: config.NETWORK === "Mainnet" ? 1n : 0n,
        minFeeA: config.MIN_FEE_A,
        minFeeB: config.MIN_FEE_B,
        strictnessProfile: config.VALIDATION_STRICTNESS_PROFILE,
        consensusProfile: deploymentIdentity.consensusProfile,
      },
      signatureVerifier: config.VALIDATION_WORKER_NODE_ED25519 ? "node" : "cml",
    },
  );

  const reportStats = Effect.sync(() => pool.stats()).pipe(
    Effect.tap((stats) => busyWorkersGauge(Effect.succeed(stats.busyWorkers))),
    Effect.tap((stats) => queueDepthGauge(Effect.succeed(stats.queueDepth))),
    Effect.asVoid,
  );
  const runJob = (
    request: ValidationJobRequest,
  ): Effect.Effect<ValidationWorkerResponse, ValidationWorkerError> =>
    Effect.tryPromise({
      try: () => pool.submit(request),
      catch: (cause) =>
        cause instanceof ValidationWorkerError
          ? cause
          : new ValidationWorkerError({
              message: "validation worker job failed",
              cause,
            }),
    }).pipe(Effect.ensuring(reportStats));

  const ready = Effect.tryPromise({
    try: () => pool.start(),
    catch: (cause) =>
      cause instanceof ValidationWorkerError
        ? cause
        : new ValidationWorkerError({
            message: "validation worker pool startup failed",
            cause,
          }),
  }).pipe(
    Effect.tap(() => poolSizeGauge(Effect.succeed(size))),
    Effect.ensuring(reportStats),
  );

  const service: ValidationPoolService = {
    poolSize: size,
    consensusProfile: deploymentIdentity.consensusProfile,
    ready,
    stats: Effect.sync(() => pool.stats()),
    runPhaseAChunk: (txs) => {
      const serializeStartedAt = Date.now();
      const request = packPhaseAJob(pool.allocateJobId(), txs);
      const jobStartedAt = Date.now();
      return Metric.update(
        serializeDuration,
        Duration.millis(Date.now() - serializeStartedAt),
      ).pipe(
        Effect.zipRight(
          runJob(request).pipe(
            Effect.flatMap((response) => {
              if (response.kind !== "phase_a") {
                return Effect.fail(
                  new ValidationWorkerError({
                    message: `expected phase_a response, got ${response.kind}`,
                  }),
                );
              }
              const deserializeStartedAt = Date.now();
              const accepted = [];
              const rejected: RejectedTx[] = [];
              for (const result of response.results) {
                if (result.ok) {
                  accepted.push(deserializePhaseACandidate(result.candidate));
                } else {
                  rejected.push({
                    txId: Buffer.from(
                      result.txId.buffer,
                      result.txId.byteOffset,
                      result.txId.byteLength,
                    ),
                    code: result.code,
                    detail: result.detail,
                  });
                }
              }
              return Metric.update(
                deserializeDuration,
                Duration.millis(Date.now() - deserializeStartedAt),
              ).pipe(Effect.as({ accepted, rejected } satisfies PhaseAResult));
            }),
            Effect.ensuring(
              Metric.update(
                phaseAJobDuration,
                Duration.millis(Date.now() - jobStartedAt),
              ).pipe(Effect.asVoid),
            ),
          ),
        ),
      );
    },
    evaluateScript: (scriptBytes, contextCbor) => {
      const request: ValidationJobRequest = {
        kind: "uplc",
        jobId: pool.allocateJobId(),
        scriptBytes: copyToTransferable(scriptBytes),
        contextCbor: copyToTransferable(contextCbor),
      };
      const jobStartedAt = Date.now();
      return runJob(request).pipe(
        Effect.flatMap((response) => {
          if (response.kind !== "uplc") {
            return Effect.fail(
              new ValidationWorkerError({
                message: `expected uplc response, got ${response.kind}`,
              }),
            );
          }
          return Effect.succeed<LocalScriptEvalResult>(
            response.result.ok
              ? {
                  kind: "accepted",
                  budget: {
                    cpu: response.result.cpu,
                    memory: response.result.memory,
                  },
                }
              : { kind: "script_invalid", detail: response.result.detail },
          );
        }),
        Effect.ensuring(
          Metric.update(
            uplcJobDuration,
            Duration.millis(Date.now() - jobStartedAt),
          ).pipe(Effect.asVoid),
        ),
      );
    },
  };
  return yield* Effect.acquireRelease(ready.pipe(Effect.as(service)), () =>
    Effect.promise(() => pool.close()),
  );
});

export const validationPoolLayer = Layer.scoped(
  ValidationPool,
  makeValidationPool,
);
