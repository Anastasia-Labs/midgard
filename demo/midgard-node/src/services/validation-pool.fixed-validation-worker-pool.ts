import { Worker } from "node:worker_threads";

import { Effect, Metric } from "effect";

import {
  packPhaseAJob,
  type ValidationJobRequest,
  type ValidationWorkerInit,
  type ValidationWorkerResponse,
} from "../workers/utils/validation-pool.js";
import {
  BUNDLED_WORKER_EXEC_ARGV,
  type PendingJob,
  restartCounter,
  timeoutCounter,
  ValidationWorkerError,
  type WorkerSlot,
} from "./validation-pool.bundled-worker-exec-argv.js";

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
    const worker = new Worker(this.entry, {
      workerData: this.init,
      execArgv: [...BUNDLED_WORKER_EXEC_ARGV],
    });
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
