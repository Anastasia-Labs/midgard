import { Worker } from "node:worker_threads";

import { Effect, Metric } from "effect";

import {
  packPhaseAJob,
  type ValidationJobRequest,
  type ValidationWorkerInit,
  type ValidationWorkerResponse,
} from "../workers/utils/validation-pool.js";
import {
  idleSlot,
  isIdle,
  takeBackgroundJob,
} from "./validation-pool.background-lanes.js";
import {
  type BackgroundJob,
  BUNDLED_WORKER_EXEC_ARGV,
  type ImportCheckLane,
  type PendingJob,
  restartCounter,
  timeoutCounter,
  ValidationWorkerError,
  type WorkerSlot,
} from "./validation-pool.bundled-worker-exec-argv.js";

export class FixedValidationWorkerPool {
  readonly slots: WorkerSlot[] = [];
  readonly queue: PendingJob[] = [];
  /** Import-check jobs, both lanes, in submission order. */
  readonly backgroundQueue: BackgroundJob[] = [];
  /** Workers reserved for the backlog lane: at least one, at most `size`. */
  readonly backlogShare: number;
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
    backlogShare = 1,
  ) {
    this.backlogShare = Math.max(1, Math.min(size, backlogShare));
  }

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
    const transferList = request.kind === "phase_a" ? [request.arena] : [];
    return new Promise<ValidationWorkerResponse>((resolve, reject) => {
      this.queue.push({ request, transferList, resolve, reject });
      this.dispatch();
    });
  }

  /**
   * An import-check job, scheduled by its lane (`takeBackgroundJob`). Its
   * worker is terminated `killMs` after the job starts; a worker not yet
   * started is started first. With `fresh`, an idle worker is replaced and
   * the job runs once the new worker has started, so the kill never counts
   * the worker's start.
   */
  submitBackground(
    request: ValidationJobRequest,
    options: Pick<BackgroundJob, "killMs" | "fresh" | "lane">,
  ): Promise<ValidationWorkerResponse> {
    if (this.closed || !this.started) {
      return Promise.reject(
        new ValidationWorkerError({
          message: "validation pool is not running",
        }),
      );
    }
    return new Promise<ValidationWorkerResponse>((resolve, reject) => {
      this.backgroundQueue.push({
        request,
        transferList: [],
        resolve,
        reject,
        ...options,
      });
      this.dispatch();
    });
  }

  /** Advisory work dispatches only to an idle slot and never enters either production queue. */
  trySubmitAdvisory(
    request: ValidationJobRequest,
  ): Promise<ValidationWorkerResponse> | undefined {
    if (
      !this.started ||
      this.closed ||
      this.size < 2 ||
      this.queue.length > 0 ||
      this.capacityWaiters.length > 0
    )
      return undefined;
    const idle = this.slots.filter(
      (slot) =>
        slot.inFlight === null && !slot.restarting && slot.worker.threadId >= 0,
    );
    if (idle.length <= Math.ceil(this.size / 2)) return undefined;
    const slot = idle[0]!;
    return new Promise<ValidationWorkerResponse>((resolve, reject) => {
      const job: PendingJob = { request, transferList: [], resolve, reject };
      try {
        this.dispatchToSlot(slot, job);
      } catch (cause) {
        this.failSlot(slot, cause);
        void slot.worker.terminate();
      }
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
    for (const job of this.backgroundQueue.splice(0)) job.reject(error);
    for (const slot of this.slots) {
      slot.reservedFor?.reject(error);
      slot.reservedFor = null;
    }
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
      started: false,
      lane: null,
      reservedFor: null,
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
    this.dispatchBackground(false);
    for (const slot of this.slots) {
      if (!isIdle(slot)) continue;
      const job = this.queue.shift();
      if (job === undefined) break;
      this.capacityWaiters.shift()?.();
      this.dispatchToSlot(slot, job);
    }
    this.dispatchBackground(
      this.queue.length === 0 && this.capacityWaiters.length === 0,
    );
  }

  /** Import-check jobs on idle workers: only the backlog lane's reserved
   * share unless `open` (`takeBackgroundJob`). */
  private dispatchBackground(open: boolean): void {
    for (;;) {
      const slot = idleSlot(this.slots);
      const job = slot && takeBackgroundJob(this, open);
      if (slot === undefined || job === undefined) return;
      this.startBackground(slot, job);
    }
  }

  private startBackground(slot: WorkerSlot, job: BackgroundJob): void {
    if (job.fresh) {
      // The exit handler respawns the slot; the replacement starts with a
      // warm-up job and then runs `job`.
      slot.reservedFor = job;
      slot.restarting = true;
      void slot.worker.terminate();
    } else if (!slot.started) {
      // A respawned worker that has not yet started: warm it up first.
      this.warmUpFor(slot, job);
    } else {
      this.dispatchToSlot(slot, job, job.killMs, job.lane);
    }
  }

  /** Starts `slot`'s worker with an empty job, then runs `job` on it. */
  private warmUpFor(slot: WorkerSlot, job: BackgroundJob): void {
    slot.reservedFor = job;
    const warmUp = packPhaseAJob(this.allocateJobId(), []);
    this.dispatchToSlot(slot, {
      request: warmUp,
      transferList: [warmUp.arena],
      resolve: () => undefined,
      reject: () => undefined,
    });
  }

  private dispatchToSlot(
    slot: WorkerSlot,
    job: PendingJob,
    timeoutMs = this.timeoutMs,
    lane: ImportCheckLane | null = null,
  ): void {
    slot.inFlight = job;
    slot.lane = lane;
    slot.inFlightStartedAt = Date.now();
    slot.timeout = setTimeout(() => {
      void Metric.increment(timeoutCounter).pipe(Effect.runPromise);
      this.failSlot(
        slot,
        new Error(
          `validation job ${job.request.jobId} timed out after ${timeoutMs}ms`,
        ),
      );
      void slot.worker.terminate();
    }, timeoutMs);
    slot.worker.postMessage(job.request, [...job.transferList]);
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
    slot.started = true;
    this.finishSlot(slot);
    job.resolve(response);
  }

  private finishSlot(slot: WorkerSlot): void {
    if (slot.timeout !== null) clearTimeout(slot.timeout);
    slot.timeout = null;
    slot.inFlight = null;
    slot.lane = null;
    slot.inFlightStartedAt = 0;
    slot.restartAttempt = 0;
    const reserved = slot.reservedFor;
    if (reserved !== null) {
      slot.reservedFor = null;
      this.dispatchToSlot(slot, reserved, reserved.killMs, reserved.lane);
      return;
    }
    this.dispatch();
  }

  private failSlot(slot: WorkerSlot, cause: unknown): void {
    if (slot.restarting || this.closed) return;
    slot.restarting = true;
    if (slot.timeout !== null) clearTimeout(slot.timeout);
    slot.timeout = null;
    const error = new ValidationWorkerError({
      message: `validation worker ${slot.index} failed`,
      cause,
    });
    slot.inFlight?.reject(error);
    slot.reservedFor?.reject(error);
    slot.inFlight = null;
    slot.reservedFor = null;
    slot.lane = null;
    slot.inFlightStartedAt = 0;
  }

  private scheduleRespawn(slot: WorkerSlot): void {
    if (this.closed) return;
    // A slot replaced for a fresh import check is not a failure.
    const reserved = slot.reservedFor;
    const delay =
      reserved !== null ? 0 : Math.min(250 * 2 ** slot.restartAttempt, 5_000);
    if (reserved === null) slot.restartAttempt += 1;
    const timer = setTimeout(() => {
      this.restartTimers.delete(timer);
      if (this.closed) return;
      const replacement = this.spawnSlot(slot.index);
      replacement.restartAttempt = slot.restartAttempt;
      this.slots[slot.index] = replacement;
      if (reserved === null) {
        void Metric.increment(restartCounter).pipe(Effect.runPromise);
        this.dispatch();
        return;
      }
      slot.reservedFor = null;
      this.warmUpFor(replacement, reserved);
    }, delay);
    this.restartTimers.add(timer);
  }
}
