/**
 * The submit check pool (design D4): `worker_threads` that run the
 * `/submit` body check (`commands/http/submission-check.ts`) off the main
 * thread, separate from the block-validation pool.
 *
 * A request reserves a slot before its body is read; the slot holds the
 * body's bytes until the worker's verdict comes back. The pool holds at most
 * `maxQueued` slots and `maxQueuedBytes` bytes, so a full pool refuses at
 * once and nothing waits unbounded.
 *
 * Bodies are sent in groups, their buffers transferred: a group goes out
 * once `FLUSH_AT_ONCE_BODIES` bodies wait, or `FLUSH_DELAY_MS` after the
 * first of them was posted. A group smaller than two chunks of
 * `MIN_CHUNK_BODIES` goes whole to the worker holding the fewest bodies; a
 * larger one is split into such chunks across the workers holding the
 * fewest. One message per body would cost the main thread more than the
 * request itself at a few thousand requests a second.
 *
 * A worker takes bodies once it has loaded the check and said so. Bodies
 * posted while every running worker is still loading wait for the first
 * one; a worker not loaded within `startTimeoutMs` is stopped. A worker
 * that stops (an uncaught error, an exit, a batch past `batchTimeoutMs`,
 * a start past `startTimeoutMs`) answers each body it held as
 * `unavailable` and is replaced after a short backoff; while no worker is
 * running, bodies are answered `unavailable` at once.
 */
import { Worker } from "node:worker_threads";

import type { SubmissionCheckConfig } from "../commands/http/submission-check.js";
import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import type {
  SubmitCheckBatch,
  SubmitCheckWorkerInit,
  SubmitCheckWorkerMessage,
} from "../workers/utils/submit-check-protocol.js";
import {
  outcomeOf,
  type SubmitCheckOutcome,
} from "./submit-check-pool.submit-check-outcome.js";
import { BUNDLED_WORKER_EXEC_ARGV } from "./validation-pool.bundled-worker-exec-argv.js";

export type { SubmitCheckOutcome } from "./submit-check-pool.submit-check-outcome.js";

/** One request's slot in the pool. */
export type SubmitCheckTicket = {
  /**
   * Posts `body` for checking; `onOutcome` runs once, after the slot is
   * freed. The body's buffer is transferred to the worker, so `body` is
   * unusable afterwards. At most once per ticket.
   */
  readonly check: (
    body: Buffer,
    onOutcome: (outcome: SubmitCheckOutcome) => void,
  ) => void;
  /**
   * Frees the slot of a body never posted (the client went away). A posted
   * body keeps its slot until its verdict. Idempotent.
   */
  readonly release: () => void;
};

export type SubmitCheckPoolStats = {
  readonly workers: number;
  /** Workers running, loaded or not. */
  readonly liveWorkers: number;
  /** Workers that have loaded the check and take bodies. */
  readonly readyWorkers: number;
  /** Slots held: bodies being read, waiting for a worker or in one. */
  readonly held: number;
  readonly heldBytes: number;
  readonly restarts: number;
};

export type SubmitCheckPool = {
  /** A slot for a body of `bytes` bytes; `undefined` when the pool is full. */
  readonly reserve: (bytes: number) => SubmitCheckTicket | undefined;
  readonly stats: () => SubmitCheckPoolStats;
  /**
   * Resolves `true` once a worker has loaded the check, or `false` if none
   * has within `timeoutMs` or the pool closes first.
   */
  readonly whenReady: (timeoutMs: number) => Promise<boolean>;
  /** Terminates the workers; every body still held is `unavailable`. */
  readonly close: () => Promise<void>;
};

export type SubmitCheckPoolOptions = {
  readonly size: number;
  readonly maxQueued: number;
  readonly maxQueuedBytes: number;
  readonly config: SubmissionCheckConfig;
  /** The worker module; by default the built `submit-check.js`. */
  readonly entry?: URL;
  /** A batch its worker has not answered by then stops the worker. */
  readonly batchTimeoutMs?: number;
  /** A worker not loaded by then is stopped. */
  readonly startTimeoutMs?: number;
  /** Called when a worker stops, before it is replaced. */
  readonly onWorkerStopped?: (workerIndex: number, detail: string) => void;
};

/** Bodies waiting that are sent at once, without the delay below. */
const FLUSH_AT_ONCE_BODIES = 32;
/** The longest a posted body waits for others to share its message. */
const FLUSH_DELAY_MS = 1;
/** The fewest bodies a group is split into per worker. */
const MIN_CHUNK_BODIES = 32;
const DEFAULT_BATCH_TIMEOUT_MS = 10_000;
export const DEFAULT_START_TIMEOUT_MS = 30_000;
const RESPAWN_BASE_DELAY_MS = 50;
const RESPAWN_MAX_DELAY_MS = 5_000;

type Posted = {
  readonly buffer: ArrayBuffer;
  readonly settle: (outcome: SubmitCheckOutcome) => void;
};

type InFlight = {
  readonly items: readonly Posted[];
  readonly timer: NodeJS.Timeout;
};

type Slot = {
  readonly index: number;
  worker: Worker | null;
  /** Whether `worker` has loaded the check. */
  ready: boolean;
  startTimer: NodeJS.Timeout | null;
  /** Bodies sent to this worker and not yet answered. */
  inFlightBodies: number;
  readonly batches: Map<number, InFlight>;
  failures: number;
};

/** The built worker module beside `dist/`. */
export const submitCheckWorkerEntry = (): URL =>
  resolveWorkerEntry(import.meta.url, "submit-check.js");

export const createSubmitCheckPool = ({
  size,
  maxQueued,
  maxQueuedBytes,
  config,
  entry = submitCheckWorkerEntry(),
  batchTimeoutMs = DEFAULT_BATCH_TIMEOUT_MS,
  startTimeoutMs = DEFAULT_START_TIMEOUT_MS,
  onWorkerStopped,
}: SubmitCheckPoolOptions): SubmitCheckPool => {
  const workerData: SubmitCheckWorkerInit = { config };
  const slots: Slot[] = Array.from({ length: size }, (_, index) => ({
    index,
    worker: null,
    ready: false,
    startTimer: null,
    inFlightBodies: 0,
    batches: new Map(),
    failures: 0,
  }));
  let pending: Posted[] = [];
  /** The delayed flush of the bodies waiting, while it is pending. */
  let flushTimer: NodeJS.Timeout | null = null;
  /** Whether a flush is queued to run once the current task ends. */
  let flushQueued = false;
  let nextBatch = 0;
  let held = 0;
  let heldBytes = 0;
  let restarts = 0;
  let closed = false;
  const readyWaiters = new Set<(ready: boolean) => void>();
  const wakeReadyWaiters = (ready: boolean) => {
    for (const wake of [...readyWaiters]) wake(ready);
  };

  const scheduleFlush = () => {
    if (flushQueued || pending.length === 0) return;
    if (pending.length >= FLUSH_AT_ONCE_BODIES) {
      // After the current task, so no outcome settles inside `check`.
      flushQueued = true;
      queueMicrotask(flush);
    } else if (flushTimer === null)
      flushTimer = setTimeout(flush, FLUSH_DELAY_MS);
  };

  const stop = (slot: Slot, worker: Worker, detail: string) => {
    if (slot.worker !== worker) return;
    slot.worker = null;
    slot.ready = false;
    if (slot.startTimer !== null) clearTimeout(slot.startTimer);
    slot.startTimer = null;
    slot.inFlightBodies = 0;
    const lost = [...slot.batches.values()];
    slot.batches.clear();
    for (const batch of lost) {
      clearTimeout(batch.timer);
      for (const item of batch.items)
        item.settle({ kind: "unavailable", detail });
    }
    void worker.terminate();
    if (closed) return;
    if (slots.some((other) => other.worker !== null)) scheduleFlush();
    else {
      // No worker is left to load or take the bodies waiting for one.
      const waiting = pending;
      pending = [];
      for (const item of waiting) item.settle({ kind: "unavailable", detail });
    }
    onWorkerStopped?.(slot.index, detail);
    const delay = Math.min(
      RESPAWN_MAX_DELAY_MS,
      RESPAWN_BASE_DELAY_MS * 2 ** slot.failures,
    );
    slot.failures += 1;
    restarts += 1;
    setTimeout(() => {
      if (!closed) spawn(slot);
    }, delay).unref();
  };

  const spawn = (slot: Slot) => {
    const worker = new Worker(entry, {
      workerData,
      execArgv: [...BUNDLED_WORKER_EXEC_ARGV],
    });
    worker.unref();
    slot.worker = worker;
    slot.startTimer = setTimeout(
      () =>
        stop(
          slot,
          worker,
          `submit check worker ${slot.index.toString()} did not load within ${startTimeoutMs.toString()} ms`,
        ),
      startTimeoutMs,
    );
    slot.startTimer.unref();
    worker.on("message", (message: SubmitCheckWorkerMessage) => {
      if (slot.worker !== worker) return;
      if ("ready" in message) {
        slot.ready = true;
        if (slot.startTimer !== null) clearTimeout(slot.startTimer);
        slot.startTimer = null;
        wakeReadyWaiters(true);
        scheduleFlush();
        return;
      }
      const { batch, results } = message;
      const inFlight = slot.batches.get(batch);
      if (inFlight === undefined) return;
      slot.batches.delete(batch);
      clearTimeout(inFlight.timer);
      slot.inFlightBodies -= inFlight.items.length;
      slot.failures = 0;
      inFlight.items.forEach((item, index) => {
        const result = results[index];
        item.settle(
          result === undefined
            ? { kind: "failed", detail: "submit check worker skipped a body" }
            : outcomeOf(result),
        );
      });
    });
    worker.once("error", (error) =>
      stop(
        slot,
        worker,
        `submit check worker ${slot.index.toString()} stopped on an error it did not handle: ${error.message}`,
      ),
    );
    worker.once("exit", (code) =>
      stop(
        slot,
        worker,
        `submit check worker ${slot.index.toString()} exited with code ${code.toString()}`,
      ),
    );
  };

  const send = (slot: Slot, worker: Worker, items: readonly Posted[]) => {
    const batch = nextBatch++;
    const timer = setTimeout(
      () =>
        stop(
          slot,
          worker,
          `submit check worker ${slot.index.toString()} did not answer a batch within ${batchTimeoutMs.toString()} ms`,
        ),
      batchTimeoutMs,
    );
    timer.unref();
    slot.batches.set(batch, { items, timer });
    slot.inFlightBodies += items.length;
    const bodies = items.map((item) => item.buffer);
    const message: SubmitCheckBatch = { batch, bodies };
    worker.postMessage(message, bodies);
  };

  const flush = () => {
    flushQueued = false;
    if (flushTimer !== null) clearTimeout(flushTimer);
    flushTimer = null;
    const items = pending;
    pending = [];
    if (items.length === 0) return;
    const live = slots.filter((slot) => slot.worker !== null && slot.ready);
    if (live.length === 0) {
      if (slots.some((slot) => slot.worker !== null)) {
        // A worker is still loading: the bodies wait for it, bounded by
        // its start timeout.
        pending = [...items, ...pending];
        return;
      }
      for (const item of items)
        item.settle({
          kind: "unavailable",
          detail: "no submit check worker is running",
        });
      return;
    }
    // The workers holding the fewest bodies take the group: whole when it
    // is small, else in equal chunks of at least `MIN_CHUNK_BODIES`.
    const targets = [...live]
      .sort((a, b) => a.inFlightBodies - b.inFlightBodies)
      .slice(0, Math.max(1, Math.floor(items.length / MIN_CHUNK_BODIES)));
    let start = 0;
    targets.forEach((slot, index) => {
      const end = Math.round(((index + 1) * items.length) / targets.length);
      send(slot, slot.worker!, items.slice(start, end));
      start = end;
    });
  };

  const reserve = (bytes: number): SubmitCheckTicket | undefined => {
    if (closed || held >= maxQueued || heldBytes + bytes > maxQueuedBytes)
      return undefined;
    held += 1;
    heldBytes += bytes;
    let state: "reserved" | "posted" | "done" = "reserved";
    const free = () => {
      held -= 1;
      heldBytes -= bytes;
    };
    return {
      release: () => {
        if (state !== "reserved") return;
        state = "done";
        free();
      },
      check: (body, onOutcome) => {
        if (state !== "reserved") return;
        state = "posted";
        const settle = (outcome: SubmitCheckOutcome) => {
          if (state !== "posted") return;
          state = "done";
          free();
          onOutcome(outcome);
        };
        if (closed) {
          settle({ kind: "unavailable", detail: "submit check pool closed" });
          return;
        }
        // A body that shares its buffer is copied, so only its own bytes
        // are transferred.
        const buffer =
          body.byteOffset === 0 && body.byteLength === body.buffer.byteLength
            ? (body.buffer as ArrayBuffer)
            : new Uint8Array(body).buffer;
        pending.push({ buffer, settle });
        scheduleFlush();
      },
    };
  };

  const whenReady = (timeoutMs: number): Promise<boolean> => {
    if (slots.some((slot) => slot.worker !== null && slot.ready))
      return Promise.resolve(true);
    if (closed) return Promise.resolve(false);
    return new Promise((resolve) => {
      const wake = (ready: boolean) => {
        clearTimeout(timer);
        readyWaiters.delete(wake);
        resolve(ready);
      };
      const timer = setTimeout(() => wake(false), timeoutMs);
      readyWaiters.add(wake);
    });
  };

  for (const slot of slots) spawn(slot);

  return {
    reserve,
    whenReady,
    stats: () => ({
      workers: size,
      liveWorkers: slots.filter((slot) => slot.worker !== null).length,
      readyWorkers: slots.filter((slot) => slot.worker !== null && slot.ready)
        .length,
      held,
      heldBytes,
      restarts,
    }),
    close: async () => {
      closed = true;
      if (flushTimer !== null) clearTimeout(flushTimer);
      flushTimer = null;
      wakeReadyWaiters(false);
      const items = pending;
      pending = [];
      for (const item of items)
        item.settle({
          kind: "unavailable",
          detail: "submit check pool closed",
        });
      const workers = slots.flatMap((slot) =>
        slot.worker === null ? [] : [slot.worker],
      );
      for (const slot of slots)
        if (slot.worker !== null)
          stop(slot, slot.worker, "submit check pool closed");
      await Promise.all(workers.map((worker) => worker.terminate()));
    },
  };
};
