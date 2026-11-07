/**
 * The submit check pool (design D4): `worker_threads` that run the
 * `/submit` body check (`commands/http/submission-check.ts`) off the main
 * thread, separate from the block-validation pool.
 *
 * A request reserves a slot before its body is read; the slot holds the
 * body's bytes until the worker's verdict comes back. The pool holds at most
 * `maxQueued` slots and `maxQueuedBytes` bytes, so a full pool refuses at
 * once and nothing waits unbounded. Bodies posted in one tick go out as one
 * message per worker, their buffers transferred.
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

import { Effect, Runtime, type Scope } from "effect";

import {
  type SubmissionCheck,
  type SubmissionCheckConfig,
  submissionCheckConfig,
} from "../commands/http/submission-check.js";
import {
  type CheckedSubmittedTx,
  submittedTxRules,
} from "../commands/http/transaction-admission.js";
import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import {
  SUBMIT_CHECK_FULL_HASH_END,
  SUBMIT_CHECK_TX_ID_BYTES,
  type SubmitCheckBatch,
  type SubmitCheckWireResult,
  type SubmitCheckWorkerInit,
  type SubmitCheckWorkerMessage,
} from "../workers/utils/submit-check-protocol.js";
import { NodeConfig } from "./config.js";
import { Lucid } from "./lucid.js";
import { BUNDLED_WORKER_EXEC_ARGV } from "./validation-pool.bundled-worker-exec-argv.js";

/** What became of one posted body. */
export type SubmitCheckOutcome =
  | { readonly kind: "checked"; readonly check: SubmissionCheck }
  /** Its worker stopped, or the pool closed, before a verdict. */
  | { readonly kind: "unavailable"; readonly detail: string }
  /** The check threw on it. */
  | { readonly kind: "failed"; readonly detail: string };

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

const DEFAULT_BATCH_TIMEOUT_MS = 10_000;
const DEFAULT_START_TIMEOUT_MS = 30_000;
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

/** The canonical bytes of a result, viewed without a copy. */
const checkedTx = (
  result: Extract<SubmitCheckWireResult, { readonly ok: true }>,
): CheckedSubmittedTx => {
  const bytes = Buffer.from(result.bytes);
  return {
    txId: bytes.subarray(0, SUBMIT_CHECK_TX_ID_BYTES),
    txIdHex: result.txIdHex,
    txFullHash: bytes.subarray(
      SUBMIT_CHECK_TX_ID_BYTES,
      SUBMIT_CHECK_FULL_HASH_END,
    ),
    txCanonicalCbor: bytes.subarray(SUBMIT_CHECK_FULL_HASH_END),
    spentOutRefs: result.spentOutRefs,
    producedOutRefs: result.producedOutRefs,
    fee: result.fee,
    byteLength: result.byteLength,
    source: "native",
  };
};

const outcomeOf = (result: SubmitCheckWireResult): SubmitCheckOutcome => {
  if (result.ok === "failed") return { kind: "failed", detail: result.detail };
  if (result.ok)
    return {
      kind: "checked",
      check: { ok: true, normalized: checkedTx(result) },
    };
  return { kind: "checked", check: result };
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
  let flushScheduled = false;
  let nextBatch = 0;
  let held = 0;
  let heldBytes = 0;
  let restarts = 0;
  let closed = false;

  const scheduleFlush = () => {
    if (flushScheduled || pending.length === 0) return;
    flushScheduled = true;
    setImmediate(flush);
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
    flushScheduled = false;
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
    // Each body goes to the worker holding the fewest bodies so far.
    const load = live.map((slot) => slot.inFlightBodies);
    const assigned: Posted[][] = live.map(() => []);
    for (const item of items) {
      let target = 0;
      for (let index = 1; index < live.length; index += 1)
        if (load[index]! < load[target]!) target = index;
      assigned[target]!.push(item);
      load[target]! += 1;
    }
    live.forEach((slot, index) => {
      const share = assigned[index]!;
      if (share.length > 0) send(slot, slot.worker!, share);
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

  for (const slot of slots) spawn(slot);

  return {
    reserve,
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

/**
 * The node's submit check pool for the enclosing scope, sized by
 * `SUBMIT_INGRESS_WORKER_POOL_SIZE`, `SUBMIT_INGRESS_WORKER_QUEUE` and
 * `SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES`, checking with the node's rules
 * (`submittedTxRules`, read once here). Worker stops are logged.
 */
export const acquireSubmitCheckPool: Effect.Effect<
  SubmitCheckPool,
  never,
  NodeConfig | Lucid | Scope.Scope
> = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const rules = yield* submittedTxRules;
  const runFork = Runtime.runFork(yield* Effect.runtime<never>());
  return yield* Effect.acquireRelease(
    Effect.sync(() =>
      createSubmitCheckPool({
        size: config.SUBMIT_INGRESS_WORKER_POOL_SIZE,
        maxQueued: config.SUBMIT_INGRESS_WORKER_QUEUE,
        maxQueuedBytes: config.SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES,
        config: submissionCheckConfig(config, rules),
        onWorkerStopped: (_, detail) => {
          runFork(Effect.logError(`${detail}; replacing it`));
        },
      }),
    ),
    (pool) => Effect.promise(() => pool.close()),
  );
});
