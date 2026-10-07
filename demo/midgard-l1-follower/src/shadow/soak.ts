import {
  type ChainSyncEvent,
  type ChainSyncOptions,
  IntersectNotFoundError,
} from "@al-ft/l1-node-transport";

import {
  applyChainSyncEvent,
  type FollowStep,
  intersectionPoints,
  stepLocked,
  stepSettled,
} from "../follow/chain-sync.js";
import type { FactStore, StartResult } from "../store/fact-store.js";
import type { Point, StoreLocked } from "../types.js";
import { SHADOW_ROLES, type ShadowComparator } from "./comparator.js";
import { compareAll } from "./compare.js";
import { type BlockRecord, Journal, readJournal } from "./journal.js";

/** The part of a chain-sync stream the soak uses. */
export type SoakStream = Readonly<{
  next: () => Promise<ChainSyncEvent | undefined>;
  ack: (seq: bigint) => void;
  close: () => Promise<void>;
}>;

export type SoakOptions = Readonly<{
  /** The soak directory: `journal.jsonl` lives here. */
  dir: string;
  /** A started, initialized store (the follower's facts for this soak). */
  store: FactStore;
  /** Opens chain-sync (an `L1NodeTransport`, or a fake in tests). */
  openChainSync: (options: ChainSyncOptions) => SoakStream;
  comparators: readonly ShadowComparator[];
  /** Stop after this many events (tests); unbounded by default. */
  maxEvents?: number;
  signal?: AbortSignal;
  /** Chain-sync credit window (default 10). */
  credit?: number;
  backoffMs?: Readonly<{ initial: number; max: number }>;
  log?: (line: string) => void;
}>;

/**
 * Why the soak stopped. `store_locked` is transient (another writer holds or
 * took over the store's lease): the caller starts the store again
 * (`startWhenFree`) and runs the soak again; nothing was acknowledged or
 * applied for the event that found the store locked.
 */
export type SoakStop = Readonly<{
  reason: "intervention" | "limit" | "signal" | "refused" | "store_locked";
  detail: string;
  events: number;
}>;

const now = (): string => new Date().toISOString();

const hex = (point: Point): string => point.hash.toString("hex");

const sleep = (ms: number, signal?: AbortSignal): Promise<void> =>
  new Promise((resolve) => {
    const timer = setTimeout(resolve, ms);
    signal?.addEventListener(
      "abort",
      () => {
        clearTimeout(timer);
        resolve();
      },
      { once: true },
    );
  });

const describeStep = (step: FollowStep): string =>
  step.result.kind === "error"
    ? step.result.error.message
    : step.result.kind === "intervention"
      ? `intervention ${step.result.reason}: ${step.result.detail}`
      : "detail" in step.result
        ? `${step.result.kind}: ${step.result.detail}`
        : step.result.kind;

/** Roles that have no comparator yet (their tickets have not plugged in). */
export const rolesWithout = (
  comparators: readonly ShadowComparator[],
): string[] =>
  SHADOW_ROLES.filter((role) => !comparators.some((c) => c.role === role));

/**
 * The devnet soak (plan §14): follows the chain into the store through the
 * sequential writer, runs every comparator after each event and appends one
 * fsynced journal line per event. It resumes from the store's cursor after
 * a restart (re-comparing at the cursor when the journal lags it) and stops
 * at the first intervention (R1, R2, R5, an undecodable block) with a
 * journal line saying why. Transient store errors and stream failures are
 * retried with backoff. A store found locked (its writer lease lost, or
 * fenced by a newer writer) stops the soak as `store_locked` before the
 * event is journaled or acknowledged; the caller starts the store again.
 */
export const runSoak = async (options: SoakOptions): Promise<SoakStop> => {
  const { store, comparators, signal } = options;
  const log = options.log ?? (() => undefined);
  const aborted = (): boolean => signal?.aborted === true;
  const backoff = options.backoffMs ?? { initial: 500, max: 30_000 };
  const journal = await Journal.open(options.dir);
  let events = 0;
  const stop = async (
    reason: SoakStop["reason"],
    detail: string,
  ): Promise<SoakStop> => {
    await journal.append({ type: "stop", at: now(), reason, detail });
    log(`soak stopped (${reason}): ${detail}`);
    return { reason, detail, events };
  };
  const compareAt = async (
    event: ChainSyncEvent | undefined,
  ): Promise<BlockRecord> => {
    const cursor = await store.cursor();
    if (cursor === null) throw new Error("the soak store has no cursor");
    const at = {
      point: cursor.point,
      height: cursor.height,
      generation: cursor.generation,
    };
    const results = await compareAll(comparators, {
      store,
      at,
      ...(event === undefined ? {} : { event }),
    });
    return {
      type: "block",
      at: now(),
      event: event?.kind ?? "resume",
      seq: event === undefined ? null : event.seq.toString(),
      slot: at.point.slot,
      hash: hex(at.point),
      height: at.height,
      generation: at.generation,
      results,
    };
  };
  try {
    const cursor = await store.cursor();
    if (cursor === null)
      return await stop("refused", "the soak store was never initialized");
    await journal.append({
      type: "start",
      at: now(),
      comparators: comparators.map((c) => `${c.role}/${c.name}`),
      rolesWithout: rolesWithout(comparators),
      cursor: {
        slot: cursor.point.slot,
        hash: hex(cursor.point),
        height: cursor.height,
      },
    });
    // The journal lags the store after a crash between apply and append (or
    // on a fresh soak): compare once at the cursor before following.
    const { records } = await readJournal(options.dir);
    const last = [...records].reverse().find((r) => r.type === "block");
    if (
      last === undefined ||
      last.slot !== cursor.point.slot ||
      last.hash !== hex(cursor.point)
    )
      await journal.append(await compareAt(undefined));
    let delay = backoff.initial;
    while (!aborted()) {
      const points = await intersectionPoints(store);
      const stream = options.openChainSync({
        points,
        credit: options.credit ?? 10,
        resume: true,
      });
      const onAbort = (): void => void stream.close();
      signal?.addEventListener("abort", onAbort, { once: true });
      try {
        for (;;) {
          const event = await stream.next();
          if (event === undefined) break;
          let step = await applyChainSyncEvent(store, event);
          for (
            let retry = backoff.initial;
            step.result.kind === "error" && !aborted();
            retry = Math.min(retry * 2, backoff.max)
          ) {
            log(`store error, retrying in ${retry} ms: ${describeStep(step)}`);
            await sleep(retry, signal);
            step = await applyChainSyncEvent(store, event);
          }
          if (aborted()) break;
          if (stepLocked(step))
            return await stop(
              "store_locked",
              `${event.kind} #${event.seq}: ${describeStep(step)}`,
            );
          if (!stepSettled(step))
            return await stop(
              step.result.kind === "intervention" ||
                step.result.kind === "block_undecodable"
                ? "intervention"
                : "refused",
              `${event.kind} #${event.seq}: ${describeStep(step)}`,
            );
          await journal.append(await compareAt(event));
          stream.ack(event.seq);
          events += 1;
          delay = backoff.initial;
          if (options.maxEvents !== undefined && events >= options.maxEvents)
            return await stop("limit", `${events} events`);
        }
      } catch (error) {
        if (error instanceof IntersectNotFoundError)
          return await stop(
            "intervention",
            `intersection_outside_history: ${error.message}`,
          );
        log(`chain-sync failed, reopening in ${delay} ms: ${String(error)}`);
      } finally {
        signal?.removeEventListener("abort", onAbort);
        await stream.close();
      }
      if (!aborted()) {
        // The stream ended without a failure: reopen after a pause.
        await sleep(delay, signal);
        delay = Math.min(delay * 2, backoff.max);
      }
    }
    return await stop("signal", "stop requested");
  } finally {
    await journal.close();
  }
};

/**
 * Starts the store, waiting out `store_locked` (another process holds the
 * writer lease) with backoff instead of exiting. Returns the first other
 * result (`ready`, or an intervention), or `undefined` once `signal` aborts.
 */
export const startWhenFree = async (
  store: FactStore,
  options: Readonly<{
    signal?: AbortSignal;
    backoffMs?: Readonly<{ initial: number; max: number }>;
    log?: (line: string) => void;
  }> = {},
): Promise<Exclude<StartResult, StoreLocked> | undefined> => {
  const backoff = options.backoffMs ?? { initial: 500, max: 30_000 };
  for (
    let delay = backoff.initial;
    ;
    delay = Math.min(delay * 2, backoff.max)
  ) {
    if (options.signal?.aborted === true) return undefined;
    const started = await store.start();
    if (started.kind !== "store_locked") return started;
    options.log?.(
      `store locked, starting again in ${delay} ms: ${started.detail}`,
    );
    await sleep(delay, options.signal);
  }
};
