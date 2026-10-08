import {
  type ChainSyncEvent,
  type ChainSyncStream,
  IntersectNotFoundError,
} from "@al-ft/l1-node-transport";

import {
  intersectionFailure,
  protocolInitStatus,
  startFromOrigin,
} from "../origin.js";
import { FollowerMigrationError } from "../schema/migrate.js";
import type { Intervention } from "../types.js";
import {
  applyChainSyncEvent,
  type FollowStep,
  intersectionPoints,
  stepLocked,
  stepSettled,
  storePoint,
} from "./chain-sync.js";
import { classifyFailure, type FailureClass } from "./failure.js";
import { startWhenFree } from "./start.js";
import {
  DEFAULT_STUCK_AFTER,
  type FollowChainOptions,
  type FollowStatus,
  type FollowWaitCause,
  LOOP_PRUNE_BUDGET,
  LOOP_PRUNE_EVERY,
  readinessOf,
} from "./status.js";

export * from "./status.js";

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const sleep = (ms: number, signal: AbortSignal): Promise<void> =>
  new Promise((resolve) => {
    if (signal.aborted) return resolve();
    const timer = setTimeout(done, ms);
    function done(): void {
      clearTimeout(timer);
      signal.removeEventListener("abort", done);
      resolve();
    }
    signal.addEventListener("abort", done, { once: true });
  });

const describeEvent = (event: ChainSyncEvent): string =>
  event.point.kind === "point"
    ? `${event.kind} ${event.point.slot}.${event.point.hash}`
    : `${event.kind} origin`;

/** What one event did to the follow loop. */
type Handled = "continue" | "relock" | "backoff" | "intervention";

/**
 * The shared follow loop (plan §4.3, §7.5): starts the store (waiting out
 * the writer lease), starts from the configured origin or resumes from the
 * store's own points, applies chain-sync events and acknowledges each one
 * the store settled, checks `protocolInitStatus` at every tip, and prunes.
 * It never throws and never exits the process: transient failures back off
 * (capped exponential) and start again; an intervention stops the loop and
 * stays in the status until the operator acts and the process restarts.
 * Resolves with the final status once stopped by an intervention or abort.
 */
export const followChain = async (
  options: FollowChainOptions,
): Promise<FollowStatus> => {
  const { store, signal } = options;
  store.watchProtocolInit(options.origin.hubOracleOneShot);
  const backoff = options.backoffMs ?? { initial: 500, max: 30_000 };
  const log = options.log ?? (() => undefined);
  const credit = options.credit ?? 64;
  const stuckAfter = options.stuckAfter ?? DEFAULT_STUCK_AFTER;
  const pruneBudget = options.prune?.budget ?? LOOP_PRUNE_BUDGET;
  const pruneEvery = options.prune?.everyEvents ?? LOOP_PRUNE_EVERY;
  const initial: Omit<FollowStatus, "readiness"> = {
    state: "starting",
    interventions: [],
    waiting: null,
    stuck: null,
    protocolInit: "unknown",
    cursor: null,
    tip: null,
    atTip: false,
    events: 0,
    lastError: null,
    prune: { steps: 0, prunedThroughSlot: null, lastError: null },
  };
  let status: FollowStatus = { ...initial, readiness: readinessOf(initial) };
  const publish = async (
    change: Partial<Omit<FollowStatus, "readiness">>,
  ): Promise<void> => {
    const next = { ...status, ...change };
    status = { ...next, readiness: readinessOf(next) };
    try {
      await options.onStatus?.(status);
    } catch (error) {
      log(`status listener failed: ${message(error)}`);
    }
  };

  let failure: { at: string; count: number } | null = null;
  /**
   * Records a failure and waits on it. `at` names the event point for
   * failures that count toward `stuck`; without it only a deterministic
   * failure escalates.
   */
  const failed = async (
    cause: FollowWaitCause,
    detail: string,
    kind: FailureClass,
    at?: string,
  ): Promise<void> => {
    let stuck = status.stuck;
    if (kind === "deterministic" || (kind === "unknown" && at !== undefined)) {
      const where = at ?? cause;
      const count = failure?.at === where ? failure.count + 1 : 1;
      failure = { at: where, count };
      if (kind === "deterministic" || count >= stuckAfter) {
        if (stuck?.at !== where)
          log(`apply stuck at ${where} after ${count} failures: ${detail}`);
        stuck = { at: where, failures: count, detail };
      }
    }
    await publish({
      state: "waiting",
      waiting: { cause, detail },
      stuck,
      lastError: detail,
    });
  };
  const stopOn = async (found: Intervention): Promise<void> => {
    log(`intervention ${found.reason}: ${found.detail}`);
    await publish({
      state: "intervention",
      waiting: null,
      interventions: [
        ...status.interventions.filter((i) => i.reason !== found.reason),
        { reason: found.reason, detail: found.detail },
      ],
    });
  };

  let sincePrune = 0;
  let pruneBacklog = false;
  const maybePrune = async (atTip: boolean): Promise<void> => {
    sincePrune += 1;
    if (!atTip && !pruneBacklog && sincePrune < pruneEvery) return;
    sincePrune = 0;
    const pruned = await store.prune(pruneBudget).catch((error: unknown) => ({
      kind: "error" as const,
      error: error instanceof Error ? error : new Error(String(error)),
    }));
    // store_locked: the next write sees it too, and the loop starts again.
    if ("kind" in pruned) {
      if (pruned.kind === "error") {
        log(`prune failed: ${pruned.error.message}`);
        await publish({
          prune: { ...status.prune, lastError: pruned.error.message },
        });
      }
      return;
    }
    pruneBacklog = !pruned.done;
    await publish({
      prune: {
        steps: status.prune.steps + 1,
        prunedThroughSlot: pruned.prunedThroughSlot,
        lastError: null,
      },
    });
  };

  const checkProtocolInit = async (event: ChainSyncEvent): Promise<void> => {
    if (event.tip.point.kind !== "point") return;
    const init = await protocolInitStatus(
      store,
      options.origin,
      storePoint(event.tip.point),
    );
    if (init.kind === "error")
      await publish({ lastError: `protocol init: ${init.error.message}` });
    else if (init.kind === "intervention") {
      // R3 does not stop the loop: it keeps following, and only a later
      // check that finds the spend (after a reorg) clears it.
      if (!status.interventions.some((i) => i.reason === init.reason))
        log(`intervention ${init.reason}: ${init.detail}`);
      await publish({
        protocolInit: "pending",
        interventions: [
          ...status.interventions.filter((i) => i.reason !== init.reason),
          { reason: init.reason, detail: init.detail },
        ],
      });
    } else if (init.kind === "pending")
      // Catching up again does not undo a caught-up miss.
      await publish({ protocolInit: "pending" });
    else
      await publish({
        protocolInit: "seen",
        interventions: status.interventions.filter(
          (i) => i.reason !== "origin_after_protocol_init",
        ),
      });
  };

  const handle = async (event: ChainSyncEvent): Promise<Handled> => {
    const at = describeEvent(event);
    let step: FollowStep;
    try {
      step = await applyChainSyncEvent(store, event);
    } catch (error) {
      await failed(
        "apply",
        `${event.kind}: ${message(error)}`,
        classifyFailure(error),
        at,
      );
      return "backoff";
    }
    if (stepLocked(step)) return "relock";
    if (!stepSettled(step)) {
      const result = step.result;
      if (result.kind === "intervention") {
        await stopOn(result);
        return "intervention";
      }
      if (result.kind === "error")
        await failed(
          "apply",
          `${event.kind}: ${result.error.message}`,
          classifyFailure(result.error),
          at,
        );
      else if (result.kind === "block_undecodable")
        // The same bytes fail the same way on every retry.
        await failed(
          "apply",
          `${event.kind}: ${result.detail}`,
          "deterministic",
          at,
        );
      else if (result.kind === "rejected")
        await failed("apply", `${event.kind}: ${result.detail}`, "unknown", at);
      return "backoff";
    }
    failure = null;
    let cursor;
    try {
      cursor = await store.cursor();
    } catch (error) {
      await failed("store", message(error), classifyFailure(error));
      return "backoff";
    }
    const tip = event.tip.point;
    const atTip =
      tip.kind === "point" &&
      cursor !== null &&
      cursor.point.slot === Number(tip.slot) &&
      cursor.point.hash.toString("hex") === tip.hash;
    await publish({
      state: "following",
      waiting: null,
      stuck: null,
      events: status.events + 1,
      lastError: null,
      atTip,
      tip:
        tip.kind === "point"
          ? { slot: Number(tip.slot), height: Number(event.tip.blockNo) }
          : null,
      cursor:
        cursor === null
          ? null
          : {
              slot: cursor.point.slot,
              height: cursor.height,
              generation: cursor.generation,
            },
    });
    await maybePrune(atTip);
    await checkProtocolInit(event);
    return "continue";
  };

  let delay = backoff.initial;
  const wait = async (): Promise<void> => {
    await sleep(delay, signal);
    delay = Math.min(delay * 2, backoff.max);
  };
  await publish({});
  while (!signal.aborted) {
    try {
      const started = await startWhenFree(store, {
        signal,
        backoffMs: backoff,
        log,
        onLocked: (locked) =>
          failed("store_locked", locked.detail, "transient"),
      });
      if (started === undefined) break;
      if (started.kind !== "ready") {
        await stopOn(started);
        return status;
      }
      const begun = await startFromOrigin({
        store,
        transport: options.transport,
        origin: options.origin.origin,
        credit,
      });
      if (begun.kind === "intervention") {
        await stopOn(begun);
        return status;
      }
      if (begun.kind === "store_locked" || begun.kind === "error") {
        if (begun.kind === "error")
          await failed(
            "store",
            begun.error.message,
            classifyFailure(begun.error),
          );
        else await failed("store_locked", begun.detail, "transient");
        await wait();
        continue;
      }
      if (
        status.stuck !== null &&
        (status.stuck.at === "store" || status.stuck.at === "migration")
      )
        await publish({ stuck: null });
      let stream: ChainSyncStream;
      let first: ChainSyncEvent | undefined;
      if (begun.kind === "resume") {
        stream = options.transport.openChainSync({
          points: await intersectionPoints(store),
          credit,
        });
      } else {
        stream = begun.stream;
        first = begun.first;
      }
      const onAbort = (): void => void stream.close();
      signal.addEventListener("abort", onAbort, { once: true });
      let outcome: Handled = "continue";
      try {
        await stream.opened;
        if (first !== undefined) {
          outcome = await handle(first);
          if (outcome === "continue") stream.ack(first.seq);
        }
        while (outcome === "continue" && !signal.aborted) {
          const event = await stream.next();
          if (event === undefined) {
            if (!signal.aborted)
              await failed(
                "stream",
                "the chain-sync stream ended",
                "transient",
              );
            outcome = "backoff";
            break;
          }
          outcome = await handle(event);
          if (outcome === "continue") {
            stream.ack(event.seq);
            delay = backoff.initial;
          }
        }
      } catch (error) {
        if (error instanceof IntersectNotFoundError) {
          await stopOn(
            intersectionFailure(
              error,
              await store.cursor(),
              options.origin.origin,
            ),
          );
          return status;
        }
        if (!signal.aborted)
          await failed("stream", message(error), "transient");
        outcome = "backoff";
      } finally {
        signal.removeEventListener("abort", onAbort);
        await stream.close();
      }
      if (outcome === "intervention") return status;
      if (outcome === "relock") {
        await failed("store_locked", "store_locked", "transient");
        continue;
      }
      if (!signal.aborted) await wait();
    } catch (error) {
      // A follower failure must not reach the role: record it, back off and
      // start again.
      log(`follower failed, starting again: ${message(error)}`);
      // A migration the store refuses (a changed or unknown applied
      // migration) fails the same way on every start: stuck at once, named.
      if (error instanceof FollowerMigrationError)
        await failed("store", message(error), "deterministic", "migration");
      else await failed("store", message(error), classifyFailure(error));
      await wait();
    }
  }
  await publish({ state: "stopped" });
  return status;
};
