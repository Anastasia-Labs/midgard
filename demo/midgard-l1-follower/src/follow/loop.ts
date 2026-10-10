import {
  type ChainSyncEvent,
  type ChainSyncStream,
  IntersectNotFoundError,
  type StreamInterruption,
  type TransportReadiness,
} from "@al-ft/l1-node-transport";

import {
  intersectionFailure,
  protocolInitStatus,
  startFromOrigin,
} from "../origin.js";
import { FollowerMigrationError } from "../schema/migrate.js";
import { describeTrackedSetCheck } from "../store/tracked-set-record.js";
import type { Intervention } from "../types.js";
import {
  applyChainSyncEvent,
  type FollowStep,
  intersectionPoints,
  stepLocked,
  stepSettled,
  storePoint,
} from "./chain-sync.js";
import { classifyFailure, classifyStreamFailure } from "./failure.js";
import { watchNodeBehind } from "./node-behind.js";
import { startWhenFree } from "./start.js";
import {
  DEFAULT_STUCK_AFTER,
  FOLLOW_CREDIT_POLICY,
  type FollowChainOptions,
  FOLLOWER_TRANSIENT_BUDGET_MS,
  type FollowStatus,
  LOOP_PRUNE_BUDGET,
  LOOP_PRUNE_EVERY,
  readinessOf,
  STREAM_INTERRUPTED_AFTER,
} from "./status.js";
import { stuckRecorder } from "./stuck.js";

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

const nodeOf = (readiness: TransportReadiness): FollowStatus["node"] =>
  readiness.ready
    ? null
    : { reason: readiness.reason, detail: readiness.detail };

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
 * It never throws and never exits the process. A transient failure
 * (`classifyFailure`; for the chain-sync stream `classifyStreamFailure`; a
 * held writer lease) backs off (capped exponential) and starts again: a
 * stream drop and a held lease without bound (waiting on the L1 node, or on
 * another process), a store that does not answer for at most
 * `transientBudgetMs` with no event settled, after which the loop stops
 * `exhausted` (`l1_follower_transient_exhausted`) and its host exits
 * non-zero. A deterministic failure stops the loop at once, an unknown one
 * once it repeats `stuckAfter` times in a row at one point (or one step);
 * `stuck` names it (`l1_follower_apply_stuck`, `l1_follower_migration_failed`)
 * until a restart. Resolves with the final status once stopped by an
 * intervention, a stuck failure, an exhausted transient budget or abort.
 */
export const followChain = async (
  options: FollowChainOptions,
): Promise<FollowStatus> => {
  const { store, signal } = options;
  store.watchProtocolInit(options.origin.hubOracleOneShot);
  const backoff = options.backoffMs ?? { initial: 500, max: 30_000 };
  const log = options.log ?? (() => undefined);
  const credit = options.credit ?? FOLLOW_CREDIT_POLICY;
  const stuckAfter = options.stuckAfter ?? DEFAULT_STUCK_AFTER;
  const transientBudgetMs =
    options.transientBudgetMs ?? FOLLOWER_TRANSIENT_BUDGET_MS;
  const pruneBudget = options.prune?.budget ?? LOOP_PRUNE_BUDGET;
  const pruneEvery = options.prune?.everyEvents ?? LOOP_PRUNE_EVERY;
  const initial: Omit<FollowStatus, "readiness"> = {
    state: "starting",
    interventions: [],
    waiting: null,
    stuck: null,
    protocolInit: "unknown",
    cursor: null,
    node: nodeOf(options.transport.readiness),
    tip: null,
    nodeBehind: null,
    atTip: false,
    replaying: false,
    events: 0,
    lastError: null,
    prune: {
      steps: 0,
      prunedThroughSlot: null,
      lastError: null,
      floorLags: [],
      failures: 0,
    },
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

  const unsubscribe = options.transport.onReadiness((readiness) => {
    const node = nodeOf(readiness);
    void publish(node === null ? { node } : { node, atTip: false });
  });

  const behind = watchNodeBehind(
    options.nodeBehind,
    log,
    () => status,
    publish,
  );
  const finish = (): FollowStatus => {
    behind.stop();
    unsubscribe();
    return status;
  };

  const { failed, settled } = stuckRecorder({
    stuckAfter,
    transientBudgetMs,
    now: options.now ?? Date.now,
    log,
    publish,
  });
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
          prune: {
            ...status.prune,
            lastError: pruned.error.message,
            failures: status.prune.failures + 1,
          },
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
        floorLags: pruned.floorLags,
        failures: 0,
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
      return (await failed(
        "apply",
        `${event.kind}: ${message(error)}`,
        classifyFailure(error),
        at,
      ))
        ? "intervention"
        : "backoff";
    }
    if (stepLocked(step)) return "relock";
    if (!stepSettled(step)) {
      const result = step.result;
      if (result.kind === "intervention") {
        await stopOn(result);
        return "intervention";
      }
      let stopped = false;
      if (result.kind === "error")
        stopped = await failed(
          "apply",
          `${event.kind}: ${result.error.message}`,
          classifyFailure(result.error),
          at,
        );
      else if (result.kind === "block_undecodable")
        // The same bytes fail the same way on every retry.
        stopped = await failed(
          "apply",
          `${event.kind}: ${result.detail}`,
          "deterministic",
          at,
        );
      else if (result.kind === "rejected")
        stopped = await failed(
          "apply",
          `${event.kind}: ${result.detail}`,
          "unknown",
          at,
        );
      return stopped ? "intervention" : "backoff";
    }
    settled();
    let cursor;
    try {
      cursor = await store.cursor();
    } catch (error) {
      return (await failed("store", message(error), classifyFailure(error)))
        ? "intervention"
        : "backoff";
    }
    const tip = event.tip.point;
    const atTip =
      tip.kind === "point" &&
      cursor !== null &&
      cursor.point.slot === Number(tip.slot) &&
      cursor.point.hash.toString("hex") === tip.hash;
    // The cursor at the tip of an available node, as published.
    const atNodeTip = atTip && status.node === null;
    // The first report at the tip, with the cursor back at the height it
    // held before the reset, ends a reset's replay; a failed clear keeps
    // the reason and the next event at the tip tries again.
    let replaying = status.replaying;
    if (replaying && atNodeTip) {
      const ended = await store.endTrackedSetReplay();
      if (ended === "ended" || ended === "not_replaying") {
        replaying = false;
        log("the reset's replay reached the node tip");
      } else if (ended === "below_replay_height") {
        // The node's chain is shorter than the one the reset left: wait.
      } else if (ended.kind === "store_locked") return "relock";
      else
        log(`clearing the tracked-set replay failed: ${ended.error.message}`);
    }
    const nodeTip =
      tip.kind === "point"
        ? { slot: Number(tip.slot), height: Number(event.tip.blockNo) }
        : null;
    const nodeBehind = await behind.at(nodeTip);
    await publish({
      state: "following",
      waiting: null,
      stuck: null,
      events: status.events + 1,
      lastError: null,
      atTip: atNodeTip,
      replaying,
      tip: nodeTip,
      ...(nodeBehind === undefined ? {} : { nodeBehind }),
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

  /**
   * A failure the stream reopens from on its own: logged, and from
   * `STREAM_INTERRUPTED_AFTER` in a row a wait on the stream, so a reopen
   * loop is visible. The next applied event clears it.
   */
  const onInterrupted = (interruption: StreamInterruption): void => {
    const { consecutive, total, last } = interruption;
    if (consecutive <= STREAM_INTERRUPTED_AFTER || consecutive % 60 === 0)
      log(
        `chain-sync stream failed and is reopening (${consecutive} in a row, ${total} in all): ${last}`,
      );
    if (consecutive >= STREAM_INTERRUPTED_AFTER)
      void failed(
        "stream",
        `the chain-sync stream failed ${consecutive} times in a row and keeps reopening: ${last}`,
        "transient",
      );
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
        onLocked: async (locked) => {
          await failed("store_locked", locked.detail, "transient");
        },
      });
      if (started === undefined) break;
      if (started.kind !== "ready") {
        await stopOn(started);
        return finish();
      }
      const change = describeTrackedSetCheck(started.trackedSet);
      if (change !== null) log(change);
      await publish({ replaying: started.replaying });
      const begun = await startFromOrigin({
        store,
        transport: options.transport,
        origin: options.origin.origin,
        credit,
        onInterrupted,
      });
      if (begun.kind === "intervention") {
        await stopOn(begun);
        return finish();
      }
      if (begun.kind === "store_locked" || begun.kind === "error") {
        if (begun.kind === "error") {
          if (
            await failed(
              "store",
              begun.error.message,
              classifyFailure(begun.error),
            )
          )
            return finish();
        } else await failed("store_locked", begun.detail, "transient");
        await wait();
        continue;
      }
      let stream: ChainSyncStream;
      let first: ChainSyncEvent | undefined;
      if (begun.kind === "resume") {
        stream = options.transport.openChainSync({
          points: await intersectionPoints(store),
          credit,
          onInterrupted,
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
          return finish();
        }
        if (
          !signal.aborted &&
          (await failed("stream", message(error), classifyStreamFailure(error)))
        )
          outcome = "intervention";
        else outcome = "backoff";
      } finally {
        signal.removeEventListener("abort", onAbort);
        await stream.close();
      }
      if (outcome === "intervention") return finish();
      if (outcome === "relock") {
        await failed("store_locked", "store_locked", "transient");
        continue;
      }
      if (!signal.aborted) await wait();
    } catch (error) {
      // A follower failure must not reach the role: record it, and back off
      // and start again while it is transient.
      log(`follower failed: ${message(error)}`);
      // A migration the store refuses (a changed or unknown applied
      // migration) fails the same way on every start: stuck at once, named.
      const stopped =
        error instanceof FollowerMigrationError
          ? await failed("store", message(error), "deterministic", "migration")
          : await failed("store", message(error), classifyFailure(error));
      if (stopped) return finish();
      await wait();
    }
  }
  await publish({ state: "stopped" });
  return finish();
};
