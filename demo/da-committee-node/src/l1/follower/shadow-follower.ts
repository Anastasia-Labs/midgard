import { rename, writeFile } from "node:fs/promises";

import {
  type ChainSyncEvent,
  type ChainSyncStream,
  IntersectNotFoundError,
  type L1NodeTransport,
} from "@al-ft/l1-node-transport";
import {
  applyChainSyncEvent,
  type FactStore,
  intersectionFailure,
  intersectionPoints,
  type Intervention,
  type InterventionReason,
  type OriginConfig,
  protocolInitStatus,
  startFromOrigin,
  startWhenFree,
  stepLocked,
  stepSettled,
  storePoint,
} from "@al-ft/midgard-l1-follower";

/**
 * The committee follower's shadow status (phase A), written to `statusPath`.
 * Interventions are recorded here and nowhere else: in phase A they never
 * touch `/readyz`, and the current L1 code stays authoritative.
 */
export type ShadowFollowerStatus = Readonly<{
  schema: "committee-l1-follower-shadow-v1";
  /**
   * `following`: applying chain-sync events. `waiting`: a transient
   * condition (the writer lease, a stream failure) it backs off from.
   * `intervention`: stopped on a condition only an operator clears; the
   * process stays up. `stopped`: asked to stop.
   */
  state: "starting" | "following" | "waiting" | "intervention" | "stopped";
  /** The interventions in force (R1 to R5, `origin_mismatch`). */
  interventions: readonly Readonly<{
    reason: InterventionReason;
    detail: string;
  }>[];
  /** Whether the protocol-init tx (the hubOracleOneShot spend) is in the facts. */
  protocolInit: "seen" | "pending" | "unknown";
  cursor: Readonly<{ slot: number; height: number; generation: number }> | null;
  /** Events applied by this process. */
  events: number;
  /** The latest transient failure, cleared by the next applied event. */
  lastError: string | null;
  updatedAt: string;
}>;

export type ShadowFollowerOptions = Readonly<{
  store: FactStore;
  transport: Pick<L1NodeTransport, "openChainSync">;
  origin: OriginConfig;
  /** Written atomically (temp file and rename) on every status change. */
  statusPath: string;
  signal: AbortSignal;
  /** The chain-sync stream's credit (default 64). */
  credit?: number;
  backoffMs?: Readonly<{ initial: number; max: number }>;
  /** Stamps the status file only; no L1 decision reads it. */
  now?: () => Date;
  log?: (line: string) => void;
}>;

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

/** What one event did to the follow loop. */
type Handled = "continue" | "relock" | "backoff" | "intervention";

/**
 * Follows the chain from the configured origin into the store (F3:
 * `startFromOrigin`, then `protocolInitStatus` at every tip), keeping the
 * committee projections current, and records what happens in the status
 * file. It never throws and never exits the process: `store_locked` and
 * stream failures back off and start again; an intervention stops this loop
 * and stays recorded until the operator acts and the process restarts.
 */
export const runShadowFollower = async (
  options: ShadowFollowerOptions,
): Promise<void> => {
  const { store, signal } = options;
  const backoff = options.backoffMs ?? { initial: 500, max: 30_000 };
  const now = options.now ?? (() => new Date());
  const log = options.log ?? (() => undefined);
  let status: ShadowFollowerStatus = {
    schema: "committee-l1-follower-shadow-v1",
    state: "starting",
    interventions: [],
    protocolInit: "unknown",
    cursor: null,
    events: 0,
    lastError: null,
    updatedAt: now().toISOString(),
  };
  let writtenShape = "";
  let writtenEvents = -1;
  /**
   * Records `change`. A change of anything but the cursor and the event count
   * is written at once; progress alone is written at the tip, or every 100
   * events while catching up.
   */
  const write = async (
    change: Partial<Omit<ShadowFollowerStatus, "schema" | "updatedAt">>,
    atTip = false,
  ): Promise<void> => {
    status = { ...status, ...change };
    const shape = JSON.stringify({
      ...status,
      updatedAt: null,
      cursor: null,
      events: null,
    });
    if (shape === writtenShape && status.events === writtenEvents) return;
    if (shape === writtenShape && !atTip && status.events - writtenEvents < 100)
      return;
    writtenShape = shape;
    writtenEvents = status.events;
    status = { ...status, updatedAt: now().toISOString() };
    try {
      await writeFile(`${options.statusPath}.tmp`, JSON.stringify(status));
      await rename(`${options.statusPath}.tmp`, options.statusPath);
    } catch (error) {
      log(`shadow status not written: ${message(error)}`);
    }
  };
  const stopOn = async (found: Intervention): Promise<void> => {
    log(`intervention ${found.reason}: ${found.detail}`);
    await write({
      state: "intervention",
      interventions: [
        ...status.interventions.filter((i) => i.reason !== found.reason),
        { reason: found.reason, detail: found.detail },
      ],
    });
  };
  const handle = async (event: ChainSyncEvent): Promise<Handled> => {
    const step = await applyChainSyncEvent(store, event);
    if (stepLocked(step)) return "relock";
    if (!stepSettled(step)) {
      const result = step.result;
      if (result.kind === "intervention") {
        await stopOn(result);
        return "intervention";
      }
      await write({
        state: "waiting",
        lastError: `${event.kind}: ${result.kind === "error" ? result.error.message : "detail" in result ? result.detail : result.kind}`,
      });
      return "backoff";
    }
    const cursor = await store.cursor();
    await write(
      {
        state: "following",
        events: status.events + 1,
        lastError: null,
        cursor:
          cursor === null
            ? null
            : {
                slot: cursor.point.slot,
                height: cursor.height,
                generation: cursor.generation,
              },
      },
      cursor !== null && BigInt(cursor.height) === event.tip.blockNo,
    );
    if (event.tip.point.kind === "point") {
      const init = await protocolInitStatus(
        store,
        options.origin,
        storePoint(event.tip.point),
      );
      if (init.kind === "error")
        await write({ lastError: `protocol init: ${init.error.message}` });
      else if (init.kind === "intervention") {
        // R3 does not stop the follower: it keeps following, and only a
        // later check that finds the spend (after a reorg) clears it.
        if (!status.interventions.some((i) => i.reason === init.reason))
          log(`intervention ${init.reason}: ${init.detail}`);
        await write({
          protocolInit: "pending",
          interventions: [
            ...status.interventions.filter((i) => i.reason !== init.reason),
            { reason: init.reason, detail: init.detail },
          ],
        });
      } else if (init.kind === "pending")
        // Catching up again does not undo a caught-up miss.
        await write({ protocolInit: "pending" });
      else
        await write({
          protocolInit: "seen",
          interventions: status.interventions.filter(
            (i) => i.reason !== "origin_after_protocol_init",
          ),
        });
    }
    return "continue";
  };
  let delay = backoff.initial;
  const wait = async (): Promise<void> => {
    await sleep(delay, signal);
    delay = Math.min(delay * 2, backoff.max);
  };
  while (!signal.aborted) {
    try {
      const started = await startWhenFree(store, {
        signal,
        backoffMs: backoff,
        log,
      });
      if (started === undefined) break;
      if (started.kind !== "ready") return await stopOn(started);
      const begun = await startFromOrigin({
        store,
        transport: options.transport,
        origin: options.origin.origin,
        credit: options.credit ?? 64,
      });
      if (begun.kind === "intervention") return await stopOn(begun);
      if (begun.kind === "store_locked" || begun.kind === "error") {
        await write({
          state: "waiting",
          lastError:
            begun.kind === "error" ? begun.error.message : begun.detail,
        });
        await wait();
        continue;
      }
      let stream: ChainSyncStream;
      let first: ChainSyncEvent | undefined;
      if (begun.kind === "resume") {
        stream = options.transport.openChainSync({
          points: await intersectionPoints(store),
          credit: options.credit ?? 64,
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
        if (error instanceof IntersectNotFoundError)
          return await stopOn(
            intersectionFailure(
              error,
              await store.cursor(),
              options.origin.origin,
            ),
          );
        if (!signal.aborted)
          await write({ state: "waiting", lastError: message(error) });
        outcome = "backoff";
      } finally {
        signal.removeEventListener("abort", onAbort);
        await stream.close();
      }
      if (outcome === "intervention") return;
      if (outcome === "relock") {
        await write({ state: "waiting", lastError: "store_locked" });
        continue;
      }
      if (!signal.aborted) await wait();
    } catch (error) {
      // A follower failure must not reach the committee: record it, back
      // off and start again.
      log(`shadow follower failed, starting again: ${message(error)}`);
      await write({ state: "waiting", lastError: message(error) });
      await wait();
    }
  }
  await write({ state: "stopped" });
};
