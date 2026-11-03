import {
  DEFAULT_NODE_BEHIND_MS,
  type FollowStatus,
  NODE_BEHIND_CHECK_EVERY_MS,
  type NodeBehindOptions,
} from "./status.js";

type NodeBehind = FollowStatus["nodeBehind"];

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * The loop's node-behind check (`FOLLOWER_NODE_BEHIND`): whether the node's
 * tip is behind wall-clock time by more than the bound (§9 Time: the wall
 * clock only holds). `at` is checked on every applied event; while none
 * arrive (a stalled node at the cursor) a timer notices the tip aging and
 * publishes only while behind or when the result changes. `stop` clears
 * the timer.
 *
 * It never waits on the slot time. A `slotTime` that answers with a promise
 * (the slot configuration is still being read from the node, maybe queued
 * behind a ledger read) makes the lag unknown (`lagMs: null`), which is not
 * ready; the result is judged and published when the promise lands. A
 * failed read (a throw or a rejection) is logged and skips that check: the
 * last judged result stands, and the next check reads again.
 */
export const watchNodeBehind = (
  options: NodeBehindOptions | undefined,
  log: (line: string) => void,
  current: () => FollowStatus,
  publish: (change: { nodeBehind: NodeBehind }) => Promise<void>,
): Readonly<{
  /** Undefined when `slotTime` threw: the last result stands. */
  at: (tip: FollowStatus["tip"]) => NodeBehind | undefined;
  stop: () => void;
}> => {
  const boundMs = options?.boundMs ?? DEFAULT_NODE_BEHIND_MS;
  /** The last judged result: what stands when a read fails. */
  let known: NodeBehind = null;
  const judge = (
    tip: NonNullable<FollowStatus["tip"]>,
    startMs: number,
  ): NodeBehind => {
    const lagMs = (options?.now ?? Date.now)() - startMs;
    known = lagMs > boundMs ? { tipSlot: tip.slot, lagMs, boundMs } : null;
    return known;
  };
  /** A slot time being read; at most one is followed at a time. */
  let reading = false;
  const land = (
    tip: NonNullable<FollowStatus["tip"]>,
    startMs: Promise<number>,
  ): void => {
    reading = true;
    startMs.then(
      (ms) => {
        reading = false;
        if (current().tip?.slot === tip.slot)
          void publish({ nodeBehind: judge(tip, ms) });
        else void recheck();
      },
      (error: unknown) => {
        reading = false;
        log(`node-behind check failed: ${message(error)}`);
        if (current().nodeBehind?.lagMs === null)
          void publish({ nodeBehind: known });
      },
    );
  };
  const at = (tip: FollowStatus["tip"]): NodeBehind | undefined => {
    if (options === undefined || tip === null) return null;
    let startMs: number | Promise<number>;
    try {
      startMs = options.slotTime(tip.slot);
    } catch (error) {
      log(`node-behind check failed: ${message(error)}`);
      return undefined;
    }
    if (typeof startMs === "number") return judge(tip, startMs);
    if (reading) startMs.catch(() => undefined);
    else land(tip, startMs);
    return { tipSlot: tip.slot, lagMs: null, boundMs };
  };
  const recheck = async (): Promise<void> => {
    const nodeBehind = at(current().tip);
    if (
      nodeBehind === undefined ||
      (nodeBehind === null && current().nodeBehind === null)
    )
      return;
    await publish({ nodeBehind });
  };
  const timer =
    options === undefined
      ? undefined
      : setInterval(
          () => void recheck(),
          options.checkEveryMs ?? NODE_BEHIND_CHECK_EVERY_MS,
        );
  timer?.unref();
  return { at, stop: () => clearInterval(timer) };
};
