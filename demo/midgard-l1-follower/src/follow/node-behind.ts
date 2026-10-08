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
 */
export const watchNodeBehind = (
  options: NodeBehindOptions | undefined,
  log: (line: string) => void,
  current: () => FollowStatus,
  publish: (change: { nodeBehind: NodeBehind }) => Promise<void>,
): Readonly<{
  /** Undefined when the slot time could not be read: the last result stands. */
  at: (tip: FollowStatus["tip"]) => Promise<NodeBehind | undefined>;
  stop: () => void;
}> => {
  const at = async (
    tip: FollowStatus["tip"],
  ): Promise<NodeBehind | undefined> => {
    if (options === undefined || tip === null) return null;
    const boundMs = options.boundMs ?? DEFAULT_NODE_BEHIND_MS;
    try {
      const lagMs =
        (options.now ?? Date.now)() - (await options.slotTime(tip.slot));
      return lagMs > boundMs ? { tipSlot: tip.slot, lagMs, boundMs } : null;
    } catch (error) {
      log(`node-behind check failed: ${message(error)}`);
      return undefined;
    }
  };
  const recheck = async (): Promise<void> => {
    const nodeBehind = await at(current().tip);
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
