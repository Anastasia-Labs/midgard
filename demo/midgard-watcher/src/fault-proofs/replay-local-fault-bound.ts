import {
  createValidationTraceReplayHolds,
  LOCAL_FAULT_RUN_LIMIT,
  type ValidationTraceReplayHold,
} from "@al-ft/midgard-fault-proofs";

import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";

/** The readiness reason of a header or confirmed head held on a local
 * replay fault. */
export const WATCHER_REPLAY_LOCAL_FAULT = "replayed_ledger_local_fault";

/** The readiness reason of a hold whose local replay fault repeated
 * `LOCAL_FAULT_RUN_LIMIT` times in a row, whatever its message. Still
 * retried. */
export const WATCHER_REPLAY_LOCAL_FAULT_STUCK =
  "replayed_ledger_local_fault_stuck";

export type WatcherReplayLocalFaultHold = Readonly<{
  reason:
    | typeof WATCHER_REPLAY_LOCAL_FAULT
    | typeof WATCHER_REPLAY_LOCAL_FAULT_STUCK;
  detail: string;
}>;

/**
 * Holds on local replay faults (`ReplayLocalFault`: this watcher's own
 * replay failed, which says nothing about the block), per header or
 * confirmed head, in memory, for this process. A held key is retried after
 * the replay-hold backoff (1 s doubling to 60 s), at most one held key per
 * pass, for as long as its fault recurs. Once it failed
 * `LOCAL_FAULT_RUN_LIMIT` times in a row, whatever the messages, the hold
 * is named stuck, an alarm logged once at error level; it is still retried,
 * and the watcher keeps running and doing its other work.
 */
export const replayLocalFaultHolds = (now?: () => number) => {
  const holds = createValidationTraceReplayHolds({
    ...(now === undefined ? {} : { now }),
    stuckAfter: LOCAL_FAULT_RUN_LIMIT,
    countEvery: true,
    onStuck: (hold) =>
      process.stderr.write(
        `${JSON.stringify({
          packageName: WATCHER_PACKAGE_NAME,
          level: "error",
          reason: WATCHER_REPLAY_LOCAL_FAULT_STUCK,
          detail: `${hold.headerHash}: ${hold.detail}`,
        })}\n`,
      ),
  });
  const event = (key: string) => ({ headerHash: key, position: 0, txId: key });
  const named = (
    hold: ValidationTraceReplayHold,
  ): WatcherReplayLocalFaultHold =>
    Object.freeze({
      reason: hold.stuck
        ? WATCHER_REPLAY_LOCAL_FAULT_STUCK
        : WATCHER_REPLAY_LOCAL_FAULT,
      detail: hold.detail,
    });
  return Object.freeze({
    /** Starts one pass: the returned check names the hold that keeps `key`
     * from a retry in this pass, if any. */
    pass: () => {
      const pass = holds.pass();
      return (key: string): WatcherReplayLocalFaultHold | undefined => {
        const hold = pass.waiting(key, event(key));
        return hold === undefined ? undefined : named(hold);
      };
    },
    /** An attempt on `key` ended on a local fault: its hold. */
    fault: (key: string, detail: string): WatcherReplayLocalFaultHold =>
      named(holds.failed(key, { ...event(key), detail })),
    /** An attempt on `key` made progress past it: its hold is released. */
    progressed: (key: string): void => {
      holds.decided(key);
    },
  });
};
