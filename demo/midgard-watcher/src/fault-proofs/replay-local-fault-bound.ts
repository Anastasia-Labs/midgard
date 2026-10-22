import { LOCAL_FAULT_RUN_LIMIT } from "@al-ft/midgard-fault-proofs";

/**
 * A local replay fault (`ReplayLocalFault`: this watcher's own replay
 * failed, which says nothing about the block) is held and retried, but not
 * without bound. `LOCAL_FAULT_RUN_LIMIT` attempts in a row on one header or
 * confirmed head that end on one, with no progress past it in between, fail
 * with this error: the decision driver's `exhausted` rejects with it,
 * `runtime.done` rejects, and the process exits non-zero, its supervisor's
 * restart being the retry after that.
 */
export class WatcherReplayLocalFaultExhaustedError extends Error {
  override readonly name = "WatcherReplayLocalFaultExhaustedError";
  readonly reason = "replayed_ledger_local_fault" as const;
  constructor(key: string, detail: string) {
    super(
      `replayed_ledger_local_fault: ${key}: ${detail}; a local replay fault persisted for ${LOCAL_FAULT_RUN_LIMIT.toString()} attempts in a row and the watcher exits non-zero`,
    );
  }
}

const MAXIMUM_COUNTED_KEYS = 4_096;

/**
 * Counts the attempts in a row that ended on a local replay fault, per
 * header or confirmed head, in memory, for this process.
 */
export const replayLocalFaultBound = () => {
  const runs = new Map<string, number>();
  return Object.freeze({
    /** An attempt on `key` ended on a local fault; throws
     * `WatcherReplayLocalFaultExhaustedError` on the limit-th in a row. */
    fault: (key: string, detail: string): void => {
      const count = (runs.get(key) ?? 0) + 1;
      runs.delete(key);
      if (count >= LOCAL_FAULT_RUN_LIMIT)
        throw new WatcherReplayLocalFaultExhaustedError(key, detail);
      if (runs.size >= MAXIMUM_COUNTED_KEYS)
        runs.delete(runs.keys().next().value!);
      runs.set(key, count);
    },
    /** An attempt on `key` made progress past it: its count starts over. */
    progressed: (key: string): void => {
      runs.delete(key);
    },
  });
};

/**
 * The decision driver's exhaustion signal: `exhausted` rejects with the
 * first `WatcherReplayLocalFaultExhaustedError` a pass failed with, and
 * stays pending otherwise.
 */
export const replayLocalFaultExhaustion = () => {
  let reject!: (error: WatcherReplayLocalFaultExhaustedError) => void;
  const exhausted = new Promise<never>((_resolve, rejectExhausted) => {
    reject = rejectExhausted;
  });
  // Observed by the runtime's liveness race; never an unhandled rejection.
  void exhausted.catch(() => undefined);
  return Object.freeze({
    exhausted,
    /** Rejects `exhausted` when `error` is the exhaustion; else nothing. */
    raise: (error: unknown): void => {
      if (error instanceof WatcherReplayLocalFaultExhaustedError) reject(error);
    },
  });
};
