import { Effect, Ref } from "effect";

import type { Globals } from "../services/globals.globals.js";
import {
  idleBackoffActive,
  recordIdleTick,
  resetIdleBackoff,
} from "../services/globals.idle-backoff.js";
import type { MergeActionResult } from "./merge.registered-merge-due-work-skip.js";

export const MERGE_IDLE_BACKOFF_KEY = "state_queue_merge";
/** The longest a provably idle scheduled merge waits between attempts. */
export const MERGE_IDLE_BACKOFF_MAX_MS = 60_000;

type MergeIdleGlobals = Pick<
  Globals,
  "BLOCKS_IN_QUEUE" | "HEARTBEAT_MERGE" | "IDLE_BACKOFF"
>;

/**
 * True when the scheduled merge tick should be skipped: its last attempt found
 * no queued block, the queue is still empty, and the backoff has not elapsed.
 * That attempt also caught up every landed merge, so skipping defers no
 * finalization. A skipped tick still refreshes the merge heartbeat.
 */
export const skipIdleMergeTick = (
  globals: MergeIdleGlobals,
): Effect.Effect<boolean> =>
  Effect.gen(function* () {
    if ((yield* Ref.get(globals.BLOCKS_IN_QUEUE)) > 0) {
      yield* resetIdleBackoff(globals, MERGE_IDLE_BACKOFF_KEY);
      return false;
    }
    if (!(yield* idleBackoffActive(globals, MERGE_IDLE_BACKOFF_KEY))) {
      return false;
    }
    yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
    return true;
  });

export const recordMergeTickIdleness = (
  globals: MergeIdleGlobals,
  result: MergeActionResult | undefined,
  baseMs: number,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    if (
      result?.status === "no_queued_block" &&
      (yield* Ref.get(globals.BLOCKS_IN_QUEUE)) === 0
    ) {
      yield* recordIdleTick(globals, MERGE_IDLE_BACKOFF_KEY, {
        baseMs,
        maxMs: MERGE_IDLE_BACKOFF_MAX_MS,
      });
    } else {
      yield* resetIdleBackoff(globals, MERGE_IDLE_BACKOFF_KEY);
    }
  });
