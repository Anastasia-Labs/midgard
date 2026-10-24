/**
 * The hold on a foreign block whose event replay failed locally
 * (`replay_held`: a `ReplayLocalFault` marked `held`, which the block's input
 * can reach). It is never bounded by an exit: the block is replayed again
 * once its backoff (1 s doubling to 60 s) passed, one attempt per run, for
 * as long as the fault recurs, and every run before that holds on it without
 * replaying. After `LOCAL_FAULT_RUN_LIMIT` failures in a row on the block,
 * whatever their message, the hold is `replay_stuck`, still retried; the
 * node keeps running.
 */
import {
  createValidationTraceReplayHolds,
  LOCAL_FAULT_RUN_LIMIT,
} from "@al-ft/midgard-fault-proofs";
import { Effect } from "effect";

import type { ReplayInput, ReplayOutcome } from "./replay.js";

/** `replay`, holding each block whose event replay failed locally. Holds
 * live in memory, for this process. */
export const heldReplays = <E, R>(
  replay: (input: ReplayInput) => Effect.Effect<ReplayOutcome, E, R>,
  now?: () => number,
): ((input: ReplayInput) => Effect.Effect<ReplayOutcome, E, R>) => {
  const holds = createValidationTraceReplayHolds({
    ...(now === undefined ? {} : { now }),
    stuckAfter: LOCAL_FAULT_RUN_LIMIT,
    countEvery: true,
  });
  const event = (headerHash: string) => ({
    headerHash,
    position: 0,
    txId: headerHash,
  });
  const named = (hold: Readonly<{ stuck: boolean; detail: string }>) =>
    ({
      kind: hold.stuck ? "replay_stuck" : "replay_held",
      detail: hold.detail,
    }) satisfies ReplayOutcome;
  return (input) =>
    Effect.suspend(() => {
      const { headerHash } = input;
      const waiting = holds.pass().waiting(headerHash, event(headerHash));
      if (waiting !== undefined) return Effect.succeed(named(waiting));
      return Effect.map(replay(input), (outcome): ReplayOutcome => {
        if (outcome.kind === "replay_held")
          return named(
            holds.failed(headerHash, {
              ...event(headerHash),
              detail: outcome.detail,
            }),
          );
        if (outcome.kind === "replayed") holds.decided(headerHash);
        return outcome;
      });
    });
};
