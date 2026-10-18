/**
 * The hold a foreign block's replay outcome (other than `replayed`) stops
 * landed-block processing at, and the alarm a local failure raises.
 */
import { Effect, Metric } from "effect";

import {
  type DriverHold,
  localFault,
  notRetried,
} from "../l1-events/driver.js";
import {
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_DA_REFETCH_PENDING,
  LANDED_BLOCK_EVENT_UNKNOWN,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
  LANDED_BLOCK_VERIFIER_UNAVAILABLE,
} from "./holds.js";
import type { ReplayOutcome } from "./replay.js";

const HOLD_OF = {
  missing: LANDED_BLOCK_AWAITING_DA,
  da_refetch_pending: LANDED_BLOCK_DA_REFETCH_PENDING,
  event_unknown: LANDED_BLOCK_EVENT_UNKNOWN,
  forced_order_pending: LANDED_BLOCK_FORCED_ORDER_PENDING,
  local_fault: LANDED_BLOCK_LOCAL_FAULT,
  incomplete: LANDED_BLOCK_REPLAY_INCOMPLETE,
  verifier_unavailable: LANDED_BLOCK_VERIFIER_UNAVAILABLE,
  invalid: LANDED_BLOCK_INVALID,
} as const satisfies Record<Exclude<ReplayOutcome["kind"], "replayed">, string>;

export type ReplayHoldKind = keyof typeof HOLD_OF;

/**
 * Only `invalid` (a verdict: the same view replays to the same answer) is
 * `notRetried`. Every other outcome is retried on the driver's backoff. A
 * local failure (`local_fault`, `incomplete`, `verifier_unavailable`: this
 * node's own computation, or its verifier, failed), and an event the facts
 * lack where they do not cover the block's range (`event_unknown`: a gap in
 * this node's facts), is a `localFault` on block `headerHash`, the block it
 * stopped at, whatever the failure's text: retried within the runner's
 * bound.
 */
export const replayOutcomeHold = (
  kind: ReplayHoldKind,
  detail: string,
  headerHash: string,
): DriverHold => {
  const hold: DriverHold = { reason: HOLD_OF[kind], detail };
  return kind === "invalid"
    ? notRetried(hold)
    : kind === "local_fault" ||
        kind === "incomplete" ||
        kind === "verifier_unavailable" ||
        kind === "event_unknown"
      ? localFault(hold, headerHash)
      : hold;
};

/** Foreign-block imports that failed locally, by outcome kind. */
export const landedBlockLocalFaultCounter = Metric.counter(
  "landed_block_local_fault_total",
  {
    description:
      "Foreign landed-block imports that failed in this node's own computation, by outcome kind",
    bigint: true,
    incremental: true,
  },
);

/**
 * Raises the `landed_block_local_fault` alarm for an import that failed
 * locally (`local_fault`, `incomplete`), once per failed attempt; any other
 * outcome raises nothing.
 */
export const replayOutcomeAlarm = (
  headerHash: string,
  kind: ReplayHoldKind,
  detail: string,
): Effect.Effect<void> =>
  kind === "local_fault" || kind === "incomplete"
    ? Effect.logError("LandedBlockLocalFault").pipe(
        Effect.annotateLogs({
          alarm: LANDED_BLOCK_LOCAL_FAULT,
          headerHash,
          kind,
          detail,
        }),
        Effect.zipRight(
          Metric.increment(
            Metric.tagged(landedBlockLocalFaultCounter, "kind", kind),
          ),
        ),
      )
    : Effect.void;
