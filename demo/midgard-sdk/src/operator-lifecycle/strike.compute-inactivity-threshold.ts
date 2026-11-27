import { type UTxO } from "@lucid-evolution/lucid";

import { type ActiveOperatorDatum } from "../active-operators.js";
import {
  MAX_INACTIVITY_STRIKES,
  MAX_VALIDITY_RANGE_LENGTH_MS,
  NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS,
  SHIFT_DURATION_MS,
  USER_EVENTS_NEGLIGENCE_TIMEOUT_MS,
} from "../protocol-parameters.js";
import {
  type ActiveOperatorNode,
  type OperatorDirectorySnapshot,
} from "./directory.js";
import type { NodeWithDatum } from "./layout.js";

// ---------------------------------------------------------------------------
// Timing parameters
// ---------------------------------------------------------------------------

export type InactivityTimingParameters = {
  readonly shiftDurationMs: bigint;
  readonly newShiftInactivityGracePeriodMs: bigint;
  readonly userEventsNegligenceTimeoutMs: bigint;
  readonly maxValidityRangeLengthMs: bigint;
  readonly maxInactivityStrikes: bigint;
};

export const DEFAULT_INACTIVITY_TIMING_PARAMETERS: InactivityTimingParameters =
  {
    shiftDurationMs: SHIFT_DURATION_MS,
    newShiftInactivityGracePeriodMs: NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS,
    userEventsNegligenceTimeoutMs: USER_EVENTS_NEGLIGENCE_TIMEOUT_MS,
    maxValidityRangeLengthMs: MAX_VALIDITY_RANGE_LENGTH_MS,
    maxInactivityStrikes: MAX_INACTIVITY_STRIKES,
  };

/**
 * A strike transaction must be a closed, short validity range that starts
 * strictly after the inactivity threshold. Two minutes leaves room for
 * submission latency while staying far below `max_validity_range_length`.
 */
export const DEFAULT_STRIKE_VALIDITY_WINDOW_MS = 120_000n;

// ---------------------------------------------------------------------------
// Inactivity threshold
// ---------------------------------------------------------------------------

export type NeglectedUserEventKind = "Deposit" | "Withdrawal" | "TxOrder";

/**
 * A user event the scheduled operator left undelivered: its inclusion time is
 * after the state-queue tail's `end_time`, so no committed block covers it. A
 * strike must cite one; an operator with no due user event cannot be struck.
 * The caller locates the UTxO itself: this module never scans the deposit,
 * withdrawal, or tx-order sets. For a deposit or withdrawal it is the event's Order node in that
 * kind's event-history list, which the scheduler authenticates by the list's
 * policy and address; for a tx order it is the tx-order UTxO.
 */
export type NeglectedUserEventClaim = {
  readonly kind: NeglectedUserEventKind;
  readonly utxo: UTxO;
  /**
   * `inclusion_time` of the event: the Order facts of a deposit or withdrawal
   * history node, or the tx-order datum.
   */
  readonly inclusionTimeMs: bigint;
};

/**
 * Which of the two terms of the on-chain `max(..)` decided the threshold.
 */
export type InactivityThresholdSource =
  | "new-shift-grace-period"
  | "neglected-user-event";

export type InactivityThresholdUnsatisfiableReason =
  /**
   * On-chain `expect event_inclusion_time > last_state_queue_elements_end_time`:
   * a committed block already covers the event.
   */
  | "neglected-event-delivered"
  /** On-chain `expect inactivity_threshold < shift_end_time(start)`. */
  | "threshold-not-before-shift-end";

export type InactivityThreshold =
  | {
      readonly kind: "threshold";
      readonly thresholdMs: bigint;
      readonly shiftEndMs: bigint;
      readonly source: InactivityThresholdSource;
    }
  | {
      readonly kind: "unsatisfiable";
      readonly reason: InactivityThresholdUnsatisfiableReason;
      readonly thresholdMs: bigint;
      readonly shiftEndMs: bigint;
      readonly detail: string;
    };

export type ComputeInactivityThresholdInput = {
  /** `start_time` of the scheduler's current `ActiveOperator` datum. */
  readonly shiftStartMs: bigint;
  /**
   * `end_time` of the last state-queue element (the root's confirmed state
   * when no block is committed).
   */
  readonly stateQueueTailEndTimeMs: bigint;
  readonly neglectedEvent: Pick<
    NeglectedUserEventClaim,
    "kind" | "inclusionTimeMs"
  >;
  readonly params?: InactivityTimingParameters;
};

/**
 * Reproduces the on-chain `inactivity_threshold`, exported on its own so a
 * status query can report the next strike time without planning a
 * transaction.
 *
 * On-chain (`validate_operator_inactivity_and_get_its_link`) the threshold is
 * `max(shift_start + new_shift_inactivity_grace_period, inclusion_time +
 * user_events_negligence_timeout)` for the cited user event, whose
 * `inclusion_time` must be after the state-queue tail's `end_time`. The
 * threshold must fall strictly before the end of the shift it convicts.
 */
export const computeInactivityThreshold = ({
  shiftStartMs,
  stateQueueTailEndTimeMs,
  neglectedEvent,
  params = DEFAULT_INACTIVITY_TIMING_PARAMETERS,
}: ComputeInactivityThresholdInput): InactivityThreshold => {
  const shiftEndMs = shiftStartMs + params.shiftDurationMs;
  const graceThresholdMs =
    shiftStartMs + params.newShiftInactivityGracePeriodMs;
  const eventThresholdMs =
    neglectedEvent.inclusionTimeMs + params.userEventsNegligenceTimeoutMs;
  const thresholdMs =
    graceThresholdMs >= eventThresholdMs ? graceThresholdMs : eventThresholdMs;
  const source: InactivityThresholdSource =
    graceThresholdMs >= eventThresholdMs
      ? "new-shift-grace-period"
      : "neglected-user-event";
  if (neglectedEvent.inclusionTimeMs <= stateQueueTailEndTimeMs) {
    return {
      kind: "unsatisfiable",
      reason: "neglected-event-delivered",
      thresholdMs,
      shiftEndMs,
      detail: `neglected ${neglectedEvent.kind} inclusion_time=${neglectedEvent.inclusionTimeMs.toString()} is not after state-queue tail end_time=${stateQueueTailEndTimeMs.toString()}, so a committed block covers it`,
    };
  }
  if (thresholdMs >= shiftEndMs) {
    return {
      kind: "unsatisfiable",
      reason: "threshold-not-before-shift-end",
      thresholdMs,
      shiftEndMs,
      detail: `inactivity threshold=${thresholdMs.toString()} is not before shift end=${shiftEndMs.toString()}`,
    };
  }
  return { kind: "threshold", thresholdMs, shiftEndMs, source };
};

// ---------------------------------------------------------------------------
// Takeover planner
// ---------------------------------------------------------------------------

/**
 * The scheduler's traversal is the active-operators list walked backwards: the
 * next shift belongs to the node whose `next` link points at the current
 * operator. When that anchor is the root, the current operator is the head and
 * the shift rewinds to the tail instead.
 */
export type InactivityTakeoverTier = "GoToNext" | "Rewind";

export type InactivityTakeoverWitnesses =
  | {
      readonly tier: "GoToNext";
      /** The node whose link points at the skipped operator. */
      readonly newOperatorNode: NodeWithDatum;
    }
  | {
      readonly tier: "Rewind";
      readonly activeRootNode: NodeWithDatum;
      /**
       * The list tail, which becomes the new shift's operator. `null` when the
       * skipped operator is the only active member: its node is being spent,
       * so it cannot also be referenced, and the shift rewinds onto itself.
       */
      readonly activeTailNode: NodeWithDatum | null;
      /**
       * The last registered-operators element, proving no registered operator
       * can activate before the strike's validity range ends.
       */
      readonly registeredWitnessNode: NodeWithDatum;
    };

export type InactivityTakeoverBlockedReason =
  | InactivityThresholdUnsatisfiableReason
  | "scheduled-operator-not-active"
  | "active-root-missing"
  | "successor-node-missing"
  | "active-tail-missing"
  | "registered-witness-missing"
  | "registered-operator-can-activate"
  | "validity-alignment-regressed"
  | "validity-window-too-long";

export type InactivityTakeoverValidity = {
  readonly validFrom: bigint;
  readonly validTo: bigint;
};

export type InactivityTakeoverPlan =
  /** The scheduler holds `NoActiveOperators`; there is no shift to strike. */
  | { readonly kind: "no-shift" }
  /**
   * No user event is undelivered: every deposit, withdrawal and tx order is
   * at or before the state-queue tail's `end_time`. An operator below the
   * strike cap with no due L1 work cannot be struck, however long its shift
   * has been idle.
   */
  | { readonly kind: "no-neglected-event"; readonly currentOperator: string }
  | {
      readonly kind: "not-yet";
      readonly currentOperator: string;
      readonly nowMs: bigint;
      readonly thresholdMs: bigint;
      readonly thresholdSource: InactivityThresholdSource;
      readonly shiftEndMs: bigint;
    }
  | {
      readonly kind: "blocked";
      readonly reason: InactivityTakeoverBlockedReason;
      readonly detail: string;
      readonly currentOperator: string;
    }
  /**
   * The node already carries `max_inactivity_strikes`, so another strike would
   * break `new_inactivity_strikes <= max_inactivity_strikes`. The operator has
   * to be force-retired instead, which the chain allows at any time and with
   * or without an undelivered event: forced retirement checks the strike
   * count alone.
   */
  | {
      readonly kind: "strikes-exhausted";
      readonly currentOperator: string;
      readonly skippedNode: ActiveOperatorNode;
      readonly inactivityStrikes: bigint;
      readonly maxInactivityStrikes: bigint;
      /** Start of the shift the exhausted operator holds. */
      readonly shiftStartMs: bigint;
    }
  | {
      readonly kind: "ready";
      readonly tier: InactivityTakeoverTier;
      readonly currentOperator: string;
      readonly shiftStartMs: bigint;
      readonly thresholdMs: bigint;
      readonly thresholdSource: InactivityThresholdSource;
      readonly shiftEndMs: bigint;
      readonly skippedNode: ActiveOperatorNode;
      readonly skippedOperatorDatum: ActiveOperatorDatum;
      readonly newOperatorKey: string;
      readonly witnesses: InactivityTakeoverWitnesses;
      readonly validity: InactivityTakeoverValidity;
      /** Equals `validTo - 1`, the inclusive upper bound the scheduler reads. */
      readonly newStartTime: bigint;
      readonly neglectedEvent: NeglectedUserEventClaim;
    };

/**
 * The slice of the directory snapshot the planner reads.
 */
export type InactivityDirectoryView = Pick<
  OperatorDirectorySnapshot,
  "active" | "registered" | "scheduler" | "stateQueueTail"
>;

export type PlanInactivityTakeoverInput = {
  readonly snapshot: InactivityDirectoryView;
  readonly nowMs: bigint;
  readonly params?: InactivityTimingParameters;
  /**
   * The undelivered user event the strike cites
   * (`selectNeglectedUserEvent`), or null when there is none.
   */
  readonly neglectedEvent: NeglectedUserEventClaim | null;
  readonly validityWindowMs?: bigint;
  /**
   * Cardano validity bounds are slots, so the POSIX times the validator reads
   * are the slot boundaries Lucid rounds to. Callers running against a chain
   * pass a ceiling-to-slot-boundary function here; it must never return a time
   * earlier than its argument, or the strike would claim a lower bound that is
   * not after the threshold. `validityWindowMs` must likewise be a whole
   * number of slots for `newStartTime` to survive the round trip.
   */
  readonly alignValidFrom?: (candidateMs: bigint) => bigint;
};

export const blocked = (
  reason: InactivityTakeoverBlockedReason,
  detail: string,
  currentOperator: string,
): InactivityTakeoverPlan => ({
  kind: "blocked",
  reason,
  detail,
  currentOperator,
});
