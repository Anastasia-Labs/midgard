import { type UTxO } from "@lucid-evolution/lucid";

import { type ActiveOperatorDatum } from "../active-operators.js";
import {
  MAX_INACTIVITY_BETWEEN_BLOCK_COMMITMENTS_MS,
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
  readonly maxInactivityBetweenBlockCommitmentsMs: bigint;
  readonly userEventsNegligenceTimeoutMs: bigint;
  readonly maxValidityRangeLengthMs: bigint;
  readonly maxInactivityStrikes: bigint;
};

export const DEFAULT_INACTIVITY_TIMING_PARAMETERS: InactivityTimingParameters =
  {
    shiftDurationMs: SHIFT_DURATION_MS,
    newShiftInactivityGracePeriodMs: NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS,
    maxInactivityBetweenBlockCommitmentsMs:
      MAX_INACTIVITY_BETWEEN_BLOCK_COMMITMENTS_MS,
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
 * A user event the scheduled operator left unprocessed. The caller locates the
 * UTxO itself: this module never scans the deposit, withdrawal, or tx-order
 * sets. For a deposit or withdrawal it is the event's Order node in that
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
 * Which of the three terms of the on-chain `max(..)` decided the threshold.
 */
export type InactivityThresholdSource =
  | "new-shift-grace-period"
  | "block-commitment-gap"
  | "neglected-user-event";

export type InactivityThresholdUnsatisfiableReason =
  /** On-chain `expect inclusion_time >= last_state_queue_elements_end_time`. */
  | "neglected-event-precedes-state-queue-tail"
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
  readonly neglectedEvent?: Pick<
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
 * `max(shift_start + new_shift_inactivity_grace_period, X)`, where `X` is the
 * state-queue tail's `end_time + max_inactivity_between_block_commitments`
 * for `NoNeglectedUserEvent`, or the referenced user event's `inclusion_time +
 * user_events_negligence_timeout` for the three neglected variants. The
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
    neglectedEvent === undefined
      ? stateQueueTailEndTimeMs + params.maxInactivityBetweenBlockCommitmentsMs
      : neglectedEvent.inclusionTimeMs + params.userEventsNegligenceTimeoutMs;
  const thresholdMs =
    graceThresholdMs >= eventThresholdMs ? graceThresholdMs : eventThresholdMs;
  const source: InactivityThresholdSource =
    graceThresholdMs >= eventThresholdMs
      ? "new-shift-grace-period"
      : neglectedEvent === undefined
        ? "block-commitment-gap"
        : "neglected-user-event";
  if (
    neglectedEvent !== undefined &&
    neglectedEvent.inclusionTimeMs < stateQueueTailEndTimeMs
  ) {
    return {
      kind: "unsatisfiable",
      reason: "neglected-event-precedes-state-queue-tail",
      thresholdMs,
      shiftEndMs,
      detail: `neglected ${neglectedEvent.kind} inclusion_time=${neglectedEvent.inclusionTimeMs.toString()} precedes state-queue tail end_time=${stateQueueTailEndTimeMs.toString()}`,
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
   * to be force-retired instead.
   */
  | {
      readonly kind: "strikes-exhausted";
      readonly currentOperator: string;
      readonly skippedNode: ActiveOperatorNode;
      readonly inactivityStrikes: bigint;
      readonly maxInactivityStrikes: bigint;
      readonly thresholdMs: bigint;
      readonly shiftEndMs: bigint;
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
      readonly neglectedEvent: NeglectedUserEventClaim | undefined;
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
  readonly neglectedEvent?: NeglectedUserEventClaim;
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
