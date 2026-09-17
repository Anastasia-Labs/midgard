/**
 * Stalled-operator strike and takeover.
 *
 * L1 never acts on its own: when the scheduled operator misses its shift,
 * somebody else has to submit a transaction that spends the scheduler with a
 * skipped-operator redeemer *and* spends the inactive operator's active node
 * with `StrikeForInactivity`. That single transaction reproduces the node with
 * one more inactivity strike and hands the shift to the next operator.
 *
 * This module carries the two halves of that endpoint:
 *
 * - `computeInactivityThreshold` / `planInactivityTakeover`: a pure mirror of
 *   the on-chain timing rules (`validators/scheduler.ak`,
 *   `validate_operator_inactivity_and_get_its_link`) so a watchdog can decide
 *   *whether* a strike is possible, and report why not when it is not.
 * - `buildStrikeInactiveOperatorTxProgram`: the transaction builder, which
 *   resolves every redeemer index from the final transaction context rather
 *   than guessing at it.
 */
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer as ActiveOperatorSpendRedeemerSchema,
  type ActiveOperatorSpendRedeemer as ActiveOperatorSpendRedeemerType,
  castActiveOperatorDatumToData,
} from "../active-operators.js";
import type { AuthenticatedValidator } from "../common.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "../linked-list.js";
import {
  MAX_INACTIVITY_BETWEEN_BLOCK_COMMITMENTS_MS,
  MAX_INACTIVITY_STRIKES,
  MAX_VALIDITY_RANGE_LENGTH_MS,
  NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS,
  SHIFT_DURATION_MS,
  USER_EVENTS_NEGLIGENCE_TIMEOUT_MS,
} from "../protocol-parameters.js";
import {
  SCHEDULER_ASSET_NAME,
  type SchedulerDatum,
  SchedulerError,
  type SchedulerSpendRedeemer,
  SchedulerSpendRedeemer as SchedulerSpendRedeemerSchema,
} from "../scheduler.js";
import { encodeSchedulerDatumForChain } from "../scheduler-refresh.js";
import { completeOptionsWithLocalEval } from "../tx-completion.js";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import {
  type ActiveOperatorNode,
  findAnchorNodeForKey,
  findNodeByKey,
  findRootNode,
  findTailNode,
  type OperatorDirectorySnapshot,
  registeredNodeKeyToPosixTime,
  schedulerCurrentOperator,
} from "./directory.js";
import type { NodeWithDatum } from "./layout.js";
import {
  outputMatchesElement,
  requirePolicyNftUnit,
} from "./output-selectors.js";

// ---------------------------------------------------------------------------
// Timing parameters
// ---------------------------------------------------------------------------

/**
 * The compiled-in timing constants the inactivity rules read. They mirror
 * `onchain/aiken/env/testnet.ak`; a deployment that compiled different values
 * (`env/default.ak` has a 30 ms shift) must pass its own set rather than rely
 * on the defaults.
 */
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
 * sets.
 */
export type NeglectedUserEventClaim = {
  readonly kind: NeglectedUserEventKind;
  readonly utxo: UTxO;
  /** `inclusion_time` of the event's datum. */
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

const blocked = (
  reason: InactivityTakeoverBlockedReason,
  detail: string,
  currentOperator: string,
): InactivityTakeoverPlan => ({
  kind: "blocked",
  reason,
  detail,
  currentOperator,
});

/**
 * Decides whether the scheduled operator can be struck for inactivity right
 * now and, when it can, returns every witness and bound the strike builder
 * needs.
 */
export const planInactivityTakeover = ({
  snapshot,
  nowMs,
  params = DEFAULT_INACTIVITY_TIMING_PARAMETERS,
  neglectedEvent,
  validityWindowMs = DEFAULT_STRIKE_VALIDITY_WINDOW_MS,
  alignValidFrom,
}: PlanInactivityTakeoverInput): InactivityTakeoverPlan => {
  const current = schedulerCurrentOperator(snapshot.scheduler);
  if (current === null) {
    return { kind: "no-shift" };
  }
  const threshold = computeInactivityThreshold({
    shiftStartMs: current.startTime,
    stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
    neglectedEvent,
    params,
  });
  if (threshold.kind === "unsatisfiable") {
    return blocked(threshold.reason, threshold.detail, current.operator);
  }
  const skippedNode = findNodeByKey(snapshot.active, current.operator);
  if (skippedNode === undefined || skippedNode.active === null) {
    return blocked(
      "scheduled-operator-not-active",
      `scheduled operator ${current.operator} has no active-operators node`,
      current.operator,
    );
  }
  if (skippedNode.active.inactivity_strikes >= params.maxInactivityStrikes) {
    return {
      kind: "strikes-exhausted",
      currentOperator: current.operator,
      skippedNode,
      inactivityStrikes: skippedNode.active.inactivity_strikes,
      maxInactivityStrikes: params.maxInactivityStrikes,
      thresholdMs: threshold.thresholdMs,
      shiftEndMs: threshold.shiftEndMs,
    };
  }
  if (nowMs <= threshold.thresholdMs) {
    return {
      kind: "not-yet",
      currentOperator: current.operator,
      nowMs,
      thresholdMs: threshold.thresholdMs,
      thresholdSource: threshold.source,
      shiftEndMs: threshold.shiftEndMs,
    };
  }

  const earliestValidFrom =
    nowMs > threshold.thresholdMs + 1n ? nowMs : threshold.thresholdMs + 1n;
  const validFrom = alignValidFrom?.(earliestValidFrom) ?? earliestValidFrom;
  if (validFrom < earliestValidFrom) {
    return blocked(
      "validity-alignment-regressed",
      `aligned valid_from=${validFrom.toString()} is earlier than the earliest permitted ${earliestValidFrom.toString()}`,
      current.operator,
    );
  }
  const validTo = validFrom + validityWindowMs;
  const newStartTime = validTo - 1n;
  if (newStartTime - validFrom > params.maxValidityRangeLengthMs) {
    return blocked(
      "validity-window-too-long",
      `validity range ${validFrom.toString()}..${newStartTime.toString()} exceeds max_validity_range_length=${params.maxValidityRangeLengthMs.toString()}`,
      current.operator,
    );
  }

  const anchor = findAnchorNodeForKey(snapshot.active, current.operator);
  if (anchor === undefined) {
    return blocked(
      "successor-node-missing",
      `no active-operators element links to ${current.operator}`,
      current.operator,
    );
  }
  const validity: InactivityTakeoverValidity = { validFrom, validTo };
  const readyBase = {
    kind: "ready" as const,
    currentOperator: current.operator,
    shiftStartMs: current.startTime,
    thresholdMs: threshold.thresholdMs,
    thresholdSource: threshold.source,
    shiftEndMs: threshold.shiftEndMs,
    skippedNode,
    skippedOperatorDatum: skippedNode.active,
    validity,
    newStartTime,
    neglectedEvent,
  };

  if (anchor.datum.key !== "Empty") {
    return {
      ...readyBase,
      tier: "GoToNext",
      newOperatorKey: anchor.datum.key.Key.key,
      witnesses: { tier: "GoToNext", newOperatorNode: anchor },
    };
  }

  // The root anchors the skipped operator, so the shift rewinds to the tail.
  const activeRootNode = findRootNode(snapshot.active);
  if (activeRootNode === undefined) {
    return blocked(
      "active-root-missing",
      "the active-operators list has no root element",
      current.operator,
    );
  }
  const activeTailNode = findTailNode(snapshot.active);
  if (activeTailNode === undefined) {
    return blocked(
      "active-tail-missing",
      "the active-operators list has no tail element",
      current.operator,
    );
  }
  const registeredWitnessNode = findTailNode(snapshot.registered);
  if (registeredWitnessNode === undefined) {
    return blocked(
      "registered-witness-missing",
      "the registered-operators list has no tail element",
      current.operator,
    );
  }
  const registeredWitnessActivation = registeredNodeKeyToPosixTime(
    registeredWitnessNode.datum.key,
  );
  if (
    registeredWitnessActivation !== undefined &&
    registeredWitnessActivation <= newStartTime
  ) {
    return blocked(
      "registered-operator-can-activate",
      `registered witness activation_time=${registeredWitnessActivation.toString()} is not after the strike validity upper bound ${newStartTime.toString()}`,
      current.operator,
    );
  }
  const skippedIsOnlyMember =
    activeTailNode.utxo.txHash === skippedNode.utxo.txHash &&
    activeTailNode.utxo.outputIndex === skippedNode.utxo.outputIndex;
  const newOperatorKey = skippedIsOnlyMember
    ? current.operator
    : activeTailNode.datum.key === "Empty"
      ? undefined
      : activeTailNode.datum.key.Key.key;
  if (newOperatorKey === undefined) {
    return blocked(
      "active-tail-missing",
      "the active-operators tail is the root, so no operator can take the shift",
      current.operator,
    );
  }
  return {
    ...readyBase,
    tier: "Rewind",
    newOperatorKey,
    witnesses: {
      tier: "Rewind",
      activeRootNode,
      activeTailNode: skippedIsOnlyMember ? null : activeTailNode,
      registeredWitnessNode,
    },
  };
};

// ---------------------------------------------------------------------------
// Strike transaction builder
// ---------------------------------------------------------------------------

export type StrikeNeglectedUserEvent = {
  readonly kind: NeglectedUserEventKind;
  readonly utxo: UTxO;
};

/**
 * Deliberately malformed redeemer fields, for negative tests that have to
 * reach a specific on-chain check. Honest callers leave this undefined; no
 * production path sets it.
 */
export type StrikeInactiveOperatorAdversarialOverrides = {
  /** Replaces the `active_node_link` the redeemer claims. */
  readonly activeNodeLink?: string | null;
  /** Replaces the `neglected_user_event` reference-input index. */
  readonly neglectedUserEventRefInputIndex?: bigint;
};

export type BuildStrikeInactiveOperatorTxConfig = {
  readonly lucid: LucidEvolution;
  readonly scheduler: AuthenticatedValidator;
  readonly activeOperators: AuthenticatedValidator;
  readonly schedulerInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  /** The last state-queue element, proving the commitment gap. */
  readonly stateQueueTailRefInput: UTxO;
  readonly skippedOperatorKeyHash: string;
  readonly skippedOperatorNode: NodeWithDatum;
  readonly skippedOperatorDatum: ActiveOperatorDatum;
  readonly newOperatorKeyHash: string;
  readonly newStartTime: bigint;
  readonly witnesses: InactivityTakeoverWitnesses;
  readonly neglectedEvent?: StrikeNeglectedUserEvent;
  readonly validFrom: bigint;
  readonly validTo: bigint;
  readonly presetWalletInputs?: readonly UTxO[];
  readonly schedulerSpendingScriptRef?: UTxO;
  readonly activeOperatorsSpendingScriptRef?: UTxO;
  /**
   * Striking is permissionless; the submitting wallet only pays the fee. A
   * caller that wants an additional required signer names it here.
   */
  readonly extraSignerKeyHash?: string;
  readonly adversarialOverrides?: StrikeInactiveOperatorAdversarialOverrides;
};

export type StrikeInactiveOperatorLayout = {
  readonly schedulerInputIndex: bigint;
  readonly schedulerOutputIndex: bigint;
  readonly schedulerRedeemerIndex: bigint;
  readonly activeNodeInputIndex: bigint;
  readonly activeNodeOutputIndex: bigint;
  readonly activeOperatorsSpendRedeemerIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueRefInputIndex: bigint;
  readonly neglectedUserEventRefInputIndex: bigint | undefined;
} & (
  | {
      readonly tier: "GoToNext";
      readonly newOperatorNodeRefInputIndex: bigint;
    }
  | {
      readonly tier: "Rewind";
      readonly activeRootRefInputIndex: bigint;
      readonly activeTailRefInputIndex: bigint | null;
      readonly registeredWitnessRefInputIndex: bigint;
    }
);

export type StrikeInactiveOperatorTxResult = {
  readonly tx: TxSignBuilder;
  readonly layout: StrikeInactiveOperatorLayout;
  readonly struckInactivityStrikes: bigint;
};

/** The two datums a strike reproduces, encoded once per build. */
type StrikeDatums = {
  readonly struckNodeDatumCbor: string;
  readonly refreshedSchedulerDatumCbor: string;
  readonly struckInactivityStrikes: bigint;
};

const encodeStrikeDatums = (
  config: BuildStrikeInactiveOperatorTxConfig,
): StrikeDatums => {
  const strikes = config.skippedOperatorDatum.inactivity_strikes + 1n;
  return {
    struckInactivityStrikes: strikes,
    struckNodeDatumCbor: encodeLinkedListNodeView({
      key: config.skippedOperatorNode.datum.key,
      next: config.skippedOperatorNode.datum.next,
      data: castActiveOperatorDatumToData({
        bond_unlock_time: config.skippedOperatorDatum.bond_unlock_time,
        inactivity_strikes: strikes,
      }) as LinkedListNodeView["data"],
    }),
    refreshedSchedulerDatumCbor: encodeSchedulerDatumForChain({
      ActiveOperator: {
        operator: config.newOperatorKeyHash,
        start_time: config.newStartTime,
      },
    } as SchedulerDatum),
  };
};

const schedulerError = (message: string, cause: unknown): SchedulerError =>
  new SchedulerError({ message, cause });

const failScheduler = (
  message: string,
  cause: unknown,
): Effect.Effect<never, SchedulerError> =>
  Effect.fail(schedulerError(message, cause));

const safeTimeNumber = (value: bigint, label: string): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw schedulerError(
      `${label} is outside the safe Lucid time range`,
      value.toString(),
    );
  }
  return Number(value);
};

const nodeLink = (node: NodeWithDatum): string | null =>
  node.datum.next === "Empty" ? null : node.datum.next.Key.key;

const witnessReferenceInputs = (
  config: BuildStrikeInactiveOperatorTxConfig,
): readonly UTxO[] => {
  const witnessNodes =
    config.witnesses.tier === "GoToNext"
      ? [config.witnesses.newOperatorNode.utxo]
      : [
          config.witnesses.activeRootNode.utxo,
          ...(config.witnesses.activeTailNode === null
            ? []
            : [config.witnesses.activeTailNode.utxo]),
          config.witnesses.registeredWitnessNode.utxo,
        ];
  return [
    config.hubOracleRefInput,
    config.stateQueueTailRefInput,
    ...witnessNodes,
    ...(config.neglectedEvent === undefined
      ? []
      : [config.neglectedEvent.utxo]),
  ];
};

const deriveStrikeLayout = ({
  config,
  ctx,
  refreshedSchedulerDatumCbor,
  struckNodeDatumCbor,
  schedulerWitnessUnit,
}: {
  readonly config: BuildStrikeInactiveOperatorTxConfig;
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly refreshedSchedulerDatumCbor: string;
  readonly struckNodeDatumCbor: string;
  readonly schedulerWitnessUnit: string;
}): StrikeInactiveOperatorLayout => {
  const shared = {
    schedulerInputIndex: requireInputIndex(
      ctx,
      config.schedulerInput,
      "inactivity strike scheduler",
    ),
    schedulerOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: config.scheduler.spendingScriptAddress,
          datum: refreshedSchedulerDatumCbor,
          unit: schedulerWitnessUnit,
        }),
      "inactivity strike scheduler",
    ),
    schedulerRedeemerIndex: requireSpendRedeemerIndex(
      ctx,
      config.schedulerInput,
      "inactivity strike scheduler",
    ),
    activeNodeInputIndex: requireInputIndex(
      ctx,
      config.skippedOperatorNode.utxo,
      "inactivity strike active node",
    ),
    activeNodeOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        outputMatchesElement({
          output,
          address: config.activeOperators.spendingScriptAddress,
          datum: struckNodeDatumCbor,
          unit: requirePolicyNftUnit(
            config.skippedOperatorNode.utxo.assets,
            config.activeOperators.policyId,
            "inactivity strike active node assets",
          ),
        }),
      "inactivity strike active node",
    ),
    activeOperatorsSpendRedeemerIndex: requireSpendRedeemerIndex(
      ctx,
      config.skippedOperatorNode.utxo,
      "inactivity strike active node",
    ),
    hubOracleRefInputIndex: requireReferenceInputIndex(
      ctx,
      config.hubOracleRefInput,
      "inactivity strike hub oracle",
    ),
    stateQueueRefInputIndex: requireReferenceInputIndex(
      ctx,
      config.stateQueueTailRefInput,
      "inactivity strike state-queue tail",
    ),
    neglectedUserEventRefInputIndex:
      config.adversarialOverrides?.neglectedUserEventRefInputIndex ??
      (config.neglectedEvent === undefined
        ? undefined
        : requireReferenceInputIndex(
            ctx,
            config.neglectedEvent.utxo,
            `inactivity strike neglected ${config.neglectedEvent.kind}`,
          )),
  };
  if (config.witnesses.tier === "GoToNext") {
    return {
      ...shared,
      tier: "GoToNext",
      newOperatorNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        config.witnesses.newOperatorNode.utxo,
        "inactivity strike new operator node",
      ),
    };
  }
  const { activeRootNode, activeTailNode, registeredWitnessNode } =
    config.witnesses;
  return {
    ...shared,
    tier: "Rewind",
    activeRootRefInputIndex: requireReferenceInputIndex(
      ctx,
      activeRootNode.utxo,
      "inactivity strike active root",
    ),
    activeTailRefInputIndex:
      activeTailNode === null
        ? null
        : requireReferenceInputIndex(
            ctx,
            activeTailNode.utxo,
            "inactivity strike active tail",
          ),
    registeredWitnessRefInputIndex: requireReferenceInputIndex(
      ctx,
      registeredWitnessNode.utxo,
      "inactivity strike registered witness",
    ),
  };
};

const encodeNeglectedUserEvent = (
  config: BuildStrikeInactiveOperatorTxConfig,
  layout: StrikeInactiveOperatorLayout,
): unknown => {
  if (config.neglectedEvent === undefined) {
    return "NoNeglectedUserEvent";
  }
  const index = layout.neglectedUserEventRefInputIndex;
  if (index === undefined) {
    throw schedulerError(
      "Inactivity strike resolved no reference-input index for its neglected user event",
      config.neglectedEvent.kind,
    );
  }
  switch (config.neglectedEvent.kind) {
    case "Deposit":
      return { NeglectedDeposit: { deposit_ref_input_index: index } };
    case "Withdrawal":
      return { NeglectedWithdrawal: { withdrawal_ref_input_index: index } };
    case "TxOrder":
      return { NeglectedTxOrder: { tx_order_ref_input_index: index } };
  }
};

const encodeSchedulerStrikeRedeemer = (
  config: BuildStrikeInactiveOperatorTxConfig,
  layout: StrikeInactiveOperatorLayout,
): string => {
  const neglected_user_event = encodeNeglectedUserEvent(config, layout);
  const advancing_approach =
    layout.tier === "GoToNext"
      ? {
          GoToNextDueToSkippedOperator: {
            new_shifts_operator_node_ref_input_index:
              layout.newOperatorNodeRefInputIndex,
            skipped_operator_node_input_index: layout.activeNodeInputIndex,
            active_operators_spend_redeemer_index:
              layout.activeOperatorsSpendRedeemerIndex,
            state_queue_ref_input_index: layout.stateQueueRefInputIndex,
            hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
            neglected_user_event,
          },
        }
      : {
          RewindDueToSkippedOperator: {
            active_operators_root_ref_input_index:
              layout.activeRootRefInputIndex,
            skipped_operator_node_input_index: layout.activeNodeInputIndex,
            active_operators_spend_redeemer_index:
              layout.activeOperatorsSpendRedeemerIndex,
            state_queue_ref_input_index: layout.stateQueueRefInputIndex,
            hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
            m_active_operators_last_node_ref_input_index:
              layout.activeTailRefInputIndex,
            registered_element_ref_input_index:
              layout.registeredWitnessRefInputIndex,
            neglected_user_event,
          },
        };
  const redeemer = {
    scheduler_input_index: layout.schedulerInputIndex,
    scheduler_output_index: layout.schedulerOutputIndex,
    advancing_approach,
  } as unknown as SchedulerSpendRedeemer;
  return Data.to(redeemer as never, SchedulerSpendRedeemerSchema as never);
};

const encodeActiveOperatorStrikeRedeemer = (
  config: BuildStrikeInactiveOperatorTxConfig,
  layout: StrikeInactiveOperatorLayout,
): string => {
  const redeemer = {
    StrikeForInactivity: {
      active_node_input_index: layout.activeNodeInputIndex,
      active_node_output_index: layout.activeNodeOutputIndex,
      operator: config.skippedOperatorKeyHash,
      active_node_link:
        config.adversarialOverrides?.activeNodeLink === undefined
          ? nodeLink(config.skippedOperatorNode)
          : config.adversarialOverrides.activeNodeLink,
      scheduler_input_index: layout.schedulerInputIndex,
      scheduler_redeemer_index: layout.schedulerRedeemerIndex,
      hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
    },
  } as unknown as ActiveOperatorSpendRedeemerType;
  return Data.to(redeemer as never, ActiveOperatorSpendRedeemerSchema as never);
};

/**
 * Assembles the strike transaction. Both script inputs receive either a
 * `BuildTxWithRedeemer` callback (first pass, resolving indices from the final
 * transaction context) or the resolved redeemer CBOR (second pass).
 */
const buildStrikeInactiveOperatorTx = (
  config: BuildStrikeInactiveOperatorTxConfig,
  { struckNodeDatumCbor, refreshedSchedulerDatumCbor }: StrikeDatums,
  redeemers: {
    readonly scheduler: BuildTxWithRedeemer | string;
    readonly activeOperators: BuildTxWithRedeemer | string;
  },
): TxBuilder => {
  const scriptRefs = [
    ...(config.schedulerSpendingScriptRef === undefined
      ? []
      : [config.schedulerSpendingScriptRef]),
    ...(config.activeOperatorsSpendingScriptRef === undefined
      ? []
      : [config.activeOperatorsSpendingScriptRef]),
  ];
  let tx = config.lucid
    .newTx()
    .validFrom(safeTimeNumber(config.validFrom, "inactivity strike validFrom"))
    .validTo(safeTimeNumber(config.validTo, "inactivity strike validTo"))
    .readFrom([...witnessReferenceInputs(config), ...scriptRefs])
    .collectFrom([config.schedulerInput], redeemers.scheduler)
    .collectFrom([config.skippedOperatorNode.utxo], redeemers.activeOperators)
    .pay.ToContract(
      config.scheduler.spendingScriptAddress,
      { kind: "inline", value: refreshedSchedulerDatumCbor },
      config.schedulerInput.assets,
    )
    .pay.ToContract(
      config.activeOperators.spendingScriptAddress,
      { kind: "inline", value: struckNodeDatumCbor },
      config.skippedOperatorNode.utxo.assets,
    );
  if (config.schedulerSpendingScriptRef === undefined) {
    tx = tx.attach.Script(config.scheduler.spendingScript);
  }
  if (config.activeOperatorsSpendingScriptRef === undefined) {
    tx = tx.attach.Script(config.activeOperators.spendingScript);
  }
  return config.extraSignerKeyHash === undefined
    ? tx
    : tx.addSignerKey(config.extraSignerKeyHash);
};

/**
 * Builds the unsigned strike-and-takeover transaction: it spends the scheduler
 * and the inactive operator's active node, reproduces both (the node with one
 * more strike, its bond and lovelace untouched), and appoints the next
 * operator for a shift starting at the validity range's inclusive upper bound.
 */
export const buildStrikeInactiveOperatorTxProgram = (
  config: BuildStrikeInactiveOperatorTxConfig,
): Effect.Effect<StrikeInactiveOperatorTxResult, SchedulerError> =>
  Effect.gen(function* () {
    const encoded = yield* Effect.try({
      try: () => encodeStrikeDatums(config),
      catch: (cause) =>
        schedulerError("Failed to encode inactivity strike datums", cause),
    });
    const schedulerWitnessUnit = toUnit(
      config.scheduler.policyId,
      SCHEDULER_ASSET_NAME,
    );
    let layout: StrikeInactiveOperatorLayout | undefined;
    let schedulerRedeemerCbor: string | undefined;
    let activeOperatorRedeemerCbor: string | undefined;
    let callbackCount = 0;

    const resolveLayout = (
      ctx: Parameters<BuildTxWithRedeemer>[0],
    ): StrikeInactiveOperatorLayout => {
      callbackCount += 1;
      const resolved = deriveStrikeLayout({
        config,
        ctx,
        refreshedSchedulerDatumCbor: encoded.refreshedSchedulerDatumCbor,
        struckNodeDatumCbor: encoded.struckNodeDatumCbor,
        schedulerWitnessUnit,
      });
      layout = resolved;
      return resolved;
    };
    const requireStable = (
      previous: string | undefined,
      next: string,
      label: string,
    ): string => {
      if (previous !== undefined && previous !== next) {
        throw schedulerError(
          `BuildTxWithRedeemer resolved inconsistent ${label} redeemers`,
          {
            callback_count: callbackCount.toString(),
            previous_redeemer_cbor: previous,
            next_redeemer_cbor: next,
          },
        );
      }
      return next;
    };
    const schedulerRedeemer = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        config.schedulerInput,
        "inactivity strike scheduler",
      );
      const next = encodeSchedulerStrikeRedeemer(config, resolveLayout(ctx));
      schedulerRedeemerCbor = requireStable(
        schedulerRedeemerCbor,
        next,
        "inactivity strike scheduler",
      );
      return next;
    }) satisfies BuildTxWithRedeemer;
    const activeOperatorRedeemer = ((ctx) => {
      requireOwnSpendPurpose(
        ctx,
        config.skippedOperatorNode.utxo,
        "inactivity strike active node",
      );
      const next = encodeActiveOperatorStrikeRedeemer(
        config,
        resolveLayout(ctx),
      );
      activeOperatorRedeemerCbor = requireStable(
        activeOperatorRedeemerCbor,
        next,
        "inactivity strike active node",
      );
      return next;
    }) satisfies BuildTxWithRedeemer;

    yield* Effect.tryPromise({
      try: () =>
        buildStrikeInactiveOperatorTx(config, encoded, {
          scheduler: schedulerRedeemer,
          activeOperators: activeOperatorRedeemer,
        }).complete(
          completeOptionsWithLocalEval({
            presetWalletInputs: config.presetWalletInputs,
          }),
        ),
      catch: (cause) =>
        schedulerError(
          `Failed to build inactivity strike tx: ${String(cause)}`,
          cause,
        ),
    });
    if (
      layout === undefined ||
      schedulerRedeemerCbor === undefined ||
      activeOperatorRedeemerCbor === undefined
    ) {
      return yield* failScheduler(
        "BuildTxWithRedeemer did not resolve both inactivity strike redeemers",
        `callback_count=${callbackCount.toString()}`,
      );
    }
    const resolvedLayout = layout;
    const resolvedSchedulerRedeemerCbor = schedulerRedeemerCbor;
    const resolvedActiveOperatorRedeemerCbor = activeOperatorRedeemerCbor;
    const tx = yield* Effect.tryPromise({
      try: () =>
        buildStrikeInactiveOperatorTx(config, encoded, {
          scheduler: resolvedSchedulerRedeemerCbor,
          activeOperators: resolvedActiveOperatorRedeemerCbor,
        }).complete(
          completeOptionsWithLocalEval({
            presetWalletInputs: config.presetWalletInputs,
          }),
        ),
      catch: (cause) =>
        schedulerError(
          `Failed to rebuild inactivity strike tx: ${String(cause)}`,
          cause,
        ),
    });
    return {
      tx,
      layout: resolvedLayout,
      struckInactivityStrikes: encoded.struckInactivityStrikes,
    };
  });
