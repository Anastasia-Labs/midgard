import {
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type ActiveOperatorDatum,
  castActiveOperatorDatumToData,
} from "../active-operators.js";
import type { AuthenticatedValidator } from "../common.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "../linked-list.js";
import { type SchedulerDatum, SchedulerError } from "../scheduler.js";
import { encodeSchedulerDatumForChain } from "../scheduler-refresh.js";
import {
  findAnchorNodeForKey,
  findNodeByKey,
  findRootNode,
  findTailNode,
  registeredNodeKeyToPosixTime,
  schedulerCurrentOperator,
} from "./directory.js";
import type { NodeWithDatum } from "./layout.js";
import {
  blocked,
  computeInactivityThreshold,
  DEFAULT_INACTIVITY_TIMING_PARAMETERS,
  DEFAULT_STRIKE_VALIDITY_WINDOW_MS,
  type InactivityTakeoverPlan,
  type InactivityTakeoverValidity,
  type InactivityTakeoverWitnesses,
  type NeglectedUserEventKind,
  type PlanInactivityTakeoverInput,
} from "./strike.compute-inactivity-threshold.js";

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
  const skippedNode = findNodeByKey(snapshot.active, current.operator);
  if (skippedNode === undefined || skippedNode.active === null) {
    return blocked(
      "scheduled-operator-not-active",
      `scheduled operator ${current.operator} has no active-operators node`,
      current.operator,
    );
  }
  // Forced retirement (`RetireOperator` with `penalize_for_inactivity`) asks
  // only `inactivity_strikes >= max_inactivity_strikes`: no event, no
  // threshold. So the cap is checked before either, and an idle network still
  // retires an operator that is already at it.
  if (skippedNode.active.inactivity_strikes >= params.maxInactivityStrikes) {
    return {
      kind: "strikes-exhausted",
      currentOperator: current.operator,
      skippedNode,
      inactivityStrikes: skippedNode.active.inactivity_strikes,
      maxInactivityStrikes: params.maxInactivityStrikes,
      shiftStartMs: current.startTime,
    };
  }
  if (neglectedEvent === null) {
    return { kind: "no-neglected-event", currentOperator: current.operator };
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
  /** The last state-queue element, proving the cited event is undelivered. */
  readonly stateQueueTailRefInput: UTxO;
  readonly skippedOperatorKeyHash: string;
  readonly skippedOperatorNode: NodeWithDatum;
  readonly skippedOperatorDatum: ActiveOperatorDatum;
  readonly newOperatorKeyHash: string;
  readonly newStartTime: bigint;
  readonly witnesses: InactivityTakeoverWitnesses;
  readonly neglectedEvent: StrikeNeglectedUserEvent;
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
  readonly neglectedUserEventRefInputIndex: bigint;
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
export type StrikeDatums = {
  readonly struckNodeDatumCbor: string;
  readonly refreshedSchedulerDatumCbor: string;
  readonly struckInactivityStrikes: bigint;
};

export const encodeStrikeDatums = (
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

export const schedulerError = (
  message: string,
  cause: unknown,
): SchedulerError => new SchedulerError({ message, cause });

export const failScheduler = (
  message: string,
  cause: unknown,
): Effect.Effect<never, SchedulerError> =>
  Effect.fail(schedulerError(message, cause));
