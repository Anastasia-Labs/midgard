/**
 * Pure decision logic for the operator watchdog. The fiber feeds it a takeover
 * plan derived from the on-chain snapshot plus local facts (our key, the
 * clock, configuration) and it answers what to do this tick. Keeping it pure
 * lets the two tiers and the patience window be tested with a fake clock.
 */

/**
 * The part of a takeover plan the policy needs. The SDK planner produces a
 * richer object; the fiber projects it onto this shape.
 */
export type WatchdogTakeoverPlan =
  | { readonly kind: "no-shift" }
  | {
      readonly kind: "not-yet";
      readonly currentOperator: string;
      readonly thresholdMs: number;
    }
  | {
      readonly kind: "ready";
      readonly currentOperator: string;
      readonly newOperatorKey: string;
      readonly thresholdMs: number;
    }
  | {
      readonly kind: "strikes-exhausted";
      readonly currentOperator: string;
      readonly thresholdMs: number;
    };

export type WatchdogTier = "successor" | "any_active";

export type WatchdogDecision =
  | { readonly action: "idle"; readonly reason: string }
  | {
      readonly action: "wait";
      readonly reason: string;
      /** Earliest wall-clock ms at which this node would act. */
      readonly untilMs: number;
    }
  | {
      readonly action: "strike";
      readonly tier: WatchdogTier;
      readonly skippedOperator: string;
      readonly newOperatorKey: string;
    }
  | {
      readonly action: "force_retire";
      readonly tier: WatchdogTier;
      readonly skippedOperator: string;
    };

export type WatchdogPolicyInput = {
  readonly enabled: boolean;
  readonly nowMs: number;
  readonly patienceMs: number;
  /** This node's operator key hash. */
  readonly ownOperatorKey: string;
  /** Whether this node's operator is currently in the active set. */
  readonly ownOperatorIsActive: boolean;
  readonly plan: WatchdogTakeoverPlan;
};

/**
 * Chooses the tier this node acts in for a ready plan. The successor acts at
 * the threshold; every other active node waits the patience window.
 */
const resolveTier = (
  input: WatchdogPolicyInput,
  successorKey: string | null,
): { readonly tier: WatchdogTier; readonly actAtMs: number } => {
  const thresholdMs =
    input.plan.kind === "no-shift" ? input.nowMs : input.plan.thresholdMs;
  if (successorKey !== null && successorKey === input.ownOperatorKey) {
    return { tier: "successor", actAtMs: thresholdMs + 1 };
  }
  return { tier: "any_active", actAtMs: thresholdMs + 1 + input.patienceMs };
};

export const decideOperatorWatchdogAction = (
  input: WatchdogPolicyInput,
): WatchdogDecision => {
  if (!input.enabled) {
    return { action: "idle", reason: "watchdog_disabled" };
  }
  if (!input.ownOperatorIsActive) {
    return { action: "idle", reason: "own_operator_not_active" };
  }
  const { plan } = input;
  switch (plan.kind) {
    case "no-shift":
      return { action: "idle", reason: "scheduler_has_no_active_operator" };
    case "not-yet":
      if (plan.currentOperator === input.ownOperatorKey) {
        return { action: "idle", reason: "own_shift" };
      }
      return {
        action: "wait",
        reason: "before_inactivity_threshold",
        untilMs: plan.thresholdMs + 1,
      };
    case "ready": {
      if (plan.currentOperator === input.ownOperatorKey) {
        return { action: "idle", reason: "own_shift" };
      }
      const { tier, actAtMs } = resolveTier(input, plan.newOperatorKey);
      if (input.nowMs < actAtMs) {
        return {
          action: "wait",
          reason:
            tier === "successor"
              ? "before_inactivity_threshold"
              : "within_patience_window",
          untilMs: actAtMs,
        };
      }
      return {
        action: "strike",
        tier,
        skippedOperator: plan.currentOperator,
        newOperatorKey: plan.newOperatorKey,
      };
    }
    case "strikes-exhausted": {
      if (plan.currentOperator === input.ownOperatorKey) {
        return { action: "idle", reason: "own_shift" };
      }
      // Nobody is the designated successor of a forced retirement, so every
      // active node acts on the any-active tier. A retirement is idempotent
      // on-chain (the second submitter simply fails on a spent input).
      const { tier, actAtMs } = resolveTier(input, null);
      if (input.nowMs < actAtMs) {
        return {
          action: "wait",
          reason: "within_patience_window",
          untilMs: actAtMs,
        };
      }
      return {
        action: "force_retire",
        tier,
        skippedOperator: plan.currentOperator,
      };
    }
  }
};

/**
 * Mutable record of the last takeover this node submitted, reported by the
 * status surface.
 */
export type OperatorWatchdogRecord = {
  readonly lastTakeoverTxHash: string | null;
  readonly lastTakeoverAt: number | null;
  readonly lastTakeoverKind: "strike" | "force_retire" | null;
  readonly lastSkipReason: string | null;
  readonly lastSkipAt: number | null;
};

const initialRecord: OperatorWatchdogRecord = {
  lastTakeoverTxHash: null,
  lastTakeoverAt: null,
  lastTakeoverKind: null,
  lastSkipReason: null,
  lastSkipAt: null,
};

let record: OperatorWatchdogRecord = initialRecord;

export const readOperatorWatchdogRecord = (): OperatorWatchdogRecord => record;

export const recordOperatorWatchdogTakeover = (input: {
  readonly txHash: string;
  readonly atMs: number;
  readonly kind: "strike" | "force_retire";
}): void => {
  record = {
    ...record,
    lastTakeoverTxHash: input.txHash,
    lastTakeoverAt: input.atMs,
    lastTakeoverKind: input.kind,
  };
};

export const recordOperatorWatchdogSkip = (input: {
  readonly reason: string;
  readonly atMs: number;
}): void => {
  record = { ...record, lastSkipReason: input.reason, lastSkipAt: input.atMs };
};

export const resetOperatorWatchdogRecordForTests = (): void => {
  record = initialRecord;
};
