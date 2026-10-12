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
  /** The shift has no undelivered user event, so nobody can be struck. */
  | { readonly kind: "no-neglected-event"; readonly currentOperator: string }
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
  /**
   * The operator is at the strike cap. Forced retirement checks nothing else,
   * so the patience window runs from the shift's start, event or not.
   */
  | {
      readonly kind: "strikes-exhausted";
      readonly currentOperator: string;
      readonly shiftStartMs: number;
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
  thresholdMs: number,
  successorKey: string | null,
): { readonly tier: WatchdogTier; readonly actAtMs: number } => {
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
    case "no-neglected-event":
      return { action: "idle", reason: "no_neglected_user_event" };
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
      const { tier, actAtMs } = resolveTier(
        input,
        plan.thresholdMs,
        plan.newOperatorKey,
      );
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
      const { tier, actAtMs } = resolveTier(input, plan.shiftStartMs, null);
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

/**
 * How many failed strikes may cite one event before the watchdog passes over
 * it, when the failure is not a script refusal (which passes over it at
 * once). Bounds the retries a plausibly transient failure gets.
 */
export const MAX_STRIKE_ATTEMPTS_PER_CITATION = 3;

/** How many passed-over citations one scheduler state remembers. */
export const MAX_EXCLUDED_CITATIONS = 32;

/**
 * The citations strikes failed on, against one scheduler UTxO. A strike that
 * lands, or any other change of shift, spends that UTxO, and the record starts
 * over: a citation refused in one state may be good in the next.
 */
export type CitationFailures = Readonly<{
  schedulerRef: string | null;
  attempts: ReadonlyMap<string, number>;
  /** Passed over, oldest first. */
  excluded: ReadonlySet<string>;
}>;

export const emptyCitationFailures: CitationFailures = {
  schedulerRef: null,
  attempts: new Map(),
  excluded: new Set(),
};

/** The record for `schedulerRef`, emptied when the scheduler moved on. */
export const citationFailuresAt = (
  failures: CitationFailures,
  schedulerRef: string,
): CitationFailures =>
  failures.schedulerRef === schedulerRef
    ? failures
    : { ...emptyCitationFailures, schedulerRef };

export type CitationFailureReason =
  | "neglected_event_refused"
  | "neglected_event_attempts_exhausted"
  | "submission_failed";

/**
 * Records one failed strike citing `citationId`. A script refusal passes over
 * the citation at once; any other failure does after
 * `MAX_STRIKE_ATTEMPTS_PER_CITATION` attempts. The next plan then cites the
 * next citable event, so one bad candidate never wedges the watchdog. At most
 * `MAX_EXCLUDED_CITATIONS` are remembered; the oldest gives way.
 */
export const recordCitationFailure = (
  failures: CitationFailures,
  input: Readonly<{
    schedulerRef: string;
    citationId: string;
    refused: boolean;
  }>,
): Readonly<{ failures: CitationFailures; reason: CitationFailureReason }> => {
  const current = citationFailuresAt(failures, input.schedulerRef);
  const attempts = (current.attempts.get(input.citationId) ?? 0) + 1;
  const passOver =
    input.refused || attempts >= MAX_STRIKE_ATTEMPTS_PER_CITATION;
  const nextAttempts = new Map(current.attempts);
  if (passOver) nextAttempts.delete(input.citationId);
  else nextAttempts.set(input.citationId, attempts);
  const excluded = new Set(current.excluded);
  if (passOver) {
    excluded.delete(input.citationId);
    excluded.add(input.citationId);
    while (excluded.size > MAX_EXCLUDED_CITATIONS)
      excluded.delete(excluded.values().next().value!);
  }
  return {
    failures: { ...current, attempts: nextAttempts, excluded },
    reason: !passOver
      ? "submission_failed"
      : input.refused
        ? "neglected_event_refused"
        : "neglected_event_attempts_exhausted",
  };
};

const SCRIPT_REFUSAL = /failed script execution/iu;

/**
 * Whether a failed build or submission is the validators refusing the
 * transaction: Lucid's local evaluation reports a script that failed. It is
 * deterministic for the same transaction, so retrying the same citation
 * cannot clear it. Follows `cause` a few levels down.
 */
export const isScriptRefusal = (error: unknown): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < 6 && current != null; depth += 1) {
    if (typeof current === "string") return SCRIPT_REFUSAL.test(current);
    if (typeof current !== "object") return false;
    const { message, stack, cause } = current as {
      readonly message?: unknown;
      readonly stack?: unknown;
      readonly cause?: unknown;
    };
    for (const text of [message, stack])
      if (typeof text === "string" && SCRIPT_REFUSAL.test(text)) return true;
    current = cause;
  }
  return false;
};
