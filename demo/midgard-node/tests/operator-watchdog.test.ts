import { beforeEach, describe, expect, it } from "vitest";

import {
  citationFailuresAt,
  decideOperatorWatchdogAction,
  emptyCitationFailures,
  isScriptRefusal,
  MAX_EXCLUDED_CITATIONS,
  MAX_STRIKE_ATTEMPTS_PER_CITATION,
  readOperatorWatchdogRecord,
  recordCitationFailure,
  recordOperatorWatchdogSkip,
  recordOperatorWatchdogTakeover,
  resetOperatorWatchdogRecordForTests,
  type WatchdogPolicyInput,
} from "../src/fibers/operator-watchdog-policy.js";

const A = "a".repeat(56);
const B = "b".repeat(56);
const C = "c".repeat(56);

const THRESHOLD = 1_000_000;
const PATIENCE = 120_000;

const base = (
  overrides: Partial<WatchdogPolicyInput> = {},
): WatchdogPolicyInput => ({
  enabled: true,
  nowMs: THRESHOLD + 1,
  patienceMs: PATIENCE,
  ownOperatorKey: B,
  ownOperatorIsActive: true,
  plan: {
    kind: "ready",
    currentOperator: A,
    newOperatorKey: B,
    thresholdMs: THRESHOLD,
  },
  ...overrides,
});

describe("operator watchdog policy", () => {
  beforeEach(() => {
    resetOperatorWatchdogRecordForTests();
  });

  it("is idle when disabled, when not active, or when there is no shift", () => {
    expect(decideOperatorWatchdogAction(base({ enabled: false }))).toEqual({
      action: "idle",
      reason: "watchdog_disabled",
    });
    expect(
      decideOperatorWatchdogAction(base({ ownOperatorIsActive: false })),
    ).toEqual({ action: "idle", reason: "own_operator_not_active" });
    expect(
      decideOperatorWatchdogAction(base({ plan: { kind: "no-shift" } })),
    ).toEqual({ action: "idle", reason: "scheduler_has_no_active_operator" });
  });

  it("never strikes a shift with no neglected user event, on any tier, however late", () => {
    const plan = { kind: "no-neglected-event" as const, currentOperator: A };
    for (const ownOperatorKey of [B, C])
      for (const patienceMs of [0, PATIENCE])
        expect(
          decideOperatorWatchdogAction(
            base({
              plan,
              ownOperatorKey,
              patienceMs,
              nowMs: THRESHOLD * 1_000,
            }),
          ),
        ).toEqual({ action: "idle", reason: "no_neglected_user_event" });
  });

  it("never strikes its own shift", () => {
    for (const plan of [
      {
        kind: "ready" as const,
        currentOperator: B,
        newOperatorKey: C,
        thresholdMs: THRESHOLD,
      },
      { kind: "not-yet" as const, currentOperator: B, thresholdMs: THRESHOLD },
      {
        kind: "strikes-exhausted" as const,
        currentOperator: B,
        shiftStartMs: THRESHOLD,
      },
    ]) {
      expect(
        decideOperatorWatchdogAction(
          base({ plan, nowMs: THRESHOLD + PATIENCE + 10 }),
        ),
      ).toEqual({ action: "idle", reason: "own_shift" });
    }
  });

  it("waits until the threshold before the plan is ready", () => {
    expect(
      decideOperatorWatchdogAction(
        base({
          nowMs: THRESHOLD - 5_000,
          plan: {
            kind: "not-yet",
            currentOperator: A,
            thresholdMs: THRESHOLD,
          },
        }),
      ),
    ).toEqual({
      action: "wait",
      reason: "before_inactivity_threshold",
      untilMs: THRESHOLD + 1,
    });
  });

  it("successor tier strikes at the threshold", () => {
    expect(
      decideOperatorWatchdogAction(base({ nowMs: THRESHOLD + 1 })),
    ).toEqual({
      action: "strike",
      tier: "successor",
      skippedOperator: A,
      newOperatorKey: B,
    });
    expect(decideOperatorWatchdogAction(base({ nowMs: THRESHOLD }))).toEqual({
      action: "wait",
      reason: "before_inactivity_threshold",
      untilMs: THRESHOLD + 1,
    });
  });

  it("any-active tier waits the patience window and then strikes", () => {
    const asC = base({ ownOperatorKey: C });
    expect(
      decideOperatorWatchdogAction({ ...asC, nowMs: THRESHOLD + 1 }),
    ).toEqual({
      action: "wait",
      reason: "within_patience_window",
      untilMs: THRESHOLD + 1 + PATIENCE,
    });
    expect(
      decideOperatorWatchdogAction({ ...asC, nowMs: THRESHOLD + PATIENCE }),
    ).toMatchObject({ action: "wait" });
    expect(
      decideOperatorWatchdogAction({ ...asC, nowMs: THRESHOLD + 1 + PATIENCE }),
    ).toEqual({
      action: "strike",
      tier: "any_active",
      skippedOperator: A,
      newOperatorKey: B,
    });
  });

  it("a zero patience window collapses both tiers onto the threshold", () => {
    expect(
      decideOperatorWatchdogAction(
        base({ ownOperatorKey: C, patienceMs: 0, nowMs: THRESHOLD + 1 }),
      ),
    ).toMatchObject({ action: "strike", tier: "any_active" });
  });

  it("force-retires an exhausted operator on the any-active tier, a patience window after its shift starts", () => {
    const plan = {
      kind: "strikes-exhausted" as const,
      currentOperator: A,
      shiftStartMs: THRESHOLD,
    };
    expect(
      decideOperatorWatchdogAction(base({ plan, nowMs: THRESHOLD + 1 })),
    ).toEqual({
      action: "wait",
      reason: "within_patience_window",
      untilMs: THRESHOLD + 1 + PATIENCE,
    });
    expect(
      decideOperatorWatchdogAction(
        base({ plan, nowMs: THRESHOLD + 1 + PATIENCE }),
      ),
    ).toEqual({
      action: "force_retire",
      tier: "any_active",
      skippedOperator: A,
    });
  });

  it("records the last takeover and the last skip for the status surface", () => {
    expect(readOperatorWatchdogRecord()).toEqual({
      lastTakeoverTxHash: null,
      lastTakeoverAt: null,
      lastTakeoverKind: null,
      lastSkipReason: null,
      lastSkipAt: null,
    });
    recordOperatorWatchdogSkip({ reason: "insufficient_funds", atMs: 5 });
    recordOperatorWatchdogTakeover({ txHash: "ff", atMs: 9, kind: "strike" });
    expect(readOperatorWatchdogRecord()).toEqual({
      lastTakeoverTxHash: "ff",
      lastTakeoverAt: 9,
      lastTakeoverKind: "strike",
      lastSkipReason: "insufficient_funds",
      lastSkipAt: 5,
    });
  });
});

describe("operator watchdog citation failures", () => {
  const at = (
    failures: typeof emptyCitationFailures,
    citationId: string,
    refused: boolean,
    schedulerRef = "sched#0",
  ) => recordCitationFailure(failures, { schedulerRef, citationId, refused });

  it("passes over a refused citation at once, leaving the next one citable", () => {
    const { failures, reason } = at(emptyCitationFailures, "Deposit:a#0", true);
    expect(reason).toBe("neglected_event_refused");
    expect([...failures.excluded]).toEqual(["Deposit:a#0"]);
    expect(failures.excluded.has("Deposit:b#0")).toBe(false);
  });

  it("retries another failure a bounded number of times, then passes over it", () => {
    let failures = emptyCitationFailures;
    const reasons: string[] = [];
    for (let i = 0; i < MAX_STRIKE_ATTEMPTS_PER_CITATION; i += 1) {
      const recorded = at(failures, "TxOrder:t#1", false);
      failures = recorded.failures;
      reasons.push(recorded.reason);
    }
    expect(reasons).toEqual([
      ...Array<string>(MAX_STRIKE_ATTEMPTS_PER_CITATION - 1).fill(
        "submission_failed",
      ),
      "neglected_event_attempts_exhausted",
    ]);
    expect(failures.excluded.has("TxOrder:t#1")).toBe(true);
    expect(failures.attempts.size).toBe(0);
  });

  it("starts over when the scheduler moves on", () => {
    const { failures } = at(emptyCitationFailures, "Deposit:a#0", true);
    expect(citationFailuresAt(failures, "sched#0")).toBe(failures);
    const moved = citationFailuresAt(failures, "sched#1");
    expect(moved.excluded.size).toBe(0);
    expect(moved.schedulerRef).toBe("sched#1");
    expect(
      at(failures, "Deposit:b#0", false, "sched#1").failures.excluded.size,
    ).toBe(0);
  });

  it("remembers a bounded number of citations, the oldest giving way", () => {
    let failures = emptyCitationFailures;
    for (let i = 0; i <= MAX_EXCLUDED_CITATIONS; i += 1)
      failures = at(failures, `Deposit:${i.toString()}#0`, true).failures;
    expect(failures.excluded.size).toBe(MAX_EXCLUDED_CITATIONS);
    expect(failures.excluded.has("Deposit:0#0")).toBe(false);
    expect(
      failures.excluded.has(`Deposit:${MAX_EXCLUDED_CITATIONS.toString()}#0`),
    ).toBe(true);
  });

  it("recognises a script refusal through wrapped causes, and nothing else", () => {
    const refusal = new Error("failed script execution\n Spend[1] ...");
    expect(isScriptRefusal(refusal)).toBe(true);
    expect(
      isScriptRefusal(
        new Error("strike build failed", {
          cause: new Error("wrapped", { cause: refusal }),
        }),
      ),
    ).toBe(true);
    expect(
      isScriptRefusal({ message: "x", cause: "failed script execution" }),
    ).toBe(true);
    expect(isScriptRefusal(new Error("fetch failed: ECONNRESET"))).toBe(false);
    expect(isScriptRefusal(undefined)).toBe(false);
  });
});
