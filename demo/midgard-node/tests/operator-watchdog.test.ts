import { beforeEach, describe, expect, it } from "vitest";

import {
  decideOperatorWatchdogAction,
  readOperatorWatchdogRecord,
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
        thresholdMs: THRESHOLD,
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

  it("force-retires an exhausted operator on the any-active tier only", () => {
    const plan = {
      kind: "strikes-exhausted" as const,
      currentOperator: A,
      thresholdMs: THRESHOLD,
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
