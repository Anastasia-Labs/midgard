import { describe, expect, it } from "vitest";

import {
  makePendingReconciliationBackoff,
  type PendingReconciliationBlocked,
} from "../src/services/history-pending-backoff.js";

/** A manual monotonic clock and timer set. */
const manualTimers = () => {
  let now = 0;
  let nextId = 0;
  const armed = new Map<number, { fire: () => void; at: number }>();
  return {
    timers: {
      now: () => now,
      setTimer: (fire: () => void, delayMs: number) => {
        nextId += 1;
        armed.set(nextId, { fire, at: now + delayMs });
        return nextId;
      },
      clearTimer: (timer: unknown) => {
        armed.delete(timer as number);
      },
    },
    /** Deadlines of the timers still armed. */
    deadlines: () => [...armed.values()].map(({ at }) => at),
    setNow: (value: number) => {
      now = value;
    },
    /** Fires every armed timer, whatever its deadline: as a timer that fires
     * before the monotonic clock reaches it would. */
    fireAll: () => {
      const due = [...armed.entries()];
      armed.clear();
      for (const [, { fire }] of due) fire();
    },
  };
};

const open = () => {
  const clock = manualTimers();
  const fired: number[] = [];
  const warnings: PendingReconciliationBlocked[] = [];
  const backoff = makePendingReconciliationBackoff({
    initialMs: 500,
    maxMs: 30_000,
    warnIntervalMs: 60_000,
    onDue: () => fired.push(clock.timers.now()),
    warn: (blocked) => warnings.push(blocked),
    timers: clock.timers,
  });
  return { clock, fired, warnings, backoff };
};

describe("pending reconciliation backoff", () => {
  it("re-arms for the remaining time when its timer fires before the deadline, and admits the retry at the deadline", () => {
    const { clock, fired, backoff } = open();
    backoff.arm("blocked");
    expect(clock.deadlines()).toEqual([500]);
    // The timer fires 2 ms early against the monotonic clock.
    clock.setNow(498);
    clock.fireAll();
    expect(fired).toEqual([498]);
    expect(backoff.due("blocked")).toBe(false);
    // The refused trigger left the retry scheduled for exactly the rest.
    expect(clock.deadlines()).toEqual([500]);
    clock.setNow(500);
    clock.fireAll();
    expect(backoff.due("blocked")).toBe(true);
  });

  it("keeps a single timer when another trigger arrives during the delay", () => {
    const { clock, backoff } = open();
    backoff.arm("blocked");
    clock.setNow(100);
    expect(backoff.due("blocked")).toBe(false);
    clock.setNow(300);
    expect(backoff.due("blocked")).toBe(false);
    expect(clock.deadlines()).toEqual([500]);
  });

  it("admits a retry at once for a different reason or with nothing armed", () => {
    const { clock, backoff } = open();
    expect(backoff.due("blocked")).toBe(true);
    backoff.arm("blocked");
    expect(backoff.due("another reason")).toBe(true);
    backoff.clear();
    expect(clock.deadlines()).toEqual([]);
    expect(backoff.due("blocked")).toBe(true);
  });

  it("doubles the delay while the reason is unchanged up to the cap, and restarts it for a new reason", () => {
    const { clock, backoff } = open();
    const delays: number[] = [];
    for (let attempt = 0; attempt < 9; attempt += 1) {
      backoff.arm("blocked");
      const [deadline] = clock.deadlines();
      delays.push(deadline! - clock.timers.now());
      clock.setNow(deadline!);
    }
    expect(delays).toEqual([
      500, 1000, 2000, 4000, 8000, 16_000, 30_000, 30_000, 30_000,
    ]);
    backoff.arm("another reason");
    expect(clock.deadlines()).toEqual([clock.timers.now() + 500]);
  });

  it("warns once a reason has blocked for the interval and then at most once per interval, and restarts the count for a new reason", () => {
    const { clock, warnings, backoff } = open();
    const retryUntil = (until: number, reason: string) => {
      for (;;) {
        backoff.arm(reason);
        const [deadline] = clock.deadlines();
        if (deadline! > until) return;
        clock.setNow(deadline!);
      }
    };
    // Blocked for less than the interval: silent.
    retryUntil(59_000, "blocked");
    expect(warnings).toEqual([]);
    retryUntil(200_000, "blocked");
    expect(warnings.length).toBeGreaterThanOrEqual(2);
    for (const warning of warnings) {
      expect(warning.reason).toBe("blocked");
      expect(warning.blockedMs).toBeGreaterThanOrEqual(60_000);
    }
    const at = warnings.map(({ blockedMs }) => blockedMs);
    for (let index = 1; index < at.length; index += 1)
      expect(at[index]! - at[index - 1]!).toBeGreaterThanOrEqual(60_000);
    // A new reason is a new block: nothing until it has lasted the interval.
    const before = warnings.length;
    backoff.arm("another reason");
    expect(warnings).toHaveLength(before);
  });
});
