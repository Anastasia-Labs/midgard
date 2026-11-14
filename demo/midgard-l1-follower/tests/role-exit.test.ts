import { afterEach, describe, expect, it, vi } from "vitest";

import { armExitDeadline, ROLE_EXIT_DEADLINE_MS } from "../src/role-exit.js";

afterEach(() => {
  vi.useRealTimers();
});

describe("armExitDeadline", () => {
  it("exits with the code at the deadline, 10 s by default, once", () => {
    vi.useFakeTimers();
    const codes: number[] = [];
    const disarm = armExitDeadline({
      exit: (code) => codes.push(code),
      code: 1,
    });
    vi.advanceTimersByTime(ROLE_EXIT_DEADLINE_MS - 1);
    expect(codes).toEqual([]);
    vi.advanceTimersByTime(1);
    expect(codes).toEqual([1]);
    // Too late: the caller must not exit again.
    expect(disarm()).toBe(false);
    expect(ROLE_EXIT_DEADLINE_MS).toBe(10_000);
  });

  it("does not exit once disarmed in time", () => {
    vi.useFakeTimers();
    const codes: number[] = [];
    const disarm = armExitDeadline({
      exit: (code) => codes.push(code),
      code: 70,
      deadlineMs: 50,
    });
    expect(disarm()).toBe(true);
    vi.advanceTimersByTime(1_000);
    expect(codes).toEqual([]);
  });
});
