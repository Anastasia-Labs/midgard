import { describe, expect, it } from "vitest";

import { stackTraceShiftWorkMs } from "./stack-trace-staging.js";

/** An attached stack's timing: its commit allowance and deployment profile. */
const stack = {
  transactionAllowanceMs: 190_000,
  eventWaitMs: 300_000,
  eventHistoryProtectionMs: 120_000,
  operatorShiftMs: 1_800_000,
};

describe("operator-2's shift work for the transition trace on an attached stack", () => {
  it("counts the predecessor deposit, the trace deposit's admission and the fault commit's late-shift lead", () => {
    // Predecessor deposit: its protection hold (181,000), then the wait for
    // its inclusion less the empty interval (282,001). Trace deposit: its
    // confirmation and the empty block's commit and attestation (760,000),
    // which outlast its admission (361,000). The shortest lead (250,000).
    expect(stackTraceShiftWorkMs(stack)).toBe(1_473_001);
    // Without history protection the holds are only their backoff.
    expect(
      stackTraceShiftWorkMs({ ...stack, eventHistoryProtectionMs: 0 }),
    ).toBe(1_353_001);
    // With a fast commit allowance the trace deposit's admission sets it.
    expect(
      stackTraceShiftWorkMs({ ...stack, transactionAllowanceMs: 50_000 }),
    ).toBe(181_000 + 282_001 + 361_000 + 110_000);
  });

  it("refuses, before any L1 work, a profile whose shift cannot hold it", () => {
    expect(() =>
      stackTraceShiftWorkMs({ ...stack, operatorShiftMs: 1_473_000 }),
    ).toThrow(
      /transitionTrace staging holds operator-2's shift for 1473001 ms/u,
    );
    expect(
      stackTraceShiftWorkMs({ ...stack, operatorShiftMs: 1_473_001 }),
    ).toBe(1_473_001);
  });
});
