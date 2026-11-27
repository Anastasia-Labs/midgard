import { afterEach, describe, expect, it, vi } from "vitest";

import {
  AvailabilityResponder,
  type AvailabilityResponderChallenge,
  type AvailabilityResponderDeps,
} from "../src/availability/responder.js";
import { AvailabilityResponderValidityDeferredError } from "../src/availability/validity-deferred-error.js";
import {
  type CommitteeL1Clock,
  committeeL1NowMs,
} from "../src/l1/follower/l1-clock.js";
import {
  deploymentFingerprint,
  deploymentIdentity,
  record,
  withResponseDeadline,
} from "./helpers/availability-challenge.js";
import { l1ClockAt, testL1Clock } from "./helpers/l1-clock.js";

/**
 * Whether a response deadline has passed is decided on the committee's L1
 * clock (the follower tip plus monotonic elapsed time), never on the wall
 * clock, and a deferred validity window is reported as pending or as a
 * missed deadline, never as a failure.
 */
const slotConfig = {
  zeroTime: 1_700_000_000_000,
  zeroSlot: 0,
  slotLength: 1_000,
};
const TIP_SLOT = 10_000;
const TIP_MS = slotConfig.zeroTime + TIP_SLOT * 1_000;
const clockAt = (elapsedMs: number) =>
  l1ClockAt({ slotConfig, tipSlot: TIP_SLOT, elapsedMs }).clock;

const responderAt = (
  clock: CommitteeL1Clock,
  challenge: AvailabilityResponderChallenge,
  execute: AvailabilityResponderDeps["execute"],
) =>
  new AvailabilityResponder({
    deploymentFingerprint,
    deploymentIdentity,
    store: { getDaPayload: async () => record },
    reconcile: async () => "ready",
    discover: async () => [challenge],
    execute,
    now: () => committeeL1NowMs(clock),
  });

afterEach(() => vi.useRealTimers());

describe("the responder's deadline decisions on the L1 clock", () => {
  it("answers before the L1 deadline even with the wall clock 120 s past it", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    const deadlineMs = TIP_MS + 80_000;
    vi.setSystemTime(deadlineMs + 120_000);
    const execute = vi.fn(async () => "pending" as const);
    const report = await responderAt(
      clockAt(50_000),
      withResponseDeadline(deadlineMs),
      execute,
    ).tick();
    expect(execute).toHaveBeenCalledTimes(1);
    expect(report).toMatchObject({ status: "pending", action: "publish" });
    expect(report.missedDeadlines).toBeUndefined();
  });

  it("reports the deadline missed on the L1 clock even with the wall clock 120 s before it", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    const deadlineMs = TIP_MS + 40_000;
    vi.setSystemTime(deadlineMs - 120_000);
    const execute = vi.fn(async () => "pending" as const);
    const report = await responderAt(
      clockAt(50_000),
      withResponseDeadline(deadlineMs),
      execute,
    ).tick();
    expect(execute).not.toHaveBeenCalled();
    expect(report.status).toBe("unavailable");
    expect(report.missedDeadlines).toEqual([
      expect.objectContaining({ responseDeadline: deadlineMs.toString() }),
    ]);
  });

  it("reports a deferred window as pending, never failed", async () => {
    const report = await responderAt(
      clockAt(90_000),
      withResponseDeadline(TIP_MS + 600_000),
      async () => {
        throw new AvailabilityResponderValidityDeferredError(
          "ledger_tip_behind",
          "the tip has not reached the lower bound",
        );
      },
    ).tick();
    expect(report).toMatchObject({ status: "pending", action: "publish" });
    expect(report.detail).toMatch(/tip has not reached the lower bound/);
    expect(report.missedDeadlines).toBeUndefined();
  });

  it("reports a closed publication window as a missed deadline, not a failure", async () => {
    const deadlineMs = TIP_MS + 600_000;
    const report = await responderAt(
      clockAt(90_000),
      withResponseDeadline(deadlineMs),
      async () => {
        throw new AvailabilityResponderValidityDeferredError(
          "deadline_passed",
          "the validity window closed",
        );
      },
    ).tick();
    expect(report.status).toBe("unavailable");
    expect(report.missedDeadlines).toEqual([
      expect.objectContaining({ responseDeadline: deadlineMs.toString() }),
    ]);
  });

  it("waits for the follower while no tip has been observed", async () => {
    const execute = vi.fn(async () => "pending" as const);
    const report = await responderAt(
      testL1Clock({ slotConfig }),
      withResponseDeadline(TIP_MS + 600_000),
      execute,
    ).tick();
    expect(report.status).toBe("awaiting_scan");
    expect(execute).not.toHaveBeenCalled();
  });
});
