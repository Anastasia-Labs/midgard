import { FraudProofL1UnavailableError } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import {
  createWatcherAvailabilityReconcileRetry,
  WATCHER_AVAILABILITY_TRANSIENT_FAILURES_BEFORE_BLOCKED,
  watcherAvailabilityRetryDelayMs,
} from "../../src/availability/runtime.reconcile-retry.js";

const transient = () =>
  new FraudProofL1UnavailableError("kupo is unavailable: connect ECONNREFUSED");

/** Records each armed timer so a test decides when, and whether, it fires. */
const clock = () => {
  const armed: { run: () => void; ms: number; cleared: boolean }[] = [];
  return {
    armed,
    timers: {
      setTimer: (run: () => void, ms: number) => {
        const timer = { run, ms, cleared: false };
        armed.push(timer);
        return timer;
      },
      clearTimer: (timer: unknown) => {
        (timer as { cleared: boolean }).cleared = true;
      },
    },
    fire: () => {
      const live = armed.filter(({ cleared }) => !cleared);
      expect(live).toHaveLength(1);
      live[0]!.cleared = true;
      live[0]!.run();
    },
  };
};

/** One reconciliation that fails with `cause` and settles. */
const failOnce = (
  retry: ReturnType<typeof createWatcherAvailabilityReconcileRetry>,
  cause: unknown,
  again: () => Promise<void>,
) => {
  const ticket = retry.request();
  const status = retry.failed(cause, ["aa"]);
  retry.settle(ticket, again);
  return status;
};

describe("availability reconciliation retry", () => {
  it("reports an L1 transient as waiting and retries it exactly once", () => {
    const { armed, timers, fire } = clock();
    const retry = createWatcherAvailabilityReconcileRetry(timers);
    const again = vi.fn(async () => undefined);
    expect(failOnce(retry, transient(), again)).toEqual({
      phase: "waiting",
      pendingHeaders: ["aa"],
      detail: "kupo is unavailable: connect ECONNREFUSED",
    });
    expect(armed.map(({ ms }) => ms)).toEqual([1_000]);
    expect(again).not.toHaveBeenCalled();
    fire();
    expect(again).toHaveBeenCalledTimes(1);
    // The retried reconciliation completes; nothing more is armed.
    retry.settle(retry.request(), again);
    expect(armed.filter(({ cleared }) => !cleared)).toHaveLength(0);
    expect(again).toHaveBeenCalledTimes(1);
  });

  it("reports a genuine refusal as blocked at once and still retries it", () => {
    const { armed, timers, fire } = clock();
    const retry = createWatcherAvailabilityReconcileRetry(timers);
    const again = vi.fn(async () => undefined);
    // A name alone is not the typed transient.
    const forged = Object.assign(new Error("kupo is unavailable"), {
      name: "FraudProofL1UnavailableError",
    });
    for (const cause of [new Error("snapshot boundary changed"), forged, "x"]) {
      const fresh = createWatcherAvailabilityReconcileRetry(timers);
      expect(failOnce(fresh, cause, again).phase).toBe("blocked");
    }
    expect(failOnce(retry, new Error("refused"), again).phase).toBe("blocked");
    expect(armed).toHaveLength(4);
    for (const timer of armed.slice(0, 3)) timer.cleared = true;
    fire();
    expect(again).toHaveBeenCalledTimes(1);
  });

  it("turns blocked after eight consecutive transients and clears on the first success", () => {
    const { armed, timers } = clock();
    const retry = createWatcherAvailabilityReconcileRetry(timers);
    const again = async () => undefined;
    const phases = Array.from(
      { length: WATCHER_AVAILABILITY_TRANSIENT_FAILURES_BEFORE_BLOCKED },
      () => failOnce(retry, transient(), again).phase,
    );
    expect(phases).toEqual([...Array(7).fill("waiting"), "blocked"]);
    expect(armed.map(({ ms }) => ms)).toEqual([
      1_000, 2_000, 4_000, 8_000, 16_000, 32_000, 60_000, 60_000,
    ]);
    retry.settle(retry.request(), again);
    expect(failOnce(retry, transient(), again).phase).toBe("waiting");
    expect(armed.at(-1)?.ms).toBe(1_000);
  });

  it("keeps a run blocked once a genuine refusal is among the consecutive failures", () => {
    const { timers } = clock();
    const retry = createWatcherAvailabilityReconcileRetry(timers);
    const again = async () => undefined;
    expect(failOnce(retry, new Error("refused"), again).phase).toBe("blocked");
    expect(failOnce(retry, transient(), again).phase).toBe("blocked");
  });

  it("drops an armed retry once a newer reconciliation or a cancel supersedes it", () => {
    const { armed, timers } = clock();
    const retry = createWatcherAvailabilityReconcileRetry(timers);
    const again = vi.fn(async () => undefined);
    failOnce(retry, transient(), again);
    retry.request();
    expect(armed[0]?.cleared).toBe(true);
    failOnce(retry, transient(), again);
    retry.cancel();
    expect(armed[1]?.cleared).toBe(true);
    // A timer that fired after it was superseded does nothing.
    const ticket = retry.request();
    retry.failed(transient(), []);
    retry.settle(ticket, again);
    retry.request();
    armed[2]!.run();
    expect(again).not.toHaveBeenCalled();
  });

  it("does not arm a retry for a failure a newer request already superseded", () => {
    const { armed, timers } = clock();
    const retry = createWatcherAvailabilityReconcileRetry(timers);
    const ticket = retry.request();
    retry.request();
    retry.failed(transient(), []);
    retry.settle(ticket, async () => undefined);
    expect(armed).toHaveLength(0);
  });

  it("swallows a rejected retry, whose own reconciliation reports it", async () => {
    const { timers, fire } = clock();
    const retry = createWatcherAvailabilityReconcileRetry(timers);
    const again = vi.fn(async () => {
      throw new Error("reported by the retry");
    });
    failOnce(retry, transient(), again);
    fire();
    await Promise.resolve();
    expect(again).toHaveBeenCalledTimes(1);
  });

  it("spaces retries from one second doubling to one minute", () => {
    expect(
      [0, 1, 2, 3, 7, 8, 100].map(watcherAvailabilityRetryDelayMs),
    ).toEqual([1_000, 1_000, 2_000, 4_000, 60_000, 60_000, 60_000]);
  });
});
