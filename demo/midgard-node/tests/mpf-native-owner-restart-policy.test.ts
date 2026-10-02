import { describe, expect, it } from "vitest";

import { NativeOwnerRestartPolicy } from "../src/services/mpf-native-owner/service.restart-policy.js";

const WINDOW_MS = 60_000;
const BASE_MS = 1_000;
const MAX_MS = 8_000;

const policyAt = (restartLimit = 3) => {
  let nowMs = 0;
  const policy = new NativeOwnerRestartPolicy(
    {
      restartLimit,
      restartWindowMs: WINDOW_MS,
      restartBackoffBaseMs: BASE_MS,
      restartBackoffMaxMs: MAX_MS,
    },
    () => nowMs,
  );
  return {
    policy,
    advance: (ms: number) => {
      nowMs += ms;
    },
  };
};

describe("native MPF owner restart policy", () => {
  it("restarts a dying child unboundedly, backing off exponentially up to the cap", () => {
    const { policy, advance } = policyAt(1);
    const delays: number[] = [];
    for (let death = 0; death < 50; death += 1) {
      delays.push(policy.startRestart());
      policy.recordSuccess();
      advance(10);
    }
    expect(delays.slice(0, 6)).toEqual([0, 1_000, 2_000, 4_000, 8_000, 8_000]);
    expect(delays.every((delay) => delay <= MAX_MS)).toBe(true);
    expect(policy.exhaustion()).toBeUndefined();
    expect(policy.health()).toMatchObject({
      restartsInWindow: 50,
      failedRestartsInWindow: 0,
      exhausted: false,
    });
  });

  it("forgets restarts that left the window, so the backoff starts over", () => {
    const { policy, advance } = policyAt();
    policy.startRestart();
    policy.startRestart();
    advance(WINDOW_MS);
    expect(policy.startRestart()).toBe(0);
    expect(policy.health().restartsInWindow).toBe(1);
  });

  it("exhausts only on restartLimit consecutive failed restarts inside the window", () => {
    const { policy, advance } = policyAt(3);
    for (let failure = 0; failure < 2; failure += 1) {
      policy.startRestart();
      policy.recordFailure(new Error("durable root unreadable"));
      advance(1_000);
    }
    expect(policy.exhaustion()).toBeUndefined();
    // A successful restart resets the count.
    policy.startRestart();
    policy.recordSuccess();
    for (let failure = 0; failure < 2; failure += 1) {
      policy.startRestart();
      policy.recordFailure(new Error("durable root unreadable"));
    }
    expect(policy.exhaustion()).toBeUndefined();
    policy.startRestart();
    policy.recordFailure(new Error("child would not start"));
    const exhaustion = policy.exhaustion();
    expect(exhaustion?.message).toMatch(
      /restart limit exhausted: 3 failed restart\(s\) within 60000 ms; restarts resume once the oldest leaves the window: child would not start/,
    );
    expect(policy.health().exhausted).toBe(true);
  });

  it("is no longer exhausted once the oldest failed restart leaves the window", () => {
    const { policy, advance } = policyAt(2);
    policy.startRestart();
    policy.recordFailure(new Error("first"));
    advance(10_000);
    policy.startRestart();
    policy.recordFailure(new Error("second"));
    expect(policy.exhaustion()).toBeDefined();
    advance(WINDOW_MS - 10_000 - 1);
    expect(policy.exhaustion()).toBeDefined();
    advance(1);
    expect(policy.exhaustion()).toBeUndefined();
  });

  it("treats a restart limit of zero as one", () => {
    const { policy } = policyAt(0);
    expect(policy.exhaustion()).toBeUndefined();
    policy.startRestart();
    policy.recordFailure(new Error("child would not start"));
    expect(policy.exhaustion()).toBeDefined();
  });

  it("cancels a pending backoff wait", async () => {
    const { policy } = policyAt();
    const started = performance.now();
    const wait = policy.wait(60_000);
    policy.cancel();
    await wait;
    expect(performance.now() - started).toBeLessThan(5_000);
  });
});
