/**
 * The native MPF owner's restart policy (owner ruling 2026-10-09): every
 * child failure restarts the child with backoff; the same failure
 * `restartLimit` times in a row holds the owner, which restarts no more; a
 * failure no restart repairs (the binary is not the pinned one) holds at
 * once.
 */
import { describe, expect, it } from "vitest";

import {
  NativeOwnerBinaryPinMismatchError,
  nativeOwnerFailureKey,
  NativeOwnerRestartPolicy,
} from "../src/services/mpf-native-owner/service.restart-policy.js";

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

const death = (stderr: string) =>
  new Error(
    `Native MPF owner exited: code=null,signal=SIGKILL,stderr=${stderr}`,
  );

describe("native MPF owner restart policy", () => {
  it("backs off exponentially up to the cap, over the restarts inside the window", () => {
    const { policy, advance } = policyAt(1_000);
    const delays: number[] = [];
    for (let restart = 0; restart < 8; restart += 1) {
      delays.push(policy.startRestart());
      advance(10);
    }
    expect(delays).toEqual([
      0, 1_000, 2_000, 4_000, 8_000, 8_000, 8_000, 8_000,
    ]);
    advance(WINDOW_MS);
    expect(policy.startRestart()).toBe(0);
    expect(policy.health().restartsInWindow).toBe(1);
  });

  it("keeps restarting through failures that differ from the one before", () => {
    const { policy, advance } = policyAt(2);
    for (let failure = 0; failure < 50; failure += 1) {
      policy.startRestart();
      policy.recordStarted();
      advance(10);
      expect(
        policy.recordFailure(
          new Error(failure % 2 === 0 ? "request timed out" : "child crashed"),
          "death",
        ),
      ).toBe(false);
    }
    expect(policy.held()).toBeUndefined();
    expect(policy.health()).toMatchObject({ failuresInARow: 1, held: false });
  });

  it("keeps restarting a child that dies the same way after each long run", () => {
    const { policy, advance } = policyAt(2);
    for (let failure = 0; failure < 10; failure += 1) {
      policy.recordStarted();
      advance(WINDOW_MS);
      expect(
        policy.recordFailure(death(`run ${failure.toString()}`), "death"),
      ).toBe(false);
    }
    expect(policy.held()).toBeUndefined();
  });

  it("holds once the same failure comes restartLimit times in a row, its stderr aside", () => {
    const { policy, advance } = policyAt(3);
    expect(policy.recordFailure(death("first"), "death")).toBe(false);
    policy.startRestart();
    expect(policy.recordFailure(death("second"), "restart")).toBe(false);
    expect(policy.held()).toBeUndefined();
    advance(1_000);
    expect(policy.recordFailure(death("third"), "death")).toBe(true);
    const held = policy.held();
    expect(held?.message).toMatch(
      /^Native MPF owner holds: the same failure 3 time\(s\) in a row, so it restarts no more until the node restarts\. .*signal=SIGKILL,stderr=third$/u,
    );
    expect(policy.health()).toMatchObject({ failuresInARow: 3, held: true });
    // Held is terminal: no later failure, run or window clears it.
    policy.recordStarted();
    advance(10 * WINDOW_MS);
    expect(policy.recordFailure(new Error("other"), "death")).toBe(true);
    expect(policy.held()).toBe(held);
  });

  it("starts the count over on a different failure", () => {
    const { policy } = policyAt(2);
    policy.recordFailure(new Error("durable root unreadable"), "restart");
    policy.recordFailure(new Error("child would not start"), "restart");
    expect(policy.held()).toBeUndefined();
    policy.recordFailure(new Error("child would not start"), "restart");
    expect(policy.held()).toBeDefined();
  });

  it("holds at once on a binary that is not the pinned one", () => {
    const { policy } = policyAt(1_000);
    expect(
      policy.recordFailure(
        new NativeOwnerBinaryPinMismatchError("SHA-256 differs from the pin"),
        "restart",
      ),
    ).toBe(true);
    expect(policy.held()?.message).toMatch(
      /^Native MPF owner holds: no restart repairs this failure.*Restore the pinned binary, then restart the node: SHA-256 differs from the pin$/u,
    );
  });

  it("treats a restart limit of zero as one", () => {
    const { policy } = policyAt(0);
    expect(policy.held()).toBeUndefined();
    expect(
      policy.recordFailure(new Error("child would not start"), "restart"),
    ).toBe(true);
  });

  it("keys a failure on its message without the child's stderr", () => {
    expect(nativeOwnerFailureKey(death("a\nb"))).toBe(
      "Native MPF owner exited: code=null,signal=SIGKILL",
    );
    expect(nativeOwnerFailureKey(new Error("plain"))).toBe("plain");
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
