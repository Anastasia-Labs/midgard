import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { Cause, Effect, Exit, FiberId, Ref, Runtime } from "effect";
import { UnknownException } from "effect/Cause";
import { describe, expect, it } from "vitest";

import { attestationTimeoutCorrectionStep } from "../src/fibers/attestation-timeout-correction.attestation-timeout-correction-action.js";
import { attestationTimeoutCorrectionReadinessBounds } from "../src/fibers/attestation-timeout-correction.reconcile-state-queue-corrections.js";
import type { AttestationTimeoutCorrectionHealth } from "../src/services/globals.js";

const unattestedHeader = { headerHash: "cd".repeat(32), deadlineMs: 1_000 };
const health = () =>
  Ref.unsafeMake<AttestationTimeoutCorrectionHealth>({
    lastProgressAtMs: 0,
    lastQueueReadAtMs: 0,
    lastFailureAtMs: 0,
    lastError: null,
    consecutiveFailures: 0,
    oldestUnattestedHeader: unattestedHeader,
  });

describe("attestation-timeout correction step", () => {
  it("logs any failure and succeeds, so the schedule retries it and nothing halts", async () => {
    const fiberFailure = Runtime.makeFiberFailure(
      Cause.fail(new Error("timeout")),
    );
    for (const cause of [
      Cause.fail(new Error("Kupo unavailable")),
      Cause.die(new Error("boom")),
      Cause.fail(new UnknownException(fiberFailure)),
      Cause.parallel(
        Cause.fail(new Error("Kupo unavailable")),
        Cause.die(fiberFailure),
      ),
      Cause.interrupt(FiberId.none),
    ]) {
      const exit = await Effect.runPromiseExit(
        attestationTimeoutCorrectionStep(Effect.failCause(cause), health()),
      );
      expect(Exit.isSuccess(exit)).toBe(true);
    }
  });

  it("counts failures since the last success for readiness, and a success resets the count", async () => {
    const recorded = health();
    const step = (action: Effect.Effect<void, unknown>) =>
      Effect.runPromiseExit(attestationTimeoutCorrectionStep(action, recorded));
    await step(Effect.fail(new Error("Kupo unavailable")));
    await step(Effect.die(new Error("Ogmios hung up")));
    await step(Effect.fail(new Error("the removal is not landed yet")));
    const failed = Ref.get(recorded).pipe(Effect.runSync);
    expect(failed).toMatchObject({
      consecutiveFailures: 3,
      lastProgressAtMs: 0,
      oldestUnattestedHeader: unattestedHeader,
    });
    expect(failed.lastFailureAtMs).toBeGreaterThan(0);
    expect(failed.lastError).toContain("the removal is not landed yet");

    await step(Effect.void);
    const succeeded = Ref.get(recorded).pipe(Effect.runSync);
    expect(succeeded.consecutiveFailures).toBe(0);
    expect(succeeded.lastProgressAtMs).toBeGreaterThanOrEqual(
      failed.lastFailureAtMs,
    );
    expect(succeeded.lastError).toBe(failed.lastError);
  });

  it.each([
    // preprod-testing and local-devnet: the stall bound floors the queue bound.
    [
      "a 4 min DA timeout",
      { maxValidityRangeMs: 480_000n, daAttestationTimeoutMs: 240_000n },
      { stallBoundMs: 510_000, queueUnknownBoundMs: 510_000 },
    ],
    // preprod-public and mainnet: the DA timeout sets the queue bound.
    [
      "a 1 h DA timeout",
      { maxValidityRangeMs: 480_000n, daAttestationTimeoutMs: 3_600_000n },
      { stallBoundMs: 510_000, queueUnknownBoundMs: 3_600_000 },
    ],
    // A validity cap below the removal window never undercuts the removal.
    [
      "a validity cap shorter than the removal window",
      { maxValidityRangeMs: 60_000n, daAttestationTimeoutMs: 240_000n },
      { stallBoundMs: 330_000, queueUnknownBoundMs: 330_000 },
    ],
  ])(
    "derives the readiness bounds for a profile with %s at a 10 s tick",
    (_profile, profile, bounds) => {
      expect(
        attestationTimeoutCorrectionReadinessBounds(10_000, profile),
      ).toEqual(bounds);
    },
  );

  it("derives the node's readiness bounds from its selected profile", () => {
    expect(attestationTimeoutCorrectionReadinessBounds(10_000)).toEqual(
      attestationTimeoutCorrectionReadinessBounds(10_000, {
        maxValidityRangeMs: SDK.MAX_VALIDITY_RANGE_LENGTH_MS,
        daAttestationTimeoutMs: SDKStateQueue.DA_ATTESTATION_TIMEOUT_MS,
      }),
    );
  });
});
