import type { TimeoutCorrectionJournal } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Exit, FiberId, Ref, Runtime } from "effect";
import { UnknownException } from "effect/Cause";
import { describe, expect, it } from "vitest";

import {
  attestationTimeoutCorrectionReadinessBounds,
  attestationTimeoutCorrectionStep,
  recordTimeoutCorrectionJournalProgress,
  withTimeoutCorrectionProgress,
} from "../src/fibers/attestation-timeout-correction.js";
import type { AttestationTimeoutCorrectionHealth } from "../src/services/globals.js";

const unattestedHeader = { headerHash: "cd".repeat(32), deadlineMs: 1_000 };
const health = () =>
  Ref.unsafeMake<AttestationTimeoutCorrectionHealth>({
    lastProgressAtMs: 0,
    lastQueueReadAtMs: 0,
    correctionProgress: null,
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
        daAttestationTimeoutMs: SDK.DA_ATTESTATION_TIMEOUT_MS,
      }),
    );
  });

  describe("correction progress", () => {
    const target = "ef".repeat(28);
    const journal = (
      targetHeaderHash: string,
      statuses: readonly ("prepared" | "submitted" | "confirmed")[],
    ): TimeoutCorrectionJournal => ({
      version: 1,
      targetHeaderHash,
      targetDeadlineMs: "1000",
      completed: false,
      steps: statuses.map((status, index) => ({
        kind: "prune-descendant",
        removedHeaderHash: "34".repeat(28),
        inputOutRefs: [],
        txHash: index.toString(16).padStart(64, "0"),
        signedCbor: "",
        validFromSlot: "0",
        validToSlot: "0",
        status,
      })),
    });
    const progressAfter = (
      recorded: Ref.Ref<AttestationTimeoutCorrectionHealth>,
      saved: TimeoutCorrectionJournal,
      nowMs: number,
    ) =>
      Effect.runSync(
        recordTimeoutCorrectionJournalProgress(recorded, saved, nowMs).pipe(
          Effect.zipRight(Ref.get(recorded)),
        ),
      ).lastProgressAtMs;

    it("credits a started correction and each confirmed removal, never a resubmission", () => {
      const recorded = health();
      expect(progressAfter(recorded, journal(target, ["prepared"]), 10)).toBe(
        10,
      );
      // Resubmitting a removal that never lands is not progress.
      expect(progressAfter(recorded, journal(target, ["submitted"]), 20)).toBe(
        10,
      );
      expect(
        progressAfter(recorded, journal(target, ["prepared", "prepared"]), 30),
      ).toBe(10);
      expect(progressAfter(recorded, journal(target, ["confirmed"]), 40)).toBe(
        40,
      );
      expect(
        progressAfter(
          recorded,
          journal(target, ["confirmed", "submitted"]),
          50,
        ),
      ).toBe(40);
      expect(
        progressAfter(
          recorded,
          journal(target, ["confirmed", "confirmed"]),
          60,
        ),
      ).toBe(60);
      // A correction of another header starts afresh.
      expect(
        progressAfter(recorded, journal("12".repeat(28), ["prepared"]), 70),
      ).toBe(70);
    });

    it("credits progress through the journal store the correction saves to", async () => {
      const recorded = health();
      const saved: TimeoutCorrectionJournal[] = [];
      const store = withTimeoutCorrectionProgress(
        {
          load: async () => saved.at(-1),
          save: async (next) => {
            saved.push(next);
          },
        },
        recorded,
      );
      await store.save(journal(target, ["confirmed"]));
      expect(saved).toHaveLength(1);
      expect(await store.load()).toBe(saved[0]);
      const credited = Effect.runSync(Ref.get(recorded));
      expect(credited.lastProgressAtMs).toBeGreaterThan(0);
      expect(credited.correctionProgress).toEqual({
        targetHeaderHash: target,
        confirmedRemovals: 1,
      });
    });
  });
});
