import type { TimeoutCorrectionJournal } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Exit, FiberId, Ref, Runtime } from "effect";
import { UnknownException } from "effect/Cause";
import { describe, expect, it } from "vitest";

import { STATE_QUEUE_CORRECTION_REWIND_CONFLICT } from "../src/fibers/attestation-timeout-correction.attestation-timeout-correction-action.js";
import {
  attestationTimeoutCorrectionReadinessBounds,
  attestationTimeoutCorrectionStep,
  findStateQueueCorrectionRewindIntegrityError,
  recordTimeoutCorrectionJournalProgress,
  withTimeoutCorrectionProgress,
} from "../src/fibers/attestation-timeout-correction.js";
import type { AttestationTimeoutCorrectionHealth } from "../src/services/globals.js";
import { HaltSource } from "../src/services/liveness-halt.js";
import { StateQueueCorrectionRewindIntegrityError } from "../src/services/state-queue-correction-observer.js";

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

const liveness = () => ({
  LIVENESS_REASONS: Ref.unsafeMake<ReadonlyMap<string, string>>(new Map()),
});

const raised = (globals: ReturnType<typeof liveness>) =>
  Effect.runSync(Ref.get(globals.LIVENESS_REASONS)).get(
    HaltSource.stateQueueCorrectionRewind,
  );

const integrity = () =>
  new StateQueueCorrectionRewindIntegrityError(
    "ab".repeat(32),
    "the authenticated state-queue view no longer removes it",
  );

/** The shapes the integrity failure takes on its way out of the observer's
 * promise callbacks: an Effect run to a promise rejects with a FiberFailure,
 * which a caller may keep as a defect, a typed failure, or an error cause. */
const wrappings = (error: StateQueueCorrectionRewindIntegrityError) => {
  const fiberFailure = Runtime.makeFiberFailure(Cause.fail(error));
  return {
    failure: Cause.fail(error),
    defect: Cause.die(error),
    "defect FiberFailure": Cause.die(fiberFailure),
    "Effect.tryPromise UnknownException": Cause.fail(
      new UnknownException(fiberFailure),
    ),
    "error cause chain": Cause.fail(
      new Error("observer scan failed", {
        cause: new Error("restore failed", { cause: fiberFailure }),
      }),
    ),
    "parallel with a transient failure": Cause.parallel(
      Cause.fail(new Error("Kupo unavailable")),
      Cause.die(fiberFailure),
    ),
  } satisfies Record<string, Cause.Cause<unknown>>;
};

describe("attestation-timeout correction step", () => {
  it("finds the rewind integrity failure through every wrapping it takes", () => {
    const error = integrity();
    for (const [shape, cause] of Object.entries(wrappings(error)))
      expect(findStateQueueCorrectionRewindIntegrityError(cause), shape).toBe(
        error,
      );
  });

  it("finds nothing in a transient failure", () => {
    const transient = new Error("Kupo unavailable", {
      cause: Runtime.makeFiberFailure(Cause.fail(new Error("timeout"))),
    });
    for (const cause of [
      Cause.fail(transient),
      Cause.die(transient),
      Cause.fail(new UnknownException(transient)),
      Cause.interrupt(FiberId.none),
    ])
      expect(
        findStateQueueCorrectionRewindIntegrityError(cause),
      ).toBeUndefined();
  });

  it("logs a transient failure and succeeds, so the schedule retries it", async () => {
    const globals = liveness();
    const exit = await Effect.runPromiseExit(
      attestationTimeoutCorrectionStep(
        Effect.fail(new Error("Kupo unavailable")),
        health(),
        globals,
      ),
    );
    expect(Exit.isSuccess(exit)).toBe(true);
    const defect = await Effect.runPromiseExit(
      attestationTimeoutCorrectionStep(
        Effect.die(new Error("boom")),
        health(),
        globals,
      ),
    );
    expect(Exit.isSuccess(defect)).toBe(true);
    // A transient failure raises nothing.
    expect(raised(globals)).toBeUndefined();
  });

  it("raises the rewind conflict however the integrity failure is wrapped, and the fiber keeps running", async () => {
    const error = integrity();
    for (const [shape, cause] of Object.entries(wrappings(error))) {
      const globals = liveness();
      const exit = await Effect.runPromiseExit(
        attestationTimeoutCorrectionStep(
          Effect.failCause(cause),
          health(),
          globals,
        ),
      );
      expect(Exit.isSuccess(exit), shape).toBe(true);
      expect(raised(globals), shape).toBe(
        STATE_QUEUE_CORRECTION_REWIND_CONFLICT,
      );
    }
  });

  it("keeps the conflict raised until a step re-derives in agreement, then clears it once", async () => {
    const globals = liveness();
    const recorded = health();
    // Each tick re-derives the removal against L1; the correction's own
    // effects run only on a tick whose re-derivation agrees.
    const outcomes: ("conflict" | "transient" | "agrees")[] = [
      "conflict",
      "conflict",
      "transient",
      "agrees",
      "agrees",
    ];
    let corrections = 0;
    const seen: (string | undefined)[] = [];
    for (const outcome of outcomes) {
      await Effect.runPromise(
        attestationTimeoutCorrectionStep(
          Effect.suspend(() => {
            if (outcome === "conflict") return Effect.fail(integrity());
            if (outcome === "transient")
              return Effect.fail(new Error("Kupo unavailable"));
            corrections += 1;
            return Effect.void;
          }),
          recorded,
          globals,
        ),
      );
      seen.push(raised(globals));
    }
    expect(seen).toEqual([
      STATE_QUEUE_CORRECTION_REWIND_CONFLICT,
      STATE_QUEUE_CORRECTION_REWIND_CONFLICT,
      // A transient failure neither clears nor re-raises it.
      STATE_QUEUE_CORRECTION_REWIND_CONFLICT,
      undefined,
      undefined,
    ]);
    // No correction ran while the re-derivation disagreed; one per agreeing
    // tick afterwards.
    expect(corrections).toBe(2);
  });

  it("counts failures since the last success for readiness, and a success resets the count", async () => {
    const recorded = health();
    const step = (action: Effect.Effect<void, unknown>) =>
      Effect.runPromiseExit(
        attestationTimeoutCorrectionStep(action, recorded, liveness()),
      );
    await step(Effect.fail(new Error("Kupo unavailable")));
    await step(Effect.die(new Error("Ogmios hung up")));
    await step(Effect.failCause(Cause.fail(integrity())));
    const failed = Ref.get(recorded).pipe(Effect.runSync);
    expect(failed).toMatchObject({
      consecutiveFailures: 3,
      lastProgressAtMs: 0,
      oldestUnattestedHeader: unattestedHeader,
    });
    expect(failed.lastFailureAtMs).toBeGreaterThan(0);
    expect(failed.lastError).toContain("the authenticated state-queue view");

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
