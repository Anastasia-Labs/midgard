import { Cause, Effect, Exit, Metric, Ref, Runtime } from "effect";
import { describe, expect, it } from "vitest";

import {
  blockConfirmationStep,
  reachedSignedIntentDecision,
} from "../src/fibers/block-confirmation.block-confirmation-fiber.js";
import { SignedIntentReplacementIntegrityError } from "../src/services/canonical-journal-recovery.js";
import {
  HaltSource,
  livenessIncidentCounter,
} from "../src/services/liveness-halt.js";
import { SIGNED_INTENT_UNDECIDED } from "../src/services/signed-intent-undecided.js";
import type { SerializedStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";

const liveness = () => ({
  LIVENESS_REASONS: Ref.unsafeMake<ReadonlyMap<string, string>>(new Map()),
});

const raised = (globals: ReturnType<typeof liveness>) =>
  Effect.runSync(Ref.get(globals.LIVENESS_REASONS)).get(
    HaltSource.blockConfirmationSignedIntent,
  );

const incidents = () =>
  Effect.runSync(
    Metric.value(
      Metric.tagged(livenessIncidentCounter, "reason", SIGNED_INTENT_UNDECIDED),
    ),
  ).count;

const integrity = () =>
  new SignedIntentReplacementIntegrityError(
    "ab".repeat(32),
    "its replacement was already locally finalized",
  );

describe("block confirmation step", () => {
  it("logs a transient failure and succeeds, raising nothing", async () => {
    const globals = liveness();
    for (const action of [
      Effect.fail(new Error("Ogmios unavailable")),
      Effect.die(new Error("worker crashed")),
    ]) {
      const exit = await Effect.runPromiseExit(
        blockConfirmationStep(action, globals),
      );
      expect(Exit.isSuccess(exit)).toBe(true);
    }
    expect(raised(globals)).toBeUndefined();
  });

  it("raises signed_intent_undecided for the replacement integrity failure however it is wrapped, and never fails", async () => {
    const error = integrity();
    for (const cause of [
      Cause.fail(error),
      Cause.die(error),
      Cause.die(Runtime.makeFiberFailure(Cause.fail(error))),
      Cause.fail(new Error("worker failed", { cause: error })),
    ]) {
      const globals = liveness();
      const exit = await Effect.runPromiseExit(
        blockConfirmationStep(Effect.failCause(cause), globals),
      );
      expect(Exit.isSuccess(exit)).toBe(true);
      expect(raised(globals)).toBe(SIGNED_INTENT_UNDECIDED);
    }
  });

  it("keeps the reason raised until a tick reaches the decision again, then clears it", async () => {
    const globals = liveness();
    const outcomes = [
      "undecided",
      "transient",
      "returned_early",
      "undecided",
      "decided",
    ];
    const seen: (string | undefined)[] = [];
    const before = incidents();
    for (const outcome of outcomes) {
      await Effect.runPromise(
        blockConfirmationStep(
          outcome === "decided" || outcome === "returned_early"
            ? Effect.succeed(outcome === "decided")
            : Effect.fail(
                outcome === "undecided"
                  ? integrity()
                  : new Error("Ogmios unavailable"),
              ),
          globals,
        ),
      );
      seen.push(raised(globals));
    }
    expect(seen).toEqual([
      SIGNED_INTENT_UNDECIDED,
      SIGNED_INTENT_UNDECIDED,
      SIGNED_INTENT_UNDECIDED,
      SIGNED_INTENT_UNDECIDED,
      undefined,
    ]);
    // A tick that returned before the decision neither clears the reason nor
    // restarts its incident (and with it the escalation clock).
    expect(incidents() - before).toBe(1);
  });

  it("counts a tick as deciding only when its worker output reached the decision on an unchanged active journal", () => {
    const pending = {
      expectedHeaderHash: "aa".repeat(32),
      submittedTxHash: "bb".repeat(32),
      intendedTxHash: null,
      blockEndTimeMs: 1_000,
      updatedAtMs: 2_000,
    };
    const input = (pendingBlock: typeof pending | null) => ({
      data: { firstRun: false, pendingBlock },
    });
    // Only whether the worker matched a pending block is read.
    const snapshot = {} as SerializedStateQueueUTxO;
    const success = (matched: SerializedStateQueueUTxO | null) => ({
      type: "SuccessfulConfirmationOutput" as const,
      latestBlocksUTxO: snapshot,
      matchedPendingBlocksUTxO: matched,
      canonicalHeaders: [],
    });
    // Reached: no pending journal, or one the worker did not match and that
    // did not change under the tick.
    expect(reachedSignedIntentDecision(input(null), success(null), null)).toBe(
      true,
    );
    expect(
      reachedSignedIntentDecision(input(pending), success(null), {
        ...pending,
      }),
    ).toBe(true);
    // Not reached: the worker never ran or confirmed nothing new, it matched
    // the pending block, or the active journal changed (a stale-snapshot
    // guard may have discarded the output).
    expect(reachedSignedIntentDecision(undefined, undefined, null)).toBe(false);
    for (const output of [
      { type: "NoTxForConfirmationOutput" as const },
      { type: "FailedConfirmationOutput" as const, error: "failed" },
      success(snapshot),
    ])
      expect(reachedSignedIntentDecision(input(pending), output, pending)).toBe(
        false,
      );
    expect(
      reachedSignedIntentDecision(input(pending), success(null), {
        ...pending,
        updatedAtMs: 3_000,
      }),
    ).toBe(false);
    expect(
      reachedSignedIntentDecision(input(null), success(null), pending),
    ).toBe(false);
    expect(
      reachedSignedIntentDecision(input(pending), success(null), null),
    ).toBe(false);
  });
});
