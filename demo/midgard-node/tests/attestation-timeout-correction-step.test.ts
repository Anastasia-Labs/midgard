import { Cause, Effect, Exit, FiberId, Runtime } from "effect";
import { UnknownException } from "effect/Cause";
import { describe, expect, it } from "vitest";

import {
  attestationTimeoutCorrectionStep,
  findStateQueueCorrectionRewindIntegrityError,
} from "../src/fibers/attestation-timeout-correction.js";
import { StateQueueCorrectionRewindIntegrityError } from "../src/services/state-queue-correction-observer.js";

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
    const exit = await Effect.runPromiseExit(
      attestationTimeoutCorrectionStep(
        Effect.fail(new Error("Kupo unavailable")),
      ),
    );
    expect(Exit.isSuccess(exit)).toBe(true);
    const defect = await Effect.runPromiseExit(
      attestationTimeoutCorrectionStep(Effect.die(new Error("boom"))),
    );
    expect(Exit.isSuccess(defect)).toBe(true);
  });

  it("fails with the rewind integrity failure however it is wrapped, so the fiber stops", async () => {
    const error = integrity();
    for (const [shape, cause] of Object.entries(wrappings(error))) {
      const exit = await Effect.runPromiseExit(
        attestationTimeoutCorrectionStep(Effect.failCause(cause)),
      );
      if (!Exit.isFailure(exit)) throw new Error(`${shape}: must fail`);
      const failures = [...Cause.failures(exit.cause)];
      expect(failures, shape).toHaveLength(1);
      expect(failures[0], shape).toBe(error);
    }
  });
});
