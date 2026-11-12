/**
 * A node that has decided to exit non-zero has the roles' shared exit
 * deadline for its teardown: a finalizer that hangs past it exits the
 * process anyway (exit code 1). A failure that holds the node up arms
 * nothing, and outside `runListen` (no `NodeProcessExit`) nothing is armed.
 */
import { Deferred, Effect, FiberId, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  NODE_EXIT_DEADLINE_MS,
  NodeProcessExit,
  scopedWithExitDeadline,
  untilTransientExhaustion,
} from "../src/commands/listen.exit-deadline.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { ADMISSION_WRITE_SHUTDOWN_FLUSH_MS } from "../src/services/admission-writer.js";
import { startupStepFailed } from "../src/services/startup-waiting.js";
import {
  signalTransientExhausted,
  type TransientBudgetExhaustedError,
} from "../src/services/transient-exhaustion.js";

const silent = Logger.replace(Logger.defaultLogger, Logger.none);

/** A scope whose finalizer never finishes. */
const hangingFinalizer = Effect.addFinalizer(() => Effect.never);

const exhaustedStep = () =>
  startupStepFailed({
    step: "database_pool:write",
    reason: "database_unreachable",
    cause: new Error("connect ECONNREFUSED"),
    exhausted: true,
    attempts: 3,
  });

/** Runs `program` with a fake process exit, returning the codes it exits with. */
const runWithExit = <A, E>(
  program: Effect.Effect<A, E>,
  processExit = true,
) => {
  const codes: number[] = [];
  const located = processExit
    ? program.pipe(
        Effect.locally(NodeProcessExit, {
          exit: (code) => codes.push(code),
          deadlineMs: 20,
        }),
      )
    : program;
  Effect.runFork(located.pipe(Effect.provide(silent)));
  return codes;
};

const settles = () => new Promise((resolve) => setTimeout(resolve, 80));

describe("the node's exit deadline", () => {
  it("exits 1 when a scope that ends the node hangs in its finalizers", async () => {
    const codes = runWithExit(
      scopedWithExitDeadline(
        Effect.zipRight(hangingFinalizer, Effect.fail(exhaustedStep())),
      ),
    );
    await vi.waitFor(() => expect(codes).toEqual([1]), { timeout: 2_000 });
  });

  it("arms nothing for a failure that holds the node up, or outside runListen", async () => {
    const held = runWithExit(
      scopedWithExitDeadline(
        Effect.zipRight(hangingFinalizer, Effect.fail(new Error("held"))),
      ),
    );
    const noProcess = runWithExit(
      scopedWithExitDeadline(
        Effect.zipRight(hangingFinalizer, Effect.fail(exhaustedStep())),
      ),
      false,
    );
    await settles();
    expect(held).toEqual([]);
    expect(noProcess).toEqual([]);
  });

  it("exits 1 when a spent transient bound interrupts a node that hangs in its finalizers", async () => {
    const exhaustion = Deferred.unsafeMake<
      never,
      TransientBudgetExhaustedError
    >(FiberId.none);
    const codes = runWithExit(
      untilTransientExhaustion(
        Effect.scoped(Effect.zipRight(hangingFinalizer, Effect.never)),
        exhaustion,
      ),
    );
    await settles();
    expect(codes).toEqual([]);
    signalTransientExhausted(exhaustion, {
      source: "driver",
      reason: "l1_events_ingestion_failed",
      detail: "the database did not answer",
    });
    await vi.waitFor(() => expect(codes).toEqual([1]), { timeout: 2_000 });
  });

  it("exits 1 when the teardown after a startup exit hangs", async () => {
    const codes = runWithExit(
      Effect.scoped(
        Effect.zipRight(
          hangingFinalizer,
          withStartupHttpServer(0, () => Effect.fail(exhaustedStep())),
        ),
      ),
    );
    await vi.waitFor(() => expect(codes).toEqual([1]), { timeout: 5_000 });
  }, 20_000);
});

it("outlasts the admission writer's bounded shutdown flush", () => {
  expect(NODE_EXIT_DEADLINE_MS).toBeGreaterThan(
    ADMISSION_WRITE_SHUTDOWN_FLUSH_MS,
  );
});
