/**
 * A commit waiting for its base (a foreign tail landed-block processing has
 * not applied yet) names that wait on `/readyz`: `commit_base_pending`
 * under `commit_base`, raised by the worker's wait output and cleared by
 * its next output that is not the wait.
 */
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyCommitWorkerReadiness,
  clearCommitWorkerFailure,
  COMMIT_BASE_PENDING,
  COMMIT_BASE_SOURCE,
  COMMIT_WORKER_FAILED,
  COMMIT_WORKER_SOURCE,
} from "../src/fibers/block-commitment.worker-readiness.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import type { WorkerOutput } from "../src/workers/utils/commit-block-header.js";

const BASE = "dd".repeat(28);

const AWAITING_BASE: WorkerOutput = {
  type: "AwaitingCommitBaseOutput",
  baseHeaderHash: BASE,
  detail: "the foreign commit base is not processed yet",
};

/** The raised reasons after applying `outputs` in order. */
const reasonsAfter = (...outputs: readonly WorkerOutput[]) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      for (const output of outputs)
        yield* applyCommitWorkerReadiness(globals, output);
      return (yield* activeLivenessReasons(globals)).map(
        ({ source, reason }) => ({ source, reason }),
      );
    }).pipe(Effect.provide(Globals.Default)),
  );

describe("commit base readiness", () => {
  it("raises commit_base_pending while the commit waits for its base", async () => {
    expect(await reasonsAfter(AWAITING_BASE)).toEqual([
      {
        source: COMMIT_BASE_SOURCE,
        reason: COMMIT_BASE_PENDING,
      },
    ]);
  });

  it("clears it on the next output that is not the wait", async () => {
    expect(
      await reasonsAfter(AWAITING_BASE, { type: "NothingToCommitOutput" }),
    ).toEqual([]);
    expect(
      await reasonsAfter(AWAITING_BASE, {
        type: "FailureOutput",
        error: "boom",
      }),
    ).toEqual([{ source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED }]);
  });

  it("is cleared by the idle preflight's clear", async () => {
    const reasons = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* applyCommitWorkerReadiness(globals, AWAITING_BASE);
        yield* clearCommitWorkerFailure(globals);
        return yield* activeLivenessReasons(globals);
      }).pipe(Effect.provide(Globals.Default)),
    );
    expect(reasons).toEqual([]);
  });
});
