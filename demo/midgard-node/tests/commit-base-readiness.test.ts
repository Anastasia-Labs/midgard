/**
 * A commit waiting for its base (a foreign tail landed-block processing has
 * not applied yet) names that wait on `/readyz`: `commit_base_pending`
 * under `commit_base`, raised by the worker's wait output and cleared by
 * its next output that is not the wait. A commit waiting for the next
 * window (a signed abandoned journal holds its header hash) names
 * `commit_window_pending` under `commit_window` by the same rule. A
 * submission refused before broadcast names `commit_submit_refused:<code>`
 * under `commit_submit`, a detail until its run escalates.
 */
import { Effect, Logger } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  applyCommitWorkerReadiness,
  clearCommitWorkerFailure,
  COMMIT_BASE_PENDING,
  COMMIT_BASE_SOURCE,
  COMMIT_SUBMIT_REFUSED,
  COMMIT_SUBMIT_SOURCE,
  COMMIT_WINDOW_PENDING,
  COMMIT_WINDOW_SOURCE,
  COMMIT_WORKER_FAILED,
  COMMIT_WORKER_FAILED_ESCALATION_MS,
  COMMIT_WORKER_SOURCE,
  livenessReadiness,
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

const AWAITING_WINDOW: WorkerOutput = {
  type: "AwaitingNextCommitWindowOutput",
  heldHeaderHash: "ee".repeat(28),
  detail: "a signed abandoned journal holds this header hash",
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

describe("commit window readiness", () => {
  it("raises commit_window_pending naming the held header while the commit waits for the next window", async () => {
    const logs: string[] = [];
    const reasons = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* applyCommitWorkerReadiness(globals, AWAITING_WINDOW);
        return yield* activeLivenessReasons(globals);
      }).pipe(
        Effect.provide(Globals.Default),
        Effect.provide(
          Logger.replace(
            Logger.defaultLogger,
            Logger.make(({ message }) => {
              logs.push([message].flat().map(String).join(" "));
            }),
          ),
        ),
      ),
    );
    expect(reasons.map(({ source, reason }) => ({ source, reason }))).toEqual([
      { source: COMMIT_WINDOW_SOURCE, reason: COMMIT_WINDOW_PENDING },
    ]);
    expect(logs).toContain(
      `${COMMIT_WINDOW_PENDING}: header ${"ee".repeat(28)}: a signed abandoned journal holds this header hash`,
    );
  });

  it("clears it on the next output that is not the wait, and each wait clears the other", async () => {
    expect(
      await reasonsAfter(AWAITING_WINDOW, { type: "NothingToCommitOutput" }),
    ).toEqual([]);
    expect(await reasonsAfter(AWAITING_BASE, AWAITING_WINDOW)).toEqual([
      { source: COMMIT_WINDOW_SOURCE, reason: COMMIT_WINDOW_PENDING },
    ]);
    expect(await reasonsAfter(AWAITING_WINDOW, AWAITING_BASE)).toEqual([
      { source: COMMIT_BASE_SOURCE, reason: COMMIT_BASE_PENDING },
    ]);
  });

  it("is cleared by the idle preflight's clear", async () => {
    const reasons = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* applyCommitWorkerReadiness(globals, AWAITING_WINDOW);
        yield* clearCommitWorkerFailure(globals);
        return yield* activeLivenessReasons(globals);
      }).pipe(Effect.provide(Globals.Default)),
    );
    expect(reasons).toEqual([]);
  });
});

// A worker that fails every tick for longer than the bound is escalated, so a
// stalled commit pipeline is named as one on /readyz, with the failure's first
// line, instead of only as a reason that is always raised.
describe("commit worker failure readiness", () => {
  it("names the failure and escalates it once it outlasts the bound", async () => {
    const failure: WorkerOutput = {
      type: "FailureOutput",
      error:
        "Block commitment worker failure: StateQueueError: Commit paused until the L1 slot is known\n    at file:///dist/chunk.js:1:1283",
    };
    const [inside, past] = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const startMs = Date.now();
        yield* applyCommitWorkerReadiness(globals, failure);
        yield* applyCommitWorkerReadiness(globals, failure);
        return [
          yield* activeLivenessReasons(globals, startMs + 60_000),
          yield* activeLivenessReasons(
            globals,
            startMs + COMMIT_WORKER_FAILED_ESCALATION_MS + 1_000,
          ),
        ] as const;
      }).pipe(Effect.provide(Globals.Default)),
    );
    expect(inside).toEqual([
      expect.objectContaining({
        source: COMMIT_WORKER_SOURCE,
        reason: COMMIT_WORKER_FAILED,
        detail:
          "Block commitment worker failure: StateQueueError: Commit paused until the L1 slot is known",
        escalateAfterMs: COMMIT_WORKER_FAILED_ESCALATION_MS,
        escalated: false,
      }),
    ]);
    expect(past).toEqual([
      expect.objectContaining({
        reason: COMMIT_WORKER_FAILED,
        escalated: true,
      }),
    ]);
  });
});

const refused = (
  code: "COMMIT_ANCHOR_NOT_CANONICAL" | "SIGNING_FAILED",
): WorkerOutput => ({
  type: "SkippedSubmissionOutput",
  mempoolTxsCount: 0,
  sizeOfProcessedTxs: 0,
  submitRefusal: { code, detail: `${code} refusal\n    at frame` },
});

// A submission refused before broadcast is retried with its payload, so it
// is a detail until its unbroken run outlasts the bound, then a reason; a
// change of code does not restart the run, and a commit clears it.
describe("commit submit refusal readiness", () => {
  it("names the refusal's code and is a detail until its unbroken run outlasts the bound", async () => {
    const [inside, past] = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const startMs = Date.now();
        yield* applyCommitWorkerReadiness(
          globals,
          refused("COMMIT_ANCHOR_NOT_CANONICAL"),
        );
        yield* applyCommitWorkerReadiness(globals, {
          type: "NothingToCommitOutput",
        });
        yield* applyCommitWorkerReadiness(globals, refused("SIGNING_FAILED"));
        return [
          yield* activeLivenessReasons(globals, startMs + 60_000),
          yield* activeLivenessReasons(
            globals,
            startMs + COMMIT_WORKER_FAILED_ESCALATION_MS + 1_000,
          ),
        ] as const;
      }).pipe(Effect.provide(Globals.Default)),
    );
    expect(inside).toEqual([
      expect.objectContaining({
        source: COMMIT_SUBMIT_SOURCE,
        reason: `${COMMIT_SUBMIT_REFUSED}:SIGNING_FAILED`,
        detail: "SIGNING_FAILED refusal",
        escalateAfterMs: COMMIT_WORKER_FAILED_ESCALATION_MS,
        escalated: false,
      }),
    ]);
    expect(livenessReadiness(inside)).toEqual({
      reasons: [],
      details: [`${COMMIT_SUBMIT_REFUSED}:SIGNING_FAILED`],
    });
    expect(past).toEqual([
      expect.objectContaining({
        reason: `${COMMIT_SUBMIT_REFUSED}:SIGNING_FAILED`,
        escalated: true,
      }),
    ]);
    expect(livenessReadiness(past)).toEqual({ reasons: past, details: [] });
  });

  describe("on the node's clock", () => {
    afterEach(() => {
      vi.useRealTimers();
    });

    it("a change of code does not restart the run", async () => {
      vi.useFakeTimers({ toFake: ["Date"], now: Date.now() });
      const reasons = await Effect.runPromise(
        Effect.gen(function* () {
          const globals = yield* Globals;
          yield* applyCommitWorkerReadiness(
            globals,
            refused("COMMIT_ANCHOR_NOT_CANONICAL"),
          );
          vi.advanceTimersByTime(6 * 60_000);
          yield* applyCommitWorkerReadiness(globals, refused("SIGNING_FAILED"));
          vi.advanceTimersByTime(5 * 60_000);
          return livenessReadiness(yield* activeLivenessReasons(globals))
            .reasons;
        }).pipe(Effect.provide(Globals.Default)),
      );
      expect(reasons).toEqual([
        expect.objectContaining({
          source: COMMIT_SUBMIT_SOURCE,
          reason: `${COMMIT_SUBMIT_REFUSED}:SIGNING_FAILED`,
          escalated: true,
        }),
      ]);
    });
  });

  it("clears on the next commit, and a skip that was not refused leaves it", async () => {
    expect(
      await reasonsAfter(refused("COMMIT_ANCHOR_NOT_CANONICAL"), {
        type: "SkippedSubmissionOutput",
        mempoolTxsCount: 0,
        sizeOfProcessedTxs: 0,
      }),
    ).toEqual([
      {
        source: COMMIT_SUBMIT_SOURCE,
        reason: `${COMMIT_SUBMIT_REFUSED}:COMMIT_ANCHOR_NOT_CANONICAL`,
      },
    ]);
    expect(
      await reasonsAfter(refused("COMMIT_ANCHOR_NOT_CANONICAL"), {
        type: "SuccessfulSubmissionOutput",
        submittedTxHash: "ab".repeat(32),
        txSize: 0,
        mempoolTxsCount: 0,
        sizeOfBlocksTxs: 0,
        blockEndTimeMs: 0,
        mempoolLedgerDeletedOutRefHexes: [],
      }),
    ).toEqual([]);
  });
});
