import { Effect, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { shouldSkipIdleCommitPipelineBeforeSchedulerAlignment } from "../src/fibers/block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import { Globals } from "../src/services/globals.js";
import {
  activeLivenessReasons,
  FIBER_HALT_SOURCES,
  raiseLivenessIncident,
} from "../src/services/liveness-halt.js";
import { provideDatabaseLayers } from "./utils.js";

const backlog = vi.hoisted(() => ({
  mempool: 0,
  deposits: 0,
  forced: 0,
  withdrawals: 0,
}));
vi.mock("../src/database/index.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/database/index.js")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    MempoolDB: {
      ...actual.MempoolDB,
      retrieveTxCount: Effect.sync(() => backlog.mempool),
    },
    DepositsDB: {
      ...actual.DepositsDB,
      retrievePendingHeaderEntriesUpTo: () =>
        Effect.succeed(
          Array(backlog.deposits).fill({
            status: "projected",
            projected_header_hash: null,
          }),
        ),
    },
    ForcedTransactionsDB: {
      ...actual.ForcedTransactionsDB,
      retrievePendingHeaderEntriesUpTo: () =>
        Effect.succeed(Array(backlog.forced).fill({})),
    },
    WithdrawalsDB: {
      ...actual.WithdrawalsDB,
      retrievePendingHeaderEntriesUpTo: () =>
        Effect.succeed(Array(backlog.withdrawals).fill({})),
    },
  };
});
vi.mock("../src/fibers/queue-metrics.js", async () => {
  const { Effect } = await import("effect");
  return { emitQueueStateMetrics: Effect.void };
});

// The worker's no-op can exclude events behind its source-owned end time.
// Only the parent's independent pending-work query proves the node is idle.
describe("failed commit worker readiness at the canonical idle preflight", () => {
  it.each([
    ["empty chain", {}, 0, false, true, []],
    [
      "due projected deposits",
      { deposits: 5 },
      0,
      false,
      false,
      ["commit_worker_failed"],
    ],
    [
      "queued transfers",
      { mempool: 2 },
      0,
      false,
      false,
      ["commit_worker_failed"],
    ],
    ["processed transfers", {}, 2, false, false, ["commit_worker_failed"]],
    ["pending local finalization", {}, 0, true, true, ["commit_worker_failed"]],
  ] as const)(
    "%s",
    async (
      _name,
      pending,
      processed,
      finalizationPending,
      expectedSkip,
      expectedReasons,
    ) => {
      Object.assign(
        backlog,
        { mempool: 0, deposits: 0, forced: 0, withdrawals: 0 },
        pending,
      );
      const result = await Effect.runPromise(
        Effect.gen(function* () {
          const globals = yield* Globals;
          yield* Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, processed);
          yield* Ref.set(
            globals.LOCAL_FINALIZATION_PENDING,
            finalizationPending,
          );
          yield* raiseLivenessIncident(
            globals,
            "commit_worker",
            "commit_worker_failed",
            "worker failed",
          );
          const skipped =
            yield* shouldSkipIdleCommitPipelineBeforeSchedulerAlignment;
          return {
            skipped,
            reasons: (yield* activeLivenessReasons(globals)).map(
              ({ reason }) => reason,
            ),
          };
        }).pipe(Effect.provide(Globals.Default), provideDatabaseLayers),
      );
      expect(result.skipped).toBe(expectedSkip);
      expect(result.reasons).toEqual(expectedReasons);
    },
  );

  it("does not hold any retrying fiber", () => {
    for (const sources of Object.values(FIBER_HALT_SOURCES)) {
      expect(sources as readonly string[]).not.toContain("commit_worker");
    }
  });
});
