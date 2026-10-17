import "./utils.js";

import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { takeCommitWorkerOutput } from "../src/fibers/block-commitment.js";
import type { CommitWorkerMessage } from "../src/fibers/block-commitment.promote-or-recover-native-mpf.js";
import { shouldSkipIdleCommitPipelineBeforeSchedulerAlignment } from "../src/fibers/block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import { Globals } from "../src/services/globals.js";
import { readyz } from "./readiness-honest-degradation-route.readyz-fixture.js";

/**
 * The commit worker's build alarms reach `/readyz` through the parent's
 * message handler. `commit_build_held:<stage>` fails readiness while builds
 * hold, until a block commits or nothing is left to build.
 * `commit_stage_breaker:<stage>` is only a
 * detail, so the node stays ready, once a stage's rejections reach a build's
 * limit, until a block commits from a build that reached none.
 */

const held = (stage: string): CommitWorkerMessage => ({
  type: "CommitBuildAlarmNotice",
  alarm: "held",
  stage,
});
const breaker = (stage: string): CommitWorkerMessage => ({
  type: "CommitBuildAlarmNotice",
  alarm: "breaker",
  stage,
});
const FAILED: CommitWorkerMessage = {
  type: "FailureOutput",
  error: "Holding the block build",
};
const COMMITTED: CommitWorkerMessage = {
  type: "SuccessfulSubmissionOutput",
  submittedTxHash: "aa".repeat(32),
  txSize: 1,
  mempoolTxsCount: 1,
  sizeOfBlocksTxs: 1,
  blockEndTimeMs: 1,
  mempoolLedgerDeletedOutRefHexes: [],
};

/** `/readyz` after the worker posts `messages`, then `after` runs:
 * whether the node is ready, and the commit alarm reasons and details it
 * names. */
const readyzAfterThen = async (
  messages: readonly CommitWorkerMessage[],
  after: Effect.Effect<unknown, unknown, unknown>,
) => {
  const response = await readyz({
    journal: Effect.flatMap(Globals, (globals) =>
      Effect.sync(() => {
        for (const message of messages)
          takeCommitWorkerOutput(globals, message, 16);
      }),
    ).pipe(Effect.zipRight(after)) as unknown as Effect.Effect<
      void,
      unknown,
      never
    >,
  });
  const alarms = (names: readonly string[]) =>
    names.filter(
      (name) =>
        name.startsWith("commit_build_held") ||
        name.startsWith("commit_stage_breaker"),
    );
  return {
    ready: response.ready,
    reasons: alarms(response.reasons),
    details: alarms(response.details),
  };
};
const readyzAfter = (...messages: readonly CommitWorkerMessage[]) =>
  readyzAfterThen(messages, Effect.void);

describe("GET /readyz commit build alarms", () => {
  it("is unready naming the stage held builds stop at until a block commits", async () => {
    expect(await readyzAfter(held("Phase B"), FAILED)).toMatchObject({
      reasons: ["commit_build_held:phase_b"],
      details: [],
    });
    expect(
      await readyzAfter(held("Phase B"), FAILED, held("Phase A"), FAILED),
    ).toMatchObject({ reasons: ["commit_build_held:phase_a"] });
    expect(await readyzAfter(held("Phase B"), FAILED, COMMITTED)).toEqual({
      ready: true,
      reasons: [],
      details: [],
    });
  }, 60_000);

  it("drops a held build once the idle preflight finds nothing left to build, and only then", async () => {
    const preflight = (processedUnsubmitted: number) =>
      Effect.flatMap(Globals, (globals) =>
        Ref.set(globals.PROCESSED_UNSUBMITTED_TXS_COUNT, processedUnsubmitted),
      ).pipe(
        Effect.zipRight(shouldSkipIdleCommitPipelineBeforeSchedulerAlignment),
      );
    expect(
      await readyzAfterThen(
        [held("Program material coverage"), FAILED],
        preflight(0),
      ),
    ).toEqual({ ready: true, reasons: [], details: [] });
    expect(
      await readyzAfterThen(
        [held("Program material coverage"), FAILED],
        preflight(1),
      ),
    ).toMatchObject({
      reasons: ["commit_build_held:program_material_coverage"],
    });
  }, 60_000);

  it("stays ready naming a stage that reached its rejection limit until a block commits from a build that reached none", async () => {
    expect(
      await readyzAfter(breaker("Program material coverage"), COMMITTED),
    ).toEqual({
      ready: true,
      reasons: [],
      details: ["commit_stage_breaker:program_material_coverage"],
    });
    expect(
      await readyzAfter(breaker("Phase A"), COMMITTED, breaker("Phase A"), {
        type: "NothingToCommitOutput",
      }),
    ).toEqual({
      ready: true,
      reasons: [],
      details: ["commit_stage_breaker:phase_a"],
    });
    expect(await readyzAfter(breaker("Phase A"), COMMITTED, COMMITTED)).toEqual(
      { ready: true, reasons: [], details: [] },
    );
  }, 60_000);
});
