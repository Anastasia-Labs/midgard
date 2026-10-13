import "./utils.js";

import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { takeCommitWorkerOutput } from "../src/fibers/block-commitment.js";
import type { CommitWorkerMessage } from "../src/fibers/block-commitment.promote-or-recover-native-mpf.js";
import { Globals } from "../src/services/globals.js";
import { readyz } from "./readiness-honest-degradation-route.readyz-fixture.js";

/**
 * The commit worker's build alarms reach `/readyz` through the parent's
 * message handler: `commit_build_held:<stage>` while builds hold, until a
 * block commits, and `commit_stage_breaker:<stage>` once a stage's
 * rejections reach a build's limit, until a block commits from a build that
 * reached none.
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

/** The commit alarm reasons `/readyz` names after the worker posts
 * `messages`. */
const alarmsAfter = async (...messages: readonly CommitWorkerMessage[]) => {
  const response = await readyz({
    journal: Effect.flatMap(Globals, (globals) =>
      Effect.sync(() => {
        for (const message of messages)
          takeCommitWorkerOutput(globals, message, 16);
      }),
    ) as unknown as Effect.Effect<void, unknown, never>,
  });
  return response.reasons.filter(
    (reason) =>
      reason.startsWith("commit_build_held") ||
      reason.startsWith("commit_stage_breaker"),
  );
};

describe("GET /readyz commit build alarms", () => {
  it("names the stage held builds stop at until a block commits", async () => {
    expect(await alarmsAfter(held("Phase B"), FAILED)).toEqual([
      "commit_build_held:phase_b",
    ]);
    expect(
      await alarmsAfter(held("Phase B"), FAILED, held("Phase A"), FAILED),
    ).toEqual(["commit_build_held:phase_a"]);
    expect(await alarmsAfter(held("Phase B"), FAILED, COMMITTED)).toEqual([]);
  }, 60_000);

  it("names a stage that reached its rejection limit until a block commits from a build that reached none", async () => {
    expect(
      await alarmsAfter(breaker("Program material coverage"), COMMITTED),
    ).toEqual(["commit_stage_breaker:program_material_coverage"]);
    expect(
      await alarmsAfter(breaker("Phase A"), COMMITTED, breaker("Phase A"), {
        type: "NothingToCommitOutput",
      }),
    ).toEqual(["commit_stage_breaker:phase_a"]);
    expect(await alarmsAfter(breaker("Phase A"), COMMITTED, COMMITTED)).toEqual(
      [],
    );
  }, 60_000);
});
