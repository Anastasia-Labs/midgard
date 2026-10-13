import type {
  CommitBuildAlarmNotice,
  WorkerOutput,
} from "../workers/utils/commit-block-header.js";

/**
 * The commit build alarms `/readyz` names. `held` is the commit stage the
 * last held build stopped at, until a block commits. `breaker` lists the
 * stages whose rejections reached the build's limit, until a block commits
 * from a build that reached none; `breakerThisBuild` collects the running
 * build's.
 */
export type CommitBuildAlarms = {
  readonly held: string | null;
  readonly breaker: readonly string[];
  readonly breakerThisBuild: readonly string[];
};

export const NO_COMMIT_BUILD_ALARMS: CommitBuildAlarms = {
  held: null,
  breaker: [],
  breakerThisBuild: [],
};

export const COMMIT_BUILD_HELD = "commit_build_held";
export const COMMIT_STAGE_BREAKER = "commit_stage_breaker";

const withStage = (stages: readonly string[], stage: string) =>
  stages.includes(stage) ? stages : [...stages, stage];

/** The alarms after the worker's notice. */
export const noticeCommitBuildAlarm = (
  alarms: CommitBuildAlarms,
  notice: CommitBuildAlarmNotice,
): CommitBuildAlarms =>
  notice.alarm === "held"
    ? { ...alarms, held: notice.stage }
    : {
        ...alarms,
        breaker: withStage(alarms.breaker, notice.stage),
        breakerThisBuild: withStage(alarms.breakerThisBuild, notice.stage),
      };

/** The alarms once the worker's build ends with `output`. */
export const settleCommitBuildAlarms = (
  alarms: CommitBuildAlarms,
  output: WorkerOutput,
): CommitBuildAlarms => {
  switch (output.type) {
    case "SuccessfulSubmissionOutput":
    case "SubmittedAwaitingConfirmationOutput":
    case "SubmittedAwaitingLocalFinalizationOutput":
      return {
        held: null,
        breaker: alarms.breakerThisBuild,
        breakerThisBuild: [],
      };
    case "SkippedSubmissionOutput":
    case "NothingToCommitOutput":
    case "FailureOutput":
    case "RegisteredDueWorkOutput":
    case "AwaitingCommitBaseOutput":
    case "AwaitingNextCommitWindowOutput":
    case "SuccessfulLocalFinalizationRecoveryOutput":
      return { ...alarms, breakerThisBuild: [] };
  }
};

const stageName = (stage: string) =>
  stage
    .toLowerCase()
    .replace(/[^a-z0-9]+/gu, "_")
    .replace(/^_|_$/gu, "");

/**
 * What `/readyz` names for the alarms. A held build is a reason: the node is
 * not producing blocks. A tripped breaker is only a detail: the node still
 * commits blocks, without the deferred transactions.
 */
export const commitBuildAlarmReadiness = (
  alarms: CommitBuildAlarms,
): {
  readonly reasons: readonly string[];
  readonly details: readonly string[];
} => ({
  reasons:
    alarms.held === null
      ? []
      : [`${COMMIT_BUILD_HELD}:${stageName(alarms.held)}`],
  details: alarms.breaker.map(
    (stage) => `${COMMIT_STAGE_BREAKER}:${stageName(stage)}`,
  ),
});
