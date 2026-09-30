import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

import { pipelinedCommitCrashCheckpointMarker } from "midgard-node/e2e/pipelined-commit-crash-checkpoint";

import {
  assertCheckpointTermination,
  processEnvForCheckpoint,
} from "./pipelined-commit-process-harness.capture-pipelined-commit-database-state.js";
import { type PipelinedCommitNodeProcessSpec } from "./pipelined-commit-process-harness.pipelined-commit-database-state.js";
import {
  assertSharedPostgresAndPrivateMpfStores,
  type PipelinedCommitLeaseContentionResult,
  writeStopFileAfterLogMarker,
} from "./pipelined-commit-process-harness.run-pipelined-commit-normal-lease-contention.js";
import { superviseHostProcess } from "./service-supervisor.js";

/**
 * Runs two actual node commands concurrently against an explicitly shared
 * Postgres identity. Both have private MPF stores. Exactly one process can
 * consume the journal-before-submit arm file; that process is SIGKILLed while
 * holding the mutation lease and the other remains available for TTL recovery.
 */
export const runPipelinedCommitLeaseContention = async ({
  left,
  right,
  armFile,
}: {
  readonly left: PipelinedCommitNodeProcessSpec;
  readonly right: PipelinedCommitNodeProcessSpec;
  readonly armFile: string;
}): Promise<PipelinedCommitLeaseContentionResult> => {
  assertSharedPostgresAndPrivateMpfStores(left, right);
  const checkpoint = "journal_prepared_before_submit" as const;
  await mkdir(dirname(armFile), { recursive: true });
  await writeFile(armFile, `contention:${left.nodeId}:${right.nodeId}\n`, {
    encoding: "utf8",
    flag: "wx",
  });
  const marker = pipelinedCommitCrashCheckpointMarker(checkpoint);
  const survivorSubmittedMarker = "pipeline_trace phase=candidate_submitted";
  const start = (spec: PipelinedCommitNodeProcessSpec) => {
    const stopFile = join(dirname(armFile), `${spec.nodeId}.submitted.stop`);
    const summary = superviseHostProcess({
      ...spec.process,
      service: `${spec.process.service}:${spec.nodeId}:contention`,
      env: processEnvForCheckpoint({ spec, checkpoint, armFile }),
      maxRestarts: 0,
      terminateOnOutput: { marker, signal: "SIGKILL" },
      terminateOnFile: { path: stopFile, signal: "SIGTERM" },
    });
    const stopAfterSubmission = Promise.race([
      writeStopFileAfterLogMarker({
        logPath: spec.process.rawLogPath,
        marker: survivorSubmittedMarker,
        stopFile,
        timeoutMs: spec.process.timeoutMs!,
      }),
      summary.then(() => undefined),
    ]);
    return { summary, stopAfterSubmission };
  };
  const leftRun = start(left);
  const rightRun = start(right);
  const [leftSummary, rightSummary] = await Promise.all([
    leftRun.summary,
    rightRun.summary,
  ]);
  await Promise.all([
    leftRun.stopAfterSubmission,
    rightRun.stopAfterSubmission,
  ]);
  const terminated = [
    { spec: left, summary: leftSummary },
    { spec: right, summary: rightSummary },
  ].filter(({ summary }) =>
    summary.attempts.some(
      (attempt) => attempt.outputTermination?.marker === marker,
    ),
  );
  if (terminated.length !== 1) {
    throw new Error(
      `Expected one journal winner to be SIGKILLed; observed ${terminated.length.toString()}`,
    );
  }
  const winner = terminated[0]!;
  assertCheckpointTermination({ summary: winner.summary, marker });
  const loser =
    winner.spec.nodeId === left.nodeId
      ? { spec: right, summary: rightSummary }
      : { spec: left, summary: leftSummary };
  const [winnerLog, loserLog] = await Promise.all([
    readFile(winner.summary.rawLogPath, "utf8"),
    readFile(loser.summary.rawLogPath, "utf8"),
  ]);
  if (!loserLog.includes("reason=state_queue_lease_busy")) {
    throw new Error(
      "Journal-kill survivor did not record state-queue lease contention",
    );
  }
  if (
    !loserLog.includes("abandoning unsubmitted journal") &&
    !loserLog.includes("submitted_tx=unknown")
  ) {
    throw new Error(
      "Journal-kill survivor did not execute unsubmitted-journal recovery after lease expiry",
    );
  }
  if (!loserLog.includes("pipeline_trace phase=candidate_submitted")) {
    throw new Error(
      "Journal-kill survivor did not submit after unsubmitted-journal recovery",
    );
  }
  return {
    winnerNodeId: winner.spec.nodeId,
    loserNodeId: loser.spec.nodeId,
    winner: winner.summary,
    loser: loser.summary,
    winnerLog,
    loserLog,
  };
};
