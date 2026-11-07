import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

import {
  assertCheckpointTermination,
  JOURNAL_KILL_CHECKPOINT_MARKER,
  processEnvForJournalCheckpoint,
} from "./journal-kill-process-harness.capture-database-state.js";
import { type JournalKillNodeProcessSpec } from "./journal-kill-process-harness.database-state.js";
import {
  type ServiceSupervisorSummary,
  superviseHostProcess,
} from "./service-supervisor.js";

/**
 * Default commit path log line printed after the L1 commit transaction is
 * submitted and local finalization waits for L1 confirmation.
 */
export const BLOCK_SUBMITTED_MARKER =
  "Block submitted; local finalization is intentionally deferred until L1 confirmation.";

/**
 * The default-path log lines the surviving node must print, in this order:
 * its commit trigger finds the lease busy while the killed winner holds it,
 * it abandons the winner's unsubmitted journal once the lease has expired and
 * the recovery grace has passed, and it then submits its own block.
 */
export const JOURNAL_KILL_SURVIVOR_MARKERS = [
  "Skipping block commitment trigger because the state-queue mutation lease is busy",
  "abandoning unsubmitted journal and recovering canonical state_queue tip",
  BLOCK_SUBMITTED_MARKER,
] as const;

/** True when every marker appears in `log`, each after the one before it. */
export const markersAppearInOrder = (
  log: string,
  markers: readonly string[],
): boolean => {
  let from = 0;
  for (const marker of markers) {
    const index = log.indexOf(marker, from);
    if (index < 0) return false;
    from = index + marker.length;
  }
  return true;
};

export type JournalKillContentionResult = {
  readonly winnerNodeId: string;
  readonly loserNodeId: string;
  readonly winner: ServiceSupervisorSummary;
  readonly loser: ServiceSupervisorSummary;
  readonly winnerLog: string;
  readonly loserLog: string;
};

export const assertSharedPostgresAndPrivateMpfStores = (
  left: JournalKillNodeProcessSpec,
  right: JournalKillNodeProcessSpec,
): void => {
  if (left.postgresIdentity !== right.postgresIdentity) {
    throw new Error(
      "Lease-contention nodes must use the same Postgres identity",
    );
  }
  if (
    left.ledgerMpfDbPath === right.ledgerMpfDbPath ||
    left.transactionsMpfDbPath === right.transactionsMpfDbPath
  ) {
    throw new Error("Lease-contention nodes must use distinct MPF store paths");
  }
  if (
    left.process.timeoutMs === undefined ||
    right.process.timeoutMs === undefined
  ) {
    throw new Error(
      "Lease-contention node specs require bounded timeouts longer than the test lease TTL",
    );
  }
  for (const spec of [left, right]) {
    if (
      !Number.isSafeInteger(spec.stateQueueMutationLeaseTtlMs) ||
      spec.stateQueueMutationLeaseTtlMs <= 0 ||
      spec.process.timeoutMs! <= spec.stateQueueMutationLeaseTtlMs * 2
    ) {
      throw new Error(
        `Lease-contention timeout for ${spec.nodeId} must exceed twice its positive test lease TTL`,
      );
    }
  }
};

export const writeStopFileAfterLogMarker = async ({
  logPath,
  marker,
  stopFile,
  timeoutMs,
}: {
  readonly logPath: string;
  readonly marker: string;
  readonly stopFile: string;
  readonly timeoutMs: number;
}): Promise<void> => {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const text = await readFile(logPath, "utf8").catch(() => "");
    if (text.includes(marker)) {
      await writeFile(stopFile, "stop\n", { encoding: "utf8", flag: "wx" });
      return;
    }
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
  throw new Error(`Timed out waiting for ${marker} in ${logPath}`);
};

/**
 * Runs two actual node commands concurrently against an explicitly shared
 * Postgres identity. Both have private MPF stores and run the default commit
 * path. Exactly one process can consume the journal-before-submit arm file;
 * that process is SIGKILLed while holding the mutation lease and the other
 * must recover the unsubmitted journal after the lease expires and submit.
 */
export const runJournalKillContention = async ({
  left,
  right,
  armFile,
}: {
  readonly left: JournalKillNodeProcessSpec;
  readonly right: JournalKillNodeProcessSpec;
  readonly armFile: string;
}): Promise<JournalKillContentionResult> => {
  assertSharedPostgresAndPrivateMpfStores(left, right);
  await mkdir(dirname(armFile), { recursive: true });
  await writeFile(armFile, `contention:${left.nodeId}:${right.nodeId}\n`, {
    encoding: "utf8",
    flag: "wx",
  });
  const marker = JOURNAL_KILL_CHECKPOINT_MARKER;
  const start = (spec: JournalKillNodeProcessSpec) => {
    const stopFile = join(dirname(armFile), `${spec.nodeId}.submitted.stop`);
    const summary = superviseHostProcess({
      ...spec.process,
      service: `${spec.process.service}:${spec.nodeId}:contention`,
      env: processEnvForJournalCheckpoint({ spec, armFile }),
      maxRestarts: 0,
      terminateOnOutput: { marker, signal: "SIGKILL" },
      terminateOnFile: { path: stopFile, signal: "SIGTERM" },
    });
    const stopAfterSubmission = Promise.race([
      writeStopFileAfterLogMarker({
        logPath: spec.process.rawLogPath,
        marker: BLOCK_SUBMITTED_MARKER,
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
  if (!markersAppearInOrder(loserLog, JOURNAL_KILL_SURVIVOR_MARKERS)) {
    const missing = JOURNAL_KILL_SURVIVOR_MARKERS.filter(
      (line) => !loserLog.includes(line),
    );
    throw new Error(
      missing.length === 0
        ? "Journal-kill survivor logged lease-busy, unsubmitted-journal recovery and submission out of order"
        : `Journal-kill survivor log is missing: ${missing.join(" | ")}`,
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
