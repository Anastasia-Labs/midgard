import { readFile, writeFile } from "node:fs/promises";

import { type PipelinedCommitCrashCheckpoint } from "midgard-node/e2e/pipelined-commit-crash-checkpoint";

import {
  assertMarkerTermination,
  processEnvForCheckpoint,
} from "./pipelined-commit-process-harness.capture-pipelined-commit-database-state.js";
import { type PipelinedCommitNodeProcessSpec } from "./pipelined-commit-process-harness.pipelined-commit-database-state.js";
import {
  type ServiceSupervisorSummary,
  superviseHostProcess,
} from "./service-supervisor.js";

/** Restarts after the ready-candidate proof and waits for the real submission. */
export const restartPipelinedCommitNodeUntilSubmission = async ({
  spec,
  checkpoint,
  consumedArmFile,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly consumedArmFile: string;
}): Promise<ServiceSupervisorSummary> => {
  const marker = "pipeline_trace phase=candidate_submitted";
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:restart-submit`,
    env: processEnvForCheckpoint({
      spec,
      checkpoint,
      armFile: consumedArmFile,
    }),
    maxRestarts: 0,
    terminateOnOutput: { marker, signal: "SIGTERM" },
  });
  assertMarkerTermination({ summary, marker, signal: "SIGTERM" });
  return summary;
};

/** Runs the non-speculative control node to a caller-selected stable marker. */
export const runPipelinedCommitFlagOffControl = async ({
  spec,
  stopMarker,
  stopOccurrence = 1,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly stopMarker: string;
  readonly stopOccurrence?: number;
}): Promise<ServiceSupervisorSummary> => {
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:flag-off-control`,
    env: {
      ...spec.process.env,
      NODE_ENV: "emulator",
      SPECULATIVE_COMMIT_BUILD: "false",
      LEDGER_MPF_DB_PATH: spec.ledgerMpfDbPath,
      TRANSACTIONS_MPF_DB_PATH: spec.transactionsMpfDbPath,
      STATE_QUEUE_MUTATION_LEASE_TTL_MS:
        spec.stateQueueMutationLeaseTtlMs.toString(),
      STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: Math.max(
        1,
        Math.floor(spec.stateQueueMutationLeaseTtlMs / 3),
      ).toString(),
    },
    maxRestarts: 0,
    terminateOnOutput: {
      marker: stopMarker,
      occurrence: stopOccurrence,
      signal: "SIGTERM",
    },
  });
  assertMarkerTermination({
    summary,
    marker: stopMarker,
    signal: "SIGTERM",
  });
  return summary;
};

export type PipelinedCommitLeaseContentionResult = {
  readonly winnerNodeId: string;
  readonly loserNodeId: string;
  readonly winner: ServiceSupervisorSummary;
  readonly loser: ServiceSupervisorSummary;
  readonly winnerLog: string;
  readonly loserLog: string;
};

export const assertSharedPostgresAndPrivateMpfStores = (
  left: PipelinedCommitNodeProcessSpec,
  right: PipelinedCommitNodeProcessSpec,
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

const processEnvForSpeculation = (
  spec: PipelinedCommitNodeProcessSpec,
): Readonly<Record<string, string | undefined>> => ({
  ...spec.process.env,
  NODE_ENV: "emulator",
  SPECULATIVE_COMMIT_BUILD: "true",
  LEDGER_MPF_DB_PATH: spec.ledgerMpfDbPath,
  TRANSACTIONS_MPF_DB_PATH: spec.transactionsMpfDbPath,
  STATE_QUEUE_MUTATION_LEASE_TTL_MS:
    spec.stateQueueMutationLeaseTtlMs.toString(),
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: Math.max(
    1,
    Math.floor(spec.stateQueueMutationLeaseTtlMs / 3),
  ).toString(),
});

/** Runs one real speculative node until a marker is observed and terminated. */
export const runPipelinedCommitNodeUntilMarker = async ({
  spec,
  marker,
  signal = "SIGTERM",
  suffix = "marker",
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly marker: string;
  readonly signal?: NodeJS.Signals;
  readonly suffix?: string;
}): Promise<ServiceSupervisorSummary> => {
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:${suffix}`,
    env: processEnvForSpeculation(spec),
    maxRestarts: 0,
    terminateOnOutput: { marker, signal },
  });
  assertMarkerTermination({ summary, marker, signal });
  return summary;
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
 * Normal two-node contention gate: one process submits and the other either
 * records a DB-lease Busy deferral or loses the journal race at the database's
 * exact single-active-journal guard before invalidating its candidate.
 */
export const runPipelinedCommitNormalLeaseContention = async ({
  left,
  right,
}: {
  readonly left: PipelinedCommitNodeProcessSpec;
  readonly right: PipelinedCommitNodeProcessSpec;
}): Promise<PipelinedCommitLeaseContentionResult> => {
  assertSharedPostgresAndPrivateMpfStores(left, right);
  const submittedMarker = "pipeline_trace phase=candidate_submitted";
  const invalidatedMarkers = [
    "pipeline_trace phase=candidate_invalidated reason=T2",
    "pipeline_trace phase=candidate_invalidated reason=T7",
  ] as const;
  const run = (spec: PipelinedCommitNodeProcessSpec) =>
    superviseHostProcess({
      ...spec.process,
      service: `${spec.process.service}:${spec.nodeId}:normal-contention`,
      env: processEnvForSpeculation(spec),
      maxRestarts: 0,
      terminateOnOutput: {
        marker: submittedMarker,
        additionalMarkers: invalidatedMarkers,
        signal: "SIGTERM",
      },
    });
  const [leftSummary, rightSummary] = await Promise.all([
    run(left),
    run(right),
  ]);
  const entries = [
    { spec: left, summary: leftSummary },
    { spec: right, summary: rightSummary },
  ];
  const submitted = entries.filter(
    ({ summary }) =>
      summary.attempts[0]?.outputTermination?.marker === submittedMarker,
  );
  const invalidated = entries.filter(({ summary }) =>
    invalidatedMarkers.some(
      (marker) => summary.attempts[0]?.outputTermination?.marker === marker,
    ),
  );
  if (submitted.length !== 1 || invalidated.length !== 1) {
    throw new Error(
      `Expected one submitted winner and one invalidated loser; observed submitted=${submitted.length.toString()},invalidated=${invalidated.length.toString()}`,
    );
  }
  const winner = submitted[0]!;
  const loser = invalidated[0]!;
  const [winnerLog, loserLog] = await Promise.all([
    readFile(winner.summary.rawLogPath, "utf8"),
    readFile(loser.summary.rawLogPath, "utf8"),
  ]);
  const recordedLeaseBusy = loserLog.includes("reason=state_queue_lease_busy");
  const recordedActiveJournalRefusal = loserLog.includes(
    "Refusing to prepare a new pending block while another active pending-finalization record exists",
  );
  if (!recordedLeaseBusy && !recordedActiveJournalRefusal) {
    throw new Error(
      "Contention loser recorded neither a state-queue lease Busy deferral nor the single-active-journal refusal",
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
