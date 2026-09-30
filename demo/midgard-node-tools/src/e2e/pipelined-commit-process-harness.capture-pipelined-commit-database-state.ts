import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import {
  DepositsDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxUtils,
} from "midgard-node/database/index";
import {
  PIPELINED_COMMIT_E2E_HARNESS_MODE,
  type PipelinedCommitCrashCheckpoint,
  pipelinedCommitCrashCheckpointMarker,
} from "midgard-node/e2e/pipelined-commit-crash-checkpoint";
import type { Database } from "midgard-node/services/database";

import {
  normalizeJournalMembers,
  normalizeTxEntries,
  type PipelinedCommitDatabaseState,
  type PipelinedCommitNodeProcessSpec,
} from "./pipelined-commit-process-harness.pipelined-commit-database-state.js";
import {
  type ServiceSupervisorSummary,
  superviseHostProcess,
} from "./service-supervisor.js";

/** Read-only evidence used by the real-process crash and contention gates. */
export const capturePipelinedCommitDatabaseState: Effect.Effect<
  PipelinedCommitDatabaseState,
  unknown,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const activeStatuses = [
    PendingBlockFinalizationsDB.Status.PendingSubmission,
    PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
    PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
    PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
  ];
  const [journal, leaseInspection, deposits, mempool, processed, activeCount] =
    yield* Effect.all([
      PendingBlockFinalizationsDB.retrieveActive(),
      StateQueueMutationLeasesDB.inspect(),
      DepositsDB.retrieveAllEntries(),
      TxUtils.retrieveAllEntries(MempoolDB.tableName),
      ProcessedMempoolDB.retrieve,
      sql<{ readonly count: string }>`SELECT COUNT(*)::text AS count
        FROM ${sql(PendingBlockFinalizationsDB.tableName)}
        WHERE ${sql(PendingBlockFinalizationsDB.Columns.STATUS)} IN ${sql.in(activeStatuses)}`,
    ]);
  const activeJournal = Option.match(journal, {
    onNone: () => null,
    onSome: (record) => ({
      headerHash:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
      headerCbor:
        record[PendingBlockFinalizationsDB.Columns.HEADER_CBOR].toString("hex"),
      journalPayloadIdentity: {
        deposits: normalizeJournalMembers(record.depositMembers),
        forcedTransactions: normalizeJournalMembers(
          record.forcedTransactionMembers,
        ),
        withdrawals: normalizeJournalMembers(record.withdrawalMembers),
        transactions: normalizeJournalMembers(record.txMembers),
        transitionTrace: normalizeJournalMembers(record.transitionTraceMembers),
        eventToStep: normalizeJournalMembers(record.eventToStepMembers),
        ledgerDelta: {
          spent: record.ledgerDelta.spent
            .map((outref) => outref.toString("hex"))
            .sort(),
          produced: record.ledgerDelta.produced
            .map((member) => ({
              outref:
                member[PendingBlockFinalizationsDB.UtxoColumns.OUTREF].toString(
                  "hex",
                ),
              output:
                member[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT].toString(
                  "hex",
                ),
            }))
            .sort((left, right) =>
              JSON.stringify(left).localeCompare(JSON.stringify(right)),
            ),
        },
      },
      submittedTxHash:
        record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]?.toString(
          "hex",
        ) ?? null,
      status: record[PendingBlockFinalizationsDB.Columns.STATUS],
      baseTailHeaderHash:
        record[
          PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH
        ]?.toString("hex") ?? null,
      baseTailOutRef:
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_OUT_REF] ?? null,
      baseTailDatumCbor:
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_DATUM_CBOR] ??
        null,
      baseRoots: {
        utxos: record[PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT],
        forcedTransactions:
          record[
            PendingBlockFinalizationsDB.Columns.BASE_FORCED_TRANSACTIONS_ROOT
          ],
        transactions:
          record[PendingBlockFinalizationsDB.Columns.BASE_TRANSACTIONS_ROOT],
        deposits:
          record[PendingBlockFinalizationsDB.Columns.BASE_DEPOSITS_ROOT],
        withdrawals:
          record[PendingBlockFinalizationsDB.Columns.BASE_WITHDRAWALS_ROOT],
      },
      expectedRoots: {
        utxos: record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
        forcedTransactions:
          record[
            PendingBlockFinalizationsDB.Columns
              .EXPECTED_FORCED_TRANSACTIONS_ROOT
          ],
        transactions:
          record[
            PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT
          ],
        deposits:
          record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT],
        withdrawals:
          record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT],
        transitionTrace:
          record[
            PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_TRACE_ROOT
          ],
        eventToStep:
          record[
            PendingBlockFinalizationsDB.Columns.EXPECTED_EVENT_TO_STEP_ROOT
          ],
      },
      mpfReplay: {
        baseRoot:
          record[
            PendingBlockFinalizationsDB.Columns.MPF_REPLAY_BASE_ROOT
          ]?.toString("hex") ?? null,
        candidateRoot:
          record[
            PendingBlockFinalizationsDB.Columns.MPF_REPLAY_CANDIDATE_ROOT
          ]?.toString("hex") ?? null,
        eventLogDigest:
          record[
            PendingBlockFinalizationsDB.Columns.MPF_REPLAY_EVENT_LOG_DIGEST
          ]?.toString("hex") ?? null,
        eventRoots:
          record[
            PendingBlockFinalizationsDB.Columns.MPF_REPLAY_EVENT_ROOTS
          ]?.toString("hex") ?? null,
        eventCount:
          record[PendingBlockFinalizationsDB.Columns.MPF_REPLAY_EVENT_COUNT] ??
          null,
      },
      leaseToken:
        record[PendingBlockFinalizationsDB.Columns.STATE_QUEUE_LEASE_TOKEN],
      depositCount: record.depositEventIds.length,
      mempoolTxCount: record.mempoolTxIds.length,
    }),
  });
  const activeLease =
    leaseInspection.activeLease === undefined
      ? null
      : {
          holder:
            leaseInspection.activeLease[
              StateQueueMutationLeasesDB.Columns.HOLDER
            ],
          token:
            leaseInspection.activeLease[
              StateQueueMutationLeasesDB.Columns.TOKEN
            ],
          status:
            leaseInspection.activeLease[
              StateQueueMutationLeasesDB.Columns.STATUS
            ],
        };
  return {
    activeJournalCount: Number(activeCount[0]?.count ?? "0"),
    activeJournal,
    activeLease,
    recentLeases: leaseInspection.recentLeases.map((lease) => ({
      holder: lease[StateQueueMutationLeasesDB.Columns.HOLDER],
      status: lease[StateQueueMutationLeasesDB.Columns.STATUS],
      lastError: lease[StateQueueMutationLeasesDB.Columns.LAST_ERROR],
    })),
    deposits: deposits.map((entry) => ({
      id: entry[DepositsDB.Columns.ID].toString("hex"),
      status: entry[DepositsDB.Columns.STATUS],
      projectedHeaderHash:
        entry[DepositsDB.Columns.PROJECTED_HEADER_HASH]?.toString("hex") ??
        null,
    })),
    mempool: normalizeTxEntries(mempool),
    processed: normalizeTxEntries(processed),
  };
});

export const processEnvForCheckpoint = ({
  spec,
  checkpoint,
  armFile,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly armFile: string;
}): Readonly<Record<string, string | undefined>> => ({
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
  MIDGARD_E2E_PIPELINED_COMMIT_HARNESS: PIPELINED_COMMIT_E2E_HARNESS_MODE,
  MIDGARD_E2E_PIPELINED_COMMIT_CRASH_CHECKPOINT: checkpoint,
  MIDGARD_E2E_PIPELINED_COMMIT_CRASH_ARM_FILE: armFile,
});

export const assertCheckpointTermination = ({
  summary,
  marker,
}: {
  readonly summary: ServiceSupervisorSummary;
  readonly marker: string;
}): void => {
  const checkpointAttempts = summary.attempts.filter(
    (attempt) => attempt.outputTermination?.marker === marker,
  );
  if (checkpointAttempts.length !== 1) {
    throw new Error(
      `Expected exactly one supervised checkpoint termination for ${marker}; observed ${checkpointAttempts.length.toString()}`,
    );
  }
  if (checkpointAttempts[0]?.signal !== "SIGKILL") {
    throw new Error(
      `Expected checkpoint process to exit via SIGKILL; observed ${checkpointAttempts[0]?.signal ?? "none"}`,
    );
  }
};

/**
 * Starts the actual node command, waits for a production-path checkpoint, and
 * externally SIGKILLs the detached process group. The one-shot arm file is
 * consumed by the child, so the same process spec can subsequently restart.
 */
export const runPipelinedCommitCheckpointCrash = async ({
  spec,
  checkpoint,
  armFile,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly armFile: string;
}): Promise<ServiceSupervisorSummary> => {
  await mkdir(dirname(armFile), { recursive: true });
  await writeFile(armFile, `${spec.nodeId}:${checkpoint}\n`, {
    encoding: "utf8",
    flag: "wx",
  });
  const marker = pipelinedCommitCrashCheckpointMarker(checkpoint);
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:${checkpoint}`,
    env: processEnvForCheckpoint({ spec, checkpoint, armFile }),
    maxRestarts: 0,
    terminateOnOutput: { marker, signal: "SIGKILL" },
  });
  assertCheckpointTermination({ summary, marker });
  return summary;
};

export const assertMarkerTermination = ({
  summary,
  marker,
  signal,
}: {
  readonly summary: ServiceSupervisorSummary;
  readonly marker: string;
  readonly signal: NodeJS.Signals;
}): void => {
  const observation = summary.attempts[0]?.outputTermination;
  if (observation?.marker !== marker || observation.signal !== signal) {
    throw new Error(
      `Expected supervised ${signal} at marker ${marker}; observed ${observation?.signal ?? "none"} at ${observation?.marker ?? "none"}`,
    );
  }
};

/** Restarts the same node state and stops only after a newly built candidate. */
export const restartPipelinedCommitNodeUntilFreshCandidate = async ({
  spec,
  checkpoint,
  consumedArmFile,
}: {
  readonly spec: PipelinedCommitNodeProcessSpec;
  readonly checkpoint: PipelinedCommitCrashCheckpoint;
  readonly consumedArmFile: string;
}): Promise<ServiceSupervisorSummary> => {
  const marker = "pipeline_trace phase=candidate_ready";
  const summary = await superviseHostProcess({
    ...spec.process,
    service: `${spec.process.service}:${spec.nodeId}:restart`,
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
