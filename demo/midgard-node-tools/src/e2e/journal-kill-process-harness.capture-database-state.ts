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
  COMMIT_CRASH_E2E_HARNESS_MODE,
  commitCrashCheckpointMarker,
} from "midgard-node/e2e/commit-crash-checkpoint";
import type { Database } from "midgard-node/services/database";

import {
  type JournalKillDatabaseState,
  type JournalKillNodeProcessSpec,
  normalizeJournalMembers,
  normalizeTxEntries,
} from "./journal-kill-process-harness.database-state.js";
import { type ServiceSupervisorSummary } from "./service-supervisor.js";

/** Read-only database evidence captured after the journal-kill recovery run. */
export const captureJournalKillDatabaseState: Effect.Effect<
  JournalKillDatabaseState,
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

/**
 * The one node checkpoint this harness arms: the default commit path has
 * written the pending-finalization journal and holds the state-queue lease,
 * but has not submitted the L1 transaction yet.
 */
export const JOURNAL_KILL_CHECKPOINT =
  "journal_prepared_before_submit" as const;

export const JOURNAL_KILL_CHECKPOINT_MARKER = commitCrashCheckpointMarker(
  JOURNAL_KILL_CHECKPOINT,
);

export const processEnvForJournalCheckpoint = ({
  spec,
  armFile,
}: {
  readonly spec: JournalKillNodeProcessSpec;
  readonly armFile: string;
}): Readonly<Record<string, string | undefined>> => ({
  ...spec.process.env,
  NODE_ENV: "emulator",
  LEDGER_MPF_DB_PATH: spec.ledgerMpfDbPath,
  TRANSACTIONS_MPF_DB_PATH: spec.transactionsMpfDbPath,
  STATE_QUEUE_MUTATION_LEASE_TTL_MS:
    spec.stateQueueMutationLeaseTtlMs.toString(),
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: Math.max(
    1,
    Math.floor(spec.stateQueueMutationLeaseTtlMs / 3),
  ).toString(),
  MIDGARD_E2E_COMMIT_CRASH_HARNESS: COMMIT_CRASH_E2E_HARNESS_MODE,
  MIDGARD_E2E_COMMIT_CRASH_CHECKPOINT: JOURNAL_KILL_CHECKPOINT,
  MIDGARD_E2E_COMMIT_CRASH_ARM_FILE: armFile,
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
