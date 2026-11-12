import { HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, Ref } from "effect";

import {
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
} from "../../database/index.js";
import * as SettlementJournal from "../../database/settlement.js";
import { ContractDeploymentIdentity, Globals } from "../../services/index.js";
import { publicReadinessPresentation } from "../listen-readiness-public-presentation.js";
import { failWith500 } from "../listen-response.js";
import { TX_STATUS_COMMITTED_MEANINGS } from "../tx-status.js";
import { PIPELINE_STATUS_ENDPOINT } from "./routes.js";

type PipelineStatusCountRow = {
  readonly status: string;
  readonly count: bigint | number | string;
};

export const PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES = [
  PendingBlockFinalizationsDB.Status.PendingSubmission,
  PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
  PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
  PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
] as const satisfies readonly PendingBlockFinalizationsDB.Status[];

export type PipelineStatusOldestActiveRow = {
  readonly header_hash: string;
  readonly submitted_tx_hash: string | null;
  readonly status: (typeof PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES)[number];
  readonly created_at: Date;
  readonly updated_at: Date;
  readonly observed_confirmed_at_ms: bigint | number | string | null;
};

type PipelineStatusCountOnlyRow = {
  readonly count: bigint | number | string;
};

const bigintString = (value: bigint | number | string): string =>
  BigInt(value).toString();

/** How many failing settlement jobs `/pipeline-status` names. */
export const PIPELINE_STATUS_FAILING_SETTLEMENT_JOB_LIMIT = 20;

export const encodePipelineStatusSettlement = ({
  unfinishedJobs,
  failingJobs,
}: {
  readonly unfinishedJobs: bigint;
  readonly failingJobs: readonly SettlementJournal.SettlementFailingJob[];
}) => ({
  unfinishedJobs: unfinishedJobs.toString(),
  failingJobs: failingJobs.map((job) => ({
    kind: job.kind,
    eventId: job.event_id,
    phase: job.phase,
    failures: job.failures,
    lastError: job.last_error,
    dueAt: job.due_at.toISOString(),
  })),
});

export const encodePipelineStatusOldestActive = (
  oldestActive: PipelineStatusOldestActiveRow | undefined,
  now: Date,
) =>
  oldestActive === undefined
    ? null
    : {
        headerHash: oldestActive.header_hash,
        submittedTxHash: oldestActive.submitted_tx_hash,
        status: oldestActive.status,
        ageMs: Math.max(0, now.getTime() - oldestActive.created_at.getTime()),
        createdAt: oldestActive.created_at.toISOString(),
        updatedAt: oldestActive.updated_at.toISOString(),
        observedConfirmedAt:
          oldestActive.observed_confirmed_at_ms === null
            ? null
            : new Date(
                Number(oldestActive.observed_confirmed_at_ms),
              ).toISOString(),
      };

export const getPipelineStatusHandler = Effect.gen(function* () {
  const globals = yield* Globals;
  const sql = yield* SqlClient;
  const { manifestId } = yield* ContractDeploymentIdentity;
  const now = new Date();
  const [
    pendingCounts,
    oldestActiveRows,
    durableAdmissionBacklog,
    mempoolTxCountRows,
    processedMempoolTxCountRows,
    unfinishedMutationJobs,
    leaseInspection,
    settlementBacklog,
  ] = yield* Effect.all(
    [
      sql<PipelineStatusCountRow>`SELECT
          status,
          COUNT(*)::bigint AS count
        FROM pending_block_finalizations
        GROUP BY status
        ORDER BY status`,
      sql<PipelineStatusOldestActiveRow>`SELECT
          encode(header_hash, 'hex') AS header_hash,
          encode(submitted_tx_hash, 'hex') AS submitted_tx_hash,
          status,
          created_at,
          updated_at,
          observed_confirmed_at_ms
        FROM pending_block_finalizations
        WHERE ${sql(PendingBlockFinalizationsDB.Columns.STATUS)} IN ${sql.in(
          PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES,
        )}
        ORDER BY created_at ASC
        LIMIT 1`,
      TxAdmissionsDB.countBacklog,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count FROM mempool WHERE included_by IS NULL`,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count FROM processed_mempool WHERE included_by IS NULL`,
      MutationJobsDB.countUnfinished,
      StateQueueMutationLeasesDB.inspect({ recentLimit: 5 }),
      manifestId === undefined
        ? Effect.succeed({ unfinishedJobs: 0n, failingJobs: [] })
        : SettlementJournal.inspectBacklog(
            manifestId,
            PIPELINE_STATUS_FAILING_SETTLEMENT_JOB_LIMIT,
          ),
    ],
    { concurrency: "unbounded" },
  );
  const queueLength = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const unconfirmedSubmittedBlockSinceMs = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const oldestActive = oldestActiveRows[0];
  return yield* HttpServerResponse.json(
    publicReadinessPresentation({
      status: "ok",
      now: now.toISOString(),
      finalityVocabulary: {
        txStatusCommittedMeaning: TX_STATUS_COMMITTED_MEANINGS,
        confirmedDrainMeaning:
          "containing state-queue header merged to confirmed ledger and locally finalized after L1 confirmation",
      },
      durableAdmission: {
        backlog: durableAdmissionBacklog.toString(),
      },
      localResidue: {
        mempoolTxCount: bigintString(mempoolTxCountRows[0]?.count ?? 0),
        processedMempoolTxCount: bigintString(
          processedMempoolTxCountRows[0]?.count ?? 0,
        ),
      },
      pendingBlockFinalizations: {
        countsByStatus: Object.fromEntries(
          pendingCounts.map((row) => [row.status, bigintString(row.count)]),
        ),
        oldestActive: encodePipelineStatusOldestActive(oldestActive, now),
      },
      stateQueue: {
        queueLength,
        unconfirmedSubmittedBlockTxHash:
          unconfirmedSubmittedBlockTxHash === ""
            ? null
            : unconfirmedSubmittedBlockTxHash,
        unconfirmedSubmittedBlockAgeMs:
          unconfirmedSubmittedBlockTxHash === "" ||
          unconfirmedSubmittedBlockSinceMs <= 0
            ? 0
            : Math.max(0, now.getTime() - unconfirmedSubmittedBlockSinceMs),
      },
      stateQueueMutationLease:
        StateQueueMutationLeasesDB.encodeInspectionJson(leaseInspection),
      localMutationJobs: {
        unfinished: unfinishedMutationJobs.toString(),
      },
      settlement: encodePipelineStatusSettlement(settlementBacklog),
    }),
  );
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", PIPELINE_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("SqlError", (e) =>
    failWith500(
      "GET",
      PIPELINE_STATUS_ENDPOINT,
      e.cause,
      "pipeline status query failed",
    ),
  ),
);
