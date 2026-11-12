import { HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, Ref } from "effect";

import {
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
  readonly status: PendingBlockFinalizationsDB.OwnBlockStatus;
  readonly count: bigint | number | string;
};

/** The oldest journal neither applied, folded nor dead: the node's own-block
 * work still outstanding. */
export type PipelineStatusOldestActiveRow = {
  readonly header_hash: string;
  readonly intended_tx_hash: string | null;
  readonly status: PendingBlockFinalizationsDB.OwnBlockStatus;
  readonly created_at: Date;
  readonly updated_at: Date;
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
        intendedTxHash: oldestActive.intended_tx_hash,
        status: oldestActive.status,
        ageMs: Math.max(0, now.getTime() - oldestActive.created_at.getTime()),
        createdAt: oldestActive.created_at.toISOString(),
        updatedAt: oldestActive.updated_at.toISOString(),
      };

/** The journal counts by derived status, every status named: they sum to
 * the journal rows. */
export const encodePipelineStatusCounts = (
  rows: readonly PipelineStatusCountRow[],
): Record<PendingBlockFinalizationsDB.OwnBlockStatus, string> =>
  Object.fromEntries(
    PendingBlockFinalizationsDB.OWN_BLOCK_STATUSES.map((status) => [
      status,
      bigintString(rows.find((row) => row.status === status)?.count ?? 0),
    ]),
  ) as Record<PendingBlockFinalizationsDB.OwnBlockStatus, string>;

export const getPipelineStatusHandler = Effect.gen(function* () {
  const globals = yield* Globals;
  const sql = yield* SqlClient;
  const { manifestId } = yield* ContractDeploymentIdentity;
  const now = new Date();
  const dead = PendingBlockFinalizationsDB.deadHeadersOf(
    yield* PendingBlockFinalizationsDB.deadJournals,
  );
  const status = PendingBlockFinalizationsDB.ownBlockStatusSql(sql, "p");
  const notDead = sql`NOT ${PendingBlockFinalizationsDB.deadHeader(
    sql,
    "p.header_hash",
    dead,
  )}`;
  const [
    pendingCounts,
    liveRows,
    oldestActiveRows,
    durableAdmissionBacklog,
    mempoolTxCountRows,
    processedMempoolTxCountRows,
    leaseInspection,
    settlementBacklog,
  ] = yield* Effect.all(
    [
      sql<PipelineStatusCountRow>`SELECT status, COUNT(*)::bigint AS count
        FROM (SELECT ${status} AS status
          FROM pending_block_finalizations p) derived
        GROUP BY status
        ORDER BY status`,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count
        FROM pending_block_finalizations p
        WHERE ${status} = 'unlanded' AND ${notDead}`,
      sql<PipelineStatusOldestActiveRow>`SELECT
          encode(p.header_hash, 'hex') AS header_hash,
          encode(p.intended_tx_hash, 'hex') AS intended_tx_hash,
          ${status} AS status,
          p.created_at,
          p.updated_at
        FROM pending_block_finalizations p
        WHERE ${PendingBlockFinalizationsDB.notAppliedOrFolded(sql, "p")}
          AND ${notDead}
        ORDER BY p.created_at ASC
        LIMIT 1`,
      TxAdmissionsDB.countBacklog,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count FROM mempool WHERE included_by IS NULL`,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count FROM processed_mempool WHERE included_by IS NULL`,
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
        countsByStatus: encodePipelineStatusCounts(pendingCounts),
        live: bigintString(liveRows[0]?.count ?? 0),
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
