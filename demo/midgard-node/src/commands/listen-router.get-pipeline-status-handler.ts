import "./listen-router.get-readiness-handler.js";

import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, Ref } from "effect";

import {
  BlocksDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
} from "../database/index.js";
import * as SettlementJournal from "../database/settlement.js";
import { blockCommitmentAction } from "../fibers/index.js";
import * as Genesis from "../genesis.js";
import { formatLandedStateQueue } from "../l1-state-queue/index.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { readLandedStateQueue } from "../services/landed-state-queue.js";
import * as Initialization from "../transactions/initialization.js";
import { failWith500 } from "./listen-response.js";
import { parseFixedHexParam } from "./listen-router.get-tx-handler.js";
import {
  BLOCK_ENDPOINT,
  COMMIT_ENDPOINT,
  INIT_ENDPOINT,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import {
  PIPELINE_STATUS_ENDPOINT,
  PROTOCOL_INFO_ENDPOINT,
} from "./listen-router.run-exact-gated-direct-l1-provider-probe.js";
import * as ProtocolInfoCommand from "./protocol-info.js";

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
      SettlementJournal.inspectBacklog(
        PIPELINE_STATUS_FAILING_SETTLEMENT_JOB_LIMIT,
      ),
    ],
    { concurrency: "unbounded" },
  );
  const queueLength = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const localFinalizationPending = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const unconfirmedSubmittedBlockSinceMs = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const oldestActive = oldestActiveRows[0];
  return yield* HttpServerResponse.json({
    status: "ok",
    now: now.toISOString(),
    finalityVocabulary: {
      txStatusCommittedMeaning:
        "immutable_db_inclusion_not_confirmed_ledger_merge",
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
      localFinalizationPending,
    },
    stateQueueMutationLease:
      StateQueueMutationLeasesDB.encodeInspectionJson(leaseInspection),
    localMutationJobs: {
      unfinished: unfinishedMutationJobs.toString(),
    },
    settlement: encodePipelineStatusSettlement(settlementBacklog),
  });
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

/**
 * `GET /protocol-info`: returns stable public facts needed by external
 * Midgard transaction builders.
 */
export const getProtocolInfoHandler = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucid = yield* Lucid;
  const deploymentIdentity = yield* ContractDeploymentIdentity;
  const response = yield* Effect.try({
    try: () =>
      ProtocolInfoCommand.encodeProtocolInfo({
        nodeConfig,
        currentSlot: lucid.api.currentSlot(),
        deploymentMarker: deploymentIdentity.deploymentMarker,
        consensusProfile: deploymentIdentity.consensusProfile,
      }),
    catch: (error) => error,
  });
  return yield* HttpServerResponse.json(response);
}).pipe(Effect.catchAll((e) => failWith500("GET", PROTOCOL_INFO_ENDPOINT, e)));

/**
 * `GET /block`: returns tx hashes referenced by a committed block header.
 */
export const getBlockHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const hdrHash = params["header_hash"];
  yield* Effect.logInfo(
    `GET /block - Request received for header_hash: ${String(hdrHash)}`,
  );

  const headerHash = parseFixedHexParam(hdrHash, 28);
  if (headerHash === null) {
    yield* Effect.logInfo(
      `GET /${BLOCK_ENDPOINT} - Invalid block hash: ${String(hdrHash)}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid block hash: ${String(hdrHash)}` },
      { status: 400 },
    );
  }
  const hashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
  yield* Effect.logInfo(
    `GET /${BLOCK_ENDPOINT} - Found ${hashes.length} txs for block: ${String(hdrHash)}`,
  );
  return yield* HttpServerResponse.json({
    hashes: hashes.map(SDK.bufferToHex),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", BLOCK_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      BLOCK_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /init`: initializes protocol state when startup policy and the landed
 * state queue (P1) allow it.
 */
export const getInitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✨ Initialization request received`);
  const contracts = yield* MidgardContracts;
  const read = yield* readLandedStateQueue(contracts.stateQueue);
  if (read.kind !== "ok") {
    return yield* HttpServerResponse.json(
      {
        error: "Cannot initialize: the landed state queue is unavailable",
        reason: read.kind,
        details: read.detail,
      },
      { status: 503 },
    );
  }
  const landed = read.queue;
  if (landed.policyOutputCount > 0) {
    const details = formatLandedStateQueue(landed);
    if (!landed.healthy) {
      yield* Effect.logWarning(
        `GET /${INIT_ENDPOINT} - Refusing to initialize over an unhealthy state queue (${details})`,
      );
      return yield* HttpServerResponse.json(
        {
          error:
            "Cannot initialize: configured state_queue policy already has an unhealthy landed queue",
          details,
          reason: landed.reason ?? "unknown",
        },
        { status: 409 },
      );
    }
    yield* Effect.logInfo(
      `GET /${INIT_ENDPOINT} - Skipping initialization (already initialized): ${details}`,
    );
    return yield* HttpServerResponse.json({
      message: "State queue already initialized",
      details,
    });
  }

  const txHash = yield* Initialization.program;
  yield* Genesis.program;
  yield* Effect.logInfo(
    `GET /${INIT_ENDPOINT} - Initialization successful: ${txHash}`,
  );
  return yield* HttpServerResponse.json({
    message: `Initiation successful: ${txHash}`,
  });
}).pipe(Effect.catchAll((e) => failWith500("GET", INIT_ENDPOINT, e)));

/**
 * `GET /commit`: triggers manual block commitment.
 */
export const getCommitEndpoint = Effect.gen(function* () {
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Manual block commitment order received`,
  );
  yield* blockCommitmentAction;
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Block commitment successful`,
  );
  return yield* HttpServerResponse.json({
    message: "Block commitment successful",
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", COMMIT_ENDPOINT, e),
  ),
  Effect.catchTag("WorkerError", (e) =>
    failWith500("GET", COMMIT_ENDPOINT, e.cause, "failed worker"),
  ),
);
