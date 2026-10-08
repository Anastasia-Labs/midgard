import { randomUUID } from "node:crypto";

import { commitLeaseOwner } from "@al-ft/midgard-core/commit-lease-owner";
import { Effect, Option } from "effect";

import {
  BlocksDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { type MergeActionResult } from "../fibers/merge.js";
import {
  ContractDeploymentIdentity,
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { runCommitBlockHeaderWorkerProgram } from "../workers/commit-block-header.js";
import {
  serializeStateQueueUTxO,
  type WorkerInput as CommitBlockWorkerInput,
} from "../workers/utils/commit-block-header.js";
import {
  evidence,
  optionRecordEvidence,
  type ReconciliationEvidence,
  type ReconciliationResult,
  type ReconciliationStatus,
  result,
} from "./reconcile.parse-reconciliation-result.js";
import {
  fetchCanonicalStateQueueHeaderHashes,
  fetchCanonicalStateQueueHeaders,
  localFinalizationJobId,
  unfinishedLocalFinalizationJobEvidence,
} from "./reconcile.reconcile-phas-registered-program.js";

export const reconcileLocalFinalizationProgram = ({
  headerHash,
  repair,
}: {
  readonly headerHash: Buffer;
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  Database | Lucid | MidgardContracts | ContractDeploymentIdentity | NodeConfig
> =>
  Effect.gen(function* () {
    const headerHashHex = headerHash.toString("hex");
    const journal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    let canonicalHeaders = yield* fetchCanonicalStateQueueHeaders;
    let canonicalHeader = canonicalHeaders.find(
      (entry) => entry.headerHash === headerHashHex,
    );
    let txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    let unfinishedJobs = yield* MutationJobsDB.retrieveUnfinished;
    const repairActions: string[] = [];
    const evidenceEntries = (): ReconciliationEvidence[] => [
      evidence("canonical_state_queue", {
        containsHeader: canonicalHeader !== undefined,
        headers: canonicalHeaders.map((entry) => entry.headerHash),
        outRef: canonicalHeader?.outRef ?? null,
      }),
      optionRecordEvidence(journal),
      unfinishedLocalFinalizationJobEvidence(headerHashHex, unfinishedJobs),
      evidence("local_block_rows", { txCount: txHashes.length }),
    ];

    const journalStatus = Option.isSome(journal)
      ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
      : null;
    const alreadyFinalized =
      journalStatus === PendingBlockFinalizationsDB.Status.Finalized &&
      txHashes.length > 0 &&
      !unfinishedJobs.some(
        (entry) =>
          entry[MutationJobsDB.Columns.JOB_ID] ===
          localFinalizationJobId(headerHashHex),
      );
    if (alreadyFinalized) {
      return result({
        milestone: "local-finalization",
        target: { headerHash: headerHashHex },
        status: "satisfied",
        evidence: evidenceEntries(),
      });
    }

    if (!repair) {
      const status: ReconciliationStatus =
        canonicalHeader === undefined
          ? "pending"
          : Option.isNone(journal)
            ? "ambiguous"
            : "pending";
      return result({
        milestone: "local-finalization",
        target: { headerHash: headerHashHex },
        status,
        evidence: evidenceEntries(),
        nextAction:
          canonicalHeader === undefined
            ? "Wait for the header to become canonical before local finalization recovery."
            : Option.isNone(journal)
              ? "No durable pending-finalization journal exists for this canonical header."
              : "Run with --repair to replay local finalization from the durable pending-finalization journal.",
      });
    }

    repairActions.push("recover_local_finalization");
    if (canonicalHeader === undefined) {
      return result({
        milestone: "local-finalization",
        target: { headerHash: headerHashHex },
        status: "blocked",
        evidence: evidenceEntries(),
        repairActions,
        nextAction:
          "Cannot recover local finalization until the header is present in canonical state_queue.",
      });
    }
    if (Option.isNone(journal)) {
      return result({
        milestone: "local-finalization",
        target: { headerHash: headerHashHex },
        status: "ambiguous",
        evidence: evidenceEntries(),
        repairActions,
        nextAction:
          "Cannot recover local finalization without a durable pending-finalization journal.",
      });
    }

    const serialized = yield* serializeStateQueueUTxO(canonicalHeader.utxo);
    const workerInput = {
      data: {
        availableConfirmedBlock: "",
        availableLocalFinalizationBlock: serialized,
        currentBlockStartTimeMs: 0,
        ledgerStoreLeaseOwner: commitLeaseOwner(randomUUID()),
        localFinalizationPending: true,
        mempoolTxsCountSoFar: 0,
        sizeOfProcessedTxsSoFar: 0,
      },
    } satisfies CommitBlockWorkerInput;
    const workerOutput = yield* runCommitBlockHeaderWorkerProgram(workerInput);
    canonicalHeaders = yield* fetchCanonicalStateQueueHeaders;
    canonicalHeader = canonicalHeaders.find(
      (entry) => entry.headerHash === headerHashHex,
    );
    txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    unfinishedJobs = yield* MutationJobsDB.retrieveUnfinished;
    const afterJournal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    const recovered =
      workerOutput.type === "SuccessfulLocalFinalizationRecoveryOutput" &&
      Option.isSome(afterJournal) &&
      afterJournal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
        PendingBlockFinalizationsDB.Status.Finalized &&
      txHashes.length > 0 &&
      !unfinishedJobs.some(
        (entry) =>
          entry[MutationJobsDB.Columns.JOB_ID] ===
          localFinalizationJobId(headerHashHex),
      );

    return result({
      milestone: "local-finalization",
      target: { headerHash: headerHashHex },
      status: recovered ? "repaired" : "failed",
      evidence: [
        evidence(
          "worker_output",
          workerOutput as unknown as Record<string, unknown>,
        ),
        evidence("canonical_state_queue", {
          containsHeader: canonicalHeader !== undefined,
          headers: canonicalHeaders.map((entry) => entry.headerHash),
          outRef: canonicalHeader?.outRef ?? null,
        }),
        optionRecordEvidence(afterJournal),
        unfinishedLocalFinalizationJobEvidence(headerHashHex, unfinishedJobs),
        evidence("local_block_rows", { txCount: txHashes.length }),
      ],
      repairActions,
      nextAction: recovered
        ? null
        : "Local finalization repair did not reach finalized state; inspect worker_output and logs.",
    });
  });

const confirmedMergeFinalizationJobEvidence = (
  jobId: string,
  job: MutationJobsDB.Entry | undefined,
): ReconciliationEvidence =>
  evidence(
    "confirmed_merge_finalization_job",
    job === undefined
      ? { present: false, jobId }
      : {
          present: true,
          jobId,
          status: job[MutationJobsDB.Columns.STATUS],
          attempts: job[MutationJobsDB.Columns.ATTEMPTS],
          lastError: job[MutationJobsDB.Columns.LAST_ERROR],
          updatedAt: job[MutationJobsDB.Columns.UPDATED_AT].toISOString(),
        },
  );

export type MergeCompletionObservation = {
  readonly canonicalHeaders: readonly string[];
  readonly canonical: boolean;
  readonly txCount: number;
  readonly job: MutationJobsDB.Entry | undefined;
  readonly journal: Option.Option<PendingBlockFinalizationsDB.Record>;
};

export const observeMergeCompletion = (headerHash: Buffer, jobId: string) =>
  Effect.gen(function* () {
    const canonicalHeaders = yield* fetchCanonicalStateQueueHeaderHashes;
    const txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    return {
      canonicalHeaders,
      canonical: canonicalHeaders.includes(headerHash.toString("hex")),
      txCount: txHashes.length,
      job: yield* MutationJobsDB.retrieveByJobId(jobId),
      journal:
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash),
    } satisfies MergeCompletionObservation;
  });

/**
 * A merge is complete only when the header has left the state queue AND its
 * confirmed-merge local finalization job completed. That job clears the
 * block's rows in the same transaction that folds its ledger delta, so rows
 * that remain after the header left the queue mean the local finalization did
 * not happen; an absent row set alone proves nothing, because a block without
 * L2 transactions never had rows.
 */
export const mergeCompletionVerdict = (
  observed: MergeCompletionObservation,
): {
  readonly status: ReconciliationStatus;
  readonly nextAction: string | null;
} => {
  if (observed.canonical)
    return {
      status: "pending",
      nextAction:
        "HeaderV1 is still queued; run with --repair only after DA/finality gates are satisfied.",
    };
  const jobStatus = observed.job?.[MutationJobsDB.Columns.STATUS];
  if (jobStatus === MutationJobsDB.Status.Completed)
    return observed.txCount === 0
      ? { status: "satisfied", nextAction: null }
      : {
          status: "ambiguous",
          nextAction:
            "The confirmed-merge finalization job completed but block rows for this header exist again; inspect local_block_rows before claiming merge complete.",
        };
  if (observed.job !== undefined)
    return {
      status: "blocked",
      nextAction: `The header left the state queue but its confirmed-merge local finalization is ${String(jobStatus)}; the running node's merge fiber retries it before every merge while the history owner is Ready, and no merge proceeds until it succeeds. If it keeps failing, inspect the job's last_error.`,
    };
  return {
    status: "ambiguous",
    nextAction:
      "The header is not queued and this database holds no confirmed-merge finalization job for it: its merge landed and the running node's merge fiber has not finalized it yet (it does so before its next merge once the history owner is Ready), it was merged before this database existed, or it was removed by state-queue correction; inspect pending_block_finalization.",
  };
};

export const mergeCompletionEvidence = (
  jobId: string,
  observed: MergeCompletionObservation,
): readonly ReconciliationEvidence[] => [
  evidence("canonical_state_queue", {
    containsHeader: observed.canonical,
    headers: observed.canonicalHeaders,
  }),
  evidence("local_block_rows", { txCount: observed.txCount }),
  confirmedMergeFinalizationJobEvidence(jobId, observed.job),
  optionRecordEvidence(observed.journal),
];

/** The merge outcome as plain JSON: the reconciliation artifact rejects the
 * raw state-queue snapshot and absent optional fields. */
export const mergeResultEvidence = (
  mergeResult: MergeActionResult,
): ReconciliationEvidence =>
  evidence(
    "merge_result",
    mergeResult.status === "merged"
      ? {
          status: mergeResult.status,
          trigger: mergeResult.trigger,
          headerHash: mergeResult.headerHash,
          txHash: mergeResult.txHash,
          // The root and the blocks, as the artifact has always counted them.
          postMergeQueueNodeCount: mergeResult.postMergeSnapshot.blockCount + 1,
        }
      : Object.fromEntries(
          Object.entries(mergeResult).filter(
            ([, value]) => value !== undefined,
          ),
        ),
  );
