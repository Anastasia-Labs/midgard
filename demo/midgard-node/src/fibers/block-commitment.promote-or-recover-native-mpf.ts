import { Effect, Metric, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { publishMempoolLedgerDelta } from "../services/globals.js";
import { Database, Globals } from "../services/index.js";
import type {
  NativeMpfGenerationHandle,
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "../services/mpf-native-owner/index.js";
import {
  type MempoolLedgerRevertedNotice,
  type SerializedStateQueueUTxO,
  WorkerOutput,
} from "../workers/utils/commit-block-header.js";

/**
 * Background block-commitment loop that packages processed L2 transactions into
 * L1 commitment transactions.
 *
 * The fiber relies on a worker thread for the heavy lifting, then translates
 * the worker's outcome into updates of the node's shared global state and
 * operational metrics.
 */
export const commitBlockNumTxGauge = Metric.gauge("commit_block_num_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the commit block",
  bigint: true,
});

export const totalTxSizeGauge = Metric.gauge("total_tx_size", {
  description:
    "A gauge for tracking the total size of transactions in the commit block",
});

export const commitBlockCounter = Metric.counter("commit_block_count", {
  description: "A counter for tracking the number of committed blocks",
  bigint: true,
  incremental: true,
});

export const commitBlockTxCounter = Metric.counter("commit_block_tx_count", {
  description:
    "A counter for tracking the number of transactions in the commit block",
  bigint: true,
  incremental: true,
});

export const commitBlockTxSizeGauge = Metric.gauge("commit_block_tx_size", {
  description: "A gauge for tracking the size of the commit block transaction",
});

export const commitWorkerDurationTimer = Metric.timer(
  "commit_worker_duration",
  "Duration of one block commitment worker attempt in milliseconds",
);

export const resolveAuthoritativeLocalFinalizationPreflight = ({
  localFinalizationPending,
  availableLocalFinalizationBlock,
  activeJournalHeaderHash,
  activeJournalSubmittedTxHash,
  activeJournalStatus,
  tailHeaderHash,
  tailBlock,
}: {
  readonly localFinalizationPending: boolean;
  readonly availableLocalFinalizationBlock: SerializedStateQueueUTxO | "";
  readonly activeJournalHeaderHash: string | null;
  readonly activeJournalSubmittedTxHash: string | null;
  readonly activeJournalStatus: PendingBlockFinalizationsDB.Status | null;
  readonly tailHeaderHash: string | null;
  readonly tailBlock: SerializedStateQueueUTxO;
}): {
  readonly localFinalizationPending: boolean;
  readonly availableLocalFinalizationBlock: SerializedStateQueueUTxO | "";
  readonly recoveredRacedJournal: boolean;
} => {
  const hasSubmittedActiveJournal =
    (activeJournalSubmittedTxHash !== null ||
      activeJournalStatus ===
        PendingBlockFinalizationsDB.Status.ObservedWaitingStability) &&
    (activeJournalStatus ===
      PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed ||
      activeJournalStatus ===
        PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending ||
      activeJournalStatus ===
        PendingBlockFinalizationsDB.Status.ObservedWaitingStability);
  const journalIsConfirmedTail =
    hasSubmittedActiveJournal &&
    activeJournalHeaderHash !== null &&
    tailHeaderHash === activeJournalHeaderHash;
  if (!hasSubmittedActiveJournal) {
    return {
      localFinalizationPending,
      availableLocalFinalizationBlock,
      recoveredRacedJournal: false,
    };
  }
  return {
    localFinalizationPending: true,
    availableLocalFinalizationBlock: journalIsConfirmedTail ? tailBlock : "",
    recoveredRacedJournal:
      journalIsConfirmedTail &&
      (!localFinalizationPending || availableLocalFinalizationBlock === ""),
  };
};

export const foreignTipReconciliationAwaitingGauge = Metric.gauge(
  "foreign_tip_reconciliation_awaiting",
  {
    description:
      "Number of durable foreign-tip evidence windows still blocking commit reconciliation",
  },
);

export const BLOCK_COMMITMENT_DUE_WORK_KIND =
  "commit_scheduler_refresh" as const;

export const BLOCK_COMMITMENT_DUE_WORK_KEY = "block_commitment";

export const recoverNativeMpfFromSubmittedJournal = async ({
  owner,
  handle,
  submitted,
  replay,
}: {
  readonly owner: NativeMpfOwnerService;
  readonly handle: NativeMpfGenerationHandle;
  readonly submitted: boolean;
  readonly replay?: PersistedNativeMpfReplay;
}): Promise<void> => {
  if (
    !submitted ||
    replay === undefined ||
    replay.baseRoot !== handle.baseRoot
  ) {
    throw new Error(
      `Architecture G restarted-child journal mismatch: handle_base=${handle.baseRoot},journal_base=${replay?.baseRoot ?? "missing"},submitted=${String(submitted)}`,
    );
  }
  await owner.recover(replay);
};

/**
 * Live-process recovery used when a commit worker exits after its submission
 * journal became durable but before it could return the promotion handle. The
 * journal replay is sufficient to reconstruct and atomically promote the
 * candidate; a node restart is not required.
 */
export const recoverNativeMpfAfterCommitWorkerFailure = async ({
  owner,
  submitted,
  replay,
}: {
  readonly owner: NativeMpfOwnerService;
  readonly submitted: boolean;
  readonly replay?: PersistedNativeMpfReplay;
}): Promise<boolean> => {
  if (!submitted) return false;
  if (replay === undefined) {
    throw new Error(
      "Architecture G submitted journal is missing native replay data after commit worker failure",
    );
  }
  await owner.recover(replay);
  return true;
};

export const recoverNativeMpfFromActiveJournalAfterWorkerFailure = (
  owner: NativeMpfOwnerService,
): Effect.Effect<boolean, unknown, Database> =>
  Effect.gen(function* () {
    const active = yield* PendingBlockFinalizationsDB.retrieveActive();
    if (Option.isNone(active)) return false;
    const journal = active.value;
    const replay = journal.nativeMpfReplay;
    const persistedReplay =
      replay === undefined
        ? undefined
        : {
            schema: 1 as const,
            ownerBinarySha256: replay.ownerBinarySha256.toString("hex"),
            baseRoot: replay.baseRoot.toString("hex"),
            candidateRoot: replay.candidateRoot.toString("hex"),
            eventLog: replay.eventLog,
            eventLogDigest: replay.eventLogDigest.toString("hex"),
            eventRoots: replay.eventRoots,
            eventCount: replay.eventCount,
          };
    return yield* Effect.tryPromise(() =>
      recoverNativeMpfAfterCommitWorkerFailure({
        owner,
        submitted:
          journal[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH] !==
            null ||
          journal[PendingBlockFinalizationsDB.Columns.STATUS] ===
            PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
        replay: persistedReplay,
      }),
    );
  });

export const promoteOrRecoverNativeMpf = ({
  owner,
  handle,
}: {
  readonly owner: NativeMpfOwnerService;
  readonly handle: NativeMpfGenerationHandle;
}): Effect.Effect<void, unknown, Database> =>
  Effect.tryPromise(() => owner.promote(handle)).pipe(
    Effect.catchAll((promotionError) =>
      Effect.gen(function* () {
        const diagnostics = yield* Effect.tryPromise(() => owner.diagnostics());
        if (
          Buffer.from(diagnostics.ownerEpoch).equals(
            Buffer.from(handle.ownerEpoch),
          )
        ) {
          return yield* Effect.fail(promotionError);
        }
        const active = yield* PendingBlockFinalizationsDB.retrieveActive();
        if (Option.isNone(active)) {
          return yield* Effect.fail(
            new Error(
              `Architecture G child restarted after submit but no active recovery journal exists: ${String(promotionError)}`,
            ),
          );
        }
        const journal = active.value;
        const replay = journal.nativeMpfReplay;
        const persistedReplay =
          replay === undefined
            ? undefined
            : {
                schema: 1 as const,
                ownerBinarySha256: replay.ownerBinarySha256.toString("hex"),
                baseRoot: replay.baseRoot.toString("hex"),
                candidateRoot: replay.candidateRoot.toString("hex"),
                eventLog: replay.eventLog,
                eventLogDigest: replay.eventLogDigest.toString("hex"),
                eventRoots: replay.eventRoots,
                eventCount: replay.eventCount,
              };
        yield* Effect.tryPromise(() =>
          recoverNativeMpfFromSubmittedJournal({
            owner,
            handle,
            submitted:
              journal[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH] !==
                null ||
              journal[PendingBlockFinalizationsDB.Columns.STATUS] ===
                PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
            replay: persistedReplay,
          }),
        );
        yield* Effect.logWarning(
          `Architecture G recovered post-submit promotion after native child restart base_root=${handle.baseRoot},candidate_root=${persistedReplay?.candidateRoot ?? "missing"}`,
        );
      }),
    ),
  );

export const publishFullMempoolLedgerReload = (
  globals: Globals,
  deltaLogMax: number,
): Effect.Effect<void> =>
  publishMempoolLedgerDelta(
    globals,
    { full: true, upserts: [], deletes: [] },
    deltaLogMax,
  ).pipe(Effect.asVoid);

/** A message the commit worker posts: its output, or a notice ahead of it. */
export type CommitWorkerMessage = WorkerOutput | MempoolLedgerRevertedNotice;

/**
 * Applies a notice the commit worker posts while it still runs and returns
 * undefined; any other message is the worker's output and is returned. A
 * commit-stage rejection rewrites mempool_ledger rows (reverted outputs,
 * restored inputs, rejected descendants) without a delta, so its notice
 * reloads the cache from the durable table.
 */
export const takeCommitWorkerOutput = (
  globals: Globals,
  message: CommitWorkerMessage,
  deltaLogMax: number,
): WorkerOutput | undefined => {
  if (message.type !== "MempoolLedgerRevertedNotice") return message;
  Effect.runSync(publishFullMempoolLedgerReload(globals, deltaLogMax));
  return undefined;
};
