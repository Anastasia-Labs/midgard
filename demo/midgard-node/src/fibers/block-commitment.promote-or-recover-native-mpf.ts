import { Effect, Metric, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { Database } from "../services/index.js";
import type {
  NativeMpfGenerationHandle,
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "../services/mpf-native-owner/index.js";

export {
  type CommitWorkerMessage,
  publishFullMempoolLedgerReload,
  takeCommitWorkerOutput,
} from "../workers/commit-worker-message.js";

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

export const BLOCK_COMMITMENT_DUE_WORK_KIND =
  "commit_scheduler_refresh" as const;

export const BLOCK_COMMITMENT_DUE_WORK_KEY = "block_commitment";

export const recoverNativeMpfFromSignedJournal = async ({
  owner,
  handle,
  signed,
  replay,
}: {
  readonly owner: NativeMpfOwnerService;
  readonly handle: NativeMpfGenerationHandle;
  readonly signed: boolean;
  readonly replay?: PersistedNativeMpfReplay;
}): Promise<void> => {
  if (!signed || replay === undefined || replay.baseRoot !== handle.baseRoot) {
    throw new Error(
      `Architecture G restarted-child journal mismatch: handle_base=${handle.baseRoot},journal_base=${replay?.baseRoot ?? "missing"},signed=${String(signed)}`,
    );
  }
  await owner.recover(replay);
};

/**
 * Live-process recovery used when a commit worker exits after its journal
 * recorded the signed commit but before it could return the promotion
 * handle. The journal replay is sufficient to reconstruct and atomically promote the
 * candidate; a node restart is not required.
 */
export const recoverNativeMpfAfterCommitWorkerFailure = async ({
  owner,
  signed,
  replay,
}: {
  readonly owner: NativeMpfOwnerService;
  readonly signed: boolean;
  readonly replay?: PersistedNativeMpfReplay;
}): Promise<boolean> => {
  if (!signed) return false;
  if (replay === undefined) {
    throw new Error(
      "Architecture G signed journal is missing native replay data after commit worker failure",
    );
  }
  await owner.recover(replay);
  return true;
};

export const recoverNativeMpfFromLiveJournalAfterWorkerFailure = (
  owner: NativeMpfOwnerService,
): Effect.Effect<boolean, unknown, Database> =>
  Effect.gen(function* () {
    const live = yield* PendingBlockFinalizationsDB.retrieveLive();
    if (Option.isNone(live)) return false;
    const journal = live.value;
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
        signed:
          journal[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] != null,
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
        const live = yield* PendingBlockFinalizationsDB.retrieveLive();
        if (Option.isNone(live)) {
          return yield* Effect.fail(
            new Error(
              `Architecture G child restarted after submit but no live recovery journal exists: ${String(promotionError)}`,
            ),
          );
        }
        const journal = live.value;
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
          recoverNativeMpfFromSignedJournal({
            owner,
            handle,
            signed:
              journal[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH] !=
              null,
            replay: persistedReplay,
          }),
        );
        yield* Effect.logWarning(
          `Architecture G recovered post-submit promotion after native child restart base_root=${handle.baseRoot},candidate_root=${persistedReplay?.candidateRoot ?? "missing"}`,
        );
      }),
    ),
  );
