import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, type Exit, Metric, Option } from "effect";

import {
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { Database, Globals } from "../../services/index.js";
import {
  fetchL1ConfirmedState,
  finalizeConfirmedMergeProgram,
  type LandedUnfinalizedMerge,
  MAX_LANDED_MERGE_CATCH_UP,
} from "./merge-to-confirmed-state.finalize-confirmed-merge-program.js";

const decodeJournalHeader = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<SDK.Header, DatabaseError> =>
  Effect.try({
    try: () =>
      Data.from(
        record[PendingBlockFinalizationsDB.Columns.HEADER_CBOR].toString("hex"),
        SDK.Header,
      ) as SDK.Header,
    catch: (cause) =>
      new DatabaseError({
        table: PendingBlockFinalizationsDB.tableName,
        message: "Failed to decode the pending-finalization journal's header",
        cause: `header_hash=${record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex")},error=${formatUnknownError(cause)}`,
      }),
  });

/**
 * The merges L1 has confirmed that this database has not finalized, oldest
 * first. The walk starts at the header L1's confirmed state names and follows
 * each header's authenticated predecessor through the local journals. It stops
 * at genesis, at a header whose confirmed-merge job completed (every earlier
 * one did too, since a merge is built only after this walk from the confirmed
 * state it spends has finalized everything), or at a header this database
 * holds no journal for (another operator's block, which this node never
 * finalized). Only a merge that landed can be the confirmed state's header or
 * its ancestor, so an unlanded merge is never returned.
 */
const landedUnfinalizedMerges = (
  confirmedState: SDK.ConfirmedState,
): Effect.Effect<
  readonly LandedUnfinalizedMerge[],
  DatabaseError | SDK.HashingError,
  Database
> =>
  Effect.gen(function* () {
    const pending: LandedUnfinalizedMerge[] = [];
    let current = confirmedState.headerHash;
    // The confirmed state carries its header's UTxO root; every older header
    // carries its own, authenticated by the header hash.
    let confirmedUtxosRoot: string | undefined = confirmedState.utxoRoot;
    while (current !== SDK.GENESIS_HEADER_HASH) {
      if (pending.length >= MAX_LANDED_MERGE_CATCH_UP) {
        return yield* Effect.fail(
          new DatabaseError({
            table: MutationJobsDB.tableName,
            message:
              "Landed-merge catch-up exceeded its bound without reaching a finalized merge",
            cause: `max_headers=${MAX_LANDED_MERGE_CATCH_UP.toString()},header_hash=${current}`,
          }),
        );
      }
      const headerHash = Buffer.from(current, "hex");
      const job = yield* MutationJobsDB.retrieveByJobId(
        MutationJobsDB.confirmedMergeFinalizationJobId(current),
      );
      if (
        job?.[MutationJobsDB.Columns.STATUS] === MutationJobsDB.Status.Completed
      )
        break;
      const journal =
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
      if (Option.isNone(journal)) break;
      const header = yield* decodeJournalHeader(journal.value);
      const recomputedHeaderHash = yield* SDK.hashBlockHeader(header);
      if (recomputedHeaderHash !== current) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Pending-finalization journal header does not hash to its header hash",
            cause: `header_hash=${current},recomputed=${recomputedHeaderHash}`,
          }),
        );
      }
      if (
        confirmedUtxosRoot !== undefined &&
        header.utxosRoot !== confirmedUtxosRoot
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "L1 confirmed-state UTxO root does not match its header's journal",
            cause: `header_hash=${current},l1_utxo_root=${confirmedUtxosRoot},header_utxos_root=${header.utxosRoot}`,
          }),
        );
      }
      if (
        journal.value[PendingBlockFinalizationsDB.Columns.STATUS] !==
        PendingBlockFinalizationsDB.Status.Finalized
      ) {
        // Its block rows are not local yet; clearing them now would leave the
        // later block finalization to write them back after the merge.
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "A landed merge's block is not locally finalized yet, so its merge cannot be finalized",
            cause: `header_hash=${current},journal_status=${journal.value[PendingBlockFinalizationsDB.Columns.STATUS]}`,
          }),
        );
      }
      pending.push({ headerHash, headerUtxosRoot: header.utxosRoot });
      confirmedUtxosRoot = undefined;
      current = header.prevHeaderHash;
    }
    return pending.reverse();
  });

/**
 * Finalizes every merge up to `confirmedState`'s header that this database has
 * not, oldest first, and returns their header hashes. `confirmedState` must be
 * one authenticated from the L1 state-queue root. Each finalization runs to
 * completion once started.
 */
export const finalizeMergesLandedThrough = (
  confirmedState: SDK.ConfirmedState,
): Effect.Effect<
  readonly string[],
  DatabaseError | SDK.HashingError,
  Database | Globals
> =>
  Effect.gen(function* () {
    const pending = yield* landedUnfinalizedMerges(confirmedState);
    const finalized: string[] = [];
    for (const merge of pending) {
      const headerHashHex = merge.headerHash.toString("hex");
      yield* Effect.logWarning(
        `🔸 Finalizing a merge L1 confirmed without its local finalization (header=${headerHashHex}).`,
      );
      yield* Effect.uninterruptible(finalizeConfirmedMergeProgram(merge)).pipe(
        Effect.tapError((error) =>
          Effect.gen(function* () {
            yield* Metric.increment(mergeLocalFinalizationFailureCounter);
            yield* Effect.logError(
              `🔸 Landed merge local finalization failed; merges stay blocked until it succeeds (header=${headerHashHex},error=${formatUnknownError(error)}).`,
            );
          }),
        ),
      );
      finalized.push(headerHashHex);
    }
    return finalized;
  });

/**
 * Finalizes every merge L1 has confirmed that this database has not (see
 * finalizeMergesLandedThrough). It is the runtime retry of a failed
 * confirmed-merge finalization and the catch-up of a merge that landed after
 * its attempt stopped waiting: a hold timeout, a restart, or a history
 * recovery that revoked the permit before the finalization could write.
 */
export const finalizeLandedMergesProgram = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
): Effect.Effect<
  readonly string[],
  | DatabaseError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.DataCoercionError,
  Database | Globals
> =>
  fetchL1ConfirmedState(lucid, fetchConfig).pipe(
    Effect.flatMap(finalizeMergesLandedThrough),
  );

export const MERGE_CONFIRMATION_PROVIDER_RETRIES = 12;

export const mergeFailureCounter = Metric.counter("merge_failure_count", {
  description: "A counter for tracking merge failures",
  bigint: true,
  incremental: true,
});

export const mergeMissingBlockTxsCounter = Metric.counter(
  "merge_missing_block_txs_count",
  {
    description: "A counter for merge attempts blocked by missing BlocksDB txs",
    bigint: true,
    incremental: true,
  },
);

export const mergeBlockTxDecodeFailureCounter = Metric.counter(
  "merge_block_tx_decode_failure_count",
  {
    description:
      "A counter for merge attempts blocked by malformed block transactions",
    bigint: true,
    incremental: true,
  },
);

export const mergeLocalFinalizationFailureCounter = Metric.counter(
  "merge_local_finalization_failure_count",
  {
    description:
      "A counter for failed local DB finalization after merge submit",
    bigint: true,
    incremental: true,
  },
);

export const mergeDurationTimer = Metric.timer(
  "merge_duration",
  "Duration of one merge attempt in milliseconds",
);

// 30 minutes.
export const MAX_LIFE_OF_LOCAL_SYNC: number = 1_800_000;

export type MergeErrorCode =
  | "E_MERGE_LAYOUT_DERIVATION_FAILED"
  | "E_MERGE_REDEEMER_INDEX_MISMATCH"
  | "E_MERGE_MISSING_BLOCK_TXS"
  | "E_MERGE_BLOCK_TX_DECODE_FAILED"
  | "E_MERGE_UPLC_EVAL_FAILED";

type MissingBlockTxsDiagnosis = {
  readonly reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE";
  readonly txHashesFound: number;
  readonly txsResolved: number;
};

export const diagnoseMissingBlockTxs = (
  txHashesFound: number,
  txsResolved: number,
): MissingBlockTxsDiagnosis | undefined => {
  if (txsResolved !== txHashesFound) {
    return {
      reason: "IMMUTABLE_DB_TX_LOOKUP_INCOMPLETE",
      txHashesFound,
      txsResolved,
    };
  }
  return undefined;
};

export type MergeOptions = {
  readonly bypassQueueLengthGuard?: boolean;
  readonly referenceScriptsAddress?: string;
  readonly leaseToken?: string;
  readonly headerHash?: string;
  readonly submitSlotSnapshot?: () => Effect.Effect<
    SubmitSlotSnapshot,
    unknown
  >;
  /**
   * Checked immediately before the merge transaction is signed and submitted:
   * the caller re-proves every authority the local finalization will need (the
   * state-queue lease and the history producer permit), so a revocation after
   * the merge started refuses the submission instead of stranding a confirmed
   * L1 merge whose local finalization cannot write.
   */
  readonly assertSubmitAuthority?: () => Effect.Effect<
    void,
    DatabaseError,
    Database
  >;
  /**
   * Called with the exit of the local finalization once the merge transaction
   * is confirmed on L1, inside the same uninterruptible region, so it runs
   * even when the caller is interrupted meanwhile. A caller that bounds the
   * attempt with a hold timeout uses it to report a merge that settled while
   * the timeout interrupted it, instead of the timeout.
   */
  readonly onConfirmedFinalization?: (
    outcome: ConfirmedMergeFinalization,
  ) => Effect.Effect<void>;
  /**
   * Unix time (ms) at which the L1 confirmation wait gives up with a
   * confirmation failure, so a caller bounded by a hold keeps room for the
   * local finalization. The merge may still land; the next attempt's
   * finalizeLandedMergesProgram then finalizes it.
   */
  readonly confirmationDeadlineMs?: number;
};

/** A merge confirmed on L1 and the exit of its local finalization. */
export type ConfirmedMergeFinalization = {
  readonly headerHash: string;
  readonly txHash: string;
  readonly exit: Exit.Exit<void, DatabaseError>;
};
