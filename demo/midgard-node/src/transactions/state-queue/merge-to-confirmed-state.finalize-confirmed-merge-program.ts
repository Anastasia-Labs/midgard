import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Metric, Option, Ref } from "effect";

import {
  BlocksDB,
  ConfirmedLedgerDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../../database/utils/common.js";
import { formatLandedStateQueue } from "../../l1-state-queue/index.js";
import {
  foldMerge,
  retrieveMergeLinks,
} from "../../landed-blocks/confirmed-merges.js";
import { ownFoldRow, ownJournalOf } from "../../landed-blocks/journal.js";
import { Frontier } from "../../landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../../mpf/ledger-hydration.js";
import { withHistoryWrite } from "../../services/event-history-producer.js";
import { Database, Globals } from "../../services/index.js";
import {
  readLandedStateQueue,
  stateQueueContractOf,
} from "../../services/landed-state-queue.js";
import {
  assertNativeMpfHashHex,
  type NativeMpfOwnerService,
} from "../../services/mpf-native-owner/index.js";

export const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merged blocks",
  bigint: true,
  incremental: true,
});

export type ConfirmedMergeNativeOwnerObservation = {
  readonly confirmedLedgerEntryCount?: number;
  readonly confirmedLedgerRoot: string;
  readonly durableLedgerRoot: string;
  readonly activeGenerations: number;
};

/**
 * What a confirmed-merge finalization did to `confirmed_ledger` (N5):
 * - `folded`: it folded the journal's delta at the frontier, through the
 *   stored-delta fold landed-block processing uses (`confirmed-merges.ts`);
 * - `already_folded`: the frontier is at the block, or a retained fold holds
 *   it (landed-block processing folded it first);
 * - `deferred`: the frontier is elsewhere (behind, waiting on a foreign
 *   block, or past it), so landed-block processing folds it in order.
 */
export type ConfirmedMergeLedgerStep = "folded" | "already_folded" | "deferred";

/**
 * Folds this node's own merged block at the frontier, from its journal, in
 * the caller's transaction. With no frontier yet (no landed-block run has
 * set one), it is set once from the journal's identity: at the block when
 * `confirmed_ledger` is at its root already, at its base when it is at the
 * base's. The only whole-ledger read is that one-time bootstrap.
 */
const foldOwnMergeAtFrontier = (journal: PendingBlockFinalizationsDB.Record) =>
  Effect.gen(function* () {
    const own = ownJournalOf(journal);
    const base = {
      headerHash: own.baseTailHeaderHash,
      utxosRoot: own.baseUtxosRoot,
    };
    let frontier = yield* Frontier.retrieve;
    if (frontier === undefined) {
      const root = yield* computeLedgerMpfRootFromLedgerEntries(
        yield* ConfirmedLedgerDB.retrieve,
      );
      if (root === own.expectedUtxosRoot) {
        yield* Frontier.upsert({
          headerHash: own.headerHash,
          utxosRoot: own.expectedUtxosRoot,
        });
        return "already_folded" satisfies ConfirmedMergeLedgerStep;
      }
      if (root !== own.baseUtxosRoot)
        return "deferred" satisfies ConfirmedMergeLedgerStep;
      yield* Frontier.upsert(base);
      frontier = base;
    }
    if (
      frontier.headerHash === own.headerHash ||
      (yield* retrieveMergeLinks).has(own.headerHash)
    )
      return "already_folded" satisfies ConfirmedMergeLedgerStep;
    if (frontier.headerHash !== base.headerHash)
      return "deferred" satisfies ConfirmedMergeLedgerStep;
    // A frontier at the base header with another root is refused here.
    yield* foldMerge(ownFoldRow(journal), null);
    return "folded" satisfies ConfirmedMergeLedgerStep;
  });

/**
 * The confirmed-merge finalization's one transaction: the own block's fold
 * at the frontier (when the frontier is its base), then its block rows
 * cleared. O(delta): no whole-ledger read, root or table lock.
 */
export const finalizeConfirmedMergeTransaction = ({
  headerHash,
  journal,
}: {
  readonly headerHash: Buffer;
  readonly journal: PendingBlockFinalizationsDB.Record;
}): Effect.Effect<ConfirmedMergeLedgerStep, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const step = yield* foldOwnMergeAtFrontier(journal);
        yield* Effect.logInfo(
          `🔸 Confirmed-ledger step of the merge finalization: ${step}; clearing the block from BlocksDB...`,
        );
        yield* BlocksDB.clearBlock(headerHash).pipe(
          Effect.withSpan("clear-block-from-BlocksDB"),
        );
        return step;
      }),
    );
  }).pipe(
    withHistoryWrite,
    Effect.mapError((error) =>
      error instanceof DatabaseError
        ? error
        : new DatabaseError({
            table: "confirmed_merge_finalization",
            message: "Failed to finalize confirmed-state merge locally",
            cause: error,
          }),
    ),
    sqlErrorToDatabaseError(
      "confirmed_merge_finalization",
      "Failed to finalize confirmed-state merge locally",
    ),
  );

/**
 * A confirmed-state merge advances the L1 queue head but does not change the
 * latest committed L2 tail. The node therefore retains the root already
 * promoted by its single native owner and verifies that owner is live instead
 * of reopening the owner's LevelDB path through MidgardMpf.
 */
export const observeNativeOwnerAfterConfirmedMerge = ({
  nativeMpfOwner,
  confirmedLedgerEntryCount,
  confirmedLedgerRoot,
}: {
  readonly nativeMpfOwner:
    | Pick<NativeMpfOwnerService, "diagnostics">
    | undefined;
  readonly confirmedLedgerEntryCount?: number;
  readonly confirmedLedgerRoot: string;
}): Effect.Effect<ConfirmedMergeNativeOwnerObservation, Error> =>
  Effect.tryPromise({
    try: async () => {
      if (nativeMpfOwner === undefined) {
        throw new Error(
          "Architecture G native owner is not initialized during confirmed-state merge finalization",
        );
      }
      const diagnostics = await nativeMpfOwner.diagnostics();
      assertNativeMpfHashHex(
        diagnostics.durableRoot,
        "Architecture G durable root",
      );
      return {
        confirmedLedgerEntryCount,
        confirmedLedgerRoot,
        durableLedgerRoot: diagnostics.durableRoot,
        activeGenerations: diagnostics.activeGenerations,
      };
    },
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Architecture G owner observation failed", { cause }),
  });

/**
 * Finalizes one L1-confirmed merge into the local database under its
 * confirmed-merge job: folds the block's journal delta into
 * `confirmed_ledger` at the frontier (marking its events terminal), clears
 * its block rows, then confirms the native MPF owner is live (see
 * observeNativeOwnerAfterConfirmedMerge). `headerUtxosRoot` is the header's
 * committed UTxO root, which the journal's expected root must be.
 *
 * Idempotent, so a failed or interrupted attempt can simply run again: a
 * block already folded is not folded twice, and clearing converges. A
 * frontier elsewhere leaves the fold to landed-block processing, which folds
 * every merged block in queue order. A failure records the job as failed.
 */
export const finalizeConfirmedMergeProgram = ({
  headerHash,
  headerUtxosRoot,
  parentHeaderHash,
  parentUtxosRoot,
}: LandedUnfinalizedMerge): Effect.Effect<
  void,
  DatabaseError,
  Database | Globals
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const jobId = MutationJobsDB.confirmedMergeFinalizationJobId(
      headerHash.toString("hex"),
    );
    const finalizedJournal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    if (Option.isNone(finalizedJournal)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Failed to finalize confirmed-state merge locally because the pending-finalization journal is missing",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
    const journal = finalizedJournal.value;
    // The fold's base is the journal's: it must be the authenticated
    // header's own parent and roots, or the fold (and a first frontier)
    // would bind the confirmed ledger to a header the chain never had.
    const own = ownJournalOf(journal);
    if (
      own.expectedUtxosRoot !== headerUtxosRoot ||
      own.baseTailHeaderHash !== parentHeaderHash ||
      own.baseUtxosRoot !== parentUtxosRoot
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message: CONFIRMED_MERGE_JOURNAL_UNBOUND,
          cause: `header_hash=${headerHash.toString("hex")},journal_base=${own.baseTailHeaderHash}/${own.baseUtxosRoot},journal_expected_root=${own.expectedUtxosRoot},header_parent=${parentHeaderHash}/${parentUtxosRoot},header_root=${headerUtxosRoot}`,
        }),
      );
    }
    yield* MutationJobsDB.start({
      jobId,
      kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
      payload: {
        headerHash: headerHash.toString("hex"),
        confirmedHeaderUtxosRoot: headerUtxosRoot,
        ledgerDeltaSpentCount: journal.ledgerDelta.spent.length,
        ledgerDeltaProducedCount: journal.ledgerDelta.produced.length,
      },
    });
    const step = yield* finalizeConfirmedMergeTransaction({
      headerHash,
      journal,
    });
    const ownerObservation = yield* observeNativeOwnerAfterConfirmedMerge({
      nativeMpfOwner: yield* Ref.get(globals.NATIVE_MPF_OWNER),
      confirmedLedgerRoot: headerUtxosRoot,
    }).pipe(
      Effect.mapError(
        (error) =>
          new DatabaseError({
            table: "confirmed_merge_finalization",
            message:
              "Failed to observe the native MPF owner after confirmed-state merge",
            cause: formatUnknownError(error),
          }),
      ),
    );
    yield* Effect.logInfo(
      `🔸 Retained Architecture G owner after merge local finalization (header=${headerHash.toString(
        "hex",
      )},confirmed_ledger=${step},confirmed_header_root=${ownerObservation.confirmedLedgerRoot},durable_tail_root=${ownerObservation.durableLedgerRoot},active_generations=${ownerObservation.activeGenerations.toString()}).`,
    );
    yield* MutationJobsDB.markCompleted(jobId);
  }).pipe(
    Effect.tapError((error) =>
      MutationJobsDB.markFailed(
        MutationJobsDB.confirmedMergeFinalizationJobId(
          headerHash.toString("hex"),
        ),
        formatUnknownError(error),
      ).pipe(Effect.catchAll(() => Effect.void)),
    ),
  );

/**
 * A landed merge whose local finalization has to run, with its
 * authenticated header's root and parent.
 */
export type LandedUnfinalizedMerge = {
  readonly headerHash: Buffer;
  readonly headerUtxosRoot: string;
  readonly parentHeaderHash: string;
  readonly parentUtxosRoot: string;
};

/** The landed merge of `header`, whose hash is `headerHash`. */
export const landedMergeOf = (
  headerHash: Buffer,
  header: SDK.Header,
): LandedUnfinalizedMerge => ({
  headerHash,
  headerUtxosRoot: header.utxosRoot,
  parentHeaderHash: header.prevHeaderHash,
  parentUtxosRoot: header.prevUtxosRoot,
});

/** The refusal of a merge's journal that is not its header's own. */
export const CONFIRMED_MERGE_JOURNAL_UNBOUND =
  "Confirmed-merge journal differs from its canonical header/parent/root";

/**
 * Upper bound on the headers one catch-up walks back through. Every merge
 * attempt runs the catch-up first and fails while it fails, so at most the
 * merges that landed while the node was down or recovering are unfinalized.
 */
export const MAX_LANDED_MERGE_CATCH_UP = 1_000;

/** The confirmed state in the landed queue's root (P1, never L1). */
export const fetchLandedConfirmedState = (
  fetchConfig: SDK.StateQueueFetchConfig,
): Effect.Effect<
  SDK.ConfirmedState,
  SDK.StateQueueError | SDK.DataCoercionError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const read = yield* readLandedStateQueue(stateQueueContractOf(fetchConfig));
    const root = read.kind === "ok" ? read.queue.root : null;
    if (root === null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "The landed state queue has no single root",
          cause:
            read.kind === "ok"
              ? formatLandedStateQueue(read.queue)
              : `${read.kind}: ${read.detail}`,
        }),
      );
    }
    return (yield* SDK.getConfirmedStateFromStateQueueDatum(root.element.datum))
      .data;
  });
