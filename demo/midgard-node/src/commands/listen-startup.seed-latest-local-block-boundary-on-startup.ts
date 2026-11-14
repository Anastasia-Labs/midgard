import "./listen-startup.ensure-protocol-initialized-on-startup.js";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";

import {
  ConfirmedLedgerDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { setUnresolvedBlockSubmission } from "../fibers/unresolved-block-submission.js";
import { foldMerge } from "../landed-blocks/confirmed-merges.js";
import { ownFoldRow } from "../landed-blocks/journal.js";
import { Frontier } from "../landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/index.js";
import { Globals, MidgardContracts } from "../services/index.js";
import { unboundJournalReason } from "../services/journal-header-binding.js";
import {
  landedStateQueueSnapshot,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/landed-state-queue.js";
import { deserializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";

/**
 * The local block boundary startup seeds: the latest of the state-queue
 * tip's end time and the end time of the tip's journal, when this node
 * built the tip.
 */
const seededLocalBlockBoundary = (
  latestHeaderHash: Option.Option<Buffer>,
  latestEndTimeMs: number,
) =>
  Effect.gen(function* () {
    if (Option.isNone(latestHeaderHash)) return latestEndTimeMs;
    const latestJournal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        latestHeaderHash.value,
      );
    if (Option.isNone(latestJournal)) return latestEndTimeMs;
    const seededBoundaryMs = Math.max(
      latestJournal.value[
        PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
      ].getTime(),
      latestEndTimeMs,
    );
    yield* Effect.logInfo(
      `Seeded latest local block boundary from pending-finalization journal for header ${latestHeaderHash.value.toString("hex")}: ${new Date(seededBoundaryMs).toISOString()}`,
    );
    return seededBoundaryMs;
  });

/**
 * Startup's one-time repair of a `confirmed_ledger` that no landed-block run
 * anchored yet (no frontier): the applied (or folded) journal chain from the
 * ledger's root up to `journal` is folded through the stored-delta fold
 * (N5), with the frontier set at the chain's base header first, and the
 * result's root must be `journal`'s. Undefined when the chain does not reach
 * the ledger's root or a journal on it is not its own header's
 * (`unboundJournalReason`); a wrong fold rolls back and fails startup as
 * before.
 * Startup runs before any history writer, so nothing else holds the ledger.
 */
const repairUnanchoredConfirmedLedger = (
  journal: PendingBlockFinalizationsDB.Record,
  confirmedRoot: string,
) =>
  Effect.gen(function* () {
    const Journals = PendingBlockFinalizationsDB;
    const chain = [journal];
    const seen = new Set<string>();
    for (
      let first = journal;
      first[Journals.Columns.BASE_UTXOS_ROOT] !== confirmedRoot;
      first = chain[0]!
    ) {
      const base = first[Journals.Columns.BASE_TAIL_HEADER_HASH];
      if (seen.has(base.toString("hex"))) return undefined;
      seen.add(base.toString("hex"));
      const parent = yield* Journals.retrieveAppliedByHeaderHash(base);
      if (Option.isNone(parent)) return undefined;
      chain.unshift(parent.value);
    }
    // Each fold's base is its journal's: a journal whose header bytes do not
    // bind that base and root is never folded, nor its base anchored.
    for (const record of chain) {
      const unbound = yield* unboundJournalReason(record);
      if (unbound !== undefined) {
        yield* Effect.logWarning(
          `Startup leaves confirmed_ledger unrepaired: ${unbound}`,
        );
        return undefined;
      }
    }
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* Frontier.upsert({
          headerHash:
            chain[0]![Journals.Columns.BASE_TAIL_HEADER_HASH].toString("hex"),
          utxosRoot: confirmedRoot,
        });
        for (const record of chain) yield* foldMerge(ownFoldRow(record), null);
        const root = yield* computeLedgerMpfRootFromLedgerEntries(
          yield* ConfirmedLedgerDB.retrieve,
        );
        const expected = journal[Journals.Columns.EXPECTED_UTXOS_ROOT];
        if (root !== expected)
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Startup's journal-chain fold of confirmed_ledger does not reach the journal's root",
              cause: `folded_root=${root},journal_root=${expected}`,
            }),
          );
        return chain.length;
      }),
    );
  });

/**
 * Seeds the in-memory local block-boundary cache from the current state-queue
 * tip during startup.
 */
export const seedLatestLocalBlockBoundaryOnStartup = Effect.gen(function* () {
  const contracts = yield* MidgardContracts;
  const globals = yield* Globals;

  const snapshot = yield* landedStateQueueSnapshot(
    contracts.stateQueue,
    "startup",
  );
  yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
  const latestBlock = yield* deserializeStateQueueUTxO(
    snapshot.tailCommitBase.utxo,
  );
  const latestEndTimeMs = snapshot.tailCommitBase.blockEndTimeMs;
  yield* Effect.logInfo(
    `Startup state-queue snapshot hydrated: tail=${snapshot.tailCommitBase.outRef},snapshot=${snapshot.snapshotId}`,
  );
  // Once a landed-block run anchored `confirmed_ledger` (a frontier), it
  // holds the ledger to the queue root itself, with a named hold for what it
  // cannot do yet, so a lag here is no startup failure (N5).
  const anchored = (yield* Frontier.retrieve) !== undefined;
  if (anchored && snapshot.blockCount === 0)
    yield* Effect.logInfo(
      "Startup left the confirmed ledger to landed-block processing: its frontier is anchored.",
    );
  if (!anchored && snapshot.blockCount === 0) {
    const confirmedLedgerEntries = yield* ConfirmedLedgerDB.retrieve;
    const confirmedLedgerRoot = yield* computeLedgerMpfRootFromLedgerEntries(
      confirmedLedgerEntries,
    );
    const onChainUtxoRoot = snapshot.tailCommitBase.roots.utxosRoot;
    if (confirmedLedgerRoot === onChainUtxoRoot) {
      yield* Effect.logInfo(
        "Startup verified confirmed ledger against the clean state queue; native owner will establish the durable MPF root before Ready.",
      );
    } else {
      const finalizedJournal =
        snapshot.tailCommitBase.headerHash === null
          ? Option.none<PendingBlockFinalizationsDB.Record>()
          : yield* PendingBlockFinalizationsDB.retrieveAppliedByHeaderHash(
              Buffer.from(snapshot.tailCommitBase.headerHash, "hex"),
            );
      const repaired =
        Option.isSome(finalizedJournal) &&
        finalizedJournal.value[
          PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT
        ] === onChainUtxoRoot
          ? yield* repairUnanchoredConfirmedLedger(
              finalizedJournal.value,
              confirmedLedgerRoot,
            )
          : undefined;
      if (repaired !== undefined) {
        yield* Effect.logInfo(
          `Startup repaired confirmed ledger by folding ${repaired.toString()} authenticated journal(s); native owner must recover the corresponding durable root before Ready.`,
        );
      } else if (confirmedLedgerEntries.length > 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Startup clean-queue confirmed ledger root does not match the on-chain state queue root",
            cause: `confirmed_ledger_entries=${confirmedLedgerEntries.length.toString()},confirmed_ledger_root=${confirmedLedgerRoot},on_chain_utxo_root=${onChainUtxoRoot},snapshot=${snapshot.snapshotId}`,
          }),
        );
      } else {
        yield* Effect.logInfo(
          `Startup skipped clean-queue commit MPF synchronization because confirmed_ledger is empty and its root (${confirmedLedgerRoot}) does not match the on-chain root (${onChainUtxoRoot}).`,
        );
      }
    }
  }
  const latestHeaderHash =
    latestBlock.datum.key === "Empty"
      ? Option.none<Buffer>()
      : Option.some(
          Buffer.from(
            yield* SDK.hashBlockHeader(
              yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum),
            ),
            "hex",
          ),
        );
  const seededBoundaryMs = yield* seededLocalBlockBoundary(
    latestHeaderHash,
    latestEndTimeMs,
  );
  yield* Ref.set(globals.LATEST_LOCAL_BLOCK_END_TIME_MS, seededBoundaryMs);
  yield* Effect.logInfo(
    `Seeded latest local block boundary from startup state: ${new Date(seededBoundaryMs).toISOString()}`,
  );
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Failed to seed latest local block boundary on startup: ${formatUnknownError(e)}`,
    ),
  ),
);

export const hydratePendingBlockFinalizationOnStartup = Effect.gen(
  function* () {
    const globals = yield* Globals;
    yield* PendingBlockFinalizationsDB.assertLiveJournalPayloadsComplete;
    const live = yield* PendingBlockFinalizationsDB.retrieveLive();
    if (Option.isNone(live)) {
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
      return;
    }

    const record = live.value;
    const intendedTxHash =
      record[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH];
    yield* setUnresolvedBlockSubmission(globals, record);
    yield* Effect.logInfo(
      `Hydrated the live block journal on startup for header ${record[
        PendingBlockFinalizationsDB.Columns.HEADER_HASH
      ].toString("hex")} (intended_tx=${
        intendedTxHash == null ? "unsigned" : intendedTxHash.toString("hex")
      }).`,
    );
  },
).pipe(
  Effect.tapError((error) =>
    Effect.logError(
      `Failed to hydrate pending block-finalization journal on startup: ${formatUnknownError(error)}`,
    ),
  ),
  Effect.orDie,
);
