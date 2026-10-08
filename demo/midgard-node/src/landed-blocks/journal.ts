/**
 * This node's block journals as landed-block processing reads them: an own
 * block's delta, events and base come from its journal, never from a replay.
 */
import { Effect, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import type { OwnJournal } from "./ports.js";

const Journals = PendingBlockFinalizationsDB;

const statusOf = (
  status: PendingBlockFinalizationsDB.Status,
): OwnJournal["status"] =>
  status === Journals.Status.LocallyApplied
    ? "locally_applied"
    : status === Journals.Status.Abandoned
      ? "abandoned"
      : "active";

export const ownJournalOf = (
  record: PendingBlockFinalizationsDB.Record,
): OwnJournal & { headerHash: string } => ({
  headerHash: record[Journals.Columns.HEADER_HASH].toString("hex"),
  status: statusOf(record[Journals.Columns.STATUS]),
  baseTailHeaderHash:
    record[Journals.Columns.BASE_TAIL_HEADER_HASH].toString("hex"),
  baseUtxosRoot: record[Journals.Columns.BASE_UTXOS_ROOT],
  expectedUtxosRoot: record[Journals.Columns.EXPECTED_UTXOS_ROOT],
  spent: record.ledgerDelta.spent,
  produced: record.ledgerDelta.produced,
  depositIds: record.depositEventIds,
  forcedIds: record.forcedTransactionEventIds,
  txIds: record.mempoolTxIds,
});

/** This node's journal of `headerHash`, if it committed that block. */
export const ownJournal = (headerHash: string) =>
  Journals.retrieveByHeaderHash(Buffer.from(headerHash, "hex")).pipe(
    Effect.map((record) =>
      Option.isNone(record) ? undefined : ownJournalOf(record.value),
    ),
  );

/** The node's one active journal, if any. */
export const activeJournal = Journals.retrieveActive().pipe(
  Effect.map((record) =>
    Option.isNone(record) ? undefined : ownJournalOf(record.value),
  ),
);
