/**
 * This node's block journals as landed-block processing reads them: an own
 * block's delta, events and base come from its journal, never from a replay.
 */
import { Effect, Option } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import type { OwnJournal } from "./ports.js";
import type { LandedBlockRow } from "./store.js";

const Journals = PendingBlockFinalizationsDB;
const Member = Journals.MemberColumns;
const WithdrawalMember = Journals.WithdrawalMemberColumns;

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
  withdrawals: record.withdrawalMembers.map((member) => ({
    id: member[Member.MEMBER_ID].toString("hex"),
    validity: member[WithdrawalMember.VALIDITY],
    detail: member[WithdrawalMember.VALIDITY_DETAIL],
    settlement: member[Member.PAYLOAD_CBOR].toString("hex"),
  })),
  txIds: record.mempoolTxIds,
  revived:
    record[Journals.Columns.STATUS] ===
      Journals.Status.ObservedWaitingStability &&
    record[Journals.Columns.CORRECTION_TRANSITION_DIGEST] != null,
});

/**
 * The landed row of this node's journaled block on the journal's own base,
 * as landed-block processing records it, for a fold at that base.
 */
export const ownFoldRow = (
  record: PendingBlockFinalizationsDB.Record,
): LandedBlockRow => {
  const own = ownJournalOf(record);
  return {
    headerHash: own.headerHash,
    parentHeaderHash: own.baseTailHeaderHash,
    parentUtxosRoot: own.baseUtxosRoot,
    utxosRoot: own.expectedUtxosRoot,
    kind: "own",
    state: "processed",
    applied: true,
    spent: own.spent,
    produced: own.produced,
    depositIds: own.depositIds,
    withdrawals: [],
    forcedIds: own.forcedIds,
    txIds: own.txIds,
  };
};

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
