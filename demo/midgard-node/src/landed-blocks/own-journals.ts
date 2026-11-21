/**
 * This node's dead block journals under whichever-lands-wins (plan §8.3,
 * I3): a journal whose block can no longer land (`deadJournals`) is
 * disposed of by the working-ledger rebase. Nothing is written to the
 * journal: its status is derived, and a dead block that lands after all is
 * landed, and taken in, like any other.
 *
 * Disposal undoes what the journal's block left in the pending tables and
 * its commit attempt held. Each effect is read again from the current rows
 * and is idempotent, so a journal is disposed of while one is pending and
 * never again once none is:
 *
 * - (a) its withdrawals, still classified for it (projected to no header at
 *   the classification it recorded) and in no journal that is not dead,
 *   lose their classification;
 * - (b) the inclusion marks its header holds are cleared;
 * - (c) its block's `blocks` rows go, with the `immutable` rows of its
 *   transactions no live block holds;
 * - (d) the state-queue mutation lease it was prepared under is released
 *   while active.
 *
 * With them, its transactions that are in no pending table, `immutable`,
 * `tx_rejections` or the foreign inclusions are restored to the table they
 * were journaled from. That is not an effect of its own: a rejection is
 * pruned in time, and a transaction rejected for good is never restored
 * once the journal's other effects are done.
 *
 * The rebase that runs it recomputes the event statuses, the working ledger
 * and the native MPF on the landed blocks; the commit path then builds a
 * replacement on the landed tip from what is selectable.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Option } from "effect";

import {
  BlocksDB,
  ImmutableDB,
  MempoolDB,
  MempoolInclusionsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import type { DriverHold } from "../l1-events/driver.js";
import { LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING } from "./holds.js";

const Journals = PendingBlockFinalizationsDB;
const Member = Journals.MemberColumns;

/** A dead journal the rebase disposes of, and why it is dead. */
export type JournalDisposal = Readonly<{
  headerHash: string;
  cause: string;
}>;

export type OwnJournalDisposition = Readonly<{
  /** The dead journals with an effect still pending. */
  dispose: readonly JournalDisposal[];
  /** Every dead journal's header, for the SQL readers of the same
   * transaction. */
  dead: PendingBlockFinalizationsDB.DeadHeaders;
  /** The signed commits (hex) of every dead journal. */
  deadCommits: ReadonlySet<string>;
  /**
   * Set when the dead set cannot be read (the follower's tables are
   * missing): the rebase does not run while it is.
   */
  held?: DriverHold;
}>;

/** The dead journals and those the rebase disposes of. */
export const ownJournalDisposition = Effect.gen(function* () {
  const dead = yield* Journals.deadJournals;
  if (dead.missing.length > 0)
    return {
      dispose: [],
      dead: [],
      deadCommits: new Set(),
      held: {
        reason: LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
        detail: `the own-journal disposition cannot read commit anchors: ${dead.missing.join(", ")} missing`,
      },
    } satisfies OwnJournalDisposition;
  const headers = Journals.deadHeadersOf(dead);
  if (headers.length === 0)
    return {
      dispose: [],
      dead: headers,
      deadCommits: new Set(),
    } satisfies OwnJournalDisposition;
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
    intended_tx_hash: Buffer | null;
    pending: boolean;
  }>`SELECT p.header_hash, p.intended_tx_hash,
      ${Journals.deadEffectPending(sql, headers)} AS pending
    FROM pending_block_finalizations p
    WHERE ${Journals.deadHeader(sql, "p.header_hash", headers)}
    ORDER BY p.created_at, p.header_hash`;
  return {
    dispose: rows
      .filter((row) => row.pending)
      .map((row) => {
        const headerHash = row.header_hash.toString("hex");
        return { headerHash, cause: dead.causes.get(headerHash)! };
      }),
    dead: headers,
    deadCommits: new Set(
      rows.flatMap((row) =>
        row.intended_tx_hash === null
          ? []
          : [row.intended_tx_hash.toString("hex")],
      ),
    ),
  } satisfies OwnJournalDisposition;
});

/**
 * Disposes of `disposals` in the caller's transaction (see above); `dead`
 * is the dead set the disposition was read over. Event statuses and the
 * working ledger are the rebase's to recompute.
 */
export const disposeJournals = (
  disposals: readonly JournalDisposal[],
  dead: PendingBlockFinalizationsDB.DeadHeaders,
) =>
  Effect.forEach(
    disposals,
    (disposal) =>
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const header = Buffer.from(disposal.headerHash, "hex");
        const record = yield* Journals.retrieveByHeaderHash(header, true);
        if (Option.isNone(record)) return;
        const journal = record.value;
        yield* sql`UPDATE withdrawal_utxos d SET status = 'awaiting',
          reopened_from_header_hash = ${header}, validity = NULL,
          validity_detail = '{}'::jsonb, settlement_event_info = NULL,
          classification_revision = d.classification_revision + 1,
          updated_at = NOW()
          FROM pending_block_finalization_withdrawals member
          WHERE member.header_hash = ${header}
            AND d.event_id = member.member_id
            AND d.status = 'projected' AND d.projected_header_hash IS NULL
            AND d.classification_revision = member.classification_revision
            AND NOT EXISTS (SELECT 1
              FROM pending_block_finalization_withdrawals other
              WHERE other.member_id = member.member_id
                AND other.header_hash <> member.header_hash
                AND NOT ${Journals.deadHeader(sql, "other.header_hash", dead)})`;
        yield* MempoolInclusionsDB.clearMarks([header]);
        yield* BlocksDB.clearBlock(header);
        const memberIds = journal.txMembers.map((member) =>
          Buffer.from(member[Member.MEMBER_ID]),
        );
        // A transaction left in `immutable` would be filtered from its next
        // block as already committed; one a live block holds stays.
        if (memberIds.length > 0)
          yield* sql`DELETE FROM ${sql(ImmutableDB.tableName)} i
            WHERE i.tx_id IN ${sql.in(memberIds)}
              AND NOT EXISTS (SELECT 1 FROM ${sql(BlocksDB.tableName)} b
                WHERE b.tx_id = i.tx_id)`;
        // A member a rebuild rejected while its block was off the chain
        // stays rejected: a rejection is final.
        const absent = new Set(
          memberIds.length === 0
            ? []
            : (yield* sql<{ tx_id: Buffer }>`SELECT member.tx_id
                FROM unnest(${(sql as PgClient).array(
                  memberIds.map((id) => `\\x${id.toString("hex")}`),
                )}::bytea[]) AS member(tx_id)
                WHERE NOT EXISTS (SELECT 1 FROM tx_rejections r
                    WHERE r.tx_id = member.tx_id)
                  AND NOT EXISTS (SELECT 1 FROM mempool m
                    WHERE m.tx_id = member.tx_id)
                  AND NOT EXISTS (SELECT 1 FROM processed_mempool m
                    WHERE m.tx_id = member.tx_id)
                  AND NOT EXISTS (SELECT 1 FROM immutable i
                    WHERE i.tx_id = member.tx_id)
                  AND NOT EXISTS (SELECT 1 FROM node_foreign_tx_inclusions f
                    WHERE f.tx_id = member.tx_id)`).map((row) =>
                row.tx_id.toString("hex"),
              ),
        );
        const fromTable = (table: string) =>
          journal.txMembers
            .filter(
              (member) =>
                member[Member.SOURCE_TABLE] === table &&
                absent.has(
                  Buffer.from(member[Member.MEMBER_ID]).toString("hex"),
                ),
            )
            .map(Journals.txMemberToEntry);
        yield* MempoolDB.restoreJournalEntries(fromTable(MempoolDB.tableName));
        yield* ProcessedMempoolDB.insertTxs(
          fromTable(ProcessedMempoolDB.tableName),
        );
        yield* StateQueueMutationLeasesDB.release(
          journal[Journals.Columns.STATE_QUEUE_LEASE_TOKEN],
        );
        yield* Effect.logWarning(
          `Disposed of own block ${disposal.headerHash}: ${disposal.cause}. Its members are pending again; if its block lands after all, the rebase takes it in.`,
        );
      }),
    { discard: true },
  );
