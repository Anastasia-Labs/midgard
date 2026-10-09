/**
 * This node's block journals under whichever-lands-wins (plan §8.3, I3): the
 * working-ledger rebase disposes of every own journal whose block cannot be
 * on the landed chain the node follows, and revives every abandoned one
 * whose block landed after all. Neither halts: the local state follows the
 * landed block, and a journal disposed of early is revived if its block
 * lands, because the tail node makes the old and the new commit mutually
 * exclusive on chain.
 *
 * A journal neither abandoned nor taken in by a processed landed row (nor
 * the frontier, nor folded into `confirmed_ledger` here) is disposed of when
 *
 * - (i) a rollback took its block off the landed chain (a `removed` row);
 * - (ii) S6 derives its signed commit dead (`isDeadStatus`) at the
 *   follower's view;
 * - (iii) its base left: the base is removed or disposed of, or another
 *   processed block took the base's successor slot;
 * - (iv) it is unfinished and includes an event whose admission the
 *   follower no longer holds (a deposit, withdrawal or forced order);
 * - (v) an abandoned block of this node landed: it is revived, and every
 *   unfinished journal (built while it was abandoned) is disposed of.
 *
 * Disposal abandons the journal under its replacement digest (its signed
 * content kept, so it stays revivable), and makes its members pending again
 * (its transactions back in their pending tables, its withdrawals
 * unclassified); the rebase that runs it recomputes the event statuses, the
 * working ledger and the native MPF on the landed blocks. The commit path
 * then builds a replacement on the landed tip from what is selectable.
 */
import { createHash } from "node:crypto";

import { isDeadStatus } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Option } from "effect";

import {
  BlocksDB,
  ImmutableDB,
  MempoolDB,
  MempoolInclusionsDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxRejectionsDB,
} from "../database/index.js";
import {
  canonicalForcedAdmission,
  orphanedAdmission,
} from "../database/l1-admission-identity.js";
import { ACTIVE_STATUSES } from "../database/pendingBlockFinalizations.columns.js";
import { DatabaseError } from "../database/utils/common.js";
import type { DriverHold } from "../l1-events/driver.js";
import { signedIntentReplacementDigest } from "../services/canonical-journal-recovery.js";
import { readIntentStatus } from "../services/intent-journal.js";
import { retrieveMergeLinks } from "./confirmed-merges.js";
import { LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING } from "./holds.js";
import type { LandedLedger } from "./ledger.js";
import type { LandedBlockRow } from "./store.js";

const Journals = PendingBlockFinalizationsDB;
const C = Journals.Columns;
const Status = Journals.Status;
const Member = Journals.MemberColumns;

const OWN_BLOCK_REVIVAL_DOMAIN = "midgard/own-block-revival/v1";

const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table: Journals.tableName, message, cause });

const bytea = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

/** A journal the rebase disposes of, and why. */
export type JournalDisposal = Readonly<{
  headerHash: string;
  cause: string;
  /** Whether it was the node's one unfinished journal. */
  active: boolean;
}>;

export type OwnJournalDisposition = Readonly<{
  dispose: readonly JournalDisposal[];
  /** Processed own rows whose abandoned journal the rebase revives. */
  revive: readonly string[];
  /**
   * Set when the disposition cannot be read (the follower's admission
   * tables are missing): the rebase does not run while it is.
   */
  held?: DriverHold;
}>;

type Candidate = Readonly<{
  headerHash: string;
  baseTailHeaderHash: string;
  status: string;
  intendedTxHash: string | null;
}>;

const isActive = (status: string) =>
  (ACTIVE_STATUSES as readonly string[]).includes(status);

/** S6's derived status of a signed commit; unknown (or unreadable) keeps it. */
const intentDead = (txHash: string) =>
  readIntentStatus(txHash).pipe(
    Effect.map((status) => status !== null && isDeadStatus(status)),
    Effect.catchAll((cause) =>
      Effect.logWarning(
        `The status of own commit ${txHash} is unreadable; its journal is kept`,
        cause,
      ).pipe(Effect.as(false)),
    ),
  );

/**
 * Unfinished journals holding an event whose admission left the chain, or
 * the follower admission tables that are missing: without them no
 * admission can be read, so the disposition is held rather than read as
 * "nothing left the chain".
 */
const orphanHolders = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [tables] = yield* sql<{ keys: boolean; orders: boolean }>`SELECT
    to_regclass('l1_event_keys') IS NOT NULL AS keys,
    to_regclass('node_l1_forced_order_fields') IS NOT NULL AS orders`;
  const missing = [
    ...(tables?.keys === true ? [] : ["l1_event_keys"]),
    ...(tables?.orders === true ? [] : ["node_l1_forced_order_fields"]),
  ];
  if (missing.length > 0) return { kind: "held", missing } as const;
  const forced = sql`OR EXISTS (SELECT 1 FROM pending_block_finalization_forced_transactions m
        JOIN forced_transaction_utxos f ON f.tx_order_id = m.member_id
        WHERE m.header_hash = p.header_hash
          AND NOT ${canonicalForcedAdmission(sql, "f")})`;
  const rows = yield* sql<{ header_hash: Buffer }>`
    SELECT p.header_hash FROM pending_block_finalizations p
    WHERE p.status IN ${sql.in(ACTIVE_STATUSES)} AND (
      EXISTS (SELECT 1 FROM pending_block_finalization_deposits m
        WHERE m.header_hash = p.header_hash
          AND ${orphanedAdmission(sql, "m", "deposit")})
      OR EXISTS (SELECT 1 FROM pending_block_finalization_withdrawals m
        WHERE m.header_hash = p.header_hash
          AND ${orphanedAdmission(sql, "m", "withdrawal")})
      ${forced})`;
  return {
    kind: "read",
    holders: new Set(rows.map((row) => row.header_hash.toString("hex"))),
  } as const;
});

/**
 * The journals the rebase disposes of and revives over `rows`, with the
 * confirmed-ledger frontier and processed chain `landed`.
 */
export const ownJournalDisposition = (
  rows: readonly LandedBlockRow[],
  landed: LandedLedger,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const revive = rows
      .filter(
        (row) =>
          row.kind === "own" && row.state === "processed" && !row.applied,
      )
      .map((row) => row.headerHash);
    const processed = new Set(
      rows
        .filter((row) => row.state === "processed")
        .map((row) => row.headerHash),
    );
    const removed = new Set(
      rows
        .filter((row) => row.state === "removed")
        .map((row) => row.headerHash),
    );
    const raw = yield* sql<{
      header_hash: Buffer;
      base_tail_header_hash: Buffer;
      status: string;
      intended_tx_hash: Buffer | null;
    }>`SELECT header_hash, base_tail_header_hash, status, intended_tx_hash
      FROM pending_block_finalizations WHERE status <> ${Status.Abandoned}
      ORDER BY created_at, header_hash`;
    const folded = yield* retrieveMergeLinks;
    const candidates: Candidate[] = [];
    for (const row of raw) {
      const headerHash = row.header_hash.toString("hex");
      if (processed.has(headerHash)) continue;
      if (headerHash === landed.frontier.headerHash) continue;
      // Folded into `confirmed_ledger` here: a retained fold (an unfold
      // makes its row processed again), or the merge fiber's finalization.
      if (folded.has(headerHash)) continue;
      if (row.status === Status.LocallyApplied) {
        const merged = yield* MutationJobsDB.retrieveByJobId(
          MutationJobsDB.confirmedMergeFinalizationJobId(headerHash),
        );
        if (
          merged?.[MutationJobsDB.Columns.STATUS] ===
          MutationJobsDB.Status.Completed
        )
          continue;
      }
      candidates.push({
        headerHash,
        baseTailHeaderHash: row.base_tail_header_hash.toString("hex"),
        status: row.status,
        intendedTxHash: row.intended_tx_hash?.toString("hex") ?? null,
      });
    }
    if (candidates.length === 0) return { dispose: [], revive };
    const chain = [
      landed.frontier.headerHash,
      ...landed.chain.map((row) => row.headerHash),
    ];
    const position = new Map(chain.map((hash, index) => [hash, index]));
    const orphanRead = yield* orphanHolders;
    if (orphanRead.kind === "held")
      return {
        dispose: [],
        revive,
        held: {
          reason: LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
          detail: `the own-journal disposition cannot read event admissions: ${orphanRead.missing.join(", ")} missing`,
        },
      } satisfies OwnJournalDisposition;
    const orphans = orphanRead.holders;
    const causes = new Map<string, string>();
    for (const candidate of candidates) {
      const { headerHash, status, intendedTxHash } = candidate;
      if (removed.has(headerHash))
        causes.set(
          headerHash,
          "a rollback took its block off the landed chain",
        );
      else if (revive.length > 0 && isActive(status))
        causes.set(
          headerHash,
          `abandoned own block ${revive[0]!} landed and is revived`,
        );
      else if (orphans.has(headerHash))
        causes.set(
          headerHash,
          "it includes an event whose admission left the chain",
        );
      else if (intendedTxHash !== null && (yield* intentDead(intendedTxHash)))
        causes.set(headerHash, `its signed commit ${intendedTxHash} is dead`);
    }
    // A base that left takes every journal built on it along.
    for (let changed = true; changed; ) {
      changed = false;
      for (const candidate of candidates) {
        if (causes.has(candidate.headerHash)) continue;
        const base = candidate.baseTailHeaderHash;
        const index = position.get(base);
        const cause = removed.has(base)
          ? `its base ${base} left the landed chain`
          : causes.has(base)
            ? `its base ${base} is disposed of`
            : index !== undefined && index < chain.length - 1
              ? `block ${chain[index + 1]!} took the slot after its base ${base}`
              : undefined;
        if (cause === undefined) continue;
        causes.set(candidate.headerHash, cause);
        changed = true;
      }
    }
    return {
      dispose: candidates.flatMap((candidate) => {
        const cause = causes.get(candidate.headerHash);
        return cause === undefined
          ? []
          : [
              {
                headerHash: candidate.headerHash,
                cause,
                active: isActive(candidate.status),
              },
            ];
      }),
      revive,
    } satisfies OwnJournalDisposition;
  });

/** The record of journal `headerHash`, row-locked. */
const lockedRecord = (headerHash: string) =>
  Journals.retrieveByHeaderHash(Buffer.from(headerHash, "hex"), true).pipe(
    Effect.flatMap((record) =>
      Option.isNone(record)
        ? Effect.fail(
            failure("An own journal left before its disposition", headerHash),
          )
        : Effect.succeed(record.value),
    ),
  );

/**
 * Disposes of `disposals` in the caller's transaction: each journal is
 * abandoned under its replacement digest, its transactions are pending
 * again (marks cleared, rows restored but for those a rebuild rejected
 * meanwhile, its local finalization undone), an
 * unfinished one's withdrawals lose their classification, and its
 * state-queue lease is released. Event statuses and the working ledger are
 * the rebase's to recompute.
 */
export const disposeJournals = (disposals: readonly JournalDisposal[]) =>
  Effect.forEach(
    disposals,
    (disposal) =>
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const pg = sql as PgClient;
        const record = yield* lockedRecord(disposal.headerHash);
        const status = record[C.STATUS];
        if (status === Status.Abandoned) return;
        const header = record[C.HEADER_HASH];
        yield* sql`UPDATE pending_block_finalizations
          SET status = ${Status.Abandoned},
            correction_transition_digest = COALESCE(correction_transition_digest,
              ${signedIntentReplacementDigest(record) ?? null}),
            updated_at = NOW()
          WHERE header_hash = ${header} AND status = ${status}`;
        if (isActive(status) && record.withdrawalEventIds.length > 0)
          yield* sql`UPDATE withdrawal_utxos d SET status = 'awaiting',
            reopened_from_header_hash = ${header}, validity = NULL,
            validity_detail = '{}'::jsonb, settlement_event_info = NULL,
            classification_revision = d.classification_revision + 1,
            updated_at = NOW()
            WHERE d.event_id = ANY(${pg.array(bytea(record.withdrawalEventIds))}::bytea[])
              AND d.projected_header_hash IS NULL AND d.status = 'projected'`;
        // A member a rebuild rejected while its block was off the chain
        // stays rejected: a rejection is final.
        const memberIds = record.txMembers.map((member) =>
          Buffer.from(member[Member.MEMBER_ID]),
        );
        const rejected = new Set(
          memberIds.length === 0
            ? []
            : (yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM tx_rejections
                  WHERE tx_id IN ${sql.in(memberIds)}`).map((row) =>
                row.tx_id.toString("hex"),
              ),
        );
        const fromTable = (table: string) =>
          record.txMembers
            .filter(
              (member) =>
                member[Member.SOURCE_TABLE] === table &&
                !rejected.has(
                  Buffer.from(member[Member.MEMBER_ID]).toString("hex"),
                ),
            )
            .map(Journals.txMemberToEntry);
        yield* MempoolInclusionsDB.clearMarks([header]);
        yield* MempoolDB.restoreJournalEntries(fromTable(MempoolDB.tableName));
        yield* ProcessedMempoolDB.insertTxs(
          fromTable(ProcessedMempoolDB.tableName),
        );
        yield* BlocksDB.clearBlock(header);
        const txIds = memberIds;
        // A transaction left in ImmutableDB would be filtered from its next
        // block as already committed; one a live block holds stays.
        if (txIds.length > 0)
          yield* sql`DELETE FROM ${sql(ImmutableDB.tableName)} i
            WHERE i.tx_id IN ${sql.in(txIds)}
              AND NOT EXISTS (SELECT 1 FROM ${sql(BlocksDB.tableName)} b
                WHERE b.tx_id = i.tx_id)`;
        yield* MutationJobsDB.abandonLocalBlockFinalization(
          header,
          disposal.cause,
        );
        yield* StateQueueMutationLeasesDB.release(
          record[C.STATE_QUEUE_LEASE_TOKEN],
        );
        yield* Effect.logWarning(
          `Disposed of own block ${disposal.headerHash} (${status}): ${disposal.cause}. Its members are pending again; a block of this node that lands is revived.`,
        );
      }),
    { discard: true },
  );

const revivalDigest = (headerHash: Buffer) =>
  createHash("sha256")
    .update(Buffer.from(OWN_BLOCK_REVIVAL_DOMAIN, "utf8"))
    .update(headerHash)
    .digest("hex");

/**
 * Revives the abandoned journal of each processed own row `headers`: it is
 * observed again and waits for local finalization, its abandonment digest
 * kept (the mark of a revived block). Its members are in the landed block,
 * so a rejection recorded for one while the journal was abandoned is
 * deleted, with every rejection whose recorded causes are all deleted ones
 * (`TxRejectionsDB.deleteWithTracedRejections`), in the caller's
 * transaction. Returns the revived records.
 */
export const reviveJournals = (headers: readonly string[]) =>
  Effect.forEach(headers, (headerHash) =>
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const record = yield* lockedRecord(headerHash);
      if (record[C.STATUS] !== Status.Abandoned) return record;
      const header = record[C.HEADER_HASH];
      const digest =
        signedIntentReplacementDigest(record) ?? revivalDigest(header);
      const revived = yield* sql`UPDATE pending_block_finalizations
        SET status = ${Status.ObservedWaitingStability},
          observed_confirmed_at_ms = COALESCE(observed_confirmed_at_ms,
            ${BigInt(Date.now())}),
          correction_transition_digest = COALESCE(correction_transition_digest,
            ${digest}),
          updated_at = NOW()
        WHERE header_hash = ${header} AND status = ${Status.Abandoned}
        RETURNING header_hash`;
      if (revived.length !== 1)
        return yield* Effect.fail(
          failure("Failed to revive a landed own block's journal", headerHash),
        );
      const cleared = yield* TxRejectionsDB.deleteWithTracedRejections(
        record.txMembers.map((member) => Buffer.from(member[Member.MEMBER_ID])),
      );
      yield* Effect.logWarning(
        `Revived own block ${headerHash}: it landed after its journal was abandoned; local finalization follows it.${cleared.length === 0 ? "" : ` Deleted ${cleared.length.toString()} rejections of its members and of transactions rejected after them.`}`,
      );
      return yield* lockedRecord(headerHash);
    }),
  );
