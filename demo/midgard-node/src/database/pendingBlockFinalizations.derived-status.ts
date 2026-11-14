/**
 * The reads of this node's block journals' derived status (plan §8.2, §8.3,
 * #753; the status itself is `pendingBlockFinalizations.status-sql.ts`).
 *
 * An unlanded journal is dead when its block can no longer land
 * (`deadJournals`); the node's live journal is its newest unlanded journal
 * that is not dead (`liveJournalSql`). A header missing from the dead set
 * holds, so every reader takes the dead set, never the live one.
 */
import { isDeadStatus } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { liveQueueNodeHeader } from "../l1-queue-terminals/reads.js";
import { newestTerminalOutcome } from "../l1-queue-terminals/terminal-outcome.js";
import { Database } from "../services/database.js";
import { readIntentStatus } from "../services/intent-journal.js";
import { commitAnchorCanonical } from "./commit-anchor.js";
import { tableName } from "./pendingBlockFinalizations.columns.js";
import {
  deadHeader,
  type DeadHeaders,
  type OwnBlockStatus,
  ownBlockStatusSql,
} from "./pendingBlockFinalizations.status-sql.js";
import { type DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** The derived status of `headerHash`'s journal, or undefined without one. */
export const retrieveStatusByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<OwnBlockStatus | undefined, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ status: OwnBlockStatus }>`SELECT
        ${ownBlockStatusSql(sql, "p")} AS status
      FROM ${sql(tableName)} p WHERE p.header_hash = ${headerHash}`;
    return rows[0]?.status;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to read an own block's status"),
  );

/** The refusal a prepare, or a commit preflight, gets while another
 * journal is live. */
export const LIVE_JOURNAL_REFUSAL =
  "Refusing to prepare a new pending block while another live own-block journal exists";

/**
 * The live journal over `dead`: the newest unlanded journal (alias `live`)
 * that is not dead. It may be unsigned.
 */
export const liveJournalSql = (sql: SqlClient.SqlClient, dead: DeadHeaders) =>
  sql`SELECT live.* FROM ${sql(tableName)} live
    WHERE ${ownBlockStatusSql(sql, "live")} = 'unlanded'
      AND NOT ${deadHeader(sql, "live.header_hash", dead)}
    ORDER BY live.created_at DESC, live.header_hash DESC
    LIMIT 1`;

export type DeadJournals = Readonly<{
  /** Dead journal header (hex) to why its block can no longer land. */
  causes: ReadonlyMap<string, string>;
  /**
   * The follower tables whose absence leaves the set unread: no anchor or
   * commit status can be read, so no journal is dead.
   */
  missing: readonly string[];
}>;

/** `dead`'s headers. */
export const deadHeadersOf = (dead: DeadJournals): DeadHeaders =>
  [...dead.causes.keys()].map((header) => Buffer.from(header, "hex"));

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
 * The unlanded journals whose block can no longer land, and why:
 *
 * - a landed transaction removed the block from the state queue (its
 *   newest queue terminal);
 * - its commit anchor is not on the follower's chain, or it has none
 *   (`commit-anchor.ts`): an included event's block may be gone;
 * - its base left the landed chain: the base is not the frontier, a
 *   processed landed row or a retained fold (a journal built on a dead
 *   journal's block is dead with it);
 * - another processed or folded block took the slot after its base;
 * - it is signed and S6 derives its commit dead (`isDeadStatus`) at the
 *   follower's view, or its commit's intent was pruned while the follower's
 *   facts hold no live queue node of its header. The follower prunes an
 *   intent once it was terminal for k blocks, which a landing is too; what
 *   keeps the intent of an own block that landed but is not processed yet
 *   is the landed frontier's floor on that prune (`prune-floor.ts`), which
 *   holds the facts back to where the frontier's header entered the queue.
 *   With the frontier's header outside the facts the floor sets none, so
 *   the live queue node is checked here too.
 *
 * Callers inside a follower-gated write compute it once in that
 * transaction and pass its headers to every SQL reader there.
 */
export const deadJournals: Effect.Effect<
  DeadJournals,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [tables] = yield* sql<{
    blocks: boolean;
    cursor: boolean;
    intents: boolean;
  }>`SELECT to_regclass('l1_blocks') IS NOT NULL AS blocks,
    to_regclass('l1_follower_cursor') IS NOT NULL AS cursor,
    to_regclass('l1_intents') IS NOT NULL
      AND to_regclass('l1_output_assets') IS NOT NULL AS intents`;
  const missing = [
    ...(tables?.blocks === true ? [] : ["l1_blocks"]),
    ...(tables?.cursor === true ? [] : ["l1_follower_cursor"]),
  ];
  if (missing.length > 0) return { causes: new Map(), missing };
  const intentPruned =
    tables?.intents === true
      ? sql`(p.intended_tx_hash IS NOT NULL AND NOT EXISTS (
          SELECT 1 FROM l1_intents intent
          WHERE intent.tx_hash = p.intended_tx_hash)
        AND NOT ${liveQueueNodeHeader(sql, "p.header_hash")})`
      : sql`FALSE`;
  const rows = yield* sql<{
    header_hash: Buffer;
    base_tail_header_hash: Buffer;
    intended_tx_hash: Buffer | null;
    removed: boolean;
    anchored: boolean;
    anchor_canonical: boolean;
    base_on_chain: boolean;
    slot_taker: Buffer | null;
    intent_pruned: boolean;
  }>`SELECT p.header_hash, p.base_tail_header_hash, p.intended_tx_hash,
      COALESCE(${newestTerminalOutcome(sql, "p.header_hash")} = 'removed',
        FALSE) AS removed,
      p.commit_anchor_hash IS NOT NULL AS anchored,
      ${commitAnchorCanonical(sql, "p")} AS anchor_canonical,
      (EXISTS (SELECT 1 FROM node_confirmed_ledger_frontier frontier
          WHERE frontier.header_hash = p.base_tail_header_hash)
        OR EXISTS (SELECT 1 FROM node_landed_blocks landed
          WHERE landed.header_hash = p.base_tail_header_hash
            AND landed.state = 'processed')
        OR EXISTS (SELECT 1 FROM node_confirmed_merges merged
          WHERE merged.header_hash = p.base_tail_header_hash)) AS base_on_chain,
      (SELECT taker.header_hash FROM (
          SELECT landed.header_hash FROM node_landed_blocks landed
          WHERE landed.parent_header_hash = p.base_tail_header_hash
            AND landed.state = 'processed'
          UNION ALL
          SELECT merged.header_hash FROM node_confirmed_merges merged
          WHERE merged.parent_header_hash = p.base_tail_header_hash) taker
        WHERE taker.header_hash <> p.header_hash
        LIMIT 1) AS slot_taker,
      ${intentPruned} AS intent_pruned
    FROM ${sql(tableName)} p
    WHERE ${ownBlockStatusSql(sql, "p")} = 'unlanded'
    ORDER BY p.created_at, p.header_hash`;
  const causes = new Map<string, string>();
  for (const row of rows) {
    const header = row.header_hash.toString("hex");
    const base = row.base_tail_header_hash.toString("hex");
    const intended = row.intended_tx_hash?.toString("hex");
    const cause = row.removed
      ? "a landed transaction removed its block from the state queue"
      : !row.anchor_canonical
        ? row.anchored
          ? "its commit anchor left the chain"
          : "it has no commit anchor"
        : !row.base_on_chain
          ? `its base ${base} left the landed chain`
          : row.slot_taker !== null
            ? `block ${row.slot_taker.toString("hex")} took the slot after its base ${base}`
            : row.intent_pruned
              ? `its signed commit ${intended!} is no longer journaled`
              : intended !== undefined && (yield* intentDead(intended))
                ? `its signed commit ${intended} is dead`
                : undefined;
    if (cause !== undefined) causes.set(header, cause);
  }
  return { causes, missing: [] };
}).pipe(sqlErrorToDatabaseError(tableName, "Failed to read dead own journals"));
