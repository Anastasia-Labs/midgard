/**
 * The status of this node's block journals, derived in one place from the
 * facts (plan §8.2, §8.3, #753); no journal stores a copy. Pure SQL
 * fragments with no service behind them: the forced-order derivation's
 * modules reach them (`l1-admission-identity.ts`), and the S3 determinism
 * lint holds everything it reaches to no clock, randomness or network. The
 * reads over them are `pendingBlockFinalizations.derived-status.ts`.
 *
 * - `folded`: `confirmed_ledger` holds it: it is the frontier or a
 *   retained fold, whatever landed row is left (the merge finalization's
 *   frontier bootstrap folds in place), or, once its fold was pruned at
 *   finality, the state queue merged it (its newest queue terminal) and no
 *   landed row holds it;
 * - `applied`: a processed landed row holds the block and the working
 *   ledger took it in;
 * - `landed`: a processed landed row holds it, not applied yet;
 * - `unlanded`: none of these. A rollback that takes a landed block off
 *   the chain makes it unlanded again; nothing is written to its journal.
 */
import type { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";

import { newestTerminalOutcome } from "../l1-queue-terminals/terminal-outcome.js";

export const OwnBlockStatus = {
  Unlanded: "unlanded",
  Landed: "landed",
  Applied: "applied",
  Folded: "folded",
} as const;

export type OwnBlockStatus =
  (typeof OwnBlockStatus)[keyof typeof OwnBlockStatus];

export const OWN_BLOCK_STATUSES: readonly OwnBlockStatus[] =
  Object.values(OwnBlockStatus);

/** The derived status of the journal aliased `alias`, as SQL text. */
export const ownBlockStatusSql = (sql: SqlClient.SqlClient, alias: string) => {
  const header = sql`${sql(alias)}.header_hash`;
  return sql`(CASE
    WHEN EXISTS (SELECT 1 FROM node_confirmed_ledger_frontier frontier
        WHERE frontier.header_hash = ${header})
      OR EXISTS (SELECT 1 FROM node_confirmed_merges merged
        WHERE merged.header_hash = ${header}) THEN 'folded'
    WHEN EXISTS (SELECT 1 FROM node_landed_blocks landed
      WHERE landed.header_hash = ${header} AND landed.state = 'processed'
        AND landed.applied) THEN 'applied'
    WHEN EXISTS (SELECT 1 FROM node_landed_blocks landed
      WHERE landed.header_hash = ${header} AND landed.state = 'processed')
      THEN 'landed'
    WHEN NOT EXISTS (SELECT 1 FROM node_landed_blocks landed
        WHERE landed.header_hash = ${header})
      AND ${newestTerminalOutcome(sql, `${alias}.header_hash`)} = 'merged'
      THEN 'folded'
    ELSE 'unlanded' END)`;
};

/** SQL condition: the journal aliased `alias` is applied or folded. */
export const appliedOrFolded = (sql: SqlClient.SqlClient, alias: string) =>
  sql`${ownBlockStatusSql(sql, alias)} IN ('applied', 'folded')`;

/** SQL condition: the journal aliased `alias` is unlanded or landed. */
export const notAppliedOrFolded = (sql: SqlClient.SqlClient, alias: string) =>
  sql`${ownBlockStatusSql(sql, alias)} IN ('unlanded', 'landed')`;

/** The headers of the dead journals (`deadJournals`), for SQL readers. */
export type DeadHeaders = readonly Buffer[];

/** SQL condition: `column` names a header of `dead`. */
export const deadHeader = (
  sql: SqlClient.SqlClient,
  column: string,
  dead: DeadHeaders,
) =>
  sql`${sql(column)} = ANY(${(sql as PgClient).array(
    dead.map((header) => `\\x${header.toString("hex")}`),
  )}::bytea[])`;

/**
 * SQL condition: the dead journal aliased `p` has a disposal effect pending
 * (`landed-blocks/own-journals.ts`): a withdrawal still classified for it
 * and in no journal outside `dead`, an inclusion mark its header holds, its
 * block's `blocks` rows, or its lease still active.
 */
export const deadEffectPending = (
  sql: SqlClient.SqlClient,
  dead: DeadHeaders,
) =>
  sql`(EXISTS (
    SELECT 1 FROM pending_block_finalization_withdrawals member
    JOIN withdrawal_utxos d ON d.event_id = member.member_id
    WHERE member.header_hash = p.header_hash
      AND d.status = 'projected' AND d.projected_header_hash IS NULL
      AND d.classification_revision = member.classification_revision
      AND NOT EXISTS (SELECT 1 FROM pending_block_finalization_withdrawals other
        WHERE other.member_id = member.member_id
          AND other.header_hash <> member.header_hash
          AND NOT ${deadHeader(sql, "other.header_hash", dead)}))
  OR EXISTS (SELECT 1 FROM mempool WHERE mempool.included_by = p.header_hash)
  OR EXISTS (SELECT 1 FROM processed_mempool processed
    WHERE processed.included_by = p.header_hash)
  OR EXISTS (SELECT 1 FROM blocks WHERE blocks.header_hash = p.header_hash)
  OR EXISTS (SELECT 1 FROM state_queue_mutation_leases lease
    WHERE lease.token = p.state_queue_lease_token
      AND lease.status = 'active'))`;
