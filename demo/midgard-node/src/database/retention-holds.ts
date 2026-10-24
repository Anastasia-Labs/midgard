import { SqlClient } from "@effect/sql";
import { Clock, Effect } from "effect";

import { finalityHeldPayload, type RetentionL1View } from "./daPayloads.js";
import { orphanedAdmission } from "./l1-admission-identity.js";

/**
 * What a housekeeping prune reads to keep challenge-relevant rows: the
 * L1 view and the verified deployment's identity digest. A history prune
 * never runs without both.
 */
export type HousekeepingHolds = Readonly<{
  view: RetentionL1View;
  deploymentIdentityDigest: Buffer;
}>;

/**
 * The SQL condition true when a row whose header is `headerColumn` is still
 * challenge-relevant and must be kept: the header is the L1 confirmed head,
 * live in the L1 state queue, or held by DA retention for finality
 * (`finalityHeldPayload`: live in the follower's facts, taken out of the
 * queue by a landed tx that is not final yet, or the merge boundary). A NULL
 * header is never matched as kept, so callers that may see one must also
 * require it to be NOT NULL.
 */
export const challengeRelevantHeader = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
  holds: HousekeepingHolds,
) => sql`(${sql.in(headerColumn, [
  holds.view.confirmedHeadHash,
  ...holds.view.liveQueueHeaderHashes,
])}
  OR ${finalityHeldPayload(sql, holds.deploymentIdentityDigest, headerColumn, holds.view.finalThroughHeight)})`;

/**
 * Journals recovery still reads, irrespective of age. Unfinished and abandoned
 * journals retain their same-base siblings (including other incarnations of a
 * non-root node) and the descendants displacement walks, plus those journals'
 * replay bases. UNION makes cycles finite.
 */
export const recoveryRelevantJournal = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
) => sql`${sql(headerColumn)} IN (
  WITH RECURSIVE recovery_seeds AS (
    SELECT header_hash, base_tail_header_hash, base_tail_out_ref, base_utxos_root
    FROM pending_block_finalizations
    WHERE status <> 'locally_applied'
  ), dependent AS (
    SELECT sibling.header_hash, sibling.base_tail_header_hash
    FROM pending_block_finalizations AS sibling
    WHERE sibling.header_hash IN (SELECT header_hash FROM recovery_seeds) OR EXISTS (
      SELECT 1 FROM recovery_seeds AS seed
      WHERE sibling.base_tail_out_ref = seed.base_tail_out_ref
        OR (seed.base_tail_header_hash <> decode(repeat('00', 28), 'hex')
          AND sibling.base_tail_header_hash = seed.base_tail_header_hash
          AND sibling.base_utxos_root = seed.base_utxos_root))
    UNION
    SELECT child.header_hash, child.base_tail_header_hash
    FROM pending_block_finalizations AS child
    JOIN dependent AS parent ON child.base_tail_header_hash = parent.header_hash
  )
  SELECT header_hash FROM dependent
  UNION SELECT base_tail_header_hash FROM dependent)`;

/**
 * The SQL condition true when the journal whose header is `headerColumn` has
 * a deposit or withdrawal member whose follower admission identity L1 no
 * longer holds in its key set (an orphan). Such a journal is incomplete:
 * ledger repair refuses to undo an orphan's admission while a journal not
 * abandoned names it as a member, so deleting the journal would turn that
 * refusal into a repair.
 */
export const orphanMemberJournal = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
) => sql`(EXISTS (
    SELECT 1 FROM pending_block_finalization_deposits AS member
    WHERE member.header_hash = ${sql(headerColumn)}
      AND ${orphanedAdmission(sql, "member", "deposit")})
  OR EXISTS (
    SELECT 1 FROM pending_block_finalization_withdrawals AS member
    WHERE member.header_hash = ${sql(headerColumn)}
      AND ${orphanedAdmission(sql, "member", "withdrawal")}))`;

/**
 * Runs `batch(limit)` until a batch deletes fewer than `limit` rows,
 * `maxBatches` ran, or the clock passes `deadlineMs`; no batch starts after the
 * deadline, so a caller holding a permit releases it within one batch of it.
 * Each batch is its own statement (and, for a history write, its own
 * transaction). Returns the number removed.
 */
export const pruneInBatches = <E, R>({
  batch,
  batchLimit,
  maxBatches,
  deadlineMs,
}: {
  readonly batch: (limit: number) => Effect.Effect<number, E, R>;
  readonly batchLimit: number;
  readonly maxBatches: number;
  readonly deadlineMs?: number;
}): Effect.Effect<number, E, R> =>
  Effect.gen(function* () {
    const limit = Math.max(1, Math.floor(batchLimit));
    let removed = 0;
    for (let index = 0; index < Math.max(1, maxBatches); index++) {
      if (
        deadlineMs !== undefined &&
        (yield* Clock.currentTimeMillis) >= deadlineMs
      )
        break;
      const deleted = yield* batch(limit);
      removed += deleted;
      if (deleted < limit) break;
    }
    return removed;
  });
