import { SqlClient } from "@effect/sql";
import { Clock, Effect } from "effect";

import { finalityHeldPayload, type RetentionL1View } from "./daPayloads.js";
import { orphanedAdmission } from "./l1-admission-identity.js";

/**
 * What a housekeeping prune reads to keep challenge-relevant rows: the
 * authenticated L1 view and the verified deployment's identity digest. A
 * history prune never runs without both (a derived bundle has no
 * authenticated transitions to consult, so it cannot tell a header still
 * inside L1 finality from one beyond it).
 */
export type HousekeepingHolds = Readonly<{
  view: RetentionL1View;
  deploymentIdentityDigest: Buffer;
}>;

/**
 * The SQL condition true when `headerColumn` names a header that some
 * transition in the correction observer's durable record, pending OR
 * admitted, took out of the state queue (a merge or a removal), or that the
 * observer's current durable cursor still names as live. A rollback revokes
 * terminal rows before saving the restored cursor: both sides must hold rows
 * even when a housekeeping pass carries an older topology snapshot.
 *
 * The observer admits a transition at confirmation depth, which is not
 * finality (k = 2160): an admitted merge or removal can still roll back, and
 * a removal reopens the events it carried. The observer keeps an admitted
 * merge until it is proven canonical deeper than k and nothing depends on
 * it, and never drops a timeout correction or fraud removal. A header in its
 * cursor queue may already have left L1's queue in a transition the observer
 * has not replayed yet; holding it covers that gap, since the replay names
 * it before the cursor moves past it. So while a header is named here, a
 * prune keeps every record it reaches.
 */
export const observerRecordedHeader = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
  deploymentIdentityDigest: Buffer,
) => sql`(${observerCursorHeader(sql, headerColumn, deploymentIdentityDigest)}
  OR ${sql(headerColumn)} IN (
  SELECT decode(named.header_hex, 'hex')
  FROM state_queue_terminal_observer_states AS observer,
    LATERAL (SELECT CASE jsonb_typeof(observer.state_record)
        WHEN 'string' THEN (observer.state_record #>> '{}')::jsonb
        ELSE observer.state_record
      END AS record) AS state,
    jsonb_array_elements(
      COALESCE(state.record -> 'pending', '[]'::jsonb) ||
      COALESCE(state.record -> 'admitted', '[]'::jsonb)) AS recorded(transition),
    jsonb_array_elements_text(
      COALESCE(recorded.transition -> 'removedHeaderHashes', '[]'::jsonb))
      AS named(header_hex)
  WHERE observer.deployment_identity_digest = ${deploymentIdentityDigest}
    -- Filtered, not projected to NULL: a NULL in the held set would make
    -- NOT IN unknown for every row and stop all pruning.
    AND named.header_hex ~ '^[0-9a-f]{56}$'))`;

/** Current durable live headers, re-read in each protective DELETE statement. */
export const observerCursorHeader = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
  deploymentIdentityDigest: Buffer,
) => sql`${sql(headerColumn)} IN (
  SELECT decode(node ->> 'headerHash', 'hex')
  FROM state_queue_terminal_observer_states AS observer,
    LATERAL (SELECT CASE jsonb_typeof(observer.state_record)
        WHEN 'string' THEN (observer.state_record #>> '{}')::jsonb
        ELSE observer.state_record
      END AS record) AS state,
    jsonb_array_elements(COALESCE(state.record -> 'cursorQueue', '[]'::jsonb)) AS node
  WHERE observer.deployment_identity_digest = ${deploymentIdentityDigest}
    AND node ->> 'headerHash' ~ '^[0-9a-f]{56}$')`;

/**
 * The SQL condition true when a row whose header is `headerColumn` is still
 * challenge-relevant and must be kept: the header is the L1 confirmed head,
 * live in the L1 state queue, held by DA retention for finality
 * (`finalityHeldPayload`), or still held by the correction observer
 * (`observerRecordedHeader`). A NULL header is never matched as kept, so
 * callers that may see one must also require it to be NOT NULL.
 */
export const challengeRelevantHeader = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
  holds: HousekeepingHolds,
) => sql`(${sql.in(headerColumn, [
  holds.view.confirmedHeadHash,
  ...holds.view.liveQueueHeaderHashes,
])}
  OR ${finalityHeldPayload(sql, holds.deploymentIdentityDigest, headerColumn)}
  OR ${observerRecordedHeader(sql, headerColumn, holds.deploymentIdentityDigest)})`;

/**
 * Journals recovery still reads, irrespective of age. Unfinished and abandoned
 * journals retain their same-base siblings (including other incarnations of a
 * non-root node) and the descendants displacement walks, plus those journals'
 * replay bases. UNION makes cycles finite. Retained native recovery plans keep
 * their primary, member and displaced headers plus immediate replay bases in
 * both states: "applied" alone is no retirement proof. Prepared plans seed the
 * same dependency closure while recovery owns the closed history gate; applied
 * receipts do not retain future finalized descendants forever. An undecodable
 * plan fails the batch closed.
 */
export const recoveryRelevantJournal = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
  deploymentIdentityDigest?: Buffer,
) => sql`${sql(headerColumn)} IN (
  WITH RECURSIVE retained_plans AS (
    SELECT header_hash, state, intent::jsonb AS intent FROM event_history_recovery_plans
    WHERE ${deploymentIdentityDigest === undefined ? sql`TRUE` : sql`manifest_id = ${deploymentIdentityDigest}`}
  ), plan_intents AS (
    SELECT state, intent FROM retained_plans
    UNION ALL
    SELECT state, intent -> 'originalIntent' FROM retained_plans
    WHERE intent ->> 'domain' = 'midgard-history-displacement-compensation-intent-v1'
  ), plan_headers AS (
    SELECT header_hash, state FROM retained_plans
  UNION SELECT decode(intent ->> 'headerHash', 'hex'), state FROM plan_intents
    WHERE intent ->> 'headerHash' ~ '^[0-9a-f]{56}$'
  UNION SELECT decode(member ->> 'headerHash', 'hex'), plan.state
    FROM plan_intents AS plan,
      jsonb_array_elements(
        COALESCE(plan.intent -> 'members', '[]'::jsonb) ||
        COALESCE(plan.intent -> 'suffixMembers', '[]'::jsonb)) AS member
    WHERE member ->> 'headerHash' ~ '^[0-9a-f]{56}$'
  UNION SELECT decode(named.header_hex, 'hex'), plan.state
    FROM plan_intents AS plan,
      jsonb_array_elements_text(
        COALESCE(plan.intent -> 'displacedHeaderHashes', '[]'::jsonb) ||
        COALESCE(plan.intent -> 'prefixHeaderHashes', '[]'::jsonb) ||
        COALESCE(plan.intent -> 'suffixHeaderHashes', '[]'::jsonb))
        AS named(header_hex)
    WHERE named.header_hex ~ '^[0-9a-f]{56}$'
  ), recovery_seeds AS (
    SELECT header_hash, base_tail_header_hash, base_tail_out_ref, base_utxos_root
    FROM pending_block_finalizations
    WHERE status <> 'locally_applied' OR header_hash IN (
      SELECT header_hash FROM plan_headers WHERE state = 'prepared')
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
  UNION SELECT base_tail_header_hash FROM dependent
  UNION SELECT header_hash FROM plan_headers
  UNION SELECT journal.base_tail_header_hash
    FROM pending_block_finalizations AS journal
    JOIN plan_headers AS plan ON journal.header_hash = plan.header_hash)`;

/**
 * The SQL condition true when the journal whose header is `headerColumn` has
 * a deposit or withdrawal member whose follower admission identity L1 no
 * longer holds in its key set (an orphan). Such a journal is incomplete:
 * signed header recovery still classifies its header
 * (`signedHeaderRecoveryCandidates`), and ledger repair refuses to undo an
 * orphan's admission while a retained header names it as a member, so
 * deleting the journal would turn that refusal into a repair.
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
