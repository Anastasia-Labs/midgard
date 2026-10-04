import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import type { Database } from "../services/database.js";
import {
  requireCandidateHistory,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import type { RetentionL1View } from "./daPayloads.js";
import {
  type EvidenceScope,
  storedVerdict,
  type UndecodableEvidence,
} from "./foreignTipReconciliations.mark-resolved.js";
import {
  type Entry,
  type RawEntry,
  tableName,
} from "./foreignTipReconciliations.parse-evidence.js";
import { decodeEntry } from "./foreignTipReconciliations.parse-foreign-tip-reconciliation.js";
import { computeChallengeableCutoff } from "./retention-policy.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const FOREIGN_TIP_RETENTION_BATCH_SIZE = 128;
export const FOREIGN_TIP_RECONCILIATION_PAGE_SIZE = 64;

/** The canonical event index is materialized before the owner publishes Ready.
 * Its retained anchor accounts for pending signed-header recovery holds. A
 * checked Ready transaction serializes deletion with ingestion and rollback;
 * wall-clock age and a resolved status alone never establish eligibility.
 *
 * Every row needs its window ingested for the whole challengeability horizon,
 * measured from the earliest of now, the history coverage head (deposits,
 * withdrawals) and `txOrdersIngestedThrough` (forced transactions, ingested
 * apart from the history journal). Past that one cutoff, three kinds of row
 * can no longer affect any block: a row of another deployment or consensus
 * profile; and, of the active deployment, a resolved row or an awaiting row
 * whose stored verdict a window can lift (neither `invalid` nor
 * `foreign_event_present_requires_finalization`, with consistent event
 * commitments), either one only while no unsettled event occupies its window.
 * An event occupies a window while the commit gate would still count it
 * (no header carries it yet) or while it is unsettled. Without a deployment
 * manifest every row of the profile is in scope. This is the only pruner of
 * the table; one call deletes at most `FOREIGN_TIP_RETENTION_BATCH_SIZE`
 * rows. */
export const pruneBeyondRetention = (args: {
  readonly challengeableCutoff: Date;
  readonly view: RetentionL1View;
  readonly deploymentManifestId: string | undefined;
  readonly consensusProfileId: string;
  readonly txOrdersIngestedThrough: Date;
}): Effect.Effect<number, DatabaseError, Database> =>
  withHistoryWrite(
    Effect.gen(function* () {
      const permit = yield* requireCandidateHistory;
      if (Option.isNone(permit)) return 0;
      const { token, coverage } = permit.value;
      const boundary = coverage.retention;
      if (
        boundary === undefined ||
        (args.deploymentManifestId !== undefined &&
          token.deploymentIdentity !== args.deploymentManifestId)
      )
        return 0;
      const sql = yield* SqlClient.SqlClient;
      // A producer may survive an append that advances the anchor. Skip that
      // sweep and reacquire fresh coverage instead of using a stale boundary.
      const cursor = yield* sql`SELECT 1 FROM event_history_cursor
      WHERE binding_digest = ${Buffer.from(coverage.bindingDigest, "hex")}
        AND manifest_id = ${Buffer.from(token.deploymentIdentity, "hex")}
        AND anchor_hash = ${Buffer.from(boundary.anchor.id, "hex")}
        AND anchor_slot = ${boundary.anchor.slot}`;
      if (cursor.length !== 1) return 0;
      const exempt = [
        args.view.confirmedHeadHash,
        ...args.view.liveQueueHeaderHashes,
      ];
      // `computeChallengeableCutoff` is monotone, so this is the horizon
      // measured from the earliest of now and both ingestion barriers; it
      // also puts every row before the coverage head.
      const cutoff = new Date(
        Math.min(
          args.challengeableCutoff.getTime(),
          computeChallengeableCutoff(
            new Date(
              Math.min(
                coverage.includedThroughMs,
                args.txOrdersIngestedThrough.getTime(),
              ),
            ),
          ).getTime(),
        ),
      );
      const otherDeployment =
        args.deploymentManifestId === undefined
          ? sql`FALSE`
          : sql`r.deployment_manifest_id <> ${args.deploymentManifestId}`;
      const empty = SDK.EMPTY_MERKLE_TREE_ROOT;
      const rows = yield* sql<{ foreign_header_hash: Buffer }>`
      WITH eligible AS (
        SELECT r.foreign_header_hash FROM foreign_tip_reconciliations r
        WHERE r.block_end_time < ${cutoff}
          AND r.block_end_time < ${new Date(boundary.includedThroughMs)}
          AND r.foreign_header_hash NOT IN ${sql.in(exempt)}
          AND NOT EXISTS (
            SELECT 1 FROM pending_block_finalizations p
            WHERE p.deployment_manifest_id = r.deployment_manifest_id
              AND (p.status NOT IN ('finalized', 'abandoned')
                OR (p.status = 'abandoned' AND p.intended_tx_hash IS NOT NULL))
              AND (p.header_hash IN (r.foreign_header_hash, r.replaced_base_header_hash)
                OR p.base_tail_header_hash IN (r.foreign_header_hash, r.replaced_base_header_hash)))
          AND NOT EXISTS (
            SELECT 1 FROM event_history_recovery_plans p
            WHERE p.manifest_id = ${Buffer.from(token.deploymentIdentity, "hex")}
              AND p.header_hash IN (r.foreign_header_hash, r.replaced_base_header_hash))
          AND CASE
            -- No block of this deployment reads another deployment's evidence.
            WHEN ${otherDeployment}
              OR r.consensus_profile_id <> ${args.consensusProfileId}
              THEN TRUE
            ELSE (r.status = 'resolved'
              OR (split_part(COALESCE(r.blocking_reason, ''), ':', 1)
                  NOT IN ('invalid', 'foreign_event_present_requires_finalization')
                AND (r.deposits_root = ${empty}) = (r.deposit_count = 0)
                AND (r.forced_transactions_root = ${empty}) = (r.forced_transaction_count = 0)
                AND (r.withdrawals_root = ${empty}) = (r.withdrawal_count = 0)))
              -- The commit gate counts every deposit no header carries yet,
              -- consumed or not; forced transactions and withdrawals it counts
              -- only while awaiting or projected, which the status test covers.
              AND NOT EXISTS (SELECT 1 FROM deposits_utxos e
                WHERE (e.status <> 'consumed' OR e.projected_header_hash IS NULL) AND
                  (e.projected_header_hash = r.foreign_header_hash OR
                    (e.inclusion_time > r.block_start_time AND e.inclusion_time <= r.block_end_time)))
              AND NOT EXISTS (SELECT 1 FROM forced_transaction_utxos e
                WHERE e.status <> 'finalized' AND
                  (e.projected_header_hash = r.foreign_header_hash OR
                    (e.inclusion_time > r.block_start_time AND e.inclusion_time <= r.block_end_time)))
              AND NOT EXISTS (SELECT 1 FROM withdrawal_utxos e
                WHERE e.status <> 'finalized' AND
                  (e.projected_header_hash = r.foreign_header_hash OR
                    (e.inclusion_time > r.block_start_time AND e.inclusion_time <= r.block_end_time)))
          END
        ORDER BY r.block_end_time, r.foreign_header_hash
        LIMIT ${FOREIGN_TIP_RETENTION_BATCH_SIZE}
        FOR UPDATE OF r
      ) DELETE FROM foreign_tip_reconciliations r USING eligible
        WHERE r.foreign_header_hash = eligible.foreign_header_hash
        RETURNING r.foreign_header_hash`;
      return rows.length;
    }),
  ).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to prune foreign-tip reconciliation evidence",
    ),
  );

export type ForeignTipEvidencePageCursor = Readonly<{
  startTime: Date;
  endTime: Date;
  headerHash: Buffer;
}>;

export type ForeignTipEvidencePage = {
  readonly entries: readonly Entry[];
  readonly undecodable: readonly UndecodableEvidence[];
  /** Where the next page starts; absent after the last page. */
  readonly next: ForeignTipEvidencePageCursor | undefined;
};

/** Pages every row of the active deployment and profile in keyset order and
 * returns its actionable windows: awaiting rows, and resolved rows whose
 * window holds an awaiting event no header has projected yet (flagged in
 * SQL, so a settled resolved row is not replayed). Every row is decoded, one
 * by one: a row of either status that no longer decodes is returned beside
 * the others instead of failing the page, so its window still gates the
 * commit. Immutable window/key fields make keyset traversal safe when a row's
 * resolution changes during reconciliation. Every page is still processed;
 * pagination must not turn an unseen awaiting row into a Ready result. */
export const retrieveActionableEvidencePage = (
  scope: EvidenceScope,
  after?: ForeignTipEvidencePageCursor,
): Effect.Effect<ForeignTipEvidencePage, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const continuation =
      after === undefined
        ? sql`TRUE`
        : sql`
      (r.block_start_time, r.block_end_time, r.foreign_header_hash) >
        (${after.startTime}, ${after.endTime}, ${after.headerHash})`;
    const deployment =
      scope.manifestId === undefined
        ? sql`TRUE`
        : sql`r.deployment_manifest_id = ${scope.manifestId}`;
    const rows = yield* sql<RawEntry & { readonly actionable: boolean }>`
      SELECT r.*, (r.status = 'awaiting'
        OR EXISTS (SELECT 1 FROM deposits_utxos e
          WHERE e.status = 'awaiting' AND e.projected_header_hash IS NULL
            AND e.inclusion_time > r.block_start_time AND e.inclusion_time <= r.block_end_time)
        OR EXISTS (SELECT 1 FROM forced_transaction_utxos e
          WHERE e.status = 'awaiting' AND e.projected_header_hash IS NULL
            AND e.inclusion_time > r.block_start_time AND e.inclusion_time <= r.block_end_time)
        OR EXISTS (SELECT 1 FROM withdrawal_utxos e
          WHERE e.status = 'awaiting' AND e.projected_header_hash IS NULL
            AND e.inclusion_time > r.block_start_time AND e.inclusion_time <= r.block_end_time)
      ) AS actionable
      FROM foreign_tip_reconciliations r
      WHERE ${continuation} AND ${deployment}
        AND r.consensus_profile_id = ${scope.consensusProfileId}
      ORDER BY r.block_start_time, r.block_end_time, r.foreign_header_hash
      LIMIT ${FOREIGN_TIP_RECONCILIATION_PAGE_SIZE}`;
    const entries: Entry[] = [];
    const undecodable: UndecodableEvidence[] = [];
    for (const { actionable, ...row } of rows) {
      const decoded = yield* Effect.either(decodeEntry(row));
      if (decoded._tag === "Left") {
        undecodable.push({
          foreignHeaderHash: row.foreign_header_hash,
          blockStartTime: row.block_start_time,
          blockEndTime: row.block_end_time,
          verdict: storedVerdict(row),
          cause: decoded.left,
        });
      } else if (actionable) {
        entries.push(decoded.right);
      }
    }
    const last = rows[rows.length - 1];
    return {
      entries,
      undecodable,
      next:
        last === undefined || rows.length < FOREIGN_TIP_RECONCILIATION_PAGE_SIZE
          ? undefined
          : {
              startTime: last.block_start_time,
              endTime: last.block_end_time,
              headerHash: last.foreign_header_hash,
            },
    };
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve actionable foreign-tip evidence page",
    ),
  );
