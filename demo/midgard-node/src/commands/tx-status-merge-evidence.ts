import { heightAtDepth } from "@al-ft/midgard-l1-follower/heads";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { followerCoveredTipHeight } from "../l1-queue-terminals/index.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";
import { settlementDepthParameters } from "../services/settlement.status.js";

export type TxStatusMergeEvidence = Readonly<{
  /** `own_block`: this node's own commit journal holds the tx.
   * `landed_block`: a landed state-queue block (any operator's) includes a
   * tx this node accepted, which its row's inclusion mark names. */
  inclusion: "own_block" | "landed_block";
  headerHash: string;
  headerStatus: string;
  mergeStatus: "finalized" | "not_finalized";
  confirmedLedgerFinalized: boolean;
}>;

/** Commit finalization is not merge finalization. An own block's merge is
 * finalized once its journal derives `folded` (`ownBlockStatusSql`) and the
 * merge tx the queue-terminal projection (`node_l1_queue_terminals`) holds
 * for the header is `safe` (at least cd deep at the follower's covered
 * tip). A rollback of the merge tx deletes its row. A dead journal (its
 * block can no longer land) holds no tx. The same statement
 * requires the follower write gate (plan §8.1) to hold an applied view with
 * no driver recompute pending, so a recompute under way cannot publish old
 * evidence. Missing retained evidence cannot assert completion, including
 * after legitimate pruning.
 *
 * A tx another operator's block included is not in this node's own journal:
 * its pending row is marked by that landed block (`mempoolInclusions.ts`)
 * until the block's fold is final. That block's merge is finalized once it is
 * folded into `confirmed_ledger` (`node_confirmed_merges`) and its merge tx is
 * `safe` in the same projection, under the same write gate. A rollback that
 * takes the block off the landed chain clears the mark, so the tx is pending
 * again. Once that fold is final (no rollback reaches its merge), the row is
 * deleted and the inclusion is kept in `node_foreign_tx_inclusions`, for as
 * long as `immutable` keeps an own tx: its block's status is `final` and its
 * merge finalized, without the merge tx's projection row, which the
 * retention sweep prunes once final. The own journal wins when both hold the
 * tx. */
export const readTxStatusMergeEvidence = (txIds: readonly Buffer[]) =>
  Effect.gen(function* () {
    const identity = yield* ContractDeploymentIdentity;
    const { confirmationDepth } = yield* settlementDepthParameters;
    const empty = () => new Map<string, TxStatusMergeEvidence>();
    if (
      identity.manifest === undefined ||
      identity.manifestId === undefined ||
      txIds.length === 0
    )
      return empty();
    const tipHeight = yield* followerCoveredTipHeight;
    if (tipHeight === null) return empty();
    const safeThroughHeight = heightAtDepth(tipHeight, confirmationDepth);
    const sql = yield* SqlClient.SqlClient;
    return yield* Effect.gen(function* () {
      // A dead journal is only ever unlanded, so the dead set read here
      // bounds which unlanded journals are reported, never a merge.
      const dead = PendingBlockFinalizationsDB.deadHeadersOf(
        yield* PendingBlockFinalizationsDB.deadJournals,
      );
      // One statement observes membership, derived status, terminal
      // projection and the write gate together, rather than mixing
      // independently read snapshots.
      const rows = yield* sql<{
        inclusion: "own_block" | "landed_block";
        tx_hash: string;
        header_hash: string;
        status: string;
        merged: boolean;
      }>`WITH gate AS (SELECT EXISTS (
            SELECT 1 FROM node_follower_write_gate gate
            WHERE gate.singleton AND gate.pending_reason IS NULL
              AND gate.applied_generation IS NOT NULL) AS open),
          marked AS (
            SELECT tx_id, included_by FROM mempool
            WHERE ${sql.in("tx_id", [...txIds])} AND included_by IS NOT NULL
            UNION
            SELECT tx_id, included_by FROM processed_mempool
            WHERE ${sql.in("tx_id", [...txIds])} AND included_by IS NOT NULL)
        SELECT 'own_block' AS inclusion,
          encode(member.member_id, 'hex') AS tx_hash,
          encode(member.header_hash, 'hex') AS header_hash, own.status,
          (own.status = 'folded' AND EXISTS (
            SELECT 1 FROM node_l1_queue_terminals outcome
            WHERE outcome.header_hash = own.header_hash
              AND outcome.terminal_outcome = 'merged'
              AND outcome.height <= ${safeThroughHeight}
          ) AND (SELECT open FROM gate)) AS merged
          FROM pending_block_finalization_txs member
          JOIN LATERAL (SELECT pending.header_hash,
              ${PendingBlockFinalizationsDB.ownBlockStatusSql(sql, "pending")} AS status
            FROM pending_block_finalizations pending
            WHERE pending.header_hash = member.header_hash) own ON TRUE
          WHERE ${sql.in("member.member_id", [...txIds])}
            AND NOT ${PendingBlockFinalizationsDB.deadHeader(sql, "member.header_hash", dead)}
        UNION ALL
        SELECT 'landed_block' AS inclusion,
          encode(marked.tx_id, 'hex') AS tx_hash,
          encode(marked.included_by, 'hex') AS header_hash,
          CASE WHEN folded.header_hash IS NULL THEN 'landed' ELSE 'folded' END AS status,
          (folded.header_hash IS NOT NULL AND EXISTS (
            SELECT 1 FROM node_l1_queue_terminals outcome
            WHERE outcome.header_hash = marked.included_by
              AND outcome.terminal_outcome = 'merged'
              AND outcome.height <= ${safeThroughHeight}
          ) AND (SELECT open FROM gate)) AS merged
          FROM marked
          LEFT JOIN node_confirmed_merges folded ON folded.header_hash = marked.included_by
          WHERE folded.header_hash IS NOT NULL OR EXISTS (
            SELECT 1 FROM node_landed_blocks landed
            WHERE landed.header_hash = marked.included_by
              AND landed.state = 'processed')
        UNION ALL
        SELECT 'landed_block' AS inclusion,
          encode(kept.tx_id, 'hex') AS tx_hash,
          encode(kept.header_hash, 'hex') AS header_hash,
          'final' AS status, TRUE AS merged
          FROM node_foreign_tx_inclusions kept
          WHERE ${sql.in("kept.tx_id", [...txIds])}`;
      const grouped = new Map<string, typeof rows>();
      for (const row of rows)
        grouped.set(row.tx_hash, [...(grouped.get(row.tx_hash) ?? []), row]);
      const result = empty();
      for (const [txHash, all] of grouped) {
        const own = all.filter((row) => row.inclusion === "own_block");
        const members = own.length > 0 ? own : all;
        if (members.length !== 1) continue;
        const row = members[0]!;
        result.set(txHash, {
          inclusion: row.inclusion,
          headerHash: row.header_hash,
          headerStatus: row.status,
          mergeStatus: row.merged ? "finalized" : "not_finalized",
          confirmedLedgerFinalized: row.merged,
        });
      }
      return result;
    });
  });
