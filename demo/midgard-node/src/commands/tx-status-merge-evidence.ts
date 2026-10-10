import { heightAtDepth } from "@al-ft/midgard-l1-follower/heads";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { followerCoveredTipHeight } from "../l1-queue-terminals/index.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";
import { settlementDepthParameters } from "../services/settlement.status.js";

export type TxStatusMergeEvidence = Readonly<{
  headerHash: string;
  headerStatus: string;
  mergeStatus: "finalized" | "not_finalized";
  confirmedLedgerFinalized: boolean;
}>;

/** Commit finalization is not merge finalization. Read the completed local
 * merge job together with the merge tx the queue-terminal projection
 * (`node_l1_queue_terminals`) holds for the header, once it is `safe` (at
 * least cd deep at the follower's covered tip). A rollback of the merge tx
 * deletes its row; completed jobs alone survive it. The same statement
 * requires the follower write gate (plan §8.1) to hold an applied view with
 * no driver recompute pending, so a recompute under way cannot publish old
 * evidence. Missing retained evidence cannot assert completion, including
 * after legitimate pruning. */
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
    const jobDocument = sql`CASE jsonb_typeof(job.payload) WHEN 'string' THEN (job.payload #>> '{}')::jsonb ELSE job.payload END`;
    return yield* Effect.gen(function* () {
      // One statement observes membership, job, terminal projection and
      // the write gate together, rather than mixing independently read
      // snapshots.
      const rows = yield* sql<{
        tx_hash: string;
        header_hash: string;
        status: string;
        merged: boolean;
      }>`SELECT encode(member.member_id, 'hex') AS tx_hash,
          encode(member.header_hash, 'hex') AS header_hash, pending.status,
          (pending.status = 'locally_applied' AND EXISTS (
            SELECT 1 FROM local_mutation_jobs job
            JOIN node_l1_queue_terminals outcome
              ON outcome.header_hash = pending.header_hash
            WHERE job.job_id = 'confirmed_merge_finalization:' || encode(pending.header_hash, 'hex')
              AND job.kind = 'confirmed_merge_finalization'
              AND job.status = 'completed'
              AND (${jobDocument}) ->> 'headerHash' = encode(pending.header_hash, 'hex')
              AND (${jobDocument}) ->> 'confirmedLedgerSnapshotRoot' = pending.expected_utxos_root
              AND outcome.terminal_outcome = 'merged'
              AND outcome.height <= ${safeThroughHeight}
          ) AND EXISTS (
            SELECT 1 FROM node_follower_write_gate gate
            WHERE gate.singleton AND gate.pending_reason IS NULL
              AND gate.applied_generation IS NOT NULL
          )) AS merged
          FROM pending_block_finalization_txs member
          JOIN pending_block_finalizations pending ON pending.header_hash = member.header_hash
          WHERE ${sql.in("member.member_id", [...txIds])} AND pending.status <> 'abandoned'`;
      const grouped = new Map<string, typeof rows>();
      for (const row of rows)
        grouped.set(row.tx_hash, [...(grouped.get(row.tx_hash) ?? []), row]);
      const result = empty();
      for (const [txHash, members] of grouped) {
        if (members.length !== 1) continue;
        const row = members[0]!;
        result.set(txHash, {
          headerHash: row.header_hash,
          headerStatus: row.status,
          mergeStatus: row.merged ? "finalized" : "not_finalized",
          confirmedLedgerFinalized: row.merged,
        });
      }
      return result;
    });
  });
