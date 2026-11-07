import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import type { Database } from "midgard-node/services/database";

import {
  type CountRow,
  countValue,
} from "./e2e-finalize-summary.collector-step.js";

/**
 * The node's counts the finalizer gates on. Each is a fact this node holds
 * whichever operator committed the run's blocks:
 * - An abandoned journal (this node's block lost its slot, its txs went back
 *   to the mempool) can never finish, so it is neither unfinished nor
 *   tx-bearing.
 * - `committed_l2_txs` is every tx this node's own blocks hold (`immutable`)
 *   or a landed block, own or another operator's, marked included. Once the
 *   block's fold is final the marked row is deleted and another operator's
 *   inclusion is kept in `node_foreign_tx_inclusions`, so it still counts.
 * - `deposits_consumed` already counts foreign blocks: the fold consumes a
 *   foreign block's deposits as it does an own block's.
 */
export const collectDbCounts = (): Effect.Effect<
  ReadonlyMap<string, bigint>,
  never,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<CountRow>`
      SELECT 'deposits_consumed' AS label, COUNT(*) AS count
        FROM deposits_utxos WHERE status = 'consumed'
      UNION ALL
      SELECT 'tx_admissions_accepted' AS label, COUNT(*) AS count
        FROM tx_admissions WHERE status = 'accepted'
      UNION ALL
      SELECT 'pending_finalizations_finalized' AS label, COUNT(*) AS count
        FROM pending_block_finalizations WHERE status = 'locally_applied'
      UNION ALL
      SELECT 'pending_finalization_tx_headers' AS label, COUNT(DISTINCT f.header_hash) AS count
        FROM pending_block_finalizations f
        JOIN pending_block_finalization_txs t USING (header_hash)
        WHERE f.status <> 'abandoned'
      UNION ALL
      SELECT 'pending_finalizations_finalized_tx_headers' AS label, COUNT(DISTINCT f.header_hash) AS count
        FROM pending_block_finalizations f
        JOIN pending_block_finalization_txs t USING (header_hash)
        WHERE f.status = 'locally_applied'
      UNION ALL
      SELECT 'pending_finalizations_unfinished' AS label, COUNT(*) AS count
        FROM pending_block_finalizations
        WHERE status NOT IN ('locally_applied', 'abandoned')
      UNION ALL
      SELECT 'mempool' AS label, COUNT(*) AS count FROM mempool
        WHERE included_by IS NULL
      UNION ALL
      SELECT 'processed_mempool' AS label, COUNT(*) AS count FROM processed_mempool
        WHERE included_by IS NULL
      UNION ALL
      SELECT 'blocks' AS label, COUNT(*) AS count FROM blocks b
        WHERE NOT EXISTS (SELECT 1 FROM node_confirmed_merges m
          WHERE m.header_hash = b.header_hash)
      UNION ALL
      SELECT 'committed_l2_txs' AS label, COUNT(*) AS count FROM (
        SELECT tx_id FROM immutable
        UNION SELECT tx_id FROM mempool WHERE included_by IS NOT NULL
        UNION SELECT tx_id FROM processed_mempool WHERE included_by IS NOT NULL
        UNION SELECT tx_id FROM node_foreign_tx_inclusions
      ) committed
      UNION ALL
      SELECT 'confirmed_ledger' AS label, COUNT(*) AS count FROM confirmed_ledger
      UNION ALL
      SELECT 'local_mutation_jobs_unfinished' AS label, COUNT(*) AS count
        FROM local_mutation_jobs WHERE status <> 'completed'
      UNION ALL
      SELECT 'da_payloads' AS label, COUNT(*) AS count FROM da_payloads
    `;
    return new Map(
      rows.map((row) => [row.label, countValue(row.count)] as const),
    );
  }).pipe(Effect.orDie);
