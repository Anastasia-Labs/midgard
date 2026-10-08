import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import type { Database } from "midgard-node/services/database";

import {
  type CountRow,
  countValue,
} from "./e2e-finalize-summary.collector-step.js";

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
      SELECT 'pending_finalization_tx_headers' AS label, COUNT(DISTINCT header_hash) AS count
        FROM pending_block_finalization_txs
      UNION ALL
      SELECT 'pending_finalizations_finalized_tx_headers' AS label, COUNT(DISTINCT f.header_hash) AS count
        FROM pending_block_finalizations f
        JOIN pending_block_finalization_txs t USING (header_hash)
        WHERE f.status = 'locally_applied'
      UNION ALL
      SELECT 'pending_finalizations_unfinished' AS label, COUNT(*) AS count
        FROM pending_block_finalizations WHERE status <> 'locally_applied'
      UNION ALL
      SELECT 'mempool' AS label, COUNT(*) AS count FROM mempool
        WHERE included_by IS NULL
      UNION ALL
      SELECT 'processed_mempool' AS label, COUNT(*) AS count FROM processed_mempool
        WHERE included_by IS NULL
      UNION ALL
      SELECT 'blocks' AS label, COUNT(*) AS count FROM blocks
      UNION ALL
      SELECT 'immutable' AS label, COUNT(*) AS count FROM immutable
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
