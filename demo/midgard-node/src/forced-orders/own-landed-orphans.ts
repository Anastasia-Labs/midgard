/**
 * Forced rows assigned to a landed own block whose order left the chain
 * (`ownLandedForcedOrphan`, `l1-admission-identity.ts`). The node follows
 * the block: commits, merges and admission continue, the forced-order hook
 * logs a warning once per header, and `/readyz` reports the degradation
 * `l1_own_block_forced_order_orphaned` until the header leaves the landed
 * queue (a landed correction, a rollback that removes it, or its merge).
 *
 * Becomes a hold once a fault proof for a fabricated forced transaction
 * exists (NIFP-04).
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { ForcedTransactionsDB } from "../database/index.js";
import { ownLandedForcedOrphan } from "../database/l1-admission-identity.js";
import { sqlErrorToDatabaseError } from "../database/utils/common.js";
import { outRefLabel } from "./derive.js";

/** The `/readyz` degradation detail (`l1_own_block_forced_order_orphaned:<count>`). */
export const L1_OWN_BLOCK_FORCED_ORDER_ORPHANED =
  "l1_own_block_forced_order_orphaned";

/** One forced row of a landed own block whose order left the chain. */
export type OwnLandedForcedOrphan = Readonly<{
  headerHash: string;
  /** The order's outref, `txHash#index`. */
  order: string;
}>;

/** The forced rows of landed own blocks whose order left the chain. */
export const readOwnLandedForcedOrphans = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    projected_header_hash: Buffer;
    tx_order_l1_tx_hash: Buffer;
    tx_order_l1_output_index: number;
  }>`SELECT t.projected_header_hash, t.tx_order_l1_tx_hash,
      t.tx_order_l1_output_index
    FROM ${sql(ForcedTransactionsDB.tableName)} t
    WHERE ${ownLandedForcedOrphan(sql, "t")}
    ORDER BY t.projected_header_hash, t.tx_order_l1_tx_hash,
      t.tx_order_l1_output_index`;
  return rows.map(
    (row): OwnLandedForcedOrphan => ({
      headerHash: row.projected_header_hash.toString("hex"),
      order: outRefLabel({
        txHash: Buffer.from(row.tx_order_l1_tx_hash),
        index: Number(row.tx_order_l1_output_index),
      }),
    }),
  );
}).pipe(
  sqlErrorToDatabaseError(
    ForcedTransactionsDB.tableName,
    "Failed to read the forced rows of landed own blocks whose order left the chain",
  ),
);
