/**
 * Forced rows for an order the emulator's chain does not carry: each row is
 * written with the order row the node ingested it from, in the follower's
 * tip block.
 */
import { SqlClient } from "@effect/sql";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { ForcedTransactionsDB } from "../../src/database/index.js";
import { DatabaseError } from "../../src/database/utils/common.js";
import { FORCED_ORDERS_TABLE } from "../../src/forced-orders/schema.js";
import { syncEmulatorChain } from "./emulator-l1-follower.js";

const failed = (message: string, cause?: unknown) =>
  new DatabaseError({ table: FORCED_ORDERS_TABLE, message, cause });

/**
 * Writes forced rows as the forced-order ingestion writes them: each with
 * the follower's projection row (`node_l1_forced_order_fields`) of the order
 * it was rebuilt from, landed in the follower's tip block once the follower
 * is at `lucid`'s emulator tip. The node treats a forced row without a
 * header whose order is gone as left the chain (N10b): it bounds the commit
 * horizon below the row's inclusion time and is never selected. The order
 * row carries what the node reads of it (outref, slot, unspent, inclusion
 * time); the order's carriage is not modelled, and the order row alone
 * backs the forced row (`canonicalForcedAdmission`).
 */
export const insertForcedEntriesWithOrders = (
  entries: readonly ForcedTransactionsDB.Entry[],
  lucid: LucidEvolution,
) =>
  Effect.gen(function* () {
    yield* syncEmulatorChain(lucid);
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const [tip] = yield* sql<{
          slot: string;
          hash: Buffer;
          height: string;
          parent_hash: Buffer | null;
          parent_slot: string | null;
        }>`SELECT c.slot::text AS slot, c.hash, c.height::text AS height,
            b.parent_hash, p.slot::text AS parent_slot
          FROM l1_follower_cursor c
          LEFT JOIN l1_blocks b ON b.hash = c.hash
          LEFT JOIN l1_blocks p ON p.hash = b.parent_hash`;
        if (tip?.parent_hash == null || tip.parent_slot === null)
          return yield* failed("The follower's tip has no stored parent");
        for (const entry of entries)
          yield* sql`INSERT INTO ${sql(FORCED_ORDERS_TABLE)} ${sql.insert({
            order_tx_hash:
              entry[ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH],
            order_output_index:
              entry[ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX],
            order_tx_index: 0,
            block_hash: tip.hash,
            height: Number(tip.height),
            order_slot: Number(tip.slot),
            spent_slot: null,
            parent_slot: Number(tip.parent_slot),
            parent_hash: tip.parent_hash,
            inclusion_time:
              entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime(),
            status: "resolved",
            reference_inputs: Buffer.alloc(0),
            block_datums: "{}",
          })}`;
        yield* ForcedTransactionsDB.insertEntries(entries);
      }),
    );
  }).pipe(
    Effect.catchTag("SqlError", (cause) =>
      Effect.fail(failed("Forced order rows could not be written", cause)),
    ),
  );
