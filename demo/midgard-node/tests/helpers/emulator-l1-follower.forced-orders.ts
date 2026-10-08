/**
 * Forced rows for the emulator follower stand-in, which does not follow
 * forced orders: each row is written with the order row the node ingested it
 * from.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { ForcedTransactionsDB } from "../../src/database/index.js";
import { DatabaseError } from "../../src/database/utils/common.js";
import { FORCED_ORDERS_TABLE } from "../../src/forced-orders/schema.js";
import { followerBlockHash } from "./follower-view.js";

const failed = (message: string, cause?: unknown) =>
  new DatabaseError({ table: FORCED_ORDERS_TABLE, message, cause });

/**
 * Writes forced rows as the forced-order ingestion writes them: each with
 * the follower's projection row (`node_l1_forced_order_fields`) of the order
 * it was rebuilt from, landed in the follower block at `slot`. The node
 * treats a forced row without a header whose order is gone as left the chain
 * (N10b): it bounds the commit horizon below the row's inclusion time and is
 * never selected. The order row carries what the node reads of it (outref,
 * slot, unspent, inclusion time); the order's carriage is not modelled.
 * The order's `forced` key is not written: the order is not on the emulator's
 * chain, so `rewindToEmulatorChain` would remove it as rolled back, and the
 * order row alone backs the forced row (`canonicalForcedAdmission`).
 */
export const insertForcedEntriesWithOrders = (
  entries: readonly ForcedTransactionsDB.Entry[],
  slot: number,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        for (const entry of entries)
          yield* sql`INSERT INTO ${sql(FORCED_ORDERS_TABLE)} ${sql.insert({
            order_tx_hash:
              entry[ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH],
            order_output_index:
              entry[ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX],
            order_tx_index: 0,
            block_hash: followerBlockHash(slot),
            height: slot,
            order_slot: slot,
            spent_slot: null,
            parent_slot: slot - 1,
            parent_hash: followerBlockHash(slot - 1),
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
