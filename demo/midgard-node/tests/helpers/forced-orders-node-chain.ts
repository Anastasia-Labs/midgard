/**
 * A simulated chain followed into the node database with the event and
 * forced-order projections, and the node's forced rows read back, for the
 * forced-order hook tests (N10b).
 */
import { type FactStore } from "@al-ft/midgard-l1-follower";
import { type SimTx, simTxHash } from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, beforeEach } from "vitest";

import { countOrphanedAdmissions } from "../../src/database/follower-events.js";
import { ForcedTransactionsDB } from "../../src/database/index.js";
import {
  forcedOrderHorizon,
  forcedOrderProjection,
  forcedOrderTrackedSet,
} from "../../src/forced-orders/index.js";
import { eventProjection } from "../../src/l1-events/index.js";
import { resetApplicationTables } from "../utils.js";
import {
  FORCED_CONFIG,
  inlineOrderMaterial,
  nativeTransactionCbor,
  orderTx,
} from "./forced-orders-chain.js";
import { db, openNodeFollowerStore } from "./forced-orders-node-store.js";
import { EVENTS_CONFIG } from "./l1-events-chain.js";
import { ChainDriver } from "./l1-events-store.js";

export const K = 4;
export const INCLUSION = 5_000n;

/** Resets the node tables before each test and closes its stores after it. */
export const nodeFollowerLifecycle = (): (() => Promise<{
  store: FactStore;
  chain: ChainDriver;
}>) => {
  const opened: FactStore[] = [];
  beforeEach(async () => {
    await db(resetApplicationTables);
  });
  afterEach(async () => {
    await Promise.all(opened.splice(0).map((store) => store.close()));
  });
  return async () => {
    const store = await openNodeFollowerStore(
      [eventProjection(EVENTS_CONFIG), forcedOrderProjection(FORCED_CONFIG)],
      K,
    );
    opened.push(store);
    const chain = new ChainDriver(store, forcedOrderTrackedSet(FORCED_CONFIG));
    await chain.init();
    return { store, chain };
  };
};

/** An order for `submitted`, every field inline in its redeemer. */
export const inlineOrder = (
  chain: ChainDriver["chain"],
  submitted: Buffer,
  inclusionTime = INCLUSION,
): SimTx => {
  const { material, inline } = inlineOrderMaterial(submitted);
  return orderTx({
    material,
    nonceInput: chain.outsideInput(),
    carriage: [],
    inline,
    inclusionTime,
    nonce: chain.nonce(),
  });
};

export const honest = () => nativeTransactionCbor([0x11]);

/** Every column of the node's forced rows that names or carries the order. */
export const rows = () =>
  db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        tx_order_id: Buffer;
        tx_order_l1_tx_hash: Buffer;
        tx_order_l1_output_index: number;
        raw_datum: Buffer;
        tx_id: Buffer;
        native_tx_cbor: Buffer;
        forced_inclusion_value: Buffer;
        transaction_commitment: Buffer;
        inclusion_time: Date;
        projected_header_hash: Buffer | null;
        status: string;
      }>`SELECT tx_order_id, tx_order_l1_tx_hash, tx_order_l1_output_index,
          raw_datum, tx_id, native_tx_cbor, forced_inclusion_value,
          transaction_commitment, inclusion_time, projected_header_hash, status
        FROM ${sql(ForcedTransactionsDB.tableName)}
        ORDER BY tx_order_l1_tx_hash`;
    }),
  );

export const horizon = () => db(forcedOrderHorizon);
export const orphans = () => db(countOrphanedAdmissions);

export const label = (order: SimTx) => `${simTxHash(order).toString("hex")}#0`;
