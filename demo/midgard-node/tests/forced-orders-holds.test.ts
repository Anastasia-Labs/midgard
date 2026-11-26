/**
 * The forced-order hook on a simulated chain with the follower store in the
 * node database as in production. An order is ingested whatever its
 * transaction's items: the row is read from the §5.1 envelope the tx-order
 * mint authenticated, so an order whose outputs or native scripts do not
 * decode gets a row (classification rejects it) and never holds the commit
 * horizon. Only an order that cannot be rebuilt holds
 * `forced_order_ingestion_failed` (forced-order-carriage.test.ts). Before
 * the node installs its ledger no order gets a row; on a node started from
 * a snapshot, an order the anchor's block or an earlier one included gets
 * none.
 */
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec";
import { simTxHash } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import {
  MINT_ADMITTED_UNDECODABLE,
  MINT_REFUSED,
} from "./forced-mint-boundary.fixtures.js";
import {
  FORCED_CONFIG,
  nativeTransactionCbor,
} from "./helpers/forced-orders-chain.js";
import {
  honest,
  horizon,
  INCLUSION,
  inlineOrder,
  nodeFollowerLifecycle,
  rows,
} from "./helpers/forced-orders-node-chain.js";
import {
  db,
  ingestionHook,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import { installEmptyLedger } from "./helpers/ledger-install.js";

const follow = nodeFollowerLifecycle();

describe("forced-order hook ingests orders whose items do not decode", () => {
  it.each(MINT_ADMITTED_UNDECODABLE)(
    "an order with %s is ingested with no hold",
    async (_name, _reason, bytes) => {
      const { store, chain } = await follow();
      const order = inlineOrder(chain.chain, bytes());
      await chain.forward([order]);
      const { hook } = ingestionHook(store, FORCED_CONFIG);
      expect(await hook(UNCHANGED)).toBeUndefined();
      const ingested = await rows();
      expect(ingested.map((row) => Buffer.from(row.native_tx_cbor))).toEqual([
        bytes(),
      ]);
      expect(Buffer.from(ingested[0]!.tx_id)).toEqual(
        Buffer.from(
          deriveMidgardForcedTxFaultEvidenceMaterial(bytes()).transactionId,
        ),
      );
      // An ingested order awaits its block without holding the horizon.
      expect(await horizon()).toBeNull();
    },
  );

  it("has no admission screen of its own: every rebuilt order is ingested", async () => {
    // The tx-order mint refuses these transactions, so the chain carries no
    // order for them; the simulated chain does not run the mint. The hook
    // still holds for none of them.
    const { store, chain } = await follow();
    const orders = [
      ...MINT_REFUSED.map(([, , bytes]) => bytes()),
      ...MINT_ADMITTED_UNDECODABLE.map(([, , bytes]) => bytes()),
      honest(),
    ];
    await chain.forward(
      orders.map((bytes, index) =>
        inlineOrder(chain.chain, bytes, 6_000n + BigInt(index)),
      ),
    );
    const { hook } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(
      (await rows())
        .map((row) => Buffer.from(row.native_tx_cbor).toString("hex"))
        .sort(),
    ).toEqual(orders.map((bytes) => bytes.toString("hex")).sort());
    expect(await horizon()).toBeNull();
  });
});

describe("forced-order hook on a node started from a snapshot", () => {
  it("gives no row to an order the snapshot anchor's block or an earlier one included, and it bounds no horizon", async () => {
    const { store, chain } = await follow({ installed: false });
    const atAnchor = inlineOrder(chain.chain, honest(), INCLUSION);
    const after = inlineOrder(
      chain.chain,
      nativeTransactionCbor([0x22]),
      INCLUSION + 1n,
    );
    await chain.forward([atAnchor, after]);
    await db(installEmptyLedger(INCLUSION));
    const { hook } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(
      (await rows()).map((row) => Buffer.from(row.tx_order_l1_tx_hash)),
    ).toEqual([simTxHash(after)]);
    // The order after the anchor has its row; the one at it is no due
    // event, so no later block is held below it.
    expect(await horizon()).toBeNull();
  });
});

describe("forced-order hook before the node's ledger is installed", () => {
  it("gives no order a row while the install is pending, and after it only the orders included after its anchor", async () => {
    const { store, chain } = await follow({ installed: false });
    const atAnchor = inlineOrder(chain.chain, honest(), INCLUSION);
    const after = inlineOrder(
      chain.chain,
      nativeTransactionCbor([0x22]),
      INCLUSION + 1n,
    );
    await chain.forward([atAnchor, after]);
    const { hook } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual([]);
    await db(installEmptyLedger(INCLUSION));
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(
      (await rows()).map((row) => Buffer.from(row.tx_order_l1_tx_hash)),
    ).toEqual([simTxHash(after)]);
  });
});
