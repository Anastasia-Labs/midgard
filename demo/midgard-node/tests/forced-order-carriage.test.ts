/**
 * Forced-order carriage resolution (N10, plan §12.3) on a simulated chain,
 * with the follower store in the node database as in production: the
 * projection resolves carriage its own block created (step 1), the driver
 * hook resolves the rest through the ledger at the order block's parent
 * (step 2), the ledger at the tip (step 3) and content sources whose answers
 * must hash to the requested id (step 4), and writes the rebuilt order into
 * `forced_transaction_utxos`. What resolves nowhere keeps the node unready
 * by name and is retried; it never exits and never writes a verdict.
 */
import {
  type FactStore,
  type OutRef,
  type TxContentSource,
} from "@al-ft/midgard-l1-follower";
import { eventProjection } from "@al-ft/midgard-l1-follower/events";
import {
  encodeTxBody,
  type SimTx,
  simTxHash,
} from "@al-ft/midgard-l1-follower/testing";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  FORCED_ORDER_CARRIAGE_PENDING,
  FORCED_ORDER_INGESTION_FAILED,
  FORCED_ORDERS_TABLE,
  forcedOrderProjection,
  forcedOrderTrackedSet,
  NO_CONTENT_SOURCE,
} from "../src/forced-orders/index.js";
import {
  createFollowerDriver,
  type FollowerEventSink,
} from "../src/l1-events/driver.js";
import { failureHold } from "../src/services/l1-follower.failure-hold.js";
import {
  FORCED_CONFIG,
  nativeTransactionCbor,
  orderTx,
  publicationTx,
  publishedOrderMaterial,
  simContentSource,
  simLedger,
  WALLET,
} from "./helpers/forced-orders-chain.js";
import {
  db,
  forcedRows,
  ingestionHook,
  openNodeFollowerStore,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import { EVENTS_CONFIG } from "./helpers/l1-events-chain.js";
import { ChainDriver } from "./helpers/l1-events-store.js";
import { resetApplicationTables } from "./utils.js";

const K = 4;

const opened: FactStore[] = [];
beforeEach(async () => {
  await db(resetApplicationTables);
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

const follow = async () => {
  const store = await openNodeFollowerStore(
    [eventProjection(EVENTS_CONFIG), forcedOrderProjection(FORCED_CONFIG)],
    K,
  );
  opened.push(store);
  const chain = new ChainDriver(store, forcedOrderTrackedSet(FORCED_CONFIG));
  await chain.init();
  const ledger = simLedger(chain, K);
  const forward = async (txs: readonly SimTx[]) => {
    await chain.forward(txs);
    ledger.record();
  };
  return { store, chain, ledger, forward };
};

/** A tier-2 order and the publication of its one carried field. */
const fixture = (chain: ChainDriver["chain"], tamper = false) => {
  const submitted = nativeTransactionCbor([0x11, 0x22]);
  const { material, preimage } = publishedOrderMaterial(submitted);
  const published = Buffer.from(preimage);
  // Same length, last byte changed: only the field commitment can refuse it.
  if (tamper) published[published.length - 1] ^= 0xff;
  const publication = publicationTx(
    published,
    chain.outsideInput(),
    chain.nonce(),
  );
  const carriage: OutRef = { txHash: simTxHash(publication), index: 0 };
  const order = orderTx({
    material,
    nonceInput: chain.outsideInput(),
    carriage: [carriage],
    otherReferences: [chain.outsideInput()],
    inclusionTime: 5_000n,
    nonce: chain.nonce(),
  });
  const spend: SimTx = {
    inputs: [carriage],
    outputs: [{ address: WALLET, lovelace: 1_000_000n }],
    nonce: chain.nonce(),
  };
  return { submitted, publication, carriage, order, spend };
};

const hookWith = (
  store: FactStore,
  options: Parameters<typeof ingestionHook>[2] = {},
) => ingestionHook(store, FORCED_CONFIG, options);

const orderRows = (store: FactStore) =>
  store.transaction("read", (tx) =>
    tx.query(`SELECT status, detail FROM ${FORCED_ORDERS_TABLE}`),
  );

const expectIngested = async (submitted: Buffer, order: SimTx) => {
  const rows = await forcedRows();
  expect(rows).toHaveLength(1);
  expect(Buffer.from(rows[0]!.native_tx_cbor)).toEqual(submitted);
  expect(Buffer.from(rows[0]!.tx_order_l1_tx_hash)).toEqual(simTxHash(order));
  expect(rows[0]!.status).toBe(ForcedTransactionsDB.Status.Awaiting);
};

describe("forced-order carriage resolution (§12.3)", () => {
  it("step 1: carriage an earlier tx of the order's own block created resolves in the projection", async () => {
    const { store, chain, forward } = await follow();
    const f = fixture(chain.chain);
    await forward([f.publication, f.order]);
    expect(await orderRows(store)).toEqual([
      { status: "resolved", detail: null },
    ]);
    // No ledger and no source: the projection's own preimages suffice.
    const { hook } = hookWith(store);
    expect(await hook(UNCHANGED)).toBeUndefined();
    await expectIngested(f.submitted, f.order);
  });

  it("step 2: carriage spent in the next block resolves through the ledger at the order block's parent", async () => {
    const { store, chain, ledger, forward } = await follow();
    const f = fixture(chain.chain);
    await forward([f.publication]);
    const parent = chain.tip.point.slot;
    await forward([f.order]);
    await forward([f.spend]);
    expect(await orderRows(store)).toEqual([
      { status: "carriage_pending", detail: null },
    ]);
    const { hook, logs } = hookWith(store, { ledger: ledger.ledger });
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(ledger.calls).toEqual([parent.toString()]);
    expect(logs.join("\n")).toMatch(/resolved \(ledger_at_parent\)/u);
    await expectIngested(f.submitted, f.order);
  });

  it("step 3: a parent point older than k with the carriage unspent resolves through the ledger at the tip", async () => {
    const { store, chain, ledger, forward } = await follow();
    const f = fixture(chain.chain);
    await forward([f.publication]);
    const parent = chain.tip.point.slot;
    await forward([f.order]);
    for (let i = 0; i <= K; i += 1) await forward([]);
    const { hook, logs } = hookWith(store, { ledger: ledger.ledger });
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(ledger.calls).toEqual([parent.toString(), "tip"]);
    expect(logs.join("\n")).toMatch(/resolved \(ledger_at_tip\)/u);
    await expectIngested(f.submitted, f.order);
  });

  it("step 4: spent and older than k holds the node unready by name, never exits or writes a verdict, and resolves once a source supplies the creating tx", async () => {
    const { store, chain, ledger, forward } = await follow();
    const f = fixture(chain.chain);
    await forward([f.publication]);
    await forward([f.order]);
    await forward([f.spend]);
    for (let i = 0; i <= K; i += 1) await forward([]);
    const sources: TxContentSource[] = [];
    const { hook, logs } = hookWith(store, { ledger: ledger.ledger, sources });
    const sink: FollowerEventSink = {
      apply: () =>
        Promise.resolve({
          kind: "applied",
          inserted: 0,
          orphans: 0,
          refused: [],
        }),
    };
    const driver = createFollowerDriver({
      failureHold,
      store,
      config: EVENTS_CONFIG,
      sink,
      hooks: { forcedOrderIngestion: hook },
    });
    for (let attempt = 0; attempt < 2; attempt += 1) {
      const run = await driver.run();
      expect(run.kind).toBe("ran");
      expect(driver.holds()).toEqual([
        {
          reason: FORCED_ORDER_CARRIAGE_PENDING,
          detail: expect.stringContaining(
            `${f.carriage.txHash.toString("hex")}#0`,
          ),
        },
      ]);
      expect(await forcedRows()).toEqual([]);
    }
    sources.push(simContentSource("indexer", [f.publication]));
    await driver.run();
    expect(driver.holds()).toEqual([]);
    expect(logs.join("\n")).toMatch(/resolved \(content indexer\)/u);
    await expectIngested(f.submitted, f.order);
  });

  it("leads a pending carriage with the missing content source only when none is configured", async () => {
    const { store, chain, ledger, forward } = await follow();
    const f = fixture(chain.chain);
    await forward([f.publication]);
    await forward([f.order]);
    await forward([f.spend]);
    for (let i = 0; i <= K; i += 1) await forward([]);
    const awaited = `${f.carriage.txHash.toString("hex")}#0`;
    const unset = await hookWith(store, {
      ledger: ledger.ledger,
      contentSourcesConfigured: false,
    }).hook(UNCHANGED);
    expect(unset?.reason).toBe(FORCED_ORDER_CARRIAGE_PENDING);
    expect(unset?.detail.startsWith(`${NO_CONTENT_SOURCE} | `)).toBe(true);
    expect(unset?.detail).toContain(awaited);
    const configured = await hookWith(store, {
      ledger: ledger.ledger,
      sources: [simContentSource("indexer", [])],
      contentSourcesConfigured: true,
    }).hook(UNCHANGED);
    expect(configured?.reason).toBe(FORCED_ORDER_CARRIAGE_PENDING);
    expect(configured?.detail).toContain(awaited);
    expect(configured?.detail).not.toContain(NO_CONTENT_SOURCE);
    expect(await forcedRows()).toEqual([]);
  });

  it("refuses a source whose bytes do not hash to the requested tx id", async () => {
    const { store, chain, forward } = await follow();
    const f = fixture(chain.chain);
    await forward([f.publication]);
    await forward([f.order]);
    // The same output under another body: the id it claims is not its hash.
    const forged = simContentSource("forger", [f.publication], (tx) =>
      encodeTxBody({ ...tx, nonce: tx.nonce + 1 }),
    );
    const { hook } = hookWith(store, { sources: [forged] });
    const hold = await hook(UNCHANGED);
    expect(forged.asked).toEqual([f.carriage.txHash.toString("hex")]);
    expect(hold).toMatchObject({ reason: FORCED_ORDER_CARRIAGE_PENDING });
    expect(hold?.detail).toMatch(/forger/u);
    expect(await forcedRows()).toEqual([]);
  });

  it("holds ingestion failed, with no row, when the carriage does not open the field commitment", async () => {
    const { store, chain, forward } = await follow();
    const f = fixture(chain.chain, true);
    await forward([f.publication, f.order]);
    expect(await orderRows(store)).toEqual([
      {
        status: "malformed",
        detail: expect.stringMatching(
          /field preimage does not match the committed field hash/u,
        ),
      },
    ]);
    const { hook } = hookWith(store);
    expect(await hook(UNCHANGED)).toMatchObject({
      reason: FORCED_ORDER_INGESTION_FAILED,
    });
    expect(await forcedRows()).toEqual([]);
  });

  it("rolling back the order block truncates its row", async () => {
    const { store, chain, forward } = await follow();
    const f = fixture(chain.chain);
    await forward([f.publication]);
    await forward([f.order]);
    expect(await orderRows(store)).toHaveLength(1);
    await chain.backward(1);
    expect(await orderRows(store)).toEqual([]);
    const { hook } = hookWith(store);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await forcedRows()).toEqual([]);
  });
});
