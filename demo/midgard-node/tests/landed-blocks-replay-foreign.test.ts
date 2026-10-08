/**
 * The production foreign replayer (`replayForeignBlock`, plan §7.3, N3) on
 * a real Postgres follower store and the node's DA table: a block's event
 * sets must equal the in-window events and forced orders the follower
 * facts hold at the view, in both polarities (honest blocks replay; a known
 * event outside the window is invalid, an unknown one a wait); a
 * block past what the view can know waits; the retained payload's identity
 * is checked; a fetched payload is kept only after the block replayed.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Clock, Effect, Option } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { FOREIGN_DA_RETRY_MAX_MS } from "../src/da/foreign-retained-da.js";
import { DaPayloadsDB } from "../src/database/index.js";
import { FORCED_ORDERS_TABLE } from "../src/forced-orders/index.js";
import { nodeLandedBlockPorts } from "../src/landed-blocks/node-ports.js";
import { sha256 } from "../src/sha256.js";
import {
  FORCED_CONFIG,
  inlineOrderMaterial,
  nativeTransactionCbor,
  orderTx,
} from "./helpers/forced-orders-chain.js";
import {
  db,
  forcedRows,
  ingestionHook,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
  retirementTx,
} from "./helpers/l1-events-chain.js";
import type { ChainDriver } from "./helpers/l1-events-store.js";
import { SIM_QUEUE_CONFIG } from "./helpers/state-queue-sim.fixtures.js";
import {
  clock,
  depositBlock,
  depositsAt,
  emptyBlock,
  FIXTURE_ORDER_ID,
  fixtureOrderTx,
  followChain,
  forcedBlock,
  idKey,
  inNode,
  inputFor,
  K,
  naming,
  nonceRef,
  repeating,
  replay,
  replayerFor,
  retain,
  retained,
  retainedRow,
  unownedHistory,
} from "./landed-blocks-replay-foreign.fixture.js";
import { serveDa } from "./landed-blocks-replay-foreign.transport.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];
const follow = async (...args: Parameters<typeof followChain>) => {
  const followed = await followChain(...args);
  opened.push(followed.store);
  return followed;
};

beforeEach(async () => {
  clock.offsetMs = 0;
  unownedHistory();
  await db(resetApplicationTables);
});
afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/** Drops the node's retained copy of `payload`. */
const forget = (payload: SDK.DaPayload) =>
  db(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql`DELETE FROM ${sql(DaPayloadsDB.tableName)}
        WHERE header_hash = ${Buffer.from(payload.block_body.header_hash, "hex")}`,
    ),
  );

/** Retains `payload` and replays it at the chain's view. */
const replayed = async (store: FactStore, payload: SDK.DaPayload) => {
  await retain(await retainedRow(payload));
  return replay(store, payload);
};

const admitDeposit = async (chain: ChainDriver, inclusionTime = 2n) => {
  const order = eventOrder("deposit", nonceRef(), { inclusionTime });
  const [hash] = await chain.forward([admissionTx(order, chain.chain.nonce())]);
  return { order, outRef: { txHash: hash!, index: 0 } };
};

const admitWithdrawal = async (chain: ChainDriver) => {
  const order = eventOrder("withdrawal", nonceRef(), { inclusionTime: 2n });
  await chain.forward([admissionTx(order, chain.chain.nonce())]);
  return order;
};

/** An order for another forced transaction (its own tx id), in the window. */
const otherOrderTx = (chain: ChainDriver, fill: number) => {
  const { material, inline } = inlineOrderMaterial(
    nativeTransactionCbor([fill]),
  );
  return orderTx({
    material,
    nonceInput: nonceRef(),
    carriage: [],
    inline,
    inclusionTime: 2n,
    nonce: chain.chain.nonce(),
  });
};

describe("foreign replay: deposits", () => {
  it("replays a block naming exactly the in-window deposit; one outside the window is not owed", async () => {
    const { store, chain } = await follow();
    await admitDeposit(chain, 5n);
    expect(await replayed(store, await emptyBlock())).toMatchObject({
      kind: "replayed",
    });
    await admitDeposit(chain);
    const [inWindow] = (await depositsAt(store)).filter(
      ({ event }) => event.inclusionTime === 2n,
    );
    const block = await depositBlock(inWindow!.entry);
    const outcome = await replayed(store, block);
    expect(outcome).toMatchObject({
      kind: "replayed",
      root: block.block_body.header.utxosRoot,
      depositIds: [Buffer.from(inWindow!.entry.idCbor, "hex")],
    });
  });

  it("refuses a block that leaves out an in-window deposit", async () => {
    const { store, chain } = await follow();
    await admitDeposit(chain);
    const outcome = await replayed(store, await emptyBlock());
    expect(outcome.kind).toBe("invalid");
    expect(outcome).toMatchObject({
      detail: expect.stringContaining("leaves out in-window deposit"),
    });
  });

  it("refuses a block naming a deposit twice", async () => {
    const { store, chain } = await follow();
    await admitDeposit(chain);
    const [{ entry }] = (await depositsAt(store)) as [
      Awaited<ReturnType<typeof depositsAt>>[number],
    ];
    const block = await depositBlock(entry);
    const twice = repeating(block, "deposits");
    expect(await replayed(store, twice)).toMatchObject({
      kind: "invalid",
      detail: "the block names a deposit twice",
    });
  });

  it("refuses a block naming a deposit the view knows outside the block's window", async () => {
    const { store, chain } = await follow();
    await admitDeposit(chain, 5n);
    const [{ entry }] = (await depositsAt(store)) as [
      Awaited<ReturnType<typeof depositsAt>>[number],
    ];
    const outside = await naming(await emptyBlock(), {
      deposits: [[entry.idCbor, entry.infoCbor]],
    });
    expect(await replayed(store, outside)).toMatchObject({
      kind: "invalid",
      detail: `deposit ${entry.idCbor} is outside the block's window`,
    });
  });

  it("waits on a deposit the view does not know (event_unknown, not awaiting DA)", async () => {
    const { store, chain } = await follow();
    await admitDeposit(chain);
    const [{ entry }] = (await depositsAt(store)) as [
      Awaited<ReturnType<typeof depositsAt>>[number],
    ];
    // The same block replayed at a view that has not seen the admission.
    await chain.backward(1);
    expect(await replayed(store, await depositBlock(entry))).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(`deposit ${entry.idCbor}`),
    });
  });
});

describe("foreign replay: withdrawals", () => {
  it("refuses a block that leaves out an in-window withdrawal", async () => {
    const { store, chain } = await follow();
    await admitWithdrawal(chain);
    expect(await replayed(store, await emptyBlock())).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("leaves out in-window withdrawal"),
    });
  });

  it("refuses a duplicated withdrawal and waits on an unknown one", async () => {
    const { store, chain } = await follow();
    const order = await admitWithdrawal(chain);
    const named = idKey(order.nonce);
    const twice = repeating(
      await naming(await emptyBlock(), { withdrawals: [[named, "00"]] }),
      "withdrawals",
    );
    expect(await replayed(store, twice)).toMatchObject({
      kind: "invalid",
      detail: "the block names a withdrawal twice",
    });
    const unknown = idKey(nonceRef());
    const stranger = await naming(await emptyBlock(), {
      withdrawals: [
        [named, "00"],
        [unknown, "00"],
      ],
    });
    expect(await replayed(store, stranger)).toMatchObject({
      kind: "event_unknown",
      detail: `withdrawal ${unknown} is not known at the view`,
    });
  });
});

describe("foreign replay: forced orders read from the follower facts", () => {
  it("replays the honest forced block against the admitted order", async () => {
    const { store, chain } = await follow();
    await chain.forward([await fixtureOrderTx(chain, FIXTURE_ORDER_ID)]);
    const block = await forcedBlock();
    expect(await replayed(store, block)).toMatchObject({
      kind: "replayed",
      forcedIds: [Buffer.from(idKey(FIXTURE_ORDER_ID), "hex")],
    });
  });

  it("an in-window order with no ingested node row still must be named", async () => {
    const { store, chain } = await follow();
    await chain.forward([
      await fixtureOrderTx(chain, FIXTURE_ORDER_ID),
      otherOrderTx(chain, 0x21),
    ]);
    // Nothing was ingested into the node's forced-transaction table.
    expect(await forcedRows()).toEqual([]);
    expect(await replayed(store, await forcedBlock())).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("leaves out in-window forced order"),
    });
  });

  it("an ingested node row whose order a rollback removed is not owed", async () => {
    const { store, chain } = await follow();
    await chain.forward([await fixtureOrderTx(chain, FIXTURE_ORDER_ID)]);
    await chain.forward([otherOrderTx(chain, 0x22)]);
    const { hook } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await forcedRows()).toHaveLength(2);
    await chain.backward(1);
    // The node row stays until ingestion catches up; the facts decide.
    expect(await forcedRows()).toHaveLength(2);
    expect(await replayed(store, await forcedBlock())).toMatchObject({
      kind: "replayed",
    });
  });

  it("refuses a duplicated order and waits on an unknown one", async () => {
    const { store, chain } = await follow();
    const block = await forcedBlock();
    expect(await replayed(store, block)).toMatchObject({
      kind: "event_unknown",
      detail: `forced order ${idKey(FIXTURE_ORDER_ID)} is not known at the view`,
    });
    await chain.forward([await fixtureOrderTx(chain, FIXTURE_ORDER_ID)]);
    await forget(block);
    expect(
      await replayed(store, repeating(block, "forced_transactions")),
    ).toMatchObject({
      kind: "invalid",
      detail: "the block names a forced order twice",
    });
  });

  it("an admitted in-window order whose output cannot be read back is a wait", async () => {
    const { store, chain } = await follow();
    const order = await fixtureOrderTx(chain, FIXTURE_ORDER_ID);
    const [hash] = await chain.forward([order]);
    await store.transaction("write", (tx) =>
      tx.query("DELETE FROM l1_outputs WHERE tx_hash = ?", [hash!]),
    );
    expect(await replayed(store, await forcedBlock())).toMatchObject({
      kind: "forced_order_pending",
    });
    const rows = await store.transaction("read", (tx) =>
      tx.query(`SELECT 1 AS one FROM ${FORCED_ORDERS_TABLE}`),
    );
    expect(rows).toHaveLength(1);
  });
});

describe("foreign replay: the view's horizon and the DA payload", () => {
  it("waits on a block that ends past what the view can know, and replays it once the view reaches it", async () => {
    const { store } = await follow();
    const block = await emptyBlock();
    const view = (await store.currentView())!;
    // The view's time plus the event wait ends just before the block's end.
    clock.offsetMs =
      Number(block.block_body.header.endTime) -
      view.point.slot * 1_000 -
      SDK.EVENT_WAIT_DURATION_MS;
    expect(await replayed(store, block)).toEqual({
      kind: "missing",
      detail: "the block ends past what the follower view can know",
    });
    clock.offsetMs += 1;
    expect(await replay(store, block)).toMatchObject({ kind: "replayed" });
  });

  it("deletes a retained payload that no longer verifies and waits on its refetch", async () => {
    const { store } = await follow();
    const block = await emptyBlock();
    const time = { ms: 1_000_000 };
    const clockAt: Clock.Clock = {
      [Clock.ClockTypeId]: Clock.ClockTypeId,
      unsafeCurrentTimeMillis: () => time.ms,
      currentTimeMillis: Effect.sync(() => time.ms),
      unsafeCurrentTimeNanos: () => BigInt(time.ms) * 1_000_000n,
      currentTimeNanos: Effect.sync(() => BigInt(time.ms) * 1_000_000n),
      sleep: () => Effect.void,
    };
    // One replayer across runs, as the follower keeps one.
    const replayer = replayerFor(store);
    const run = async (payload: SDK.DaPayload) =>
      inNode(
        replayer(await inputFor(store, payload)).pipe(
          Effect.withClock(clockAt),
        ),
      );
    await retain(
      await retainedRow(block, (row) => ({
        ...row,
        payload_sha256: sha256(Buffer.from("other")),
      })),
    );
    // No peer serves it: the row is gone, and the wait is named as a refetch.
    expect(await run(block)).toMatchObject({
      kind: "da_refetch_pending",
      detail: expect.stringContaining(
        "its stored digest or identity does not verify",
      ),
    });
    expect(Option.isNone(await retained(block))).toBe(true);
    // A later run still owes the refetch, so it still says so.
    expect(await run(block)).toMatchObject({ kind: "da_refetch_pending" });
    // Served once its backoff is over: refetched, replayed and retained.
    time.ms += FOREIGN_DA_RETRY_MAX_MS;
    serveDa(() => block);
    expect(await run(block)).toMatchObject({ kind: "replayed" });
    expect(Option.isSome(await retained(block))).toBe(true);
    // Another block's payload under this header hash, digest intact: its
    // body is not the block's, so it is deleted too and the block's own
    // payload fetched. The identity check comes first, so its forced order
    // is never waited on.
    await forget(block);
    const elsewhere = await forcedBlock();
    await retain(
      await retainedRow(elsewhere, (row) => ({
        ...row,
        header_hash: Buffer.from(block.block_body.header_hash, "hex"),
      })),
    );
    expect(await replay(store, block)).toMatchObject({ kind: "replayed" });
    const kept = await retained(block);
    expect(Option.isSome(kept) && kept.value.payload_cbor).toEqual(
      (await retainedRow(block)).payload_cbor,
    );
  });

  it("names a plain fetch wait missing when no retained row was deleted", async () => {
    const { store } = await follow();
    const block = await emptyBlock();
    expect(await replay(store, block)).toMatchObject({ kind: "missing" });
  });

  it("keeps a fetched payload only once its block replayed", async () => {
    const { store, chain } = await follow();
    const block = await emptyBlock();
    serveDa(() => undefined);
    expect(await replay(store, block)).toMatchObject({ kind: "missing" });
    expect(Option.isNone(await retained(block))).toBe(true);
    await admitDeposit(chain);
    serveDa(() => block);
    expect(await replay(store, block)).toMatchObject({ kind: "invalid" });
    expect(Option.isNone(await retained(block))).toBe(true);
    await chain.backward(1);
    expect(await replay(store, block)).toMatchObject({ kind: "replayed" });
    expect(Option.isSome(await retained(block))).toBe(true);
  });
});

describe("foreign replay: retired events under the landed prune floor", () => {
  it("replays a block naming a deposit retired more than k blocks ago while the floor holds it", async () => {
    const floor: { slot: number | null } = { slot: null };
    const { store, chain } = await follow({
      pruneFloors: [{ name: "test", floor: () => Promise.resolve(floor.slot) }],
    });
    const admittedFrom = chain.tip.point.slot;
    const { order, outRef } = await admitDeposit(chain);
    const [{ entry }] = (await depositsAt(store)) as [
      Awaited<ReturnType<typeof depositsAt>>[number],
    ];
    await chain.forward([
      retirementTx(order, outRef, "absorbed", chain.chain.nonce()),
    ]);
    for (let block = 0; block < K + 3; block++) await chain.forward([]);
    const block = await depositBlock(entry);
    floor.slot = admittedFrom;
    await store.prune();
    expect(await replayed(store, block)).toMatchObject({ kind: "replayed" });
    floor.slot = null;
    await store.prune();
    expect(await replay(store, block)).toMatchObject({
      kind: "event_unknown",
    });
  });
});

describe("foreign replay reads no node event table", () => {
  it("is independent of the node's deposit rows", async () => {
    const { store, chain } = await follow();
    await admitDeposit(chain);
    const rows = await db(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql`SELECT 1 AS one FROM deposits_utxos`,
      ),
    );
    expect(rows).toEqual([]);
    const [{ entry }] = (await depositsAt(store)) as [
      Awaited<ReturnType<typeof depositsAt>>[number],
    ];
    expect(await replayed(store, await depositBlock(entry))).toMatchObject({
      kind: "replayed",
    });
  });
});

describe("the node's landed ports at a follower view", () => {
  it("confirm a view, and refuse it and its queue history once the follower left it", async () => {
    const { store, chain } = await follow();
    const ports = nodeLandedBlockPorts(
      store,
      {
        projection: EVENTS_CONFIG,
        forcedOrders: FORCED_CONFIG,
        stateQueue: SIM_QUEUE_CONFIG,
      },
      // Not reached: no landed row asks for a rebase here.
      () => Effect.succeed(undefined),
    );
    await admitDeposit(chain);
    const before = (await store.currentView())!;
    expect(await inNode(ports.confirmView(before))).toBe(true);
    expect(await inNode(ports.queueHistory(before))).toEqual([]);
    await chain.backward(1);
    const after = (await store.currentView())!;
    expect(await inNode(ports.confirmView(before))).toBe(false);
    await expect(inNode(ports.queueHistory(before))).rejects.toThrow();
    expect(await inNode(ports.confirmView(after))).toBe(true);
  });
});
