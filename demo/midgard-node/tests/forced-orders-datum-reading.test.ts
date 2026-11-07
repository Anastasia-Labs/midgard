/**
 * Forced orders read exactly as the chain admits them (N10, N3), on the
 * follower store in the node database. The chain reads an order datum as
 * decoded Data, whatever its encoding, and keys and values it re-serialised,
 * so an order whose datum the SDK would encode otherwise is derived, read,
 * ingested and replayed like the canonical one, under the same id and value;
 * so is an order nested as deep as a transaction can carry. A datum no order
 * cast accepts is recorded unreadable (`readForcedOrder`): it is never due,
 * and a block naming it is invalid.
 */
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CANONICAL_MAX_DEPTHS,
  canonicalDeepPlutusDataCbor,
} from "@al-ft/midgard-test-support/deep-plutus-data";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  FORCED_ORDERS_TABLE,
  forcedOrderHorizon,
  forcedOrdersAdmittedAt,
  forcedOrdersAt,
} from "../src/forced-orders/index.js";
import { FORCED_CONFIG } from "./helpers/forced-orders-chain.js";
import {
  db,
  ingestionHook,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import {
  clock,
  emptyBlock,
  FIXTURE_ORDER_ID,
  fixtureOrderTx,
  followChain,
  forcedBlock,
  idKey,
  replay,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];

beforeEach(async () => {
  clock.offsetMs = 0;
  await db(resetApplicationTables);
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/** A followed chain holding the fixture order, its datum rewritten by `datum`. */
const admitted = async (
  datum: (canonical: string) => string,
  rewrites = true,
) => {
  const { store, chain } = await followChain();
  opened.push(store);
  const tx = await fixtureOrderTx(chain, FIXTURE_ORDER_ID);
  const [output, ...rest] = tx.outputs;
  const canonical = output!.datum!.toString("hex");
  const rewritten = datum(canonical);
  expect(rewritten !== canonical).toBe(rewrites);
  const order: SimTx = {
    ...tx,
    outputs: [{ ...output!, datum: Buffer.from(rewritten, "hex") }, ...rest],
  };
  await chain.forward([order]);
  return { store, datum: rewritten };
};

const ID = idKey(FIXTURE_ORDER_ID);

const orderRows = (store: FactStore) =>
  store.transaction("read", (tx) =>
    tx.query(`SELECT status, detail FROM ${FORCED_ORDERS_TABLE}`),
  );

const viewPoint = async (store: FactStore) =>
  (await store.currentView())!.point;

/** What the admitted-order read gives each order at the view. */
const admittedReads = async (store: FactStore) => {
  const read = await forcedOrdersAdmittedAt(
    store,
    FORCED_CONFIG,
    await viewPoint(store),
  );
  if (read.kind !== "ok") throw new Error(`admitted read: ${read.kind}`);
  return read.orders.map(({ read: order }) =>
    order === null
      ? null
      : { kind: order.kind, id: order.idCbor.toString("hex") },
  );
};

const liveOrders = async (store: FactStore) => {
  const read = await forcedOrdersAt(store, await viewPoint(store));
  if (read.kind !== "ok") throw new Error(`live read: ${read.kind}`);
  return read.orders.map(({ status }) => status);
};

/** Runs the ingestion hook and returns the node's forced rows. */
const ingested = async (store: FactStore) => {
  const { hook } = ingestionHook(store, FORCED_CONFIG);
  expect(await hook(UNCHANGED)).toBeUndefined();
  const C = ForcedTransactionsDB.Columns;
  const rows = await db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{ id: Buffer; value: Buffer; datum: Buffer }>`SELECT
          ${sql(C.TX_ORDER_ID)} AS id, ${sql(C.FORCED_INCLUSION_VALUE)} AS value,
          ${sql(C.RAW_DATUM)} AS datum
        FROM ${sql(ForcedTransactionsDB.tableName)}`;
    }),
  );
  return rows.map((row) => ({
    id: row.id.toString("hex"),
    value: row.value.toString("hex"),
    datum: row.datum.toString("hex"),
  }));
};

/** What ingestion stores for the canonical fixture order, on a fresh node. */
const canonicalIngestion = async () => {
  const { store } = await admitted((d) => d, false);
  const [row, ...more] = await ingested(store);
  expect(more).toEqual([]);
  await Promise.all(opened.splice(0).map((open) => open.close()));
  await db(resetApplicationTables);
  return row!;
};

/** Ingestion stores the order as it stores the canonical one, under the id
 * the block names, with the datum as it landed. */
const expectIngested = async (
  store: FactStore,
  datum: string,
  canonical: Awaited<ReturnType<typeof canonicalIngestion>>,
) => {
  expect(canonical.id).toBe((await blockEntry()).id);
  expect(await ingested(store)).toEqual([{ ...canonical, datum }]);
};

const replayed = async (store: FactStore, block: SDK.DaPayload) => {
  await retain(await retainedRow(block));
  return replay(store, block);
};

/** The fixture block's one forced entry: the id key it names. */
const blockEntry = async () => {
  const [entry] = (await forcedBlock()).block_body.forced_transactions;
  return { id: entry![0] };
};

/** Each of the four sites yields the order, as it yields the canonical one. */
const expectReadEverywhere = async (
  store: FactStore,
  datum: string,
  canonical: Awaited<ReturnType<typeof canonicalIngestion>>,
) => {
  expect(await orderRows(store)).toMatchObject([{ status: "resolved" }]);
  expect(await liveOrders(store)).toEqual(["resolved"]);
  expect(await admittedReads(store)).toEqual([{ kind: "order", id: ID }]);
  await expectIngested(store, datum, canonical);
  expect(await replayed(store, await forcedBlock())).toMatchObject({
    kind: "replayed",
    forcedIds: [Buffer.from(ID, "hex")],
  });
};

const field = (datum: string, path: readonly number[]) =>
  plutusConstrFieldCbor(datum, path);

const ENCODINGS: readonly {
  name: string;
  datum: (canonical: string) => string;
}[] = [
  {
    name: "an overlong inclusion time",
    datum: (d) => replacePlutusConstrFieldCbor(d, [1], "1b0000000000000002"),
  },
  {
    name: "definite-length datum fields",
    datum: (d) => `d87985${[0, 1, 2, 3, 4].map((i) => field(d, [i])).join("")}`,
  },
  {
    name: "an id with definite-length fields and an overlong index",
    datum: (d) =>
      replacePlutusConstrFieldCbor(
        d,
        [0, 0],
        `d87982${field(d, [0, 0, 0])}1a00000000`,
      ),
  },
  {
    name: "a witness in chunks",
    datum: (d) => {
      const witness = field(d, [2]).slice(4);
      return replacePlutusConstrFieldCbor(
        d,
        [2],
        `5f4e${witness.slice(0, 28)}4e${witness.slice(28)}ff`,
      );
    },
  },
  {
    name: "an empty refund datum as an indefinite-length list",
    datum: (d) => replacePlutusConstrFieldCbor(d, [4], "d8799fff"),
  },
  {
    name: "a refund key hash that is not 28 bytes",
    datum: (d) =>
      replacePlutusConstrFieldCbor(d, [3, 0, 0], `581b${"77".repeat(27)}`),
  },
];

describe("a forced order whose datum the chain admits in another encoding", () => {
  describe.each(ENCODINGS)("with $name", ({ datum: rewrite }) => {
    it("is derived as a resolved order", async () => {
      const { store } = await admitted(rewrite);
      expect(await orderRows(store)).toMatchObject([
        { status: "resolved", detail: null },
      ]);
    });

    it("is read as a live order and as an admitted one under its id", async () => {
      const { store } = await admitted(rewrite);
      expect(await liveOrders(store)).toEqual(["resolved"]);
      expect(await admittedReads(store)).toEqual([{ kind: "order", id: ID }]);
    });

    it("is ingested under the id the honest block names, as the canonical order is", async () => {
      const canonical = await canonicalIngestion();
      const { store, datum } = await admitted(rewrite);
      await expectIngested(store, datum, canonical);
    });

    it("replays the honest block naming it", async () => {
      const { store } = await admitted(rewrite);
      expect(await replayed(store, await forcedBlock())).toMatchObject({
        kind: "replayed",
        forcedIds: [Buffer.from(ID, "hex")],
      });
    });
  });
});

describe("decodes every tx order the chain mints a token for, at the deepest nesting a transaction can carry", () => {
  it.each(CANONICAL_MAX_DEPTHS)(
    "an inline refund datum, a $shape $depth deep: derivation, the reads, ingestion and replay each yield the order",
    async (deep) => {
      const canonical = await canonicalIngestion();
      const { store, datum } = await admitted((d) =>
        replacePlutusConstrFieldCbor(
          d,
          [4],
          `d87b9f${canonicalDeepPlutusDataCbor(deep)}ff`,
        ),
      );
      await expectReadEverywhere(store, datum, canonical);
    },
  );
});

const UNREADABLE: readonly {
  name: string;
  datum: (canonical: string) => string;
}[] = [
  { name: "another outer constructor", datum: (d) => `d87a${d.slice(4)}` },
  { name: "an extra trailing field", datum: (d) => `${d.slice(0, -2)}00ff` },
  {
    name: "no refund datum",
    datum: (d) => `d8799f${[0, 1, 2, 3].map((i) => field(d, [i])).join("")}ff`,
  },
];

describe("a forced order whose datum no order cast accepts", () => {
  describe.each(UNREADABLE)("with $name", ({ datum: rewrite }) => {
    it("is recorded unreadable and is never due", async () => {
      const { store } = await admitted(rewrite);
      const [row, ...more] = await orderRows(store);
      expect(more).toEqual([]);
      expect(row).toMatchObject({
        status: "unreadable",
        detail: expect.stringContaining("datum"),
      });
      expect(await admittedReads(store)).toEqual([
        { kind: "unreadable", id: ID },
      ]);
      expect(await liveOrders(store)).toEqual([]);
      expect(await ingested(store)).toEqual([]);
      expect(await db(forcedOrderHorizon)).toBeNull();
    });

    it("replays a block without it and refuses one naming it", async () => {
      const { store } = await admitted(rewrite);
      const [row] = await orderRows(store);
      expect(await replayed(store, await emptyBlock())).toMatchObject({
        kind: "replayed",
      });
      expect(await replayed(store, await forcedBlock())).toEqual({
        kind: "invalid",
        detail: `forced order ${ID} is not a user event the node reads: ${String(row!.detail)}`,
      });
    });
  });
});
