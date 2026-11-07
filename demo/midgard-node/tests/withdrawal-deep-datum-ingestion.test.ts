/**
 * The node decodes a withdrawal whose L1 datum is deeply nested, or nested
 * to the admission bound: the follower-change driver's sink ingests it
 * (never `l1_event_undecodable`), and the history projection builds its
 * row. Event info of the wrong shape is still refused by both. Every row
 * ingestion stores is one the commit's withdrawal classification reads.
 */
import {
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventsAt,
  eventTrackedSet,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import {
  ADMISSION_PAYLOAD_NODES,
  DEEP_PLUTUS_DATA_CASES,
  deepPlutusDataCbor,
  depthAtAdmissionBound,
  plutusDataNodeCount,
  replaceCborItemOnce,
} from "@al-ft/midgard-test-support/deep-plutus-data";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { reconcileFollowerEvents } from "../src/database/follower-events.js";
import { withdrawalDataToEntry } from "../src/database/user-event-entries.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { EVENT_UNDECODABLE } from "../src/l1-events/driver.js";
import {
  FollowerWriteFixture,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.globals.js";
import { classifyWithdrawalsInOrder } from "../src/workers/utils/mpf/withdrawal-classification.js";
import { writeFollowerView } from "./helpers/follower-view.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
} from "./helpers/l1-events-chain.js";
import {
  ChainDriver,
  storeOpener,
  testDatabases,
} from "./helpers/l1-events-store.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | Globals>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(effect.pipe(Effect.provide(Globals.Default))),
  );

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xd5);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: nonces % 3 };
};

/** An honest withdrawal as the follower projects it. */
const projectedWithdrawal = async (): Promise<ProjectedEvent> => {
  const store = await storeOpener("sqlite", databases)(
    [eventProjection(EVENTS_CONFIG)],
    4,
  );
  opened.push(store);
  const d = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
  await d.init();
  await d.forward([admissionTx(eventOrder("withdrawal", nonceRef()), 1)]);
  const read = await eventsAt(store, listOf("withdrawal"), d.tip.point);
  if (read.kind !== "ok") throw new Error(JSON.stringify(read));
  return read.value[0]!;
};

// WithdrawalPayload: [event [id, info [body, signature, validity]], ...].
const INFO = [0, 1];
const BODY = [...INFO, 0];
const SIGNATURE = [...INFO, 1];

/** `event` with the payload item at `path` replaced by `cbor`. */
const withPayloadItem = (
  event: ProjectedEvent,
  path: readonly number[],
  cbor: string,
): ProjectedEvent => ({
  ...event,
  payloadCbor: replaceCborItemOnce(
    event.payloadCbor,
    plutusConstrFieldCbor(event.payloadCbor, path),
    cbor,
  ),
});

/** `event` with `InlineDatum { data: datum }` as its l1_datum, body field 4. */
const withInlineDatum = (event: ProjectedEvent, datum: string) => {
  const body = plutusConstrFieldCbor(event.payloadCbor, BODY);
  return withPayloadItem(
    event,
    BODY,
    replaceCborItemOnce(
      body,
      plutusConstrFieldCbor(body, [4]),
      `d87b81${datum}`,
    ),
  );
};

/** A projected withdrawal whose `shape` datum makes its payload the bound. */
const atAdmissionBound = async (shape: "list" | "constr") => {
  const projected = await projectedWithdrawal();
  const depth = depthAtAdmissionBound(
    withInlineDatum(projected, "00").payloadCbor,
  );
  const datum = deepPlutusDataCbor({ shape, depth });
  const event = withInlineDatum(projected, datum);
  expect(plutusDataNodeCount(event.payloadCbor)).toBe(ADMISSION_PAYLOAD_NODES);
  return { event, datum };
};

// The l1_datum is InlineDatum { data }: field 0 holds the datum.
const inlineDatumOf = (l1Datum: Buffer) =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(l1Datum.toString("hex"), [0]),
  );

const ingest = (event: ProjectedEvent) =>
  run(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      const plan = yield* writeFollowerView(100, [event]);
      const outcome = yield* withFollowerWrite(
        reconcileFollowerEvents(plan, {
          network: "Preprod",
          slotToUnixTime: (slot: number) => slot * 1000,
          cutoffMs: 60_000,
        }),
      ).pipe(Effect.provideService(FollowerWriteFixture, true));
      if (outcome.kind !== "applied")
        throw new Error(`outcome ${outcome.kind}`);
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<WithdrawalsDB.Entry>`
        SELECT * FROM ${sql(WithdrawalsDB.tableName)}`;
      return { ingestion: outcome.ingestion, rows };
    }),
  );

/** The history projection's input for a projected withdrawal. */
const historyData = (event: ProjectedEvent) => ({
  idCbor: Buffer.from(event.idCbor, "hex"),
  infoCbor: Buffer.from(plutusConstrFieldCbor(event.payloadCbor, INFO), "hex"),
  inclusionTime: new Date(Number(event.inclusionTime)),
  location: {
    txHash: event.location.txHash.toString("hex"),
    outputIndex: event.location.index,
  },
  assetName: event.key,
  payloadCbor: event.payloadCbor,
});

describe("follower ingestion of deeply nested withdrawal datums (Postgres)", () => {
  it.each(DEEP_PLUTUS_DATA_CASES)(
    "ingests a withdrawal with a $shape datum $depth deep ($bound)",
    async (deep) => {
      const datum = deepPlutusDataCbor(deep);
      const event = withInlineDatum(await projectedWithdrawal(), datum);
      const { ingestion, rows } = await ingest(event);
      expect(ingestion.refused).toEqual([]);
      expect(ingestion).toMatchObject({ inserted: 1 });
      expect(rows).toHaveLength(1);
      expect(rows[0]!.event_id.toString("hex")).toBe(event.idCbor);
      expect(inlineDatumOf(rows[0]!.l1_datum)).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(datum),
      );
    },
  );

  // Guard rows: these hold with a recursive decoder as well.
  it.each(["list", "constr"] as const)(
    "ingests a withdrawal whose %s datum is nested to the admission bound",
    async (shape) => {
      const { event, datum } = await atAdmissionBound(shape);
      const { ingestion, rows } = await ingest(event);
      expect(ingestion.refused).toEqual([]);
      expect(ingestion).toMatchObject({ inserted: 1 });
      expect(rows).toHaveLength(1);
      expect(inlineDatumOf(rows[0]!.l1_datum)).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(datum),
      );
    },
  );

  it.each([
    { name: "a shallow datum", datum: "00" },
    {
      name: "a list datum 15000 deep",
      datum: deepPlutusDataCbor({ shape: "list", depth: 15_000 }),
    },
  ])(
    "refuses withdrawal info of the wrong shape with $name by name",
    async ({ datum }) => {
      const event = withPayloadItem(
        withInlineDatum(await projectedWithdrawal(), datum),
        SIGNATURE,
        "00",
      );
      const { ingestion, rows } = await ingest(event);
      expect(ingestion).toMatchObject({ inserted: 0 });
      expect(ingestion.refused).toMatchObject([
        { kind: "withdrawal", key: event.key, reason: EVENT_UNDECODABLE },
      ]);
      expect(rows).toEqual([]);
    },
  );
});

describe("history projection of deeply nested withdrawal datums", () => {
  it.each(DEEP_PLUTUS_DATA_CASES)(
    "builds the row of a withdrawal with a $shape datum $depth deep ($bound)",
    async (deep) => {
      const datum = deepPlutusDataCbor(deep);
      const event = withInlineDatum(await projectedWithdrawal(), datum);
      const entry = await Effect.runPromise(
        withdrawalDataToEntry(historyData(event)),
      );
      expect(entry[WithdrawalsDB.Columns.ID].toString("hex")).toBe(
        event.idCbor,
      );
      expect(inlineDatumOf(entry[WithdrawalsDB.Columns.L1_DATUM])).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(datum),
      );
    },
  );

  // Guard rows: these hold with a recursive decoder as well.
  it.each(["list", "constr"] as const)(
    "builds the row of a withdrawal whose %s datum is nested to the admission bound",
    async (shape) => {
      const { event, datum } = await atAdmissionBound(shape);
      const entry = await Effect.runPromise(
        withdrawalDataToEntry(historyData(event)),
      );
      expect(inlineDatumOf(entry[WithdrawalsDB.Columns.L1_DATUM])).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(datum),
      );
    },
  );

  it("refuses withdrawal info of the wrong shape", async () => {
    const event = withPayloadItem(await projectedWithdrawal(), SIGNATURE, "00");
    const result = await Effect.runPromise(
      Effect.either(withdrawalDataToEntry(historyData(event))),
    );
    if (result._tag === "Right") throw new Error("built a row");
    expect(result.left.message).toBe(
      "Failed to project withdrawal history data into an offchain withdrawal entry",
    );
  });
});

/** The helper's withdrawals are owned by this key hash and carry 9 ADA. */
const OWNER = "cd".repeat(28);
const OWNER_OUTPUT = encodeMidgardTxOutput({
  address: midgardAddressFromText(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex(OWNER)),
    )
      .to_address()
      .to_bech32(undefined),
  ),
  value: { lovelace: 9_000_000n, assets: new Map() },
});

/** The commit's classification of `rows` over a ledger holding each row's
 * outref at its owner, as the block build runs it. */
const classified = (rows: readonly WithdrawalsDB.Entry[]) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const baseOutputs = new Map<string, Buffer>();
      for (const row of rows)
        baseOutputs.set(
          (yield* WithdrawalsDB.toLedgerOutRef(row)).toString("hex"),
          OWNER_OUTPUT,
        );
      return yield* classifyWithdrawalsInOrder({ entries: rows, baseOutputs });
    }).pipe(Effect.either),
  );

describe("classification of the withdrawal rows ingestion stores (Postgres)", () => {
  const ingested = [
    { name: "an honest withdrawal", event: () => projectedWithdrawal() },
    ...DEEP_PLUTUS_DATA_CASES.map((deep) => ({
      name: `a withdrawal with a ${deep.shape} datum ${deep.depth} deep`,
      event: async () =>
        withInlineDatum(await projectedWithdrawal(), deepPlutusDataCbor(deep)),
    })),
    ...(["list", "constr"] as const).map((shape) => ({
      name: `a withdrawal whose ${shape} datum is nested to the admission bound`,
      event: async () => (await atAdmissionBound(shape)).event,
    })),
  ];

  it.each(ingested)("reads the row of $name", async ({ event }) => {
    const { rows } = await ingest(await event());
    expect(rows).toHaveLength(1);
    const result = await classified(rows);
    if (result._tag === "Left") throw result.left;
    // The helper's signature is not the owner's: a verdict on the signature
    // shows every column it stored was read.
    expect(result.right.map(({ validity }) => validity)).toEqual([
      "IncorrectWithdrawalSignature",
    ]);
  });
});
