/**
 * The follower-change driver's Postgres sink refuses, by name, a projected
 * deposit it cannot decode into the node's row, and ingests the rest of the
 * run: one user-made admission must never fail every run.
 */
import {
  encodeOutRef,
  type FactStore,
  type OutRef,
} from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventsAt,
  eventTrackedSet,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  type FollowerIngestionOutcome,
  reconcileFollowerEvents,
} from "../src/database/follower-events.js";
import { EVENT_UNDECODABLE } from "../src/l1-events/driver.js";
import {
  UnownedHistoryFixture,
  withHistoryIngestion,
} from "../src/services/event-history-producer.js";
import { Globals } from "../src/services/globals.globals.js";
import { writeFollowerView } from "./helpers/follower-view.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
} from "./helpers/l1-events-chain.js";
import {
  ChainDriver,
  DROP_ALL_TIMEOUT_MS,
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
}, DROP_ALL_TIMEOUT_MS);

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | Globals>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(effect.pipe(Effect.provide(Globals.Default))),
  );

const VIEW_SLOT = 100;

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xe7);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: nonces % 3 };
};

/** The driver's sink under the unowned-history fixture gate. */
const ingest = (
  plan: Parameters<typeof reconcileFollowerEvents>[0],
  cutoffMs: number,
) =>
  withHistoryIngestion(
    reconcileFollowerEvents(plan, {
      network: "Preprod",
      slotToUnixTime: (slot: number) => slot * 1000,
      cutoffMs,
    }),
  ).pipe(Effect.provideService(UnownedHistoryFixture, true));

const applied = (outcome: FollowerIngestionOutcome) => {
  if (outcome.kind !== "applied") throw new Error(`outcome ${outcome.kind}`);
  return outcome.ingestion;
};

const eventRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const deposits = yield* sql<{
    event_id: Buffer;
    l1_event_key: Buffer;
    l1_origin_outref: Buffer;
  }>`SELECT event_id, l1_event_key, l1_origin_outref FROM deposits_utxos`;
  const mempool = yield* sql<{
    source_event_id: Buffer;
  }>`SELECT source_event_id FROM mempool_ledger WHERE source_event_id IS NOT NULL`;
  return { deposits, mempool };
});

/** The identity the follower's key set records (its own outref encoding). */
const identityOf = (event: ProjectedEvent) => ({
  event_id: Buffer.from(event.idCbor, "hex"),
  l1_event_key: Buffer.from(event.key, "hex"),
  l1_origin_outref: encodeOutRef(event.admission.outRef),
});

describe("follower event ingestion refusals (Postgres)", () => {
  it("refuses an undecodable deposit by name and ingests the rest of the run", async () => {
    const store = await storeOpener("sqlite", databases)(
      [eventProjection(EVENTS_CONFIG)],
      4,
    );
    opened.push(store);
    const d = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
    await d.init();
    // On-chain admission does not bound a deposit's L2 network id; the node
    // decodes only Midgard's own.
    const foreign = eventOrder("deposit", nonceRef(), { l2NetworkId: 7n });
    const honest = eventOrder("deposit", nonceRef(), { inclusionTime: 2_000n });
    await d.forward([admissionTx(foreign, 1), admissionTx(honest, 2)]);
    const read = await eventsAt(store, listOf("deposit"), d.tip.point);
    if (read.kind !== "ok") throw new Error(JSON.stringify(read));
    const byKey = (key: string) =>
      read.value.find((event) => event.key === key)!;
    const result = await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const plan = yield* writeFollowerView(VIEW_SLOT, [
          byKey(foreign.key),
          byKey(honest.key),
        ]);
        const first = applied(yield* ingest(plan, 60_000));
        const again = applied(yield* ingest(plan, 60_000));
        return { first, again, rows: yield* eventRows };
      }),
    );
    const refusal = {
      kind: "deposit",
      key: foreign.key,
      idCbor: byKey(foreign.key).idCbor,
      reason: EVENT_UNDECODABLE,
      detail: "unsupported committed deposit L2 network id",
    };
    expect(result.first).toMatchObject({ inserted: 1, projected: 1 });
    expect(result.first.refused).toEqual([refusal]);
    // Refusals are re-derived each run: the event stays refused, never adopted.
    expect(result.again).toMatchObject({ inserted: 0 });
    expect(result.again.refused).toEqual([refusal]);
    expect(result.rows.deposits).toHaveLength(1);
    expect(result.rows.deposits[0]).toMatchObject(
      identityOf(byKey(honest.key)),
    );
    expect(result.rows.mempool).toEqual([
      { source_event_id: identityOf(byKey(honest.key)).event_id },
    ]);
  });
});
