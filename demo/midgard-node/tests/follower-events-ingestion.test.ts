/**
 * The follower-change driver's Postgres sink (`reconcileFollowerEvents`,
 * N1), on the node database: events
 * the follower's own projection derives (a simulated chain on a SQLite
 * follower store) are ingested at a follower view written into the node
 * database's follower tables.
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
  countOrphanedAdmissions,
  followerEligibilityHorizon,
  type FollowerIngestionOutcome,
  reconcileFollowerEvents,
} from "../src/database/follower-events.js";
import {
  EVENT_IDENTITY_CONFLICT,
  type IngestionPlan,
} from "../src/l1-events/driver.js";
import {
  FollowerWriteFixture,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.globals.js";
import {
  admitFollowerKeys,
  rewindFollowerKey,
  writeFollowerTip,
  writeFollowerView,
} from "./helpers/follower-view.js";
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

/** Model slots: 1 s each from zero. */
const slotToUnixTime = (slot: number) => slot * 1000;
const VIEW_SLOT = 100;

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xe1);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: nonces % 3 };
};

/**
 * Admits a deposit at inclusion time 1 s, one at 9 s and a withdrawal on a
 * simulated chain, and returns them as the follower's projection reads them.
 */
const projectedEvents = async () => {
  const store = await storeOpener("sqlite", databases)(
    [eventProjection(EVENTS_CONFIG)],
    4,
  );
  opened.push(store);
  const d = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
  await d.init();
  const early = eventOrder("deposit", nonceRef(), { inclusionTime: 1_000n });
  const late = eventOrder("deposit", nonceRef(), { inclusionTime: 9_000n });
  const withdrawal = eventOrder("withdrawal", nonceRef());
  await d.forward([
    admissionTx(early, 1),
    admissionTx(late, 2),
    admissionTx(withdrawal, 3),
  ]);
  const read = async (kind: ProjectedEvent["kind"]) => {
    const result = await eventsAt(store, listOf(kind), d.tip.point);
    if (result.kind !== "ok") throw new Error(JSON.stringify(result));
    return result.value;
  };
  const deposits = await read("deposit");
  const byKey = (key: string) => deposits.find((event) => event.key === key)!;
  return {
    early: byKey(early.key),
    late: byKey(late.key),
    withdrawal: (await read("withdrawal"))[0]!,
  };
};

/** The driver's sink under the follower write gate's fixture capability. */
const ingest = (plan: IngestionPlan, cutoffMs = 0) =>
  withFollowerWrite(
    reconcileFollowerEvents(plan, {
      network: "Preprod",
      slotToUnixTime,
      cutoffMs,
    }),
  ).pipe(Effect.provideService(FollowerWriteFixture, true));

const applied = (outcome: FollowerIngestionOutcome) => {
  if (outcome.kind !== "applied") throw new Error(`outcome ${outcome.kind}`);
  return outcome.ingestion;
};

const eventRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const deposits = yield* sql<{
    event_id: Buffer;
    status: string;
    l1_event_key: Buffer;
    l1_origin_outref: Buffer;
    deposit_l1_tx_hash: Buffer;
  }>`SELECT event_id, status, l1_event_key, l1_origin_outref, deposit_l1_tx_hash
    FROM deposits_utxos ORDER BY inclusion_time`;
  const withdrawals = yield* sql<{
    event_id: Buffer;
    l1_event_key: Buffer;
    l1_origin_outref: Buffer;
    withdrawal_l1_tx_hash: Buffer;
    withdrawal_l1_output_index: number;
  }>`SELECT event_id, l1_event_key, l1_origin_outref, withdrawal_l1_tx_hash,
      withdrawal_l1_output_index FROM withdrawal_utxos`;
  const mempool = yield* sql<{
    source_event_id: Buffer;
  }>`SELECT source_event_id FROM mempool_ledger WHERE source_event_id IS NOT NULL`;
  return { deposits, withdrawals, mempool };
});

/** The identity the follower's key set records (its own outref encoding). */
const identityOf = (event: ProjectedEvent) => ({
  event_id: Buffer.from(event.idCbor, "hex"),
  l1_event_key: Buffer.from(event.key, "hex"),
  l1_origin_outref: encodeOutRef(event.admission.outRef),
});

/** `event` readmitted at another output (its key's admission after a rewind). */
const readmitted = (event: ProjectedEvent): ProjectedEvent => ({
  ...event,
  admission: {
    ...event.admission,
    outRef: { txHash: Buffer.alloc(32, 0x5a), index: 0 },
  },
});

describe("follower event ingestion (Postgres)", () => {
  it("inserts each projected event once with its follower admission identity", async () => {
    const { early, late, withdrawal } = await projectedEvents();
    const events = [early, late, withdrawal];
    const result = await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const plan = yield* writeFollowerView(VIEW_SLOT, events);
        const first = applied(yield* ingest(plan));
        const once = yield* eventRows;
        const again = applied(yield* ingest(plan));
        return { first, again, once, twice: yield* eventRows };
      }),
    );
    expect(result.first).toMatchObject({
      inserted: 3,
      locationsMoved: 0,
      orphans: 0,
      retiredUnseen: 0,
      projected: 0,
    });
    expect(result.again).toMatchObject({ inserted: 0, orphans: 0 });
    expect(result.twice).toEqual(result.once);
    expect(
      result.once.deposits.map(
        ({ event_id, l1_event_key, l1_origin_outref }) => ({
          event_id,
          l1_event_key,
          l1_origin_outref,
        }),
      ),
    ).toEqual([identityOf(early), identityOf(late)]);
    // Ruling 2: a deposit's L1 tx hash is its admission tx.
    expect(result.once.deposits[0]!.deposit_l1_tx_hash).toEqual(
      early.admission.outRef.txHash,
    );
    expect(result.once.withdrawals).toHaveLength(1);
    expect(result.once.withdrawals[0]).toMatchObject(identityOf(withdrawal));
  });

  it("moves only a withdrawal's location for a known admission", async () => {
    const { withdrawal } = await projectedEvents();
    const moved: ProjectedEvent = {
      ...withdrawal,
      location: { txHash: Buffer.alloc(32, 0x77), index: 2 },
    };
    const result = await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        applied(
          yield* ingest(yield* writeFollowerView(VIEW_SLOT, [withdrawal])),
        );
        const ingestion = applied(
          yield* ingest(yield* writeFollowerView(VIEW_SLOT + 1, [moved])),
        );
        return { ingestion, rows: yield* eventRows };
      }),
    );
    expect(result.ingestion).toMatchObject({ inserted: 0, locationsMoved: 1 });
    expect(result.rows.withdrawals).toEqual([
      {
        ...identityOf(withdrawal),
        withdrawal_l1_tx_hash: Buffer.alloc(32, 0x77),
        withdrawal_l1_output_index: 2,
      },
    ]);
  });

  it("projects deposits due by the cutoff into the mempool ledger, never later ones", async () => {
    const { early, late } = await projectedEvents();
    const result = await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const plan = yield* writeFollowerView(VIEW_SLOT, [early, late]);
        const before = applied(yield* ingest(plan, 5_000));
        const partial = yield* eventRows;
        const after = applied(yield* ingest(plan, 9_000));
        return { before, partial, after, full: yield* eventRows };
      }),
    );
    expect(result.before.projected).toBe(1);
    expect(result.partial.deposits.map((row) => row.status)).toEqual([
      "projected",
      "awaiting",
    ]);
    expect(result.partial.mempool).toEqual([
      { source_event_id: identityOf(early).event_id },
    ]);
    expect(result.after.projected).toBe(1);
    expect(result.full.deposits.map((row) => row.status)).toEqual([
      "projected",
      "projected",
    ]);
  });

  it("counts a row whose admission the follower rewound as an orphan and never adopts its id", async () => {
    const { early, late } = await projectedEvents();
    const result = await run(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        applied(
          yield* ingest(yield* writeFollowerView(VIEW_SLOT, [early, late])),
        );
        // A live row of another admission of the same public id is refused
        // by name; the run applies without it.
        const adopted = applied(
          yield* ingest(
            yield* writeFollowerView(VIEW_SLOT, [readmitted(early), late]),
          ),
        );
        // The follower rewinds past the admission and admits the id afresh.
        yield* rewindFollowerKey(early, encodeOutRef(early.admission.outRef));
        yield* admitFollowerKeys([readmitted(early)]);
        const orphaned = applied(
          yield* ingest(
            yield* writeFollowerView(VIEW_SLOT + 1, [readmitted(early), late]),
            60_000,
          ),
        );
        return {
          adopted,
          orphaned,
          counted: yield* countOrphanedAdmissions,
          rows: yield* eventRows,
        };
      }),
    );
    expect(result.adopted).toMatchObject({ inserted: 0, orphans: 0 });
    expect(result.adopted.refused).toEqual([
      {
        kind: "deposit",
        key: early.key,
        idCbor: early.idCbor,
        reason: EVENT_IDENTITY_CONFLICT,
        detail: "a local row of its public id holds another live admission",
      },
    ]);
    expect(result.orphaned).toMatchObject({ inserted: 0, orphans: 1 });
    expect(result.counted).toBe(1);
    // The orphan keeps its old identity until recovery removes it, and is
    // never projected; the canonical deposit is.
    expect(result.rows.deposits.map((row) => row.status)).toEqual([
      "awaiting",
      "projected",
    ]);
    expect(result.rows.deposits[0]).toMatchObject(identityOf(early));
    expect(result.rows.mempool).toEqual([
      { source_event_id: identityOf(late).event_id },
    ]);
  });

  it("writes nothing at a view a follower rewind removed, and its horizon goes with it", async () => {
    const { early } = await projectedEvents();
    const result = await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* resetApplicationTables;
        const none = yield* followerEligibilityHorizon;
        const old = yield* writeFollowerView(VIEW_SLOT - 10, []);
        applied(yield* ingest(old));
        const ingested = yield* followerEligibilityHorizon;
        const stalePlan = yield* writeFollowerView(VIEW_SLOT, [early]);
        // The follower rewinds below both views: blocks above go, the
        // generation advances.
        yield* sql`DELETE FROM l1_blocks WHERE slot > ${VIEW_SLOT - 20}`;
        yield* writeFollowerTip(VIEW_SLOT - 20, 2);
        const stale = yield* ingest(stalePlan);
        return {
          none,
          ingested,
          stale,
          rewound: yield* followerEligibilityHorizon,
          rows: yield* eventRows,
        };
      }),
    );
    expect(result.none).toBeNull();
    expect(result.ingested).toBeTypeOf("number");
    expect(result.stale).toEqual({ kind: "stale" });
    expect(result.rewound).toBeNull();
    expect(result.rows.deposits).toEqual([]);
  });
});
