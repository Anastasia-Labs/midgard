/**
 * The follower-change driver's Postgres sink (`reconcileFollowerEvents`) on
 * a node that took its ledger from a peer's snapshot: an event the anchor's
 * block or an earlier one included gets no row, so it is never projected
 * into the working ledger and never becomes a due event the next block
 * cannot take. Before the node installs its ledger no event gets a row:
 * the install's anchor then admits the events after it.
 */
import { type FactStore, type OutRef } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventsAt,
  eventTrackedSet,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  resolveIncludedDepositEntriesForWindow,
  resolveIncludedWithdrawalEntriesForWindow,
} from "../src/mpf/event-window.resolve-included-forced-transaction-entries-for-window.js";
import {
  FollowerWriteFixture,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import {
  reconcileOverDead,
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
import { installEmptyLedger } from "./helpers/ledger-install.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const ANCHOR_END_MS = 5_000n;
const VIEW_SLOT = 100;

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xe2);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: nonces % 3 };
};

/**
 * A deposit and a withdrawal included at or before `ANCHOR_END_MS` (the
 * deposit exactly at it) and one of each just after it, as the follower's
 * projection reads them.
 */
const eventsAroundAnchor = async () => {
  const store = await storeOpener("sqlite", databases)(
    [eventProjection(EVENTS_CONFIG)],
    4,
  );
  opened.push(store);
  const d = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
  await d.init();
  const after = ANCHOR_END_MS + 1n;
  const orders = {
    depositAtAnchor: eventOrder("deposit", nonceRef(), {
      inclusionTime: ANCHOR_END_MS,
    }),
    depositAfter: eventOrder("deposit", nonceRef(), { inclusionTime: after }),
    withdrawalBefore: eventOrder("withdrawal", nonceRef(), {
      inclusionTime: 1_000n,
    }),
    withdrawalAfter: eventOrder("withdrawal", nonceRef(), {
      inclusionTime: after,
    }),
  };
  await d.forward(
    Object.values(orders).map((order, index) => admissionTx(order, index + 1)),
  );
  const all: ProjectedEvent[] = [];
  for (const kind of ["deposit", "withdrawal"] as const) {
    const result = await eventsAt(store, listOf(kind), d.tip.point);
    if (result.kind !== "ok") throw new Error(JSON.stringify(result));
    all.push(...result.value);
  }
  const idOf = (key: string) => all.find((event) => event.key === key)!.idCbor;
  return {
    events: all,
    depositAfter: idOf(orders.depositAfter.key),
    withdrawalAfter: idOf(orders.withdrawalAfter.key),
  };
};

const hexIds = (rows: readonly { event_id: Buffer }[]) =>
  rows.map((row) => row.event_id.toString("hex"));

const planned = <A extends { event_id: Buffer }>(
  selection: Either.Either<readonly A[], unknown>,
) => {
  if (Either.isLeft(selection)) throw selection.left;
  return hexIds(selection.right);
};

const reconcileAt = (
  plan: Effect.Effect.Success<ReturnType<typeof writeFollowerView>>,
) =>
  withFollowerWrite(
    reconcileOverDead(plan, {
      network: "Preprod",
      slotToUnixTime: (slot) => slot * 1000,
      cutoffMs: 60_000,
    }),
  ).pipe(Effect.provideService(FollowerWriteFixture, true));

const eventRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return {
    deposits: hexIds(
      yield* sql<{ event_id: Buffer }>`SELECT event_id FROM deposits_utxos`,
    ),
    withdrawals: hexIds(
      yield* sql<{ event_id: Buffer }>`SELECT event_id FROM withdrawal_utxos`,
    ),
  };
});

describe("follower event ingestion before the node's ledger is installed", () => {
  it("gives no event a row while the install is pending, and after it only the events included after its anchor", async () => {
    const around = await eventsAroundAnchor();
    const result = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          const plan = yield* writeFollowerView(VIEW_SLOT, around.events);
          yield* reconcileAt(plan);
          const pending = yield* eventRows;
          yield* installEmptyLedger(ANCHOR_END_MS);
          yield* reconcileAt(plan);
          return { pending, installed: yield* eventRows };
        }),
      ),
    );
    expect(result.pending).toEqual({ deposits: [], withdrawals: [] });
    expect(result.installed).toEqual({
      deposits: [around.depositAfter],
      withdrawals: [around.withdrawalAfter],
    });
  });
});

describe("follower event ingestion on a node started from a snapshot", () => {
  it("gives no row to a deposit or withdrawal the anchor's block or an earlier one included, and the next block plans", async () => {
    const around = await eventsAroundAnchor();
    const result = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* resetApplicationTables;
          yield* installEmptyLedger(ANCHOR_END_MS);
          const plan = yield* writeFollowerView(VIEW_SLOT, around.events);
          yield* reconcileAt(plan);
          const deposits = yield* sql<{
            event_id: Buffer;
          }>`SELECT event_id FROM deposits_utxos`;
          const withdrawals = yield* sql<{
            event_id: Buffer;
          }>`SELECT event_id FROM withdrawal_utxos`;
          const mempool = yield* sql<{
            event_id: Buffer;
          }>`SELECT source_event_id AS event_id FROM mempool_ledger
            WHERE source_event_id IS NOT NULL`;
          // The block after the anchor's starts at the anchor's end time.
          const window = {
            currentBlockStartTime: new Date(Number(ANCHOR_END_MS)),
            effectiveEndTime: new Date(60_000),
          };
          return {
            deposits,
            withdrawals,
            mempool,
            plannedDeposits: yield* Effect.either(
              resolveIncludedDepositEntriesForWindow(window),
            ),
            plannedWithdrawals: yield* Effect.either(
              resolveIncludedWithdrawalEntriesForWindow(window),
            ),
          };
        }),
      ),
    );
    expect(hexIds(result.deposits)).toEqual([around.depositAfter]);
    expect(hexIds(result.withdrawals)).toEqual([around.withdrawalAfter]);
    // Only the deposit after the anchor enters the working ledger.
    expect(hexIds(result.mempool)).toEqual([around.depositAfter]);
    expect(planned(result.plannedDeposits)).toEqual([around.depositAfter]);
    expect(planned(result.plannedWithdrawals)).toEqual([
      around.withdrawalAfter,
    ]);
  });
});
