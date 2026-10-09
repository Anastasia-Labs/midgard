/**
 * The journal-preparation depth check (`assertIncludedEventsDeep`, plan
 * §8.1) on the node database. At d > 0 a commit that includes events is
 * refused, with a named reason, unless each included deposit and withdrawal
 * is measured against the follower's cursor: the follower's event table must
 * exist, its cursor must exist, and each event must have its admission row.
 * At d = 0 nothing is refused. The events are the follower projection's
 * own (a simulated chain on a SQLite follower store), ingested into the node
 * database as the follower-change driver ingests them; the admission rows
 * are written as the projection writes them.
 */
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventsAt,
  eventTrackedSet,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { afterAll, afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  assertIncludedEventsDeep,
  COMMIT_EVENT_DEPTH_NO_CURSOR_MESSAGE,
  COMMIT_EVENT_DEPTH_NO_EVENT_TABLE_MESSAGE,
  COMMIT_EVENT_DEPTH_UNADMITTED_MESSAGE,
  COMMIT_EVENT_NOT_DEEP_MESSAGE,
} from "../src/database/commit-event-depth.js";
import { reconcileFollowerEvents } from "../src/database/follower-events.js";
import {
  FollowerWriteFixture,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.globals.js";
import {
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

/** The follower's cursor: model slot and height 100. */
const VIEW_SLOT = 100;

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xd7);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: nonces % 3 };
};

/** A deposit and a withdrawal, as the follower's projection reads them. */
const projectedEvents = async () => {
  const store = await storeOpener("sqlite", databases)(
    [eventProjection(EVENTS_CONFIG)],
    4,
  );
  opened.push(store);
  const driver = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
  await driver.init();
  const deposit = eventOrder("deposit", nonceRef(), { inclusionTime: 1_000n });
  const withdrawal = eventOrder("withdrawal", nonceRef());
  await driver.forward([admissionTx(deposit, 1), admissionTx(withdrawal, 2)]);
  const read = async (kind: ProjectedEvent["kind"]) => {
    const result = await eventsAt(store, listOf(kind), driver.tip.point);
    if (result.kind !== "ok") throw new Error(JSON.stringify(result));
    return result.value[0]!;
  };
  return {
    deposit: await read("deposit"),
    withdrawal: await read("withdrawal"),
  };
};

/** The node's deposit and withdrawal rows for `events`, at the cursor. */
const ingest = (events: readonly ProjectedEvent[]) =>
  Effect.gen(function* () {
    yield* resetApplicationTables;
    const plan = yield* writeFollowerView(VIEW_SLOT, events);
    const outcome = yield* withFollowerWrite(
      reconcileFollowerEvents(plan, {
        network: "Preprod",
        slotToUnixTime: (slot) => slot * 1000,
        cutoffMs: 0,
      }),
    ).pipe(Effect.provideService(FollowerWriteFixture, true));
    if (outcome.kind !== "applied") throw new Error(`outcome ${outcome.kind}`);
  });

/** `event`'s admission row, admitted at `height`. */
const writeAdmissionRow = (event: ProjectedEvent, height: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO node_l1_events (kind, event_key, event_id,
        inclusion_time, facts_cbor, payload_cbor, original_assets_cbor,
        admission_tx_hash, admission_output_index, admission_tx_index,
        admitted_block_hash, admitted_height, admitted_slot)
      VALUES (${event.kind}, ${Buffer.from(event.key, "hex")},
        ${Buffer.from(event.idCbor, "hex")}, ${event.inclusionTime.toString()},
        ${Buffer.from(event.factsCbor, "hex")},
        ${Buffer.from(event.payloadCbor, "hex")},
        ${Buffer.from(event.originalAssetsCbor, "hex")},
        ${Buffer.from(event.admission.outRef.txHash)},
        ${event.admission.outRef.index}, ${event.admission.txIndex},
        ${Buffer.from(event.admission.blockHash, "hex")}, ${height}, ${height})`;
  });

const idOf = (event: ProjectedEvent) => Buffer.from(event.idCbor, "hex");

/** The check over `included`, at `lagBlocks`: the refusal message, or null. */
const check = (
  lagBlocks: number,
  included: Readonly<{ deposit?: ProjectedEvent; withdrawal?: ProjectedEvent }>,
) =>
  Effect.either(
    assertIncludedEventsDeep({
      lagBlocks,
      depositIds:
        included.deposit === undefined ? [] : [idOf(included.deposit)],
      forcedIds: [],
      withdrawalIds:
        included.withdrawal === undefined ? [] : [idOf(included.withdrawal)],
    }),
  ).pipe(
    Effect.map((result) =>
      Either.isLeft(result) ? result.left.message : null,
    ),
  );

/** Rolls back whatever `effect` wrote, returning what it returned. */
const rolledBack = <A>(effect: Effect.Effect<A, never, SqlClient.SqlClient>) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rollback = Symbol("rollback");
    let value: A | undefined;
    yield* sql
      .withTransaction(
        Effect.gen(function* () {
          value = yield* effect;
          return yield* Effect.fail(rollback);
        }),
      )
      .pipe(
        Effect.catchIf(
          (error) => error === rollback,
          () => Effect.void,
        ),
      );
    return value as A;
  });

describe("journal-preparation depth check, when depth cannot be measured", () => {
  let events: Awaited<ReturnType<typeof projectedEvents>>;
  beforeEach(async () => {
    events = await projectedEvents();
    await run(ingest([events.deposit, events.withdrawal]));
  });

  it("refuses at d > 0 an included deposit with no admission row, and measures it once the row exists", async () => {
    const deposit = { deposit: events.deposit };
    expect(await run(check(1, deposit))).toBe(
      COMMIT_EVENT_DEPTH_UNADMITTED_MESSAGE,
    );
    expect(await run(check(0, deposit))).toBeNull();
    await run(writeAdmissionRow(events.deposit, VIEW_SLOT));
    expect(await run(check(1, deposit))).toBe(COMMIT_EVENT_NOT_DEEP_MESSAGE);
    expect(await run(check(0, deposit))).toBeNull();
  });

  it("refuses at d > 0 an included withdrawal with no admission row, and measures it once the row exists", async () => {
    const included = { deposit: events.deposit, withdrawal: events.withdrawal };
    await run(writeAdmissionRow(events.deposit, VIEW_SLOT - 1));
    expect(await run(check(1, included))).toBe(
      COMMIT_EVENT_DEPTH_UNADMITTED_MESSAGE,
    );
    expect(await run(check(0, included))).toBeNull();
    await run(writeAdmissionRow(events.withdrawal, VIEW_SLOT - 1));
    expect(await run(check(1, included))).toBeNull();
  });

  it("refuses at d > 0 when the follower has no cursor", async () => {
    const included = { deposit: events.deposit, withdrawal: events.withdrawal };
    await run(writeAdmissionRow(events.deposit, VIEW_SLOT - 1));
    await run(writeAdmissionRow(events.withdrawal, VIEW_SLOT - 1));
    expect(await run(check(1, included))).toBeNull();
    const withoutCursor = await run(
      rolledBack(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`DELETE FROM l1_follower_cursor`.pipe(Effect.orDie);
          return {
            d1: yield* check(1, included),
            d0: yield* check(0, included),
          };
        }),
      ),
    );
    expect(withoutCursor).toEqual({
      d1: COMMIT_EVENT_DEPTH_NO_CURSOR_MESSAGE,
      d0: null,
    });
    await run(writeFollowerTip(VIEW_SLOT));
    expect(await run(check(1, included))).toBeNull();
  });

  it("refuses at d > 0 when the follower's event table is missing", async () => {
    const included = { deposit: events.deposit };
    await run(writeAdmissionRow(events.deposit, VIEW_SLOT - 1));
    const withoutTable = await run(
      rolledBack(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`ALTER TABLE node_l1_events RENAME TO node_l1_events_absent`.pipe(
            Effect.orDie,
          );
          return {
            d1: yield* check(1, included),
            d0: yield* check(0, included),
          };
        }),
      ),
    );
    expect(withoutTable).toEqual({
      d1: COMMIT_EVENT_DEPTH_NO_EVENT_TABLE_MESSAGE,
      d0: null,
    });
    expect(await run(check(1, included))).toBeNull();
  });
});
