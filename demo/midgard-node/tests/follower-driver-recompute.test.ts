/**
 * The follower-change driver's write gate and recompute (plan §7.3, §8.1,
 * N1, N3) on the node database:
 *
 * - a producer's write at a view the follower left, under an epoch a later
 *   recompute superseded, or while a recompute is pending, is refused by
 *   its named hold and runs nothing; retried at the view the driver applies
 *   next, it is admitted;
 * - the driver's recompute disposes of a live own journal that holds an
 *   orphaned deposit (a member whose admission left the chain) and, in the
 *   same run, removes that deposit with its working-ledger row, then opens
 *   the gate; an orphan a processed landed block holds keeps the gate
 *   pending as `l1_events_orphan_recovery` (producers refused, retried, no
 *   failure) until it is no longer orphaned;
 * - a failed startup preparation is the driver sink's named hold, the gate
 *   stays pending, and the next run prepares and opens it; nothing fails.
 */
import { createHash } from "node:crypto";

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
import { Effect, Either, Ref } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { reconcileFollowerEvents } from "../src/database/follower-events.js";
import {
  classifyChange,
  EVENTS_ORPHAN_RECOVERY,
  type IngestionPlan,
} from "../src/l1-events/driver.js";
import { beginDriverRecompute } from "../src/services/follower-write-gate.driver.js";
import {
  DRIVER_RECOMPUTE_PENDING,
  FOLLOWER_VIEW_STALE,
  FollowerWrite,
  followerWriteHoldOf,
  readFollowerWriteGate,
  runAtFollowerView,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import { followerWriteGateReasons } from "../src/services/follower-write-gate.local.js";
import { Globals } from "../src/services/globals.globals.js";
import { driverSink } from "../src/services/l1-follower.driver-sink.js";
import { STARTUP_PREPARATION_FAILED } from "../src/services/l1-follower.recompute.js";
import { Lucid } from "../src/services/lucid.js";
import {
  DRIVER_TEST_SLOT,
  modelSlotLucid,
  testDriverRecompute,
  testWrite,
} from "./helpers/driver-recompute.js";
import {
  FOLLOWER_GENERATION,
  modelSlotTime,
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
import {
  insertOwnJournal,
  JournalStatus,
  journalStatuses,
} from "./helpers/landed-blocks-sim.own.js";
import {
  BLOCK,
  freshNative,
  processOf,
  R1,
  root,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";
import { resetApplicationTables } from "./utils.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

/** The named hold of a refused write, if it was refused by one. */
const holdOf = (result: Either.Either<unknown, unknown>) =>
  Either.isLeft(result) ? followerWriteHoldOf(result.left)?.reason : undefined;

/** One deposit admitted on a simulated chain, as the follower's projection reads it. */
const projectedDeposit = async (): Promise<ProjectedEvent> => {
  const store = await storeOpener("sqlite", databases)(
    [eventProjection(EVENTS_CONFIG)],
    4,
  );
  opened.push(store);
  const chain = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
  await chain.init();
  const nonce: OutRef = { txHash: Buffer.alloc(32, 0xd5), index: 1 };
  await chain.forward([
    admissionTx(eventOrder("deposit", nonce, { inclusionTime: 1_000n }), 1),
  ]);
  const read = await eventsAt(store, listOf("deposit"), chain.tip.point);
  if (read.kind !== "ok" || read.value.length !== 1)
    throw new Error(JSON.stringify(read));
  return read.value[0]!;
};

describe("the follower write gate", () => {
  it("refuses a producer's write by name while its view or epoch is stale or a recompute is pending, and admits it retried", async () => {
    const globals = await processOf(freshNative());
    let at = { slot: DRIVER_TEST_SLOT, generation: FOLLOWER_GENERATION };
    let writes = 0;
    const write = Effect.either(
      withFollowerWrite(
        Effect.sync(() => {
          writes += 1;
        }),
      ),
    );
    await run(
      globals,
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const recompute = yield* testDriverRecompute({
          plan: Effect.suspend(() =>
            writeFollowerView(at.slot, [], at.generation),
          ),
        });
        expect((yield* recompute.run("first view")).published).toBe(true);
        const permit = yield* runAtFollowerView(FollowerWrite);
        expect(permit.view).toMatchObject({ slot: DRIVER_TEST_SLOT });
        const asProducer = Effect.provideService(FollowerWrite, permit);
        expect(Either.isRight(yield* asProducer(write))).toBe(true);
        expect(writes).toBe(1);

        // The follower rewinds off the permit's view.
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM l1_blocks WHERE slot = ${DRIVER_TEST_SLOT}`;
        at = {
          slot: DRIVER_TEST_SLOT - 10,
          generation: FOLLOWER_GENERATION + 1,
        };
        yield* writeFollowerTip(at.slot, at.generation);
        expect(holdOf(yield* asProducer(write))).toBe(FOLLOWER_VIEW_STALE);
        // A producer started now is permitted at the applied view only to
        // be refused at its write: the view is checked in the write's
        // own transaction.
        const late = yield* Effect.either(runAtFollowerView(write));
        expect(Either.isRight(late) && holdOf(late.right)).toBe(
          FOLLOWER_VIEW_STALE,
        );
        expect(writes).toBe(1);

        // Retried once the driver applied the follower's new view.
        expect((yield* recompute.run("the follower rewound")).published).toBe(
          true,
        );
        const retried = yield* runAtFollowerView(
          Effect.flatMap(FollowerWrite, (next) =>
            Effect.map(write, (result) => ({ next, result })),
          ),
        );
        expect(Either.isRight(retried.result)).toBe(true);
        expect(retried.next.view).toMatchObject({
          generation: FOLLOWER_GENERATION + 1,
          slot: DRIVER_TEST_SLOT - 10,
        });
        expect(writes).toBe(2);

        // A later recompute supersedes the retried permit's epoch.
        expect((yield* recompute.run("a later recompute")).published).toBe(
          true,
        );
        expect(
          holdOf(
            yield* Effect.provideService(write, FollowerWrite, retried.next),
          ),
        ).toBe(FOLLOWER_VIEW_STALE);

        // While a recompute is pending, no producer starts or writes.
        const current = yield* runAtFollowerView(FollowerWrite);
        yield* beginDriverRecompute(DRIVER_RECOMPUTE_PENDING, "test recompute");
        expect(
          holdOf(yield* Effect.provideService(write, FollowerWrite, current)),
        ).toBe(DRIVER_RECOMPUTE_PENDING);
        expect(holdOf(yield* Effect.either(runAtFollowerView(write)))).toBe(
          DRIVER_RECOMPUTE_PENDING,
        );
        expect(
          followerWriteGateReasons(yield* Ref.get(globals.FOLLOWER_WRITE_GATE)),
        ).toEqual([DRIVER_RECOMPUTE_PENDING]);
        expect(writes).toBe(2);
        expect((yield* recompute.run("the pending recompute")).published).toBe(
          true,
        );
        expect(Either.isRight(yield* runAtFollowerView(write))).toBe(true);
        expect(writes).toBe(3);
      }),
    );
  });
});

describe("the driver's recompute repairs orphans with the rebase", () => {
  const C = "c4".repeat(28);

  /**
   * The rebase is due (processed foreign block BLOCK waits unapplied on the
   * frontier) and the follower admitted one deposit at the view. Its holder
   * is `"journal"`: own block C, live on BLOCK, names it as a member and
   * holds it by header; or `"landed"`: BLOCK includes it. Then the follower
   * rewinds past the deposit's admission, orphaning it.
   */
  const arrange = async (holder: "journal" | "landed") => {
    const deposit = await projectedDeposit();
    const eventId = Buffer.from(deposit.idCbor, "hex");
    const globals = await processOf(freshNative());
    await seed(globals, holder === "landed" ? { depositIds: [eventId] } : {});
    const view = { events: [deposit] as readonly ProjectedEvent[] };
    const plan = Effect.suspend(() =>
      writeFollowerView(DRIVER_TEST_SLOT, view.events),
    );
    await run(
      globals,
      testWrite(
        Effect.gen(function* () {
          const at: IngestionPlan = yield* plan;
          yield* reconcileFollowerEvents(at, {
            network: "Preprod",
            slotToUnixTime: modelSlotTime,
            cutoffMs: modelSlotTime(DRIVER_TEST_SLOT),
          });
          if (holder === "journal") {
            yield* insertOwnJournal({
              headerHash: C,
              baseHeaderHash: BLOCK,
              baseUtxosRoot: R1,
              expectedUtxosRoot: root(0x12),
              spent: [],
              produced: [],
              txIds: [],
              at: new Date(1_000),
            });
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE deposits_utxos SET status = 'projected',
              projected_header_hash = ${Buffer.from(C, "hex")}`;
            const payload = Buffer.from("orphan-member");
            yield* sql`INSERT INTO pending_block_finalization_deposits ${sql.insert(
              {
                header_hash: Buffer.from(C, "hex"),
                member_id: eventId,
                ordinal: 0,
                payload_cbor: payload,
                payload_sha256: createHash("sha256").update(payload).digest(),
                source_table: "deposits_utxos",
                source_id: eventId,
                source_time_stamp_tz: new Date(1_000),
                l1_event_key: Buffer.from(deposit.key, "hex"),
                l1_origin_outref: encodeOutRef(deposit.admission.outRef),
              } as never,
            )}`;
          }
          view.events = [];
          yield* rewindFollowerKey(deposit);
        }),
      ),
    );
    const rows = () =>
      run(
        globals,
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const deposits = yield* sql<{
            status: string;
            projected: Buffer | null;
          }>`SELECT status::text AS status, projected_header_hash AS projected FROM deposits_utxos`;
          const working = yield* sql<{
            count: string;
          }>`SELECT count(*)::text AS count FROM mempool_ledger WHERE source_event_id IS NOT NULL`;
          const applied = yield* sql<{
            applied: boolean;
          }>`SELECT applied FROM node_landed_blocks`;
          return {
            deposits,
            working: Number(working[0]!.count),
            applied: applied.map((row) => row.applied),
            journal: (yield* journalStatuses).get(C),
            gate: yield* readFollowerWriteGate,
          };
        }),
      );
    return { globals, plan, deposit, view, rows };
  };

  it("disposes of the live own journal holding an orphan and removes the orphan and its working-ledger row, in one run", async () => {
    const { globals, plan, rows } = await arrange("journal");
    const before = await rows();
    expect(before.deposits).toMatchObject([{ status: "projected" }]);
    expect(before.working).toBe(1);
    expect(before.applied).toEqual([false]);
    expect(before.journal).toBe(JournalStatus.SubmittedUnconfirmed);
    const outcome = await run(
      globals,
      Effect.flatMap(testDriverRecompute({ plan }), (recompute) =>
        recompute.run("first view"),
      ),
    );
    expect(outcome).toMatchObject({ published: true, hold: undefined });
    const after = await rows();
    expect(after.applied).toEqual([true]);
    expect(after.journal).toBe(JournalStatus.Abandoned);
    expect(after.deposits).toEqual([]);
    expect(after.working).toBe(0);
    expect(after.gate.pending).toBeUndefined();
    expect(after.gate.applied).toMatchObject({ slot: DRIVER_TEST_SLOT });
  });

  it("holds the gate by name while a landed block holds an orphan, and opens it once the admission returns", async () => {
    const { globals, plan, deposit, view, rows } = await arrange("landed");
    let writes = 0;
    const write = Effect.either(
      withFollowerWrite(
        Effect.sync(() => {
          writes += 1;
        }),
      ),
    );
    await run(
      globals,
      Effect.gen(function* () {
        const recompute = yield* testDriverRecompute({ plan });
        expect(yield* recompute.run("first view")).toMatchObject({
          published: false,
          hold: { reason: EVENTS_ORPHAN_RECOVERY },
        });
        expect(holdOf(yield* Effect.either(runAtFollowerView(write)))).toBe(
          DRIVER_RECOMPUTE_PENDING,
        );
        // Retried, it holds again; nothing fails.
        expect((yield* recompute.run("retry")).hold?.reason).toBe(
          EVENTS_ORPHAN_RECOVERY,
        );
        expect(writes).toBe(0);
      }),
    );
    const during = await rows();
    expect(during.applied).toEqual([true]);
    expect(during.deposits).toMatchObject([
      { projected: Buffer.from(BLOCK, "hex") },
    ]);
    expect(during.gate.pending?.reason).toBe(EVENTS_ORPHAN_RECOVERY);

    // The follower re-admits the deposit (the rewind reverted).
    view.events = [deposit];
    await run(
      globals,
      Effect.gen(function* () {
        const recompute = yield* testDriverRecompute({ plan });
        expect(yield* recompute.run("the admission returned")).toMatchObject({
          published: true,
          hold: undefined,
        });
        expect(Either.isRight(yield* runAtFollowerView(write))).toBe(true);
      }),
    );
    const after = await rows();
    expect(after.deposits).toHaveLength(1);
    expect(after.gate.pending).toBeUndefined();
    expect(writes).toBe(1);
  });
});

describe("the driver's startup preparation", () => {
  it("is a named hold of the driver's first view while it fails, retried until it runs", async () => {
    const globals = await processOf(freshNative());
    let failing = true;
    let prepared = 0;
    await run(
      globals,
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const recompute = yield* testDriverRecompute({
          startupPreparation: Effect.suspend(() =>
            failing
              ? Effect.fail(new Error("the startup preparation failed"))
              : // It runs under the driver's write capability.
                withFollowerWrite(
                  Effect.sync(() => {
                    prepared += 1;
                  }),
                ),
          ),
        });
        const sink = yield* driverSink(recompute).pipe(
          Effect.provideService(Lucid, modelSlotLucid),
        );
        const plan = yield* writeFollowerView(DRIVER_TEST_SLOT, []);
        const change = classifyChange(null, plan.view);
        const first = yield* Effect.promise(() => sink.apply(change, plan));
        expect(first).toEqual({
          kind: "held",
          hold: {
            reason: STARTUP_PREPARATION_FAILED,
            detail: expect.stringContaining("the startup preparation failed"),
          },
        });
        const gate = yield* readFollowerWriteGate;
        expect(gate.pending?.reason).toBe(STARTUP_PREPARATION_FAILED);
        expect(gate.applied).toBeUndefined();
        expect(
          followerWriteGateReasons(yield* Ref.get(globals.FOLLOWER_WRITE_GATE)),
        ).toEqual([DRIVER_RECOMPUTE_PENDING]);
        expect(
          holdOf(yield* Effect.either(runAtFollowerView(Effect.void))),
        ).toBe(DRIVER_RECOMPUTE_PENDING);

        // The next driver run retries it.
        const again = yield* Effect.promise(() => sink.apply(change, plan));
        expect(again).toMatchObject({
          kind: "held",
          hold: { reason: STARTUP_PREPARATION_FAILED },
        });
        failing = false;
        const ran = yield* Effect.promise(() => sink.apply(change, plan));
        expect(ran).toMatchObject({ kind: "applied" });
        expect(prepared).toBe(1);
        expect((yield* readFollowerWriteGate).pending).toBeUndefined();
        // Once per process: the next recompute does not prepare again.
        expect((yield* recompute.run("a later recompute")).published).toBe(
          true,
        );
        expect(prepared).toBe(1);
        yield* runAtFollowerView(Effect.void);
      }).pipe(Effect.provideService(Globals, globals)),
    );
  });
});
