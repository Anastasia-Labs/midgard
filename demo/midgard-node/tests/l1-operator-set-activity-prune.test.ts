/**
 * The activity record in the follower's prune step (N6-R5), on SQLite and
 * Postgres: the step drops a record whose activation a fork orphaned, so a
 * catch-up that prunes past the orphaned point before the operator-set hook
 * runs, or a prune while a store reset replays, never leaves a record that
 * reads `operator_removed`; on Postgres also through the node database's
 * record, in the prune step's transaction.
 */
import { readFileSync } from "node:fs";

import {
  type FactStore,
  openPostgresFactStore,
  pointStatusIn,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Redacted } from "effect";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  type ActivityRecordBinding,
  databaseActivityRecord,
  memoryActivityRecord,
  type OperatorActivityRecord,
  operatorSetProjection,
} from "../src/l1-operator-set/index.js";
import { storeOpener, testDatabases } from "./helpers/l1-events-store.js";
import {
  loadOperatorSetChainFixture,
  type OperatorSetChainFixture,
} from "./helpers/operator-set-chain.js";
import {
  K,
  type OperatorSetChainOver,
  operatorSetChainOver,
  operatorSetNodeProcess,
  OWN,
  pruneAll,
} from "./helpers/operator-set-node.js";

const databases = testDatabases();
const opened: FactStore[] = [];
let fixture: OperatorSetChainFixture;

beforeAll(async () => {
  fixture = await loadOperatorSetChainFixture();
}, 120_000);
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const nodeProcess = (store: FactStore, activity: OperatorActivityRecord) =>
  operatorSetNodeProcess(fixture, store, activity);

/**
 * The catch-up race (N6-R5): the node records its activation, then the
 * follower applies a fork that orphans it and more than k blocks, and
 * prunes, with no hook run in between. The prune step drops the record;
 * once a later prune passes the orphaned point a kept record would count
 * (`point_beyond_retention`), yet nothing reads removed, in this process or
 * a restart.
 */
const orphanedCatchUp = async (
  { store, driver, lists, live, land, idle }: OperatorSetChainOver,
  record: OperatorActivityRecord,
): Promise<void> => {
  await idle(1);
  await land(lists.insert(live(), "active", OWN));
  const node = await nodeProcess(store, record);
  expect((await node.step()).run.membership.state).toBe("active");
  const recorded = await record.read();
  expect(recorded).not.toBeNull();

  await driver.backward(1);
  await idle(K + 2);
  await pruneAll(store);
  await pruneAll(store);
  expect((await store.cursor())!.prunedThroughSlot).toBeGreaterThan(
    recorded!.point.slot,
  );
  for (const proc of [node, await nodeProcess(store, record)]) {
    const read = await proc.step();
    expect(read.run.membership.state).toBe("unknown");
    expect(read.reason).toBeUndefined();
  }
  expect(await record.read()).toBeNull();
};

describe.each(["sqlite", "postgres"] as const)(
  "the activity record in the follower prune step (%s)",
  (dialect) => {
    const open = storeOpener(dialect, databases);

    const chainOf = async (activity: ActivityRecordBinding) => {
      const store = await open(
        [operatorSetProjection(fixture.config, activity)],
        K,
      );
      opened.push(store);
      return operatorSetChainOver(fixture, store);
    };

    it("drops a record whose activation a fork orphaned when a catch-up prunes past it before the hook runs, so it never reads removed", async () => {
      const record = memoryActivityRecord();
      await orphanedCatchUp(await chainOf(() => record), record);
    });

    it("keeps an orphaned activation decidable while a store reset replays, and drops its record at the first prune after the replay", async () => {
      const record = memoryActivityRecord();
      const { store, driver, lists, live, land, idle } = await chainOf(
        () => record,
      );
      await idle(1);
      await land(lists.insert(live(), "active", OWN));
      const node = await nodeProcess(store, record);
      expect((await node.step()).run.membership.state).toBe("active");
      const recorded = (await record.read())!;
      await driver.backward(1);
      await idle(K + 2);

      // A store reset's replay mark: the prune step skips every retention
      // hook, so the record stays, and the floor holds the boundary at the
      // orphaned point, which still reads orphaned, never beyond retention.
      await store.transaction("write", (tx) =>
        tx.query(
          "UPDATE l1_follower_tracked_set SET replaying = 1, replay_height = NULL WHERE id = 1",
        ),
      );
      await pruneAll(store);
      expect(await record.read()).not.toBeNull();
      expect((await store.cursor())!.prunedThroughSlot).toBeLessThanOrEqual(
        recorded.point.slot,
      );
      const during = await store.transaction("read", (tx) =>
        pointStatusIn(tx, store.dialect, recorded.point),
      );
      expect(during.kind).toBe("point_not_canonical");

      // The replay ends; the first prune after it drops the record, and the
      // next passes the point.
      expect(await store.endTrackedSetReplay()).toBe("ended");
      await pruneAll(store);
      expect(await record.read()).toBeNull();
      await pruneAll(store);
      expect((await store.cursor())!.prunedThroughSlot).toBeGreaterThan(
        recorded.point.slot,
      );
      for (const proc of [node, await nodeProcess(store, record)]) {
        const read = await proc.step();
        expect(read.run.membership.state).toBe("unknown");
        expect(read.reason).toBeUndefined();
      }
    });
  },
);

describe("the node database's activity record in the follower prune step (postgres)", () => {
  it("drops the node database's record of an orphaned activation in the prune step's transaction", async () => {
    const url = await databases.create();
    const run = <A>(
      effect: Effect.Effect<A, unknown, SqlClient.SqlClient>,
    ): Promise<A> =>
      Effect.runPromise(
        Effect.provide(
          effect,
          PgClient.layer({ url: Redacted.make(url), maxConnections: 1 }),
        ),
      );
    await run(
      Effect.flatMap(SqlClient.SqlClient, (sql) =>
        sql.unsafe(
          readFileSync(
            new URL(
              "../src/database/migrations/sql/0003_operator_membership.sql",
              import.meta.url,
            ),
            "utf8",
          ),
        ),
      ),
    );
    const record = databaseActivityRecord({
      run,
      manifestId: "ab".repeat(32),
      ownKey: OWN,
    });
    const store = openPostgresFactStore({
      ...projectionStoreOptions(
        [operatorSetProjection(fixture.config, () => record)],
        {
          securityParameter: K,
          trackedSet: {
            addresses: new Set(),
            paymentCredentials: new Set(),
            policies: new Set(),
          },
        },
        "postgres",
      ),
      connection: { connectionString: url },
    });
    opened.push(store);
    expect((await store.start()).kind).toBe("ready");
    await orphanedCatchUp(await operatorSetChainOver(fixture, store), record);
  });
});
