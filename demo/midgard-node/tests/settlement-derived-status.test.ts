/**
 * A settlement attempt's confirmation, derived from the intent journal (plan
 * §8.2, §15 N6 and L7, D-N4), over a real Postgres follower store in the
 * node's database, with the node's settlement and journal projections:
 *
 * - Landed at depth >= cd, the attempt's job takes the next phase; a
 *   rollback deeper than cd that un-lands it reverts the phase, and the
 *   attempt blocks new work (no body may reuse its restored fee coin) until
 *   it relands.
 * - Landed more than k deep, the follower's prune step stores `final` in
 *   the step that prunes its journal entry; never earlier.
 * - Migration 0015, on a database a release left at v12 with rows in the
 *   old columns: a `confirmed` attempt the journal still holds is pending
 *   again and reads the journal's level; one it does not hold stays
 *   terminal (`final`); a database with no follower tables migrates too.
 */
import "./utils.js";

import { createHash, randomUUID } from "node:crypto";

import {
  currentViewIn,
  type FactStore,
  intentJournalProjection,
  openPostgresFactStore,
  projectionStoreOptions,
  recordIntentIn,
} from "@al-ft/midgard-l1-follower";
import {
  encodeSimTx,
  type SimTx,
  simTxHash,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, ManagedRuntime, Redacted } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { MIGRATIONS } from "../src/database/migrations/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { splitSqlStatements } from "../src/database/migrations/runner.js";
import * as Journal from "../src/database/settlement.js";
import { readIntentStatus } from "../src/services/intent-journal.js";
import { settlementProjection } from "../src/services/settlement.final-hook.js";
import {
  noOpenAttempt,
  settleAttempts,
  settlementLevel,
} from "../src/services/settlement.status.js";
import { openFollowerWriteGate } from "./helpers/follower-write-gate.js";
import { ChainDriver, testDatabases } from "./helpers/l1-events-store.js";

const K = 6;
const CD = 3;
const depths = { confirmationDepth: CD, securityParameter: K };
const deploymentId = "a7".repeat(32);
const databases = testDatabases();
const closers: (() => Promise<void>)[] = [];

afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});
afterAll(async () => {
  await databases.dropAll();
});

/** `0012`: the last version a release shipped with a stored `confirmed`. */
const LEGACY_VERSION = 12;

const manifestHashThrough = (version: number): string =>
  createHash("sha256")
    .update(
      MIGRATIONS.filter((migration) => migration.version <= version)
        .map((m) => `${m.version}:${m.name}:${m.checksumSha256}`)
        .join("\n"),
    )
    .digest("hex");

/** A node database: fully migrated, or as a release left it at `through`. */
const nodeDatabase = async (through?: number) => {
  const connectionString = await databases.create();
  const runtime = ManagedRuntime.make(
    PgClient.layer({ url: Redacted.make(connectionString) }),
  );
  closers.push(() => runtime.dispose());
  const run = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
    runtime.runPromise(effect);
  if (through === undefined)
    await run(MigrationRunner.migrate({ appVersion: "test", actor: "n6b" }));
  else
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* MigrationRunner.getStatus;
        for (const migration of MIGRATIONS.filter(
          (m) => m.version <= through,
        )) {
          for (const statement of splitSqlStatements(migration.sql))
            yield* sql.unsafe(statement);
          yield* sql`INSERT INTO schema_migrations
            (version, name, checksum_sha256, manifest_hash_sha256,
             app_version, execution_ms, applied_by)
            VALUES (${migration.version}, ${migration.name},
                    ${migration.checksumSha256}, ${manifestHashThrough(through)},
                    'earlier-release', 1, 'earlier-release')`;
        }
      }),
    );
  return { connectionString, run };
};

/** The node's follower store over its database, with the node's settlement and journal projections. */
const followerOver = async (connectionString: string) => {
  const store: FactStore = openPostgresFactStore({
    ...projectionStoreOptions(
      [settlementProjection, intentJournalProjection],
      { securityParameter: K, trackedSet: simUniverse().tracked },
      "postgres",
    ),
    connection: { connectionString },
  });
  closers.push(() => store.close());
  const started = await store.start();
  if (started.kind !== "ready")
    throw new Error(`store start: ${JSON.stringify(started)}`);
  const driver = new ChainDriver(store, simUniverse().tracked);
  await driver.init();
  const own = simUniverse().trackedAddress;
  const idle = async (blocks: number) => {
    for (let i = 0; i < blocks; i += 1)
      await driver.forward([
        {
          inputs: [driver.chain.outsideInput()],
          outputs: [
            { address: simUniverse().untrackedAddress, lovelace: 2_000_000n },
          ],
          nonce: driver.chain.nonce(),
        },
      ]);
  };
  /** A settlement body spending a fresh own output, journaled as S6 sends it. */
  const journaled = async (): Promise<SimTx> => {
    const fund: SimTx = {
      inputs: [driver.chain.outsideInput()],
      outputs: [{ address: own, lovelace: 10_000_000n }],
      nonce: driver.chain.nonce(),
    };
    await driver.forward([fund]);
    const body: SimTx = {
      inputs: [{ txHash: simTxHash(fund), index: 0 }],
      outputs: [{ address: own, lovelace: 9_000_000n }],
      nonce: driver.chain.nonce(),
    };
    const recorded = await store.transaction("write", async (tx) =>
      recordIntentIn(tx, store.dialect, {
        family: "settlement",
        workflowKey: `settlement:${simTxHash(body).toString("hex")}`,
        txCbor: encodeSimTx(body),
        isOwnOutput: (output) => output.address.equals(own),
        builtAt: (await currentViewIn(tx, store.dialect))!,
      }),
    );
    if (recorded.kind !== "recorded")
      throw new Error(`record: ${recorded.kind}`);
    return body;
  };
  const pruneAll = async (): Promise<void> => {
    for (;;) {
      const pruned = await store.prune();
      if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
      if (pruned.done) return;
    }
  };
  return { store, driver, idle, journaled, pruneAll };
};

const owner: Journal.SettlementOwner = {
  deploymentId,
  walletAddress: "settlement-wallet",
  token: randomUUID(),
};

/** The open follower write gate and settlement ownership the journal's
 * writes need. */
const ownSettlement = Effect.gen(function* () {
  yield* openFollowerWriteGate;
  yield* Journal.renew(owner);
});

const attemptOf = (
  body: SimTx,
  eventId: string,
): Journal.SettlementAttempt => ({
  deployment_id: deploymentId,
  kind: "deposit",
  event_id: eventId,
  phase: "absorb",
  tx_hash: simTxHash(body).toString("hex"),
  signed_cbor: encodeSimTx(body).toString("hex"),
  required_outputs: [0],
  fee_inputs: [`${body.inputs[0]!.txHash.toString("hex")}#0`],
  status: "pending",
});

const insertJob = (eventId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
      VALUES (${deploymentId}, 'deposit', ${eventId}, 'absorb')`;
  });

const jobPhase = (eventId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ phase: string }>`SELECT phase FROM settlement_jobs
      WHERE deployment_id = ${deploymentId} AND event_id = ${eventId}`;
    return rows[0]?.phase;
  });

const storedStatus = (txHash: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ status: string }>`SELECT status
      FROM settlement_attempts WHERE tx_hash = ${txHash}`;
    return rows[0]?.status;
  });

describe("settlement confirmation from the intent journal", () => {
  it("reverts a cd-deep confirmation on a rollback deeper than cd, and stores final only once the prune step passes k", async () => {
    const { connectionString, run } = await nodeDatabase();
    const { driver, idle, journaled, pruneAll } =
      await followerOver(connectionString);
    await run(ownSettlement);
    const body = await journaled();
    const attempt = attemptOf(body, "01");
    await run(insertJob("01"));
    await run(insertJob("02"));
    await run(
      Journal.saveAttempt(owner, attempt, noOpenAttempt(owner, depths)),
    );
    const tick = () => run(settleAttempts(owner, depths));
    const level = async () =>
      settlementLevel(await run(readIntentStatus(attempt.tx_hash)), depths);
    // A second body reusing the attempt's fee coin, which a rollback restores.
    const reusing: Journal.SettlementAttempt = {
      ...attemptOf(body, "02"),
      tx_hash: "e2".repeat(32),
    };
    const saveReusing = () =>
      run(
        Effect.either(
          Journal.saveAttempt(owner, reusing, noOpenAttempt(owner, depths)),
        ),
      );

    // Live, then landed short of cd: open, and it blocks.
    expect((await tick())?.attempt.tx_hash).toBe(attempt.tx_hash);
    await driver.forward([body]);
    await idle(CD - 2);
    expect(await level()).toBe("open");
    expect((await tick())?.level).toBe("open");
    expect(await run(jobPhase("01"))).toBe("absorb");

    // cd deep: safe; the job takes the next phase and nothing blocks.
    await idle(2);
    expect(await level()).toBe("safe");
    expect(await tick()).toBeUndefined();
    expect(await run(jobPhase("01"))).toBe("complete");
    // A prune while it is not yet k deep stores nothing.
    await pruneAll();
    expect(await run(storedStatus(attempt.tx_hash))).toBe("pending");

    // A rollback deeper than cd un-lands it: the phase reverts and it
    // blocks again; no body may reuse its restored fee coin.
    await driver.backward(CD + 1);
    expect(await level()).toBe("open");
    expect((await tick())?.attempt.tx_hash).toBe(attempt.tx_hash);
    expect(await run(jobPhase("01"))).toBe("absorb");
    expect((await saveReusing())._tag).toBe("Left");
    expect(await run(storedStatus(reusing.tx_hash))).toBeUndefined();

    // Relanded and k deep: final by derivation, stored only by the prune
    // step, which prunes its journal entry in the same step.
    await driver.forward([body]);
    await idle(K);
    expect(await level()).toBe("final");
    expect(await tick()).toBeUndefined();
    expect(await run(jobPhase("01"))).toBe("complete");
    expect(await run(storedStatus(attempt.tx_hash))).toBe("pending");
    await pruneAll();
    expect(await run(readIntentStatus(attempt.tx_hash))).toBeNull();
    expect(await run(storedStatus(attempt.tx_hash))).toBe("final");
    expect(await tick()).toBeUndefined();
    expect(await run(jobPhase("01"))).toBe("complete");
    expect(await run(Journal.openAttempts(deploymentId))).toEqual([]);
  });
});

describe("migration 0015 on a database holding the old columns", () => {
  const legacyRows = (
    rows: readonly { hash: string; event: string; status: string }[],
  ) =>
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      for (const { hash, event, status } of rows) {
        yield* sql`INSERT INTO settlement_jobs
          (deployment_id, kind, event_id, phase, verified_generation)
          VALUES (${deploymentId}, 'deposit', ${event},
            ${status === "confirmed" ? "complete" : "absorb"},
            ${status === "confirmed" ? 0 : -1})`;
        yield* sql`INSERT INTO settlement_attempts
          (deployment_id, kind, event_id, phase, tx_hash, signed_cbor,
           required_outputs, fee_inputs, status, hold_slot, recovery)
          VALUES (${deploymentId}, 'deposit', ${event}, 'absorb', ${hash},
            'legacy-body', ARRAY[0], ARRAY[${`${hash}#9`}], ${status}, 10,
            false)`;
      }
    });
  const migrate = (run: Awaited<ReturnType<typeof nodeDatabase>>["run"]) =>
    run(
      Effect.gen(function* () {
        const status = yield* MigrationRunner.migrate({
          appVersion: "test",
          actor: "n6b-upgrade",
        });
        expect(status.actualVersion).toBe(MIGRATIONS.at(-1)!.version);
        yield* MigrationRunner.assertCompatible;
        const sql = yield* SqlClient.SqlClient;
        const columns = yield* sql<{ column_name: string }>`SELECT column_name
          FROM information_schema.columns
          WHERE table_name IN ('settlement_attempts', 'settlement_jobs')
            AND column_name IN ('recovery', 'verified_generation')`;
        expect(columns).toEqual([]);
        const refused = yield* Effect.either(
          sql`UPDATE settlement_attempts SET status = 'confirmed'`,
        );
        expect(refused._tag).toBe("Left");
      }),
    );

  it("reads a journaled confirmed attempt from the journal again and keeps an unjournaled one terminal", async () => {
    const { connectionString, run } = await nodeDatabase(LEGACY_VERSION);
    // The node ran its follower on this database before the upgrade.
    const { driver, idle, journaled } = await followerOver(connectionString);
    const body = await journaled();
    await driver.forward([body]);
    await idle(CD);
    const landed = simTxHash(body).toString("hex");
    const unjournaled = "f1".repeat(32);
    const pending = "f2".repeat(32);
    const expired = "f3".repeat(32);
    await run(
      legacyRows([
        { hash: landed, event: "01", status: "confirmed" },
        { hash: unjournaled, event: "02", status: "confirmed" },
        { hash: pending, event: "03", status: "pending" },
        { hash: expired, event: "04", status: "expired" },
      ]),
    );
    await migrate(run);
    expect(await run(storedStatus(landed))).toBe("pending");
    expect(await run(storedStatus(unjournaled))).toBe("final");
    expect(await run(storedStatus(pending))).toBe("pending");
    expect(await run(storedStatus(expired))).toBe("expired");

    // The migrated attempt's level is the journal's: landed CD + 1 deep.
    const status = await run(readIntentStatus(landed));
    expect(status).toMatchObject({ kind: "landed", depth: CD + 1 });
    expect(settlementLevel(status, depths)).toBe("safe");
    await run(ownSettlement);
    const tick = () => run(settleAttempts(owner, depths));
    // The legacy pending attempt (not journaled) is the oldest open one.
    expect((await tick())?.attempt.tx_hash).toBe(pending);
    expect(await run(jobPhase("01"))).toBe("complete");
    // A rollback past cd reverts what the stored `confirmed` never could.
    await driver.backward(CD + 1);
    expect(settlementLevel(await run(readIntentStatus(landed)), depths)).toBe(
      "open",
    );
    await tick();
    expect(await run(jobPhase("01"))).toBe("absorb");
    expect(await run(jobPhase("02"))).toBe("complete");
  });

  it("migrates a database that never ran a follower: every confirmed attempt stays terminal", async () => {
    const { run } = await nodeDatabase(LEGACY_VERSION);
    await run(
      legacyRows([
        { hash: "f4".repeat(32), event: "01", status: "confirmed" },
        { hash: "f5".repeat(32), event: "02", status: "pending" },
      ]),
    );
    await migrate(run);
    expect(await run(storedStatus("f4".repeat(32)))).toBe("final");
    expect(await run(storedStatus("f5".repeat(32)))).toBe("pending");
  });
});
