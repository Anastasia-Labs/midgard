import postgres from "postgres";
import { afterAll, beforeAll, expect, it, vi } from "vitest";

import { AcceptanceReadSockets } from "../src/devnet-stack/acceptance-payout-sources.js";
import { readAcceptanceSettlements } from "../src/devnet-stack/acceptance-payout-sql.js";
import type { RunEnv } from "../src/devnet-stack/layout.js";
import { collectorFixture } from "./devnet-stack-acceptance-payout-collector.fixtures.js";

const prefix = process.env.MIDGARD_TEST_DATABASE_PREFIX;
if (prefix === undefined || !/^[a-z][a-z0-9_]*$/u.test(prefix))
  throw new Error("payout SQL probe requires a safe unique test prefix");
const database = `${prefix}_sql_${process.pid}`;
// CI runs Postgres on POSTGRES_PORT (5432); the local test server is 5433.
const host = process.env.POSTGRES_HOST ?? "127.0.0.1";
const port = Number(process.env.POSTGRES_PORT ?? "5433");
const deployment = "33".repeat(32);
const maintenance = postgres({
  host,
  port,
  user: "postgres",
  password: "postgres",
  database: "postgres",
  max: 1,
  onnotice: () => {},
});
const sql = postgres({
  host,
  port,
  user: "postgres",
  password: "postgres",
  database,
  max: 1,
  onnotice: () => {},
});
const run = {
  postgresPort: port,
  postgresUser: "postgres",
  postgresPassword: "postgres",
  postgresDatabase: database,
} as RunEnv;
let created = false;
const noReaders = async () => {
  const deadline = Date.now() + 6000;
  for (;;) {
    const active =
      await maintenance`SELECT count(*)::text AS n FROM pg_stat_activity
      WHERE datname = ${database} AND application_name = 'midgard-exact-payout-reader'`;
    if (active[0]!.n === "0") return;
    if (Date.now() > deadline)
      throw new Error("read backend exceeded its5s statement budget");
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
};
beforeAll(async () => {
  if (process.env.MIDGARD_SKIP_DB_TESTS === "1") return;
  await maintenance.unsafe(`CREATE DATABASE "${database}"`);
  created = true;
  await sql`CREATE TABLE authority_fixture (singleton boolean, deployment_identity bytea, generation bigint, state text, lease_until timestamptz)`;
  await sql`INSERT INTO authority_fixture VALUES (true, decode(${deployment}, 'hex'), 9, 'ready', clock_timestamp() + interval '30 minutes')`;
  await sql.unsafe(`CREATE FUNCTION assert_read_only() RETURNS boolean LANGUAGE plpgsql AS $$ BEGIN
    IF current_setting('default_transaction_read_only') <> 'on'
       OR current_setting('transaction_read_only') <> 'on'
       OR current_setting('statement_timeout') <> '5s' THEN RAISE EXCEPTION 'read flags refused'; END IF;
    RETURN true; END $$`);
  await sql`CREATE VIEW event_history_authority AS SELECT * FROM authority_fixture WHERE assert_read_only()`;
  await sql`CREATE TABLE settlement_attempts (deployment_id text, kind text, event_id text,
    tx_hash text, phase text, signed_cbor text, required_outputs integer[], status text)`;
  for (const row of collectorFixture().snapshot.attempts)
    await sql`INSERT INTO settlement_attempts
    (deployment_id,kind,event_id,tx_hash,phase,signed_cbor,required_outputs,status)
    VALUES (${deployment}, 'withdrawal', ${row.event_id}, ${row.tx_hash}, ${row.phase}, ${row.signed_cbor}, ${row.required_outputs}, 'confirmed')`;
});
afterAll(async () => {
  await sql.end();
  if (created) {
    await noReaders();
    await maintenance.unsafe(`DROP DATABASE "${database}"`);
  }
  await maintenance.end();
});
const read = (
  fixture: ReturnType<typeof collectorFixture>,
  rows = 16,
  bytes = 16384,
) =>
  readAcceptanceSettlements(
    run,
    fixture.scope,
    deployment,
    fixture.records.map((row) => row.withdrawalEventId),
    rows,
    bytes,
  );

it.skipIf(process.env.MIDGARD_SKIP_DB_TESTS === "1")(
  "uses actual startup default-read-only plus transaction read-only and5s statement timeout",
  async () => {
    const result = await read(collectorFixture());
    expect(result.generation).toBe("9");
    expect(result.attempts).toHaveLength(16);
    expect(
      (await sql`SELECT count(*)::text AS n FROM settlement_attempts`)[0]!.n,
    ).toBe("16");
    await noReaders();
    expect(
      (
        await maintenance`SELECT count(*)::text AS n FROM pg_stat_activity WHERE datname = ${database}`
      )[0]!.n,
    ).toBe("1");
  },
);
it.skipIf(process.env.MIDGARD_SKIP_DB_TESTS === "1")(
  "refuses excess rows, oversized CBOR, foreign deployment and stale history before acknowledgement",
  async () => {
    await expect(read(collectorFixture(), 15)).rejects.toThrow(
      /read-only settlement source refused/,
    );
    await expect(read(collectorFixture(), 16, 1)).rejects.toThrow(
      /read-only settlement source refused/,
    );
    const fixture = collectorFixture();
    await expect(
      readAcceptanceSettlements(
        run,
        fixture.scope,
        "44".repeat(32),
        fixture.records.map((row) => row.withdrawalEventId),
        16,
        16384,
      ),
    ).rejects.toThrow(/refused/);
    await sql`UPDATE authority_fixture SET state = 'held'`;
    try {
      await expect(read(collectorFixture())).rejects.toThrow(/refused/);
    } finally {
      await sql`UPDATE authority_fixture SET state = 'ready'`;
    }
  },
);
it.skipIf(process.env.MIDGARD_SKIP_DB_TESTS === "1")(
  "destroys and joins an actual pending database read on native revocation",
  async () => {
    await sql.unsafe(
      `CREATE OR REPLACE FUNCTION assert_read_only() RETURNS boolean LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_sleep(20); RETURN true; END $$`,
    );
    const fixture = collectorFixture();
    const sockets: ReturnType<AcceptanceReadSockets["open"]>[] = [];
    const originalOpen = AcceptanceReadSockets.prototype.open;
    const opening = vi
      .spyOn(AcceptanceReadSockets.prototype, "open")
      .mockImplementation(function (this: AcceptanceReadSockets) {
        const socket = originalOpen.call(this);
        sockets.push(socket);
        return socket;
      });
    const pending = read(fixture);
    const refusal = expect(pending).rejects.toThrow(/refused/);
    try {
      for (;;) {
        const active =
          await maintenance`SELECT count(*)::text AS n FROM pg_stat_activity
        WHERE datname = ${database} AND query LIKE 'SELECT generation%' AND state = 'active'`;
        if (active[0]!.n === "1") break;
        if (Date.now() >= fixture.scope.deadlineEpochMs)
          throw new Error("fixture query did not start");
        await new Promise((resolve) => setTimeout(resolve, 10));
      }
      fixture.controller.abort();
      await refusal;
      expect(sockets.length).toBeGreaterThan(0);
      expect(sockets.every((socket) => socket.closed && socket.destroyed)).toBe(
        true,
      );
      // Client physical close is immediate; PostgreSQL may notice it after the5s statement timeout.
      await noReaders();
    } finally {
      opening.mockRestore();
      fixture.controller.abort();
      await pending.catch(() => {});
    }
  },
);
