// Main-thread CPU of durable admission (design D9): the admitter `/submit`
// and `/peer/txs` hand checked txs to, and the group-committing admission
// writer behind it, on the test Postgres. `bench:submit-main-thread` stubs
// the writer; this bench is the part it leaves out.
//
//   pnpm --dir demo --filter midgard-node run bench:admission-writer
//
// Needs the test Postgres (127.0.0.1:5433, `scripts/start-test-postgres.sh`);
// the bench creates its own test database there and drops it afterwards.
// Txs are offered one per admitter call, as single `/submit` requests are,
// at a fixed rate from a 1 ms timer on the main thread. Each tx carries
// distinct bytes, id, input and outputs, so every one is new and written.
// The main thread's CPU is read from /proc/<pid>/task/<pid>/stat from the
// first offer until the writer has flushed the last tx, and divided by the
// txs written: the admitter's registration plus everything the writer runs
// on the event loop for them (batching, the statement's encode and the
// result's parse, completion). The 1 ms driver's own cost is included; it
// is the same in every run.
//
// ADMISSION_WRITER_BENCH_RATE (default 5000/s), ADMISSION_WRITER_BENCH_SECONDS
// (default 10), ADMISSION_WRITER_BENCH_WARMUP_SECONDS (default 3, a run on
// its own txs left out of the numbers) and ADMISSION_WRITER_BENCH_TX_BYTES
// (default 400) set the load. ADMISSION_WRITER_BENCH_SHARDS and
// ADMISSION_WRITER_BENCH_LINGER_MS override the writer's shard count and
// batch linger; unset, the writer runs with the node's defaults.
// ADMISSION_WRITER_BENCH_OUTPUT names a file for the JSON row.

import { createHash, randomBytes } from "node:crypto";
import fs from "node:fs";
import path from "node:path";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Either, Redacted } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  cpuShares,
  sampleProcessCpu,
} from "../../scripts/submit-ingress-load.report.mjs";
import {
  type AdmissionCandidate,
  type CheckedTransactionsAdmitter,
  makeCheckedTransactionsAdmitter,
} from "../../src/commands/http/checked-transaction-admission.js";
import { MigrationRunner, TxAdmissionsDB } from "../../src/database/index.js";
import {
  AdmissionWriter,
  type AdmissionWriterOptions,
  type AdmissionWriterService,
  makeAdmissionWriterWithOptions,
} from "../../src/services/admission-writer.js";
import { DEFAULT_OPTIONS } from "../../src/services/admission-writer.validate-options.js";
import { AdmissionSql } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { testDatabaseName } from "../test-env.js";
import { provideDatabaseLayers, resetApplicationTables } from "../utils.js";

const RATE = Number(process.env.ADMISSION_WRITER_BENCH_RATE ?? "5000");
const SECONDS = Number(process.env.ADMISSION_WRITER_BENCH_SECONDS ?? "10");
const WARMUP_SECONDS = Number(
  process.env.ADMISSION_WRITER_BENCH_WARMUP_SECONDS ?? "3",
);
const TX_BYTES = Number(process.env.ADMISSION_WRITER_BENCH_TX_BYTES ?? "400");
const SHARDS = process.env.ADMISSION_WRITER_BENCH_SHARDS || undefined;
const LINGER_MS = process.env.ADMISSION_WRITER_BENCH_LINGER_MS || undefined;
const OUTPUT = process.env.ADMISSION_WRITER_BENCH_OUTPUT || undefined;

/** The main thread's CPU ticks, timed on the monotonic clock: the wall
 * clock can step under a VM. */
const sampleCpu = () => ({
  ...sampleProcessCpu(process.pid),
  atMs: performance.now(),
});

const writerOptions = (): AdmissionWriterOptions => ({
  ...DEFAULT_OPTIONS,
  ...(SHARDS === undefined ? {} : { shardCount: Number(SHARDS) }),
  ...(LINGER_MS === undefined ? {} : { lingerMs: Number(LINGER_MS) }),
});

/** Runs `statement` on the server's maintenance database. */
const onMaintenanceDatabase = (statement: string) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql.unsafe(statement);
    }).pipe(
      Effect.provide(
        PgClient.layer({
          host: process.env.POSTGRES_HOST ?? "127.0.0.1",
          port: Number(process.env.POSTGRES_PORT ?? "5433"),
          username: process.env.POSTGRES_USER ?? "postgres",
          password: Redacted.make(process.env.POSTGRES_PASSWORD ?? "postgres"),
          database: "postgres",
          maxConnections: 1,
        }),
      ),
    ),
  );

/** `count` distinct new txs; each spends an input no other tx spends. */
const makeCandidates = (label: string, count: number): AdmissionCandidate[] =>
  Array.from({ length: count }, (_, index) => {
    const txCanonicalCbor = randomBytes(TX_BYTES);
    const txId = createHash("sha256")
      .update(`${label}:${index.toString()}`)
      .update(txCanonicalCbor)
      .digest();
    const txIdHex = txId.toString("hex");
    return {
      txId,
      txIdHex,
      txCanonicalCbor,
      txFullHash: createHash("sha256").update(txCanonicalCbor).digest(),
      spentOutRefs: [
        `${createHash("sha256").update(`in:${txIdHex}`).digest("hex")}00`,
      ],
      producedOutRefs: [`${txIdHex}00`, `${txIdHex}01`],
    };
  });

/** Offers `candidates` one per admitter call at `RATE`; counts refusals. */
const drive = (
  admit: CheckedTransactionsAdmitter,
  candidates: readonly AdmissionCandidate[],
): Promise<{ readonly refused: number; readonly offeredMs: number }> =>
  new Promise((resolve) => {
    const startedAt = performance.now();
    let sent = 0;
    let refused = 0;
    const tick = () => {
      const due = Math.min(
        candidates.length,
        Math.floor(((performance.now() - startedAt) * RATE) / 1_000),
      );
      while (sent < due) {
        const [result] = admit([candidates[sent]!], "native");
        sent += 1;
        if (Either.isLeft(result!)) refused += 1;
      }
      if (sent < candidates.length) setTimeout(tick, 1);
      else resolve({ refused, offeredMs: performance.now() - startedAt });
    };
    tick();
  });

const batchTotals = (stats: AdmissionWriterService["stats"]) =>
  Effect.map(stats, ({ lanes }) =>
    lanes.reduce(
      (sum, lane) => ({
        batches: sum.batches + lane.batches,
        rows: sum.rows + lane.rows,
        commitMs:
          sum.commitMs + (lane.commitDurationMs.average ?? 0) * lane.batches,
      }),
      { batches: 0, rows: 0, commitMs: 0 },
    ),
  );

describe("admission writer main-thread cost", () => {
  // The bench runs without the test suite's global setup, so it provisions
  // its database shard the way that setup does, synchronous commits on.
  const database = testDatabaseName();
  beforeAll(async () => {
    await onMaintenanceDatabase(
      `DROP DATABASE IF EXISTS "${database}" WITH (FORCE)`,
    );
    await onMaintenanceDatabase(`CREATE DATABASE "${database}"`);
    await onMaintenanceDatabase(
      `ALTER DATABASE "${database}" SET synchronous_commit = on`,
    );
  });
  afterAll(() =>
    onMaintenanceDatabase(`DROP DATABASE IF EXISTS "${database}" WITH (FORCE)`),
  );

  it("measures main-thread CPU per admitted and written tx", async () => {
    const warmup = makeCandidates("warmup", Math.ceil(RATE * WARMUP_SECONDS));
    const measured = makeCandidates("measured", Math.ceil(RATE * SECONDS));
    const options = writerOptions();

    const row = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* MigrationRunner.migrate({
            appVersion: "admission-writer-bench",
            actor: "admission-writer-bench",
          });
          yield* resetApplicationTables;
          const admissionSql = yield* AdmissionSql;
          const writer = yield* makeAdmissionWriterWithOptions(
            (requests) =>
              TxAdmissionsDB.admitReservedBatch(requests).pipe(
                Effect.provideService(SqlClient.SqlClient, admissionSql),
              ),
            options,
          );
          const admit = yield* makeCheckedTransactionsAdmitter(
            Effect.void,
          ).pipe(Effect.provideService(AdmissionWriter, writer));

          if (warmup.length > 0) {
            const warm = yield* Effect.promise(() => drive(admit, warmup));
            expect(warm.refused).toBe(0);
            yield* writer.flush;
          }
          const totalsBefore = yield* batchTotals(writer.stats);
          const before = sampleCpu();
          const run = yield* Effect.promise(() => drive(admit, measured));
          yield* writer.flush;
          const after = sampleCpu();
          const totalsAfter = yield* batchTotals(writer.stats);

          const sql = yield* SqlClient.SqlClient;
          const [{ count }] = yield* sql<{
            count: string;
          }>`SELECT count(*)::text AS count FROM tx_admissions`;
          expect(run.refused).toBe(0);
          expect(Number(count)).toBe(warmup.length + measured.length);

          const shares = cpuShares(before, after);
          const batches = totalsAfter.batches - totalsBefore.batches;
          const rows = totalsAfter.rows - totalsBefore.rows;
          return {
            offeredRatePerSecond: RATE,
            achievedRatePerSecond: Math.round(
              (measured.length * 1_000) / run.offeredMs,
            ),
            seconds: Number(shares.seconds.toFixed(2)),
            txs: measured.length,
            txBytes: TX_BYTES,
            shards: options.shardCount,
            lingerMs: options.lingerMs ?? "default",
            mainThreadCores: Number(shares.mainThreadCores.toFixed(3)),
            processCores: Number(shares.processCores.toFixed(3)),
            mainThreadUsPerTx: Number(
              (
                (shares.mainThreadCores * shares.seconds * 1e6) /
                measured.length
              ).toFixed(1),
            ),
            batches,
            rowsPerBatch: Number((rows / batches).toFixed(1)),
            commitsPerSecond: Math.round((batches * 1_000) / run.offeredMs),
            averageCommitMs: Number(
              (
                (totalsAfter.commitMs - totalsBefore.commitMs) /
                batches
              ).toFixed(2),
            ),
          };
        }).pipe(Effect.provide(Globals.Default), Effect.scoped),
      ),
    );
    console.table([row]);
    console.log(`ADMISSION_WRITER_MAIN_THREAD ${JSON.stringify(row)}`);
    if (OUTPUT !== undefined) {
      fs.mkdirSync(path.dirname(path.resolve(OUTPUT)), { recursive: true });
      fs.writeFileSync(OUTPUT, `${JSON.stringify(row, null, 2)}\n`);
    }
  });
});
