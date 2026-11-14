/**
 * A real admission writer over the real admission statement and test
 * Postgres, with requests, a clean table and a way to hold the table locked.
 */
import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Fiber } from "effect";

import { MigrationRunner, TxAdmissionsDB } from "../../src/database/index.js";
import {
  type AdmissionWriteRequest,
  makeAdmissionWriterWithOptions,
} from "../../src/services/admission-writer.js";
import { AdmissionSql } from "../../src/services/database.js";
import { provideDatabaseLayers, resetApplicationTables } from "../utils.js";

export type Settlement = readonly [label: string, written: boolean];

/** A request for `label`, recording its settlement in `settlements`. */
export const writeRequest = (
  label: string,
  settlements: Settlement[],
  txId: Buffer = createHash("sha256").update(`tx-id:${label}`).digest(),
): AdmissionWriteRequest => {
  const txCanonicalCbor = Buffer.from(`canonical:${label}`);
  return {
    txId,
    txCanonicalCbor,
    submitSource: "native",
    txFullHash: createHash("sha256").update(txCanonicalCbor).digest(),
    settle: (written) =>
      Effect.sync(() => {
        settlements.push([label, written]);
      }),
    wakeOnCommit: Effect.void,
  };
};

/** Runs `effect` on the migrated test database with empty tables. */
export const withCleanAdmissions = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* MigrationRunner.migrate({
        appVersion: "admission-writer-test",
        actor: "admission-writer-test",
      });
      yield* resetApplicationTables;
      return yield* effect;
    }),
  );

/** A writer over the real admission statement, with test options. */
export const makeRealWriter = (
  options: Partial<Parameters<typeof makeAdmissionWriterWithOptions>[1]> = {},
) =>
  Effect.gen(function* () {
    const admissionSql = yield* AdmissionSql;
    return yield* makeAdmissionWriterWithOptions(
      (requests) =>
        TxAdmissionsDB.admitReservedBatch(requests).pipe(
          Effect.provideService(SqlClient.SqlClient, admissionSql),
        ),
      {
        shardCount: 1,
        batchMaxRows: 64,
        queueCapacity: 1_000,
        queueBytes: 1_048_576,
        ...options,
      },
    );
  });

/**
 * Holds ACCESS EXCLUSIVE on `tx_admissions` from another connection until
 * the returned effect runs: every read or write of the table waits.
 */
export const lockAdmissionsTable = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const locked = yield* Deferred.make<void>();
  const unlock = yield* Deferred.make<void>();
  const holder = yield* Effect.fork(
    sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`LOCK TABLE tx_admissions IN ACCESS EXCLUSIVE MODE`;
        yield* Deferred.succeed(locked, undefined);
        yield* Deferred.await(unlock);
      }),
    ),
  );
  yield* Deferred.await(locked);
  return Deferred.succeed(unlock, undefined).pipe(
    Effect.zipRight(Fiber.join(holder)),
    Effect.orDie,
  );
});

/** Must answer while the table is locked: a database read would wait. */
export const withinLock = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  effect.pipe(
    Effect.timeoutFail({
      duration: "5 seconds",
      onTimeout: () => new Error("waited on the locked admissions table"),
    }),
  );
