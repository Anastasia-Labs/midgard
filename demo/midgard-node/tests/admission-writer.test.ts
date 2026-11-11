import { createHash } from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Exit, Fiber, MutableRef, Scope } from "effect";
import { describe, expect, it } from "vitest";

import { getTxStatusHandler } from "../src/commands/http/transaction-status.js";
import { MigrationRunner, TxAdmissionsDB } from "../src/database/index.js";
import {
  AdmissionWriter,
  type AdmissionWriteRequest,
  type AdmissionWriterService,
  makeAdmissionWriterWithOptions,
} from "../src/services/admission-writer.js";
import { NodeConfig } from "../src/services/config.js";
import { AdmissionSql } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import {
  makeNativeSubmitTx,
  submitThroughRouter,
  wrapNativeSubmitTx,
} from "./database.test/fixtures.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

type Settlement = readonly [label: string, written: boolean];

const request = (
  label: string,
  txId: Buffer = createHash("sha256").update(`tx-id:${label}`).digest(),
): TxAdmissionsDB.ReservedAdmissionRequest => ({
  txId,
  txCanonicalCbor: Buffer.from(`canonical:${label}`),
  submitSource: "native",
});

const writeRequest = (
  label: string,
  settlements: Settlement[],
  txId?: Buffer,
): AdmissionWriteRequest => {
  const reserved = request(label, txId);
  return {
    ...reserved,
    txFullHash: createHash("sha256").update(reserved.txCanonicalCbor).digest(),
    settle: (written) =>
      Effect.sync(() => {
        settlements.push([label, written]);
      }),
    wakeOnCommit: Effect.void,
  };
};

const withCleanAdmissions = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
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
const makeRealWriter = (
  options: Partial<Parameters<typeof makeAdmissionWriterWithOptions>[1]> = {},
  hooks: Parameters<typeof makeAdmissionWriterWithOptions>[2] = {},
  beforeWrite: Effect.Effect<void> = Effect.void,
) =>
  Effect.gen(function* () {
    const admissionSql = yield* AdmissionSql;
    return yield* makeAdmissionWriterWithOptions(
      (requests) =>
        beforeWrite.pipe(
          Effect.zipRight(TxAdmissionsDB.admitReservedBatch(requests)),
          Effect.provideService(SqlClient.SqlClient, admissionSql),
        ),
      {
        shardCount: 1,
        batchMaxRows: 64,
        queueCapacity: 1_000,
        queueBytes: 1_048_576,
        ...options,
      },
      hooks,
    );
  });

/**
 * Holds ACCESS EXCLUSIVE on `tx_admissions` from another connection until
 * the returned effect runs: every read or write of the table waits.
 */
const lockAdmissionsTable = Effect.gen(function* () {
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
const withinLock = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  effect.pipe(
    Effect.timeoutFail({
      duration: "5 seconds",
      onTimeout: () => new Error("waited on the locked admissions table"),
    }),
  );

const submit = (
  txCanonicalCbor: Buffer,
  writer?: AdmissionWriterService,
  flush = false,
) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const submitted = submitThroughRouter(
      wrapNativeSubmitTx(txCanonicalCbor),
      Effect.void,
      { flush },
    ).pipe(Effect.provideService(NodeConfig, nodeConfig));
    return yield* writer === undefined
      ? submitted
      : submitted.pipe(Effect.provideService(AdmissionWriter, writer));
  });

const txStatus = (txId: Buffer) =>
  Effect.gen(function* () {
    const response = yield* getTxStatusHandler.pipe(
      Effect.provideService(ParsedSearchParams, {
        tx_hash: txId.toString("hex"),
      }),
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make({
          kind: "derived",
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
    );
    if (response.body._tag !== "Uint8Array")
      throw new Error("expected a JSON body");
    return {
      status: response.status,
      body: JSON.parse(new TextDecoder().decode(response.body.body)) as Record<
        string,
        unknown
      >,
    };
  });

describe("durable admission microbatch writer", () => {
  it("preserves first-winner, duplicate, conflict, FIFO, and request-count semantics", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const first = request("first");
          const firstDuplicate = { ...first };
          const firstConflict = {
            ...first,
            txCanonicalCbor: Buffer.from("conflicting:first"),
          };
          const second = request("second");
          const outcomes = yield* TxAdmissionsDB.admitReservedBatch([
            first,
            firstDuplicate,
            firstConflict,
            second,
          ]);
          expect(
            outcomes.map((outcome) =>
              outcome._tag === "Success"
                ? outcome.result.kind
                : outcome.error._tag,
            ),
          ).toEqual(["new", "duplicate", "TxAdmissionConflictError", "new"]);
          const firstStored = yield* TxAdmissionsDB.getByTxId(first.txId);
          const secondStored = yield* TxAdmissionsDB.getByTxId(second.txId);
          expect(firstStored?.request_count).toBe(2n);
          expect(secondStored?.request_count).toBe(1n);
          expect(firstStored!.arrival_seq).toBeLessThan(
            secondStored!.arrival_seq,
          );

          // Existing persisted bytes win independently of which byte variant
          // arrives first in the later microbatch.
          const later = yield* TxAdmissionsDB.admitReservedBatch([
            firstConflict,
            first,
            firstDuplicate,
          ]);
          expect(
            later.map((outcome) =>
              outcome._tag === "Success"
                ? outcome.result.kind
                : outcome.error._tag,
            ),
          ).toEqual(["TxAdmissionConflictError", "duplicate", "duplicate"]);
          expect(
            (yield* TxAdmissionsDB.getByTxId(first.txId))?.request_count,
          ).toBe(4n);
        }),
      ),
    );
  });

  it("acknowledges /submit while Postgres cannot take the write, and answers duplicates from memory", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const writer = yield* AdmissionWriter;
          const globals = yield* Globals;
          const tx = makeNativeSubmitTx();

          const unlock = yield* lockAdmissionsTable;
          const accepted = yield* withinLock(submit(tx.txCanonicalCbor));
          expect(accepted).toMatchObject({
            status: 202,
            body: { txId: tx.txIdHex, status: "queued", duplicate: false },
          });
          expect(writer.pendingSince(tx.txId)).toBeInstanceOf(Date);
          // The pending tx reads as queued without touching the table.
          expect(yield* withinLock(txStatus(tx.txId))).toMatchObject({
            status: 200,
            body: { txId: tx.txIdHex, status: "queued" },
          });
          const pendingDuplicate = yield* withinLock(
            submit(tx.txCanonicalCbor),
          );
          expect(pendingDuplicate).toMatchObject({
            status: 200,
            body: {
              status: "queued",
              duplicate: true,
              firstSeenAt: accepted.body.firstSeenAt,
            },
          });
          yield* unlock;

          yield* writer.flush;
          expect(writer.pendingSince(tx.txId)).toBeUndefined();
          expect(yield* TxAdmissionsDB.getByTxId(tx.txId)).toMatchObject({
            status: "queued",
            payload_bytes: tx.txCanonicalCbor.length,
          });
          // The written row took its backlog reservation for good.
          expect(MutableRef.get(globals.ADMISSION_BACKLOG_GAUGE)).toMatchObject(
            {
              ADMISSION_BACKLOG_LOCAL_DELTA: 1n,
              ADMISSION_BACKLOG_IN_FLIGHT: 0n,
            },
          );

          // A recently written tx is answered from memory as well.
          const unlockAgain = yield* lockAdmissionsTable;
          expect(yield* withinLock(submit(tx.txCanonicalCbor))).toMatchObject({
            status: 200,
            body: {
              duplicate: true,
              firstSeenAt: accepted.body.firstSeenAt,
            },
          });
          yield* unlockAgain;
        }).pipe(Effect.provide(Globals.Default)),
      ),
    );
  });

  it("turns stalled and full while Postgres is locked, refuses with 503, and recovers by itself", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const writer = yield* makeRealWriter({
            queueCapacity: 2,
            stallMs: 200,
          });
          const txs = Array.from({ length: 4 }, (_, index) =>
            makeNativeSubmitTx(BigInt(index + 1)),
          );

          const unlock = yield* lockAdmissionsTable;
          for (const tx of txs.slice(0, 2)) {
            expect(
              (yield* withinLock(submit(tx.txCanonicalCbor, writer))).status,
            ).toBe(202);
          }
          expect(
            yield* withinLock(submit(txs[2]!.txCanonicalCbor, writer)),
          ).toEqual({
            status: 503,
            body: {
              error: "Durable submission admission writer is full",
              unit: "rows",
            },
            retryAfter: "5",
          });
          yield* Effect.sleep("300 millis");
          const stalled = yield* writer.health;
          expect(stalled).toMatchObject({ stalled: true, pending: 2 });
          const refused = yield* withinLock(
            submit(txs[3]!.txCanonicalCbor, writer),
          );
          expect(refused).toMatchObject({
            status: 503,
            body: {
              error: "Durable submission admission writes are stalled",
              stallMs: 200,
            },
            retryAfter: "5",
          });
          expect(refused.body.oldestPendingAgeMs).toBeGreaterThan(200);

          yield* unlock;
          yield* writer.flush;
          expect(yield* writer.health).toMatchObject({
            stalled: false,
            pending: 0,
          });
          expect(
            (yield* submit(txs[3]!.txCanonicalCbor, writer, true)).status,
          ).toBe(202);
          for (const tx of [txs[0]!, txs[1]!, txs[3]!]) {
            expect(yield* TxAdmissionsDB.getByTxId(tx.txId)).not.toBeNull();
          }
          expect(yield* TxAdmissionsDB.getByTxId(txs[2]!.txId)).toBeNull();
        }).pipe(Effect.provide(Globals.Default), Effect.scoped),
      ),
    );
  });

  it("drops the one row Postgres refuses and writes the rest of its batch", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const gate = yield* Deferred.make<void>();
          const writer = yield* makeRealWriter(
            {},
            { beforePersist: () => Deferred.await(gate) },
          );
          const settlements: Settlement[] = [];
          const good = ["poison-good-0", "poison-good-1", "poison-good-2"].map(
            (label) => writeRequest(label, settlements),
          );
          // tx_admissions checks octet_length(tx_id) = 32: this row can never
          // be written, whatever the retry.
          const bad = writeRequest(
            "poison-bad",
            settlements,
            Buffer.alloc(31, 7),
          );
          for (const entry of [good[0]!, bad, good[1]!, good[2]!]) {
            expect((yield* writer.admit(entry)).kind).toBe("new");
          }
          yield* Deferred.succeed(gate, undefined);
          yield* writer.flush;

          for (const entry of good) {
            expect(yield* TxAdmissionsDB.getByTxId(entry.txId)).not.toBeNull();
          }
          expect(writer.pendingSince(bad.txId)).toBeUndefined();
          expect(yield* writer.stats).toMatchObject({
            poisoned: 1,
            pending: 0,
          });
          expect([...settlements].sort()).toEqual(
            [
              ["poison-bad", false],
              ["poison-good-0", true],
              ["poison-good-1", true],
              ["poison-good-2", true],
            ].sort(),
          );
          // The shard keeps writing.
          const after = writeRequest("poison-after", settlements);
          yield* writer.admit(after);
          yield* writer.flush;
          expect(yield* TxAdmissionsDB.getByTxId(after.txId)).not.toBeNull();
        }).pipe(Effect.scoped),
      ),
    );
  });

  it("stops answering a written id from memory once its row may have been pruned", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          // The node passes ADMISSION_TERMINAL_RETENTION_MS here.
          const writer = yield* makeRealWriter({ recentTxIdMaxAgeMs: 300 });
          const entry = writeRequest("pruned-later", []);
          yield* writer.admit(entry);
          yield* writer.flush;
          expect(writer.lookup(entry.txId, entry.txFullHash)).toMatchObject({
            _tag: "Duplicate",
          });
          yield* Effect.sleep("400 millis");
          // Past the retention the row may be gone, so the resubmission
          // must reach the database to be admitted and validated again.
          expect(writer.lookup(entry.txId, entry.txFullHash)).toEqual({
            _tag: "Unknown",
          });
        }).pipe(Effect.scoped),
      ),
    );
  });

  it("stops within its flush bound while Postgres is locked", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const scope = yield* Scope.make();
          const writer = yield* Scope.extend(
            makeRealWriter({ shutdownFlushMs: 200 }),
            scope,
          );
          const settlements: Settlement[] = [];
          const unlock = yield* lockAdmissionsTable;
          yield* writer.admit(writeRequest("locked-stop", settlements));
          while ((yield* writer.stats).lanes[0]!.stages.persisting !== 1) {
            yield* Effect.sleep("1 millis");
          }
          // The write waits on the lock; the stop must still end, settling
          // the tx as possibly committed so its backlog slot is kept.
          yield* Scope.close(scope, Exit.void).pipe(
            Effect.timeoutFail({
              duration: "10 seconds",
              onTimeout: () => new Error("the stop hung on the locked table"),
            }),
          );
          expect(settlements).toEqual([["locked-stop", true]]);
          yield* unlock;
        }),
      ),
    );
  });

  it("writes every pending tx on a graceful stop", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const scope = yield* Scope.make();
          // Each batch takes a while, so the stop finds most txs pending.
          const writer = yield* Scope.extend(
            makeRealWriter({ batchMaxRows: 4 }, {}, Effect.sleep("20 millis")),
            scope,
          );
          const settlements: Settlement[] = [];
          const entries = Array.from({ length: 20 }, (_, index) =>
            writeRequest(`graceful-${index.toString()}`, settlements),
          );
          for (const entry of entries) yield* writer.admit(entry);
          expect((yield* writer.stats).pending).toBeGreaterThan(4);

          yield* Scope.close(scope, Exit.void);

          expect(settlements).toHaveLength(20);
          expect(settlements.every(([, written]) => written)).toBe(true);
          for (const entry of entries) {
            expect(yield* TxAdmissionsDB.getByTxId(entry.txId)).not.toBeNull();
          }
        }),
      ),
    );
  });
});
