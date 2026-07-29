import "./utils.js";

import { createHash } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import { SqlClient } from "@effect/sql";
import { Cause, Deferred, Effect, Exit, Fiber, Ref, Scope } from "effect";
import { describe, expect, it } from "vitest";

import { MigrationRunner, TxAdmissionsDB } from "@/database/index.js";
import {
  admissionFailureDefinitelyDidNotInsert,
  commitAdmissionBacklogSlot,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "@/fibers/admission-backlog-gauge.js";
import {
  AdmissionWriter,
  admissionWriterShardForTxId,
  AdmissionWriterShutdownError,
  makeAdmissionWriterWithOptions,
} from "@/services/admission-writer.js";
import { Globals } from "@/services/globals.js";

import { provideDatabaseLayers } from "./utils.js";

const request = (
  label: string,
  txId: Buffer = createHash("sha256").update(`tx-id:${label}`).digest(),
): TxAdmissionsDB.ReservedAdmissionRequest => ({
  txId,
  txCanonicalCbor: Buffer.from(`canonical:${label}`),
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([]),
  submitSource: "native",
});

const requestsInShard = (shard: number, count: number) => {
  const requests: TxAdmissionsDB.ReservedAdmissionRequest[] = [];
  for (let index = 0; requests.length < count; index += 1) {
    const candidate = request(`shard-${shard.toString()}-${index.toString()}`);
    if (admissionWriterShardForTxId(candidate.txId) === shard) {
      requests.push(candidate);
    }
  }
  return requests;
};

const waitUntil = <E, R>(
  predicate: Effect.Effect<boolean, E, R>,
): Effect.Effect<void, E, R> =>
  Effect.gen(function* () {
    while (!(yield* predicate)) yield* Effect.sleep("1 millis");
  });

const withCleanAdmissions = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* MigrationRunner.migrate({
        appVersion: "admission-writer-test",
        actor: "admission-writer-test",
      });
      yield* sql`TRUNCATE TABLE tx_rejections, tx_admission_payloads, tx_admissions RESTART IDENTITY CASCADE`;
      return yield* effect;
    }),
  );

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

  it("coalesces parallel callers and fails every promise when the atomic batch rolls back", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const writer = yield* AdmissionWriter;
          const successful = yield* Effect.all(
            Array.from({ length: 256 }, (_, index) =>
              writer.admitReserved(request(`coalesced-${index.toString()}`)),
            ),
            { concurrency: "unbounded" },
          );
          expect(successful.every((result) => result.kind === "new")).toBe(
            true,
          );
          expect(yield* TxAdmissionsDB.countBacklog).toBe(256n);

          const sql = yield* SqlClient.SqlClient;
          yield* sql`TRUNCATE TABLE tx_admission_payloads, tx_admissions RESTART IDENTITY CASCADE`;
          const rollbackGood = request("rollback-good");
          const invalidTxId = Buffer.alloc(31, 7);
          for (
            let suffix = 0;
            admissionWriterShardForTxId(invalidTxId) !==
            admissionWriterShardForTxId(rollbackGood.txId);
            suffix += 1
          ) {
            invalidTxId[invalidTxId.length - 1] = suffix;
          }
          const rolledBack = yield* Effect.all(
            [
              writer.admitReserved(rollbackGood),
              writer.admitReserved(request("rollback-bad", invalidTxId)),
            ].map(Effect.either),
            { concurrency: "unbounded" },
          );
          expect(rolledBack.map((result) => result._tag)).toEqual([
            "Left",
            "Left",
          ]);
          expect(yield* TxAdmissionsDB.countBacklog).toBe(0n);
        }),
      ),
    );
  });

  it("fails the whole batch before acknowledgment on missing or extra outcomes", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const writer = yield* makeAdmissionWriterWithOptions(
          (requests) => {
            const label = requests[0]!.txCanonicalCbor.toString();
            if (label === "canonical:missing-outcome") {
              return Effect.succeed([]);
            }
            const conflict = {
              _tag: "Conflict" as const,
              error: new TxAdmissionsDB.TxAdmissionConflictError({
                txIdHex: requests[0]!.txId.toString("hex"),
                message: "controlled cardinality result",
              }),
            };
            return Effect.succeed([conflict, conflict]);
          },
          {
            shardCount: 1,
            batchMaxRows: 1,
            batchTargetRows: 1,
            batchDeadlineMs: 0,
            queueCapacity: 4,
          },
        );
        for (const label of ["missing-outcome", "extra-outcome"]) {
          const exit = yield* Effect.exit(writer.admitReserved(request(label)));
          expect(Exit.isFailure(exit)).toBe(true);
          if (Exit.isFailure(exit)) {
            expect(Cause.pretty(exit.cause)).toContain(
              "Admission microbatch outcome cardinality mismatch",
            );
          }
        }
        expect(yield* writer.stats).toMatchObject({ pending: 0 });
      }).pipe(Effect.scoped),
    );
  });

  it("preserves FIFO order and same-id conflict order within one lane", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const writer = yield* AdmissionWriter;
          const ordered = requestsInShard(0, 8);
          expect(
            ordered.map((entry) => admissionWriterShardForTxId(entry.txId)),
          ).toEqual(Array.from({ length: ordered.length }, () => 0));
          expect(
            admissionWriterShardForTxId(Buffer.from(ordered[0]!.txId)),
          ).toBe(0);

          const fibers = [];
          for (const entry of ordered) {
            fibers.push(yield* Effect.fork(writer.admitReserved(entry)));
            yield* Effect.yieldNow();
          }
          const outcomes = yield* Effect.forEach(fibers, Fiber.join);
          expect(outcomes.every((outcome) => outcome.kind === "new")).toBe(
            true,
          );
          const stored = yield* Effect.forEach(ordered, (entry) =>
            TxAdmissionsDB.getByTxId(entry.txId),
          );
          expect(stored.map((entry) => entry?.arrival_seq)).toEqual(
            [...stored]
              .map((entry) => entry!.arrival_seq)
              .sort((left, right) => (left < right ? -1 : 1)),
          );

          const winner = request("same-id-winner");
          const conflict = {
            ...winner,
            txCanonicalCbor: Buffer.from("same-id-conflict"),
          };
          expect(admissionWriterShardForTxId(winner.txId)).toBe(
            admissionWriterShardForTxId(conflict.txId),
          );
          const firstFiber = yield* Effect.fork(writer.admitReserved(winner));
          yield* Effect.yieldNow();
          const conflictFiber = yield* Effect.fork(
            Effect.either(writer.admitReserved(conflict)),
          );
          expect((yield* Fiber.join(firstFiber)).kind).toBe("new");
          const conflictResult = yield* Fiber.join(conflictFiber);
          expect(conflictResult._tag).toBe("Left");
          if (conflictResult._tag === "Left") {
            expect(conflictResult.left._tag).toBe("TxAdmissionConflictError");
          }
        }),
      ),
    );
  });

  it("consumes N reservations for identical requests but retains one backlog slot and one row", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const globals = yield* Globals;
          const writer = yield* AdmissionWriter;
          const identical = request("identical-reservations");
          const count = 32;
          const reservations = yield* Effect.all(
            Array.from({ length: count }, () =>
              reserveAdmissionBacklogSlot(100),
            ),
            { concurrency: "unbounded" },
          );
          expect(reservations.every((entry) => entry.reserved)).toBe(true);
          expect(
            (yield* Ref.get(globals.ADMISSION_BACKLOG_GAUGE))
              .ADMISSION_BACKLOG_IN_FLIGHT,
          ).toBe(BigInt(count));

          const admitted = yield* Effect.all(
            Array.from({ length: count }, () =>
              writer
                .admitReserved(identical)
                .pipe(
                  Effect.tap((result) =>
                    result.kind === "new"
                      ? commitAdmissionBacklogSlot
                      : releaseAdmissionBacklogSlot,
                  ),
                ),
            ),
            { concurrency: "unbounded" },
          );
          expect(
            admitted.filter((result) => result.kind === "new"),
          ).toHaveLength(1);
          expect(
            admitted.filter((result) => result.kind === "duplicate"),
          ).toHaveLength(count - 1);
          expect(yield* TxAdmissionsDB.countBacklog).toBe(1n);
          expect(
            (yield* TxAdmissionsDB.getByTxId(identical.txId))?.request_count,
          ).toBe(BigInt(count));
          expect(yield* Ref.get(globals.ADMISSION_BACKLOG_GAUGE)).toMatchObject(
            {
              ADMISSION_BACKLOG_LOCAL_DELTA: 1n,
              ADMISSION_BACKLOG_IN_FLIGHT: 0n,
            },
          );
        }).pipe(Effect.provide(Globals.Default)),
      ),
    );
  });

  it("releases known-unstarted shutdown reservations and retains durability-unknown ones", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const knownUnstarted = new AdmissionWriterShutdownError({
          message: "queued during shutdown",
          mayHaveCommitted: false,
        });
        const durabilityUnknown = new AdmissionWriterShutdownError({
          message: "in flight during shutdown",
          mayHaveCommitted: true,
        });
        expect(admissionFailureDefinitelyDidNotInsert(knownUnstarted)).toBe(
          true,
        );
        expect(admissionFailureDefinitelyDidNotInsert(durabilityUnknown)).toBe(
          false,
        );

        for (const error of [knownUnstarted, durabilityUnknown]) {
          expect((yield* reserveAdmissionBacklogSlot(10)).reserved).toBe(true);
          yield* Effect.fail(error).pipe(
            Effect.onExit((exit) => {
              const failure = Exit.isFailure(exit)
                ? Cause.failureOption(exit.cause)
                : undefined;
              const value =
                failure !== undefined && failure._tag === "Some"
                  ? failure.value
                  : undefined;
              return admissionFailureDefinitelyDidNotInsert(value)
                ? releaseAdmissionBacklogSlot
                : commitAdmissionBacklogSlot;
            }),
            Effect.either,
          );
        }
        expect(yield* Ref.get(globals.ADMISSION_BACKLOG_GAUGE)).toMatchObject({
          ADMISSION_BACKLOG_LOCAL_DELTA: 1n,
          ADMISSION_BACKLOG_IN_FLIGHT: 0n,
        });
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("fails in-flight and capacity-backpressured waiters on scoped shutdown", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const scope = yield* Scope.make();
        const persistStarted = yield* Deferred.make<void>();
        const persistRelease = yield* Deferred.make<void>();
        const writer = yield* Scope.extend(
          makeAdmissionWriterWithOptions(
            () =>
              Deferred.succeed(persistStarted, undefined).pipe(
                Effect.zipRight(Deferred.await(persistRelease)),
                Effect.as([]),
              ),
            {
              shardCount: 1,
              batchMaxRows: 1,
              batchTargetRows: 1,
              batchDeadlineMs: 100,
              queueCapacity: 1,
            },
          ),
          scope,
        );
        const fibers = yield* Effect.forEach(
          [
            request("shutdown-inflight"),
            request("shutdown-queued"),
            request("shutdown-backpressure"),
          ],
          (entry) =>
            Effect.forkDaemon(Effect.exit(writer.admitReserved(entry))),
        );
        yield* Deferred.await(persistStarted);
        yield* waitUntil(
          writer.stats.pipe(Effect.map((stats) => stats.pending === 3)),
        );
        const beforeShutdown = yield* writer.stats;
        expect(beforeShutdown).toMatchObject({
          accepting: true,
          pending: 3,
        });
        expect(beforeShutdown).toMatchObject({
          capacityUsed: 1,
          waitingCapacity: 2,
          queueDepths: [0],
        });

        yield* Scope.close(scope, Exit.void);
        const exits = yield* Effect.forEach(fibers, Fiber.join);
        const mayHaveCommitted: boolean[] = [];
        for (const exit of exits) {
          expect(Exit.isFailure(exit)).toBe(true);
          if (Exit.isFailure(exit)) {
            const failure = Cause.failureOption(exit.cause);
            expect(
              failure._tag === "Some" ? failure.value._tag : undefined,
            ).toBe("AdmissionWriterShutdownError");
            if (
              failure._tag === "Some" &&
              failure.value._tag === "AdmissionWriterShutdownError"
            ) {
              mayHaveCommitted.push(failure.value.mayHaveCommitted);
            }
          }
        }
        expect(mayHaveCommitted.sort()).toEqual([false, false, true]);
      }),
    );
  });

  it("continues queued writes after caller interruption without leaking bounded capacity", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const firstStarted = yield* Deferred.make<void>();
        const releaseFirst = yield* Deferred.make<void>();
        const persisted = yield* Ref.make<string[]>([]);
        let calls = 0;
        const writer = yield* makeAdmissionWriterWithOptions(
          (requests) =>
            Effect.gen(function* () {
              calls += 1;
              if (calls === 1) {
                yield* Deferred.succeed(firstStarted, undefined);
                yield* Deferred.await(releaseFirst);
              }
              yield* Ref.update(persisted, (labels) => [
                ...labels,
                ...requests.map((entry) => entry.txCanonicalCbor.toString()),
              ]);
              return requests.map((entry) => ({
                _tag: "Conflict" as const,
                error: new TxAdmissionsDB.TxAdmissionConflictError({
                  txIdHex: entry.txId.toString("hex"),
                  message: "controlled lifecycle persistence",
                }),
              }));
            }),
          {
            shardCount: 1,
            batchMaxRows: 1,
            batchTargetRows: 1,
            batchDeadlineMs: 100,
            queueCapacity: 3,
          },
        );
        const entries = [
          request("interrupt-inflight"),
          request("interrupt-queued"),
          request("interrupt-backpressured"),
        ];
        const first = yield* Effect.fork(writer.admitReserved(entries[0]!));
        yield* Deferred.await(firstStarted);
        const second = yield* Effect.fork(writer.admitReserved(entries[1]!));
        yield* waitUntil(
          writer.stats.pipe(Effect.map((stats) => stats.queueDepth === 1)),
        );
        const third = yield* Effect.fork(
          Effect.either(writer.admitReserved(entries[2]!)),
        );
        yield* waitUntil(
          writer.stats.pipe(Effect.map((stats) => stats.pending === 3)),
        );
        yield* Fiber.interrupt(first);
        yield* Fiber.interrupt(second);
        yield* Deferred.succeed(releaseFirst, undefined);
        expect((yield* Fiber.join(third))._tag).toBe("Left");
        yield* waitUntil(
          Ref.get(persisted).pipe(Effect.map((labels) => labels.length === 3)),
        );
        expect(yield* Ref.get(persisted)).toEqual(
          entries.map((entry) => entry.txCanonicalCbor.toString()),
        );
        yield* Effect.sleep("5 millis");
        expect(yield* writer.stats).toMatchObject({
          pending: 0,
          queueDepth: 0,
        });
      }).pipe(Effect.scoped),
    );
  });
});
