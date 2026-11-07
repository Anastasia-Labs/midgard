import { createHash } from "node:crypto";

import { it } from "@effect/vitest";
import { Cause, Deferred, Effect, Exit, Fiber, Ref, Scope } from "effect";
import { describe, expect } from "vitest";

import * as TxAdmissionsDB from "../src/database/txAdmissions.js";
import {
  admissionWriterShardForTxId,
  AdmissionWriterShutdownError,
  makeAdmissionWriterWithOptions,
} from "../src/services/admission-writer.js";

const request = (
  label: string,
  txId: Buffer = createHash("sha256").update(`pipeline:${label}`).digest(),
): TxAdmissionsDB.ReservedAdmissionRequest => ({
  txId,
  txCanonicalCbor: Buffer.from(label),
  submitSource: "native",
});

const requestsInLane = (lane: number, count: number, laneCount = 2) => {
  const requests: TxAdmissionsDB.ReservedAdmissionRequest[] = [];
  for (let index = 0; requests.length < count; index += 1) {
    const candidate = request(`lane-${lane.toString()}-${index.toString()}`);
    if (admissionWriterShardForTxId(candidate.txId, laneCount) === lane) {
      requests.push(candidate);
    }
  }
  return requests;
};

const conflicts = (
  requests: readonly TxAdmissionsDB.ReservedAdmissionRequest[],
): readonly TxAdmissionsDB.ReservedAdmissionOutcome[] =>
  requests.map((entry) => ({
    _tag: "Conflict" as const,
    error: new TxAdmissionsDB.TxAdmissionConflictError({
      txIdHex: entry.txId.toString("hex"),
      message: "controlled pipeline completion",
    }),
  }));

const waitUntil = (effect: Effect.Effect<boolean>): Effect.Effect<void> =>
  Effect.gen(function* () {
    while (!(yield* effect)) yield* Effect.yieldNow();
  });

const shutdownMayHaveCommitted = (
  exit: Exit.Exit<unknown, unknown>,
): boolean => {
  expect(Exit.isFailure(exit)).toBe(true);
  if (Exit.isSuccess(exit)) throw new Error("expected shutdown failure");
  const failure = Cause.failureOption(exit.cause);
  expect(failure._tag).toBe("Some");
  if (failure._tag === "None") throw new Error("missing shutdown failure");
  expect(failure.value).toBeInstanceOf(AdmissionWriterShutdownError);
  if (!(failure.value instanceof AdmissionWriterShutdownError)) {
    throw new Error("unexpected shutdown error");
  }
  return failure.value.mayHaveCommitted;
};

describe("two-lane admission writer pipeline", () => {
  it.effect("persists both deterministic FIFO lanes concurrently", () =>
    Effect.gen(function* () {
      const started = [
        yield* Deferred.make<void>(),
        yield* Deferred.make<void>(),
      ] as const;
      const release = yield* Deferred.make<void>();
      const active = yield* Ref.make(0);
      const maxActive = yield* Ref.make(0);
      const writer = yield* makeAdmissionWriterWithOptions(
        (requests) =>
          Effect.gen(function* () {
            const lane = admissionWriterShardForTxId(requests[0]!.txId, 2);
            const current = yield* Ref.updateAndGet(
              active,
              (value) => value + 1,
            );
            yield* Ref.update(maxActive, (value) => Math.max(value, current));
            yield* Deferred.succeed(started[lane]!, undefined);
            yield* Deferred.await(release);
            yield* Ref.update(active, (value) => value - 1);
            return conflicts(requests);
          }),
        {
          shardCount: 2,
          batchMaxRows: 1,
          queueBytes: 1_048_576,
          queueCapacity: 4,
        },
      );
      const callers = yield* Effect.forEach(
        [requestsInLane(0, 1)[0]!, requestsInLane(1, 1)[0]!],
        (entry) => Effect.fork(Effect.either(writer.admitReserved(entry))),
      );
      yield* Deferred.await(started[0]);
      yield* Deferred.await(started[1]);
      expect(yield* Ref.get(maxActive)).toBe(2);
      const stats = yield* writer.stats;
      expect(stats.lanes.map((lane) => lane.stages.persisting)).toEqual([1, 1]);
      yield* Deferred.succeed(release, undefined);
      yield* Effect.forEach(callers, Fiber.join);
    }).pipe(Effect.scoped),
  );

  it.effect(
    "groups everything that arrived during a write into the next batch",
    () =>
      Effect.gen(function* () {
        const firstPersistStarted = yield* Deferred.make<void>();
        const persistRelease = yield* Deferred.make<void>();
        const batches = yield* Ref.make<readonly (readonly string[])[]>([]);
        let persistCalls = 0;
        const writer = yield* makeAdmissionWriterWithOptions(
          (requests) =>
            Effect.gen(function* () {
              persistCalls += 1;
              yield* Ref.update(batches, (current) => [
                ...current,
                requests.map((entry) => entry.txCanonicalCbor.toString()),
              ]);
              if (persistCalls === 1) {
                yield* Deferred.succeed(firstPersistStarted, undefined);
                yield* Deferred.await(persistRelease);
              }
              return conflicts(requests);
            }),
          {
            shardCount: 1,
            batchMaxRows: 4,
            queueCapacity: 8,
            queueBytes: 1_048_576,
          },
        );
        const entries = Array.from({ length: 6 }, (_, index) =>
          request(`group-${index.toString()}`),
        );
        const callers: Fiber.RuntimeFiber<unknown, unknown>[] = [];
        callers.push(
          yield* Effect.fork(Effect.either(writer.admitReserved(entries[0]!))),
        );
        yield* Deferred.await(firstPersistStarted);
        for (const entry of entries.slice(1)) {
          callers.push(
            yield* Effect.fork(Effect.either(writer.admitReserved(entry))),
          );
        }
        yield* waitUntil(
          writer.stats.pipe(
            Effect.map((stats) => stats.lanes[0]!.stages.input === 5),
          ),
        );
        // While the persister is busy nothing is collected: the rows wait in
        // the input queue so the next batch can take all of them.
        expect((yield* writer.stats).lanes[0]!.stages).toMatchObject({
          input: 5,
          prepared: 0,
          persisting: 1,
        });
        yield* Deferred.succeed(persistRelease, undefined);
        yield* Effect.forEach(callers, Fiber.join);
        const labels = entries.map((entry) => entry.txCanonicalCbor.toString());
        expect(yield* Ref.get(batches)).toEqual([
          labels.slice(0, 1),
          labels.slice(1, 5),
          labels.slice(5),
        ]);
      }).pipe(Effect.scoped),
  );

  it.effect("continues persistence while completion is blocked", () =>
    Effect.gen(function* () {
      const completionRelease = yield* Deferred.make<void>();
      const persisted = yield* Ref.make<readonly string[]>([]);
      const writer = yield* makeAdmissionWriterWithOptions(
        (requests) =>
          Ref.update(persisted, (current) => [
            ...current,
            ...requests.map((entry) => entry.txCanonicalCbor.toString()),
          ]).pipe(Effect.as(conflicts(requests))),
        {
          shardCount: 1,
          batchMaxRows: 1,
          queueBytes: 1_048_576,
          queueCapacity: 4,
        },
        { beforeComplete: () => Deferred.await(completionRelease) },
      );
      const callers = yield* Effect.forEach(
        Array.from({ length: 3 }, (_, index) =>
          request(`completion-${index.toString()}`),
        ),
        (entry) => Effect.fork(Effect.either(writer.admitReserved(entry))),
      );
      yield* waitUntil(
        Ref.get(persisted).pipe(Effect.map((labels) => labels.length === 3)),
      );
      expect(yield* writer.stats).toMatchObject({
        pending: 3,
        capacityUsed: 3,
        lanes: [{ stages: { completion: 3, persisting: 0 } }],
      });
      yield* Deferred.succeed(completionRelease, undefined);
      yield* Effect.forEach(callers, Fiber.join);
    }).pipe(Effect.scoped),
  );

  it.effect(
    "refuses at once when every stage together holds the row bound",
    () =>
      Effect.gen(function* () {
        const completionRelease = yield* Deferred.make<void>();
        const persisted = yield* Ref.make(0);
        const writer = yield* makeAdmissionWriterWithOptions(
          (requests) =>
            Ref.update(persisted, (value) => value + requests.length).pipe(
              Effect.as(conflicts(requests)),
            ),
          {
            shardCount: 1,
            batchMaxRows: 1,
            queueCapacity: 2,
            queueBytes: 1_048_576,
          },
          { beforeComplete: () => Deferred.await(completionRelease) },
        );
        const firstTwo = yield* Effect.forEach(
          [request("capacity-0"), request("capacity-1")],
          (entry) => Effect.fork(Effect.either(writer.admitReserved(entry))),
        );
        yield* waitUntil(
          writer.stats.pipe(
            Effect.map(
              (stats) =>
                stats.capacityUsed === 2 &&
                stats.lanes[0]!.stages.completion === 2,
            ),
          ),
        );
        const refused = yield* Effect.either(
          writer.admitReserved(request("capacity-2")),
        );
        expect(refused).toMatchObject({
          _tag: "Left",
          left: {
            _tag: "AdmissionWriterFullError",
            unit: "rows",
            used: 2,
            capacity: 2,
          },
        });
        expect(yield* writer.stats).toMatchObject({
          pending: 2,
          capacity: 2,
          capacityUsed: 2,
        });
        yield* Deferred.succeed(completionRelease, undefined);
        yield* Effect.forEach(firstTwo, Fiber.join);
        // The freed rows take a new request.
        yield* Effect.either(writer.admitReserved(request("capacity-3")));
        expect(yield* Ref.get(persisted)).toBe(3);
        expect(yield* writer.stats).toMatchObject({
          capacityUsed: 0,
          bytesUsed: 0,
          pending: 0,
        });
      }).pipe(Effect.scoped),
  );

  it.effect("refuses a request that would overrun the byte bound", () =>
    Effect.gen(function* () {
      const completionRelease = yield* Deferred.make<void>();
      const persisted = yield* Ref.make<readonly string[]>([]);
      const writer = yield* makeAdmissionWriterWithOptions(
        (requests) =>
          Ref.update(persisted, (current) => [
            ...current,
            ...requests.map((entry) => entry.txCanonicalCbor.toString()),
          ]).pipe(Effect.as(conflicts(requests))),
        {
          shardCount: 1,
          batchMaxRows: 1,
          queueCapacity: 8,
          queueBytes: 10,
        },
        { beforeComplete: () => Deferred.await(completionRelease) },
      );
      const held = yield* Effect.fork(
        Effect.either(writer.admitReserved(request("sixsix"))),
      );
      yield* waitUntil(
        writer.stats.pipe(Effect.map((stats) => stats.bytesUsed === 6)),
      );
      // 6 held + 5 would be 11 bytes, one past the bound of 10.
      expect(
        yield* Effect.either(writer.admitReserved(request("fivee"))),
      ).toMatchObject({
        _tag: "Left",
        left: {
          _tag: "AdmissionWriterFullError",
          unit: "bytes",
          used: 6,
          capacity: 10,
        },
      });
      // 6 + 4 fills the bound exactly and is taken.
      const fits = yield* Effect.fork(
        Effect.either(writer.admitReserved(request("four"))),
      );
      yield* waitUntil(
        writer.stats.pipe(Effect.map((stats) => stats.bytesUsed === 10)),
      );
      yield* Deferred.succeed(completionRelease, undefined);
      yield* Effect.forEach([held, fits], Fiber.join);
      expect(yield* Ref.get(persisted)).toEqual(["sixsix", "four"]);
      expect(yield* writer.stats).toMatchObject({ bytesUsed: 0, pending: 0 });
    }).pipe(Effect.scoped),
  );

  it.effect("keeps same-id variants ordered across persistence batches", () =>
    Effect.gen(function* () {
      const batches = yield* Ref.make<readonly (readonly string[])[]>([]);
      const writer = yield* makeAdmissionWriterWithOptions(
        (requests) =>
          Ref.update(batches, (current) => [
            ...current,
            requests.map((entry) => entry.txCanonicalCbor.toString()),
          ]).pipe(Effect.as(conflicts(requests))),
        {
          shardCount: 2,
          batchMaxRows: 1,
          queueBytes: 1_048_576,
          queueCapacity: 4,
        },
      );
      const winner = request("same-id-winner");
      const conflicting = {
        ...winner,
        txCanonicalCbor: Buffer.from("same-id-conflicting"),
      };
      expect(admissionWriterShardForTxId(winner.txId, 2)).toBe(
        admissionWriterShardForTxId(conflicting.txId, 2),
      );
      const first = yield* Effect.fork(
        Effect.either(writer.admitReserved(winner)),
      );
      yield* waitUntil(
        Ref.get(batches).pipe(Effect.map((current) => current.length === 1)),
      );
      const second = yield* Effect.fork(
        Effect.either(writer.admitReserved(conflicting)),
      );
      yield* Effect.forEach([first, second], Fiber.join);
      expect(yield* Ref.get(batches)).toEqual([
        ["same-id-winner"],
        ["same-id-conflicting"],
      ]);
    }).pipe(Effect.scoped),
  );

  it.effect("fails input, inflight, and completion stages precisely", () =>
    Effect.gen(function* () {
      for (const stage of ["input", "inflight", "completion"] as const) {
        const statsStage: "input" | "persisting" | "completion" =
          stage === "inflight" ? "persisting" : stage;
        const scope = yield* Scope.make();
        const gate = yield* Deferred.make<void>();
        const writer = yield* Scope.extend(
          makeAdmissionWriterWithOptions(
            (requests) =>
              (stage === "inflight" ? Deferred.await(gate) : Effect.void).pipe(
                Effect.as(conflicts(requests)),
              ),
            {
              shardCount: 1,
              batchMaxRows: 1,
              queueBytes: 1_048_576,
              queueCapacity: 2,
            },
            {
              beforeCollect:
                stage === "input" ? () => Deferred.await(gate) : undefined,
              beforeComplete:
                stage === "completion" ? () => Deferred.await(gate) : undefined,
            },
          ),
          scope,
        );
        const caller = yield* Effect.forkDaemon(
          Effect.exit(writer.admitReserved(request(`shutdown-${stage}`))),
        );
        yield* waitUntil(
          writer.stats.pipe(
            Effect.map((stats) => stats.lanes[0]!.stages[statsStage] === 1),
          ),
        );
        yield* Scope.close(scope, Exit.void);
        expect(shutdownMayHaveCommitted(yield* Fiber.join(caller))).toBe(
          stage === "inflight" || stage === "completion",
        );
      }
    }),
  );
});
