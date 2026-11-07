import { createHash } from "node:crypto";

import { it } from "@effect/vitest";
import {
  Deferred,
  Effect,
  Exit,
  HashMap,
  Logger,
  Metric,
  Ref,
  Scope,
} from "effect";
import { describe, expect } from "vitest";

import * as TxAdmissionsDB from "../src/database/txAdmissions.js";
import { DatabaseError } from "../src/database/utils/common.js";
import {
  admissionWriteLostCounter,
  type AdmissionWriteRequest,
  admissionWriteRetryCounter,
  admissionWriterShardForTxId,
  makeAdmissionWriterWithOptions,
} from "../src/services/admission-writer.js";

type Settlement = readonly [label: string, written: boolean];

const request = (
  label: string,
  settlements: Settlement[] = [],
  txId: Buffer = createHash("sha256").update(`pipeline:${label}`).digest(),
): AdmissionWriteRequest => {
  const txCanonicalCbor = Buffer.from(label);
  return {
    txId,
    txCanonicalCbor,
    txFullHash: createHash("sha256").update(txCanonicalCbor).digest(),
    submitSource: "native",
    settle: (written) =>
      Effect.sync(() => {
        settlements.push([label, written]);
      }),
    wakeOnCommit: Effect.void,
  };
};

const requestsInLane = (lane: number, count: number, laneCount = 2) => {
  const requests: AdmissionWriteRequest[] = [];
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

const counterValue = (counter: typeof admissionWriteRetryCounter) =>
  Metric.value(counter).pipe(Effect.map((state) => state.count));

type CapturedLog = {
  readonly message: string;
  readonly annotations: Record<string, unknown>;
};

/** A logger that records each message with its annotations. */
const capturingLogger = (logs: CapturedLog[]) =>
  Logger.replace(
    Logger.defaultLogger,
    Logger.make(({ message, annotations }) => {
      logs.push({
        message: Array.isArray(message)
          ? message.map(String).join(" ")
          : String(message),
        annotations: Object.fromEntries(HashMap.toEntries(annotations)),
      });
    }),
  );

const waitUntil = (effect: Effect.Effect<boolean>): Effect.Effect<void> =>
  Effect.gen(function* () {
    while (!(yield* effect)) yield* Effect.yieldNow();
  });

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
      for (const entry of [requestsInLane(0, 1)[0]!, requestsInLane(1, 1)[0]!])
        yield* writer.admit(entry);
      yield* Deferred.await(started[0]);
      yield* Deferred.await(started[1]);
      expect(yield* Ref.get(maxActive)).toBe(2);
      const stats = yield* writer.stats;
      expect(stats.lanes.map((lane) => lane.stages.persisting)).toEqual([1, 1]);
      yield* Deferred.succeed(release, undefined);
      yield* writer.flush;
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
        yield* writer.admit(entries[0]!);
        yield* Deferred.await(firstPersistStarted);
        for (const entry of entries.slice(1)) yield* writer.admit(entry);
        // While the persister is busy nothing is collected: the rows wait in
        // the input queue so the next batch can take all of them.
        expect((yield* writer.stats).lanes[0]!.stages).toMatchObject({
          input: 5,
          prepared: 0,
          persisting: 1,
        });
        yield* Deferred.succeed(persistRelease, undefined);
        yield* writer.flush;
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
      for (let index = 0; index < 3; index += 1)
        yield* writer.admit(request(`completion-${index.toString()}`));
      // Wait on the stage counts, not on the persisted labels: the last
      // batch is recorded inside the write, before it moves to completion.
      yield* waitUntil(
        writer.stats.pipe(
          Effect.map((stats) => stats.lanes[0]!.stages.completion === 3),
        ),
      );
      expect(yield* Ref.get(persisted)).toHaveLength(3);
      expect(yield* writer.stats).toMatchObject({
        pending: 3,
        capacityUsed: 3,
        lanes: [{ stages: { completion: 3, persisting: 0 } }],
      });
      yield* Deferred.succeed(completionRelease, undefined);
      yield* writer.flush;
      expect(yield* writer.stats).toMatchObject({ pending: 0 });
    }).pipe(Effect.scoped),
  );

  it.effect("refuses at once when the pending set holds the row bound", () =>
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
      yield* writer.admit(request("capacity-0"));
      yield* writer.admit(request("capacity-1"));
      const refused = yield* Effect.either(writer.admit(request("capacity-2")));
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
      yield* writer.flush;
      // The freed rows take a new request.
      yield* writer.admit(request("capacity-3"));
      yield* writer.flush;
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
      yield* writer.admit(request("sixsix"));
      // 6 held + 5 would be 11 bytes, one past the bound of 10.
      expect(
        yield* Effect.either(writer.admit(request("fivee"))),
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
      yield* writer.admit(request("four"));
      expect((yield* writer.stats).bytesUsed).toBe(10);
      yield* Deferred.succeed(completionRelease, undefined);
      yield* writer.flush;
      expect(yield* Ref.get(persisted)).toEqual(["sixsix", "four"]);
      expect(yield* writer.stats).toMatchObject({ bytesUsed: 0, pending: 0 });
    }).pipe(Effect.scoped),
  );

  it.effect(
    "answers a pending id from memory: same bytes duplicate, other bytes conflict",
    () =>
      Effect.gen(function* () {
        const persistRelease = yield* Deferred.make<void>();
        const batches = yield* Ref.make<readonly (readonly string[])[]>([]);
        const writer = yield* makeAdmissionWriterWithOptions(
          (requests) =>
            Deferred.await(persistRelease).pipe(
              Effect.zipRight(
                Ref.update(batches, (current) => [
                  ...current,
                  requests.map((entry) => entry.txCanonicalCbor.toString()),
                ]),
              ),
              Effect.as(conflicts(requests)),
            ),
          {
            shardCount: 2,
            batchMaxRows: 4,
            queueBytes: 1_048_576,
            queueCapacity: 4,
          },
        );
        const winner = request("same-id-winner");
        const conflicting = request("same-id-conflicting", [], winner.txId);
        const first = yield* writer.admit(winner);
        expect(first).toMatchObject({ kind: "new", status: "queued" });
        expect(writer.pendingSince(winner.txId)).toEqual(first.firstSeenAt);
        const duplicate = yield* writer.admit(request("same-id-winner"));
        expect(duplicate).toMatchObject({
          kind: "duplicate",
          status: "queued",
          firstSeenAt: first.firstSeenAt,
        });
        expect(yield* Effect.either(writer.admit(conflicting))).toMatchObject({
          _tag: "Left",
          left: { _tag: "TxAdmissionConflictError" },
        });
        yield* Deferred.succeed(persistRelease, undefined);
        yield* writer.flush;
        // Only the first bytes were ever handed to the database.
        expect(yield* Ref.get(batches)).toEqual([["same-id-winner"]]);
        expect(writer.pendingSince(winner.txId)).toBeUndefined();
      }).pipe(Effect.scoped),
  );

  it.live(
    "retries a transiently failed batch until it is written, keeping the txs pending",
    () =>
      Effect.gen(function* () {
        let attempts = 0;
        const retriesBefore = yield* counterValue(admissionWriteRetryCounter);
        const writer = yield* makeAdmissionWriterWithOptions(
          (requests) =>
            Effect.suspend(() => {
              attempts += 1;
              // Two serialization failures, then the write goes through.
              return attempts <= 2
                ? Effect.fail(
                    new DatabaseError({
                      message: "could not serialize access",
                      cause: { code: "40001" },
                      table: "tx_admissions",
                    }),
                  )
                : Effect.succeed(conflicts(requests));
            }),
          {
            shardCount: 1,
            batchMaxRows: 4,
            queueBytes: 1_048_576,
            queueCapacity: 4,
            retryBaseMs: 1,
            retryMaxMs: 2,
          },
        );
        yield* writer.admit(request("retry"));
        yield* writer.flush;
        expect(attempts).toBe(3);
        expect(
          (yield* counterValue(admissionWriteRetryCounter)) - retriesBefore,
        ).toBe(2);
        // The tx was not dropped: it reached a resolved outcome.
        expect(yield* writer.stats).toMatchObject({ pending: 0, poisoned: 0 });
      }).pipe(Effect.scoped),
  );

  it.live(
    "settles each stage's reservation on a stop that cannot flush in time",
    () =>
      Effect.gen(function* () {
        for (const stage of ["input", "inflight", "completion"] as const) {
          const scope = yield* Scope.make();
          const gate = yield* Deferred.make<void>();
          const settlements: Settlement[] = [];
          const logs: CapturedLog[] = [];
          const writer = yield* Scope.extend(
            makeAdmissionWriterWithOptions(
              (requests) =>
                (stage === "inflight"
                  ? Deferred.await(gate)
                  : Effect.void
                ).pipe(Effect.as(conflicts(requests))),
              {
                shardCount: 1,
                batchMaxRows: 1,
                queueBytes: 1_048_576,
                queueCapacity: 2,
                shutdownFlushMs: 50,
              },
              {
                beforeCollect:
                  stage === "input" ? () => Deferred.await(gate) : undefined,
                beforeComplete:
                  stage === "completion"
                    ? () => Deferred.await(gate)
                    : undefined,
              },
            ),
            scope,
            // The finalizer logs with the loggers it was registered under.
          ).pipe(Effect.provide(capturingLogger(logs)));
          const label = `shutdown-${stage}`;
          const entry = request(label, settlements);
          yield* writer.admit(entry);
          const statsStage = stage === "inflight" ? "persisting" : stage;
          yield* waitUntil(
            writer.stats.pipe(
              Effect.map((stats) => stats.lanes[0]!.stages[statsStage] === 1),
            ),
          );
          const lostBefore = yield* counterValue(admissionWriteLostCounter);
          yield* Scope.close(scope, Exit.void);
          // A row that may have committed keeps its backlog slot.
          expect(settlements).toEqual([
            [label, stage === "inflight" || stage === "completion"],
          ]);
          // The stop counts and names the tx it could not write.
          expect(
            (yield* counterValue(admissionWriteLostCounter)) - lostBefore,
          ).toBe(1);
          expect(logs).toContainEqual({
            message:
              "Admission writer stopped with 1 acknowledged txs unwritten.",
            annotations: { unwrittenTxIds: [entry.txId.toString("hex")] },
          });
          expect(
            yield* Effect.either(writer.admit(request(`${label}-late`))),
          ).toMatchObject({
            _tag: "Left",
            left: { _tag: "AdmissionWriterShutdownError" },
          });
        }
      }),
  );
});
