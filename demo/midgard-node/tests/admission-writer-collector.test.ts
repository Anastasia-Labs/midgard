import { createHash } from "node:crypto";

import { it } from "@effect/vitest";
import { Deferred, Effect, Fiber, Ref } from "effect";
import { describe, expect } from "vitest";

import * as TxAdmissionsDB from "../src/database/txAdmissions.js";
import {
  ADMISSION_WRITE_BATCH_MAX_ROWS,
  makeAdmissionWriterWithOptions,
} from "../src/services/admission-writer.js";

const request = (label: string): TxAdmissionsDB.ReservedAdmissionRequest => ({
  txId: createHash("sha256").update(`collector:${label}`).digest(),
  txCanonicalCbor: Buffer.from(label),
  submitSource: "native",
});

const conflicts = (
  requests: readonly TxAdmissionsDB.ReservedAdmissionRequest[],
): readonly TxAdmissionsDB.ReservedAdmissionOutcome[] =>
  requests.map((entry) => ({
    _tag: "Conflict" as const,
    error: new TxAdmissionsDB.TxAdmissionConflictError({
      txIdHex: entry.txId.toString("hex"),
      message: "controlled collector completion",
    }),
  }));

describe("group-commit admission writer collector", () => {
  it.effect("takes up to batchMaxRows queued requests in one FIFO batch", () =>
    Effect.gen(function* () {
      const firstStarted = yield* Deferred.make<void>();
      const release = yield* Deferred.make<void>();
      const batches = yield* Ref.make<readonly (readonly string[])[]>([]);
      const writer = yield* makeAdmissionWriterWithOptions(
        (requests) =>
          Effect.gen(function* () {
            const seen = yield* Ref.updateAndGet(batches, (current) => [
              ...current,
              requests.map((entry) => entry.txCanonicalCbor.toString()),
            ]);
            if (seen.length === 1) {
              yield* Deferred.succeed(firstStarted, undefined);
              yield* Deferred.await(release);
            }
            return conflicts(requests);
          }),
        {
          shardCount: 1,
          batchMaxRows: ADMISSION_WRITE_BATCH_MAX_ROWS,
          queueCapacity: ADMISSION_WRITE_BATCH_MAX_ROWS + 2,
          queueBytes: 1_048_576,
        },
      );
      const labels = Array.from(
        { length: ADMISSION_WRITE_BATCH_MAX_ROWS + 2 },
        (_, index) => `fifo-${index.toString().padStart(3, "0")}`,
      );
      const fibers = [
        yield* Effect.fork(
          Effect.either(writer.admitReserved(request(labels[0]!))),
        ),
      ];
      yield* Deferred.await(firstStarted);
      for (const label of labels.slice(1)) {
        fibers.push(
          yield* Effect.fork(
            Effect.either(writer.admitReserved(request(label))),
          ),
        );
        yield* Effect.yieldNow();
      }
      yield* Deferred.succeed(release, undefined);
      yield* Effect.forEach(fibers, Fiber.join);
      expect(yield* Ref.get(batches)).toEqual([
        labels.slice(0, 1),
        labels.slice(1, ADMISSION_WRITE_BATCH_MAX_ROWS + 1),
        labels.slice(ADMISSION_WRITE_BATCH_MAX_ROWS + 1),
      ]);
    }).pipe(Effect.scoped),
  );

  it.effect("writes a lone request without waiting on a clock", () =>
    Effect.gen(function* () {
      const calls = yield* Ref.make(0);
      const writer = yield* makeAdmissionWriterWithOptions(
        (requests) =>
          Ref.update(calls, (value) => value + 1).pipe(
            Effect.as(conflicts(requests)),
          ),
        {
          shardCount: 1,
          batchMaxRows: ADMISSION_WRITE_BATCH_MAX_ROWS,
          queueCapacity: 128,
          queueBytes: 1_048_576,
        },
      );
      const caller = yield* Effect.fork(
        Effect.either(writer.admitReserved(request("lone"))),
      );
      // The TestClock never moves here, so a batch deadline would hold the
      // request forever; the write must happen within plain fiber yields.
      for (
        let step = 0;
        step < 1_000 && (yield* Ref.get(calls)) === 0;
        step++
      ) {
        yield* Effect.yieldNow();
      }
      expect(yield* Ref.get(calls)).toBe(1);
      expect((yield* Fiber.join(caller))._tag).toBe("Left");
    }).pipe(Effect.scoped),
  );
});
