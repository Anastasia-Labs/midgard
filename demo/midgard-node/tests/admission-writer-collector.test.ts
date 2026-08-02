import { createHash } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import { it } from "@effect/vitest";
import { Deferred, Duration, Effect, Fiber, Ref, TestClock } from "effect";
import { describe, expect } from "vitest";

import * as TxAdmissionsDB from "@/database/txAdmissions.js";
import {
  ADMISSION_WRITE_BATCH_DEADLINE_MS,
  ADMISSION_WRITE_BATCH_MAX_ROWS,
  ADMISSION_WRITE_BATCH_TARGET_ROWS,
  makeAdmissionWriterWithOptions,
} from "@/services/admission-writer.js";

const request = (label: string): TxAdmissionsDB.ReservedAdmissionRequest => ({
  txId: createHash("sha256").update(`collector:${label}`).digest(),
  txCanonicalCbor: Buffer.from(label),
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([]),
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

describe("adaptive admission writer collector", () => {
  it.effect(
    "flushes immediately at the target and preserves FIFO request order",
    () =>
      Effect.gen(function* () {
        const captured = yield* Deferred.make<readonly string[]>();
        const writer = yield* makeAdmissionWriterWithOptions(
          (requests) =>
            Deferred.succeed(
              captured,
              requests.map((entry) => entry.txCanonicalCbor.toString()),
            ).pipe(Effect.as(conflicts(requests))),
          {
            shardCount: 1,
            batchMaxRows: ADMISSION_WRITE_BATCH_MAX_ROWS,
            batchTargetRows: ADMISSION_WRITE_BATCH_TARGET_ROWS,
            batchDeadlineMs: ADMISSION_WRITE_BATCH_DEADLINE_MS,
            queueCapacity: 128,
          },
        );
        const labels = Array.from(
          { length: ADMISSION_WRITE_BATCH_TARGET_ROWS },
          (_, index) => `fifo-${index.toString().padStart(2, "0")}`,
        );
        const fibers = [];
        for (const label of labels) {
          fibers.push(
            yield* Effect.fork(
              Effect.either(writer.admitReserved(request(label))),
            ),
          );
          yield* Effect.yieldNow();
        }
        expect(yield* Deferred.await(captured)).toEqual(labels);
        expect(
          (yield* Effect.forEach(fibers, Fiber.join)).every(
            (result) => result._tag === "Left",
          ),
        ).toBe(true);
        const stats = yield* writer.stats;
        expect(stats.shards[0]).toMatchObject({
          batches: 1,
          rows: ADMISSION_WRITE_BATCH_TARGET_ROWS,
          maxRowsPerBatch: ADMISSION_WRITE_BATCH_TARGET_ROWS,
          batchSizeCounts: {
            [ADMISSION_WRITE_BATCH_TARGET_ROWS.toString()]: 1,
          },
        });
      }).pipe(Effect.scoped),
  );

  it.effect("flushes a partial batch only at the absolute deadline", () =>
    Effect.gen(function* () {
      const captured = yield* Deferred.make<readonly string[]>();
      const calls = yield* Ref.make(0);
      const writer = yield* makeAdmissionWriterWithOptions(
        (requests) =>
          Ref.update(calls, (value) => value + 1).pipe(
            Effect.zipRight(
              Deferred.succeed(
                captured,
                requests.map((entry) => entry.txCanonicalCbor.toString()),
              ),
            ),
            Effect.as(conflicts(requests)),
          ),
        {
          shardCount: 1,
          batchMaxRows: ADMISSION_WRITE_BATCH_MAX_ROWS,
          batchTargetRows: ADMISSION_WRITE_BATCH_TARGET_ROWS,
          batchDeadlineMs: ADMISSION_WRITE_BATCH_DEADLINE_MS,
          queueCapacity: 128,
        },
      );
      const caller = yield* Effect.fork(
        Effect.either(writer.admitReserved(request("partial"))),
      );
      yield* Effect.yieldNow();
      yield* TestClock.adjust(
        Duration.millis(ADMISSION_WRITE_BATCH_DEADLINE_MS - 1),
      );
      expect(yield* Ref.get(calls)).toBe(0);
      yield* TestClock.adjust("1 millis");
      expect(yield* Deferred.await(captured)).toEqual(["partial"]);
      expect((yield* Fiber.join(caller))._tag).toBe("Left");
      expect(yield* Ref.get(calls)).toBe(1);
    }).pipe(Effect.scoped),
  );
});
