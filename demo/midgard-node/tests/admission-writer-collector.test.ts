import { createHash } from "node:crypto";

import { it } from "@effect/vitest";
import { Deferred, Effect, Ref, TestClock } from "effect";
import { describe, expect } from "vitest";

import * as TxAdmissionsDB from "../src/database/txAdmissions.js";
import {
  ADMISSION_WRITE_BATCH_MAX_ROWS,
  type AdmissionWriteRequest,
  makeAdmissionWriterWithOptions,
} from "../src/services/admission-writer.js";

const request = (label: string): AdmissionWriteRequest => {
  const txCanonicalCbor = Buffer.from(label);
  return {
    txId: createHash("sha256").update(`collector:${label}`).digest(),
    txCanonicalCbor,
    txFullHash: createHash("sha256").update(txCanonicalCbor).digest(),
    submitSource: "native",
    settle: () => Effect.void,
    wakeOnCommit: Effect.void,
  };
};

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
          lingerMs: 0,
          queueCapacity: ADMISSION_WRITE_BATCH_MAX_ROWS + 2,
          queueBytes: 1_048_576,
        },
      );
      const labels = Array.from(
        { length: ADMISSION_WRITE_BATCH_MAX_ROWS + 2 },
        (_, index) => `fifo-${index.toString().padStart(3, "0")}`,
      );
      yield* writer.admit(request(labels[0]!));
      yield* Deferred.await(firstStarted);
      for (const label of labels.slice(1)) yield* writer.admit(request(label));
      yield* Deferred.succeed(release, undefined);
      yield* writer.flush;
      expect(yield* Ref.get(batches)).toEqual([
        labels.slice(0, 1),
        labels.slice(1, ADMISSION_WRITE_BATCH_MAX_ROWS + 1),
        labels.slice(ADMISSION_WRITE_BATCH_MAX_ROWS + 1),
      ]);
    }).pipe(Effect.scoped),
  );

  it.effect("writes a lone request once the linger has passed", () =>
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
          lingerMs: 10,
          queueCapacity: 128,
          queueBytes: 1_048_576,
        },
      );
      const yieldMany = Effect.gen(function* () {
        for (let step = 0; step < 1_000; step++) yield* Effect.yieldNow();
      });
      const ack = yield* writer.admit(request("lone"));
      expect(ack.kind).toBe("new");
      // The batch lingers for more rows while the test clock stands still.
      yield* yieldMany;
      expect(yield* Ref.get(calls)).toBe(0);
      yield* TestClock.adjust("10 millis");
      for (
        let step = 0;
        step < 1_000 && (yield* writer.stats).pending > 0;
        step++
      ) {
        yield* Effect.yieldNow();
      }
      expect(yield* Ref.get(calls)).toBe(1);
      expect((yield* writer.stats).pending).toBe(0);
    }).pipe(Effect.scoped),
  );

  it.effect("writes a batch that fills during the linger at once", () =>
    Effect.gen(function* () {
      const batches = yield* Ref.make<readonly number[]>([]);
      const writer = yield* makeAdmissionWriterWithOptions(
        (requests) =>
          Ref.update(batches, (current) => [...current, requests.length]).pipe(
            Effect.as(conflicts(requests)),
          ),
        {
          shardCount: 1,
          batchMaxRows: 4,
          lingerMs: 60_000,
          queueCapacity: 128,
          queueBytes: 1_048_576,
        },
      );
      yield* writer.admit(request("fill-0"));
      for (let step = 0; step < 100; step++) yield* Effect.yieldNow();
      // The collector holds one row and lingers; three more fill the batch.
      for (const label of ["fill-1", "fill-2", "fill-3"])
        yield* writer.admit(request(label));
      for (
        let step = 0;
        step < 1_000 && (yield* Ref.get(batches)).length === 0;
        step++
      ) {
        yield* Effect.yieldNow();
      }
      expect(yield* Ref.get(batches)).toEqual([4]);
    }).pipe(Effect.scoped),
  );
});
