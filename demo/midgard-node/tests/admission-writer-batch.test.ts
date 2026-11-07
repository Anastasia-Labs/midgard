/**
 * The admission writer's batch admit (#882), on the real admission statement
 * and test Postgres: one registration answers each request on its own, from
 * memory, and writes the registered ones.
 */
import "./utils.js";

import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { TxAdmissionsDB } from "../src/database/index.js";
import {
  lockAdmissionsTable,
  makeRealWriter,
  type Settlement,
  withCleanAdmissions,
  withinLock,
  writeRequest,
} from "./helpers/admission-writer-real.js";

describe("admission writer batch admit", () => {
  it("admits a batch in one registration: each request answers on its own, from memory", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const writer = yield* makeRealWriter({ queueCapacity: 3 });
          const settlements: Settlement[] = [];
          const first = writeRequest("batch-first", settlements);
          const conflict = writeRequest(
            "batch-conflict",
            settlements,
            first.txId,
          );
          const second = writeRequest("batch-second", settlements);
          const third = writeRequest("batch-third", settlements);
          const overflow = writeRequest("batch-overflow", settlements);

          const unlock = yield* lockAdmissionsTable;
          const answers = yield* withinLock(
            Effect.sync(() =>
              writer.admitBatch([
                first,
                first,
                conflict,
                second,
                third,
                overflow,
              ]),
            ),
          );
          expect(
            answers.map((answer) =>
              Either.isRight(answer) ? answer.right.kind : answer.left._tag,
            ),
          ).toEqual([
            "new",
            "duplicate",
            "TxAdmissionConflictError",
            "new",
            "new",
            "AdmissionWriterFullError",
          ]);
          expect((yield* writer.health).pending).toBe(3);

          yield* unlock;
          yield* writer.flush;
          // The in-batch duplicate counts as a second request for `first`.
          for (const [written, requests] of [
            [first, 2n],
            [second, 1n],
            [third, 1n],
          ] as const)
            expect(
              (yield* TxAdmissionsDB.getByTxId(written.txId))?.request_count,
            ).toBe(requests);
          expect(yield* TxAdmissionsDB.getByTxId(overflow.txId)).toBeNull();
          expect([...settlements].sort()).toEqual(
            [
              ["batch-first", true],
              ["batch-second", true],
              ["batch-third", true],
            ].sort(),
          );
        }).pipe(Effect.scoped),
      ),
    );
  });
});
