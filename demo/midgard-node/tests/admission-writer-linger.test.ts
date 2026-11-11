/**
 * The admission writer's batch linger, on the real admission statement and
 * test Postgres: a part-full batch waits briefly for more rows, so a steady
 * stream is written in few large batches, and nothing that must not wait on
 * a clock (a lone tx's write beyond the linger, `flush`, a stop) does.
 */

import { Effect, Exit, Scope } from "effect";
import { describe, expect, it } from "vitest";

import { TxAdmissionsDB } from "../src/database/index.js";
import {
  makeRealWriter,
  type Settlement,
  withCleanAdmissions,
  writeRequest,
} from "./helpers/admission-writer-real.js";

describe("admission writer batch linger", () => {
  it("writes a steady trickle in few batches, each lingering to fill", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const writer = yield* makeRealWriter({ lingerMs: 300 });
          const entries = Array.from({ length: 20 }, (_, index) =>
            writeRequest(`trickle-${index.toString()}`, []),
          );
          // One tx every 5 ms, slower than a commit: without the linger
          // each would go out in a batch of its own.
          for (const entry of entries) {
            yield* writer.admit(entry);
            yield* Effect.sleep("5 millis");
          }
          while ((yield* writer.stats).pending > 0) {
            yield* Effect.sleep("10 millis");
          }
          expect((yield* writer.stats).lanes[0]!.batches).toBeLessThanOrEqual(
            2,
          );
          for (const entry of entries) {
            expect(yield* TxAdmissionsDB.getByTxId(entry.txId)).not.toBeNull();
          }
        }).pipe(Effect.scoped),
      ),
    );
  });

  it("writes a lone tx within the linger and one commit", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const writer = yield* makeRealWriter({ lingerMs: 100 });
          const entry = writeRequest("lone-linger", []);
          const startedAt = Date.now();
          yield* writer.admit(entry);
          while (writer.pendingSince(entry.txId) !== undefined) {
            yield* Effect.sleep("5 millis");
          }
          expect(Date.now() - startedAt).toBeLessThan(100 + 2_000);
          expect(yield* TxAdmissionsDB.getByTxId(entry.txId)).not.toBeNull();
        }).pipe(Effect.scoped),
      ),
    );
  });

  it("does not make flush or a stop wait out the linger", async () => {
    await Effect.runPromise(
      withCleanAdmissions(
        Effect.gen(function* () {
          const scope = yield* Scope.make();
          const writer = yield* Scope.extend(
            makeRealWriter({ lingerMs: 600_000, shutdownFlushMs: 60_000 }),
            scope,
          );
          const settlements: Settlement[] = [];
          const fast = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
            effect.pipe(
              Effect.timeoutFail({
                duration: "5 seconds",
                onTimeout: () => new Error("waited out the linger"),
              }),
            );
          // Flush while the collector lingers on a part-full batch...
          const lingered = writeRequest("flush-lingered", settlements);
          yield* writer.admit(lingered);
          yield* Effect.sleep("50 millis");
          yield* fast(writer.flush);
          // ...and before it has taken the tx.
          const immediate = writeRequest("flush-immediate", settlements);
          yield* writer.admit(immediate);
          yield* fast(writer.flush);
          // A stop while the collector lingers writes the tx at once.
          const stopped = writeRequest("stop-lingered", settlements);
          yield* writer.admit(stopped);
          yield* Effect.sleep("50 millis");
          yield* fast(Scope.close(scope, Exit.void));

          expect(settlements).toEqual([
            ["flush-lingered", true],
            ["flush-immediate", true],
            ["stop-lingered", true],
          ]);
          for (const entry of [lingered, immediate, stopped]) {
            expect(yield* TxAdmissionsDB.getByTxId(entry.txId)).not.toBeNull();
          }
        }),
      ),
    );
  });
});
