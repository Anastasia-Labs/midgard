import { Deferred, Effect, Fiber, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  makeOutput,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { publishCommitMempoolLedgerMutation } from "../src/fibers/block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import { Globals } from "../src/services/globals.globals.js";
import { markMempoolLedgerStale } from "../src/services/globals.with-l1-control-plane-held.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.make-mempool-ledger-cache-service.js";
import {
  type CommitWorkerMessage,
  takeCommitWorkerOutput,
} from "../src/workers/commit-worker-message.js";
import {
  mempoolLedgerRevertedNotice,
  type WorkerOutput,
} from "../src/workers/utils/commit-block-header.js";
import {
  fullReloads,
  keyedReadsOver,
  noKeyedReads,
  readAt,
  row,
} from "./mempool-ledger-cache.row.js";

type Rows = readonly MempoolLedgerDB.EntryWithTimeStamp[];

const hex = (outref: Buffer) => outref.toString("hex");

const readFailed = new DatabaseError({
  table: MempoolLedgerDB.tableName,
  message: "read refused",
  cause: "test",
});

const unmarked = { all: false, keys: new Set<string>() };

describe("mempool ledger cache stale marks", () => {
  it("reads again at its key a row marked while the full load runs", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const added = outRefFromByte(0x70);
        const output = makeOutput(12n);
        let rows: Rows = [];
        let loads = 0;
        const reads = keyedReadsOver(() => rows);
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.gen(function* () {
            loads += 1;
            const captured = rows;
            // Another writer commits after the load's snapshot, then marks.
            rows = [row(added, output)];
            yield* markMempoolLedgerStale(globals, [hex(added)]);
            return captured;
          }),
          reads.spendableAt,
        );

        const state = yield* readAt(service, [added]);
        expect(state.get(hex(added))).toEqual(output);
        expect(loads).toBe(1);
        expect(reads.asked).toStrictEqual([[hex(added)]]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("loads again when every key is marked during the full load", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const added = outRefFromByte(0x6f);
        const output = makeOutput(13n);
        let loads = 0;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.gen(function* () {
            loads += 1;
            if (loads > 1) return [row(added, output)];
            yield* markMempoolLedgerStale(globals, "all");
            return [];
          }),
          noKeyedReads,
        );

        const state = yield* readAt(service, [added]);
        expect(loads).toBe(2);
        expect(state.get(hex(added))).toEqual(output);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("reads a marked key again without a full load, and loads in full when every key is marked", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const first = outRefFromByte(0x71);
        const second = outRefFromByte(0x72);
        const output = makeOutput(10n);
        let rows: Rows = [row(first, output)];
        const reads = keyedReadsOver(() => rows);
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => rows),
          reads.spendableAt,
        );
        yield* readAt(service, [first]);
        const reloadsAfterLoad = yield* fullReloads;

        rows = [row(first, output), row(second, output)];
        yield* markMempoolLedgerStale(globals, [hex(second)]);
        expect([...(yield* readAt(service, [first, second])).keys()]).toEqual([
          hex(first),
          hex(second),
        ]);
        expect(reads.asked).toStrictEqual([[hex(second)]]);
        expect(yield* fullReloads).toBe(reloadsAfterLoad);

        rows = [row(second, output)];
        yield* markMempoolLedgerStale(globals, "all");
        expect([...(yield* readAt(service, [first, second])).keys()]).toEqual([
          hex(second),
        ]);
        expect(reads.asked).toHaveLength(1);
        expect(yield* fullReloads).toBe(reloadsAfterLoad + 1n);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("leaves the keys of a failed read marked, and every key after a failed full load", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const spent = outRefFromByte(0x73);
        const added = outRefFromByte(0x74);
        const output = makeOutput(11n);
        let rows: Rows = [row(spent, output)];
        let failKeyed = false;
        let failLoad = false;
        const reads = keyedReadsOver(() => rows);
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.suspend(() =>
            failLoad
              ? ((failLoad = false), Effect.fail(readFailed))
              : Effect.succeed(rows),
          ),
          (keys) =>
            failKeyed
              ? ((failKeyed = false), Effect.fail(readFailed))
              : reads.spendableAt(keys),
        );
        yield* readAt(service, [spent]);
        const view = Effect.map(service.spendableView, (holds) => [
          holds(hex(spent)),
          holds(hex(added)),
        ]);

        rows = [];
        yield* markMempoolLedgerStale(globals, [hex(spent)]);
        failKeyed = true;
        expect((yield* Effect.either(readAt(service, [spent])))._tag).toBe(
          "Left",
        );
        expect(yield* view).toStrictEqual([undefined, false]);
        expect((yield* readAt(service, [spent])).size).toBe(0);
        expect(reads.asked).toStrictEqual([[hex(spent)]]);

        rows = [row(added, output)];
        yield* markMempoolLedgerStale(globals, "all");
        failLoad = true;
        expect((yield* Effect.either(readAt(service, [added])))._tag).toBe(
          "Left",
        );
        expect(yield* view).toStrictEqual([undefined, undefined]);
        expect([...(yield* readAt(service, [spent, added])).keys()]).toEqual([
          hex(added),
        ]);
        expect(yield* view).toStrictEqual([false, true]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("the spendable view cannot tell at a marked key or one being read again, or anywhere while every key is marked or being loaded again", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const spent = outRefFromByte(0x75);
        const held = outRefFromByte(0x76);
        const absent = outRefFromByte(0x77);
        const output = makeOutput(14n);
        let rows: Rows = [row(spent, output), row(held, output)];
        const reads = keyedReadsOver(() => rows);
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        let holdLoad = false;
        const loadEntered = yield* Deferred.make<void>();
        const loadRelease = yield* Deferred.make<void>();
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.suspend(() =>
            holdLoad
              ? Deferred.succeed(loadEntered, undefined).pipe(
                  Effect.zipRight(Deferred.await(loadRelease)),
                  Effect.zipRight(Effect.sync(() => rows)),
                )
              : Effect.sync(() => rows),
          ),
          (keys) =>
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Deferred.await(release)),
              Effect.zipRight(reads.spendableAt(keys)),
            ),
        );
        const view = Effect.map(service.spendableView, (holds) => [
          holds(hex(spent)),
          holds(hex(held)),
          holds(hex(absent)),
        ]);
        expect(yield* view).toStrictEqual([undefined, undefined, undefined]);
        yield* readAt(service, []);
        expect(yield* view).toStrictEqual([true, true, false]);

        rows = [row(held, output)];
        yield* markMempoolLedgerStale(globals, [hex(spent)]);
        expect(yield* view).toStrictEqual([undefined, true, false]);

        const reading = yield* Effect.fork(readAt(service, []));
        yield* Deferred.await(entered);
        expect(yield* view).toStrictEqual([undefined, true, false]);
        yield* Deferred.succeed(release, undefined);
        yield* Fiber.join(reading);
        expect(yield* view).toStrictEqual([false, true, false]);

        yield* markMempoolLedgerStale(globals, "all");
        expect(yield* view).toStrictEqual([undefined, undefined, undefined]);
        holdLoad = true;
        const loading = yield* Effect.fork(readAt(service, []));
        yield* Deferred.await(loadEntered);
        expect(yield* view).toStrictEqual([undefined, undefined, undefined]);
        yield* Deferred.succeed(loadRelease, undefined);
        yield* Fiber.join(loading);
        expect(yield* view).toStrictEqual([false, true, false]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("a failed commit attempt marks every key; other outputs mark none", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const outputs: WorkerOutput[] = [
          {
            type: "SuccessfulSubmissionOutput",
            submittedTxHash: "commit-tx",
            txSize: 1,
            mempoolTxsCount: 1,
            sizeOfBlocksTxs: 1,
            blockEndTimeMs: 1,
          },
          {
            type: "SubmittedAwaitingConfirmationOutput",
            submittedTxHash: "commit-tx",
            txSize: 1,
            mempoolTxsCount: 1,
            sizeOfBlocksTxs: 1,
            blockEndTimeMs: 1,
            submittedHeaderHash: "aa".repeat(28),
            submittedUtxosRoot: "bb".repeat(32),
          },
          { type: "NothingToCommitOutput" },
        ];
        for (const output of outputs) {
          yield* publishCommitMempoolLedgerMutation(globals, output);
          expect(takeCommitWorkerOutput(globals, output)).toBe(output);
        }
        expect(yield* Ref.get(globals.MEMPOOL_LEDGER_STALE)).toStrictEqual(
          unmarked,
        );

        yield* publishCommitMempoolLedgerMutation(globals, {
          type: "FailureOutput",
          error: "submission failed",
        });
        expect((yield* Ref.get(globals.MEMPOOL_LEDGER_STALE)).all).toBe(true);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("a commit worker's revert notice has exactly its outrefs read again", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const reverted = outRefFromByte(0x78);
        const restored = outRefFromByte(0x79);
        const untouched = outRefFromByte(0x7a);
        const output = makeOutput(15n);
        let rows: Rows = [row(reverted, output), row(untouched, output)];
        const reads = keyedReadsOver(() => rows);
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => rows),
          reads.spendableAt,
        );
        yield* readAt(service, []);
        const reloadsAfterLoad = yield* fullReloads;

        // A commit-stage rejection deletes an output it produced and
        // restores an input it spent.
        rows = [row(restored, output), row(untouched, output)];
        expect(
          takeCommitWorkerOutput(
            globals,
            mempoolLedgerRevertedNotice([hex(reverted), hex(restored)]),
          ),
        ).toBeUndefined();
        expect([
          ...(yield* readAt(service, [reverted, restored, untouched])).keys(),
        ]).toEqual([hex(restored), hex(untouched)]);
        expect(reads.asked).toStrictEqual([
          [hex(reverted), hex(restored)].sort(),
        ]);
        expect(yield* fullReloads).toBe(reloadsAfterLoad);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it.each([
    ["no outrefs", {}],
    ["outrefs that are not a list", { outRefs: "ab" }],
    ["a null list", { outRefs: null }],
    ["an outref that is not a string", { outRefs: ["ab", 1] }],
    ["an upper-case outref", { outRefs: ["AB"] }],
    ["an outref of odd length", { outRefs: ["abc"] }],
    ["an empty outref", { outRefs: [""] }],
  ])("a revert notice with %s marks every key", async (_label, payload) => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const notice = {
          type: "MempoolLedgerRevertedNotice",
          ...payload,
        } as unknown as CommitWorkerMessage;
        expect(takeCommitWorkerOutput(globals, notice)).toBeUndefined();
        expect((yield* Ref.get(globals.MEMPOOL_LEDGER_STALE)).all).toBe(true);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
});
