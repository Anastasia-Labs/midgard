import { runPhaseBValidationWithPatch } from "@al-ft/midgard-validation";
import { Deferred, Effect, Fiber, Metric, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE_V1,
  makeOutput,
  makePhaseBCandidate,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { publishCommitMempoolLedgerMutation } from "../src/fibers/block-commitment.js";
import { Globals, publishMempoolLedgerDelta } from "../src/services/globals.js";
import {
  makeMempoolLedgerCacheService,
  type MempoolLedgerState,
  validationPhaseBLockWaitTimer,
} from "../src/services/mempool-ledger-cache.js";

const row = (
  outref: Buffer,
  output: Buffer,
): MempoolLedgerDB.EntryWithTimeStamp => ({
  [MempoolLedgerDB.Columns.TX_ID]: Buffer.alloc(32, 0x77),
  [MempoolLedgerDB.Columns.OUTREF]: outref,
  [MempoolLedgerDB.Columns.OUTPUT]: output,
  [MempoolLedgerDB.Columns.ADDRESS]: "addr_test1_phase2_cache",
  [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: Buffer.alloc(32, 0x33),
  [MempoolLedgerDB.Columns.TIMESTAMPTZ]: new Date(0),
});

const snapshot = (state: MempoolLedgerState) =>
  new Map([...state].map(([key, value]) => [key, Buffer.from(value)]));

describe("mempool ledger cache", () => {
  it("replays a delta published while the full snapshot is loading", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const inserted = outRefFromByte(0x70);
        const insertedOutput = makeOutput(12n);
        let loads = 0;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.gen(function* () {
            loads += 1;
            const snapshot: readonly MempoolLedgerDB.EntryWithTimeStamp[] = [];
            yield* publishMempoolLedgerDelta(
              globals,
              {
                full: false,
                upserts: [[inserted.toString("hex"), insertedOutput]],
                deletes: [],
              },
              64,
            );
            return snapshot;
          }),
        );

        const state = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect(loads).toBe(1);
        expect(state.get(inserted.toString("hex"))).toEqual(insertedOutput);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("reloads again when a full marker is published during the snapshot", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const inserted = outRefFromByte(0x6f);
        const insertedOutput = makeOutput(13n);
        let loads = 0;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.gen(function* () {
            loads += 1;
            if (loads === 1) {
              yield* publishMempoolLedgerDelta(
                globals,
                { full: true, upserts: [], deletes: [] },
                64,
              );
              return [];
            }
            return [row(inserted, insertedOutput)];
          }),
        );

        const state = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect(loads).toBe(2);
        expect(state.get(inserted.toString("hex"))).toEqual(insertedOutput);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("applies contiguous deltas and reloads on gaps and full markers", async () => {
    let backing: readonly MempoolLedgerDB.EntryWithTimeStamp[] = [];
    let loads = 0;
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => {
            loads += 1;
            return backing;
          }),
        );
        const first = outRefFromByte(0x71);
        const second = outRefFromByte(0x72);
        const firstOutput = makeOutput(10n);
        const secondOutput = makeOutput(11n);

        let state = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect(state.size).toBe(0);
        expect(loads).toBe(1);

        yield* publishMempoolLedgerDelta(
          globals,
          {
            full: false,
            upserts: [[first.toString("hex"), firstOutput]],
            deletes: [],
          },
          64,
        );
        state = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect(state.get(first.toString("hex"))?.equals(firstOutput)).toBe(
          true,
        );
        expect(loads).toBe(1);

        backing = [row(second, secondOutput)];
        yield* Ref.update(globals.MEMPOOL_LEDGER_DELTA_LOG, (journal) => ({
          ...journal,
          version: journal.version + 1,
        }));
        state = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect(state.has(first.toString("hex"))).toBe(false);
        expect(state.get(second.toString("hex"))?.equals(secondOutput)).toBe(
          true,
        );
        expect(loads).toBe(2);

        backing = [row(first, firstOutput)];
        yield* publishMempoolLedgerDelta(
          globals,
          { full: true, upserts: [], deletes: [] },
          64,
        );
        state = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect(state.get(first.toString("hex"))?.equals(firstOutput)).toBe(
          true,
        );
        expect(state.has(second.toString("hex"))).toBe(false);
        expect(loads).toBe(3);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("publishes exact commit deletes and a full recovery marker only for ambiguous finalization", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const deletedOutRefHex = outRefFromByte(0x75).toString("hex");
        yield* publishCommitMempoolLedgerMutation(
          globals,
          {
            type: "SuccessfulSubmissionOutput",
            submittedTxHash: "commit-tx",
            txSize: 1,
            mempoolTxsCount: 1,
            sizeOfBlocksTxs: 1,
            blockEndTimeMs: 1,
            mempoolLedgerDeletedOutRefHexes: [deletedOutRefHex],
          },
          64,
        );
        yield* publishCommitMempoolLedgerMutation(
          globals,
          {
            type: "SubmittedAwaitingLocalFinalizationOutput",
            submittedTxHash: "commit-tx",
            txSize: 1,
            mempoolTxsCount: 1,
            sizeOfBlocksTxs: 1,
            blockEndTimeMs: 1,
            error: "local finalization outcome is ambiguous",
            submittedHeaderHash: "aa".repeat(28),
            submittedUtxosRoot: "bb".repeat(32),
          },
          64,
        );
        yield* publishCommitMempoolLedgerMutation(
          globals,
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
          64,
        );
        const journal = yield* Ref.get(globals.MEMPOOL_LEDGER_DELTA_LOG);
        expect(journal.version).toBe(2);
        expect(journal.entries).toStrictEqual([
          {
            version: 1,
            full: false,
            upserts: [],
            deletes: [deletedOutRefHex],
          },
          { version: 2, full: true, upserts: [], deletes: [] },
        ]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("serializes concurrent Phase B critical sections", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.succeed([]),
        );
        const events: string[] = [];
        const section = (name: string) =>
          service.withPhaseBLock(
            Effect.sync(() => events.push(`${name}:start`)).pipe(
              Effect.zipRight(Effect.sleep("20 millis")),
              Effect.zipRight(Effect.sync(() => events.push(`${name}:end`))),
            ),
          );
        yield* Effect.all([section("a"), section("b")], {
          concurrency: "unbounded",
        });
        expect(events).toStrictEqual(["a:start", "a:end", "b:start", "b:end"]);
        const waitMetric = yield* Metric.value(validationPhaseBLockWaitTimer);
        expect(waitMetric.max).toBeGreaterThanOrEqual(10);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("preserves claim order when a later batch finishes Phase A first", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.succeed([]),
        );
        const first = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const second = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const events: string[] = [];
        yield* Effect.all(
          [
            Effect.sleep("20 millis").pipe(
              Effect.zipRight(
                first.runDecision(Effect.sync(() => events.push("first"))),
              ),
              Effect.zipRight(first.runPersistence(Effect.void)),
              Effect.ensuring(first.cancel),
            ),
            second
              .runDecision(Effect.sync(() => events.push("second")))
              .pipe(
                Effect.zipRight(second.runPersistence(Effect.void)),
                Effect.ensuring(second.cancel),
              ),
          ],
          { concurrency: "unbounded" },
        );
        expect(events).toStrictEqual(["first", "second"]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("poisons later persistence and reloads speculative cache after an injected failure", async () => {
    const durable = outRefFromByte(0x61);
    const speculativeFirst = outRefFromByte(0x62);
    const speculativeSecond = outRefFromByte(0x63);
    const recovered = outRefFromByte(0x64);
    const output = makeOutput(10n);
    let durableRows: readonly MempoolLedgerDB.EntryWithTimeStamp[] = [
      row(durable, output),
    ];
    let laterTerminalWrites = 0;
    let loads = 0;

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => {
            loads += 1;
            return durableRows;
          }),
        );
        yield* service.withPhaseBLock(service.currentState);
        const first = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const second = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const patch = (outref: Buffer) => ({
          deletedOutRefs: [],
          upsertedOutRefs: [[outref.toString("hex"), output]] as const,
        });
        yield* first.runDecision(
          service.applySpeculativePatch(
            first.sequence,
            patch(speculativeFirst),
          ),
        );
        yield* second.runDecision(
          service.applySpeculativePatch(
            second.sequence,
            patch(speculativeSecond),
          ),
        );

        const firstExit = yield* Effect.exit(
          first.runPersistence(Effect.fail(new Error("injected persist N"))),
        );
        yield* first.cancel;
        const secondExit = yield* Effect.exit(
          second.runPersistence(
            Effect.sync(() => {
              laterTerminalWrites += 1;
            }),
          ),
        );
        yield* second.cancel;
        expect(firstExit._tag).toBe("Failure");
        expect(secondExit._tag).toBe("Failure");
        expect(laterTerminalWrites).toBe(0);

        yield* service.recoverPoisonedEpoch;
        const afterRecovery = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect([...afterRecovery.keys()]).toStrictEqual([
          durable.toString("hex"),
        ]);
        expect(loads).toBe(2);

        const next = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        expect(next.epoch).toBeGreaterThan(first.epoch);
        yield* next.runDecision(
          service.applySpeculativePatch(next.sequence, patch(recovered)),
        );
        yield* next.runPersistence(
          Effect.sync(() => {
            laterTerminalWrites += 1;
            durableRows = [...durableRows, row(recovered, output)];
          }),
        );
        yield* next.cancel;
        expect(laterTerminalWrites).toBe(1);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("reapplies an unpersisted ordered overlay after an external full marker", async () => {
    const durable = outRefFromByte(0x69);
    const speculative = outRefFromByte(0x6a);
    const output = makeOutput(10n);
    let durableRows: readonly MempoolLedgerDB.EntryWithTimeStamp[] = [
      row(durable, output),
    ];

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => durableRows),
        );
        yield* service.withPhaseBLock(service.currentState);
        const sequence = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* sequence.runDecision(
          service.applySpeculativePatch(sequence.sequence, {
            deletedOutRefs: [],
            upsertedOutRefs: [[speculative.toString("hex"), output]],
          }),
        );

        yield* publishMempoolLedgerDelta(
          globals,
          { full: true, upserts: [], deletes: [] },
          64,
        );
        const duringPersistence = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect([...duringPersistence.keys()]).toStrictEqual([
          durable.toString("hex"),
          speculative.toString("hex"),
        ]);

        yield* sequence.runPersistence(
          Effect.sync(() => {
            durableRows = [...durableRows, row(speculative, output)];
          }),
        );
        yield* sequence.cancel;
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("keeps a successfully persisted patch in the durable cache base", async () => {
    const spent = outRefFromByte(0x5a);
    const persisted = outRefFromByte(0x5b);
    const external = outRefFromByte(0x5c);
    const output = makeOutput(10n);
    let durableRows: readonly MempoolLedgerDB.EntryWithTimeStamp[] = [
      row(spent, output),
    ];

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => durableRows),
        );
        yield* service.withPhaseBLock(service.currentState);
        const sequence = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* sequence.runDecision(
          service.applySpeculativePatch(sequence.sequence, {
            deletedOutRefs: [spent.toString("hex")],
            upsertedOutRefs: [[persisted.toString("hex"), output]],
          }),
        );
        yield* sequence.runPersistence(
          Effect.sync(() => {
            durableRows = [row(persisted, output)];
          }),
        );
        yield* sequence.cancel;

        durableRows = [row(persisted, output), row(external, output)];
        yield* publishMempoolLedgerDelta(
          globals,
          {
            full: false,
            upserts: [[external.toString("hex"), output]],
            deletes: [],
          },
          64,
        );
        const afterExternalDelta = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect([...afterExternalDelta.keys()]).toStrictEqual([
          persisted.toString("hex"),
          external.toString("hex"),
        ]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("serializes post-persistence promotion behind an overlapping full reload", async () => {
    const spent = outRefFromByte(0x5d);
    const persisted = outRefFromByte(0x5e);
    const external = outRefFromByte(0x5f);
    const output = makeOutput(10n);
    let durableRows: readonly MempoolLedgerDB.EntryWithTimeStamp[] = [
      row(spent, output),
    ];
    let loads = 0;

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const reloadStarted = yield* Deferred.make<void>();
        const releaseReload = yield* Deferred.make<void>();
        const persistenceCommitted = yield* Deferred.make<void>();
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.gen(function* () {
            loads += 1;
            const captured = durableRows;
            if (loads === 2) {
              yield* Deferred.succeed(reloadStarted, undefined);
              yield* Deferred.await(releaseReload);
            }
            return captured;
          }),
        );
        yield* service.withPhaseBLock(service.currentState);
        const sequence = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* sequence.runDecision(
          service.applySpeculativePatch(sequence.sequence, {
            deletedOutRefs: [spent.toString("hex")],
            upsertedOutRefs: [[persisted.toString("hex"), output]],
          }),
        );
        yield* publishMempoolLedgerDelta(
          globals,
          { full: true, upserts: [], deletes: [] },
          64,
        );

        const reloadFiber = yield* Effect.fork(
          service.withPhaseBLock(service.currentState),
        );
        yield* Deferred.await(reloadStarted);
        const persistenceFiber = yield* Effect.fork(
          sequence
            .runPersistence(
              Effect.sync(() => {
                durableRows = [row(persisted, output)];
              }).pipe(
                Effect.zipRight(
                  Deferred.succeed(persistenceCommitted, undefined),
                ),
              ),
            )
            .pipe(Effect.ensuring(sequence.cancel)),
        );
        yield* Deferred.await(persistenceCommitted);
        yield* Deferred.succeed(releaseReload, undefined);
        yield* Fiber.join(reloadFiber);
        yield* Fiber.join(persistenceFiber);

        durableRows = [row(persisted, output), row(external, output)];
        yield* publishMempoolLedgerDelta(
          globals,
          {
            full: false,
            upserts: [[external.toString("hex"), output]],
            deletes: [],
          },
          64,
        );
        const afterExternalDelta = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect([...afterExternalDelta.keys()]).toStrictEqual([
          persisted.toString("hex"),
          external.toString("hex"),
        ]);
        expect(loads).toBe(2);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("waits for earlier persistence before recovering an interrupted sequence", async () => {
    const durable = outRefFromByte(0x65);
    const persistedBeforeFailure = outRefFromByte(0x66);
    const interrupted = outRefFromByte(0x67);
    const later = outRefFromByte(0x68);
    const output = makeOutput(10n);
    let durableRows: readonly MempoolLedgerDB.EntryWithTimeStamp[] = [
      row(durable, output),
    ];
    let loads = 0;
    let laterTerminalWrites = 0;

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => {
            loads += 1;
            return durableRows;
          }),
        );
        yield* service.withPhaseBLock(service.currentState);
        const first = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const second = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const third = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const patch = (outref: Buffer) => ({
          deletedOutRefs: [],
          upsertedOutRefs: [[outref.toString("hex"), output]] as const,
        });
        yield* first.runDecision(
          service.applySpeculativePatch(
            first.sequence,
            patch(persistedBeforeFailure),
          ),
        );
        yield* second.runDecision(
          service.applySpeculativePatch(second.sequence, patch(interrupted)),
        );
        yield* third.runDecision(
          service.applySpeculativePatch(third.sequence, patch(later)),
        );

        const releaseFirst = yield* Deferred.make<void>();
        const firstStarted = yield* Deferred.make<void>();
        const firstFiber = yield* Effect.fork(
          first
            .runPersistence(
              Deferred.succeed(firstStarted, undefined).pipe(
                Effect.zipRight(Deferred.await(releaseFirst)),
                Effect.tap(() =>
                  Effect.sync(() => {
                    durableRows = [
                      ...durableRows,
                      row(persistedBeforeFailure, output),
                    ];
                  }),
                ),
              ),
            )
            .pipe(Effect.ensuring(first.cancel)),
        );
        yield* Deferred.await(firstStarted);
        const secondFiber = yield* Effect.fork(
          second
            .runPersistence(Effect.void)
            .pipe(Effect.ensuring(second.cancel)),
        );
        yield* Fiber.interrupt(secondFiber);
        yield* second.cancel;

        const recoveryFiber = yield* Effect.fork(service.recoverPoisonedEpoch);
        yield* Effect.yieldNow();
        expect(loads).toBe(1);
        yield* Deferred.succeed(releaseFirst, undefined);
        yield* Fiber.join(firstFiber);
        yield* Fiber.join(recoveryFiber);

        const thirdExit = yield* Effect.exit(
          third.runPersistence(
            Effect.sync(() => {
              laterTerminalWrites += 1;
            }),
          ),
        );
        yield* third.cancel;
        expect(thirdExit._tag).toBe("Failure");
        expect(laterTerminalWrites).toBe(0);
        const afterRecovery = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect([...afterRecovery.keys()]).toStrictEqual([
          durable.toString("hex"),
          persistedBeforeFailure.toString("hex"),
        ]);
        expect(afterRecovery.has(interrupted.toString("hex"))).toBe(false);
        expect(afterRecovery.has(later.toString("hex"))).toBe(false);
        expect(loads).toBe(2);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("matches retrieveSpendable semantics through deposit projection and header confirmation", async () => {
    const spent = outRefFromByte(0x73);
    // Phase B now enforces MIN-ADA-TX on every PRODUCED output, so a candidate
    // paying 10 lovelace is rejected with `E_MIN_ADA` before the cache
    // semantics under test are ever reached. `FUNDED_OUTPUT_LOVELACE_V1` is the
    // fixtures' clears-the-floor-with-headroom amount; the pre-state entry is
    // funded to the same value so the produced output is also conserved.
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE_V1);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE_V1,
    });
    let retrieveSpendableRows: readonly MempoolLedgerDB.EntryWithTimeStamp[] =
      [];

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => retrieveSpendableRows),
        );
        const read = () =>
          service.withPhaseBLock(
            service.currentState.pipe(Effect.map(snapshot)),
          );

        // Awaiting -> projected inserts a row, but it is intentionally hidden
        // until projected_header_hash is assigned by block confirmation.
        let cached = yield* read();
        expect(cached.size).toBe(0);
        expect(cached.size).toBe(retrieveSpendableRows.length);

        retrieveSpendableRows = [row(spent, output)];
        yield* publishMempoolLedgerDelta(
          globals,
          {
            full: false,
            upserts: [[spent.toString("hex"), output]],
            deletes: [],
          },
          64,
        );
        cached = yield* read();
        expect([...cached.keys()]).toStrictEqual(
          retrieveSpendableRows.map((entry) =>
            entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
          ),
        );

        const verdict = yield* runPhaseBValidationWithPatch(
          [candidate],
          cached,
          { nowCardanoSlotNo: 0n, bucketConcurrency: 1 },
        );
        expect(verdict.accepted.map((tx) => tx.ledgerTx.txId)).toStrictEqual([
          candidate.ledgerTx.txId,
        ]);
        expect(verdict.rejected).toHaveLength(0);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("prevents a double accept across two concurrent drain-loop Phase B sections", async () => {
    const spent = outRefFromByte(0x74);
    // `makePhaseBCandidate` produces a `FUNDED_OUTPUT_LOVELACE_V1` output by
    // default, so the pre-state entry it spends has to carry the same value:
    // a 10-lovelace input makes both candidates fail value conservation and
    // the double-accept guard would then pass vacuously.
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE_V1);
    const candidates = [
      makePhaseBCandidate({ arrivalSeq: 0n, spent: [spent] }),
      makePhaseBCandidate({
        arrivalSeq: 1n,
        spent: [spent],
        validityIntervalEnd: 100n,
      }),
    ];
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.succeed([row(spent, output)]),
        );
        const validateOne = (candidate: (typeof candidates)[number]) =>
          service.withPhaseBLock(
            Effect.gen(function* () {
              const state = yield* service.currentState;
              const verdict = yield* runPhaseBValidationWithPatch(
                [candidate],
                state,
                { nowCardanoSlotNo: 0n, bucketConcurrency: 1 },
              );
              yield* service.applyPatchAndSync(verdict.statePatch);
              return verdict;
            }),
          );
        const verdicts = yield* Effect.all(candidates.map(validateOne), {
          concurrency: "unbounded",
        });
        expect(
          verdicts.reduce(
            (total, verdict) => total + verdict.accepted.length,
            0,
          ),
        ).toBe(1);
        expect(
          verdicts.reduce(
            (total, verdict) => total + verdict.rejected.length,
            0,
          ),
        ).toBe(1);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
});
