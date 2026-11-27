import "./mempool-ledger-cache.canonical-history-cache-retirement.js";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { runPhaseBValidationWithPatch } from "@al-ft/midgard-validation/phase-b";
import { Deferred, Effect, Fiber, Metric } from "effect";
import { describe, expect, it } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  makePhaseBCandidate,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { Globals } from "../src/services/globals.globals.js";
import { markMempoolLedgerStale } from "../src/services/globals.with-l1-control-plane-held.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.make-mempool-ledger-cache-service.js";
import { validationPhaseBLockWaitTimer } from "../src/services/mempool-ledger-cache.mempool-ledger-cache-service.js";
import {
  fullReloads,
  keyedReadsOver,
  noKeyedReads,
  readAt,
  row,
  snapshot,
} from "./mempool-ledger-cache.row.js";

describe("mempool ledger cache", () => {
  it("serializes concurrent Phase B critical sections", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.succeed([]),
          noKeyedReads,
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
          noKeyedReads,
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

  it("poisons later persistence and re-reads the speculative keys after an injected failure", async () => {
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
    const reads = keyedReadsOver(() => durableRows);

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => {
            loads += 1;
            return durableRows;
          }),
          reads.spendableAt,
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

        const reloadsBefore = yield* fullReloads;
        yield* service.recoverPoisonedEpoch;
        const afterRecovery = yield* readAt(service, [
          durable,
          speculativeFirst,
          speculativeSecond,
          recovered,
        ]);
        expect([...afterRecovery.keys()]).toStrictEqual([
          durable.toString("hex"),
        ]);
        expect(loads).toBe(1);
        expect(yield* fullReloads).toBe(reloadsBefore);
        expect(reads.asked).toStrictEqual([
          [speculativeFirst, speculativeSecond]
            .map((key) => key.toString("hex"))
            .sort(),
        ]);

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

  it("reapplies an unpersisted ordered overlay after every key is marked stale", async () => {
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
          noKeyedReads,
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

        yield* markMempoolLedgerStale(globals, "all");
        const duringPersistence = yield* readAt(service, [
          durable,
          speculative,
        ]);
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
          keyedReadsOver(() => durableRows).spendableAt,
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
        yield* markMempoolLedgerStale(globals, [external.toString("hex")]);
        const afterExternalWrite = yield* readAt(service, [
          spent,
          persisted,
          external,
        ]);
        expect([...afterExternalWrite.keys()]).toStrictEqual([
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
          keyedReadsOver(() => durableRows).spendableAt,
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
        yield* markMempoolLedgerStale(globals, "all");

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
        yield* markMempoolLedgerStale(globals, [external.toString("hex")]);
        const afterExternalWrite = yield* readAt(service, [
          spent,
          persisted,
          external,
        ]);
        expect([...afterExternalWrite.keys()]).toStrictEqual([
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
    const reads = keyedReadsOver(() => durableRows);

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => {
            loads += 1;
            return durableRows;
          }),
          reads.spendableAt,
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

        const reloadsBefore = yield* fullReloads;
        const recoveryFiber = yield* Effect.fork(service.recoverPoisonedEpoch);
        yield* Effect.yieldNow();
        expect(reads.asked).toStrictEqual([]);
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
        const afterRecovery = yield* readAt(service, [
          durable,
          persistedBeforeFailure,
          interrupted,
          later,
        ]);
        expect([...afterRecovery.keys()]).toStrictEqual([
          durable.toString("hex"),
          persistedBeforeFailure.toString("hex"),
        ]);
        expect(afterRecovery.has(interrupted.toString("hex"))).toBe(false);
        expect(afterRecovery.has(later.toString("hex"))).toBe(false);
        expect(loads).toBe(1);
        expect(yield* fullReloads).toBe(reloadsBefore);
        expect(reads.asked).toStrictEqual([
          [interrupted, later].map((key) => key.toString("hex")).sort(),
        ]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("matches retrieveSpendable semantics through deposit projection and header confirmation", async () => {
    const spent = outRefFromByte(0x73);
    // Phase B enforces MIN-ADA-TX on every PRODUCED output, so a candidate
    // paying 10 lovelace is rejected with `E_MIN_ADA` before the cache
    // semantics under test are ever reached. `FUNDED_OUTPUT_LOVELACE` is the
    // fixtures' clears-the-floor-with-headroom amount; the pre-state entry is
    // funded to the same value so the produced output is also conserved.
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    let retrieveSpendableRows: readonly MempoolLedgerDB.EntryWithTimeStamp[] =
      [];

    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => retrieveSpendableRows),
          keyedReadsOver(() => retrieveSpendableRows).spendableAt,
        );
        const read = () =>
          service.withPhaseBLock(
            service.currentState.pipe(Effect.map(snapshot([spent]))),
          );

        // Awaiting -> projected inserts a row, but it is intentionally hidden
        // until projected_header_hash is assigned by the working-ledger
        // rebase that takes in its landed block.
        let cached = yield* read();
        expect(cached.size).toBe(0);
        expect(cached.size).toBe(retrieveSpendableRows.length);

        retrieveSpendableRows = [row(spent, output)];
        yield* markMempoolLedgerStale(globals, [spent.toString("hex")]);
        cached = yield* read();
        expect([...cached.keys()]).toStrictEqual(
          retrieveSpendableRows.map((entry) =>
            entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
          ),
        );

        const verdict = yield* runPhaseBValidationWithPatch(
          [candidate],
          cached,
          {
            l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
            nowCardanoSlotNo: 0n,
            bucketConcurrency: 1,
          },
        );
        expect(verdict.accepted.map((tx) => tx.ledgerTx.txId)).toStrictEqual([
          candidate.ledgerTx.txId,
        ]);
        expect(verdict.rejected).toHaveLength(0);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
});
