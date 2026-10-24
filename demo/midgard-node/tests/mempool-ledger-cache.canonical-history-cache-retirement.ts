import { Deferred, Effect, Fiber } from "effect";
import { describe, expect, it } from "vitest";

import {
  makeOutput,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { Globals } from "../src/services/globals.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { row, snapshot } from "./mempool-ledger-cache.row.js";

describe("canonical history cache retirement", () => {
  it("fresh sequences do not wait on a retired Phase A job after recovery", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.succeed([]),
        );
        const parked = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const recovery = yield* service.retireCanonicalEpoch;
        yield* recovery.runRecovery(Effect.void, Effect.void);
        const fresh = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* fresh.runDecision(Effect.void).pipe(Effect.timeout("1 second"));
        yield* fresh
          .runPersistence(Effect.void)
          .pipe(Effect.timeout("1 second"));
        expect(
          (yield* Effect.either(parked.runDecision(Effect.void)))._tag,
        ).toBe("Left");
        yield* parked.cancel;
        yield* fresh.cancel;
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("an old poisoned recovery cannot advance a newly poisoned canonical epoch", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        let loads = 0;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => {
            loads += 1;
            return [];
          }),
        );
        const oldFirst = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const oldSecond = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* oldSecond.cancel;
        const waiting = yield* Effect.fork(service.recoverPoisonedEpoch);
        yield* Effect.yieldNow();
        const canonical = yield* service.retireCanonicalEpoch;
        yield* canonical.runRecovery(Effect.void, Effect.void);
        const newFirst = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const newSecond = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* newSecond.cancel;
        yield* oldFirst.cancel;
        yield* Fiber.join(waiting);
        expect(loads).toBe(1);
        yield* newFirst.runDecision(Effect.void);
        yield* newFirst.runPersistence(Effect.void);
        expect(
          (yield* Effect.either(newSecond.runDecision(Effect.void)))._tag,
        ).toBe("Left");
        yield* newFirst.cancel;
        yield* service.recoverPoisonedEpoch;
        expect(loads).toBe(2);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("fences claims/decisions/persistence and discards old speculative overlays before ready publication", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const orphan = outRefFromByte(0xa1);
        const speculative = outRefFromByte(0xa2);
        const canonical = outRefFromByte(0xa3);
        const output = makeOutput(10n);
        let rows = [row(orphan, output)];
        let writes = 0;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => rows),
        );
        const sequence = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* sequence.runDecision(
          service.applySpeculativePatch(sequence.sequence, {
            deletedOutRefs: [],
            upsertedOutRefs: [[speculative.toString("hex"), output]],
          }),
        );
        const recovery = yield* service.retireCanonicalEpoch;
        expect(
          (yield* Effect.either(service.withClaimLock(Effect.void)))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(service.withPhaseBLock(service.currentState)))
            ._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(
            sequence.runPersistence(
              Effect.sync(() => {
                writes += 1;
              }),
            ),
          ))._tag,
        ).toBe("Left");
        yield* sequence.cancel;
        yield* recovery.runRecovery(
          Effect.sync(() => {
            rows = [row(canonical, output)];
          }),
          Effect.gen(function* () {
            expect((yield* Effect.either(service.currentState))._tag).toBe(
              "Left",
            );
          }),
        );
        const state = yield* service.withPhaseBLock(
          service.currentState.pipe(Effect.map(snapshot)),
        );
        expect([...state.keys()]).toEqual([canonical.toString("hex")]);
        expect(writes).toBe(0);
        const next = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        expect(next.epoch).toBe(recovery.epoch);
        yield* next.runDecision(Effect.void);
        yield* next.runPersistence(Effect.void);
        yield* next.cancel;
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("drains active persistence without waiting for its stale Phase B promotion", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const old = outRefFromByte(0xb1);
        const canonical = outRefFromByte(0xb2);
        const output = makeOutput(10n);
        let rows = [row(old, output)];
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.sync(() => rows),
        );
        const sequence = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* sequence.runDecision(Effect.void);
        const entered = yield* Deferred.make<void>();
        const finish = yield* Deferred.make<void>();
        const writer = yield* Effect.fork(
          Effect.either(
            sequence
              .runPersistence(
                Effect.gen(function* () {
                  yield* Deferred.succeed(entered, undefined);
                  yield* Deferred.await(finish);
                }),
              )
              .pipe(Effect.ensuring(sequence.cancel)),
          ),
        );
        yield* Deferred.await(entered);
        const recovery = yield* service.retireCanonicalEpoch;
        let repaired = false;
        const repair = yield* Effect.fork(
          recovery.runRecovery(
            Effect.sync(() => {
              repaired = true;
              rows = [row(canonical, output)];
            }),
            Effect.void,
          ),
        );
        yield* Effect.yieldNow();
        expect(repaired).toBe(false);
        yield* Deferred.succeed(finish, undefined);
        yield* Fiber.join(repair);
        expect((yield* Fiber.join(writer))._tag).toBe("Left");
        const state = yield* service.withPhaseBLock(service.currentState);
        expect([...state.keys()]).toEqual([canonical.toString("hex")]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("a second retirement during reload prevents the older recovery publishing ready", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const entered = yield* Deferred.make<void>();
        const finish = yield* Deferred.make<void>();
        let loads = 0;
        let publications = 0;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.gen(function* () {
            if (++loads === 1) {
              yield* Deferred.succeed(entered, undefined);
              yield* Deferred.await(finish);
            }
            return [];
          }),
        );
        const first = yield* service.retireCanonicalEpoch;
        const pending = yield* Effect.fork(
          Effect.either(
            first.runRecovery(
              Effect.void,
              Effect.sync(() => {
                publications += 1;
              }),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const second = yield* service.retireCanonicalEpoch;
        yield* Deferred.succeed(finish, undefined);
        expect((yield* Fiber.join(pending))._tag).toBe("Left");
        expect(publications).toBe(0);
        expect(
          (yield* Effect.either(service.withPhaseBLock(service.currentState)))
            ._tag,
        ).toBe("Left");
        yield* second.runRecovery(
          Effect.void,
          Effect.sync(() => {
            publications += 1;
          }),
        );
        expect(publications).toBe(1);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("a retirement during durable ready publication cannot resume the older cache epoch", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.succeed([]),
        );
        const entered = yield* Deferred.make<void>();
        const finish = yield* Deferred.make<void>();
        const first = yield* service.retireCanonicalEpoch;
        const pending = yield* Effect.fork(
          Effect.either(
            first.runRecovery(
              Effect.void,
              Effect.gen(function* () {
                yield* Deferred.succeed(entered, undefined);
                yield* Deferred.await(finish);
              }),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const second = yield* service.retireCanonicalEpoch;
        yield* Deferred.succeed(finish, undefined);
        expect((yield* Fiber.join(pending))._tag).toBe("Left");
        expect(
          (yield* Effect.either(service.withClaimLock(Effect.void)))._tag,
        ).toBe("Left");
        yield* second.runRecovery(Effect.void, Effect.void);
        yield* service.withClaimLock(Effect.void);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("failed durable recovery remains suspended and can retry without reviving old sequences", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.succeed([]),
        );
        const old = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        const recovery = yield* service.retireCanonicalEpoch;
        expect(
          (yield* Effect.either(
            recovery.runRecovery(Effect.fail("repair failed"), Effect.void),
          ))._tag,
        ).toBe("Left");
        expect(
          (yield* Effect.either(service.withClaimLock(Effect.void)))._tag,
        ).toBe("Left");
        yield* recovery.runRecovery(Effect.void, Effect.void);
        expect((yield* Effect.either(old.runDecision(Effect.void)))._tag).toBe(
          "Left",
        );
        yield* old.cancel;
        yield* service.withPhaseBLock(service.currentState);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("a poisoned-epoch reload cannot clear canonical suspension that arrives while SQL is pending", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const entered = yield* Deferred.make<void>();
        const finish = yield* Deferred.make<void>();
        let loads = 0;
        const service = yield* makeMempoolLedgerCacheService(
          globals,
          Effect.gen(function* () {
            if (++loads === 1) {
              yield* Deferred.succeed(entered, undefined);
              yield* Deferred.await(finish);
            }
            return [];
          }),
        );
        const failed = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* Effect.either(failed.runDecision(Effect.fail("phase B failed")));
        yield* failed.cancel;
        const poisoned = yield* Effect.fork(service.recoverPoisonedEpoch);
        yield* Deferred.await(entered);
        const canonical = yield* service.retireCanonicalEpoch;
        yield* Deferred.succeed(finish, undefined);
        yield* Fiber.join(poisoned);
        expect(
          (yield* Effect.either(service.withPhaseBLock(service.currentState)))
            ._tag,
        ).toBe("Left");
        yield* canonical.runRecovery(Effect.void, Effect.void);
        yield* service.withClaimLock(Effect.void);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
});
