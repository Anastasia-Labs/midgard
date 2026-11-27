import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  runPhaseBValidationWithPatch,
  type UTxOStatePatch,
} from "@al-ft/midgard-validation/phase-b";
import { Deferred, Effect, Fiber, Metric } from "effect";
import { describe, expect, it } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  makePhaseBCandidate,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { Globals } from "../src/services/globals.globals.js";
import { markMempoolLedgerStale } from "../src/services/globals.with-l1-control-plane-held.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.make-mempool-ledger-cache-service.js";
import {
  type MempoolLedgerCacheService,
  validationLedgerCacheEntryWriteCounter,
} from "../src/services/mempool-ledger-cache.mempool-ledger-cache-service.js";
import {
  fullReloads,
  noKeyedReads,
  row,
  snapshot,
} from "./mempool-ledger-cache.row.js";

const outRefAt = (index: number) => {
  const outref = Buffer.alloc(36, 0);
  outref.writeUInt32BE(index, 0);
  return outref;
};

const entriesWritten = Metric.value(
  validationLedgerCacheEntryWriteCounter,
).pipe(Effect.map((state) => state.count));

/** The outputs `rows` holds at `keys`. */
const spendableIn = (
  rows: ReadonlyMap<string, Buffer>,
  keys: ReadonlySet<string>,
) =>
  new Map(
    [...keys].flatMap((key) => {
      const output = rows.get(key);
      return output === undefined ? [] : [[key, output] as const];
    }),
  );

/**
 * A cache loaded from scratch over `rows`, reading `rows` at keys and logging
 * each keyed read's keys (sorted) in `asked`.
 */
const freshOver = (
  globals: Globals,
  rows: ReadonlyMap<string, Buffer>,
  asked: string[][] = [],
) =>
  makeMempoolLedgerCacheService(
    globals,
    Effect.sync(() =>
      [...rows].map(([key, output]) => row(Buffer.from(key, "hex"), output)),
    ),
    (keys) =>
      Effect.sync(() => {
        asked.push([...keys].sort());
        return spendableIn(rows, keys);
      }),
  );

/**
 * A cache over `rows` with its own Globals: stale marks are taken by the
 * cache that reads them, so a reference must not share the tested cache's.
 */
const referenceOver = (rows: ReadonlyMap<string, Buffer>) =>
  Globals.pipe(
    Effect.flatMap((own) => freshOver(own, rows)),
    Effect.provide(Globals.Default),
  );

describe("validation cache working ledger", () => {
  it("a keyed recovery and a promoted patch write only their own entries into a loaded ledger", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const loaded = 2_000;
        const rows = new Map(
          Array.from({ length: loaded }, (_, index) => [
            outRefAt(index).toString("hex"),
            makeOutput(BigInt(index + 1)),
          ]),
        );
        const service = yield* freshOver(globals, rows);
        const beforeLoad = yield* entriesWritten;
        yield* service.withPhaseBLock(service.currentState);
        expect((yield* entriesWritten) - beforeLoad).toBe(BigInt(loaded));

        const removed = outRefAt(3).toString("hex");
        const changed = outRefAt(4).toString("hex");
        const added = outRefAt(loaded).toString("hex");
        const keys = new Set([removed, changed, added]);
        const beforeRepair = yield* entriesWritten;
        const recovery = yield* service.retireCanonicalEpoch;
        yield* recovery.runRecovery(
          Effect.sync(() => {
            rows.delete(removed);
            rows.set(changed, makeOutput(9_999n));
            rows.set(added, makeOutput(7_777n));
          }),
          Effect.void,
          () => ({ keys, spendable: spendableIn(rows, keys) }),
        );
        expect((yield* entriesWritten) - beforeRepair).toBe(BigInt(keys.size));

        const spent = outRefAt(10);
        const produced = outRefAt(loaded + 1);
        const patch: UTxOStatePatch = {
          deletedOutRefs: [spent.toString("hex")],
          upsertedOutRefs: [[produced.toString("hex"), makeOutput(5n)]],
        };
        const beforePromotion = yield* entriesWritten;
        const sequence = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* sequence.runDecision(
          service.applySpeculativePatch(sequence.sequence, patch),
        );
        yield* sequence.runPersistence(
          Effect.sync(() => {
            rows.delete(spent.toString("hex"));
            rows.set(produced.toString("hex"), makeOutput(5n));
          }),
        );
        yield* sequence.cancel;
        expect((yield* entriesWritten) - beforePromotion).toBe(2n);

        const universe = Array.from({ length: loaded + 2 }, (_, index) =>
          outRefAt(index),
        );
        const reference = yield* referenceOver(rows);
        expect(
          yield* service.withPhaseBLock(
            service.currentState.pipe(Effect.map(snapshot(universe))),
          ),
        ).toEqual(
          yield* reference.withPhaseBLock(
            reference.currentState.pipe(Effect.map(snapshot(universe))),
          ),
        );
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("answers every key as a cache loaded from scratch would, across patches, promotions, abandoned sequences, stale marks and recoveries", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const universe = Array.from({ length: 16 }, (_, index) =>
          outRefFromByte(0x10 + index),
        );
        const hexAt = (index: number) => universe[index]!.toString("hex");
        const rows = new Map(
          [0, 1, 2, 3, 4, 5].map((index) => [
            hexAt(index),
            makeOutput(BigInt(index + 1)),
          ]),
        );
        // Deterministic choices: a fixed linear congruential sequence, read
        // from its high bits (its low bits cycle with short periods).
        let seed = 0x5eed;
        const next = (bound: number) => {
          seed = (seed * 1_103_515_245 + 12_345) % 2_147_483_648;
          return Math.floor((seed / 2_147_483_648) * bound);
        };
        let minted = 100n;
        const freshOutput = () => makeOutput((minted += 1n));
        const service = yield* freshOver(globals, rows);

        const view = (cache: MempoolLedgerCacheService) =>
          cache.withPhaseBLock(
            cache.currentState.pipe(Effect.map(snapshot(universe))),
          );
        const expectMatches = (live: readonly [bigint, UTxOStatePatch][]) =>
          Effect.gen(function* () {
            const reference = yield* referenceOver(rows);
            yield* reference.withPhaseBLock(reference.currentState);
            for (const [sequence, patch] of live)
              yield* reference.withPhaseBLock(
                reference.applySpeculativePatch(sequence, patch),
              );
            expect(yield* view(service)).toEqual(yield* view(reference));
          });
        /** A patch that spends one held key and produces one absent key. */
        const patchOver = Effect.gen(function* () {
          const held = yield* view(service);
          const present = [...held.keys()];
          const absent = universe
            .map((outref) => outref.toString("hex"))
            .filter((key) => !held.has(key));
          const patch: UTxOStatePatch = {
            deletedOutRefs:
              present.length === 0 ? [] : [present[next(present.length)]!],
            upsertedOutRefs:
              absent.length === 0
                ? []
                : [[absent[next(absent.length)]!, freshOutput()]],
          };
          return patch;
        });
        const writeRows = (patch: UTxOStatePatch) => {
          for (const key of patch.deletedOutRefs) rows.delete(key);
          for (const [key, output] of patch.upsertedOutRefs)
            rows.set(key, Buffer.from(output));
        };
        const flipRow = (key: string) => {
          if (rows.has(key)) rows.delete(key);
          else rows.set(key, freshOutput());
        };

        yield* expectMatches([]);
        for (let step = 0; step < 80; step += 1) {
          const operation = next(6);
          if (operation === 0) {
            // Another writer changes a row and marks it stale.
            const key = hexAt(next(universe.length));
            flipRow(key);
            yield* markMempoolLedgerStale(globals, [key]);
            yield* expectMatches([]);
          } else if (operation === 1 || operation === 2) {
            // Two accepted sequences; the first is promoted.
            const first = yield* service.withClaimLock(
              service.registerPhaseBSequence,
            );
            const firstPatch = yield* patchOver;
            yield* first.runDecision(
              service.applySpeculativePatch(first.sequence, firstPatch),
            );
            const second = yield* service.withClaimLock(
              service.registerPhaseBSequence,
            );
            const secondPatch = yield* patchOver;
            yield* second.runDecision(
              service.applySpeculativePatch(second.sequence, secondPatch),
            );
            yield* expectMatches([
              [first.sequence, firstPatch],
              [second.sequence, secondPatch],
            ]);
            yield* first.runPersistence(
              Effect.sync(() => writeRows(firstPatch)),
            );
            yield* first.cancel;
            yield* expectMatches([[second.sequence, secondPatch]]);
            if (operation === 1) {
              yield* second.runPersistence(
                Effect.sync(() => writeRows(secondPatch)),
              );
              yield* second.cancel;
            } else {
              // Abandoned: the epoch is poisoned and recovered.
              yield* second.cancel;
              yield* service.recoverPoisonedEpoch;
            }
            yield* expectMatches([]);
          } else if (operation === 3) {
            // An abandoned sequence whose persistence committed without its
            // promotion.
            const sequence = yield* service.withClaimLock(
              service.registerPhaseBSequence,
            );
            const patch = yield* patchOver;
            yield* sequence.runDecision(
              service.applySpeculativePatch(sequence.sequence, patch),
            );
            writeRows(patch);
            yield* sequence.cancel;
            yield* service.recoverPoisonedEpoch;
            yield* expectMatches([]);
          } else {
            // Canonical recovery, at keys or in full.
            const keys = new Set([
              hexAt(next(universe.length)),
              hexAt(next(universe.length)),
            ]);
            const recovery = yield* service.retireCanonicalEpoch;
            yield* recovery.runRecovery(
              Effect.sync(() => keys.forEach(flipRow)),
              Effect.void,
              operation === 4
                ? () => ({ keys, spendable: spendableIn(rows, keys) })
                : undefined,
            );
            yield* expectMatches([]);
          }
        }
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
  it("a poisoned recovery reads again only the unpromoted patches' keys and finds a persistence that committed before its promotion was interrupted", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const held = outRefAt(1).toString("hex");
        const spent = outRefAt(2).toString("hex");
        const produced = outRefAt(3).toString("hex");
        const rows = new Map([
          [held, makeOutput(1n)],
          [spent, makeOutput(2n)],
        ]);
        const asked: string[][] = [];
        const service = yield* freshOver(globals, rows, asked);
        yield* service.withPhaseBLock(service.currentState);
        const sequence = yield* service.withClaimLock(
          service.registerPhaseBSequence,
        );
        yield* sequence.runDecision(
          service.applySpeculativePatch(sequence.sequence, {
            deletedOutRefs: [spent],
            upsertedOutRefs: [[produced, makeOutput(3n)]],
          }),
        );

        // Phase B holds its lock, so the promotion after the commit waits.
        const locked = yield* Deferred.make<void>();
        const unlock = yield* Deferred.make<void>();
        const phaseB = yield* Effect.fork(
          service.withPhaseBLock(
            Deferred.succeed(locked, undefined).pipe(
              Effect.zipRight(Deferred.await(unlock)),
            ),
          ),
        );
        yield* Deferred.await(locked);
        const committed = yield* Deferred.make<void>();
        const writer = yield* Effect.fork(
          sequence.runPersistence(
            Effect.sync(() => {
              rows.delete(spent);
              rows.set(produced, makeOutput(3n));
            }).pipe(Effect.zipRight(Deferred.succeed(committed, undefined))),
          ),
        );
        yield* Deferred.await(committed);
        yield* Fiber.interrupt(writer);
        yield* sequence.cancel;
        yield* Deferred.succeed(unlock, undefined);
        yield* Fiber.join(phaseB);

        const reloadsBefore = yield* fullReloads;
        yield* service.recoverPoisonedEpoch;
        expect(yield* fullReloads).toBe(reloadsBefore);
        expect(asked).toStrictEqual([[produced, spent].sort()]);
        const universe = [1, 2, 3].map(outRefAt);
        const reference = yield* referenceOver(rows);
        expect(
          yield* service.withPhaseBLock(
            service.currentState.pipe(Effect.map(snapshot(universe))),
          ),
        ).toEqual(
          yield* reference.withPhaseBLock(
            reference.currentState.pipe(Effect.map(snapshot(universe))),
          ),
        );
      }).pipe(Effect.provide(Globals.Default)),
    );
  });

  it("prevents a double accept across two concurrent drain-loop Phase B sections", async () => {
    const spent = outRefFromByte(0x74);
    // `makePhaseBCandidate` produces a `FUNDED_OUTPUT_LOVELACE` output by
    // default, so the pre-state entry it spends has to carry the same value:
    // a 10-lovelace input makes both candidates fail value conservation and
    // the double-accept guard would then pass vacuously.
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
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
          noKeyedReads,
        );
        const validateOne = (
          candidate: (typeof candidates)[number],
          sequence: number,
        ) =>
          service.withPhaseBLock(
            Effect.gen(function* () {
              const state = yield* service.currentState;
              const verdict = yield* runPhaseBValidationWithPatch(
                [candidate],
                state,
                {
                  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
                  nowCardanoSlotNo: 0n,
                  bucketConcurrency: 1,
                },
              );
              yield* service.applySpeculativePatch(
                BigInt(sequence),
                verdict.statePatch,
              );
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
