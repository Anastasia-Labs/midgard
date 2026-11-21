/**
 * A seeded random run of apply, persist and retain over a small key space,
 * checked against a model of the retained ledgers after every retain. Run by
 * every adapter through `describeLedgerStoreConformance`. The kit imports
 * this module, so nothing here reads the kit's exports while the module
 * loads.
 */
import { describe, expect, it } from "vitest";

import {
  createLedgerStore,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  type LedgerMutation,
  LedgerNodeMissing,
  LedgerNotRetained,
  LedgerOutputMissing,
  type LedgerStorageCounts,
} from "../../src/ledger-store/index.js";
import {
  type OpenLedgerStorage,
  testLedgerEntry,
  testOutRef,
} from "./ledger-store-conformance.js";

const SEED = 0x5eed_1ed9;
const STEPS = 250;
/** Entry numbers drawn from: small, so keys are deleted and inserted again. */
const KEYS = 24;
/** Ledger values kept to build on; older ones are forgotten. */
const POOL = 10;

/** A deterministic generator of integers below a bound (mulberry32). */
const generator = (seed: number) => {
  let state = seed >>> 0;
  return (bound: number): number => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) % bound | 0;
  };
};

type Known = { readonly ledger: Ledger; readonly keys: ReadonlySet<number> };

const isMissing = (error: unknown) =>
  error instanceof LedgerNodeMissing || error instanceof LedgerOutputMissing;

const outRefsOf = (keys: Iterable<number>) =>
  [...keys].map((n) => testOutRef(n).toString("hex")).sort();

/**
 * The counts of a fresh store holding only the ledgers of `model`, admitted
 * under the roots the model already knows.
 */
const countsOfOnly = async (
  open: OpenLedgerStorage,
  model: ReadonlyMap<string, ReadonlySet<number>>,
): Promise<LedgerStorageCounts> => {
  const opened = await open();
  try {
    const store = createLedgerStore(opened.storage);
    for (const [root, keys] of model)
      await store.persist(
        await store.admit(
          [...keys].map((n) => testLedgerEntry(n)),
          root,
        ),
      );
    return await opened.storage.counts();
  } finally {
    await opened.close();
  }
};

/** Registers the random-run case for one adapter. */
export const describeLedgerStoreRandomConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  describe(`ledger store random run: ${adapter}`, () => {
    it(`matches a model of the retained ledgers over ${STEPS} seeded steps`, async () => {
      const opened = await open();
      try {
        const { storage } = opened;
        const store = createLedgerStore(storage);
        const next = generator(SEED);
        const pool: Known[] = [
          { ledger: await store.at(EMPTY_LEDGER_ROOT), keys: new Set() },
        ];
        /** The retained roots, with their key sets. */
        const model = new Map<string, ReadonlySet<number>>();
        /** How often the run reached each path it is meant to check. */
        const seen = { retains: 0, concurrent: 0, refused: 0, largest: 0 };
        const pick = () => pool[next(pool.length)]!;
        const remember = (known: Known) => {
          pool.push(known);
          if (pool.length > POOL) pool.shift();
        };

        const apply = async () => {
          const from = pick();
          const keys = new Set(from.keys);
          const mutations: LedgerMutation[] = [];
          const touched = new Set<number>();
          for (let count = 1 + next(3); count > 0; count -= 1) {
            const n = next(KEYS);
            if (touched.has(n)) continue;
            touched.add(n);
            if (keys.delete(n))
              mutations.push({ type: "delete", outRef: testOutRef(n) });
            else {
              keys.add(n);
              mutations.push({ type: "insert", ...testLedgerEntry(n) });
            }
          }
          const applied = await from.ledger.apply(mutations).then(
            (ledger) => ledger,
            (error: unknown) => {
              // Only a lineage whose root a retain dropped may lack nodes.
              if (!isMissing(error) || model.has(from.ledger.root)) throw error;
              return undefined;
            },
          );
          if (applied !== undefined) remember({ ledger: applied, keys });
        };

        /** Persists `known`; answers whether it was refused. */
        const persist = async (persisting: Promise<void>, known: Known) => {
          const refused = await persisting.then(
            () => false,
            (error: unknown) => {
              if (!isMissing(error)) throw error;
              return true;
            },
          );
          if (refused) {
            expect(model.has(known.ledger.root)).toBe(false);
            await expect(store.at(known.ledger.root)).rejects.toBeInstanceOf(
              LedgerNotRetained,
            );
          } else if (known.ledger.root !== EMPTY_LEDGER_ROOT) {
            model.set(known.ledger.root, known.keys);
            // Read back by root, its overlay is empty: a ledger built on it
            // reaches the store's own records.
            remember({
              ledger: await store.at(known.ledger.root),
              keys: known.keys,
            });
          }
          return refused;
        };

        const requested = () => {
          const roots = new Set([...model.keys()].filter(() => next(8) !== 0));
          if (next(4) === 0) roots.add("ef".repeat(32));
          if (next(4) === 0) roots.add(EMPTY_LEDGER_ROOT);
          return roots;
        };

        /** The model after `retain(roots)`, and the roots it must list. */
        const retainModel = (roots: ReadonlySet<string>) => {
          for (const root of [...model.keys()])
            if (!roots.has(root)) model.delete(root);
          return new Set(
            [...roots].filter(
              (root) => root !== EMPTY_LEDGER_ROOT && !model.has(root),
            ),
          );
        };

        /** A fresh store's counts, by the retained roots it holds. */
        const freshCounts = new Map<string, LedgerStorageCounts>();
        const expectModel = async () => {
          seen.largest = Math.max(seen.largest, model.size);
          for (const [root, keys] of model) {
            const listed: string[] = [];
            for await (const { outRef } of (await store.at(root)).entries())
              listed.push(outRef.toString("hex"));
            expect(listed.sort()).toEqual(outRefsOf(keys));
          }
          const retained = [...model.keys()].sort().join();
          let expected = freshCounts.get(retained);
          if (expected === undefined) {
            expected = await countsOfOnly(open, model);
            freshCounts.set(retained, expected);
          }
          expect(await storage.counts()).toEqual(expected);
        };

        for (let step = 0; step < STEPS; step += 1) {
          const roll = next(100);
          if (roll < 45) await apply();
          else if (roll < 75) {
            const known = pick();
            const before = await storage.counts();
            if (await persist(store.persist(known.ledger), known)) {
              expect(await storage.counts()).toEqual(before);
              seen.refused += 1;
            }
          } else if (roll < 90) {
            const roots = requested();
            const result = await store.retain(roots);
            expect(result).toEqual({ notRetained: retainModel(roots) });
            await expectModel();
            seen.retains += 1;
          } else {
            // A persist called right after a retain lands between the
            // retained set's replacement and the deletion.
            const roots = requested();
            const known = pick();
            const retaining = store.retain(roots);
            const persisting = store.persist(known.ledger);
            const notRetained = retainModel(roots);
            const [result] = await Promise.all([
              retaining,
              persist(persisting, known),
            ]);
            notRetained.delete(known.ledger.root);
            const listed = new Set(result.notRetained);
            listed.delete(known.ledger.root);
            expect(listed).toEqual(notRetained);
            await expectModel();
            seen.concurrent += 1;
          }
        }
        expect(seen.retains).toBeGreaterThan(10);
        expect(seen.concurrent).toBeGreaterThan(10);
        expect(seen.refused).toBeGreaterThan(0);
        expect(seen.largest).toBeGreaterThan(3);
      } finally {
        await opened.close();
      }
    });
  });
};
