/**
 * Conformance cases on what `retain` leaves behind and on persists around
 * it, run by every adapter through `describeLedgerStoreConformance`. The kit
 * imports this module, so nothing here reads the kit's exports while the
 * module loads.
 */
import { midgardBlake2b } from "@al-ft/midgard-core/codec/hash";
import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot, type LedgerEntry } from "../../src/ledger-at.js";
import {
  createLedgerStore,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  LedgerNodeMissing,
  LedgerNotRetained,
  LedgerOutputMissing,
  type LedgerStorage,
  type LedgerStore,
  recordWitness,
} from "../../src/ledger-store/index.js";
import {
  appliedEntries,
  countsOfOnly,
  event,
  expectLedgerMatches,
  type OpenLedgerStorage,
  testLedgerEntry,
  testOutRef,
  UNDECODABLE_ENTRY,
} from "./ledger-store-conformance.js";

/** The entry numbers of the base ledger. */
const baseNumbers = () => [
  ...Array.from({ length: 40 }, (_, n) => n),
  UNDECODABLE_ENTRY,
];
const base = () => baseNumbers().map((n) => testLedgerEntry(n));

/** Out-ref `n`'s path in the trie: the hex digest of its bytes. */
const pathOf = (n: number): string =>
  Buffer.from(midgardBlake2b(testOutRef(n), { dkLen: 32 })).toString("hex");

const sharedNibbles = (left: string, right: string): number => {
  let shared = 0;
  while (shared < left.length && left[shared] === right[shared]) shared += 1;
  return shared;
};

/**
 * An entry `y` of `numbers` and a new entry `z` whose insertion moves `y`'s
 * leaf: `z`'s path shares more nibbles with `y`'s than any other entry's does,
 * so the two end up under a new branch. `y`'s parent branch has at least two
 * other children, so deleting `y` collapses nothing.
 */
const movingPair = (numbers: readonly number[]) => {
  for (const y of numbers) {
    const path = pathOf(y);
    const others = numbers.filter((n) => n !== y).map(pathOf);
    const depth = Math.max(
      ...others.map((other) => sharedNibbles(other, path)),
    );
    const siblings = new Set(
      others
        .filter((other) => sharedNibbles(other, path) === depth)
        .map((other) => other[depth]),
    );
    if (siblings.size < 2) continue;
    for (let z = 5_000; z < 500_000; z += 1)
      if (sharedNibbles(pathOf(z), path) > depth) return { y, z };
  }
  throw new Error("no entry's leaf can be moved without a collapse");
};

/** The prefix of the leaf that holds `n` in `ledger`, read through a
 * witness. */
const leafPrefix = async (ledger: Ledger, n: number) => {
  const recorder = recordWitness(ledger);
  await recorder.ledger.read([testOutRef(n)]);
  const key = testOutRef(n).toString("hex");
  const leaf = recorder
    .witness()
    .nodes.find((node) => node.__kind === "Leaf" && node.key === key);
  return leaf?.prefix;
};

/**
 * `persist(ledger)` either refused with `LedgerNodeMissing` or
 * `LedgerOutputMissing`, wrote nothing and left the root unretained, or
 * retained a ledger every read of which succeeds.
 */
const expectRefusedOrComplete = async (
  store: LedgerStore,
  storage: LedgerStorage,
  ledger: Ledger,
  entries: readonly LedgerEntry[],
) => {
  const before = await storage.counts();
  const refused = await store.persist(ledger).then(
    () => undefined,
    (error: unknown) => error,
  );
  if (refused === undefined) {
    await expectLedgerMatches(await store.at(ledger.root), entries);
    return;
  }
  expect([LedgerNodeMissing, LedgerOutputMissing]).toContain(
    (refused as Error).constructor,
  );
  expect(await storage.counts()).toEqual(before);
  await expect(store.at(ledger.root)).rejects.toBeInstanceOf(LedgerNotRetained);
};

/** Registers the retain cases for one adapter. */
export const describeLedgerStoreRetainConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  /**
   * Every `kept` ledger reads completely, every `dropped` root is refused,
   * and the records equal those of a fresh store holding only `kept`.
   */
  const expectKeepsExactly = async (
    store: LedgerStore,
    storage: LedgerStorage,
    kept: readonly (readonly LedgerEntry[])[],
    dropped: readonly string[],
  ) => {
    for (const entries of kept)
      await expectLedgerMatches(
        await store.at(await ledgerEntriesRoot(entries)),
        entries,
      );
    for (const root of dropped)
      await expect(store.at(root)).rejects.toBeInstanceOf(LedgerNotRetained);
    expect(await storage.counts()).toEqual(await countsOfOnly(open, kept));
  };

  const withStore = async (
    run: (store: LedgerStore, storage: LedgerStorage) => Promise<void>,
  ) => {
    const opened = await open();
    try {
      await run(createLedgerStore(opened.storage), opened.storage);
    } finally {
      await opened.close();
    }
  };

  describe(`ledger store retain: ${adapter}`, () => {
    it.each(["the parent", "the child"] as const)(
      "drops %s of two roots that share most of their nodes",
      async (dropped) =>
        await withStore(async (store, storage) => {
          const entries = base();
          const a = await store.admit(
            entries,
            await ledgerEntriesRoot(entries),
          );
          const mutations = event([0], [400]);
          const b = await a.apply(mutations);
          await store.persist(b);
          const bEntries = appliedEntries(entries, mutations);
          const [kept, gone, keptEntries] =
            dropped === "the parent"
              ? ([b, a, bEntries] as const)
              : ([a, b, entries] as const);
          expect(await store.retain(new Set([kept.root]))).toEqual({
            notRetained: new Set(),
          });
          await expectKeepsExactly(store, storage, [keptEntries], [gone.root]);
        }),
    );

    it.each(["A", "B"] as const)(
      "drops %s of a recurrence A, B, A and persists it again",
      async (dropped) =>
        await withStore(async (store, storage) => {
          const entries = base();
          const a = await store.admit(
            entries,
            await ledgerEntriesRoot(entries),
          );
          const b = await a.apply(event([], [410, 411]));
          await store.persist(b);
          const again = await b.apply(event([410, 411], []));
          expect(again.root).toBe(a.root);
          const before = await storage.counts();
          await store.persist(again);
          expect(await storage.counts()).toEqual(before);
          const bEntries = appliedEntries(entries, event([], [410, 411]));
          const [kept, gone, keptEntries] =
            dropped === "A"
              ? ([b, a, bEntries] as const)
              : ([a, b, entries] as const);
          await store.retain(new Set([kept.root]));
          await expectKeepsExactly(store, storage, [keptEntries], [gone.root]);
          const keptValue = await store.at(kept.root);
          const recreated = await keptValue.apply(
            dropped === "A" ? event([410, 411], []) : event([], [410, 411]),
          );
          expect(recreated.root).toBe(gone.root);
          await store.persist(recreated);
          await expectKeepsExactly(store, storage, [entries, bEntries], []);
          await store.retain(new Set([a.root, b.root]));
          await expectKeepsExactly(store, storage, [entries, bEntries], []);
        }),
    );

    it("reads the requested roots when retain is called", async () => {
      await withStore(async (store) => {
        const entries = base();
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        const bMutations = event([0], [450]);
        const b = await a.apply(bMutations);
        await store.persist(b);
        const window = new Set([a.root, b.root]);
        const retaining = store.retain(window);
        // The caller reuses its set at once.
        window.delete(a.root);
        window.add("ab".repeat(32));
        expect(await retaining).toEqual({ notRetained: new Set() });
        await expectLedgerMatches(await store.at(a.root), entries);
        await expectLedgerMatches(
          await store.at(b.root),
          appliedEntries(entries, bMutations),
        );
      });
    });

    it("lists the requested roots not retained when the deletion ends", async () => {
      await withStore(async (store) => {
        const entries = base();
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        const b = await a.apply(event([0, 1], [460]));
        await store.persist(b);
        const pMutations = event([2], [461]);
        const p = await (await store.at(a.root)).apply(pMutations);
        const neverPersisted = "ab".repeat(32);
        const retaining = store.retain(
          new Set([a.root, p.root, neverPersisted]),
        );
        // Lands after the retained set's replacement, before the deletion
        // ends.
        const persisting = store.persist(p);
        const [retained] = await Promise.all([retaining, persisting]);
        expect(retained).toEqual({ notRetained: new Set([neverPersisted]) });
        await expectLedgerMatches(
          await store.at(p.root),
          appliedEntries(entries, pMutations),
        );
        await expect(store.at(b.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
      });
    });

    it("drops the parent of a fork, then one of its children", async () => {
      await withStore(async (store, storage) => {
        const entries = base();
        const parent = await store.admit(
          entries,
          await ledgerEntriesRoot(entries),
        );
        const [left, right] = [event([1], [420]), event([2], [421])];
        const l = await parent.apply(left);
        await store.persist(l);
        const r = await parent.apply(right);
        await store.persist(r);
        const [lEntries, rEntries] = [left, right].map((mutations) =>
          appliedEntries(entries, mutations),
        );
        await store.retain(new Set([parent.root, l.root, r.root]));
        await expectKeepsExactly(
          store,
          storage,
          [entries, lEntries!, rEntries!],
          [],
        );
        await store.retain(new Set([l.root, r.root]));
        await expectKeepsExactly(
          store,
          storage,
          [lEntries!, rEntries!],
          [parent.root],
        );
        await store.retain(new Set([l.root]));
        await expectKeepsExactly(store, storage, [lEntries!], [r.root]);
      });
    });

    it("drops every root, earlier persisted chains and forks included", async () => {
      await withStore(async (store, storage) => {
        const entries = base();
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        const b = await a.apply(event([0, 1], [400, 401]));
        await store.persist(b);
        const c = await b.apply(event([2, 400], [402]));
        await store.persist(c);
        const d = await a.apply(event([3], [403]));
        await store.persist(d);
        const cEntries = appliedEntries(
          appliedEntries(entries, event([0, 1], [400, 401])),
          event([2, 400], [402]),
        );
        await store.retain(new Set([c.root]));
        await expectKeepsExactly(
          store,
          storage,
          [cEntries],
          [a, b, d].map(({ root }) => root),
        );
        expect(await store.retain(new Set([EMPTY_LEDGER_ROOT]))).toEqual({
          notRetained: new Set(),
        });
        await expect(store.at(c.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        expect(await storage.counts()).toEqual({
          nodes: 0,
          outputs: 0,
          roots: 0,
          nodeRefs: 0,
          outputRefs: 0,
          dead: 0,
        });
        await expectLedgerMatches(await store.at(EMPTY_LEDGER_ROOT), []);
      });
    });

    it("refuses, or keeps complete, a ledger derived from a dropped root", async () => {
      await withStore(async (store, storage) => {
        const entries = base();
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        const bMutations = event([0], [430, 431]);
        const b = await a.apply(bMutations);
        await store.persist(b);
        const derivedMutations = event([5], [432]);
        // Read back by root, so its overlay holds only its own nodes.
        const derived = await (await store.at(b.root)).apply(derivedMutations);
        await store.retain(new Set([a.root]));
        await expectRefusedOrComplete(
          store,
          storage,
          derived,
          appliedEntries(appliedEntries(entries, bMutations), derivedMutations),
        );
        await expectKeepsExactly(store, storage, [entries], [b.root]);
      });
    });

    it("refuses, or keeps complete, a ledger whose moved leaf's output a retain deleted", async () => {
      await withStore(async (store, storage) => {
        const entries = base();
        const x = await store.admit(entries, await ledgerEntriesRoot(entries));
        const { y, z } = movingPair(baseNumbers());
        const s = await (await store.at(x.root)).apply(event([y], []));
        await store.persist(s);
        const moved = await (await store.at(x.root)).apply(event([], [z]));
        const before = await leafPrefix(x, y);
        const after = await leafPrefix(moved, y);
        expect(before).toBeDefined();
        expect(after).toBeDefined();
        expect(after!.length).toBeLessThan(before!.length);
        await store.retain(new Set([s.root]));
        await expectRefusedOrComplete(
          store,
          storage,
          moved,
          appliedEntries(entries, event([], [z])),
        );
      });
    });

    it("lets a persist called during a deletion in between its batches", async () => {
      const opened = await open();
      try {
        /** The writes, in order: a persist's, or a deletion batch's. */
        const writes: ("persist" | "batch")[] = [];
        let duringDeletion: (() => void) | undefined;
        const storage: LedgerStorage = {
          ...opened.storage,
          write: async (change) => {
            await opened.storage.write(change);
            if (change.addRoots.length > 0) writes.push("persist");
            else if (change.removeRoots.length === 0) {
              writes.push("batch");
              const start = duringDeletion;
              duringDeletion = undefined;
              start?.();
            }
          },
        };
        const store = createLedgerStore(storage);
        const entries = base();
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        const bMutations = event([0, 1], [710, 711]);
        const b = await a.apply(bMutations);
        await store.persist(b);
        const c = await b.apply(event([2, 710], [712]));
        await store.persist(c);
        const pMutations = event([3], [713]);
        const p = await (await store.at(a.root)).apply(pMutations);
        let persisting: Promise<void> | undefined;
        duringDeletion = () => {
          persisting = store.persist(p);
        };
        writes.length = 0;
        await store.retain(new Set([a.root, p.root]));
        await persisting;
        expect(writes.indexOf("persist")).toBe(1);
        expect(writes.lastIndexOf("batch")).toBeGreaterThan(1);
        await expectKeepsExactly(
          store,
          opened.storage,
          [entries, appliedEntries(entries, pMutations)],
          [b.root, c.root],
        );
      } finally {
        await opened.close();
      }
    });

    it("keeps a root persisted right after a retain is called, with the dropped nodes it reuses", async () => {
      await withStore(async (store, storage) => {
        const entries = base();
        const a = await store.admit(entries, await ledgerEntriesRoot(entries));
        const bMutations = event([0, 1], [700]);
        const b = await a.apply(bMutations);
        await store.persist(b);
        const nextMutations = event([2], [701]);
        const next = await (await store.at(b.root)).apply(nextMutations);
        const retaining = store.retain(new Set([a.root]));
        const persisting = store.persist(next);
        const [retained] = await Promise.all([retaining, persisting]);
        expect(retained).toEqual({ notRetained: new Set() });
        await expectKeepsExactly(
          store,
          storage,
          [
            entries,
            appliedEntries(appliedEntries(entries, bMutations), nextMutations),
          ],
          [b.root],
        );
      });
    });
  });
};
