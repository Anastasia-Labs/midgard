/**
 * Conformance cases on what a sweep leaves behind, run by every adapter
 * through `describeLedgerStoreConformance`. The kit imports this module, so
 * nothing here reads the kit's exports while the module loads.
 */
import { midgardBlake2b } from "@al-ft/midgard-core/codec/hash";
import { describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../../src/ledger-at.js";
import {
  createLedgerStore,
  type Ledger,
  LedgerNotRetained,
  LedgerOutputMissing,
  type LedgerStore,
  recordWitness,
} from "../../src/ledger-store/index.js";
import {
  appliedEntries,
  event,
  expectLedgerMatches,
  type OpenLedgerStorage,
  testLedgerEntry,
  testOutRef,
  UNDECODABLE_ENTRY,
} from "./ledger-store-conformance.js";
import { gatedStorage } from "./ledger-store-sweep-faults.js";

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
 * A persisted base ledger `x`, its child `s` without `y` (persisted), and its
 * child `moved` with `z` inserted (not persisted), whose leaf for `y` is a new
 * node carrying no output: the trie moved it under a branch with `z`.
 */
const movedLeafScene = async (store: LedgerStore) => {
  const entries = base();
  const admitted = await store.admit(entries, await ledgerEntriesRoot(entries));
  await store.persist(admitted);
  const x = await store.at(admitted.root);
  const { y, z } = movingPair(baseNumbers());
  const s = await x.apply(event([y], []));
  await store.persist(s);
  const moved = await x.apply(event([], [z]));
  const before = await leafPrefix(x, y);
  const after = await leafPrefix(moved, y);
  expect(before).toBeDefined();
  expect(after).toBeDefined();
  expect(after!.length).toBeLessThan(before!.length);
  return { s, moved, movedEntries: appliedEntries(entries, event([], [z])) };
};

/**
 * `persist(ledger)` either refused with `LedgerOutputMissing` and left the
 * root unretained, or retained a ledger every read of which succeeds.
 */
const expectRefusedOrComplete = async (
  store: LedgerStore,
  ledger: Ledger,
  entries: Parameters<typeof expectLedgerMatches>[1],
  persisted: unknown,
) => {
  if (persisted === undefined) {
    await expectLedgerMatches(await store.at(ledger.root), entries);
    return;
  }
  expect(persisted).toBeInstanceOf(LedgerOutputMissing);
  await expect(store.at(ledger.root)).rejects.toBeInstanceOf(LedgerNotRetained);
};

const outcome = (persisting: Promise<void>): Promise<unknown> =>
  persisting.then(
    () => undefined,
    (error: unknown) => error,
  );

/** Registers the sweep cases for one adapter. */
export const describeLedgerStoreSweepConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  describe(`ledger store sweeps: ${adapter}`, () => {
    it("never retains a ledger whose moved leaf's output a finished sweep deleted", async () => {
      const opened = await open();
      try {
        const store = createLedgerStore(opened.storage);
        const { s, moved, movedEntries } = await movedLeafScene(store);
        await store.retain(new Set([s.root]));
        await expectRefusedOrComplete(
          store,
          moved,
          movedEntries,
          await outcome(store.persist(moved)),
        );
      } finally {
        await opened.close();
      }
    });

    it("never retains a ledger persisted while a sweep deletes outputs without its moved leaf's output", async () => {
      const opened = await open();
      try {
        const gate = gatedStorage(opened.storage, "outputs");
        const store = createLedgerStore(gate.storage);
        const { s, moved, movedEntries } = await movedLeafScene(store);
        const sweeping = store.retain(new Set([s.root]));
        await gate.atGate;
        const persisted = await outcome(store.persist(moved));
        gate.release();
        await sweeping;
        await expectRefusedOrComplete(store, moved, movedEntries, persisted);
      } finally {
        await opened.close();
      }
    });
  });
};
