/**
 * Conformance cases every ledger storage adapter runs. Expected roots and
 * proofs come from the MPF library's in-memory trie (`Trie.fromList`) over
 * the whole entry set, never from the store under test.
 */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import {
  ledgerEntriesRoot,
  type LedgerEntry,
  ledgerEntryLeaf,
  LedgerRootMismatchError,
} from "../../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  LedgerEntryNotCanonical,
  type LedgerMutation,
  LedgerMutationRefused,
  LedgerNotRetained,
  type LedgerStorage,
  type LedgerStorageCounts,
  type LedgerStore,
  type NodeHash,
  type StoredNode,
} from "../../src/ledger-store/index.js";
import {
  type KeyValuePhasRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { describeLedgerStoreAdmitConformance } from "./ledger-store-conformance-admit.js";
import { describeLedgerStoreCrashConformance } from "./ledger-store-conformance-crash.js";
import { describeLedgerStoreRandomConformance } from "./ledger-store-conformance-random.js";
import { describeLedgerStoreRetainConformance } from "./ledger-store-conformance-retain.js";

export type OpenedLedgerStorage = {
  readonly storage: LedgerStorage;
  readonly close: () => Promise<void>;
  /**
   * Closes these records and opens them again, as a restarted process would:
   * a new storage object over the same records.
   */
  readonly reopen: () => Promise<OpenedLedgerStorage>;
};

export type OpenLedgerStorage = () => Promise<OpenedLedgerStorage>;

const plainOutput = Buffer.from(
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  "hex",
);
// A native script_ref whose body does not decode: its leaf is the
// bytes-only descriptor, not a canonical one.
const undecodableScriptRefOutput = Buffer.concat([
  Buffer.from("a3", "hex"),
  plainOutput.subarray(1),
  Buffer.from("038200428109", "hex"),
]);

/** Out-ref `n`: a distinct transaction id per `n`. */
export const testOutRef = (n: number): Buffer => {
  const txId = Buffer.alloc(32, 0x5a);
  txId.writeUInt32BE(n, 0);
  return encodeMidgardSpendInputItem({ txId, outputIndex: n % 3 });
};

/** The entry whose output has the bytes-only fallback descriptor. */
export const UNDECODABLE_ENTRY = 1_000;

/** Entry `n`: a one-coin output, except `UNDECODABLE_ENTRY`. */
export const testLedgerEntry = (n: number): LedgerEntry => ({
  outRef: testOutRef(n),
  output:
    n === UNDECODABLE_ENTRY
      ? undecodableScriptRefOutput
      : makeOutput(BigInt(2_000_000 + n)),
});

/**
 * The output of a `testOutRef` out-ref, for an adapter that reads outputs
 * from elsewhere than the store (the node's LevelDB adapter).
 */
export const testOutputOf = (outRef: string): Buffer | undefined => {
  const bytes = Buffer.from(outRef, "hex");
  if (bytes.length < 7) return undefined;
  const n = bytes.readUInt32BE(3);
  return testOutRef(n).equals(bytes) ? testLedgerEntry(n).output : undefined;
};

export const testLedgerEntries = (from: number, count: number): LedgerEntry[] =>
  Array.from({ length: count }, (_, index) => testLedgerEntry(from + index));

/** Today's trie over `entries`, the reference the store must equal. */
const referenceTrie = async (entries: readonly LedgerEntry[]) =>
  await keyValuePhasRootWithCount(
    entries.map((candidate) => {
      const { entry, leaf } = ledgerEntryLeaf(candidate, "reference");
      return { key: entry.outRef, value: leaf };
    }),
  );

/** Each key's proof CBOR from a library trie of `reference`'s entries:
 * membership when held, absence otherwise. Absence proofs edit the trie they
 * are built on, so each takes a trie of its own. */
const referenceProofs = async (
  reference: KeyValuePhasRoot,
  keys: readonly Buffer[],
): Promise<Buffer[]> => {
  const trieOf = async () =>
    await Trie.fromList(
      reference.entries.map(({ key, value }) => ({
        key: Buffer.from(key),
        value: Buffer.from(value),
      })),
    );
  const held = new Set(reference.entries.map(({ key }) => key.toString("hex")));
  const members = await trieOf();
  const proofs: Buffer[] = [];
  for (const key of keys) {
    const member = held.has(key.toString("hex"));
    const trie = member ? members : await trieOf();
    proofs.push(
      Buffer.from((await trie.prove(Buffer.from(key), !member)).toCBOR()),
    );
  }
  return proofs;
};

const sortedHex = (entries: Iterable<{ outRef: Buffer; output: Buffer }>) =>
  [...entries]
    .map(({ outRef, output }) => [
      outRef.toString("hex"),
      output.toString("hex"),
    ])
    .sort(([left], [right]) => (left! < right! ? -1 : 1));

/**
 * `ledger` holds exactly `entries`: its root, each held and each `absent`
 * out-ref's output and proof bytes, and its whole entry set equal today's
 * trie over `entries`.
 */
export const expectLedgerMatches = async (
  ledger: Ledger,
  entries: readonly LedgerEntry[],
  absent: readonly Buffer[] = [testOutRef(900_001), testOutRef(900_002)],
): Promise<void> => {
  const reference = await referenceTrie(entries);
  expect(ledger.root).toBe(reference.root);
  const keys = [...entries.map(({ outRef }) => outRef), ...absent];
  const expected = await referenceProofs(reference, keys);
  const reads = await ledger.read(keys);
  const outputs = new Map(
    entries.map(({ outRef, output }) => [outRef.toString("hex"), output]),
  );
  for (const [index, key] of keys.entries()) {
    const read = reads.get(key.toString("hex"))!;
    expect(read.output?.toString("hex")).toBe(
      outputs.get(key.toString("hex"))?.toString("hex"),
    );
    expect((await read.proof()).toString("hex")).toBe(
      expected[index]!.toString("hex"),
    );
  }
  const listed = [];
  for await (const entry of ledger.entries()) listed.push(entry);
  expect(sortedHex(listed)).toEqual(sortedHex(entries));
};

/** The entry set after `mutations`, as the store must see it. */
export const appliedEntries = (
  entries: readonly LedgerEntry[],
  mutations: readonly LedgerMutation[],
): LedgerEntry[] => {
  const byOutRef = new Map(
    entries.map((entry) => [entry.outRef.toString("hex"), entry]),
  );
  for (const mutation of mutations) {
    const key = mutation.outRef.toString("hex");
    if (mutation.type === "delete") byOutRef.delete(key);
    else
      byOutRef.set(key, { outRef: mutation.outRef, output: mutation.output });
  }
  return [...byOutRef.values()];
};

/** One event: delete `deleted`, insert entries `inserted`. */
export const event = (
  deleted: readonly number[],
  inserted: readonly number[],
): LedgerMutation[] => [
  ...deleted.map((n) => ({ type: "delete" as const, outRef: testOutRef(n) })),
  ...inserted.map((n) => ({ type: "insert" as const, ...testLedgerEntry(n) })),
];

/**
 * Every node reachable from `root` in `storage`, keyed by hash: the nodes a
 * caller that already holds a ledger hands `adopt`.
 */
export const storedNodesOf = async (
  storage: LedgerStorage,
  root: string,
): Promise<Map<NodeHash, StoredNode>> => {
  const nodes = new Map<NodeHash, StoredNode>();
  let level = [root];
  while (level.length > 0) {
    const unread = [...new Set(level)].filter((hash) => !nodes.has(hash));
    const read = await storage.readNodes(unread);
    level = [];
    for (const [index, hash] of unread.entries()) {
      const node = read[index];
      if (node === undefined) throw new Error(`node ${hash} is not stored`);
      nodes.set(hash, node);
      if (node.__kind === "Branch")
        for (const child of node.children)
          if (child !== null) level.push(child);
    }
  }
  return nodes;
};

/** Record counts of a fresh store over `open` holding only `ledgers`. */
export const countsOfOnly = async (
  open: OpenLedgerStorage,
  ledgers: readonly (readonly LedgerEntry[])[],
): Promise<LedgerStorageCounts> => {
  const opened = await open();
  try {
    const store = createLedgerStore(opened.storage);
    const roots = new Set<string>();
    for (const entries of ledgers)
      roots.add(
        (await store.admit(entries, await ledgerEntriesRoot(entries))).root,
      );
    // Deletes what a snapshot written in several batches left unreferenced.
    await store.retain(roots);
    return await opened.storage.counts();
  } finally {
    await opened.close();
  }
};

const unknownRoot = "ab".repeat(32);

const BASE = [...testLedgerEntries(0, 40), testLedgerEntry(UNDECODABLE_ENTRY)];

/** Fresh memory storage, which is never reopened. */
const openMemory: OpenLedgerStorage = async () => ({
  storage: createMemoryLedgerStorage(),
  close: async () => undefined,
  reopen: async () => {
    throw new Error("memory comparison storage is never reopened");
  },
});

/**
 * Registers the conformance cases for one adapter. `reference` opens the
 * storage the random run builds its comparison stores on: memory, unless the
 * adapter's `counts()` differs from memory's for the same records.
 */
export const describeLedgerStoreConformance = (
  adapter: string,
  open: OpenLedgerStorage,
  reference: OpenLedgerStorage = openMemory,
): void => {
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

  describe(`ledger store conformance: ${adapter}`, () => {
    it("admits a snapshot with today's root and proofs, and retains it", async () => {
      await withStore(async (store) => {
        const root = await ledgerEntriesRoot(BASE);
        const admitted = await store.admit(BASE, root);
        await expectLedgerMatches(admitted, BASE);
        await expectLedgerMatches(await store.at(root), BASE);
        async function* streamed() {
          yield* BASE;
        }
        expect((await store.admit(streamed(), root)).root).toBe(root);
      });
    });

    it("applies events with today's roots and proofs, persisted or not", async () => {
      await withStore(async (store) => {
        let entries: LedgerEntry[] = [];
        let ledger = await store.at(EMPTY_LEDGER_ROOT);
        const events = [
          event([], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]),
          event([0, 1], [100, 101]),
          event([2, 100], [102, 103]),
          event([3, 4], [104, 0]),
          event([101, 102, 103, 104, 0, 5, 6], []),
        ];
        for (const [index, mutations] of events.entries()) {
          ledger = await ledger.apply(mutations);
          entries = appliedEntries(entries, mutations);
          await expectLedgerMatches(ledger, entries);
          if (index % 2 === 0) {
            await store.persist(ledger);
            await expectLedgerMatches(await store.at(ledger.root), entries);
          }
        }
      });
    });

    it("deletes down to the empty ledger, which is always available", async () => {
      await withStore(async (store) => {
        await expectLedgerMatches(await store.at(EMPTY_LEDGER_ROOT), []);
        const full = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        const empty = await full.apply(
          BASE.map(({ outRef }) => ({ type: "delete" as const, outRef })),
        );
        expect(empty.root).toBe(EMPTY_LEDGER_ROOT);
        await expectLedgerMatches(empty, []);
        await store.persist(empty);
        await expectLedgerMatches(await store.at(EMPTY_LEDGER_ROOT), []);
      });
    });

    it("refuses a mutation as a typed value and leaves the ledger unchanged", async () => {
      await withStore(async (store) => {
        const ledger = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        const absent = await ledger
          .apply([{ type: "delete", outRef: testOutRef(500) }])
          .catch((error: unknown) => error);
        expect(absent).toBeInstanceOf(LedgerMutationRefused);
        expect(absent).toMatchObject({ reason: "absent", index: 0 });
        const present = await ledger
          .apply(event([0], [5]))
          .catch((error: unknown) => error);
        expect(present).toBeInstanceOf(LedgerMutationRefused);
        expect(present).toMatchObject({ reason: "present", index: 1 });
        expect((present as LedgerMutationRefused).outRef).toEqual(
          testOutRef(5),
        );
        await expectLedgerMatches(ledger, BASE);
        await expectLedgerMatches(await store.at(ledger.root), BASE);
      });
    });

    it("refuses an entry with no trie leaf as a typed value, inserted or admitted", async () => {
      await withStore(async (store) => {
        const ledger = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        const leafless = {
          outRef: testOutRef(600),
          output: Buffer.from("00", "hex"),
        };
        const inserted = await ledger
          .apply([...event([0], []), { type: "insert", ...leafless }])
          .catch((error: unknown) => error);
        expect(inserted).toBeInstanceOf(LedgerEntryNotCanonical);
        expect((inserted as LedgerEntryNotCanonical).outRef).toEqual(
          leafless.outRef,
        );
        await expectLedgerMatches(ledger, BASE);
        await expect(
          store.admit([...BASE, leafless], ledger.root),
        ).rejects.toBeInstanceOf(LedgerEntryNotCanonical);
      });
    });

    it("refuses roots it does not retain and snapshots that do not open their root", async () => {
      await withStore(async (store, storage) => {
        const ledger = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        const next = await ledger.apply(event([0], [200]));
        await expect(store.at(next.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        await expect(store.at(unknownRoot)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        const before = await storage.counts();
        expect(
          await store.retain(new Set([ledger.root, unknownRoot, next.root])),
        ).toEqual({ notRetained: new Set([unknownRoot, next.root]) });
        expect(await storage.counts()).toEqual(before);
        await expectLedgerMatches(await store.at(ledger.root), BASE);
        await expect(
          store.admit(BASE.slice(1), ledger.root),
        ).rejects.toBeInstanceOf(LedgerRootMismatchError);
      });
    });

    it("keeps every earlier ledger value valid while later ones are built and proved", async () => {
      await withStore(async (store) => {
        const base = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        const first = await base.apply(event([0, 1], [300, 301]));
        const second = await first.apply(event([300, 2], [302]));
        const expectations = [
          [base, BASE],
          [first, appliedEntries(BASE, event([0, 1], [300, 301]))],
          [
            second,
            appliedEntries(
              appliedEntries(BASE, event([0, 1], [300, 301])),
              event([300, 2], [302]),
            ),
          ],
        ] as const;
        await Promise.all(
          expectations.map(([ledger, entries]) =>
            expectLedgerMatches(
              ledger,
              entries,
              [
                testOutRef(0),
                testOutRef(300),
                testOutRef(302),
                testOutRef(999_999),
              ].filter(
                (key) => !entries.some(({ outRef }) => outRef.equals(key)),
              ),
            ),
          ),
        );
      });
    });

    it("reads, proves and persists ledgers derived from one parent at different depths", async () => {
      await withStore(async (store) => {
        // `parent` is derived once; `deep` is eight more applies on it,
        // which fold `parent`'s in-memory records into new ones below its
        // own; `sibling` is one apply on `parent` after that.
        const base = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        const parentEvent = event([0, 1], [400, 401]);
        const parent = await base.apply(parentEvent);
        const parentEntries = appliedEntries(BASE, parentEvent);
        let deep = parent;
        let deepEntries = parentEntries;
        for (let step = 0; step < 8; step += 1) {
          const mutations = event([2 + step], [410 + step]);
          deep = await deep.apply(mutations);
          deepEntries = appliedEntries(deepEntries, mutations);
        }
        const siblingEvent = event([400, 20], [0, 450]);
        const sibling = await parent.apply(siblingEvent);
        const siblingEntries = appliedEntries(parentEntries, siblingEvent);
        const expectations = [
          [parent, parentEntries],
          [deep, deepEntries],
          [sibling, siblingEntries],
        ] as const;
        for (const [ledger, entries] of expectations)
          await expectLedgerMatches(ledger, entries);
        for (const [ledger, entries] of [...expectations].reverse()) {
          await store.persist(ledger);
          await expectLedgerMatches(await store.at(ledger.root), entries);
        }
      });
    });

    it("refuses a second store over the same storage", async () => {
      await withStore(async (_store, storage) => {
        expect(() => createLedgerStore(storage)).toThrow(
          /already owns this storage/,
        );
      });
    });
  });
  describeLedgerStoreAdmitConformance(adapter, open);
  describeLedgerStoreRetainConformance(adapter, open);
  describeLedgerStoreRandomConformance(adapter, open, reference);
  describeLedgerStoreCrashConformance(adapter, open);
};
