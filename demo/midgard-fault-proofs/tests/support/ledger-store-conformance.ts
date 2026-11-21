/**
 * Conformance cases every ledger storage adapter runs. Expected roots and
 * proofs come from today's trie path (`ledgerEntriesRoot`,
 * `keyValuePhasResolutionProofs`), which builds a fresh in-memory trie from
 * the whole entry set, never from the store under test.
 */
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
  EMPTY_LEDGER_ROOT,
  type Ledger,
  type LedgerMutation,
  LedgerMutationRefused,
  LedgerNotRetained,
  type LedgerStorage,
  type LedgerStore,
} from "../../src/ledger-store/index.js";
import {
  keyValuePhasResolutionProofs,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";

export type OpenedLedgerStorage = {
  readonly storage: LedgerStorage;
  readonly close: () => Promise<void>;
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

/** Entry `n` with a one-coin output; `undecodable` gives it the fallback
 * descriptor instead. */
export const testLedgerEntry = (
  n: number,
  undecodable = false,
): LedgerEntry => ({
  outRef: testOutRef(n),
  output: undecodable
    ? undecodableScriptRefOutput
    : makeOutput(BigInt(2_000_000 + n)),
});

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
  const expected = await keyValuePhasResolutionProofs(reference, keys);
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

const unknownRoot = "ab".repeat(32);

/**
 * A storage whose first `sweepNodes` page is held back until `release`: the
 * sweep has listed its candidates but deleted none of them yet.
 */
const gatedStorage = (storage: LedgerStorage) => {
  let reached!: () => void;
  let release!: () => void;
  const atGate = new Promise<void>((resolve) => (reached = resolve));
  const opened = new Promise<void>((resolve) => (release = resolve));
  let first = true;
  return {
    atGate,
    release: () => release(),
    storage: {
      ...storage,
      sweepNodes: async (after: string | undefined, limit: number) => {
        const page = await storage.sweepNodes(after, limit);
        if (first) {
          first = false;
          reached();
          await opened;
        }
        return page;
      },
    } satisfies LedgerStorage,
  };
};

const BASE = [...testLedgerEntries(0, 40), testLedgerEntry(1_000, true)];

/** Registers the conformance cases for one adapter. */
export const describeLedgerStoreConformance = (
  adapter: string,
  open: OpenLedgerStorage,
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

  /** Record counts of a fresh store holding only `ledgers`. */
  const countsOfOnly = async (ledgers: readonly (readonly LedgerEntry[])[]) => {
    const opened = await open();
    try {
      const store = createLedgerStore(opened.storage);
      for (const entries of ledgers)
        await store.persist(
          await store.admit(entries, await ledgerEntriesRoot(entries)),
        );
      return await opened.storage.counts();
    } finally {
      await opened.close();
    }
  };

  describe(`ledger store conformance: ${adapter}`, () => {
    it("admits a snapshot with today's root and proofs, before and after persist", async () => {
      await withStore(async (store) => {
        const root = await ledgerEntriesRoot(BASE);
        const admitted = await store.admit(BASE, root);
        await expectLedgerMatches(admitted, BASE);
        await store.persist(admitted);
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
        await store.persist(full);
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
        await store.persist(ledger);
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

    it("refuses roots it does not retain and snapshots that do not open their root", async () => {
      await withStore(async (store) => {
        const ledger = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        await expect(store.at(ledger.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        await store.persist(ledger);
        const next = await ledger.apply(event([0], [200]));
        await expect(store.at(next.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        await expect(store.at(unknownRoot)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        await expect(
          store.retain(new Set([ledger.root, unknownRoot])),
        ).rejects.toBeInstanceOf(LedgerNotRetained);
        await expectLedgerMatches(await store.at(ledger.root), BASE);
        await expect(
          store.admit(BASE.slice(1), ledger.root),
        ).rejects.toBeInstanceOf(LedgerRootMismatchError);
      });
    });

    it("keeps every earlier ledger value valid while later ones are built and proved", async () => {
      await withStore(async (store) => {
        const base = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        await store.persist(base);
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

    it("retain deletes everything its roots do not reach, earlier persisted roots included", async () => {
      await withStore(async (store, storage) => {
        const a = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        await store.persist(a);
        const b = await a.apply(event([0, 1], [400, 401]));
        await store.persist(b);
        const c = await b.apply(event([2, 400], [402]));
        await store.persist(c);
        const d = await a.apply(event([3], [403]));
        await store.persist(d);
        const cEntries = appliedEntries(
          appliedEntries(BASE, event([0, 1], [400, 401])),
          event([2, 400], [402]),
        );
        await store.retain(new Set([c.root]));
        for (const dropped of [a, b, d])
          await expect(store.at(dropped.root)).rejects.toBeInstanceOf(
            LedgerNotRetained,
          );
        await expectLedgerMatches(await store.at(c.root), cEntries);
        expect(await storage.counts()).toEqual(await countsOfOnly([cEntries]));
        await store.retain(new Set([EMPTY_LEDGER_ROOT]));
        await expect(store.at(c.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        expect(await storage.counts()).toEqual({
          nodes: 0,
          outputs: 0,
          roots: 0,
        });
        await expectLedgerMatches(await store.at(EMPTY_LEDGER_ROOT), []);
      });
    });

    it("keeps a root persisted while a sweep runs, with the dropped nodes it reuses", async () => {
      const opened = await open();
      try {
        const gate = gatedStorage(opened.storage);
        const store = createLedgerStore(gate.storage);
        const a = await store.admit(BASE, await ledgerEntriesRoot(BASE));
        await store.persist(a);
        const kept = await a.apply(event([0, 1], [500, 501]));
        await store.persist(kept);
        const dropped = await a.apply(event([2, 3, 4], [502]));
        await store.persist(dropped);
        const sweeping = store.retain(new Set([kept.root]));
        await gate.atGate;
        await expect(store.at(dropped.root)).rejects.toBeInstanceOf(
          LedgerNotRetained,
        );
        // Built on the dropped root: it shares nodes the sweep has not marked.
        const late = await dropped.apply(event([5], [503]));
        await store.persist(late);
        gate.release();
        await sweeping;
        const keptEntries = appliedEntries(BASE, event([0, 1], [500, 501]));
        const lateEntries = appliedEntries(
          appliedEntries(BASE, event([2, 3, 4], [502])),
          event([5], [503]),
        );
        await expectLedgerMatches(await store.at(kept.root), keptEntries);
        await expectLedgerMatches(await store.at(late.root), lateEntries);
        for (const gone of [a, dropped])
          await expect(store.at(gone.root)).rejects.toBeInstanceOf(
            LedgerNotRetained,
          );
        await store.retain(new Set([kept.root, late.root]));
        expect(await opened.storage.counts()).toEqual(
          await countsOfOnly([keptEntries, lateEntries]),
        );
      } finally {
        await opened.close();
      }
    });
  });
};
