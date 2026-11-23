/**
 * Conformance cases for `Ledger.entries({ after })`, the resume a ledger
 * snapshot is served from page by page. Expected entries come from a plain
 * `entries()` of the same ledger and from the out-refs' trie paths computed
 * here (blake2b-256 of the out-ref, the trie library's path), never from a
 * resumed iteration. Run by every adapter through
 * `describeLedgerStoreConformance`. The kit imports this module, so nothing
 * here reads the kit's exports while the module loads.
 */
import { midgardBlake2b } from "@al-ft/midgard-core/codec/hash";
import { describe, expect, it } from "vitest";

import {
  createLedgerStore,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  type LedgerStorage,
  type LedgerStore,
} from "../../src/ledger-store/index.js";
import { oracleLedgerRoot } from "./ledger-root-oracle.js";
import {
  type OpenLedgerStorage,
  SIZED_ENTRIES_FROM,
  testLedgerEntries,
  testOutRef,
} from "./ledger-store-conformance.js";

/** `storage`, counting the nodes read through it. */
const countingNodeReads = (storage: LedgerStorage) => {
  let reads = 0;
  const counted: LedgerStorage = {
    readNodes: async (hashes) => {
      reads += hashes.length;
      return await storage.readNodes(hashes);
    },
    readOutputs: (outRefs) => storage.readOutputs(outRefs),
    nodeRefs: (hashes) => storage.nodeRefs(hashes),
    outputRefs: (outRefs) => storage.outputRefs(outRefs),
    retainedRoots: () => storage.retainedRoots(),
    deadNodes: (limit) => storage.deadNodes(limit),
    write: (change) => storage.write(change),
    counts: () => storage.counts(),
  };
  return {
    storage: counted,
    /** The nodes read since the last call. */
    take: () => {
      const taken = reads;
      reads = 0;
      return taken;
    },
  };
};

type Listed = [outRef: string, output: string][];

const listed = async (
  ledger: Ledger,
  options?: { readonly after?: Buffer },
): Promise<Listed> => {
  const result: Listed = [];
  for await (const { outRef, output } of ledger.entries(options))
    result.push([outRef.toString("hex"), output.toString("hex")]);
  return result;
};

const pathOf = (outRef: Buffer): string =>
  Buffer.from(midgardBlake2b(outRef, { dkLen: 32 })).toString("hex");

/** Registers the `entries({ after })` cases for one adapter. */
export const describeLedgerStoreEntriesConformance = (
  adapter: string,
  open: OpenLedgerStorage,
): void => {
  const withStore = async (
    run: (store: LedgerStore, reads: () => number) => Promise<void>,
  ) => {
    const opened = await open();
    try {
      const counting = countingNodeReads(opened.storage);
      await run(createLedgerStore(counting.storage), counting.take);
    } finally {
      await opened.close();
    }
  };

  describe(`ledger store entries resumed after an out-ref: ${adapter}`, () => {
    // Outputs of mixed size: `n % 6` asset policies with names of `n % 4`
    // lengths each.
    const entries = testLedgerEntries(SIZED_ENTRIES_FROM, 300);

    const admitted = async (store: LedgerStore) =>
      await store.at(
        (await store.admit(entries, await oracleLedgerRoot(entries))).root,
      );

    it("resumes after every held entry with exactly the entries after it", async () => {
      await withStore(async (store) => {
        const ledger = await admitted(store);
        const all = await listed(ledger);
        expect(all).toHaveLength(entries.length);
        const paths = all.map(([outRef]) => pathOf(Buffer.from(outRef, "hex")));
        expect(paths).toEqual([...paths].sort());
        // The outputs do differ in size.
        expect(
          new Set(all.map(([, output]) => output.length)).size,
        ).toBeGreaterThan(4);
        for (const [index, [outRef]] of all.entries())
          expect(
            await listed(ledger, { after: Buffer.from(outRef, "hex") }),
          ).toEqual(all.slice(index + 1));
      });
    }, 120_000);

    it("resumes after an out-ref it does not hold at the next path, and after the last entry with nothing", async () => {
      await withStore(async (store) => {
        const ledger = await admitted(store);
        const all = await listed(ledger);
        expect(
          await listed(ledger, { after: Buffer.from(all.at(-1)![0], "hex") }),
        ).toEqual([]);
        for (let n = 900_000; n < 900_040; n += 1) {
          const after = testOutRef(n);
          expect(all.some(([outRef]) => outRef === after.toString("hex"))).toBe(
            false,
          );
          expect(await listed(ledger, { after })).toEqual(
            all.filter(
              ([outRef]) => pathOf(Buffer.from(outRef, "hex")) > pathOf(after),
            ),
          );
        }
      });
    }, 120_000);

    it("reads only the nodes on the way to the resume point, not those before it", async () => {
      await withStore(async (store, reads) => {
        const ledger = await admitted(store);
        reads();
        const all = await listed(ledger);
        const full = reads();
        const resumed = await listed(ledger, {
          after: Buffer.from(all.at(-2)![0], "hex"),
        });
        expect(resumed).toEqual(all.slice(-1));
        expect(reads()).toBeLessThan(full / 10);
      });
    });

    it("resumes the empty ledger with nothing", async () => {
      await withStore(async (store) => {
        const empty = await store.at(EMPTY_LEDGER_ROOT);
        expect(await listed(empty, { after: testOutRef(1) })).toEqual([]);
      });
    });
  });
};
