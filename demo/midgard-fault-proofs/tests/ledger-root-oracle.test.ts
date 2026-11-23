/**
 * The test-support root oracle and the ledger store's `admit` reach the same
 * root over the same fixture entries by two separate paths (the library's
 * `Trie.fromList`, and the store's batched build), and over a canonical
 * block fixture both equal the `utxos_root` its builder committed through the
 * counted-root helper.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { type LedgerEntry, LedgerRootMismatchError } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
} from "../src/ledger-store/index.js";
import { buildCanonicalBlockFixture } from "./helpers/canonical-block-evidence-fixture.js";
import { fixtureLedgerEntriesByRoot } from "./support/fixture-ledger.js";
import { oracleLedgerRoot } from "./support/ledger-root-oracle.js";
import {
  testLedgerEntries,
  testLedgerEntry,
  UNDECODABLE_ENTRY,
} from "./support/ledger-store-conformance.js";

/** `entries` admitted to a fresh memory store against `root`; `admit`
 * refuses a root other than the one it builds. */
const admittedRoot = async (entries: readonly LedgerEntry[], root: string) =>
  (await createLedgerStore(createMemoryLedgerStorage()).admit(entries, root))
    .root;

const blockFixtureLedger = async () => {
  const block = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: testLedgerEntries(0, 24).map(({ outRef, output }) => ({
      key: outRef,
      value: output,
    })),
  });
  return {
    entries: fixtureLedgerEntriesByRoot(block.header.utxosRoot),
    committed: block.header.utxosRoot,
  };
};

describe("the ledger root oracle", () => {
  it("gives admit's root for the conformance snapshot, whatever the order, the bytes-only descriptor entry included", async () => {
    const entries = [
      ...testLedgerEntries(0, 300),
      testLedgerEntry(UNDECODABLE_ENTRY),
    ];
    const root = await oracleLedgerRoot(entries);
    expect(await admittedRoot(entries, root)).toBe(root);
    expect(await oracleLedgerRoot([...entries].reverse())).toBe(root);
    // One entry fewer is another ledger, which `admit` does not take at
    // this root.
    await expect(admittedRoot(entries.slice(1), root)).rejects.toBeInstanceOf(
      LedgerRootMismatchError,
    );
  });

  it("gives the root a canonical block fixture committed, and admit's", async () => {
    const { entries, committed } = await blockFixtureLedger();
    expect(entries).toHaveLength(24);
    expect(await oracleLedgerRoot(entries)).toBe(committed);
    expect(await admittedRoot(entries, committed)).toBe(committed);
  });

  it("gives the empty ledger's root for no entries", async () => {
    expect(await oracleLedgerRoot([])).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(await admittedRoot([], SDK.EMPTY_MERKLE_TREE_ROOT)).toBe(
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
  });

  it("refuses an out-ref listed twice", async () => {
    await expect(
      oracleLedgerRoot([testLedgerEntry(1), testLedgerEntry(1)]),
    ).rejects.toThrow("listed twice");
  });
});
