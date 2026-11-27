/**
 * Expected ledger roots and proofs for tests. Each is computed from the
 * entries with the MPF library's own in-memory trie (`Trie.fromList`) over
 * each entry's canonical leaf, and never reads a ledger store, so a store
 * under test is checked against a path it does not share. Tests that need a
 * `Ledger` admit the entries to a memory store against the oracle's root.
 */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk/core";

import { type LedgerEntry, ledgerEntryLeaf } from "../../src/ledger-at.js";

/** A fresh library trie over the leaves of `entries`, refusing an out-ref
 * listed twice, which no ledger holds. */
const trieOf = async (entries: readonly LedgerEntry[]): Promise<Trie> => {
  const seen = new Set<string>();
  const pairs = entries.map((candidate) => {
    const { entry, leaf } = ledgerEntryLeaf(candidate, "ledger root oracle");
    const key = entry.outRef.toString("hex");
    if (seen.has(key))
      throw new Error(`ledger root oracle: out-ref ${key} is listed twice`);
    seen.add(key);
    return { key: entry.outRef, value: leaf };
  });
  return await Trie.fromList(pairs);
};

/** The library's root hash as a ledger root: an empty trie has no hash, and
 * the all-zero hash names the empty ledger. */
const ledgerRootOf = (hash: Uint8Array | null | undefined): string => {
  if (hash === null || hash === undefined) return EMPTY_MERKLE_TREE_ROOT;
  const hex = Buffer.from(hash).toString("hex");
  return hex === "00".repeat(32) ? EMPTY_MERKLE_TREE_ROOT : hex;
};

/** The root of the ledger holding exactly `entries`. */
export const oracleLedgerRoot = async (
  entries: readonly LedgerEntry[],
): Promise<string> => ledgerRootOf((await trieOf(entries)).hash);

/** Each key's proof CBOR over the ledger holding exactly `entries`:
 * membership when the key is held, absence otherwise. Absence proofs edit
 * the trie they are built on, so each takes a trie of its own. */
export const oracleLedgerProofs = async (
  entries: readonly LedgerEntry[],
  keys: readonly Buffer[],
): Promise<Buffer[]> => {
  const held = new Set(entries.map(({ outRef }) => outRef.toString("hex")));
  const members = await trieOf(entries);
  const proofs: Buffer[] = [];
  for (const key of keys) {
    const member = held.has(key.toString("hex"));
    const trie = member ? members : await trieOf(entries);
    proofs.push(
      Buffer.from((await trie.prove(Buffer.from(key), !member)).toCBOR()),
    );
  }
  return proofs;
};
