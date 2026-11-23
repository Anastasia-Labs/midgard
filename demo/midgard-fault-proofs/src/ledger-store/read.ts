/**
 * Read-side helpers over a `Ledger`: a ledger over nodes the caller can only
 * read, the library trie of a ledger for code that takes a `Trie`, and the
 * SDK proof of one key.
 */
import type { Trie } from "@aiken-lang/merkle-patricia-forestry";
import type * as SDK from "@al-ft/midgard-sdk/core";

import { sdkProofFromCbor } from "../transition-trace/phas.js";
import { LedgerTrieStore, loadTrie } from "./engine.js";
import { type Ledger, type LedgerSource, LedgerValue } from "./ledger.js";

/**
 * The ledger with `root` over `source`, for a process that can read nodes and
 * outputs but does not own the store (a worker asking the owner for nodes, a
 * family reading a step). Its `apply` keeps new nodes in memory, as every
 * ledger's does; it cannot be persisted. A node `source` does not return is
 * refused with `LedgerNodeMissing` when a read reaches it.
 */
export const ledgerAt = (root: string, source: LedgerSource): Ledger =>
  new LedgerValue(root, undefined, undefined, source);

/**
 * A fresh library trie of `ledger`: it reads the ledger's nodes, and its own
 * edits stay in this trie, so `ledger` is unchanged by them. For code that
 * takes a `Trie` (`applyValidationLedgerMutations`,
 * `buildMidgardMpfDeletionOpening`, many proofs from one trie).
 */
export const ledgerTrie = async (ledger: Ledger): Promise<Trie> => {
  if (!(ledger instanceof LedgerValue))
    throw new Error("ledgerTrie: not a ledger store ledger");
  return await loadTrie(new LedgerTrieStore(ledger.readNode), ledger.root);
};

/**
 * The proof of `key` against `ledger.root` in the SDK's on-chain shape:
 * membership when the ledger holds `key`, non-membership when it does not.
 */
export const ledgerProof = async (
  ledger: Ledger,
  key: Buffer,
): Promise<SDK.Proof> => {
  const trie = await ledgerTrie(ledger);
  const absent = (await trie.get(Buffer.from(key))) === undefined;
  const proof = await trie.prove(Buffer.from(key), absent);
  return sdkProofFromCbor(proof.toCBOR());
};
