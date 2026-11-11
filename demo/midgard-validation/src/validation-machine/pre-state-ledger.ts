/**
 * The ledger a transaction's input resolution proves against: the MPF of
 * §5.3 out-ref keys to canonical ledger descriptors that the prior
 * `utxos_root` commits.
 */
import { Store, Trie } from "#merkle-patricia-forestry";

import type { ValidationMachineLedgerEntry } from "./ledger-mutation.js";

export type PreStateLedger = {
  /**
   * The MPF proof CBOR of each key, in order: a membership proof for a key
   * the ledger holds, the trie's absence proof for one it does not.
   */
  readonly proveResolution: (
    keys: readonly Buffer[],
  ) => Promise<readonly Buffer[]>;
};

/**
 * The pre-state ledger of `entries`, each under the descriptor `descriptors`
 * holds for its out-ref (hex). Each call builds the trie from the entries.
 */
export const ledgerEntriesPreState = (
  entries: readonly ValidationMachineLedgerEntry[],
  descriptors: ReadonlyMap<string, Buffer>,
): PreStateLedger => ({
  proveResolution: async (keys) => {
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    for (const entry of [...entries].sort((left, right) =>
      Buffer.compare(left.outRef, right.outRef),
    )) {
      const descriptorCbor = descriptors.get(entry.outRef.toString("hex"));
      if (descriptorCbor === undefined) {
        throw new Error(
          "input-resolution descriptor state lost a persisted ledger entry",
        );
      }
      await trie.insert(entry.outRef, descriptorCbor);
    }
    // One proof at a time: an absence proof edits the trie while it builds.
    const proofs: Buffer[] = [];
    for (const key of keys) {
      const proof = await trie.prove(
        key,
        !descriptors.has(key.toString("hex")),
      );
      proofs.push(Buffer.from(proof.toCBOR()));
    }
    return proofs;
  },
});
