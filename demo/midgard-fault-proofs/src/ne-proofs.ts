import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";

export type TrieEntry = {
  readonly key: Buffer;
  readonly value: Buffer;
};

const buildTrie = async (entries: readonly TrieEntry[]): Promise<Trie> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of entries) {
    await trie.insert(entry.key, entry.value);
  }
  return trie;
};

export const trieRootHex = (trie: Trie): string =>
  trie.hash == null ? "" : Buffer.from(trie.hash).toString("hex");

/** Root of a trie built from the given entries. */
export const computeTrieRoot = async (
  entries: readonly TrieEntry[],
): Promise<string> => trieRootHex(await buildTrie(entries));

/** Membership proof CBOR for a key present in the trie. */
export const buildMembershipProof = async (
  entries: readonly TrieEntry[],
  key: Buffer,
): Promise<string> => {
  const trie = await buildTrie(entries);
  const proof = await trie.prove(key);
  return proof.toCBOR().toString("hex");
};

/**
 * Non-membership (exclusion) proof CBOR for a key absent from the trie. The MPF
 * library only proves present keys, so insert the key with a throwaway value,
 * prove it, and return that proof: `excluding(key, proof)` then reconstructs the
 * original (key-absent) root, which is exactly what the on-chain
 * `pexcludes.exclusion.withdraw` validator checks.
 */
export const buildNonMembershipProof = async (
  entries: readonly TrieEntry[],
  absentKey: Buffer,
): Promise<string> => {
  const trie = await buildTrie(entries);
  // `get` walks the trie and throws on an empty one, so only probe when present.
  if (trie.hash != null && (await trie.get(absentKey)) !== undefined) {
    throw new Error(
      "Cannot build a non-membership proof for a key that is present in the trie.",
    );
  }
  await trie.insert(absentKey, Buffer.from(""));
  const proof = await trie.prove(absentKey);
  return proof.toCBOR().toString("hex");
};
