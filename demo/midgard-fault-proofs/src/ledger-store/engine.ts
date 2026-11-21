/**
 * The vendored trie library over a ledger's node source.
 *
 * Every trie operation gets its own `LedgerTrieStore` and its own root trie
 * object: the library edits trie objects in place, a non-membership proof
 * inserts and deletes while it builds, so nothing mutable is shared between
 * operations. Reads go to the operation's own writes first and then to the
 * ledger's node source; writes stay in the operation. The library's delete
 * hook and root marker are ignored: nodes are removed only by a sweep, and a
 * ledger is addressed by its root hash alone.
 *
 * The library's retained-write and hydration store flags are left unset, so
 * the library runs exactly its upstream algorithm.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk/core";

import { LedgerNodeMissing } from "./errors.js";
import type { NodeHash, StoredNode } from "./storage.js";

/** The root of the ledger with no entries. */
export const EMPTY_LEDGER_ROOT: string = EMPTY_MERKLE_TREE_ROOT;

/** The library's hash of an empty trie. */
const NULL_ROOT_HEX = "00".repeat(32);

export type NodeReader = (hash: NodeHash) => Promise<StoredNode | undefined>;

type Deserialise = (hash: Buffer, blob: unknown, store: Store) => Promise<Trie>;

/** The library's own node decoder (private in its typings). */
const deserialiseNode = (Trie as unknown as { deserialise: Deserialise })
  .deserialise;

type Serialisable = { serialise(): unknown };

/** A node as the library serialised it, with empty children as `null`. */
const storedNode = (serialised: unknown): StoredNode => {
  const node = serialised as StoredNode;
  if (node.__kind === "Leaf")
    return {
      __kind: "Leaf",
      prefix: node.prefix,
      key: node.key,
      value: node.value,
    };
  return {
    __kind: "Branch",
    prefix: node.prefix,
    children: node.children.map((child) => child ?? null),
    size: node.size,
  };
};

export class LedgerTrieStore extends Store {
  /** Nodes this operation wrote, by hash. */
  readonly writes = new Map<NodeHash, StoredNode>();

  constructor(private readonly read: NodeReader) {
    super(undefined);
  }

  override async ready(): Promise<void> {}

  override async batch<T>(callback: () => Promise<T>): Promise<T> {
    return await callback();
  }

  override async get(key: Buffer, deserialise: Deserialise): Promise<Trie> {
    const hash = Buffer.from(key).toString("hex");
    const node = this.writes.get(hash) ?? (await this.read(hash));
    if (node === undefined) throw new LedgerNodeMissing(hash);
    return await deserialise(key, node, this);
  }

  override async put(key: Buffer | string, value: Serialisable): Promise<void> {
    // The string key is the library's root marker.
    if (typeof key === "string") return;
    this.writes.set(key.toString("hex"), storedNode(value.serialise()));
  }

  override async del(): Promise<void> {}
}

/** A fresh, unshared trie object for `root`. */
export const loadTrie = async (
  store: LedgerTrieStore,
  root: string,
): Promise<Trie> => {
  if (root === EMPTY_LEDGER_ROOT) return new Trie(store);
  const trie = await store.get(Buffer.from(root, "hex"), deserialiseNode);
  trie.isRoot = true;
  return trie;
};

/** The ledger root of `trie`: the library's null hash is the empty root. */
export const rootOf = (trie: Trie): string => {
  const hash = trie.hash as Buffer | null;
  if (hash === null) return EMPTY_LEDGER_ROOT;
  const hex = Buffer.from(hash).toString("hex");
  return hex === NULL_ROOT_HEX ? EMPTY_LEDGER_ROOT : hex;
};

/** Builds the trie of `pairs` into `store`; returns its root. */
export const buildTrie = async (
  store: LedgerTrieStore,
  pairs: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Promise<string> =>
  pairs.length === 0
    ? EMPTY_LEDGER_ROOT
    : rootOf(await Trie.fromList([...pairs], store));
