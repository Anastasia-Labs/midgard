/**
 * The storage seam under `LedgerStore`: trie nodes keyed by hash, outputs
 * keyed by out-ref, each with a reference count, the set of retained roots,
 * and a queue of nodes waiting for deletion. Every adapter (memory, SQLite
 * for the watcher, LevelDB for the node) implements this interface and
 * nothing else; the trie engine, persistence and the reference counting live
 * above it, in one place.
 *
 * Nodes are stored in the vendored trie library's own serialised shape, the
 * shape the node's LevelDB already holds.
 */

/** A trie node hash, 64 lowercase hex digits. */
export type NodeHash = string;

/** A ledger out-ref key (the §5.3 out-ref bytes), lowercase hex. */
export type OutRefHex = string;

/** A leaf as the trie library serialises it: `key` and `value` are hex. */
export type StoredLeaf = {
  readonly __kind: "Leaf";
  readonly prefix: string;
  readonly key: string;
  readonly value: string;
};

/** A branch as the trie library serialises it: 16 child hashes or `null`. */
export type StoredBranch = {
  readonly __kind: "Branch";
  readonly prefix: string;
  readonly children: readonly (NodeHash | null)[];
  readonly size: number;
};

export type StoredNode = StoredLeaf | StoredBranch;

/**
 * One atomic write. The store computes every count itself and hands the
 * adapter absolute values. The adapter applies the whole change or nothing.
 */
export type LedgerStorageChange = {
  /** Nodes not held before, with their reference counts. */
  readonly putNodes: ReadonlyMap<
    NodeHash,
    { readonly node: StoredNode; readonly refs: number }
  >;
  /** New reference counts of held nodes. */
  readonly nodeRefs: ReadonlyMap<NodeHash, number>;
  /** Held nodes to delete; their counts go with them. */
  readonly deleteNodes: readonly NodeHash[];
  /** Outputs not held before, with their reference counts. */
  readonly putOutputs: ReadonlyMap<
    OutRefHex,
    { readonly output: Buffer; readonly refs: number }
  >;
  /** New reference counts of held outputs. */
  readonly outputRefs: ReadonlyMap<OutRefHex, number>;
  /** Held outputs to delete; their counts go with them. */
  readonly deleteOutputs: readonly OutRefHex[];
  readonly addRoots: readonly NodeHash[];
  readonly removeRoots: readonly NodeHash[];
  /** Hashes that join the dead queue. */
  readonly queueDead: readonly NodeHash[];
  /** Hashes that leave the dead queue; disjoint from `queueDead`. */
  readonly unqueueDead: readonly NodeHash[];
};

export type LedgerStorageCounts = {
  readonly nodes: number;
  readonly outputs: number;
  readonly roots: number;
  /** The sum of the held nodes' reference counts. */
  readonly nodeRefs: number;
  /** The sum of the held outputs' reference counts. */
  readonly outputRefs: number;
  /** Hashes in the dead queue. */
  readonly dead: number;
};

/**
 * One adapter's records. A single `LedgerStore` is the only writer of an
 * adapter while it is open.
 *
 * A held node's reference count is the number of held branch nodes that list
 * it as a child, plus one when it is a retained root; a held output's is the
 * number of held leaf nodes with its out-ref key. A node whose count reaches
 * zero joins the dead queue and stays held until the store deletes it.
 */
export interface LedgerStorage {
  readNodes(hashes: readonly NodeHash[]): Promise<(StoredNode | undefined)[]>;
  readOutputs(outRefs: readonly OutRefHex[]): Promise<(Buffer | undefined)[]>;
  /** Reference counts; `undefined` when the node is not held. */
  nodeRefs(hashes: readonly NodeHash[]): Promise<(number | undefined)[]>;
  /** Reference counts; `undefined` when the output is not held. */
  outputRefs(outRefs: readonly OutRefHex[]): Promise<(number | undefined)[]>;
  /** Every retained root. The store reads them once and then keeps its own
   * copy current. */
  retainedRoots(): Promise<NodeHash[]>;
  /** Up to `limit` hashes in the dead queue. */
  deadNodes(limit: number): Promise<NodeHash[]>;
  /** Applies `change` in one atomic write. */
  write(change: LedgerStorageChange): Promise<void>;
  /** Record counts, for tests and measurements. */
  counts(): Promise<LedgerStorageCounts>;
}
