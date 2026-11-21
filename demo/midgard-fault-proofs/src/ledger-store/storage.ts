/**
 * The storage seam under `LedgerStore`: trie nodes keyed by hash, outputs
 * keyed by out-ref, and the set of retained roots. Every adapter (memory,
 * SQLite for the watcher, LevelDB for the node) implements this interface and
 * nothing else; the trie engine, persistence and the mark-and-sweep live
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

/** One `persist`: everything a block adds, written atomically. */
export type LedgerBlockWrite = {
  /** The persisted ledger's root; it joins the retained set. */
  readonly root: NodeHash;
  /** Nodes the store did not hold. */
  readonly nodes: ReadonlyMap<NodeHash, StoredNode>;
  /** Outputs of leaves the block added. */
  readonly outputs: ReadonlyMap<OutRefHex, Buffer>;
  /**
   * Records already held, reachable from `root`, that the running sweep has
   * not marked. They must survive that sweep, exactly as if rewritten now.
   * Empty when no sweep runs.
   */
  readonly keptNodes: readonly NodeHash[];
  readonly keptOutputs: readonly OutRefHex[];
};

export type LedgerStorageCounts = {
  readonly nodes: number;
  readonly outputs: number;
  readonly roots: number;
};

/**
 * One adapter's records. A single `LedgerStore` is the only writer of an
 * adapter while it is open, and it never runs two sweeps at once.
 *
 * Sweep protocol: `beginSweep` replaces the retained set and starts a sweep
 * epoch. Records written or kept after it are never offered by `sweepNodes`
 * / `sweepOutputs`, and `deleteSwept` skips them even when offered before.
 * `endSweep` closes the epoch.
 */
export interface LedgerStorage {
  readNodes(hashes: readonly NodeHash[]): Promise<(StoredNode | undefined)[]>;
  readOutputs(outRefs: readonly OutRefHex[]): Promise<(Buffer | undefined)[]>;
  isRetained(root: NodeHash): Promise<boolean>;
  /** Writes one block atomically. */
  writeBlock(block: LedgerBlockWrite): Promise<void>;
  /** Replaces the retained set with `retained` and starts a sweep epoch. */
  beginSweep(retained: ReadonlySet<NodeHash>): Promise<void>;
  /** Up to `limit` node hashes stored before `beginSweep`, ascending, after
   * `after`. */
  sweepNodes(after: NodeHash | undefined, limit: number): Promise<NodeHash[]>;
  /** As `sweepNodes`, for outputs. */
  sweepOutputs(
    after: OutRefHex | undefined,
    limit: number,
  ): Promise<OutRefHex[]>;
  /** Deletes the given records, except those written or kept since
   * `beginSweep`. */
  deleteSwept(records: {
    readonly nodes: readonly NodeHash[];
    readonly outputs: readonly OutRefHex[];
  }): Promise<void>;
  endSweep(): Promise<void>;
  /** Record counts, for tests and measurements. */
  counts(): Promise<LedgerStorageCounts>;
}
