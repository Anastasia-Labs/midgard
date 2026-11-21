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
  /** Outputs of leaves the block added, carried by the block. */
  readonly outputs: ReadonlyMap<OutRefHex, Buffer>;
  /**
   * Held nodes reachable from `root` that the running sweep has not marked.
   * Empty when no sweep runs.
   */
  readonly keptNodes: readonly NodeHash[];
  /**
   * Held outputs `root` relies on: that of every new leaf whose output the
   * block does not carry (a leaf the trie moved), sweep running or not, and
   * that of every leaf under a node in `keptNodes`.
   *
   * Every kept record must be held: otherwise `writeBlock` writes nothing and
   * fails with `LedgerNodeMissing` or `LedgerOutputMissing`. A kept record
   * survives a running sweep exactly as if rewritten now.
   */
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
 * Sweep protocol: `beginSweep` replaces the retained set, records that a
 * sweep is running, and starts a sweep epoch. Records written or kept after
 * it are never offered by `sweepNodes` / `sweepOutputs`, and `deleteSwept`
 * skips them even when offered before. `endSweep` removes the roots the
 * sweep found incomplete from the retained set, closes the epoch and clears
 * the record. A sweep deletes in key order, not parents before children, so
 * until it ends a held node may have lost a descendant: the store finishes an
 * unfinished sweep (`unfinishedSweep`) before it relies on held nodes again.
 */
export interface LedgerStorage {
  readNodes(hashes: readonly NodeHash[]): Promise<(StoredNode | undefined)[]>;
  readOutputs(outRefs: readonly OutRefHex[]): Promise<(Buffer | undefined)[]>;
  isRetained(root: NodeHash): Promise<boolean>;
  /** Writes one block atomically, or nothing when a kept record is not
   * held. */
  writeBlock(block: LedgerBlockWrite): Promise<void>;
  /**
   * Replaces the retained set with `retained` and records that a sweep is
   * running, in one atomic write, then starts a sweep epoch.
   */
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
  /**
   * Removes `dropped` from the retained set and clears the record that a
   * sweep is running, in one atomic write, then closes the sweep epoch.
   */
  endSweep(dropped: ReadonlySet<NodeHash>): Promise<void>;
  /**
   * The retained set, when a sweep began and never ended (its process died,
   * or its store failed mid-sweep); otherwise `undefined`.
   */
  unfinishedSweep(): Promise<ReadonlySet<NodeHash> | undefined>;
  /** Record counts, for tests and measurements. */
  counts(): Promise<LedgerStorageCounts>;
}
