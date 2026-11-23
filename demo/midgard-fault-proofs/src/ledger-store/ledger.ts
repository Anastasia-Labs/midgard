/**
 * An immutable ledger value: a root plus where its nodes and outputs come
 * from. `apply` returns a new value and leaves this one valid; its new nodes
 * and outputs stay in memory (an overlay) until `LedgerStore.persist`.
 */
import { type LedgerEntry, ledgerEntryLeaf } from "../ledger-at.js";
import {
  EMPTY_LEDGER_ROOT,
  keyPath,
  LedgerTrieStore,
  loadTrie,
  rootOf,
} from "./engine.js";
import {
  LedgerEntryNotCanonical,
  LedgerMutationRefused,
  LedgerNodeMissing,
  LedgerOutputMissing,
} from "./errors.js";
import type { NodeHash, OutRefHex, StoredNode } from "./storage.js";

/** One edit: delete a held out-ref, or insert an entry not held. */
export type LedgerMutation =
  | { readonly type: "delete"; readonly outRef: Buffer }
  | ({ readonly type: "insert" } & LedgerEntry);

/** One out-ref read from a ledger. */
export type LedgerRead = {
  /** The output CBOR; `undefined` when the ledger does not hold the out-ref. */
  readonly output?: Buffer;
  /**
   * The MPF proof CBOR against the ledger's root, as the on-chain verifier
   * reads it: membership when the out-ref is held, non-membership when it is
   * not.
   */
  proof(): Promise<Buffer>;
};

export interface Ledger {
  readonly root: string;
  /** Reads `outRefs`, keyed by out-ref hex. */
  read(outRefs: readonly Buffer[]): Promise<ReadonlyMap<OutRefHex, LedgerRead>>;
  /**
   * The ledger after `mutations`, in order. Refuses with
   * `LedgerMutationRefused` on a delete of an absent out-ref or an insert over
   * a present one, and with `LedgerEntryNotCanonical` on an insert of an
   * entry with no trie leaf. Cost is proportional to the mutated keys.
   */
  apply(mutations: readonly LedgerMutation[]): Promise<Ledger>;
  /**
   * Every entry, in trie path order (not out-ref order). With `after`, only
   * the entries whose trie path comes after the path of the out-ref `after`,
   * in the same order; `after` need not be held, and is never yielded. The
   * iteration descends to that path first, so the subtrees wholly before it
   * are not read.
   */
  entries(options?: { readonly after?: Buffer }): AsyncIterable<{
    readonly outRef: Buffer;
    readonly output: Buffer;
  }>;
}

/**
 * Nodes and outputs created in memory by `apply` or `admit`, one layer over
 * the layers of the ledgers before. A layer is never changed once built:
 * several ledgers may share it.
 */
export type Overlay = {
  readonly nodes: ReadonlyMap<NodeHash, StoredNode>;
  readonly outputs: ReadonlyMap<OutRefHex, Buffer>;
  readonly parent: Overlay | undefined;
  /** The layer's records: its nodes plus its outputs. */
  readonly size: number;
};

/**
 * The overlay of a ledger `apply` derives: `nodes` and `outputs` as a layer
 * over `parent`. A lookup reads one layer after another from the top, so a
 * block replayed one `apply` per step would otherwise read one layer per
 * earlier step, quadratic in the block's steps. Instead the new layer is
 * merged into a new copy of each layer below it that is not at least twice
 * its size (the binary-counter rule): layer sizes then at least double going
 * down, so a lookup reads at most log2 of the lineage's records plus one
 * layers, and each record is copied a logarithmic number of times. A merged
 * layer takes the upper layer's record where both hold a key, which is what
 * the lookup through both would find, so every lookup answers as before.
 * `parent` and every layer below it stay as they are. An `apply` that
 * created nothing adds no layer.
 */
const overlayOver = (
  parent: Overlay | undefined,
  nodes: ReadonlyMap<NodeHash, StoredNode>,
  outputs: ReadonlyMap<OutRefHex, Buffer>,
): Overlay | undefined => {
  if (nodes.size === 0 && outputs.size === 0) return parent;
  let layer: Overlay = {
    nodes,
    outputs,
    parent,
    size: nodes.size + outputs.size,
  };
  while (layer.parent !== undefined && layer.parent.size < 2 * layer.size) {
    const below: Overlay = layer.parent;
    const merged = {
      nodes: new Map([...below.nodes, ...layer.nodes]),
      outputs: new Map([...below.outputs, ...layer.outputs]),
    };
    layer = {
      ...merged,
      parent: below.parent,
      size: merged.nodes.size + merged.outputs.size,
    };
  }
  return layer;
};

/** Where a ledger's nodes and outputs come from below its overlay. */
export type LedgerSource = {
  readonly node: (hash: NodeHash) => Promise<StoredNode | undefined>;
  readonly outputs: (
    outRefs: readonly OutRefHex[],
  ) => Promise<(Buffer | undefined)[]>;
};

export const overlayNode = (
  overlay: Overlay | undefined,
  hash: NodeHash,
): StoredNode | undefined => {
  for (let layer = overlay; layer !== undefined; layer = layer.parent) {
    const node = layer.nodes.get(hash);
    if (node !== undefined) return node;
  }
  return undefined;
};

export const overlayOutput = (
  overlay: Overlay | undefined,
  outRef: OutRefHex,
): Buffer | undefined => {
  for (let layer = overlay; layer !== undefined; layer = layer.parent) {
    const output = layer.outputs.get(outRef);
    if (output !== undefined) return output;
  }
  return undefined;
};

/** `entry` and its trie leaf; refused with `LedgerEntryNotCanonical` when it
 * has none. */
export const storedEntryLeaf = (
  entry: LedgerEntry,
): { entry: LedgerEntry; leaf: Buffer } => {
  try {
    return ledgerEntryLeaf(entry, "ledger store");
  } catch (cause) {
    throw new LedgerEntryNotCanonical(Buffer.from(entry.outRef), cause);
  }
};

const ENTRY_BATCH = 256;

export class LedgerValue implements Ledger {
  constructor(
    readonly root: string,
    /** The store that may persist this ledger; `undefined` for none. */
    readonly owner: object | undefined,
    readonly overlay: Overlay | undefined,
    readonly source: LedgerSource,
  ) {}

  /** A node of this ledger's lineage: overlay first, then the source. */
  readonly readNode = async (hash: NodeHash): Promise<StoredNode | undefined> =>
    overlayNode(this.overlay, hash) ?? (await this.source.node(hash));

  /** Outputs of this ledger's lineage: overlay first, then the source. */
  readonly readOutputs = async (
    outRefs: readonly OutRefHex[],
  ): Promise<(Buffer | undefined)[]> => {
    const found = outRefs.map((outRef) => overlayOutput(this.overlay, outRef));
    const missing = outRefs.filter((_, index) => found[index] === undefined);
    if (missing.length === 0) return found;
    const fetched = await this.source.outputs(missing);
    let next = 0;
    return found.map((output) => output ?? fetched[next++]);
  };

  private readonly trie = async () => {
    const store = new LedgerTrieStore(this.readNode);
    return { store, trie: await loadTrie(store, this.root) };
  };

  async read(
    outRefs: readonly Buffer[],
  ): Promise<ReadonlyMap<OutRefHex, LedgerRead>> {
    const { trie } = await this.trie();
    const keys = outRefs.map((outRef) => Buffer.from(outRef));
    const held: OutRefHex[] = [];
    for (const key of keys)
      if ((await trie.get(key)) !== undefined) held.push(key.toString("hex"));
    const outputs = await this.readOutputs(held);
    const byOutRef = new Map<OutRefHex, Buffer>();
    held.forEach((outRef, index) => {
      const output = outputs[index];
      if (output === undefined) throw new LedgerOutputMissing(outRef);
      byOutRef.set(outRef, output);
    });
    const result = new Map<OutRefHex, LedgerRead>();
    for (const key of keys) {
      const outRef = key.toString("hex");
      const output = byOutRef.get(outRef);
      result.set(outRef, {
        ...(output === undefined ? {} : { output }),
        proof: async () => {
          const fresh = await this.trie();
          const proof = await fresh.trie.prove(key, output === undefined);
          return Buffer.from(proof.toCBOR());
        },
      });
    }
    return result;
  }

  async apply(mutations: readonly LedgerMutation[]): Promise<Ledger> {
    const { store, trie } = await this.trie();
    const outputs = new Map<OutRefHex, Buffer>();
    for (const [index, mutation] of mutations.entries()) {
      const key = Buffer.from(mutation.outRef);
      const present = (await trie.get(key)) !== undefined;
      if (mutation.type === "delete") {
        if (!present) throw new LedgerMutationRefused("absent", key, index);
        await trie.delete(key);
        continue;
      }
      if (present) throw new LedgerMutationRefused("present", key, index);
      const { entry, leaf } = storedEntryLeaf({
        outRef: key,
        output: mutation.output,
        descriptor: mutation.descriptor,
      });
      await trie.insert(key, leaf);
      outputs.set(key.toString("hex"), Buffer.from(entry.output));
    }
    return new LedgerValue(
      rootOf(trie),
      this.owner,
      overlayOver(this.overlay, store.writes, outputs),
      this.source,
    );
  }

  /**
   * The subtrees that hold exactly the entries whose path comes after
   * `path`, as a stack with the first of them on top. Descends from the root
   * along `path`: at a branch on it, the children after `path`'s next nibble
   * are kept and the child at that nibble is descended into. A node whose
   * prefix leaves `path` holds paths that are all before it or all after it,
   * so it is kept whole or dropped, and the descent ends there.
   */
  private async subtreesAfter(path: string): Promise<NodeHash[]> {
    const pending: NodeHash[] = [];
    let hash: NodeHash = this.root;
    let cursor = 0;
    for (;;) {
      const node = await this.readNode(hash);
      if (node === undefined) throw new LedgerNodeMissing(hash);
      const along = path.slice(cursor, cursor + node.prefix.length);
      if (node.prefix !== along) {
        if (node.prefix > along) pending.push(hash);
        return pending;
      }
      // A leaf whose prefix runs along `path` is the entry at `path`.
      if (node.__kind === "Leaf") return pending;
      cursor += node.prefix.length;
      const nibble = Number.parseInt(path[cursor]!, 16);
      for (let index = node.children.length - 1; index > nibble; index -= 1) {
        const child = node.children[index];
        if (child !== null && child !== undefined) pending.push(child);
      }
      const next = node.children[nibble];
      if (next === null || next === undefined) return pending;
      hash = next;
      cursor += 1;
    }
  }

  async *entries(options: { readonly after?: Buffer } = {}): AsyncIterable<{
    readonly outRef: Buffer;
    readonly output: Buffer;
  }> {
    if (this.root === EMPTY_LEDGER_ROOT) return;
    const pending: NodeHash[] =
      options.after === undefined
        ? [this.root]
        : await this.subtreesAfter(keyPath(options.after));
    let leaves: OutRefHex[] = [];
    const flush = async function* (
      ledger: LedgerValue,
      batch: readonly OutRefHex[],
    ) {
      const outputs = await ledger.readOutputs(batch);
      for (const [index, outRef] of batch.entries()) {
        const output = outputs[index];
        if (output === undefined) throw new LedgerOutputMissing(outRef);
        yield { outRef: Buffer.from(outRef, "hex"), output };
      }
    };
    while (pending.length > 0) {
      const hash = pending.pop()!;
      const node = await this.readNode(hash);
      if (node === undefined) throw new LedgerNodeMissing(hash);
      if (node.__kind === "Leaf") {
        leaves.push(node.key);
        if (leaves.length >= ENTRY_BATCH) {
          yield* flush(this, leaves);
          leaves = [];
        }
        continue;
      }
      for (let index = node.children.length - 1; index >= 0; index -= 1) {
        const child = node.children[index];
        if (child !== null && child !== undefined) pending.push(child);
      }
    }
    if (leaves.length > 0) yield* flush(this, leaves);
  }
}
