/**
 * An immutable ledger value: a root plus where its nodes and outputs come
 * from. `apply` returns a new value and leaves this one valid; its new nodes
 * and outputs stay in memory (an overlay) until `LedgerStore.persist`.
 */
import { type LedgerEntry, ledgerEntryLeaf } from "../ledger-at.js";
import {
  EMPTY_LEDGER_ROOT,
  LedgerTrieStore,
  loadTrie,
  rootOf,
} from "./engine.js";
import {
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
   * not. Byte for byte what `keyValuePhasResolutionProofs` returns.
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
   * a present one. Cost is proportional to the mutated keys.
   */
  apply(mutations: readonly LedgerMutation[]): Promise<Ledger>;
  /** Every entry, in trie path order (not out-ref order). */
  entries(): AsyncIterable<{
    readonly outRef: Buffer;
    readonly output: Buffer;
  }>;
}

/** Nodes and outputs created in memory by `apply` or `admit`. */
export type Overlay = {
  readonly nodes: ReadonlyMap<NodeHash, StoredNode>;
  readonly outputs: ReadonlyMap<OutRefHex, Buffer>;
  readonly parent: Overlay | undefined;
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
      const { entry, leaf } = ledgerEntryLeaf(
        {
          outRef: key,
          output: mutation.output,
          descriptor: mutation.descriptor,
        },
        "ledger mutation",
      );
      await trie.insert(key, leaf);
      outputs.set(key.toString("hex"), Buffer.from(entry.output));
    }
    return new LedgerValue(
      rootOf(trie),
      this.owner,
      { nodes: store.writes, outputs, parent: this.overlay },
      this.source,
    );
  }

  async *entries(): AsyncIterable<{
    readonly outRef: Buffer;
    readonly output: Buffer;
  }> {
    if (this.root === EMPTY_LEDGER_ROOT) return;
    const pending: NodeHash[] = [this.root];
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
