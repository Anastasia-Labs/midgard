/**
 * Ledger witnesses: the trie nodes and outputs one replay read, recorded so a
 * verifier can repeat that replay without the store.
 *
 * A witness is content-addressed: nodes carry no hash, the verifier computes
 * each one. A branch carries no `size` either. The trie library keeps `size`
 * on every node but never hashes it (`Leaf.computeHash`, `Branch.computeHash`
 * in its `lib/trie.js`); its behaviour only ever compares `size` with 0
 * (`isEmpty`, used by `fromList`, by `prove` on an empty trie and by
 * `Branch.from`'s child check, and `childAt`'s early return), and its edits
 * only carry it forward (`tryInsert` adds 1, `tryDelete` subtracts 1 and takes
 * a collapsed neighbour's). So a branch rebuilt from a witness takes a large
 * positive constant as its size, and every root and proof it yields is the
 * store's.
 */
import { midgardBlake2b } from "@al-ft/midgard-core/codec/hash";

import { ledgerEntryLeaf } from "../ledger-at.js";
import { EMPTY_LEDGER_ROOT, keyPath } from "./engine.js";
import { LedgerWitnessRefused } from "./errors.js";
import { type Ledger, LedgerValue } from "./ledger.js";
import type {
  NodeHash,
  OutRefHex,
  StoredBranch,
  StoredLeaf,
  StoredNode,
} from "./storage.js";

/** A witnessed branch: the stored shape without `size`. */
export type WitnessBranch = Omit<StoredBranch, "size">;
export type WitnessNode = StoredLeaf | WitnessBranch;

export type LedgerWitness = {
  readonly nodes: readonly WitnessNode[];
  readonly outputs: readonly {
    readonly outRef: Buffer;
    readonly output: Buffer;
  }[];
};

/** The size every branch rebuilt from a witness takes (see the module
 * comment): positive, and far above any reachable ledger size. */
const WITNESS_BRANCH_SIZE = 2 ** 40;

const HASH_BYTES = 32;
const PATH_NIBBLES = HASH_BYTES * 2;
const NIBBLES = /^[0-9a-f]*$/;
const HASH = /^[0-9a-f]{64}$/;
const BYTES = /^(?:[0-9a-f]{2})+$/;
/** What `nodeHash` hashes an empty child slot as. */
const EMPTY_SLOT_HASH = "00".repeat(HASH_BYTES);

const digest = (...parts: readonly Uint8Array[]): Buffer =>
  Buffer.from(midgardBlake2b(Buffer.concat(parts), { dkLen: HASH_BYTES }));

const nibbleBytes = (nibbles: string): Buffer =>
  Buffer.from([...nibbles].map((nibble) => Number.parseInt(nibble, 16)));

/** The node's hash, as the trie library computes it. */
const nodeHash = (node: WitnessNode): NodeHash => {
  if (node.__kind === "Leaf") {
    const odd = node.prefix.length % 2 === 1;
    const head = odd
      ? Buffer.from([0x10, Number.parseInt(node.prefix[0]!, 16)])
      : Buffer.from([0xff]);
    const tail = Buffer.from(odd ? node.prefix.slice(1) : node.prefix, "hex");
    return digest(head, tail, digest(Buffer.from(node.value, "hex"))).toString(
      "hex",
    );
  }
  let layer = node.children.map((child) =>
    child === null ? Buffer.alloc(HASH_BYTES) : Buffer.from(child, "hex"),
  );
  while (layer.length > 1)
    layer = Array.from({ length: layer.length / 2 }, (_, index) =>
      digest(layer[index * 2]!, layer[index * 2 + 1]!),
    );
  return digest(nibbleBytes(node.prefix), layer[0]!).toString("hex");
};

/** Refuses a node that is not a well-formed leaf or branch. */
const checkShape = (node: WitnessNode, index: number): void => {
  const refuse = (what: string) => {
    throw new LedgerWitnessRefused(`node ${index}: ${what}`);
  };
  if (typeof node.prefix !== "string" || !NIBBLES.test(node.prefix))
    refuse("prefix is not a nibble string");
  if (node.__kind === "Leaf") {
    if (typeof node.key !== "string" || !BYTES.test(node.key))
      refuse("leaf key is not hex bytes");
    if (typeof node.value !== "string" || !BYTES.test(node.value))
      refuse("leaf value is not hex bytes");
    return;
  }
  if (node.__kind !== "Branch") refuse("neither a leaf nor a branch");
  if (!Array.isArray(node.children) || node.children.length !== 16)
    refuse("a branch has 16 child slots");
  let present = 0;
  for (const child of node.children) {
    if (child === null) continue;
    if (typeof child !== "string" || !HASH.test(child))
      refuse("child is not a node hash");
    // The trie hashes an empty slot as this value, so a child equal to it
    // would add a slot the hash does not see.
    if (child === EMPTY_SLOT_HASH) refuse("child is the empty slot's hash");
    present += 1;
  }
  if (present < 2) refuse("a branch has at least two children");
};

/**
 * Wraps `ledger` so that everything read through the returned ledger, and
 * through every ledger `apply` derives from it, is recorded. `witness()`
 * returns what has been read so far. The recording ledger cannot be
 * persisted.
 */
export const recordWitness = (
  ledger: Ledger,
): { readonly ledger: Ledger; witness(): LedgerWitness } => {
  if (!(ledger instanceof LedgerValue))
    throw new Error("recordWitness: not a ledger store ledger");
  const nodes = new Map<NodeHash, StoredNode>();
  const outputs = new Map<OutRefHex, Buffer>();
  const recording = new LedgerValue(ledger.root, undefined, undefined, {
    node: async (hash) => {
      const node = await ledger.readNode(hash);
      if (node !== undefined) nodes.set(hash, node);
      return node;
    },
    outputs: async (outRefs) => {
      const found = await ledger.readOutputs(outRefs);
      outRefs.forEach((outRef, index) => {
        const output = found[index];
        if (output !== undefined) outputs.set(outRef, output);
      });
      return found;
    },
  });
  return {
    ledger: recording,
    witness: () => ({
      nodes: [...nodes]
        .sort(([left], [right]) => (left < right ? -1 : 1))
        .map(([, node]) =>
          node.__kind === "Leaf"
            ? node
            : {
                __kind: "Branch",
                prefix: node.prefix,
                children: node.children,
              },
        ),
      outputs: [...outputs]
        .sort(([left], [right]) => (left < right ? -1 : 1))
        .map(([outRef, output]) => ({
          outRef: Buffer.from(outRef, "hex"),
          output: Buffer.from(output),
        })),
    }),
  };
};

/**
 * The ledger with `root` as far as `witness` opens it. Refuses
 * (`LedgerWitnessRefused`) a witness with a malformed node (a branch child
 * equal to the empty slot's hash included), a node not reachable from `root` through witnessed nodes, a node reached at two
 * different paths, a leaf not at its key's path, or an output that is not
 * the value of a witnessed leaf. Reading a node the witness does not hold
 * fails with `LedgerNodeMissing`. The ledger cannot be persisted.
 */
export const ledgerFromWitness = (
  root: string,
  witness: LedgerWitness,
): Ledger => {
  const byHash = new Map<NodeHash, StoredNode>();
  witness.nodes.forEach((node, index) => {
    checkShape(node, index);
    const hash = nodeHash(node);
    if (byHash.has(hash))
      throw new LedgerWitnessRefused(`node ${hash} is witnessed twice`);
    byHash.set(
      hash,
      node.__kind === "Leaf"
        ? {
            __kind: "Leaf",
            prefix: node.prefix,
            key: node.key,
            value: node.value,
          }
        : {
            __kind: "Branch",
            prefix: node.prefix,
            children: [...node.children],
            size: WITNESS_BRANCH_SIZE,
          },
    );
  });

  const paths = new Map<NodeHash, string>();
  const leafValues = new Map<OutRefHex, string>();
  const pending: [NodeHash, string][] =
    root === EMPTY_LEDGER_ROOT ? [] : [[root, ""]];
  while (pending.length > 0) {
    const [hash, path] = pending.pop()!;
    const node = byHash.get(hash);
    if (node === undefined) continue;
    const seen = paths.get(hash);
    if (seen !== undefined) {
      if (seen !== path)
        throw new LedgerWitnessRefused(`node ${hash} is reached at two paths`);
      continue;
    }
    paths.set(hash, path);
    const full = path + node.prefix;
    if (node.__kind === "Leaf") {
      if (full !== keyPath(Buffer.from(node.key, "hex")))
        throw new LedgerWitnessRefused(`leaf ${hash} is not at its key's path`);
      leafValues.set(node.key, node.value);
      continue;
    }
    if (full.length >= PATH_NIBBLES)
      throw new LedgerWitnessRefused(`branch ${hash} is below the key length`);
    node.children.forEach((child, nibble) => {
      if (child !== null) pending.push([child, full + nibble.toString(16)]);
    });
  }
  if (paths.size !== byHash.size)
    throw new LedgerWitnessRefused("a node is not reachable from the root");

  const outputs = new Map<OutRefHex, Buffer>();
  for (const { outRef, output } of witness.outputs) {
    const key = Buffer.from(outRef).toString("hex");
    const value = leafValues.get(key);
    if (value === undefined)
      throw new LedgerWitnessRefused(`output ${key} has no witnessed leaf`);
    if (outputs.has(key))
      throw new LedgerWitnessRefused(`output ${key} is witnessed twice`);
    let leaf: Buffer;
    try {
      leaf = ledgerEntryLeaf({ outRef, output }, "ledger witness").leaf;
    } catch {
      throw new LedgerWitnessRefused(`output ${key} is not a ledger output`);
    }
    if (leaf.toString("hex") !== value)
      throw new LedgerWitnessRefused(`output ${key} is not its leaf's value`);
    outputs.set(key, Buffer.from(output));
  }

  return new LedgerValue(root, undefined, undefined, {
    node: async (hash) => byHash.get(hash),
    outputs: async (outRefs) => outRefs.map((outRef) => outputs.get(outRef)),
  });
};
