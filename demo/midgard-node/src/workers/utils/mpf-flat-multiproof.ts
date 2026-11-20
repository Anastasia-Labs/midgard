import { Trie } from "@aiken-lang/merkle-patricia-forestry";

const HASH_BYTES = 32;
const CHILDREN_PER_BRANCH = 16;
const METADATA_FIELDS = 10;

const enum FlatNodeKind {
  Empty = 0,
  Leaf = 1,
  Branch = 2,
}

type HydratedTrie = Trie & {
  readonly hash?: Buffer | null;
  readonly prefix: string;
  readonly size: number;
  readonly key?: Buffer;
  readonly value?: Buffer;
  readonly children?: readonly (
    | HydratedTrie
    | { readonly hash?: Buffer | null }
    | undefined
  )[];
  readonly assertHydratedNodeHashes?: (maxDepth: number) => {
    readonly verifiedNodes: number;
  };
};

export type AuthenticatedFlatMpfMultiproof = {
  readonly rootId: number;
  readonly rootHash: Buffer;
  readonly nodeCount: number;
  readonly leafCount: number;
  readonly branchCount: number;
  /**
   * Ten uint32 fields per node:
   * kind, prefix offset/length, subtree size, payload index,
   * key offset/length, value offset/length, reserved.
   */
  readonly metadata: Uint32Array;
  readonly nodeHashes: Uint8Array;
  readonly prefixNibbles: Uint8Array;
  readonly branchChildHashes: Uint8Array;
  /** Local hydrated child IDs, or -1 when the child remains a hash frontier. */
  readonly branchChildNodeIds: Int32Array;
  readonly leafKeys: Uint8Array;
  readonly leafValues: Uint8Array;
  readonly estimatedBytes: number;
};

const prefixNibbles = (prefix: string): readonly number[] =>
  [...prefix].map((digit) => {
    const nibble = Number.parseInt(digit, 16);
    if (!Number.isInteger(nibble) || nibble < 0 || nibble > 15) {
      throw new Error(`Invalid MPF prefix nibble ${digit}`);
    }
    return nibble;
  });

const copyHash = (
  target: Uint8Array,
  offset: number,
  hash: Buffer | null | undefined,
): void => {
  const normalized = hash ?? Buffer.alloc(HASH_BYTES);
  if (normalized.length !== HASH_BYTES) {
    throw new Error(`MPF hash must be ${HASH_BYTES.toString()} bytes`);
  }
  target.set(normalized, offset);
};

/**
 * Compile the already-hydrated touched-path closure into a compact flat
 * multiproof arena. Every hydrated object is fully content-authenticated before
 * any bytes enter the arena; hashed frontier children remain immutable hashes.
 * This is the worker-transfer layout for the custom event engine prototype.
 */
export const compileAuthenticatedFlatMpfMultiproof = (
  root: Trie,
): AuthenticatedFlatMpfMultiproof => {
  const nodes: HydratedTrie[] = [];
  const ids = new Map<object, number>();
  const pending: HydratedTrie[] = [root as HydratedTrie];
  while (pending.length > 0) {
    const node = pending.pop()!;
    if (ids.has(node)) continue;
    ids.set(node, nodes.length);
    nodes.push(node);
    for (const child of node.children ?? []) {
      if (child instanceof Trie) pending.push(child as HydratedTrie);
    }
  }

  let leafCount = 0;
  let branchCount = 0;
  let prefixBytes = 0;
  let keyBytes = 0;
  let valueBytes = 0;
  for (const node of nodes) {
    if (node.assertHydratedNodeHashes === undefined) {
      throw new Error("Flat MPF compiler requires authenticated trie nodes");
    }
    node.assertHydratedNodeHashes(0);
    prefixBytes += node.prefix.length;
    if (node.children !== undefined) branchCount += 1;
    else if (Buffer.isBuffer(node.key) && Buffer.isBuffer(node.value)) {
      leafCount += 1;
      keyBytes += node.key.length;
      valueBytes += node.value.length;
    }
  }

  const metadata = new Uint32Array(nodes.length * METADATA_FIELDS);
  const nodeHashes = new Uint8Array(nodes.length * HASH_BYTES);
  const prefixes = new Uint8Array(prefixBytes);
  const childHashes = new Uint8Array(
    branchCount * CHILDREN_PER_BRANCH * HASH_BYTES,
  );
  const childNodeIds = new Int32Array(branchCount * CHILDREN_PER_BRANCH);
  childNodeIds.fill(-1);
  const leafKeys = new Uint8Array(keyBytes);
  const leafValues = new Uint8Array(valueBytes);

  let prefixOffset = 0;
  let keyOffset = 0;
  let valueOffset = 0;
  let leafIndex = 0;
  let branchIndex = 0;
  for (const [nodeId, node] of nodes.entries()) {
    const metadataOffset = nodeId * METADATA_FIELDS;
    const encodedPrefix = prefixNibbles(node.prefix);
    prefixes.set(encodedPrefix, prefixOffset);
    metadata[metadataOffset + 1] = prefixOffset;
    metadata[metadataOffset + 2] = encodedPrefix.length;
    metadata[metadataOffset + 3] = node.size;
    copyHash(nodeHashes, nodeId * HASH_BYTES, node.hash);
    prefixOffset += encodedPrefix.length;

    if (node.children !== undefined) {
      if (node.children.length !== CHILDREN_PER_BRANCH) {
        throw new Error("Flat MPF branch must have exactly 16 children");
      }
      metadata[metadataOffset] = FlatNodeKind.Branch;
      metadata[metadataOffset + 4] = branchIndex;
      for (
        let childIndex = 0;
        childIndex < CHILDREN_PER_BRANCH;
        childIndex += 1
      ) {
        const child = node.children[childIndex];
        const flatChildIndex = branchIndex * CHILDREN_PER_BRANCH + childIndex;
        copyHash(childHashes, flatChildIndex * HASH_BYTES, child?.hash);
        if (child instanceof Trie) {
          childNodeIds[flatChildIndex] = ids.get(child as object)!;
        }
      }
      branchIndex += 1;
      continue;
    }

    if (Buffer.isBuffer(node.key) && Buffer.isBuffer(node.value)) {
      metadata[metadataOffset] = FlatNodeKind.Leaf;
      metadata[metadataOffset + 4] = leafIndex;
      metadata[metadataOffset + 5] = keyOffset;
      metadata[metadataOffset + 6] = node.key.length;
      metadata[metadataOffset + 7] = valueOffset;
      metadata[metadataOffset + 8] = node.value.length;
      leafKeys.set(node.key, keyOffset);
      leafValues.set(node.value, valueOffset);
      keyOffset += node.key.length;
      valueOffset += node.value.length;
      leafIndex += 1;
      continue;
    }

    metadata[metadataOffset] = FlatNodeKind.Empty;
  }

  const estimatedBytes =
    metadata.byteLength +
    nodeHashes.byteLength +
    prefixes.byteLength +
    childHashes.byteLength +
    childNodeIds.byteLength +
    leafKeys.byteLength +
    leafValues.byteLength;
  return {
    rootId: 0,
    rootHash: Buffer.from(nodeHashes.subarray(0, HASH_BYTES)),
    nodeCount: nodes.length,
    leafCount,
    branchCount,
    metadata,
    nodeHashes,
    prefixNibbles: prefixes,
    branchChildHashes: childHashes,
    branchChildNodeIds: childNodeIds,
    leafKeys,
    leafValues,
    estimatedBytes,
  };
};
