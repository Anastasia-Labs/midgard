import { createRequire } from "node:module";
import { Worker } from "node:worker_threads";

import { Trie } from "@aiken-lang/merkle-patricia-forestry";

import {
  createEventFlatDigest,
  eventFlatDigest as digest,
  prepareEventFlatDigest,
} from "./mpf-event-flat-digest.js";

const HASH_BYTES = 32;
const CHILDREN_PER_BRANCH = 16;
const BRANCH_MERKLE_INTERNALS = 15;
const METADATA_FIELDS = 10;
const EMPTY_ROOT = Buffer.from(
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
  "hex",
);
const ZERO_HASH = Buffer.alloc(HASH_BYTES);

const enum PackedNodeKind {
  Leaf = 1,
  Branch = 2,
}

export type PackedMpfLeaf = {
  readonly __kind: "Leaf";
  readonly prefix: string;
  readonly key: string;
  readonly value: string;
};

export type PackedMpfBranch = {
  readonly __kind: "Branch";
  readonly prefix: string;
  readonly children: readonly (string | null | undefined)[];
  readonly size: number;
};

export type PackedMpfStoredValue = PackedMpfLeaf | PackedMpfBranch;

export type PackedMpfProofNode = {
  readonly hash: Buffer;
  readonly value: PackedMpfStoredValue;
};

export type AuthenticatedPackedMpfRecord = PackedMpfProofNode & {
  readonly branchMerkleNodes?: readonly Buffer[];
};

export type ParkedEventFlatShard = {
  readonly nodeCount: number;
  readonly nodeHashes: ArrayBuffer;
  readonly nodeValues: ArrayBuffer;
  readonly nodeValueOffsets: ArrayBuffer;
  readonly digest: ArrayBuffer;
  readonly encodedBytes: number;
};

export type ParkedEventFlatOverlay = {
  readonly schemaVersion: 1;
  readonly trieName: string;
  readonly baseRoot: ArrayBuffer;
  readonly candidateRoot: ArrayBuffer;
  readonly closureDigest: ArrayBuffer;
  readonly nodeCount: number;
  readonly shards: readonly ParkedEventFlatShard[];
  readonly encodedBytes: number;
};

export type EventFlatMutationDiagnostics = {
  readonly nodeCount: number;
  readonly baseNodeCount: number;
  readonly reachableNodeCount: number;
  readonly reachableDirtyNodeCount: number;
  readonly estimatedBytes: number;
  readonly authenticatedBranches: number;
  readonly generatedFullBranches: number;
  readonly generatedFullBranchDigests: number;
  readonly incrementalBranchUpdates: number;
  readonly incrementalBranchDigests: number;
  readonly copiedBranchCacheBytes: number;
  readonly branchCacheBytes: number;
};

const exactArrayBuffer = (value: Uint8Array): ArrayBuffer =>
  value.buffer.slice(
    value.byteOffset,
    value.byteOffset + value.byteLength,
  ) as ArrayBuffer;

const assertHash = (hash: Buffer, field: string): void => {
  if (hash.length !== HASH_BYTES) {
    throw new Error(`${field} must be a 32-byte hash`);
  }
};

function assertPrefix(prefix: unknown): asserts prefix is string {
  if (typeof prefix !== "string" || !/^[0-9a-f]*$/.test(prefix)) {
    throw new Error("Packed MPF prefix must contain lowercase hex nibbles");
  }
}

const prefixBytes = (prefix: string): Buffer =>
  Buffer.from([...prefix].map((digit) => Number.parseInt(digit, 16)));

export const merkleRoot = (children: readonly Buffer[]): Buffer => {
  if (children.length !== CHILDREN_PER_BRANCH) {
    throw new Error("Packed MPF branch must have 16 children");
  }
  let level = children.map((child) => Buffer.from(child));
  while (level.length > 1) {
    const next: Buffer[] = [];
    for (let index = 0; index < level.length; index += 2) {
      next.push(digest(Buffer.concat([level[index]!, level[index + 1]!])));
    }
    level = next;
  }
  return level[0]!;
};

const leafHash = (prefix: string, value: Buffer): Buffer => {
  const isOdd = prefix.length % 2 === 1;
  const head = isOdd
    ? Buffer.from([0, Number.parseInt(prefix[0]!, 16)])
    : Buffer.from([255]);
  const tail = Buffer.from(isOdd ? prefix.slice(1) : prefix, "hex");
  return digest(Buffer.concat([head, tail, digest(value)]));
};

const computeBranchMerkleNodes = (
  children: readonly (Buffer | undefined)[],
): readonly Buffer[] => {
  if (children.length !== CHILDREN_PER_BRANCH) {
    throw new Error("Packed MPF branch must have 16 children");
  }
  const nodes = Array<Buffer>(CHILDREN_PER_BRANCH * 2);
  for (let index = 0; index < CHILDREN_PER_BRANCH; index += 1) {
    nodes[CHILDREN_PER_BRANCH + index] = children[index] ?? ZERO_HASH;
  }
  for (let index = BRANCH_MERKLE_INTERNALS; index >= 1; index -= 1) {
    nodes[index] = digest(
      Buffer.concat([nodes[index * 2]!, nodes[index * 2 + 1]!]),
    );
  }
  return nodes;
};

export const authenticatePackedMpfRecord = (
  expectedHash: Buffer,
  value: PackedMpfStoredValue,
): readonly Buffer[] | undefined => {
  assertHash(expectedHash, "Packed MPF record key");
  if (value.__kind === "Leaf") {
    assertPrefix(value.prefix);
    if (!/^(?:[0-9a-f]{2})*$/.test(value.key)) {
      throw new Error("Packed MPF leaf key is not canonical hex");
    }
    if (!/^(?:[0-9a-f]{2})*$/.test(value.value)) {
      throw new Error("Packed MPF leaf value is not canonical hex");
    }
    const key = Buffer.from(value.key, "hex");
    const path = digest(key).toString("hex");
    if (!path.endsWith(value.prefix)) {
      throw new Error("Packed MPF leaf prefix does not extend its key path");
    }
    const actual = leafHash(value.prefix, Buffer.from(value.value, "hex"));
    if (!actual.equals(expectedHash)) {
      throw new Error(
        `Packed MPF leaf hash mismatch: expected=${expectedHash.toString("hex")},actual=${actual.toString("hex")}`,
      );
    }
    return undefined;
  }
  if (value.__kind !== "Branch") {
    throw new Error("Packed MPF record has an unknown kind");
  }
  assertPrefix(value.prefix);
  if (
    !Array.isArray(value.children) ||
    value.children.length !== CHILDREN_PER_BRANCH ||
    value.children.filter((child) => child != null).length < 2 ||
    !Number.isSafeInteger(value.size) ||
    value.size < 2
  ) {
    throw new Error("Packed MPF branch has an invalid shape");
  }
  for (const child of value.children) {
    if (child != null && !/^[0-9a-f]{64}$/.test(child)) {
      throw new Error("Packed MPF branch child is not a canonical hash");
    }
  }
  const merkleNodes = computeBranchMerkleNodes(
    value.children.map((child) =>
      child == null ? undefined : Buffer.from(child, "hex"),
    ),
  );
  const actual = digest(
    Buffer.concat([prefixBytes(value.prefix), merkleNodes[1]!]),
  );
  if (!actual.equals(expectedHash)) {
    throw new Error(
      `Packed MPF branch hash mismatch: expected=${expectedHash.toString("hex")},actual=${actual.toString("hex")}`,
    );
  }
  return merkleNodes;
};

const eventFlatShardDigest = ({
  shardIndex,
  nodeCount,
  nodeHashes,
  nodeValues,
  nodeValueOffsets,
}: {
  readonly shardIndex: number;
  readonly nodeCount: number;
  readonly nodeHashes: ArrayBuffer;
  readonly nodeValues: ArrayBuffer;
  readonly nodeValueOffsets: ArrayBuffer;
}): Buffer => {
  const header = Buffer.alloc(8);
  header.writeUInt32BE(shardIndex, 0);
  header.writeUInt32BE(nodeCount, 4);
  const hash = createEventFlatDigest();
  hash.update(header);
  hash.update(new Uint8Array(nodeHashes));
  hash.update(new Uint8Array(nodeValueOffsets));
  hash.update(new Uint8Array(nodeValues));
  return hash.digest();
};

const eventFlatClosureDigest = ({
  trieName,
  baseRoot,
  candidateRoot,
  shards,
}: {
  readonly trieName: string;
  readonly baseRoot: ArrayBuffer;
  readonly candidateRoot: ArrayBuffer;
  readonly shards: readonly ParkedEventFlatShard[];
}): Buffer => {
  const trieNameBytes = Buffer.from(trieName);
  const header = Buffer.alloc(12);
  header.writeUInt32BE(2, 0);
  header.writeUInt32BE(trieNameBytes.length, 4);
  header.writeUInt32BE(shards.length, 8);
  const hash = createEventFlatDigest();
  hash.update(header);
  hash.update(trieNameBytes);
  hash.update(new Uint8Array(baseRoot));
  hash.update(new Uint8Array(candidateRoot));
  for (const shard of shards) {
    const shardHeader = Buffer.alloc(12);
    shardHeader.writeUInt32BE(shard.nodeCount, 0);
    shardHeader.writeUInt32BE(shard.encodedBytes, 4);
    shardHeader.writeUInt32BE(shard.nodeValues.byteLength, 8);
    hash.update(shardHeader);
    hash.update(new Uint8Array(shard.digest));
  }
  return hash.digest();
};

const packEventFlatShard = (
  records: readonly (readonly [string, PackedMpfStoredValue])[],
): Omit<ParkedEventFlatShard, "digest" | "encodedBytes"> => {
  const nodeHashes = new Uint8Array(records.length * HASH_BYTES);
  const encodedValues = records.map(([, value]) =>
    Buffer.from(JSON.stringify(value)),
  );
  const nodeValueOffsets = new Uint32Array(records.length * 2);
  const nodeValues = new Uint8Array(
    encodedValues.reduce((total, value) => total + value.length, 0),
  );
  let valueOffset = 0;
  for (const [index, [hashHex]] of records.entries()) {
    const hash = Buffer.from(hashHex, "hex");
    assertHash(hash, "Event-flat shard record");
    nodeHashes.set(hash, index * HASH_BYTES);
    const encoded = encodedValues[index]!;
    nodeValues.set(encoded, valueOffset);
    nodeValueOffsets.set([valueOffset, encoded.length], index * 2);
    valueOffset += encoded.length;
  }
  return {
    nodeCount: records.length,
    nodeHashes: exactArrayBuffer(nodeHashes),
    nodeValues: exactArrayBuffer(nodeValues),
    nodeValueOffsets: exactArrayBuffer(new Uint8Array(nodeValueOffsets.buffer)),
  };
};

type EventFlatWorkerShard = ParkedEventFlatShard & {
  readonly branchMerkleNodes?: ArrayBuffer;
  readonly branchMerkleDigest?: ArrayBuffer;
};

const runEventFlatFreezeWorker = (
  shardIndex: number,
  packed: Omit<ParkedEventFlatShard, "digest" | "encodedBytes">,
  includeMerkleNodes = false,
): Promise<EventFlatWorkerShard> => {
  const require = createRequire(import.meta.url);
  const blake2bPath = require.resolve("blake2b");
  const source = `
const { parentPort, workerData } = require("node:worker_threads");
const blake2b = require(workerData.blake2bPath);
const HASH_BYTES = ${HASH_BYTES.toString()};
const CHILDREN_PER_BRANCH = ${CHILDREN_PER_BRANCH.toString()};
const digest = (value) => Buffer.from(blake2b(HASH_BYTES).update(value).digest());
const assertHash = (hash, field) => {
  if (!Buffer.isBuffer(hash) || hash.length !== HASH_BYTES) {
    throw new Error(field + " must be a 32-byte hash");
  }
};
const assertPrefix = (prefix) => {
  if (typeof prefix !== "string" || !/^[0-9a-f]*$/.test(prefix)) {
    throw new Error("Packed MPF prefix must contain lowercase hex nibbles");
  }
};
const prefixBytes = (prefix) =>
  Buffer.from(Array.from(prefix, (digit) => Number.parseInt(digit, 16)));
const computeBranchMerkleNodes = (children) => {
  const nodes = Array(CHILDREN_PER_BRANCH * 2);
  for (let index = 0; index < CHILDREN_PER_BRANCH; index += 1) {
    nodes[CHILDREN_PER_BRANCH + index] = children[index];
  }
  for (let index = CHILDREN_PER_BRANCH - 1; index >= 1; index -= 1) {
    nodes[index] = digest(Buffer.concat([nodes[index * 2], nodes[index * 2 + 1]]));
  }
  return nodes;
};
const leafHash = (prefix, value) => {
  const isOdd = prefix.length % 2 === 1;
  const head = isOdd
    ? Buffer.from([0, Number.parseInt(prefix[0], 16)])
    : Buffer.from([255]);
  const tail = Buffer.from(isOdd ? prefix.slice(1) : prefix, "hex");
  return digest(Buffer.concat([head, tail, digest(value)]));
};
const branchHash = (prefix, children) => {
  const childHashes = Array.from({ length: CHILDREN_PER_BRANCH }, (_, index) =>
    children[index] == null
      ? Buffer.alloc(HASH_BYTES)
      : Buffer.from(children[index], "hex")
  );
  for (const child of childHashes) assertHash(child, "Packed MPF child");
  const nodes = computeBranchMerkleNodes(childHashes);
  return {
    hash: digest(Buffer.concat([prefixBytes(prefix), nodes[1]])),
    nodes,
  };
};
const authenticatePackedMpfRecord = (expectedHash, value) => {
  assertHash(expectedHash, "Packed MPF record key");
  if (value.__kind === "Leaf") {
    assertPrefix(value.prefix);
    if (!/^(?:[0-9a-f]{2})*$/.test(value.key)) {
      throw new Error("Packed MPF leaf key is not canonical hex");
    }
    if (!/^(?:[0-9a-f]{2})*$/.test(value.value)) {
      throw new Error("Packed MPF leaf value is not canonical hex");
    }
    const key = Buffer.from(value.key, "hex");
    const path = digest(key).toString("hex");
    if (!path.endsWith(value.prefix)) {
      throw new Error("Packed MPF leaf prefix does not extend its key path");
    }
    const actual = leafHash(value.prefix, Buffer.from(value.value, "hex"));
    if (!actual.equals(expectedHash)) {
      throw new Error("Packed MPF leaf hash mismatch");
    }
    return;
  }
  if (value.__kind !== "Branch") {
    throw new Error("Packed MPF record has an unknown kind");
  }
  assertPrefix(value.prefix);
  if (
    !Array.isArray(value.children) ||
    value.children.length !== CHILDREN_PER_BRANCH ||
    value.children.filter((child) => child != null).length < 2 ||
    !Number.isSafeInteger(value.size) ||
    value.size < 2
  ) {
    throw new Error("Packed MPF branch has an invalid shape");
  }
  for (const child of value.children) {
    if (child != null && !/^[0-9a-f]{64}$/.test(child)) {
      throw new Error("Packed MPF branch child is not a canonical hash");
    }
  }
  const actual = branchHash(value.prefix, value.children);
  if (!actual.hash.equals(expectedHash)) {
    throw new Error("Packed MPF branch hash mismatch");
  }
  return actual.nodes;
};
const eventFlatShardDigest = ({ shardIndex, nodeCount, nodeHashes, nodeValues, nodeValueOffsets }) => {
  const header = Buffer.alloc(8);
  header.writeUInt32BE(shardIndex, 0);
  header.writeUInt32BE(nodeCount, 4);
  const hash = blake2b(HASH_BYTES);
  hash.update(header);
  hash.update(new Uint8Array(nodeHashes));
  hash.update(new Uint8Array(nodeValueOffsets));
  hash.update(new Uint8Array(nodeValues));
  return Buffer.from(hash.digest());
};
const handleRequest = (request) => {
  try {
    const hashes = new Uint8Array(request.nodeHashes);
    const values = new Uint8Array(request.nodeValues);
    const offsets = new Uint32Array(request.nodeValueOffsets);
    const branchMerkleNodes = request.includeMerkleNodes
      ? new Uint8Array(request.nodeCount * ${BRANCH_MERKLE_INTERNALS.toString()} * HASH_BYTES)
      : undefined;
    for (let index = 0; index < request.nodeCount; index += 1) {
      const hash = Buffer.from(hashes.subarray(index * HASH_BYTES, (index + 1) * HASH_BYTES));
      const offset = offsets[index * 2];
      const length = offsets[index * 2 + 1];
      if (offset + length > values.length) throw new Error("Event-flat shard value exceeds its arena");
      const value = JSON.parse(Buffer.from(values.subarray(offset, offset + length)).toString());
      const authenticatedMerkleNodes = authenticatePackedMpfRecord(hash, value);
      if (branchMerkleNodes !== undefined && authenticatedMerkleNodes !== undefined) {
        for (let merkleIndex = 1; merkleIndex < CHILDREN_PER_BRANCH; merkleIndex += 1) {
          branchMerkleNodes.set(
            authenticatedMerkleNodes[merkleIndex],
            (index * ${BRANCH_MERKLE_INTERNALS.toString()} + merkleIndex - 1) * HASH_BYTES
          );
        }
      }
    }
    const shardDigest = eventFlatShardDigest(request);
    const digestBuffer = shardDigest.buffer.slice(shardDigest.byteOffset, shardDigest.byteOffset + shardDigest.byteLength);
    const branchMerkleDigest = branchMerkleNodes === undefined
      ? undefined
      : digest(branchMerkleNodes);
    const branchMerkleDigestBuffer = branchMerkleDigest?.buffer.slice(
      branchMerkleDigest.byteOffset,
      branchMerkleDigest.byteOffset + branchMerkleDigest.byteLength
    );
    const transfers = [
      request.nodeHashes,
      request.nodeValues,
      request.nodeValueOffsets,
      digestBuffer,
    ];
    if (branchMerkleNodes !== undefined) transfers.push(branchMerkleNodes.buffer);
    if (branchMerkleDigestBuffer !== undefined) transfers.push(branchMerkleDigestBuffer);
    parentPort.postMessage({
      ...request,
      digest: digestBuffer,
      branchMerkleNodes: branchMerkleNodes?.buffer,
      branchMerkleDigest: branchMerkleDigestBuffer,
    }, transfers);
  } catch (error) {
    parentPort.postMessage({ error: error instanceof Error ? error.message : String(error) });
  }
};
blake2b.ready(() => {
  parentPort.on("message", (request) => {
    if (blake2b.WASM_SUPPORTED !== true || blake2b.WASM_LOADED !== true) {
      parentPort.postMessage({ error: "Event-flat worker BLAKE2b WebAssembly failed to initialize" });
      return;
    }
    handleRequest(request);
  });
});`;
  return new Promise((resolve, reject) => {
    const worker = new Worker(source, {
      eval: true,
      workerData: { blake2bPath },
    });
    const timeout = setTimeout(() => {
      void worker.terminate();
      reject(new Error("Event-flat freeze worker timed out"));
    }, 120_000);
    worker.once(
      "message",
      (
        response:
          | (Omit<ParkedEventFlatShard, "encodedBytes"> & {
              readonly shardIndex: number;
              readonly branchMerkleNodes?: ArrayBuffer;
              readonly branchMerkleDigest?: ArrayBuffer;
            })
          | { readonly error: string },
      ) => {
        clearTimeout(timeout);
        void worker.terminate();
        if ("error" in response) {
          reject(new Error(response.error));
          return;
        }
        resolve({
          nodeCount: response.nodeCount,
          nodeHashes: response.nodeHashes,
          nodeValues: response.nodeValues,
          nodeValueOffsets: response.nodeValueOffsets,
          digest: response.digest,
          branchMerkleNodes: response.branchMerkleNodes,
          branchMerkleDigest: response.branchMerkleDigest,
          encodedBytes:
            response.nodeHashes.byteLength +
            response.nodeValues.byteLength +
            response.nodeValueOffsets.byteLength,
        });
      },
    );
    worker.once("error", (error) => {
      clearTimeout(timeout);
      void worker.terminate();
      reject(error);
    });
    worker.postMessage({ shardIndex, ...packed, includeMerkleNodes }, [
      packed.nodeHashes,
      packed.nodeValues,
      packed.nodeValueOffsets,
    ]);
  });
};

const cloneStoredValue = (value: PackedMpfStoredValue): PackedMpfStoredValue =>
  value.__kind === "Leaf"
    ? { ...value }
    : { ...value, children: [...value.children] };

export class PackedMpfProof {
  public constructor(
    public readonly rootHash: Buffer,
    public readonly key: Buffer,
    public readonly nodes: readonly PackedMpfProofNode[],
  ) {}

  public verify(includingItem: boolean): Buffer {
    if (this.rootHash.equals(EMPTY_ROOT)) {
      if (this.nodes.length !== 0 || includingItem) {
        throw new Error("Invalid empty packed MPF proof");
      }
      return Buffer.from(EMPTY_ROOT);
    }
    if (this.nodes.length === 0) {
      throw new Error("Packed MPF proof is missing its root node");
    }
    const path = digest(this.key).toString("hex");
    let cursor = 0;
    let expectedHash = Buffer.from(this.rootHash);
    let terminalPresent: boolean | undefined;
    for (const [index, proofNode] of this.nodes.entries()) {
      if (!proofNode.hash.equals(expectedHash)) {
        throw new Error("Packed MPF proof child linkage mismatch");
      }
      authenticatePackedMpfRecord(proofNode.hash, proofNode.value);
      const isLast = index === this.nodes.length - 1;
      if (proofNode.value.__kind === "Leaf") {
        terminalPresent =
          proofNode.value.key === this.key.toString("hex") &&
          proofNode.value.prefix === path.slice(cursor);
        if (!isLast) throw new Error("Packed MPF proof continues after a leaf");
        break;
      }
      const prefix = proofNode.value.prefix;
      if (!path.slice(cursor).startsWith(prefix)) {
        terminalPresent = false;
        if (!isLast) {
          throw new Error("Packed MPF proof continues after prefix divergence");
        }
        break;
      }
      cursor += prefix.length;
      const branch = Number.parseInt(path[cursor]!, 16);
      const child = proofNode.value.children[branch];
      cursor += 1;
      if (child == null) {
        terminalPresent = false;
        if (!isLast) {
          throw new Error("Packed MPF proof continues through an empty child");
        }
        break;
      }
      expectedHash = Buffer.from(child, "hex");
      if (isLast) {
        throw new Error("Packed MPF proof stops before its terminal node");
      }
    }
    if (terminalPresent !== includingItem) {
      throw new Error(
        `Packed MPF proof presence mismatch: expected=${includingItem.toString()},actual=${String(terminalPresent)}`,
      );
    }
    return Buffer.from(this.rootHash);
  }
}

export class ResumedEventFlatOverlay {
  private readonly arena: AuthenticatedPackedMpfArena;
  private readonly records: ReadonlyMap<string, PackedMpfStoredValue>;

  public constructor(public readonly artifact: ParkedEventFlatOverlay) {
    if (
      artifact.schemaVersion !== 1 ||
      artifact.baseRoot.byteLength !== HASH_BYTES ||
      artifact.candidateRoot.byteLength !== HASH_BYTES ||
      artifact.closureDigest.byteLength !== HASH_BYTES ||
      !Number.isSafeInteger(artifact.nodeCount) ||
      artifact.nodeCount < 0 ||
      artifact.shards.length < 1 ||
      artifact.shards.length > 16
    ) {
      throw new Error("Invalid event-flat V1 artifact shape");
    }
    const expectedClosure = eventFlatClosureDigest({
      trieName: artifact.trieName,
      baseRoot: artifact.baseRoot,
      candidateRoot: artifact.candidateRoot,
      shards: artifact.shards,
    });
    if (!expectedClosure.equals(Buffer.from(artifact.closureDigest))) {
      throw new Error("Event-flat V1 closure digest mismatch");
    }
    const records = new Map<string, PackedMpfStoredValue>();
    let nodeCount = 0;
    let encodedBytes = 0;
    for (const [shardIndex, shard] of artifact.shards.entries()) {
      if (
        !Number.isSafeInteger(shard.nodeCount) ||
        shard.nodeCount < 0 ||
        shard.nodeHashes.byteLength !== shard.nodeCount * HASH_BYTES ||
        shard.nodeValueOffsets.byteLength !== shard.nodeCount * 8 ||
        shard.digest.byteLength !== HASH_BYTES ||
        shard.encodedBytes !==
          shard.nodeHashes.byteLength +
            shard.nodeValues.byteLength +
            shard.nodeValueOffsets.byteLength
      ) {
        throw new Error("Invalid event-flat V1 shard shape");
      }
      const expectedDigest = eventFlatShardDigest({
        shardIndex,
        nodeCount: shard.nodeCount,
        nodeHashes: shard.nodeHashes,
        nodeValues: shard.nodeValues,
        nodeValueOffsets: shard.nodeValueOffsets,
      });
      if (!expectedDigest.equals(Buffer.from(shard.digest))) {
        throw new Error("Event-flat V1 shard digest mismatch");
      }
      const hashes = new Uint8Array(shard.nodeHashes);
      const values = new Uint8Array(shard.nodeValues);
      const offsets = new Uint32Array(shard.nodeValueOffsets);
      for (let index = 0; index < shard.nodeCount; index += 1) {
        const offset = offsets[index * 2]!;
        const length = offsets[index * 2 + 1]!;
        if (offset + length > values.length) {
          throw new Error("Event-flat V1 node exceeds its shard arena");
        }
        const hash = Buffer.from(
          hashes.subarray(index * HASH_BYTES, (index + 1) * HASH_BYTES),
        );
        const value = JSON.parse(
          Buffer.from(values.subarray(offset, offset + length)).toString(),
        ) as PackedMpfStoredValue;
        authenticatePackedMpfRecord(hash, value);
        const key = hash.toString("hex");
        if (records.has(key)) {
          throw new Error(`Event-flat V1 contains duplicate node ${key}`);
        }
        records.set(key, value);
      }
      nodeCount += shard.nodeCount;
      encodedBytes += shard.encodedBytes;
    }
    if (
      nodeCount !== artifact.nodeCount ||
      encodedBytes !== artifact.encodedBytes
    ) {
      throw new Error("Event-flat V1 aggregate counts do not match shards");
    }
    const candidateRoot = Buffer.from(artifact.candidateRoot);
    const reachable = new Set<string>();
    const pending = candidateRoot.equals(EMPTY_ROOT)
      ? []
      : [candidateRoot.toString("hex")];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (reachable.has(key)) continue;
      const value = records.get(key);
      if (value === undefined) continue;
      reachable.add(key);
      if (value.__kind !== "Branch") continue;
      for (const child of value.children) {
        if (child != null && records.has(child)) pending.push(child);
      }
    }
    if (reachable.size !== records.size) {
      throw new Error(
        `Event-flat V1 contains unreachable nodes: reachable=${reachable.size.toString()},packed=${records.size.toString()}`,
      );
    }
    this.records = records;
    this.arena = AuthenticatedPackedMpfArena.fromRecords(
      candidateRoot,
      records,
    );
  }

  public rootHash(): Buffer {
    return this.arena.rootHash();
  }

  public get(key: Buffer): Buffer | undefined {
    return this.arena.get(key);
  }

  public prove(key: Buffer): PackedMpfProof {
    return this.arena.prove(key);
  }

  public flushReadyRecords(): ReadonlyMap<string, PackedMpfStoredValue> {
    return this.records;
  }
}

export class AuthenticatedPackedMpfArena {
  private readonly metadata: Uint32Array;
  private readonly hashes: Uint8Array;
  private readonly prefixNibbles: Uint8Array;
  private readonly childHashes: Uint8Array;
  private readonly childIds: Int32Array;
  private readonly keys: Uint8Array;
  private readonly values: Uint8Array;
  private readonly idsByHash = new Map<string, number>();

  private constructor(
    private readonly rootHashValue: Buffer,
    records: readonly PackedMpfProofNode[],
  ) {
    let prefixLength = 0;
    let keyLength = 0;
    let valueLength = 0;
    for (const { hash, value } of records) {
      authenticatePackedMpfRecord(hash, value);
      const key = hash.toString("hex");
      if (this.idsByHash.has(key)) {
        throw new Error(`Packed MPF arena contains duplicate node ${key}`);
      }
      this.idsByHash.set(key, this.idsByHash.size);
      prefixLength += value.prefix.length;
      if (value.__kind === "Leaf") {
        keyLength += value.key.length / 2;
        valueLength += value.value.length / 2;
      }
    }
    this.metadata = new Uint32Array(records.length * METADATA_FIELDS);
    this.hashes = new Uint8Array(records.length * HASH_BYTES);
    this.prefixNibbles = new Uint8Array(prefixLength);
    this.childHashes = new Uint8Array(
      records.length * CHILDREN_PER_BRANCH * HASH_BYTES,
    );
    this.childIds = new Int32Array(records.length * CHILDREN_PER_BRANCH);
    this.childIds.fill(-1);
    this.keys = new Uint8Array(keyLength);
    this.values = new Uint8Array(valueLength);

    let prefixOffset = 0;
    let keyOffset = 0;
    let valueOffset = 0;
    for (const [id, { hash, value }] of records.entries()) {
      const meta = id * METADATA_FIELDS;
      this.hashes.set(hash, id * HASH_BYTES);
      const encodedPrefix = prefixBytes(value.prefix);
      this.prefixNibbles.set(encodedPrefix, prefixOffset);
      this.metadata[meta + 1] = prefixOffset;
      this.metadata[meta + 2] = encodedPrefix.length;
      prefixOffset += encodedPrefix.length;
      if (value.__kind === "Leaf") {
        const key = Buffer.from(value.key, "hex");
        const leafValue = Buffer.from(value.value, "hex");
        this.metadata[meta] = PackedNodeKind.Leaf;
        this.metadata[meta + 5] = keyOffset;
        this.metadata[meta + 6] = key.length;
        this.metadata[meta + 7] = valueOffset;
        this.metadata[meta + 8] = leafValue.length;
        this.keys.set(key, keyOffset);
        this.values.set(leafValue, valueOffset);
        keyOffset += key.length;
        valueOffset += leafValue.length;
        continue;
      }
      this.metadata[meta] = PackedNodeKind.Branch;
      this.metadata[meta + 3] = value.size;
      for (let branch = 0; branch < CHILDREN_PER_BRANCH; branch += 1) {
        const child = value.children[branch];
        const childOffset = (id * CHILDREN_PER_BRANCH + branch) * HASH_BYTES;
        if (child == null) continue;
        const childHash = Buffer.from(child, "hex");
        this.childHashes.set(childHash, childOffset);
        this.childIds[id * CHILDREN_PER_BRANCH + branch] =
          this.idsByHash.get(child) ?? -1;
      }
    }

    if (
      !this.rootHashValue.equals(EMPTY_ROOT) &&
      !this.idsByHash.has(this.rootHashValue.toString("hex"))
    ) {
      throw new Error("Packed MPF arena does not contain its root node");
    }
  }

  public static fromRecords(
    rootHash: Buffer,
    records: ReadonlyMap<string, PackedMpfStoredValue>,
  ): AuthenticatedPackedMpfArena {
    assertHash(rootHash, "Packed MPF root");
    const ordered = [...records].map(([hash, value]) => ({
      hash: Buffer.from(hash, "hex"),
      value: cloneStoredValue(value),
    }));
    return new AuthenticatedPackedMpfArena(Buffer.from(rootHash), ordered);
  }

  public static fromParkedArtifact(artifact: {
    readonly candidateRoot: ArrayBuffer;
    readonly nodeCount: number;
    readonly nodeHashes: ArrayBuffer;
    readonly nodeValues: ArrayBuffer;
    readonly nodeValueOffsets: ArrayBuffer;
  }): AuthenticatedPackedMpfArena {
    if (
      !Number.isSafeInteger(artifact.nodeCount) ||
      artifact.nodeCount < 0 ||
      artifact.nodeHashes.byteLength !== artifact.nodeCount * HASH_BYTES ||
      artifact.nodeValueOffsets.byteLength !== artifact.nodeCount * 8
    ) {
      throw new Error("Invalid packed MPF parked artifact shape");
    }
    const hashes = new Uint8Array(artifact.nodeHashes);
    const values = new Uint8Array(artifact.nodeValues);
    const offsets = new Uint32Array(artifact.nodeValueOffsets);
    const records = new Map<string, PackedMpfStoredValue>();
    for (let index = 0; index < artifact.nodeCount; index += 1) {
      const offset = offsets[index * 2]!;
      const length = offsets[index * 2 + 1]!;
      if (offset + length > values.length) {
        throw new Error("Packed MPF parked node exceeds its value arena");
      }
      const hash = Buffer.from(
        hashes.subarray(index * HASH_BYTES, (index + 1) * HASH_BYTES),
      );
      const value = JSON.parse(
        Buffer.from(values.subarray(offset, offset + length)).toString(),
      ) as PackedMpfStoredValue;
      records.set(hash.toString("hex"), value);
    }
    return AuthenticatedPackedMpfArena.fromRecords(
      Buffer.from(artifact.candidateRoot),
      records,
    );
  }

  public rootHash(): Buffer {
    return Buffer.from(this.rootHashValue);
  }

  public nodeCount(): number {
    return this.hashes.byteLength / HASH_BYTES;
  }

  private prefix(id: number): string {
    const meta = id * METADATA_FIELDS;
    const offset = this.metadata[meta + 1]!;
    const length = this.metadata[meta + 2]!;
    return [...this.prefixNibbles.subarray(offset, offset + length)]
      .map((nibble) => nibble.toString(16))
      .join("");
  }

  private hash(id: number): Buffer {
    return Buffer.from(
      this.hashes.subarray(id * HASH_BYTES, (id + 1) * HASH_BYTES),
    );
  }

  private record(id: number): PackedMpfStoredValue {
    const meta = id * METADATA_FIELDS;
    if (this.metadata[meta] === Number(PackedNodeKind.Leaf)) {
      const keyOffset = this.metadata[meta + 5]!;
      const keyLength = this.metadata[meta + 6]!;
      const valueOffset = this.metadata[meta + 7]!;
      const valueLength = this.metadata[meta + 8]!;
      return {
        __kind: "Leaf",
        prefix: this.prefix(id),
        key: Buffer.from(
          this.keys.subarray(keyOffset, keyOffset + keyLength),
        ).toString("hex"),
        value: Buffer.from(
          this.values.subarray(valueOffset, valueOffset + valueLength),
        ).toString("hex"),
      };
    }
    return {
      __kind: "Branch",
      prefix: this.prefix(id),
      size: this.metadata[meta + 3]!,
      children: Array.from({ length: CHILDREN_PER_BRANCH }, (_, branch) => {
        const offset = (id * CHILDREN_PER_BRANCH + branch) * HASH_BYTES;
        const hash = Buffer.from(
          this.childHashes.subarray(offset, offset + HASH_BYTES),
        );
        return hash.equals(Buffer.alloc(HASH_BYTES))
          ? undefined
          : hash.toString("hex");
      }),
    };
  }

  public get(key: Buffer): Buffer | undefined {
    const proof = this.prove(key);
    const last = proof.nodes.at(-1)?.value;
    if (
      last?.__kind !== "Leaf" ||
      last.key !== key.toString("hex") ||
      last.prefix !==
        digest(key)
          .toString("hex")
          .slice(
            proof.nodes
              .slice(0, -1)
              .reduce(
                (cursor, node) =>
                  cursor +
                  (node.value.__kind === "Branch"
                    ? node.value.prefix.length + 1
                    : 0),
                0,
              ),
          )
    ) {
      return undefined;
    }
    return Buffer.from(last.value, "hex");
  }

  public prove(key: Buffer): PackedMpfProof {
    if (this.rootHashValue.equals(EMPTY_ROOT)) {
      return new PackedMpfProof(Buffer.from(EMPTY_ROOT), Buffer.from(key), []);
    }
    const path = digest(key).toString("hex");
    const nodes: PackedMpfProofNode[] = [];
    let cursor = 0;
    let id = this.idsByHash.get(this.rootHashValue.toString("hex"));
    while (id !== undefined) {
      const value = this.record(id);
      nodes.push({ hash: this.hash(id), value });
      if (value.__kind === "Leaf") break;
      if (!path.slice(cursor).startsWith(value.prefix)) break;
      cursor += value.prefix.length;
      const branch = Number.parseInt(path[cursor]!, 16);
      cursor += 1;
      const childIndex = id * CHILDREN_PER_BRANCH + branch;
      const childHash = Buffer.from(
        this.childHashes.subarray(
          childIndex * HASH_BYTES,
          (childIndex + 1) * HASH_BYTES,
        ),
      );
      if (childHash.equals(Buffer.alloc(HASH_BYTES))) break;
      const childId = this.childIds[childIndex]!;
      if (childId < 0) {
        throw new Error(
          `Packed MPF proof crossed an unavailable durable frontier ${childHash.toString("hex")}`,
        );
      }
      id = childId;
    }
    return new PackedMpfProof(
      Buffer.from(this.rootHashValue),
      Buffer.from(key),
      nodes,
    );
  }
}

class GrowableByteArena {
  private bytes = new Uint8Array(1024);
  private used = 0;

  public append(value: Uint8Array): {
    readonly offset: number;
    readonly length: number;
  } {
    const required = this.used + value.length;
    if (required > this.bytes.length) {
      let capacity = this.bytes.length;
      while (capacity < required) capacity *= 2;
      const grown = new Uint8Array(capacity);
      grown.set(this.bytes.subarray(0, this.used));
      this.bytes = grown;
    }
    const offset = this.used;
    this.bytes.set(value, offset);
    this.used = required;
    return { offset, length: value.length };
  }

  public read(offset: number, length: number): Uint8Array {
    return this.bytes.subarray(offset, offset + length);
  }

  public byteLength(): number {
    return this.used;
  }
}

const commonPrefix = (left: string, right: string): string => {
  const length = Math.min(left.length, right.length);
  let index = 0;
  while (index < length && left[index] === right[index]) index += 1;
  return left.slice(0, index);
};

/**
 * Append-only copy-on-write mutation arena. Published records never change
 * after their hash is assigned; an event replaces only the root id.
 */
export class EventFlatMutationArena {
  private metadata = new Uint32Array(16 * METADATA_FIELDS);
  private hashes = new Uint8Array(16 * HASH_BYTES);
  private childHashes = new Uint8Array(16 * CHILDREN_PER_BRANCH * HASH_BYTES);
  private branchMerkleNodes = new Uint8Array(
    16 * BRANCH_MERKLE_INTERNALS * HASH_BYTES,
  );
  private readonly prefixes = new GrowableByteArena();
  private readonly keys = new GrowableByteArena();
  private readonly values = new GrowableByteArena();
  private readonly idsByHash = new Map<string, number>();
  private count = 0;
  private rootId: number | undefined;
  private rootHashValue = Buffer.from(EMPTY_ROOT);
  private baseNodeCount = 0;
  private authenticatedBranches = 0;
  private generatedFullBranches = 0;
  private generatedFullBranchDigests = 0;
  private incrementalBranchUpdates = 0;
  private incrementalBranchDigests = 0;

  private constructor() {}

  public static fromHydratedTrie(root: Trie): EventFlatMutationArena {
    const hydrated = root as Trie & {
      readonly hash?: Buffer | null;
      readonly children?: readonly (
        | (Trie & { readonly hash?: Buffer | null })
        | { readonly hash?: Buffer | null }
        | undefined
      )[];
      readonly serialise?: () => PackedMpfStoredValue;
    };
    if (hydrated.hash == null) return new EventFlatMutationArena();
    if (hydrated.serialise === undefined) {
      throw new Error("Event-flat source trie cannot serialize");
    }
    const arena = new EventFlatMutationArena();
    const pending: (typeof hydrated)[] = [hydrated];
    const visited = new Set<object>();
    while (pending.length > 0) {
      const node = pending.pop()!;
      if (visited.has(node)) continue;
      visited.add(node);
      if (node.hash == null || node.serialise === undefined) {
        throw new Error("Event-flat hydrated node is missing its content hash");
      }
      arena.appendAuthenticated(node.hash, node.serialise());
      for (const child of node.children ?? []) {
        if (child instanceof Trie) pending.push(child as typeof hydrated);
      }
    }
    arena.rootHashValue = Buffer.from(hydrated.hash);
    arena.rootId = arena.idsByHash.get(arena.rootHashValue.toString("hex"));
    if (arena.rootId === undefined) {
      throw new Error("Event-flat hydrated arena is missing its root");
    }
    arena.baseNodeCount = arena.count;
    return arena;
  }

  public static async fromHydratedTrieParallel(
    root: Trie,
    shardCount = 4,
  ): Promise<EventFlatMutationArena> {
    await prepareEventFlatDigest();
    const hydrated = root as Trie & {
      readonly hash?: Buffer | null;
      readonly children?: readonly (
        | (Trie & { readonly hash?: Buffer | null })
        | { readonly hash?: Buffer | null }
        | undefined
      )[];
      readonly serialise?: () => PackedMpfStoredValue;
    };
    if (hydrated.hash == null) return new EventFlatMutationArena();
    if (hydrated.serialise === undefined) {
      throw new Error("Event-flat source trie cannot serialize");
    }
    const records = new Map<string, PackedMpfStoredValue>();
    const pending: (typeof hydrated)[] = [hydrated];
    const visited = new Set<object>();
    while (pending.length > 0) {
      const node = pending.pop()!;
      if (visited.has(node)) continue;
      visited.add(node);
      if (node.hash == null || node.serialise === undefined) {
        throw new Error("Event-flat hydrated node is missing its content hash");
      }
      records.set(node.hash.toString("hex"), node.serialise());
      for (const child of node.children ?? []) {
        if (child instanceof Trie) pending.push(child as typeof hydrated);
      }
    }
    const boundedShards = Math.max(
      1,
      Math.min(
        16,
        records.size,
        Number.isSafeInteger(shardCount) ? shardCount : 4,
      ),
    );
    const partitions = Array.from(
      { length: boundedShards },
      () => [] as (readonly [string, PackedMpfStoredValue])[],
    );
    let recordIndex = 0;
    for (const record of records) {
      partitions[recordIndex % boundedShards]!.push(record);
      recordIndex += 1;
    }
    const shards = await Promise.all(
      partitions.map((partition, shardIndex) =>
        runEventFlatFreezeWorker(
          shardIndex,
          packEventFlatShard(partition),
          true,
        ),
      ),
    );
    const arena = new EventFlatMutationArena();
    for (const [shardIndex, shard] of shards.entries()) {
      const expectedDigest = eventFlatShardDigest({
        shardIndex,
        nodeCount: shard.nodeCount,
        nodeHashes: shard.nodeHashes,
        nodeValues: shard.nodeValues,
        nodeValueOffsets: shard.nodeValueOffsets,
      });
      if (!expectedDigest.equals(Buffer.from(shard.digest))) {
        throw new Error("Event-flat compile worker shard digest mismatch");
      }
      if (
        shard.branchMerkleNodes === undefined ||
        shard.branchMerkleDigest === undefined ||
        shard.branchMerkleNodes.byteLength !==
          shard.nodeCount * BRANCH_MERKLE_INTERNALS * HASH_BYTES
      ) {
        throw new Error("Event-flat compile worker cache shape mismatch");
      }
      if (
        !digest(new Uint8Array(shard.branchMerkleNodes)).equals(
          Buffer.from(shard.branchMerkleDigest),
        )
      ) {
        throw new Error("Event-flat compile worker cache digest mismatch");
      }
      const hashes = new Uint8Array(shard.nodeHashes);
      const values = new Uint8Array(shard.nodeValues);
      const offsets = new Uint32Array(shard.nodeValueOffsets);
      const merkleCache = new Uint8Array(shard.branchMerkleNodes);
      for (let index = 0; index < shard.nodeCount; index += 1) {
        const hash = Buffer.from(
          hashes.subarray(index * HASH_BYTES, (index + 1) * HASH_BYTES),
        );
        const offset = offsets[index * 2]!;
        const length = offsets[index * 2 + 1]!;
        if (offset + length > values.length) {
          throw new Error("Event-flat compile worker value exceeds its arena");
        }
        const value = JSON.parse(
          Buffer.from(values.subarray(offset, offset + length)).toString(),
        ) as PackedMpfStoredValue;
        const merkleNodes =
          value.__kind === "Branch"
            ? Array.from(
                { length: CHILDREN_PER_BRANCH * 2 },
                (_, merkleIndex) =>
                  merkleIndex === 0 || merkleIndex > BRANCH_MERKLE_INTERNALS
                    ? ZERO_HASH
                    : Buffer.from(
                        merkleCache.subarray(
                          (index * BRANCH_MERKLE_INTERNALS + merkleIndex - 1) *
                            HASH_BYTES,
                          (index * BRANCH_MERKLE_INTERNALS + merkleIndex) *
                            HASH_BYTES,
                        ),
                      ),
              )
            : undefined;
        arena.appendPacked(hash, value, merkleNodes);
      }
    }
    arena.rootHashValue = Buffer.from(hydrated.hash);
    arena.rootId = arena.idsByHash.get(arena.rootHashValue.toString("hex"));
    if (arena.rootId === undefined) {
      throw new Error("Event-flat hydrated arena is missing its root");
    }
    if (arena.reachableRecords().size !== arena.idsByHash.size) {
      throw new Error("Event-flat hydrated arena contains unreachable records");
    }
    arena.baseNodeCount = arena.count;
    return arena;
  }

  public static fromAuthenticatedRecords(
    rootHash: Buffer,
    records: readonly AuthenticatedPackedMpfRecord[],
  ): EventFlatMutationArena {
    const arena = new EventFlatMutationArena();
    for (const record of records) {
      arena.appendPacked(record.hash, record.value, record.branchMerkleNodes);
    }
    arena.rootHashValue = Buffer.from(rootHash);
    if (rootHash.equals(EMPTY_ROOT)) {
      if (records.length !== 0) {
        throw new Error("Empty event-flat root has authenticated records");
      }
      return arena;
    }
    arena.rootId = arena.idsByHash.get(rootHash.toString("hex"));
    if (arena.rootId === undefined) {
      throw new Error("Event-flat authenticated records are missing the root");
    }
    if (arena.reachableRecords().size !== arena.idsByHash.size) {
      throw new Error(
        "Event-flat authenticated records contain unreachable nodes",
      );
    }
    arena.baseNodeCount = arena.count;
    return arena;
  }

  public static fromParkedArtifact(artifact: {
    readonly candidateRoot: ArrayBuffer;
    readonly nodeCount: number;
    readonly nodeHashes: ArrayBuffer;
    readonly nodeValues: ArrayBuffer;
    readonly nodeValueOffsets: ArrayBuffer;
  }): EventFlatMutationArena {
    const authenticated =
      AuthenticatedPackedMpfArena.fromParkedArtifact(artifact);
    const arena = new EventFlatMutationArena();
    const hashes = new Uint8Array(artifact.nodeHashes);
    const values = new Uint8Array(artifact.nodeValues);
    const offsets = new Uint32Array(artifact.nodeValueOffsets);
    for (let index = 0; index < artifact.nodeCount; index += 1) {
      const hash = Buffer.from(
        hashes.subarray(index * HASH_BYTES, (index + 1) * HASH_BYTES),
      );
      const offset = offsets[index * 2]!;
      const length = offsets[index * 2 + 1]!;
      const value = JSON.parse(
        Buffer.from(values.subarray(offset, offset + length)).toString(),
      ) as PackedMpfStoredValue;
      arena.appendAuthenticated(hash, value);
    }
    arena.rootHashValue = authenticated.rootHash();
    arena.rootId = arena.idsByHash.get(arena.rootHashValue.toString("hex"));
    if (!arena.rootHashValue.equals(EMPTY_ROOT) && arena.rootId === undefined) {
      throw new Error(
        "Event-flat mutation arena is missing its candidate root",
      );
    }
    return arena;
  }

  private ensureNodeCapacity(required: number): void {
    const capacity = this.hashes.byteLength / HASH_BYTES;
    if (required <= capacity) return;
    let grownCapacity = capacity;
    while (grownCapacity < required) grownCapacity *= 2;
    const metadata = new Uint32Array(grownCapacity * METADATA_FIELDS);
    metadata.set(this.metadata);
    this.metadata = metadata;
    const hashes = new Uint8Array(grownCapacity * HASH_BYTES);
    hashes.set(this.hashes);
    this.hashes = hashes;
    const childHashes = new Uint8Array(
      grownCapacity * CHILDREN_PER_BRANCH * HASH_BYTES,
    );
    childHashes.set(this.childHashes);
    this.childHashes = childHashes;
    const branchMerkleNodes = new Uint8Array(
      grownCapacity * BRANCH_MERKLE_INTERNALS * HASH_BYTES,
    );
    branchMerkleNodes.set(this.branchMerkleNodes);
    this.branchMerkleNodes = branchMerkleNodes;
  }

  private appendAuthenticated(
    hash: Buffer,
    value: PackedMpfStoredValue,
  ): number {
    const merkleNodes = authenticatePackedMpfRecord(hash, value);
    return this.appendPacked(hash, value, merkleNodes);
  }

  private appendPacked(
    hash: Buffer,
    value: PackedMpfStoredValue,
    authenticatedMerkleNodes?: readonly Buffer[],
  ): number {
    const hashHex = hash.toString("hex");
    const existing = this.idsByHash.get(hashHex);
    if (existing !== undefined) return existing;
    const id = this.count;
    this.ensureNodeCapacity(id + 1);
    this.count += 1;
    this.idsByHash.set(hashHex, id);
    this.hashes.set(hash, id * HASH_BYTES);
    const meta = id * METADATA_FIELDS;
    const prefix = this.prefixes.append(prefixBytes(value.prefix));
    this.metadata[meta + 1] = prefix.offset;
    this.metadata[meta + 2] = prefix.length;
    if (value.__kind === "Leaf") {
      const key = this.keys.append(Buffer.from(value.key, "hex"));
      const leafValue = this.values.append(Buffer.from(value.value, "hex"));
      this.metadata[meta] = PackedNodeKind.Leaf;
      this.metadata[meta + 5] = key.offset;
      this.metadata[meta + 6] = key.length;
      this.metadata[meta + 7] = leafValue.offset;
      this.metadata[meta + 8] = leafValue.length;
      return id;
    }
    this.metadata[meta] = PackedNodeKind.Branch;
    this.authenticatedBranches += 1;
    this.metadata[meta + 3] = value.size;
    for (let branch = 0; branch < CHILDREN_PER_BRANCH; branch += 1) {
      const child = value.children[branch];
      if (child == null) continue;
      this.childHashes.set(
        Buffer.from(child, "hex"),
        (id * CHILDREN_PER_BRANCH + branch) * HASH_BYTES,
      );
    }
    this.storeBranchMerkleNodes(
      id,
      authenticatedMerkleNodes ??
        computeBranchMerkleNodes(
          value.children.map((child) =>
            child == null ? undefined : Buffer.from(child, "hex"),
          ),
        ),
    );
    return id;
  }

  private prefix(id: number): string {
    const meta = id * METADATA_FIELDS;
    const digits = this.prefixes.read(
      this.metadata[meta + 1]!,
      this.metadata[meta + 2]!,
    );
    let prefix = "";
    for (const digit of digits) prefix += digit.toString(16);
    return prefix;
  }

  private hash(id: number): Buffer {
    return Buffer.from(
      this.hashes.subarray(id * HASH_BYTES, (id + 1) * HASH_BYTES),
    );
  }

  private childHash(id: number, branch: number): Buffer | undefined {
    const offset = (id * CHILDREN_PER_BRANCH + branch) * HASH_BYTES;
    const hash = Buffer.from(
      this.childHashes.subarray(offset, offset + HASH_BYTES),
    );
    return hash.equals(ZERO_HASH) ? undefined : hash;
  }

  private branchMerkleNode(id: number, index: number): Buffer {
    if (index < 1 || index > BRANCH_MERKLE_INTERNALS) {
      throw new Error(
        `Invalid event-flat Merkle node index ${index.toString()}`,
      );
    }
    const offset = (id * BRANCH_MERKLE_INTERNALS + index - 1) * HASH_BYTES;
    return Buffer.from(
      this.branchMerkleNodes.subarray(offset, offset + HASH_BYTES),
    );
  }

  private setBranchMerkleNode(id: number, index: number, hash: Buffer): void {
    this.branchMerkleNodes.set(
      hash,
      (id * BRANCH_MERKLE_INTERNALS + index - 1) * HASH_BYTES,
    );
  }

  private storeBranchMerkleNodes(id: number, nodes: readonly Buffer[]): void {
    for (let index = 1; index <= BRANCH_MERKLE_INTERNALS; index += 1) {
      this.setBranchMerkleNode(id, index, nodes[index]!);
    }
  }

  private children(id: number): (Buffer | undefined)[] {
    return Array.from({ length: CHILDREN_PER_BRANCH }, (_, branch) =>
      this.childHash(id, branch),
    );
  }

  private leafKey(id: number): Buffer {
    const meta = id * METADATA_FIELDS;
    return Buffer.from(
      this.keys.read(this.metadata[meta + 5]!, this.metadata[meta + 6]!),
    );
  }

  private leafValue(id: number): Buffer {
    const meta = id * METADATA_FIELDS;
    return Buffer.from(
      this.values.read(this.metadata[meta + 7]!, this.metadata[meta + 8]!),
    );
  }

  private record(id: number): PackedMpfStoredValue {
    const meta = id * METADATA_FIELDS;
    if (this.metadata[meta] === Number(PackedNodeKind.Leaf)) {
      return {
        __kind: "Leaf",
        prefix: this.prefix(id),
        key: Buffer.from(
          this.keys.read(this.metadata[meta + 5]!, this.metadata[meta + 6]!),
        ).toString("hex"),
        value: Buffer.from(
          this.values.read(this.metadata[meta + 7]!, this.metadata[meta + 8]!),
        ).toString("hex"),
      };
    }
    return {
      __kind: "Branch",
      prefix: this.prefix(id),
      size: this.metadata[meta + 3]!,
      children: Array.from({ length: CHILDREN_PER_BRANCH }, (_, branch) =>
        this.childHash(id, branch)?.toString("hex"),
      ),
    };
  }

  private resolveChild(hash: Buffer): number {
    const id = this.idsByHash.get(hash.toString("hex"));
    if (id === undefined) {
      throw new Error(
        `Event-flat mutation crossed an unavailable durable frontier ${hash.toString("hex")}`,
      );
    }
    return id;
  }

  private appendLeaf(prefix: string, key: Buffer, value: Buffer): number {
    const hash = leafHash(prefix, value);
    return this.appendPacked(hash, {
      __kind: "Leaf",
      prefix,
      key: key.toString("hex"),
      value: value.toString("hex"),
    });
  }

  private appendBranch(
    prefix: string,
    children: readonly (Buffer | undefined)[],
    size: number,
  ): number {
    this.generatedFullBranches += 1;
    this.generatedFullBranchDigests += CHILDREN_PER_BRANCH;
    const merkleNodes = computeBranchMerkleNodes(children);
    const hash = digest(Buffer.concat([prefixBytes(prefix), merkleNodes[1]!]));
    const hashHex = hash.toString("hex");
    const existing = this.idsByHash.get(hashHex);
    if (existing !== undefined) return existing;
    const id = this.count;
    this.ensureNodeCapacity(id + 1);
    this.count += 1;
    this.idsByHash.set(hashHex, id);
    this.hashes.set(hash, id * HASH_BYTES);
    const meta = id * METADATA_FIELDS;
    const storedPrefix = this.prefixes.append(prefixBytes(prefix));
    this.metadata[meta] = PackedNodeKind.Branch;
    this.metadata[meta + 1] = storedPrefix.offset;
    this.metadata[meta + 2] = storedPrefix.length;
    this.metadata[meta + 3] = size;
    for (let branch = 0; branch < CHILDREN_PER_BRANCH; branch += 1) {
      const child = children[branch];
      if (child === undefined) continue;
      this.childHashes.set(
        child,
        (id * CHILDREN_PER_BRANCH + branch) * HASH_BYTES,
      );
    }
    this.storeBranchMerkleNodes(id, merkleNodes);
    return id;
  }

  private appendUpdatedBranch(
    sourceId: number,
    branch: number,
    child: Buffer | undefined,
    size: number,
  ): number {
    this.incrementalBranchUpdates += 1;
    this.incrementalBranchDigests += 5;
    const sourceMeta = sourceId * METADATA_FIELDS;
    if (this.metadata[sourceMeta] !== Number(PackedNodeKind.Branch)) {
      throw new Error("Event-flat Merkle update source is not a branch");
    }
    const updated = new Map<number, Buffer>();
    let index = CHILDREN_PER_BRANCH + branch;
    let current = child ?? ZERO_HASH;
    while (index > 1) {
      const siblingIndex = index ^ 1;
      const sibling =
        siblingIndex >= CHILDREN_PER_BRANCH
          ? (this.childHash(sourceId, siblingIndex - CHILDREN_PER_BRANCH) ??
            ZERO_HASH)
          : (updated.get(siblingIndex) ??
            this.branchMerkleNode(sourceId, siblingIndex));
      current =
        index % 2 === 0
          ? digest(Buffer.concat([current, sibling]))
          : digest(Buffer.concat([sibling, current]));
      index >>= 1;
      updated.set(index, current);
    }
    const prefix = this.prefix(sourceId);
    const hash = digest(Buffer.concat([prefixBytes(prefix), current]));
    const hashHex = hash.toString("hex");
    const existing = this.idsByHash.get(hashHex);
    if (existing !== undefined) return existing;
    const id = this.count;
    this.ensureNodeCapacity(id + 1);
    this.count += 1;
    this.idsByHash.set(hashHex, id);
    this.hashes.set(hash, id * HASH_BYTES);
    const meta = id * METADATA_FIELDS;
    const storedPrefix = this.prefixes.append(prefixBytes(prefix));
    this.metadata[meta] = PackedNodeKind.Branch;
    this.metadata[meta + 1] = storedPrefix.offset;
    this.metadata[meta + 2] = storedPrefix.length;
    this.metadata[meta + 3] = size;
    const sourceChildrenOffset = sourceId * CHILDREN_PER_BRANCH * HASH_BYTES;
    const targetChildrenOffset = id * CHILDREN_PER_BRANCH * HASH_BYTES;
    this.childHashes.set(
      this.childHashes.subarray(
        sourceChildrenOffset,
        sourceChildrenOffset + CHILDREN_PER_BRANCH * HASH_BYTES,
      ),
      targetChildrenOffset,
    );
    const targetChildOffset = targetChildrenOffset + branch * HASH_BYTES;
    if (child === undefined) {
      this.childHashes.fill(
        0,
        targetChildOffset,
        targetChildOffset + HASH_BYTES,
      );
    } else {
      this.childHashes.set(child, targetChildOffset);
    }
    const sourceMerkleOffset = sourceId * BRANCH_MERKLE_INTERNALS * HASH_BYTES;
    const targetMerkleOffset = id * BRANCH_MERKLE_INTERNALS * HASH_BYTES;
    this.branchMerkleNodes.set(
      this.branchMerkleNodes.subarray(
        sourceMerkleOffset,
        sourceMerkleOffset + BRANCH_MERKLE_INTERNALS * HASH_BYTES,
      ),
      targetMerkleOffset,
    );
    for (const [merkleIndex, merkleHash] of updated) {
      this.setBranchMerkleNode(id, merkleIndex, merkleHash);
    }
    return id;
  }

  private insertAt(
    id: number | undefined,
    cursor: number,
    path: string,
    key: Buffer,
    value: Buffer,
  ): number {
    if (id === undefined)
      return this.appendLeaf(path.slice(cursor), key, value);
    const meta = id * METADATA_FIELDS;
    const nodePrefix = this.prefix(id);
    if (this.metadata[meta] === Number(PackedNodeKind.Leaf)) {
      const nodeKey = this.leafKey(id);
      if (nodeKey.equals(key)) {
        throw new Error(
          `Event-flat key already exists: ${nodeKey.toString("hex")}`,
        );
      }
      const remaining = path.slice(cursor);
      const prefix = commonPrefix(nodePrefix, remaining);
      const oldNibble = Number.parseInt(nodePrefix[prefix.length]!, 16);
      const newNibble = Number.parseInt(remaining[prefix.length]!, 16);
      if (oldNibble === newNibble) {
        throw new Error("Event-flat leaf split did not diverge");
      }
      const oldLeaf = this.appendLeaf(
        nodePrefix.slice(prefix.length + 1),
        nodeKey,
        this.leafValue(id),
      );
      const newLeaf = this.appendLeaf(
        remaining.slice(prefix.length + 1),
        key,
        value,
      );
      const children: (Buffer | undefined)[] = Array(CHILDREN_PER_BRANCH);
      children[oldNibble] = this.hash(oldLeaf);
      children[newNibble] = this.hash(newLeaf);
      return this.appendBranch(prefix, children, 2);
    }

    const remaining = path.slice(cursor);
    const prefix = commonPrefix(nodePrefix, remaining);
    const nodeSize = this.metadata[meta + 3]!;
    if (prefix.length < nodePrefix.length) {
      const oldNibble = Number.parseInt(nodePrefix[prefix.length]!, 16);
      const newNibble = Number.parseInt(remaining[prefix.length]!, 16);
      if (oldNibble === newNibble) {
        throw new Error("Event-flat branch split did not diverge");
      }
      const oldBranch = this.appendBranch(
        nodePrefix.slice(prefix.length + 1),
        this.children(id),
        nodeSize,
      );
      const newLeaf = this.appendLeaf(
        remaining.slice(prefix.length + 1),
        key,
        value,
      );
      const children: (Buffer | undefined)[] = Array(CHILDREN_PER_BRANCH);
      children[oldNibble] = this.hash(oldBranch);
      children[newNibble] = this.hash(newLeaf);
      return this.appendBranch(prefix, children, nodeSize + 1);
    }

    const branch = Number.parseInt(path[cursor + nodePrefix.length]!, 16);
    const existingHash = this.childHash(id, branch);
    const child =
      existingHash === undefined ? undefined : this.resolveChild(existingHash);
    const inserted = this.insertAt(
      child,
      cursor + nodePrefix.length + 1,
      path,
      key,
      value,
    );
    return this.appendUpdatedBranch(
      id,
      branch,
      this.hash(inserted),
      nodeSize + 1,
    );
  }

  private deleteAt(
    id: number,
    cursor: number,
    path: string,
    key: Buffer,
  ): number | undefined {
    const meta = id * METADATA_FIELDS;
    const nodePrefix = this.prefix(id);
    if (this.metadata[meta] === Number(PackedNodeKind.Leaf)) {
      if (!this.leafKey(id).equals(key) || nodePrefix !== path.slice(cursor)) {
        throw new Error(`Event-flat key is absent: ${key.toString("hex")}`);
      }
      return undefined;
    }
    if (!path.slice(cursor).startsWith(nodePrefix)) {
      throw new Error(`Event-flat key is absent: ${key.toString("hex")}`);
    }
    const branch = Number.parseInt(path[cursor + nodePrefix.length]!, 16);
    const childHash = this.childHash(id, branch);
    if (childHash === undefined) {
      throw new Error(`Event-flat key is absent: ${key.toString("hex")}`);
    }
    const deleted = this.deleteAt(
      this.resolveChild(childHash),
      cursor + nodePrefix.length + 1,
      path,
      key,
    );
    const children = this.children(id);
    children[branch] = deleted === undefined ? undefined : this.hash(deleted);
    const remaining = children.flatMap((hash, index) =>
      hash === undefined ? [] : [{ hash, index }],
    );
    if (remaining.length === 0) return undefined;
    if (remaining.length === 1) {
      const only = remaining[0]!;
      const childId = this.resolveChild(only.hash);
      const childMeta = childId * METADATA_FIELDS;
      const childPrefix = this.prefix(childId);
      const prefix = `${nodePrefix}${only.index.toString(16)}${childPrefix}`;
      return this.metadata[childMeta] === Number(PackedNodeKind.Leaf)
        ? this.appendLeaf(
            prefix,
            this.leafKey(childId),
            this.leafValue(childId),
          )
        : this.appendBranch(
            prefix,
            this.children(childId),
            this.metadata[childMeta + 3]!,
          );
    }
    return this.appendUpdatedBranch(
      id,
      branch,
      children[branch],
      this.metadata[meta + 3]! - 1,
    );
  }

  public applyEvent(
    ops: readonly (
      | {
          readonly type: "insert";
          readonly key: Buffer;
          readonly value: Buffer;
        }
      | { readonly type: "delete"; readonly key: Buffer }
    )[],
  ): Buffer {
    let root = this.rootId;
    for (const op of ops) {
      const path = digest(op.key).toString("hex");
      root =
        op.type === "insert"
          ? this.insertAt(root, 0, path, op.key, op.value)
          : root === undefined
            ? (() => {
                throw new Error(
                  `Event-flat key is absent: ${op.key.toString("hex")}`,
                );
              })()
            : this.deleteAt(root, 0, path, op.key);
    }
    this.rootId = root;
    this.rootHashValue =
      root === undefined ? Buffer.from(EMPTY_ROOT) : this.hash(root);
    return Buffer.from(this.rootHashValue);
  }

  public rootHash(): Buffer {
    return Buffer.from(this.rootHashValue);
  }

  public nodeCount(): number {
    return this.count;
  }

  public estimatedBytes(): number {
    return (
      this.count * METADATA_FIELDS * Uint32Array.BYTES_PER_ELEMENT +
      this.count * HASH_BYTES +
      this.count * CHILDREN_PER_BRANCH * HASH_BYTES +
      this.count * BRANCH_MERKLE_INTERNALS * HASH_BYTES +
      this.prefixes.byteLength() +
      this.keys.byteLength() +
      this.values.byteLength()
    );
  }

  public diagnostics(): EventFlatMutationDiagnostics {
    const reachableNodeCount = this.reachableRecords().size;
    const reachableDirtyNodeCount = this.reachableDirtyRecords().size;
    return {
      nodeCount: this.count,
      baseNodeCount: this.baseNodeCount,
      reachableNodeCount,
      reachableDirtyNodeCount,
      estimatedBytes: this.estimatedBytes(),
      authenticatedBranches: this.authenticatedBranches,
      generatedFullBranches: this.generatedFullBranches,
      generatedFullBranchDigests: this.generatedFullBranchDigests,
      incrementalBranchUpdates: this.incrementalBranchUpdates,
      incrementalBranchDigests: this.incrementalBranchDigests,
      copiedBranchCacheBytes:
        this.incrementalBranchUpdates *
        (CHILDREN_PER_BRANCH + BRANCH_MERKLE_INTERNALS) *
        HASH_BYTES,
      branchCacheBytes: this.count * BRANCH_MERKLE_INTERNALS * HASH_BYTES,
    };
  }

  public get(key: Buffer): Buffer | undefined {
    return this.freeze().get(key);
  }

  public prove(key: Buffer): PackedMpfProof {
    return this.freeze().prove(key);
  }

  public async freezeParallel({
    trieName,
    baseRoot,
    shardCount = 4,
  }: {
    readonly trieName: string;
    readonly baseRoot: Buffer;
    readonly shardCount?: number;
  }): Promise<ParkedEventFlatOverlay> {
    assertHash(baseRoot, "Event-flat V1 base root");
    const boundedShardCount = Math.max(1, Math.min(16, Math.floor(shardCount)));
    const records = [...this.reachableDirtyRecords()].sort(([left], [right]) =>
      left.localeCompare(right),
    );
    const recordsPerShard = Math.max(
      1,
      Math.ceil(records.length / boundedShardCount),
    );
    const packedShards = Array.from(
      { length: boundedShardCount },
      (_, shardIndex) =>
        packEventFlatShard(
          records.slice(
            shardIndex * recordsPerShard,
            (shardIndex + 1) * recordsPerShard,
          ),
        ),
    );
    const shards = await Promise.all(
      packedShards.map((packed, shardIndex) =>
        runEventFlatFreezeWorker(shardIndex, packed),
      ),
    );
    const baseRootBuffer = exactArrayBuffer(baseRoot);
    const candidateRoot = exactArrayBuffer(this.rootHashValue);
    const closureDigest = eventFlatClosureDigest({
      trieName,
      baseRoot: baseRootBuffer,
      candidateRoot,
      shards,
    });
    return {
      schemaVersion: 1,
      trieName,
      baseRoot: baseRootBuffer,
      candidateRoot,
      closureDigest: exactArrayBuffer(closureDigest),
      nodeCount: records.length,
      shards,
      encodedBytes: shards.reduce(
        (total, shard) => total + shard.encodedBytes,
        0,
      ),
    };
  }

  public reachableRecords(): ReadonlyMap<string, PackedMpfStoredValue> {
    if (this.rootId === undefined) return new Map();
    const records = new Map<string, PackedMpfStoredValue>();
    const pending = [this.rootId];
    while (pending.length > 0) {
      const id = pending.pop()!;
      const hash = this.hash(id);
      const hashHex = hash.toString("hex");
      if (records.has(hashHex)) continue;
      const record = this.record(id);
      records.set(hashHex, record);
      if (record.__kind !== "Branch") continue;
      for (const child of record.children) {
        if (child == null) continue;
        const childId = this.idsByHash.get(child);
        if (childId !== undefined) pending.push(childId);
      }
    }
    return records;
  }

  public reachableDirtyRecords(): ReadonlyMap<string, PackedMpfStoredValue> {
    if (this.rootId === undefined) return new Map();
    const records = new Map<string, PackedMpfStoredValue>();
    const visited = new Set<number>();
    const pending = [this.rootId];
    while (pending.length > 0) {
      const id = pending.pop()!;
      if (visited.has(id)) continue;
      visited.add(id);
      if (id < this.baseNodeCount) continue;
      const hash = this.hash(id);
      const record = this.record(id);
      records.set(hash.toString("hex"), record);
      if (record.__kind !== "Branch") continue;
      for (const child of record.children) {
        if (child == null) continue;
        const childId = this.idsByHash.get(child);
        if (childId !== undefined) pending.push(childId);
      }
    }
    return records;
  }

  public freeze(): AuthenticatedPackedMpfArena {
    if (this.rootId === undefined) {
      return AuthenticatedPackedMpfArena.fromRecords(
        Buffer.from(EMPTY_ROOT),
        new Map(),
      );
    }
    return AuthenticatedPackedMpfArena.fromRecords(
      Buffer.from(this.rootHashValue),
      this.reachableRecords(),
    );
  }
}
