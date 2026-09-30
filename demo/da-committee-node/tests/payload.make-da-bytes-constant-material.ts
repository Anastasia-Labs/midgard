import {
  commitMidgardCekBlob,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardCekDataNode,
  hashMidgardCekDataNode,
  midgardCekDataBytesCborLength,
} from "@al-ft/midgard-core/cek-semantic";
import type { Hash32 } from "@al-ft/midgard-core/codec/hash";

export const makeDaBytesConstantMaterial = (
  rawByteLength: number,
  wrapperCount: number,
): {
  readonly envelopes: readonly MidgardCekProgramEnvelope[];
  readonly material: readonly MidgardCekProgramMaterialEntry[];
  readonly payloadCborLength: number;
} => {
  const rawBytes = Buffer.alloc(rawByteLength, 0x5a);
  const chunks: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < rawBytes.length; offset += 64) {
    const chunk = rawBytes.subarray(offset, offset + 64);
    chunks.push(
      chunk.length < 24
        ? Buffer.from([0x40 + chunk.length])
        : Buffer.from([0x58, chunk.length]),
      chunk,
    );
  }
  chunks.push(Buffer.from([0xff]));
  const payloadCbor = Buffer.concat(chunks);
  const typeBlob = commitMidgardCekBlob(Buffer.from("9f01ff", "hex"));
  const rawBlob = commitMidgardCekBlob(rawBytes);
  const semanticNode = {
    kind: "bytes",
    bytesRoot: rawBlob.root,
    bytesLength: BigInt(rawBytes.length),
    cborLength: midgardCekDataBytesCborLength(BigInt(rawBytes.length)),
    memory: 4n + BigInt(rawBytes.length),
  } as const;
  const semanticRoot = hashMidgardCekDataNode(semanticNode);
  const valueNode = {
    kind: "constant",
    typeRoot: typeBlob.root,
    payloadRoot: semanticRoot,
    payloadLength: BigInt(payloadCbor.length),
    semanticRoot,
    memory: BigInt(rawBytes.length),
  } as const;
  const valueRoot = hashMidgardCekValueNode(valueNode);
  const termNode = { kind: "constant", value: valueRoot } as const;
  let termRoot = hashMidgardCekTermNode(termNode);
  const material: MidgardCekProgramMaterialEntry[] = [
    {
      kind: "term",
      root: termRoot,
      preimage: encodeMidgardCekTermNode(termNode),
    },
    {
      kind: "value",
      root: valueRoot,
      preimage: encodeMidgardCekValueNode(valueNode),
    },
    {
      kind: "dataNode",
      root: semanticRoot,
      preimage: encodeMidgardCekDataNode(semanticNode),
    },
    ...[...typeBlob.nodes.entries(), ...rawBlob.nodes.entries()].map(
      ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(rootHex, "hex") as Hash32,
        preimage: node.preimage,
      }),
    ),
  ];
  let reachableNodeCount = BigInt(material.length);
  let reachableByteLength = material.reduce(
    (total, entry) => total + BigInt(entry.preimage.length),
    0n,
  );
  const envelopes: MidgardCekProgramEnvelope[] = [];
  if (wrapperCount === 0) {
    envelopes.push({
      uplcVersion: [1n, 1n, 0n],
      termRoot,
      nodeCount: reachableNodeCount,
      materialByteLength: reachableByteLength,
    });
  }
  const sharedConstantTermRoot = termRoot;
  for (let index = 0; index < wrapperCount; index += 1) {
    const wrapper = {
      kind: "application",
      function: termRoot,
      argument: sharedConstantTermRoot,
    } as const;
    const preimage = encodeMidgardCekTermNode(wrapper);
    termRoot = hashMidgardCekTermNode(wrapper);
    material.push({ kind: "term", root: termRoot, preimage });
    reachableNodeCount += 1n;
    reachableByteLength += BigInt(preimage.length);
    envelopes.push({
      uplcVersion: [1n, 1n, 0n],
      termRoot,
      nodeCount: reachableNodeCount,
      materialByteLength: reachableByteLength,
    });
  }
  return { envelopes, material, payloadCborLength: payloadCbor.length };
};
