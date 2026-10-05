import {
  commitMidgardCekBlob,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  type MidgardCekProgramMaterialEntry,
  type MidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardCekConstantWitness,
  midgardCekConstantMemorySize,
} from "@al-ft/midgard-validation/cek-constant";
import { commitMidgardCekDataTree } from "@al-ft/midgard-validation/cek-data-tree";
import { Data } from "@lucid-evolution/lucid";

/** A constant application whose only failure is the selected builtin. */
export const cekBuiltinFailureProgram = (tag: 12 | 21 | 52 | 82 | 83) => {
  const material = new Map<string, MidgardCekProgramMaterialEntry>();
  const add = (node: MidgardCekTermNode) => {
    const root = hashMidgardCekTermNode(node);
    material.set(root.toString("hex"), {
      kind: "term",
      root,
      preimage: encodeMidgardCekTermNode(node),
    });
    return root;
  };
  const constant = (type: 0 | 1, payload: bigint | string) => {
    const typeCbor = Buffer.from([0x9f, type, 0xff]);
    const payloadCbor = Buffer.from(Data.to(payload), "hex");
    const decoded = decodeMidgardCekConstantWitness({ typeCbor, payloadCbor });
    const tree = commitMidgardCekDataTree(decoded.payload);
    for (const [key, node] of tree.dataNodes)
      material.set(key, {
        kind: "dataNode",
        root: Buffer.from(key, "hex"),
        preimage: node.preimage,
      });
    for (const [key, node] of tree.blobNodes)
      material.set(key, {
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(key, "hex"),
        preimage: node.preimage,
      });
    const typeBlob = commitMidgardCekBlob(typeCbor);
    for (const [key, node] of typeBlob.nodes)
      material.set(key, {
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(key, "hex"),
        preimage: node.preimage,
      });
    const value = {
      kind: "constant" as const,
      typeRoot: typeBlob.root,
      payloadRoot: tree.root,
      payloadLength: tree.cborLength,
      semanticRoot: tree.root,
      memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
    };
    const root = hashMidgardCekValueNode(value);
    material.set(root.toString("hex"), {
      kind: "value",
      root,
      preimage: encodeMidgardCekValueNode(value),
    });
    return add({ kind: "constant", value: root });
  };
  const overflow = () => constant(0, 1n << 63n);
  const source = () => constant(1, "abcd");
  const n = "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141";
  const args =
    tag === 12
      ? [overflow(), constant(0, 1n), source()]
      : tag === 21
        ? [
            constant(1, "00".repeat(31)),
            constant(1, ""),
            constant(1, "00".repeat(64)),
          ]
        : tag === 52
          ? [
              constant(
                1,
                "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
              ),
              constant(1, "00".repeat(32)),
              constant(1, n + "00".repeat(31) + "01"),
            ]
          : [source(), overflow()];
  let termRoot = add({ kind: "builtin", tag: BigInt(tag) });
  for (const argument of args)
    termRoot = add({ kind: "application", function: termRoot, argument });
  termRoot = add({ kind: "lambda", body: termRoot });
  const envelopeCbor = encodeMidgardCekProgramEnvelope({
    uplcVersion: [1n, 1n, 0n],
    termRoot,
    nodeCount: BigInt(material.size),
    materialByteLength: [...material.values()].reduce(
      (total, entry) => total + BigInt(entry.preimage.length),
      0n,
    ),
  });
  return { material, envelopeCbor };
};
