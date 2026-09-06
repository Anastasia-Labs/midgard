import {
  encodeMidgardCekBlobChunk,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  hashMidgardCekBlobChunk,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  type MidgardCekProgramMaterialEntry,
  type MidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardCekDataNode,
  hashMidgardCekDataNode,
} from "@al-ft/midgard-core/cek-semantic";

/** Every lambda is reachable material; the argument is ignored and the context returned. */
export const cekSelectionProgram = (lambdaCount: number, dataGraph = false) => {
  if (!Number.isSafeInteger(lambdaCount) || lambdaCount < 1)
    throw new Error("positive lambda count required");
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
  let termRoot;
  if (dataGraph) {
    if (lambdaCount < 2)
      throw new Error("Data graph requires an ignored lambda body");
    const typeBytes = Buffer.from("9f00ff", "hex");
    const integerBytes = Buffer.from([1]);
    const typeRoot = hashMidgardCekBlobChunk(typeBytes);
    const integerRoot = hashMidgardCekBlobChunk(integerBytes);
    material.set(typeRoot.toString("hex"), {
      kind: "blobChunk",
      root: typeRoot,
      preimage: encodeMidgardCekBlobChunk(typeBytes),
    });
    material.set(integerRoot.toString("hex"), {
      kind: "blobChunk",
      root: integerRoot,
      preimage: encodeMidgardCekBlobChunk(integerBytes),
    });
    const node = {
      kind: "integer" as const,
      cborRoot: integerRoot,
      cborLength: 1n,
      memory: 5n,
    };
    const dataRoot = hashMidgardCekDataNode(node);
    material.set(dataRoot.toString("hex"), {
      kind: "dataNode",
      root: dataRoot,
      preimage: encodeMidgardCekDataNode(node),
    });
    const value = {
      kind: "constant" as const,
      typeRoot,
      payloadRoot: dataRoot,
      payloadLength: 1n,
      semanticRoot: dataRoot,
      memory: 1n,
    };
    const valueRoot = hashMidgardCekValueNode(value);
    material.set(valueRoot.toString("hex"), {
      kind: "value",
      root: valueRoot,
      preimage: encodeMidgardCekValueNode(value),
    });
    const constant = add({ kind: "constant", value: valueRoot });
    termRoot = add({
      kind: "application",
      function: constant,
      argument: constant,
    });
  } else termRoot = add({ kind: "variable", index: 0n });
  for (let i = 0; i < lambdaCount; i++)
    termRoot = add({ kind: "lambda", body: termRoot });
  if (lambdaCount > 1) {
    const context = add({ kind: "variable", index: 1n });
    const ignoreArgument = add({ kind: "lambda", body: context });
    const application = add({
      kind: "application",
      function: ignoreArgument,
      argument: termRoot,
    });
    termRoot = add({ kind: "lambda", body: application });
  }
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
