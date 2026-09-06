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
  midgardCekDataBytesCborLength,
  midgardCekDataBytesMemory,
} from "@al-ft/midgard-core/cek-semantic";
import {
  decodeMidgardCekConstantWitness,
  midgardCekConstantMemorySize,
} from "@al-ft/midgard-validation/cek-constant";
import { commitMidgardCekDataTree } from "@al-ft/midgard-validation/cek-data-tree";
import { Data } from "@lucid-evolution/lucid";

/** Every lambda is reachable material; the argument is ignored and the context returned. */
export const cekSelectionProgram = (
  lambdaCount: number,
  dataGraph = false,
  directBuiltin = false,
  blsFinal = false,
  maximumDirect = false,
  semanticTag?: number,
) => {
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
  if (semanticTag !== undefined) {
    const context = add({ kind: "variable", index: 0n });
    const call = (tag: number, forces: number, args: readonly Buffer[]) => {
      let term = add({ kind: "builtin", tag: BigInt(tag) });
      for (let i = 0; i < forces; i++) term = add({ kind: "force", term });
      for (const argument of args)
        term = add({ kind: "application", function: term, argument });
      return term;
    };
    const pair = () => call(42, 0, [context]);
    const fields = () => call(30, 2, [pair()]);
    switch (semanticTag) {
      case 43: {
        const txInfo = call(33, 1, [fields()]);
        let txFields = call(30, 2, [call(42, 0, [txInfo])]);
        for (let i = 0; i < 9; i++) txFields = call(34, 1, [txFields]);
        termRoot = call(43, 0, [call(33, 1, [txFields])]);
        break;
      }
      case 42:
        termRoot = pair();
        break;
      case 29:
        termRoot = call(29, 2, [pair()]);
        break;
      case 33:
        termRoot = call(33, 1, [fields()]);
        break;
      case 35:
        termRoot = call(35, 1, [fields()]);
        break;
      case 36:
        termRoot = call(36, 1, [
          context,
          context,
          context,
          context,
          context,
          context,
        ]);
        break;
      case 40:
        termRoot = call(40, 0, [call(29, 2, [pair()])]);
        break;
      case 47:
        termRoot = call(47, 0, [context, context]);
        break;
      default:
        throw new Error(`Unsupported semantic fixture tag ${semanticTag}`);
    }
  } else if (maximumDirect) {
    const typeCbor = Buffer.from("9f02ff", "hex");
    const payloadCbor = Buffer.from(
      Data.to(Buffer.alloc(8933, 0x61).toString("hex")),
      "hex",
    );
    if (payloadCbor.length !== 9215)
      throw new Error(`Maximum trace payload is ${payloadCbor.length} bytes`);
    const decoded = decodeMidgardCekConstantWitness({ typeCbor, payloadCbor });
    const semantic = commitMidgardCekDataTree(decoded.payload);
    for (const [key, entry] of semantic.dataNodes)
      material.set(key, {
        kind: "dataNode",
        root: Buffer.from(key, "hex"),
        preimage: entry.preimage,
      });
    for (const [key, entry] of semantic.blobNodes)
      material.set(key, {
        kind: entry.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(key, "hex"),
        preimage: entry.preimage,
      });
    const typeRoot = hashMidgardCekBlobChunk(typeCbor);
    material.set(typeRoot.toString("hex"), {
      kind: "blobChunk",
      root: typeRoot,
      preimage: encodeMidgardCekBlobChunk(typeCbor),
    });
    const value = {
      kind: "constant" as const,
      typeRoot,
      payloadRoot: Buffer.from(semantic.root),
      payloadLength: semantic.cborLength,
      semanticRoot: Buffer.from(semantic.root),
      memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
    };
    const valueRoot = hashMidgardCekValueNode(value);
    material.set(valueRoot.toString("hex"), {
      kind: "value",
      root: valueRoot,
      preimage: encodeMidgardCekValueNode(value),
    });
    const message = add({ kind: "constant", value: valueRoot });
    const trace = add({
      kind: "force",
      term: add({ kind: "builtin", tag: 28n }),
    });
    termRoot = add({
      kind: "application",
      function: add({
        kind: "application",
        function: trace,
        argument: message,
      }),
      argument: add({ kind: "variable", index: 0n }),
    });
  } else if (blsFinal) {
    const blob = (bytes: Buffer) => {
      const root = hashMidgardCekBlobChunk(bytes);
      material.set(root.toString("hex"), {
        kind: "blobChunk",
        root,
        preimage: encodeMidgardCekBlobChunk(bytes),
      });
      return root;
    };
    const constant = (tag: number, hex: string) => {
      const bytes = Buffer.from(hex, "hex");
      const typeRoot = blob(Buffer.from([0x9f, tag, 0xff]));
      const node = {
        kind: "bytes" as const,
        bytesRoot: blob(bytes),
        bytesLength: BigInt(bytes.length),
        cborLength: midgardCekDataBytesCborLength(BigInt(bytes.length)),
        memory: midgardCekDataBytesMemory(BigInt(bytes.length)),
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
        payloadLength: node.cborLength,
        semanticRoot: dataRoot,
        memory: BigInt(bytes.length),
      };
      const root = hashMidgardCekValueNode(value);
      material.set(root.toString("hex"), {
        kind: "value",
        root,
        preimage: encodeMidgardCekValueNode(value),
      });
      return add({ kind: "constant", value: root });
    };
    const g1 = constant(
      9,
      "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
    );
    const g2 = constant(
      10,
      "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
    );
    const apply = (tag: bigint, left: Buffer, right: Buffer) =>
      add({
        kind: "application",
        function: add({
          kind: "application",
          function: add({ kind: "builtin", tag }),
          argument: left,
        }),
        argument: right,
      });
    const miller = apply(68n, g1, g2);
    const two = apply(69n, miller, miller);
    const three = apply(69n, two, miller);
    const five = apply(69n, three, two);
    termRoot = apply(70n, five, five);
  } else if (dataGraph || directBuiltin) {
    if (lambdaCount < 2 && !directBuiltin)
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
      function: directBuiltin
        ? add({
            kind: "application",
            function: add({ kind: "builtin", tag: 0n }),
            argument: constant,
          })
        : constant,
      argument: constant,
    });
  } else termRoot = add({ kind: "variable", index: 0n });
  for (
    let i = 0;
    i <
    (directBuiltin || blsFinal || semanticTag !== undefined ? 0 : lambdaCount);
    i++
  )
    termRoot = add({ kind: "lambda", body: termRoot });
  if (
    lambdaCount > 1 ||
    directBuiltin ||
    blsFinal ||
    semanticTag !== undefined
  ) {
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
