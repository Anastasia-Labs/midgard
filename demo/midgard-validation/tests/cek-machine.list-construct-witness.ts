import { decodeMidgardCekConstantWitness } from "../src/cek-constant.js";
import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";
import { type MidgardCekCoreStepWitness } from "../src/cek-machine.js";

export type SemanticListConstructWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinSemantic" }
>;

const rootHex = (root: Uint8Array): string => Buffer.from(root).toString("hex");

/**
 * A mkCons step's semantic witness. The executor emits the direct witness over
 * an element type the split list hop can't read; its semantic form carries the
 * same values with the source's top list node and first link as material.
 */
export const listConstructSemanticWitness = (
  witness: MidgardCekCoreStepWitness,
): SemanticListConstructWitness => {
  if (witness.kind === "executeBuiltinSemantic") return witness;
  if (witness.kind !== "executeBuiltinDirect" || witness.tag !== 32n) {
    throw new Error("expected a mkCons builtin step");
  }
  const source = witness.arguments[1]!;
  if (source.kind !== "constant") throw new Error("expected a revealed source");
  const tree = commitMidgardCekDataTree(
    decodeMidgardCekConstantWitness(source.witness).payload,
  );
  const node = tree.dataNodes.get(rootHex(tree.root))!.node;
  if (node.kind !== "list") throw new Error("expected a list node");
  return {
    kind: "executeBuiltinSemantic",
    tag: witness.tag,
    arguments: witness.arguments,
    result: witness.result,
    material: {
      dataNodes: [node],
      listNodes:
        node.itemsCount === 0n
          ? []
          : [tree.listNodes.get(rootHex(node.itemsRoot))!.node],
      pairNodes: [],
      scalarPreimages: [],
    },
  };
};
