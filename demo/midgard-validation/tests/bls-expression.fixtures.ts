/** BLS expression fixtures shared by the BLS differential and depth tests. */
import { hashMidgardCekBlsExpressionNode } from "@al-ft/midgard-core";

import {
  evaluateMidgardCekDirectBuiltin,
  type MidgardCekBlsExpressionWitness,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import {
  hashMidgardCekConstantWitness,
  type MidgardCekConstantWitness,
} from "../src/cek-constant.js";

export const G1: MidgardCekConstantWitness = {
  typeCbor: Buffer.from("9f09ff", "hex"),
  payloadCbor: Buffer.from(
    "583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
    "hex",
  ),
};
export const G2: MidgardCekConstantWitness = {
  typeCbor: Buffer.from("9f0aff", "hex"),
  payloadCbor: Buffer.from(
    "5f584093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc510515820c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8ff",
    "hex",
  ),
};

/** `a + b` through the pinned reference evaluator (54 = G1 add, 61 = G2 add). */
const add = (
  tag: bigint,
  a: MidgardCekConstantWitness,
  b: MidgardCekConstantWitness,
): MidgardCekConstantWitness => {
  const sum = evaluateMidgardCekDirectBuiltin(tag, [
    { kind: "constant", witness: a },
    { kind: "constant", witness: b },
  ] satisfies MidgardCekDirectValueWitness[]);
  if (sum.kind !== "success" || sum.result.kind !== "constant") {
    throw new Error("fixture point addition failed");
  }
  return sum.result.witness;
};

export const G1_POINTS = [G1, add(54n, G1, G1)];
export const G2_POINTS = [G2, add(61n, G2, G2)];

export const leafRoot = (
  leaf: Extract<MidgardCekBlsExpressionWitness, { kind: "millerLoop" }>,
): Buffer =>
  Buffer.from(
    hashMidgardCekBlsExpressionNode({
      kind: "millerLoop",
      g1Value: hashMidgardCekConstantWitness(leaf.g1),
      g2Value: hashMidgardCekConstantWitness(leaf.g2),
    }),
  );

/** The expression root, walked with its own stack. */
export const expressionRoot = (
  expression: MidgardCekBlsExpressionWitness,
): Buffer => {
  const roots = new Map<MidgardCekBlsExpressionWitness, Buffer>();
  const work: [MidgardCekBlsExpressionWitness, boolean][] = [
    [expression, false],
  ];
  while (work.length > 0) {
    const [node, expanded] = work.pop()!;
    if (roots.has(node)) continue;
    if (node.kind === "millerLoop") {
      roots.set(node, leafRoot(node));
    } else if (expanded) {
      roots.set(
        node,
        Buffer.from(
          hashMidgardCekBlsExpressionNode({
            kind: "multiply",
            left: roots.get(node.left)!,
            right: roots.get(node.right)!,
          }),
        ),
      );
    } else {
      work.push([node, true], [node.right, false], [node.left, false]);
    }
  }
  return roots.get(expression)!;
};

/** `leaf * leaf * ... * leaf`, `levels` products deep down the left. */
export const blsLeftChain = (
  leaf: MidgardCekBlsExpressionWitness,
  levels: number,
): MidgardCekBlsExpressionWitness => {
  let chain = leaf;
  for (let level = 0; level < levels; level += 1) {
    chain = { kind: "multiply", left: chain, right: leaf };
  }
  return chain;
};
