import { type MidgardCekDataNode } from "@al-ft/midgard-core";

import { type MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import { semanticizeMidgardCekDirectConstant } from "./cek-builtin.evaluate-reference-builtin.js";
import { encodeMidgardCekDataTreeInteger } from "./cek-data-tree.js";
import { type MidgardCekCoreStepWitness } from "./cek-machine.midgard-cek-core-step-witness.js";

/** The largest constrData index: an unsigned 64-bit integer. */
export const MIDGARD_CEK_MAX_CONSTR_INDEX = (1n << 64n) - 1n;

export const constrIndexIsOutOfRange = (index: bigint): boolean =>
  index < 0n || index > MIDGARD_CEK_MAX_CONSTR_INDEX;

/**
 * The failure witness of constrData with an index outside the unsigned 64-bit
 * range. The index is presented as a semantic constant: a magnitude of two or
 * more words is proved from its node memory, a smaller one from its revealed
 * leaf.
 */
export const constrIndexFailureWitness = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  index: bigint,
  indexNode: MidgardCekDataNode,
): MidgardCekCoreStepWitness => ({
  kind: "executeBuiltinSemanticFailure",
  tag: 37n,
  arguments: [
    semanticizeMidgardCekDirectConstant(arguments_[0]!),
    arguments_[1]!,
  ],
  material: {
    dataNodes: [indexNode],
    listNodes: [],
    pairNodes: [],
    scalarPreimages:
      (index < 0n ? -index : index) > MIDGARD_CEK_MAX_CONSTR_INDEX
        ? []
        : [encodeMidgardCekDataTreeInteger(index)],
  },
});
