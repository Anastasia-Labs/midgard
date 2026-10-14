import {
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
} from "@al-ft/midgard-core";

import { type MidgardCekDirectValueWitness } from "./cek-builtin.js";
import { type MidgardCekConstantType } from "./cek-constant.js";
import { type MidgardCekDataNodeSummary } from "./cek-data-tree.js";
import { type Bytes } from "./cek-executor.build-midgard-cek-execution-graph.js";
import { type MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";

/** The authenticated Data material a semantic builtin step reveals. */
export type MidgardCekSemanticStepMaterial = {
  readonly dataNodes: readonly MidgardCekDataNode[];
  readonly listNodes: readonly MidgardCekDataListNode[];
  readonly pairNodes: readonly MidgardCekDataPairNode[];
  readonly scalarPreimages: readonly Bytes[];
};

export type MidgardCekSemanticBuiltinOutcome =
  | {
      readonly kind: "result";
      readonly result: MidgardCekDirectValueWitness;
      /** The result's value root when this step created it. */
      readonly root: Bytes | undefined;
      readonly material: MidgardCekSemanticStepMaterial;
    }
  | {
      readonly kind: "failure";
      readonly material: MidgardCekSemanticStepMaterial;
    };

/** A constant argument read through the store. */
export type MidgardCekSemanticArgument = {
  readonly type: MidgardCekConstantType;
  readonly root: Bytes;
  readonly node: MidgardCekDataNode;
};

export const MIDGARD_CEK_SEMANTIC_EMPTY_MATERIAL: MidgardCekSemanticStepMaterial =
  Object.freeze({
    dataNodes: [],
    listNodes: [],
    pairNodes: [],
    scalarPreimages: [],
  });

export const MIDGARD_CEK_DATA_TYPE: MidgardCekConstantType = Object.freeze({
  kind: "data",
});
export const MIDGARD_CEK_INTEGER_TYPE: MidgardCekConstantType = Object.freeze({
  kind: "integer",
});
export const MIDGARD_CEK_LIST_DATA_TYPE: MidgardCekConstantType = Object.freeze(
  { kind: "list", element: MIDGARD_CEK_DATA_TYPE },
);
export const MIDGARD_CEK_PAIR_DATA_TYPE: MidgardCekConstantType = Object.freeze(
  {
    kind: "pair",
    first: MIDGARD_CEK_DATA_TYPE,
    second: MIDGARD_CEK_DATA_TYPE,
  },
);

/** The summary a node is linked under. */
export const midgardCekNodeSummary = (
  root: Bytes,
  node: MidgardCekDataNode,
): MidgardCekDataNodeSummary => ({
  root,
  cborLength: node.cborLength,
  memory: node.memory,
});

/**
 * The top node of a constant's payload with its first list or map link, the
 * material a step over that constant's shape reveals.
 */
export const midgardCekSemanticTopMaterial = (
  store: MidgardCekSemanticStore,
  root: Bytes,
): MidgardCekSemanticStepMaterial => {
  const node = store.node(root);
  const listRoot =
    node.kind === "constrSmall" || node.kind === "constrLarge"
      ? node.fieldsCount > 0n
        ? node.fieldsRoot
        : null
      : node.kind === "list" && node.itemsCount > 0n
        ? node.itemsRoot
        : null;
  const pairRoot =
    node.kind === "map" && node.entriesCount > 0n ? node.entriesRoot : null;
  return {
    dataNodes: [node],
    listNodes: listRoot === null ? [] : [store.link(listRoot)],
    pairNodes: pairRoot === null ? [] : [store.pairLink(pairRoot)],
    scalarPreimages: [],
  };
};

/** What one semantic builtin step reads and how it reports its outcome. */
export type MidgardCekSemanticStep = {
  readonly store: MidgardCekSemanticStore;
  readonly arguments_: readonly MidgardCekDirectValueWitness[];
  readonly resolve: (index: number) => MidgardCekSemanticArgument;
  readonly failure: (
    source: MidgardCekSemanticArgument,
  ) => MidgardCekSemanticBuiltinOutcome;
  /** The constant of `type` at `dataRoot`, with its splice when known. */
  readonly built: (
    type: MidgardCekConstantType,
    dataRoot: Bytes,
    material: MidgardCekSemanticStepMaterial,
    bytes?: () => Buffer | undefined,
  ) => MidgardCekSemanticBuiltinOutcome;
  /** An argument returned unchanged. */
  readonly selected: (
    result: MidgardCekDirectValueWitness,
    material: MidgardCekSemanticStepMaterial,
  ) => MidgardCekSemanticBuiltinOutcome;
  readonly cached: (root: Bytes) => Buffer | undefined;
};

export const midgardCekSemanticStep = (
  store: MidgardCekSemanticStore,
  arguments_: readonly MidgardCekDirectValueWitness[],
  roots: readonly Bytes[],
): MidgardCekSemanticStep => ({
  store,
  arguments_,
  resolve: (index) => {
    const info = store.constant(roots[index]!, arguments_[index]!);
    return {
      type: info.type,
      root: info.dataRoot,
      node: store.node(info.dataRoot),
    };
  },
  failure: (source) => ({
    kind: "failure",
    material: midgardCekSemanticTopMaterial(store, source.root),
  }),
  built: (type, dataRoot, material, bytes) => {
    const added = store.addConstant(type, dataRoot, bytes);
    return {
      kind: "result",
      result: added.result,
      root: added.root,
      material,
    };
  },
  selected: (result, material) => ({
    kind: "result",
    result,
    root: undefined,
    material,
  }),
  cached: (root) => store.cachedBytes(root),
});
