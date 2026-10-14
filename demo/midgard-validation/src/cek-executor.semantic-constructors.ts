import {
  buildMidgardCekDataConstrNode,
  buildMidgardCekDataIntegerNode,
  buildMidgardCekDataListLink,
  buildMidgardCekDataListNode,
  encodeMidgardCekDataTreeInteger,
  midgardCekDataSequenceSummary,
} from "./cek-data-tree.js";
import {
  spliceMidgardCekConstr,
  spliceMidgardCekListCons,
  spliceMidgardCekPair,
  spliceMidgardCekUnConstr,
} from "./cek-executor.semantic-bytes.js";
import {
  MIDGARD_CEK_DATA_TYPE,
  MIDGARD_CEK_INTEGER_TYPE,
  MIDGARD_CEK_LIST_DATA_TYPE,
  MIDGARD_CEK_PAIR_DATA_TYPE,
  MIDGARD_CEK_SEMANTIC_EMPTY_MATERIAL,
  midgardCekNodeSummary,
  type MidgardCekSemanticArgument,
  type MidgardCekSemanticBuiltinOutcome,
  type MidgardCekSemanticStep,
  type MidgardCekSemanticStepMaterial,
  midgardCekSemanticTopMaterial,
} from "./cek-executor.semantic-step.js";
import { sameConstantType } from "./cek-machine.verify-semantic-builtin-control.js";

/**
 * The structural builtins that build a new Data spine from their arguments:
 * each creates only its new links and top node and shares every argument
 * node below them.
 */

const UNCONSTR = Object.freeze({
  kind: "pair",
  first: MIDGARD_CEK_INTEGER_TYPE,
  second: MIDGARD_CEK_LIST_DATA_TYPE,
} as const);

const EMPTY = new Uint8Array(0);

/** The two-link spine `[first, second]`, returned as its summary. */
const pairSpine = (
  step: MidgardCekSemanticStep,
  first: MidgardCekSemanticArgument,
  second: MidgardCekSemanticArgument,
) => {
  const { store } = step;
  const secondLink = buildMidgardCekDataListLink(
    midgardCekNodeSummary(second.root, second.node),
    store.items(EMPTY, 0n),
  );
  const secondRoot = store.addListLink(secondLink);
  const firstLink = buildMidgardCekDataListLink(
    midgardCekNodeSummary(first.root, first.node),
    midgardCekDataSequenceSummary(secondRoot, secondLink),
  );
  return midgardCekDataSequenceSummary(store.addListLink(firstLink), firstLink);
};

/** `mkCons item list` (tag 32) over a list argument already read. */
export const midgardCekSemanticMkCons = (
  step: MidgardCekSemanticStep,
  source: MidgardCekSemanticArgument,
  top: MidgardCekSemanticStepMaterial,
): MidgardCekSemanticBuiltinOutcome => {
  const { store } = step;
  if (source.node.kind !== "list") {
    throw new Error("CEK semantic list builtin requires a list");
  }
  const item = step.resolve(0);
  if (
    source.type.kind !== "list" ||
    !sameConstantType(item.type, source.type.element)
  ) {
    throw new Error("CEK semantic mkCons requires an item of the list type");
  }
  const link = buildMidgardCekDataListLink(
    midgardCekNodeSummary(item.root, item.node),
    store.items(source.node.itemsRoot, source.node.itemsCount),
  );
  const resultRoot = store.addDataNode(
    buildMidgardCekDataListNode(
      midgardCekDataSequenceSummary(store.addListLink(link), link),
    ),
  );
  return step.built(source.type, resultRoot, top, () => {
    const head = step.cached(item.root);
    const tail = step.cached(source.root);
    return head === undefined || tail === undefined
      ? undefined
      : spliceMidgardCekListCons(head, tail);
  });
};

/** `constrData index fields` (tag 37). */
export const midgardCekSemanticConstrData = (
  step: MidgardCekSemanticStep,
): MidgardCekSemanticBuiltinOutcome => {
  const { store } = step;
  const index = step.resolve(0);
  const fields = step.resolve(1);
  if (
    index.type.kind !== "integer" ||
    index.node.kind !== "integer" ||
    fields.type.kind !== "list" ||
    fields.type.element.kind !== "data" ||
    fields.node.kind !== "list"
  ) {
    throw new Error("constrData requires non-negative index and Data list");
  }
  const constructor = store.integer(index.root);
  if (constructor < 0n) {
    throw new Error("constrData requires non-negative index and Data list");
  }
  const fieldsTop = midgardCekSemanticTopMaterial(store, fields.root);
  const indexIsSemantic = step.arguments_[0]?.kind === "semanticConstant";
  const fieldsList = fields.node;
  return step.built(
    MIDGARD_CEK_DATA_TYPE,
    store.addDataNode(
      buildMidgardCekDataConstrNode(
        constructor,
        store.items(fieldsList.itemsRoot, fieldsList.itemsCount),
        store.addBlob,
      ),
    ),
    {
      dataNodes: indexIsSemantic ? [index.node, fields.node] : [fields.node],
      listNodes: fieldsTop.listNodes,
      pairNodes: [],
      scalarPreimages: indexIsSemantic
        ? [encodeMidgardCekDataTreeInteger(constructor)]
        : [],
    },
    () => {
      const bytes = step.cached(fields.root);
      return bytes === undefined
        ? undefined
        : spliceMidgardCekConstr(constructor, bytes);
    },
  );
};

/** `unConstrData` (tag 42). */
export const midgardCekSemanticUnConstrData = (
  step: MidgardCekSemanticStep,
): MidgardCekSemanticBuiltinOutcome => {
  const { store } = step;
  const source = step.resolve(0);
  const constr = source.node;
  if (
    source.type.kind !== "data" ||
    (constr.kind !== "constrSmall" && constr.kind !== "constrLarge")
  ) {
    throw new Error("unConstrData requires constructor Data");
  }
  const constructor = store.constructorOf(constr);
  const top = midgardCekSemanticTopMaterial(store, source.root);
  const indexNode = buildMidgardCekDataIntegerNode(constructor, store.addBlob);
  const fieldsNode = buildMidgardCekDataListNode(
    store.items(constr.fieldsRoot, constr.fieldsCount),
  );
  const spine = pairSpine(
    step,
    {
      type: MIDGARD_CEK_INTEGER_TYPE,
      root: store.addDataNode(indexNode),
      node: indexNode,
    },
    {
      type: MIDGARD_CEK_LIST_DATA_TYPE,
      root: store.addDataNode(fieldsNode),
      node: fieldsNode,
    },
  );
  return step.built(
    UNCONSTR,
    store.addDataNode(buildMidgardCekDataConstrNode(0n, spine, store.addBlob)),
    {
      dataNodes: top.dataNodes,
      listNodes: top.listNodes,
      pairNodes: [],
      scalarPreimages:
        constructor <= 127n
          ? []
          : [encodeMidgardCekDataTreeInteger(constructor)],
    },
    () => {
      const bytes = step.cached(source.root);
      return bytes === undefined
        ? undefined
        : spliceMidgardCekUnConstr(bytes, constructor);
    },
  );
};

/** `mkPairData first second` (tag 48). */
export const midgardCekSemanticMkPairData = (
  step: MidgardCekSemanticStep,
): MidgardCekSemanticBuiltinOutcome => {
  const { store } = step;
  const first = step.resolve(0);
  const second = step.resolve(1);
  if (first.type.kind !== "data" || second.type.kind !== "data") {
    throw new Error("mkPairData requires Data arguments");
  }
  const spine = pairSpine(step, first, second);
  return step.built(
    MIDGARD_CEK_PAIR_DATA_TYPE,
    store.addDataNode(buildMidgardCekDataConstrNode(0n, spine, store.addBlob)),
    MIDGARD_CEK_SEMANTIC_EMPTY_MATERIAL,
    () => {
      const key = step.cached(first.root);
      const value = step.cached(second.root);
      return key === undefined || value === undefined
        ? undefined
        : spliceMidgardCekPair(key, value);
    },
  );
};
