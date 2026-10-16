import {
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekContinuationFrame,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";

import {
  type MidgardCekConstantValueWitness,
  type MidgardCekDirectValueWitness,
} from "./cek-builtin.js";
import {
  midgardCekConstantMemorySize,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import type { commitMidgardCekDataTree } from "./cek-data-tree.js";
import {
  type Bytes,
  errorSuccessor,
  exactState,
  type SequenceNode,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import {
  type MidgardCekCoreStepWitness,
  MidgardCekErrorCodes,
  type MidgardCekSemanticBuiltinWitness,
} from "./cek-machine.js";

type DataTree = ReturnType<typeof commitMidgardCekDataTree>;

/** The executor state a `case` on a constant reads and extends. */
export type CaseConstantHost = {
  readonly constant: (
    root: Bytes,
  ) => MidgardCekConstantValueWitness | undefined;
  readonly resolve: (value: MidgardCekDirectValueWitness) => {
    readonly type: MidgardCekConstantType;
    readonly payload: Data;
    readonly tree: DataTree;
  };
  readonly top: (tree: DataTree) => {
    readonly node: MidgardCekDataNode;
    readonly lists: readonly MidgardCekDataListNode[];
    readonly pairs: readonly MidgardCekDataPairNode[];
  };
  readonly dataNode: (root: Bytes) => MidgardCekDataNode | undefined;
  readonly dataList: (root: Bytes) => MidgardCekDataListNode | undefined;
  readonly addResult: (type: MidgardCekConstantType, payload: Data) => Bytes;
  readonly addSequence: (node: SequenceNode) => Bytes;
  readonly addContinuation: (frame: MidgardCekContinuationFrame) => Bytes;
  readonly record: (
    witness: MidgardCekCoreStepWitness,
    post: MidgardCekMachineState,
  ) => void;
};

type Selected =
  | {
      readonly tag: bigint;
      readonly values: readonly (readonly [MidgardCekConstantType, Data])[];
    }
  | "missing"
  | "unsupported";

const EMPTY_MATERIAL: MidgardCekSemanticBuiltinWitness = Object.freeze({
  dataNodes: [],
  listNodes: [],
  pairNodes: [],
  scalarPreimages: [],
});

/** Cardano's `caseBuiltinNoData`: the branch and the values it receives. */
const select = (
  type: MidgardCekConstantType,
  payload: Data,
  count: bigint,
): Selected => {
  switch (type.kind) {
    case "unit":
      return count === 1n ? { tag: 0n, values: [] } : "missing";
    case "boolean":
      if (payload instanceof DataConstr && payload.constr === 1n) {
        return count === 2n ? { tag: 1n, values: [] } : "missing";
      }
      return count === 1n || count === 2n ? { tag: 0n, values: [] } : "missing";
    case "integer": {
      const value = payload instanceof DataI ? payload.int : -1n;
      return value >= 0n && value < count
        ? { tag: value, values: [] }
        : "missing";
    }
    case "list": {
      if (!(payload instanceof DataList)) return "missing";
      const [head, ...tail] = payload.list;
      if (head === undefined) {
        return count === 2n ? { tag: 1n, values: [] } : "missing";
      }
      return count === 1n || count === 2n
        ? {
            tag: 0n,
            values: [
              [type.element, head],
              [type, new DataList(tail)],
            ],
          }
        : "missing";
    }
    case "pair": {
      if (!(payload instanceof DataConstr) || count !== 1n) return "missing";
      const [first, second] = payload.fields;
      return {
        tag: 0n,
        values: [
          [type.first, first!],
          [type.second, second!],
        ],
      };
    }
    case "string":
    case "bytes":
    case "data":
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
      return "unsupported";
  }
};

/**
 * The list or pair nodes a semantic scrutinee's values are read from: the
 * same material `headList`/`tailList` and `fstPair`/`sndPair` reveal.
 */
const semanticMaterial = (
  host: CaseConstantHost,
  type: MidgardCekConstantType,
  tree: DataTree,
): MidgardCekSemanticBuiltinWitness => {
  const top = host.top(tree);
  const firstLink = top.lists[0];
  if (firstLink === undefined) {
    throw new Error("CEK case scrutinee is missing its first list link");
  }
  const firstNode = host.dataNode(firstLink.head);
  const secondLink =
    type.kind === "list" && firstLink.length === 1n
      ? undefined
      : host.dataList(firstLink.tail);
  if (firstNode === undefined) {
    throw new Error("CEK case scrutinee material is incomplete");
  }
  if (type.kind === "list") {
    return {
      dataNodes: [top.node, firstNode],
      listNodes:
        secondLink === undefined ? [firstLink] : [firstLink, secondLink],
      pairNodes: [],
      scalarPreimages: [],
    };
  }
  const secondNode =
    secondLink === undefined ? undefined : host.dataNode(secondLink.head);
  if (secondLink === undefined || secondNode === undefined) {
    throw new Error("CEK case scrutinee pair material is incomplete");
  }
  return {
    dataNodes: [top.node, firstNode, secondNode],
    listNodes: [firstLink, secondLink],
    pairNodes: [],
    scalarPreimages: [],
  };
};

/**
 * The shortest list of integers, bytestrings, booleans or units whose case
 * step is proved in its committed form. The committed step reads two links
 * and the head whatever the list's length, about 1.35M memory units; the
 * inline step rereads every item, about 150k to 180k more per item. At three
 * items the committed form is the cheaper one for booleans and units but not
 * yet for integers and bytestrings; from four on it is the cheaper one for all
 * four. The inline step is out of budget at about 76 one-word items.
 */
export const COMMITTED_LIST_CASE_MIN_LENGTH = 4n;

/** Element types whose committed list the case-constant arm can read. */
const COMMITTED_LIST_ELEMENTS: ReadonlySet<MidgardCekConstantType["kind"]> =
  new Set(["integer", "bytes", "boolean", "unit"]);

/**
 * Whether a case on this inline constant is proved in its committed form: an
 * integer of two or more words, which is past every branch index from its
 * memory alone, or a long enough list of committed-readable items.
 */
const committedScrutinee = (
  source: { readonly type: MidgardCekConstantType; readonly payload: Data },
  memory: bigint,
): boolean => {
  if (source.type.kind === "integer") return memory >= 2n;
  return (
    source.type.kind === "list" &&
    COMMITTED_LIST_ELEMENTS.has(source.type.element.kind) &&
    source.payload instanceof DataList &&
    BigInt(source.payload.list.length) >= COMMITTED_LIST_CASE_MIN_LENGTH
  );
};

/** Returns a constant into a case frame: one `returnCaseConstant` step. */
export const returnCaseConstant = (
  host: CaseConstantHost,
  pre: MidgardCekMachineState,
  frame: Extract<MidgardCekContinuationFrame, { kind: "case" }>,
): void => {
  const stored = host.constant(pre.focusRoot);
  if (stored === undefined) {
    throw new Error("CEK case scrutinee constant has no witness");
  }
  const source = host.resolve(stored);
  const memory = midgardCekConstantMemorySize(source.type, source.payload);
  const scrutinee: MidgardCekDirectValueWitness =
    stored.kind === "constant" && committedScrutinee(source, memory)
      ? {
          kind: "semanticConstant",
          witness: {
            typeCbor: stored.witness.typeCbor,
            payload: {
              root: source.tree.root,
              cborLength: source.tree.cborLength,
              memory: source.tree.memory,
            },
            memory,
          },
        }
      : stored;
  const selected = select(source.type, source.payload, frame.branchesCount);
  const witness = {
    kind: "returnCaseConstant",
    scrutinee,
    material:
      scrutinee.kind === "semanticConstant" &&
      typeof selected !== "string" &&
      selected.values.length > 0
        ? semanticMaterial(host, source.type, source.tree)
        : EMPTY_MATERIAL,
    branchesCount: frame.branchesCount,
    branchesRoot: frame.branchesRoot,
    capturedEnvironment: frame.environment,
    tail: frame.tail,
  } as const;
  if (typeof selected === "string") {
    host.record(
      witness,
      errorSuccessor(
        pre,
        selected === "missing"
          ? MidgardCekErrorCodes.CaseBranchMissing
          : MidgardCekErrorCodes.InvalidCaseScrutinee,
      ),
    );
    return;
  }
  // Values are pushed so that the first is applied first, as a
  // constructor's fields are.
  let valuesRoot: Bytes = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT;
  selected.values.forEach(([type, payload], index) => {
    valuesRoot = host.addSequence({
      head: Buffer.from(host.addResult(type, payload)),
      tail: Buffer.from(valuesRoot),
      length: BigInt(index + 1),
    });
  });
  host.record(
    witness,
    exactState(pre, {
      mode: "caseSelect",
      focusRoot: frame.branchesRoot,
      environmentRoot: valuesRoot,
      continuationRoot: host.addContinuation({
        kind: "caseSelect",
        environment: frame.environment,
        tail: frame.tail,
        valuesCount: BigInt(selected.values.length),
      }),
      auxiliary: selected.tag,
    }),
  );
};
