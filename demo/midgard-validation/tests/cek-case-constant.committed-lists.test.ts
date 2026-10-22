import {
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
} from "@al-ft/midgard-core";
import { DataConstr } from "@harmoniclabs/plutus-data";
import {
  constT,
  Lambda,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekPlutusData,
  midgardCekConstantMemorySize,
} from "../src/cek-constant.js";
import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";
import { COMMITTED_LIST_CASE_MIN_LENGTH } from "../src/cek-executor.case-constant.js";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "../src/cek-executor.js";
import {
  type MidgardCekCoreStepWitness,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import {
  bytes,
  constCase,
  integers,
} from "./fixtures/cek-case-constant-v1.corpus.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

type CaseConstantWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "returnCaseConstant" }
>;

const caseConstantStep = (term: UPLCTerm) => {
  const program = buildCekProgramFromRawFlat(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram([1, 1, 0], new Lambda(term)),
    ),
  );
  const graph = buildMidgardCekExecutionGraph(
    program.termRoot,
    program.material.values(),
    encodeMidgardCekPlutusData(new DataConstr(0n, [])),
  );
  const trace = executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: 64,
  });
  const step = trace.steps.find(
    (candidate) => candidate.witness.kind === "returnCaseConstant",
  );
  if (step?.witness.kind !== "returnCaseConstant") {
    throw new Error("the program has no case-on-constant step");
  }
  return { ...step, witness: step.witness };
};

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

/**
 * The same scrutinee in its committed form: the root-equal semantic constant,
 * opened through its top node, its first one (list) or two (pair) item nodes
 * and its first two links.
 */
const committed = (witness: CaseConstantWitness): CaseConstantWitness => {
  if (witness.scrutinee.kind !== "constant") {
    throw new Error("the scrutinee is already committed");
  }
  const decoded = decodeMidgardCekConstantWitness(witness.scrutinee.witness);
  const tree = commitMidgardCekDataTree(decoded.payload);
  const node = (root: Uint8Array): MidgardCekDataNode => {
    const found = tree.dataNodes.get(hex(root))?.node;
    if (found === undefined) throw new Error("missing data node");
    return found;
  };
  const link = (root: Uint8Array): MidgardCekDataListNode => {
    const found = tree.listNodes.get(hex(root))?.node;
    if (found === undefined) throw new Error("missing list link");
    return found;
  };
  const top = node(tree.root);
  if (top.kind !== "list" && top.kind !== "constrSmall") {
    throw new Error("the scrutinee is not a list or a pair");
  }
  const first = link(top.kind === "list" ? top.itemsRoot : top.fieldsRoot);
  const second = first.length === 1n ? undefined : link(first.tail);
  return {
    ...witness,
    scrutinee: {
      kind: "semanticConstant",
      witness: {
        typeCbor: witness.scrutinee.witness.typeCbor,
        payload: {
          root: tree.root,
          cborLength: tree.cborLength,
          memory: tree.memory,
        },
        memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
      },
    },
    material: {
      dataNodes:
        top.kind === "list" || second === undefined
          ? [top, node(first.head)]
          : [top, node(first.head), node(second.head)],
      listNodes: second === undefined ? [first] : [first, second],
      pairNodes: [],
      scalarPreimages: [],
    },
  };
};

const byteStrings = (widths: readonly number[]) =>
  UPLCConst.listOf(constT.byteStr)(
    widths.map(
      (width, index) =>
        bytes((index % 256).toString(16).padStart(2, "0").repeat(width)).value,
    ) as never,
  );

// `(lam h (lam t (con unit ())))`
const toUnit = () => new Lambda(new Lambda(UPLCConst.unit));

const LISTS: readonly (readonly [string, () => UPLCTerm])[] = [
  ["integers", () => constCase(integers([1n, 300n, 1n << 70n]), toUnit())],
  [
    "bytestrings of one, two and three words",
    () => constCase(byteStrings([1, 9, 17]), toUnit()),
  ],
  [
    "booleans",
    () =>
      constCase(
        UPLCConst.listOf(constT.bool)([true, false, true] as never),
        toUnit(),
      ),
  ],
  [
    "units",
    () =>
      constCase(
        UPLCConst.listOf(constT.unit)([
          UPLCConst.unit.value,
          UPLCConst.unit.value,
        ] as never),
        toUnit(),
      ),
  ],
  [
    "a pair of a bytestring and a boolean",
    () =>
      constCase(
        UPLCConst.pairOf(constT.byteStr, constT.bool)(bytes("00").value, true),
        toUnit(),
      ),
  ],
];

describe("case on a committed list or pair of scalars", () => {
  it.each(LISTS)(
    "%s: the committed form proves the inline step's successor",
    (_, term) => {
      const { pre, post, witness } = caseConstantStep(term());
      expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
      const semantic = committed(witness);
      expect(verifyMidgardCekCoreStep(pre, post, semantic)).toBe(true);
      // The head node is bound to the list's first link.
      expect(
        verifyMidgardCekCoreStep(pre, post, {
          ...semantic,
          material: {
            ...semantic.material,
            dataNodes: [...semantic.material.dataNodes].reverse(),
          },
        }),
      ).toBe(false);
    },
  );
});

const repeat = <T>(length: number, item: (index: number) => T): T[] =>
  Array.from({ length }, (_, index) => item(index));

// Lists of each committed-readable item type, of a given length.
const ITEM_LISTS: readonly (readonly [string, (length: number) => UPLCTerm])[] =
  [
    [
      "integers",
      (length) => integers(repeat(length, (index) => BigInt(index))),
    ],
    ["bytestrings", (length) => byteStrings(repeat(length, () => 1))],
    [
      "booleans",
      (length) =>
        UPLCConst.listOf(constT.bool)(
          repeat(length, (index) => index % 2 === 0) as never,
        ),
    ],
    [
      "units",
      (length) =>
        UPLCConst.listOf(constT.unit)(
          repeat(length, () => UPLCConst.unit.value) as never,
        ),
    ],
  ];

const threshold = Number(COMMITTED_LIST_CASE_MIN_LENGTH);

describe("which form the executor proves a list case in", () => {
  it.each(ITEM_LISTS)(
    "%s: inline below the threshold length, committed from it",
    (_, list) => {
      const below = caseConstantStep(constCase(list(threshold - 1), toUnit()));
      expect(below.witness.scrutinee.kind).toBe("constant");
      expect(below.witness.material.dataNodes).toEqual([]);
      expect(
        verifyMidgardCekCoreStep(below.pre, below.post, below.witness),
      ).toBe(true);
      // The committed form of the same step also proves it.
      const semantic = committed(below.witness);
      expect(verifyMidgardCekCoreStep(below.pre, below.post, semantic)).toBe(
        true,
      );

      const at = caseConstantStep(constCase(list(threshold), toUnit()));
      expect(at.witness.scrutinee.kind).toBe("semanticConstant");
      expect(at.witness.material.listNodes).toHaveLength(2);
      expect(verifyMidgardCekCoreStep(at.pre, at.post, at.witness)).toBe(true);
    },
  );

  it.each(ITEM_LISTS)(
    "%s: a 100-item list is proved in its committed form",
    (_, list) => {
      const { pre, post, witness } = caseConstantStep(
        constCase(list(100), toUnit()),
      );
      expect(witness.scrutinee.kind).toBe("semanticConstant");
      expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
      // The head node is bound to the list's first link.
      expect(
        verifyMidgardCekCoreStep(pre, post, {
          ...witness,
          material: {
            ...witness.material,
            dataNodes: [...witness.material.dataNodes].reverse(),
          },
        }),
      ).toBe(false);
    },
  );

  it.each(ITEM_LISTS)(
    "%s: a committed list with three branches is missing its branch",
    (_, list) => {
      const { pre, post, witness } = caseConstantStep(
        constCase(list(threshold), toUnit(), toUnit(), toUnit()),
      );
      expect(witness.scrutinee.kind).toBe("semanticConstant");
      expect(post.mode).toBe("haltError");
      expect(post.auxiliary).toBe(6n);
      expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
    },
  );

  it.each([
    ["a wide integer head", () => integers([1n << 600n, 1n, 2n, 3n])],
    ["a wide bytestring head", () => byteStrings([200, 1, 1, 1])],
  ] as const)("%s is read from its committed node", (_, list) => {
    const { pre, post, witness } = caseConstantStep(
      constCase(list(), toUnit()),
    );
    expect(witness.scrutinee.kind).toBe("semanticConstant");
    expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
  });

  it.each([
    [
      "strings",
      () => UPLCConst.listOf(constT.str)(["a", "b", "c", "d"] as never),
    ],
    [
      "lists of integers",
      () =>
        UPLCConst.listOf(constT.listOf(constT.int))(
          repeat(threshold, () => [1n]) as never,
        ),
    ],
    [
      "pairs of integers",
      () =>
        UPLCConst.listOf(constT.pairOf(constT.int, constT.int))(
          repeat(threshold, () => ({ fst: 1n, snd: 2n })) as never,
        ),
    ],
  ] as const)("a list of %s stays inline", (_, list) => {
    const { pre, post, witness } = caseConstantStep(
      constCase(list(), toUnit()),
    );
    expect(witness.scrutinee.kind).toBe("constant");
    expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
  });
});
