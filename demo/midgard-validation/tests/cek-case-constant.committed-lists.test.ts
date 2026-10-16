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
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "../src/cek-executor.js";
import {
  type MidgardCekCoreStepWitness,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
import {
  bytes,
  constCase,
  integers,
} from "./fixtures/cek-case-constant-v1.corpus.js";

type CaseConstantWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "returnCaseConstant" }
>;

const caseConstantStep = (term: UPLCTerm) => {
  const program = buildMidgardCanonicalCekProgram(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram([1, 1, 0], new Lambda(term)),
    ),
  );
  const graph = buildMidgardCekExecutionGraph(
    program.envelope,
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
    "100 one-byte bytestrings",
    () =>
      constCase(byteStrings(Array.from({ length: 100 }, () => 1)), toUnit()),
  ],
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
