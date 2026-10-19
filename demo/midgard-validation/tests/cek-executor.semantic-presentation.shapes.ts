import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  constT,
  Force,
  Lambda,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";

import { type MidgardCekDirectValueWitness } from "../src/cek-builtin.js";
import { encodeMidgardCekCoreStepDataCbor } from "../src/cek-data.js";
import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import {
  type MidgardCekCoreStepWitness,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

// A semantic builtin step (tags 29 to 50 except mapData and unMapData)
// presents each Data argument, each list or pair argument whose memory the
// verifier derives from its payload nodes, and each Data, list or pair result
// by its semantic summary. The summary has the value's root and memory, so
// the machine states are those of the revealed step; only the witness
// shrinks, and a near-maximum source no longer has to be revealed.

export type SemanticStepWitness = Extract<
  MidgardCekCoreStepWitness,
  {
    readonly kind: "executeBuiltinSemantic" | "executeBuiltinSemanticFailure";
  }
>;

type Kind = MidgardCekDirectValueWitness["kind"];

export const D = constT.data;
export const unit = UPLCConst.unit;
export const apply = (head: UPLCTerm, ...arguments_: UPLCTerm[]): UPLCTerm =>
  arguments_.reduce<UPLCTerm>(
    (term, argument) => new Application(term, argument),
    head,
  );
export const forced = (builtin: UPLCTerm, forces: number): UPLCTerm =>
  forces === 0 ? builtin : forced(new Force(builtin), forces - 1);
export const ints = (count: number): DataI[] =>
  Array.from({ length: count }, (_, index) => new DataI(BigInt(index % 23)));
export const data = (value: Data) => UPLCConst.data(value);
export const listData = (items: Data[]) => UPLCConst.listOf(D)(items);
export const pairData = (first: Data, second: Data) =>
  UPLCConst.pairOf(D, D)(first, second);

/** The program's semantic step, after checking that every step verifies. */
export const semanticStep = (term: UPLCTerm) => {
  const program = buildCekProgramFromRawFlat(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [1, 1, 0],
        new Application(new Lambda(UPLCConst.unit), term),
      ),
    ),
  );
  const execution = executeMidgardCekStructuralProgram({
    root: program.termRoot,
    material: program.material.values(),
    constantWitnesses: program.constantWitnesses,
    maxSteps: 64,
  });
  const step = execution.steps.find(
    (candidate) =>
      candidate.witness.kind === "executeBuiltinSemantic" ||
      candidate.witness.kind === "executeBuiltinSemanticFailure",
  );
  if (step === undefined) {
    throw new Error("the program has no semantic builtin step");
  }
  return {
    pre: step.pre,
    post: step.post,
    witness: step.witness as SemanticStepWitness,
    allVerify: execution.steps.every((candidate) =>
      verifyMidgardCekCoreStep(
        candidate.pre,
        candidate.post,
        candidate.witness,
      ),
    ),
    evidence: encodeMidgardCekCoreStepDataCbor(step),
  };
};

export type Shape = {
  readonly name: string;
  /** The label of the step's evidence in the Aiken presentation module. */
  readonly pinned?: string;
  readonly term: () => UPLCTerm;
  readonly arguments: readonly Kind[];
  readonly result: Kind | null;
};

/** One shape per presented position, with the kinds the executor emits. */
export const SHAPES: readonly Shape[] = [
  {
    name: "fstPair over pair<data, data>",
    term: () =>
      apply(
        forced(Builtin.fstPair, 2),
        pairData(new DataI(7n), new DataList(ints(2))),
      ),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "sndPair over pair<data, data>",
    pinned: "snd_pair_data",
    term: () =>
      apply(
        forced(Builtin.sndPair, 2),
        pairData(new DataI(7n), new DataList(ints(2))),
      ),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "fstPair over pair<string, data>",
    pinned: "fst_pair_string_data",
    term: () =>
      apply(
        forced(Builtin.fstPair, 2),
        UPLCConst.pairOf(constT.str, D)(
          "ab",
          new DataB(Buffer.from("00", "hex")),
        ),
      ),
    arguments: ["semanticConstant"],
    result: "constant",
  },
  {
    name: "sndPair over pair<integer, list<data>>",
    term: () =>
      apply(
        forced(Builtin.sndPair, 2),
        UPLCConst.pairOf(constT.int, constT.listOf(D))(3n, [new DataI(1n)]),
      ),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "sndPair over a nested-list pair",
    term: () =>
      apply(
        forced(Builtin.sndPair, 2),
        UPLCConst.pairOf(constT.listOf(constT.listOf(D)), constT.listOf(D))(
          [[new DataI(1n)]],
          [new DataI(2n)],
        ),
      ),
    arguments: ["constant"],
    result: "semanticConstant",
  },
  {
    name: "chooseList over list<data>",
    pinned: "choose_list_data",
    term: () =>
      apply(
        forced(Builtin.chooseList, 2),
        listData([new DataI(1n)]),
        unit,
        unit,
      ),
    arguments: ["semanticConstant", "opaque", "opaque"],
    result: "opaque",
  },
  {
    name: "mkCons of a Data item",
    pinned: "mk_cons_data",
    term: () =>
      apply(forced(Builtin.mkCons, 1), data(new DataI(0n)), listData(ints(2))),
    arguments: ["semanticConstant", "semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "mkCons of a pair<data, data> item",
    term: () =>
      apply(
        forced(Builtin.mkCons, 1),
        pairData(new DataI(1n), new DataI(2n)),
        UPLCConst.listOf(constT.pairOf(D, D))([
          { fst: new DataI(3n), snd: new DataI(4n) },
        ] as never),
      ),
    arguments: ["semanticConstant", "semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "mkCons of an integer item",
    pinned: "mk_cons_int",
    term: () =>
      apply(
        forced(Builtin.mkCons, 1),
        UPLCConst.int(0n),
        UPLCConst.listOf(constT.int)([1n, 2n] as never),
      ),
    arguments: ["constant", "semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "headList over list<data>",
    pinned: "head_list_data",
    term: () => apply(forced(Builtin.headList, 1), listData(ints(3))),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "headList over another list<data> with the same head",
    pinned: "head_list_data_same_head",
    term: () =>
      apply(
        forced(Builtin.headList, 1),
        listData([new DataI(0n), new DataI(5n)]),
      ),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "headList over list<integer>",
    term: () =>
      apply(
        forced(Builtin.headList, 1),
        UPLCConst.listOf(constT.int)([5n, 6n] as never),
      ),
    arguments: ["semanticConstant"],
    result: "constant",
  },
  {
    name: "headList over list<pair<data, data>>",
    term: () =>
      apply(
        forced(Builtin.headList, 1),
        UPLCConst.listOf(constT.pairOf(D, D))([
          { fst: new DataI(1n), snd: new DataI(2n) },
        ] as never),
      ),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "headList over list<string>",
    term: () =>
      apply(
        forced(Builtin.headList, 1),
        UPLCConst.listOf(constT.str)(["a", "b"] as never),
      ),
    arguments: ["constant"],
    result: "constant",
  },
  {
    name: "tailList over list<data>",
    term: () => apply(forced(Builtin.tailList, 1), listData(ints(3))),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "tailList over list<integer>",
    pinned: "tail_list_int",
    term: () =>
      apply(
        forced(Builtin.tailList, 1),
        UPLCConst.listOf(constT.int)([5n, 6n] as never),
      ),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "tailList over list<list<data>>",
    term: () =>
      apply(
        forced(Builtin.tailList, 1),
        UPLCConst.listOf(constT.listOf(D))([
          [new DataI(1n)],
          [new DataI(2n)],
        ] as never),
      ),
    arguments: ["constant"],
    result: "semanticConstant",
  },
  {
    name: "nullList over list<data>",
    pinned: "null_list_data",
    term: () => apply(forced(Builtin.nullList, 1), listData(ints(1))),
    arguments: ["semanticConstant"],
    result: "constant",
  },
  {
    name: "chooseData over a list",
    pinned: "choose_data",
    term: () =>
      apply(
        forced(Builtin.chooseData, 1),
        data(new DataList(ints(1))),
        unit,
        unit,
        unit,
        unit,
        unit,
      ),
    arguments: [
      "semanticConstant",
      "opaque",
      "opaque",
      "opaque",
      "opaque",
      "opaque",
    ],
    result: "opaque",
  },
  {
    name: "constrData",
    pinned: "constr_data",
    term: () => apply(Builtin.constrData, UPLCConst.int(3n), listData(ints(1))),
    arguments: ["constant", "semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "listData",
    pinned: "list_data",
    term: () =>
      apply(
        Builtin.listData,
        listData([new DataI(1n), new DataB(Buffer.from("01", "hex"))]),
      ),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "iData",
    pinned: "i_data",
    term: () => apply(Builtin.iData, UPLCConst.int(5n)),
    arguments: ["constant"],
    result: "semanticConstant",
  },
  {
    name: "bData",
    term: () =>
      apply(Builtin.bData, UPLCConst.byteString(Buffer.from("0102", "hex"))),
    arguments: ["constant"],
    result: "semanticConstant",
  },
  {
    name: "unConstrData",
    pinned: "un_constr_data",
    term: () =>
      apply(Builtin.unConstrData, data(new DataConstr(2n, [new DataI(1n)]))),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "unListData",
    pinned: "un_list_data",
    term: () => apply(Builtin.unListData, data(new DataList(ints(2)))),
    arguments: ["semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "unIData",
    term: () => apply(Builtin.unIData, data(new DataI(9n))),
    arguments: ["semanticConstant"],
    result: "constant",
  },
  {
    name: "unBData",
    pinned: "un_b_data",
    term: () =>
      apply(Builtin.unBData, data(new DataB(Buffer.from("aa", "hex")))),
    arguments: ["semanticConstant"],
    result: "constant",
  },
  {
    name: "equalsData",
    pinned: "equals_data_true",
    term: () =>
      apply(
        Builtin.equalsData,
        data(new DataList(ints(1))),
        data(new DataList(ints(1))),
      ),
    arguments: ["semanticConstant", "semanticConstant"],
    result: "constant",
  },
  {
    name: "mkPairData",
    pinned: "mk_pair_data",
    term: () =>
      apply(
        Builtin.mkPairData,
        data(new DataI(1n)),
        data(new DataB(Buffer.from("01", "hex"))),
      ),
    arguments: ["semanticConstant", "semanticConstant"],
    result: "semanticConstant",
  },
  {
    name: "mkNilData",
    pinned: "mk_nil_data",
    term: () => apply(Builtin.mkNilData, unit),
    arguments: ["constant"],
    result: "semanticConstant",
  },
  {
    name: "mkNilPairData",
    term: () => apply(Builtin.mkNilPairData, unit),
    arguments: ["constant"],
    result: "semanticConstant",
  },
  {
    name: "headList over an empty list",
    pinned: "head_list_empty",
    term: () => apply(forced(Builtin.headList, 1), listData([])),
    arguments: ["semanticConstant"],
    result: null,
  },
  {
    name: "tailList over an empty list",
    term: () => apply(forced(Builtin.tailList, 1), listData([])),
    arguments: ["semanticConstant"],
    result: null,
  },
  {
    name: "unConstrData over an integer",
    term: () => apply(Builtin.unConstrData, data(new DataI(1n))),
    arguments: ["semanticConstant"],
    result: null,
  },
  {
    name: "unListData over an integer",
    term: () => apply(Builtin.unListData, data(new DataI(1n))),
    arguments: ["semanticConstant"],
    result: null,
  },
  {
    name: "unIData over bytes",
    term: () => apply(Builtin.unIData, data(new DataB(Buffer.alloc(0)))),
    arguments: ["semanticConstant"],
    result: null,
  },
  {
    name: "unBData over an integer",
    term: () => apply(Builtin.unBData, data(new DataI(1n))),
    arguments: ["semanticConstant"],
    result: null,
  },
];
