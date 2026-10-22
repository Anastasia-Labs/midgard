/**
 * Further known answers for `case` on a constant: edges of each rule and wide
 * scrutinees that reach the case step as semantic constants. Each expected
 * result and budget was produced by `aiken uplc eval`, as described in
 * `cek-case-constant-v1.corpus.ts`.
 */

import { DataB, DataConstr, DataI, DataList } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  constT,
  Delay,
  Lambda,
  UPLCConst,
  UPLCVar,
} from "@harmoniclabs/uplc";

import {
  bool,
  BRANCH_MISSING,
  BYTES,
  bytes,
  CASE_CONSTANT_CONFORMANCE,
  type CaseConstantCorpusEntry,
  constCase,
  DATA,
  first,
  int,
  INTEGER,
  integer,
  integers,
  second,
  UNSUPPORTED,
  UPLC_EVAL,
} from "./cek-case-constant-v1.corpus.js";

const TWO_TO_THE_64 = 1n << 64n;

/** Further edges, each evaluated with the reference evaluator. */
const EDGES: readonly CaseConstantCorpusEntry[] = [
  {
    label: "integer-zero-one-branch",
    source: UPLC_EVAL,
    uplc: "(case (con integer 0) (con integer 7))",
    term: () => constCase(int(0), int(7)),
    expected: integer(7n, 48_100n, 400n),
  },
  {
    label: "integer-two-to-the-64",
    source: UPLC_EVAL,
    uplc: "(case (con integer 18446744073709551616) (con integer 1) (con integer 2) (con integer 3))",
    term: () => constCase(int(TWO_TO_THE_64), int(1), int(2), int(3)),
    expected: BRANCH_MISSING,
  },
  {
    label: "integer-minus-two-to-the-64",
    source: UPLC_EVAL,
    uplc: "(case (con integer -18446744073709551616) (con integer 1))",
    term: () => constCase(int(-TWO_TO_THE_64), int(1)),
    expected: BRANCH_MISSING,
  },
  {
    label: "bool-false-two-branches",
    source: UPLC_EVAL,
    uplc: "(case (con bool False) (con integer 5) (con integer 6))",
    term: () => constCase(bool(false), int(5), int(6)),
    expected: integer(5n, 48_100n, 400n),
  },
  {
    label: "list-tail",
    source: UPLC_EVAL,
    uplc: "(case (con (list integer) [1, 2, 3]) (lam x (lam xs xs)))",
    term: () => constCase(integers([1n, 2n, 3n]), second()),
    expected: {
      kind: "constant",
      type: { kind: "list", element: INTEGER },
      payload: new DataList([new DataI(2n), new DataI(3n)]),
      cpu: 80_100n,
      memory: 600n,
    },
  },
  {
    label: "list-single-tail",
    source: UPLC_EVAL,
    uplc: "(case (con (list integer) [9]) (lam x (lam xs xs)) (con integer 0))",
    term: () => constCase(integers([9n]), second(), int(0)),
    expected: {
      kind: "constant",
      type: { kind: "list", element: INTEGER },
      payload: new DataList([]),
      cpu: 80_100n,
      memory: 600n,
    },
  },
  {
    label: "list-empty-three-branches",
    source: UPLC_EVAL,
    uplc: "(case (con (list integer) []) (con integer 1) (con integer 2) (con integer 3))",
    term: () => constCase(integers([]), int(1), int(2), int(3)),
    expected: BRANCH_MISSING,
  },
  {
    label: "list-no-branches",
    source: UPLC_EVAL,
    uplc: "(case (con (list integer) [1]))",
    term: () => constCase(integers([1n])),
    expected: BRANCH_MISSING,
  },
  {
    label: "list-bytestring-head",
    source: UPLC_EVAL,
    uplc: "(case (con (list bytestring) [#01, #0203]) (lam x (lam xs x)))",
    term: () =>
      constCase(
        UPLCConst.listOf(constT.byteStr)([
          bytes("01").value,
          bytes("0203").value,
        ] as never),
        first(),
      ),
    expected: {
      kind: "constant",
      type: BYTES,
      payload: new DataB(Buffer.from("01", "hex")),
      cpu: 80_100n,
      memory: 600n,
    },
  },
  {
    label: "list-pair-head",
    source: UPLC_EVAL,
    uplc: "(case (con (list (pair integer bytestring)) [(1, #aa), (2, #bb)]) (lam x (lam xs x)))",
    term: () => {
      const pairType = constT.pairOf(constT.int, constT.byteStr);
      const pair = (key: bigint, hex: string) =>
        UPLCConst.pairOf(constT.int, constT.byteStr)(key, bytes(hex).value)
          .value;
      return constCase(
        UPLCConst.listOf(pairType)([pair(1n, "aa"), pair(2n, "bb")] as never),
        first(),
      );
    },
    expected: {
      kind: "constant",
      type: { kind: "pair", first: INTEGER, second: BYTES },
      payload: new DataConstr(0n, [
        new DataI(1n),
        new DataB(Buffer.from("aa", "hex")),
      ]),
      cpu: 80_100n,
      memory: 600n,
    },
  },
  {
    label: "list-of-lists-tail",
    source: UPLC_EVAL,
    uplc: "(case (con (list (list integer)) [[1, 2], [3]]) (lam x (lam xs xs)))",
    term: () =>
      constCase(
        UPLCConst.listOf(constT.listOf(constT.int))([[1n, 2n], [3n]] as never),
        second(),
      ),
    expected: {
      kind: "constant",
      type: { kind: "list", element: { kind: "list", element: INTEGER } },
      payload: new DataList([new DataList([new DataI(3n)])]),
      cpu: 80_100n,
      memory: 600n,
    },
  },
  {
    label: "pair-data-second",
    source: UPLC_EVAL,
    uplc: "(case (con (pair data data) (I 1, B #ff)) (lam l (lam r r)))",
    term: () =>
      constCase(
        UPLCConst.pairOf(constT.data, constT.data)(
          UPLCConst.data(new DataI(1n)).value,
          UPLCConst.data(new DataB(Buffer.from("ff", "hex"))).value,
        ),
        second(),
      ),
    expected: {
      kind: "constant",
      type: DATA,
      payload: new DataB(Buffer.from("ff", "hex")),
      cpu: 80_100n,
      memory: 600n,
    },
  },
  {
    label: "pair-list-unit-first",
    source: UPLC_EVAL,
    uplc: "(case (con (pair (list integer) unit) ([1, 2], ())) (lam l (lam r l)))",
    term: () =>
      constCase(
        UPLCConst.pairOf(constT.listOf(constT.int), constT.unit)(
          [1n, 2n] as never,
          UPLCConst.unit.value,
        ),
        first(),
      ),
    expected: {
      kind: "constant",
      type: { kind: "list", element: INTEGER },
      payload: new DataList([new DataI(1n), new DataI(2n)]),
      cpu: 80_100n,
      memory: 600n,
    },
  },
  {
    label: "pair-no-branches",
    source: UPLC_EVAL,
    uplc: "(case (con (pair integer integer) (1, 2)))",
    term: () => constCase(UPLCConst.pairOf(constT.int, constT.int)(1n, 2n)),
    expected: BRANCH_MISSING,
  },
  {
    label: "bytestring",
    source: UPLC_EVAL,
    uplc: "(case (con bytestring #00) (con integer 1))",
    term: () => constCase(bytes("00"), int(1)),
    expected: UNSUPPORTED,
  },
  {
    label: "string",
    source: UPLC_EVAL,
    uplc: '(case (con string "a") (con integer 1))',
    term: () => constCase(UPLCConst.str("a"), int(1)),
    expected: UNSUPPORTED,
  },
  {
    label: "data",
    source: UPLC_EVAL,
    uplc: "(case (con data (I 0)) (con integer 1))",
    term: () => constCase(UPLCConst.data(new DataI(0n)), int(1)),
    expected: UNSUPPORTED,
  },
  {
    label: "delay",
    source: UPLC_EVAL,
    uplc: "(case (delay (con integer 0)) (con integer 1))",
    term: () => constCase(new Delay(int(0)), int(1)),
    expected: UNSUPPORTED,
  },
  {
    label: "unit-builtin-branch",
    source: UPLC_EVAL,
    uplc: "(case (con unit ()) (builtin addInteger))",
    term: () => constCase(UPLCConst.unit, Builtin.addInteger),
    expected: { kind: "builtin", tag: 0n, cpu: 48_100n, memory: 400n },
  },
  {
    label: "list-head-into-builtin",
    source: UPLC_EVAL,
    uplc: "(case (con (list integer) [4, 5]) (lam x (lam xs [(builtin addInteger) x (con integer 1)])))",
    term: () =>
      constCase(
        integers([4n, 5n]),
        new Lambda(
          new Lambda(
            new Application(
              new Application(Builtin.addInteger, new UPLCVar(1)),
              int(1),
            ),
          ),
        ),
      ),
    expected: integer(5n, 245_308n, 1_002n),
  },
];

const filler = () => new DataB(Buffer.alloc(5_000, 0x2a));

/** `List [I 1, B #2a.., B #2a..]`: a list payload over the inline bound. */
export const WIDE_LIST = () =>
  new DataList([new DataI(1n), filler(), filler()]);

/** `Constr 5 [B #2a.., B #2a..]`: its `unConstrData` pair is wide too. */
export const WIDE_CONSTR = () => new DataConstr(5n, [filler(), filler()]);

const ctx = () => new UPLCVar(0);
const unListData = () => new Application(Builtin.unListData, ctx());
const unConstrData = () => new Application(Builtin.unConstrData, ctx());
/** The tail of `WIDE_LIST`, and the field list of `WIDE_CONSTR`. */
const WIDE_TAIL = () => new DataList([filler(), filler()]);

/**
 * Wide scrutinees, which reach the case step as semantic constants. Each was
 * evaluated as `uplc eval (program 1.1.0 (lam ctx term)) context`.
 */
const WIDE: readonly CaseConstantCorpusEntry[] = [
  {
    label: "wide-list-head",
    source: UPLC_EVAL,
    uplc: "(lam ctx (case [(builtin unListData) ctx] (lam x (lam xs x))))",
    term: () => constCase(unListData(), first()),
    context: WIDE_LIST,
    expected: {
      kind: "constant",
      type: DATA,
      payload: new DataI(1n),
      cpu: 186_033n,
      memory: 1_132n,
    },
  },
  {
    label: "wide-list-tail",
    source: UPLC_EVAL,
    uplc: "(lam ctx (case [(builtin unListData) ctx] (lam x (lam xs xs)) (con integer 0)))",
    term: () => constCase(unListData(), second(), int(0)),
    context: WIDE_LIST,
    expected: {
      kind: "constant",
      type: { kind: "list", element: DATA },
      payload: WIDE_TAIL(),
      cpu: 186_033n,
      memory: 1_132n,
    },
  },
  {
    label: "wide-list-three-branches",
    source: UPLC_EVAL,
    uplc: "(lam ctx (case [(builtin unListData) ctx] (con integer 1) (con integer 2) (con integer 3)))",
    term: () => constCase(unListData(), int(1), int(2), int(3)),
    context: WIDE_LIST,
    expected: BRANCH_MISSING,
  },
  {
    label: "wide-list-no-branches",
    source: UPLC_EVAL,
    uplc: "(lam ctx (case [(builtin unListData) ctx]))",
    term: () => constCase(unListData()),
    context: WIDE_LIST,
    expected: BRANCH_MISSING,
  },
  {
    label: "wide-pair-first",
    source: UPLC_EVAL,
    uplc: "(lam ctx (case [(builtin unConstrData) ctx] (lam l (lam r l))))",
    term: () => constCase(unConstrData(), first()),
    context: WIDE_CONSTR,
    expected: integer(5n, 184_688n, 1_132n),
  },
  {
    label: "wide-pair-second",
    source: UPLC_EVAL,
    uplc: "(lam ctx (case [(builtin unConstrData) ctx] (lam l (lam r r))))",
    term: () => constCase(unConstrData(), second()),
    context: WIDE_CONSTR,
    expected: {
      kind: "constant",
      type: { kind: "list", element: DATA },
      payload: WIDE_TAIL(),
      cpu: 184_688n,
      memory: 1_132n,
    },
  },
  {
    label: "wide-pair-two-branches",
    source: UPLC_EVAL,
    uplc: "(lam ctx (case [(builtin unConstrData) ctx] (lam l (lam r r)) (lam l (lam r l))))",
    term: () => constCase(unConstrData(), second(), first()),
    context: WIDE_CONSTR,
    expected: BRANCH_MISSING,
  },
  {
    label: "wide-data",
    source: UPLC_EVAL,
    uplc: "(lam ctx (case ctx (con integer 1)))",
    term: () => constCase(ctx(), int(1)),
    context: WIDE_LIST,
    expected: UNSUPPORTED,
  },
];

export const CASE_CONSTANT_CORPUS: readonly CaseConstantCorpusEntry[] = [
  ...CASE_CONSTANT_CONFORMANCE,
  ...EDGES,
  ...WIDE,
];
