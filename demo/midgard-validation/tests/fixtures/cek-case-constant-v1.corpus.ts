/**
 * Known answers for `case` on a constant under the protocol version 11 rules.
 *
 * Every expected result and budget below was produced by a reference
 * evaluator, never by the Midgard machine:
 *
 * - `conformance/...` entries are the UPLC conformance suite's
 *   `v3/term/constant-case` programs, with their `.expected` and
 *   `.budget.expected` files, at uplc rev 6868a8d.
 * - `uplc eval` entries were evaluated with `aiken uplc eval` (aiken
 *   v1.1.23+5adf783), which also reproduces every conformance answer.
 *   Programs with a `context` were evaluated as `uplc eval program context`,
 *   so their budget already includes the context application.
 *
 * The test runs each program the way a script runs: under a context lambda,
 * applied to its context. A closed program therefore costs three more machine
 * steps (application, lambda, context constant) than its reference budget.
 * Cardano reports one failure for every refused program; `errorCode` is the
 * Midgard halt reason that the failing step must prove.
 */

import type { MidgardCekConstantType } from "../../src/cek-constant.js";
import { constT, UPLCConst } from "../../src/uplc-constant.js";
import { type Data, DataConstr, DataI } from "../../src/uplc-data.js";
import { Case, Lambda, type UPLCTerm, UPLCVar } from "../../src/uplc-term.js";

export type CaseConstantExpectation =
  | {
      readonly kind: "constant";
      readonly type: MidgardCekConstantType;
      readonly payload: Data;
      readonly cpu: bigint;
      readonly memory: bigint;
    }
  | {
      readonly kind: "builtin";
      readonly tag: bigint;
      readonly cpu: bigint;
      readonly memory: bigint;
    }
  | { readonly kind: "failure"; readonly errorCode: bigint };

export type CaseConstantCorpusEntry = {
  readonly label: string;
  readonly source: string;
  readonly uplc: string;
  /** The program's term; with a `context` it is the body under `lam ctx`. */
  readonly term: () => UPLCTerm;
  readonly context?: () => Data;
  readonly expected: CaseConstantExpectation;
};

export const INTEGER = { kind: "integer" } as const;
const BOOLEAN = { kind: "boolean" } as const;
export const BYTES = { kind: "bytes" } as const;
export const DATA = { kind: "data" } as const;

export const BRANCH_MISSING = { kind: "failure", errorCode: 6n } as const;
export const UNSUPPORTED = { kind: "failure", errorCode: 5n } as const;
const NOT_A_FUNCTION = { kind: "failure", errorCode: 2n } as const;

export const integer = (value: bigint, cpu: bigint, memory: bigint) =>
  ({
    kind: "constant",
    type: INTEGER,
    payload: new DataI(value),
    cpu,
    memory,
  }) as const;

export const int = (value: number | bigint) => UPLCConst.int(value);
export const bool = (value: boolean) => UPLCConst.bool(value);
export const bytes = (hex: string) =>
  UPLCConst.byteString(Buffer.from(hex, "hex"));
export const integers = (values: readonly bigint[]) =>
  UPLCConst.listOf(constT.int)(values as never);
export const constCase = (scrutinee: UPLCTerm, ...branches: UPLCTerm[]) =>
  new Case(scrutinee, branches);

/** `(lam a (lam b a))` and `(lam a (lam b b))`. */
export const first = () => new Lambda(new Lambda(new UPLCVar(1)));
export const second = () => new Lambda(new Lambda(new UPLCVar(0)));

const conformance = (group: string, name: string) =>
  `conformance v3/term/constant-case/${group}/${name} (uplc 6868a8d)`;
export const UPLC_EVAL = "aiken uplc eval (aiken v1.1.23+5adf783)";

const integerPairFortyTwoFalse = () =>
  UPLCConst.pairOf(constT.int, constT.bool)(42n, false);

/** Programs from the conformance suite, each with its checked-in answer. */
export const CASE_CONSTANT_CONFORMANCE: readonly CaseConstantCorpusEntry[] = [
  {
    label: "bool-01",
    source: conformance("bool", "bool-01"),
    uplc: "(case (con bool False) (con integer 0) (con integer 1))",
    term: () => constCase(bool(false), int(0), int(1)),
    expected: integer(0n, 48_100n, 400n),
  },
  {
    label: "bool-02",
    source: conformance("bool", "bool-02"),
    uplc: "(case (con bool True) (con integer 0) (con integer 1))",
    term: () => constCase(bool(true), int(0), int(1)),
    expected: integer(1n, 48_100n, 400n),
  },
  {
    label: "bool-03",
    source: conformance("bool", "bool-03"),
    uplc: "(case (con bool False) (con integer 0))",
    term: () => constCase(bool(false), int(0)),
    expected: integer(0n, 48_100n, 400n),
  },
  {
    label: "bool-04",
    source: conformance("bool", "bool-04"),
    uplc: "(case (con bool True) (con integer 0))",
    term: () => constCase(bool(true), int(0)),
    expected: BRANCH_MISSING,
  },
  {
    label: "bool-05",
    source: conformance("bool", "bool-05"),
    uplc: "(case (con bool False) (con integer 0) (con integer 1) (con integer 2))",
    term: () => constCase(bool(false), int(0), int(1), int(2)),
    expected: BRANCH_MISSING,
  },
  {
    label: "bool-06",
    source: conformance("bool", "bool-06"),
    uplc: "(case (con bool True) (con integer 0) (con integer 1) (con integer 2))",
    term: () => constCase(bool(true), int(0), int(1), int(2)),
    expected: BRANCH_MISSING,
  },
  {
    label: "bool-07",
    source: conformance("bool", "bool-07"),
    uplc: "(case (con bool False))",
    term: () => constCase(bool(false)),
    expected: BRANCH_MISSING,
  },
  {
    label: "integer-01",
    source: conformance("integer", "integer-01"),
    uplc: "(case (con integer 0))",
    term: () => constCase(int(0)),
    expected: BRANCH_MISSING,
  },
  {
    label: "integer-02",
    source: conformance("integer", "integer-02"),
    uplc: "(case (con integer 1) (con integer 42) (con integer 43) (con integer 443))",
    term: () => constCase(int(1), int(42), int(43), int(443)),
    expected: integer(43n, 48_100n, 400n),
  },
  {
    label: "integer-03",
    source: conformance("integer", "integer-03"),
    uplc: "(case (con integer 3) (con integer 42) (con integer 43) (con integer 44))",
    term: () => constCase(int(3), int(42), int(43), int(44)),
    expected: BRANCH_MISSING,
  },
  {
    label: "integer-04",
    source: conformance("integer", "integer-04"),
    uplc: "(case (con integer -1) (con integer 42) (con integer 43) (con integer 44))",
    term: () => constCase(int(-1), int(42), int(43), int(44)),
    expected: BRANCH_MISSING,
  },
  {
    label: "list-01",
    source: conformance("list", "list-01"),
    uplc: "(case (con (list integer) [1, 2, 3, 4]) (lam x (lam xs x)) (con integer -1))",
    term: () => constCase(integers([1n, 2n, 3n, 4n]), first(), int(-1)),
    expected: integer(1n, 80_100n, 600n),
  },
  {
    label: "list-02",
    source: conformance("list", "list-02"),
    uplc: "(case (con (list integer) []) (lam x (lam xs x)) (con integer -1))",
    term: () => constCase(integers([]), first(), int(-1)),
    expected: integer(-1n, 48_100n, 400n),
  },
  {
    label: "list-03",
    source: conformance("list", "list-03"),
    uplc: "(case (con (list integer) [1, 2, 3, 4]) (lam x (lam xs x)))",
    term: () => constCase(integers([1n, 2n, 3n, 4n]), first()),
    expected: integer(1n, 80_100n, 600n),
  },
  {
    label: "list-04",
    source: conformance("list", "list-04"),
    uplc: "(case (con (list integer) []) (lam x (lam xs x)))",
    term: () => constCase(integers([]), first()),
    expected: BRANCH_MISSING,
  },
  {
    label: "list-05",
    source: conformance("list", "list-05"),
    uplc: "(case (con (list integer) [1, 2, 3, 4]) (con integer -1))",
    term: () => constCase(integers([1n, 2n, 3n, 4n]), int(-1)),
    expected: NOT_A_FUNCTION,
  },
  {
    label: "list-06",
    source: conformance("list", "list-06"),
    uplc: "(case (con (list integer) [1, 2, 3, 4]) (con integer -1) (con integer -1))",
    term: () => constCase(integers([1n, 2n, 3n, 4n]), int(-1), int(-1)),
    expected: NOT_A_FUNCTION,
  },
  {
    label: "list-07",
    source: conformance("list", "list-07"),
    uplc: "(case (con (list integer) [1, 2, 3, 4]) (con integer -1) (lam x (lam xs x)))",
    term: () => constCase(integers([1n, 2n, 3n, 4n]), int(-1), first()),
    expected: NOT_A_FUNCTION,
  },
  {
    label: "pair-01",
    source: conformance("pair", "pair-01"),
    uplc: "(case (con (pair integer bool) (42, False)) (lam l (lam r l)))",
    term: () => constCase(integerPairFortyTwoFalse(), first()),
    expected: integer(42n, 80_100n, 600n),
  },
  {
    label: "pair-02",
    source: conformance("pair", "pair-02"),
    uplc: "(case (con (pair integer bool) (42, False)) (lam l (lam r r)))",
    term: () => constCase(integerPairFortyTwoFalse(), second()),
    expected: {
      kind: "constant",
      type: BOOLEAN,
      payload: new DataConstr(0n, []),
      cpu: 80_100n,
      memory: 600n,
    },
  },
  {
    label: "pair-03",
    source: conformance("pair", "pair-03"),
    uplc: "(case (con (pair integer bool) (42, False)))",
    term: () => constCase(integerPairFortyTwoFalse()),
    expected: BRANCH_MISSING,
  },
  {
    label: "pair-04",
    source: conformance("pair", "pair-04"),
    uplc: "(case (con (pair integer bool) (42, False)) (lam l (lam r r)) (lam l (lam r r)))",
    term: () => constCase(integerPairFortyTwoFalse(), second(), second()),
    expected: BRANCH_MISSING,
  },
  {
    label: "pair-05",
    source: conformance("pair", "pair-05"),
    uplc: "(case (con (pair integer bool) (42, False)) (lam l l))",
    term: () =>
      constCase(integerPairFortyTwoFalse(), new Lambda(new UPLCVar(0))),
    expected: NOT_A_FUNCTION,
  },
  {
    label: "unit-01",
    source: conformance("unit", "unit-01"),
    uplc: "(case (con unit ()) (con integer 42))",
    term: () => constCase(UPLCConst.unit, int(42)),
    expected: integer(42n, 48_100n, 400n),
  },
  {
    label: "unit-02",
    source: conformance("unit", "unit-02"),
    uplc: "(case (con unit ()))",
    term: () => constCase(UPLCConst.unit),
    expected: BRANCH_MISSING,
  },
  {
    label: "unit-03",
    source: conformance("unit", "unit-03"),
    uplc: "(case (con unit ()) (con integer 42) (con integer 43))",
    term: () => constCase(UPLCConst.unit, int(42), int(43)),
    expected: BRANCH_MISSING,
  },
];
