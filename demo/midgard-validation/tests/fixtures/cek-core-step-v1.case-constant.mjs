/**
 * The `case` on a constant programs of the V1 CEK core-step golden vectors
 * (`cek-core-step-v1.vectors.mjs`), one per kind of outcome, plus a `case` on
 * a constructor that selects its first branch. Like the rest of the set, these
 * are UPLC terms only; their constants come from the package's `uplc` entry.
 */

import {
  Case,
  Constr,
  constT,
  DataB,
  ErrorUPLC,
  Lambda,
  UPLCConst,
  UPLCVar,
} from "@al-ft/midgard-validation/uplc";

/** The programs, each applied to `context`. */
export const caseConstantPrograms = (context) => [
  {
    label: "case_constant_boolean",
    note: "returnCaseConstant: True selects the second branch",
    term: () =>
      new Lambda(
        new Case(UPLCConst.bool(true), [UPLCConst.int(7), UPLCConst.int(8)]),
      ),
    context,
    maxSteps: 32,
  },
  {
    label: "case_constant_integer_first",
    note: "returnCaseConstant: zero selects the first of two branches",
    term: () =>
      new Lambda(new Case(UPLCConst.int(0), [UPLCConst.unit, new ErrorUPLC()])),
    context,
    maxSteps: 32,
  },
  {
    label: "case_constant_list",
    note: "returnCaseConstant: a list applies its branch to head and tail",
    term: () =>
      new Lambda(
        new Case(UPLCConst.listOf(constT.int)([1n, 2n]), [
          new Lambda(new Lambda(new UPLCVar(1))),
        ]),
      ),
    context,
    maxSteps: 32,
  },
  {
    label: "case_constant_pair",
    note: "returnCaseConstant: a pair applies its branch to both components",
    term: () =>
      new Lambda(
        new Case(UPLCConst.pairOf(constT.int, constT.bool)(42n, false), [
          new Lambda(new Lambda(new UPLCVar(0))),
        ]),
      ),
    context,
    maxSteps: 32,
  },
  {
    label: "case_constant_branch_missing",
    note: "returnCaseConstant: error_case_branch_missing",
    term: () => new Lambda(new Case(UPLCConst.int(3), [UPLCConst.int(7)])),
    context,
    maxSteps: 32,
  },
  {
    label: "case_constant_unsupported",
    note: "returnCaseConstant: error_invalid_case_scrutinee",
    term: () =>
      new Lambda(
        new Case(
          UPLCConst.byteString(new DataB(Buffer.from("00", "hex")).bytes),
          [UPLCConst.int(7)],
        ),
      ),
    context,
    maxSteps: 32,
  },
  {
    label: "case_constr_first",
    note: "returnCaseConstr: tag 0 selects the first of three branches",
    term: () =>
      new Lambda(
        new Case(new Constr(0n, []), [
          UPLCConst.unit,
          new ErrorUPLC(),
          new ErrorUPLC(),
        ]),
      ),
    context,
    maxSteps: 32,
  },
];
