/**
 * Complete boundary programs for TS verdict checks. A `pinBuiltinStep` case
 * emits only its named builtin transition in the Aiken golden, independently
 * of constant decoding in the trace prefix. The maximum successful result is
 * tag-102 Data, which the pre-en5 decoder cannot prove in direct presentation;
 * only the two semantic failure transitions join the core-step golden set.
 */

import { constT, DataConstr, UPLCConst } from "@al-ft/midgard-validation/uplc";
import { Application, Builtin, Lambda } from "@harmoniclabs/uplc";

const EMPTY_CONTEXT = () => new DataConstr(0n, []);

/** Full constrData boundary programs used by Phase B and the TS machine. */
export const CEK_CONSTR_DATA_INDEX_PROGRAMS = [
  {
    label: "constr_data_index_zero",
    note: "constrData accepts the minimum unsigned 64-bit index; only the builtin step is pinned",
    term: () =>
      new Lambda(
        new Application(
          new Lambda(UPLCConst.unit),
          new Application(
            new Application(Builtin.constrData, UPLCConst.int(0n)),
            UPLCConst.listOf(constT.data)([]),
          ),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
    pinBuiltinStep: 37n,
  },
  {
    label: "constr_data_index_maximum",
    note: "constrData accepts the maximum unsigned 64-bit index; only the builtin step is pinned",
    term: () =>
      new Lambda(
        new Application(
          new Lambda(UPLCConst.unit),
          new Application(
            new Application(
              Builtin.constrData,
              UPLCConst.int((1n << 64n) - 1n),
            ),
            UPLCConst.listOf(constT.data)([]),
          ),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
    pinBuiltinStep: 37n,
  },
  {
    label: "constr_data_index_overflow",
    note: "constrData refuses an index above Word64 without a builtin charge; only the builtin step is pinned",
    term: () =>
      new Lambda(
        new Application(
          new Lambda(UPLCConst.unit),
          new Application(
            new Application(Builtin.constrData, UPLCConst.int(1n << 64n)),
            UPLCConst.listOf(constT.data)([]),
          ),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
    pinBuiltinStep: 37n,
  },
  {
    label: "constr_data_index_negative",
    note: "constrData refuses a negative index without a builtin charge; only the builtin step is pinned",
    term: () =>
      new Lambda(
        new Application(
          new Lambda(UPLCConst.unit),
          new Application(
            new Application(Builtin.constrData, UPLCConst.int(-1n)),
            UPLCConst.listOf(constT.data)([]),
          ),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
    pinBuiltinStep: 37n,
  },
];

/** The two refusals, which join the core-step golden set. */
export const CEK_CONSTR_DATA_INDEX_REFUSALS =
  CEK_CONSTR_DATA_INDEX_PROGRAMS.filter(
    (program) =>
      program.label === "constr_data_index_overflow" ||
      program.label === "constr_data_index_negative",
  );
