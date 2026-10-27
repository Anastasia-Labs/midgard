/**
 * The protocol version 11 builtin programs (tags 87 to 100) of the V1 CEK
 * core-step golden vectors (`cek-core-step-v1.vectors.mjs`). A program cannot
 * hold a Value or an array constant, so each one builds them: a Value from
 * Data with unValueData, an array from a list with listToArray. Like the rest
 * of the set, these are UPLC terms only, written in Midgard's own term
 * classes, which carry the PV 11 builtin tags.
 */

import {
  Application,
  Builtin,
  constT,
  DataB,
  DataI,
  DataMap,
  Force,
  Lambda,
  UPLCConst,
} from "@al-ft/midgard-validation/uplc";

const apply = (head, ...arguments_) =>
  arguments_.reduce((term, argument) => new Application(term, argument), head);
const builtin = (tag, forces = 0) => {
  let term = new Builtin(tag);
  for (let index = 0; index < forces; index += 1) term = new Force(term);
  return term;
};
const int = (value) => UPLCConst.int(value);
const bytes = (hex) => UPLCConst.byteString(Buffer.from(hex, "hex"));
const ints = (...values) => UPLCConst.listOf(constT.int)(values);

const POLICY = "aa".repeat(28);
const TOKEN = "bb".repeat(4);
const OTHER_TOKEN = "cc".repeat(4);

/** A one-token Value's Data: `{policy: {token: quantity}}`. */
const valueData = (quantity) =>
  UPLCConst.data(
    new DataMap([
      {
        fst: new DataB(Buffer.from(POLICY, "hex")),
        snd: new DataMap([
          {
            fst: new DataB(Buffer.from(TOKEN, "hex")),
            snd: new DataI(quantity),
          },
        ]),
      },
    ]),
  );
const value = (quantity) => apply(builtin(99), valueData(quantity));
const array = (...values) => apply(builtin(90, 1), ints(...values));

/** The programs, each applied to `context`. */
export const pv11Programs = (context) => [
  {
    label: "pv11_exp_mod_integer",
    note: "executeBuiltinDirect on expModInteger, an inverse power",
    term: () =>
      new Lambda(apply(builtin(87), int(3n), int(-5n), int(1000003n))),
    context,
    maxSteps: 64,
  },
  {
    label: "pv11_list_and_array",
    note: "executeBuiltinDirect on dropList, listToArray, lengthOfArray and indexArray",
    term: () =>
      new Lambda(
        apply(
          builtin(0),
          apply(
            builtin(89, 1),
            apply(
              builtin(90, 1),
              apply(builtin(88, 1), int(1n), ints(5n, 6n, 7n)),
            ),
          ),
          apply(builtin(91, 1), array(5n, 6n, 7n), int(2n)),
        ),
      ),
    context,
    maxSteps: 128,
  },
  {
    label: "pv11_value_arithmetic",
    note: "executeBuiltinDirect on unValueData, insertCoin, scaleValue, unionValue and lookupCoin",
    term: () =>
      new Lambda(
        apply(
          builtin(95),
          bytes(POLICY),
          bytes(TOKEN),
          apply(
            builtin(96),
            apply(
              builtin(94),
              bytes(POLICY),
              bytes(OTHER_TOKEN),
              int(3n),
              value(1n),
            ),
            apply(builtin(100), int(2n), value(1n)),
          ),
        ),
      ),
    context,
    maxSteps: 128,
  },
  {
    label: "pv11_value_contains",
    note: "executeBuiltinDirect on valueContains and valueData",
    term: () =>
      new Lambda(
        apply(
          builtin(97),
          value(2n),
          apply(builtin(99), apply(builtin(98), value(1n))),
        ),
      ),
    context,
    maxSteps: 128,
  },
  {
    label: "pv11_exp_mod_integer_failure",
    note: "executeBuiltinFailure on expModInteger with no inverse, charged",
    term: () => new Lambda(apply(builtin(87), int(2n), int(-1n), int(4n))),
    context,
    maxSteps: 64,
  },
  {
    label: "pv11_index_array_failure",
    note: "executeBuiltinFailure on indexArray past the end, charged",
    term: () => new Lambda(apply(builtin(91, 1), array(5n, 6n, 7n), int(3n))),
    context,
    maxSteps: 64,
  },
  {
    label: "pv11_index_array_outside_int64",
    note: "executeBuiltinFailure on indexArray at 2^64, uncharged",
    term: () =>
      new Lambda(apply(builtin(91, 1), array(5n, 6n, 7n), int(2n ** 64n))),
    context,
    maxSteps: 64,
  },
];
