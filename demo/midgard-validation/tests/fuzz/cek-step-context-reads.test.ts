import {
  hashMidgardCekMachineState,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  midgardCekCoreStepWitnessData,
  midgardCekMachineStateData,
} from "../../src/cek-data.js";
import { executeMidgardCekStructuralProgram } from "../../src/cek-executor.js";
import { constT, UPLCConst } from "../../src/uplc-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "../../src/uplc-data.js";
import { Builtin, type UPLCTerm } from "../../src/uplc-term.js";
import {
  app,
  data,
  discard,
  force,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  prepareMidgardCekProgram,
  v,
} from "../helpers/cek-uplc-terms.js";
import { builtin } from "../helpers/uplc-builtins.js";
import {
  type AikenStepOracle,
  makeAikenStepOracle,
} from "./cek-step-fuzz.oracle.js";

/**
 * Scripts that read a transaction-shaped context, or build Data of the same
 * size: every step of their run must verify on the Aiken machine, within the
 * evaluator's default budget, to the post-state TypeScript computed.
 */

const bytes = (length: number): Data =>
  new DataB(new Uint8Array(length).fill(9));
const constr = (index: bigint, fields: Data[]): Data =>
  new DataConstr(index, fields);
const integers = (count: number): Data[] =>
  Array.from({ length: count }, (_, index) => new DataI(BigInt(index)));

/** A transaction input with its resolved output, shaped like a TxInInfo. */
const txInInfo = (index: number): Data =>
  constr(0n, [
    constr(0n, [bytes(32), new DataI(BigInt(index))]),
    constr(0n, [
      constr(0n, [constr(0n, [bytes(28)]), constr(1n, [])]),
      new DataMap([
        new DataPair(
          bytes(0),
          new DataMap([new DataPair(bytes(0), new DataI(2_000_000n))]),
        ),
      ]),
      constr(0n, []),
      constr(1n, []),
    ]),
  ]);
const inputs = (count: number): Data =>
  new DataList(Array.from({ length: count }, (_, index) => txInInfo(index)));
const dataList = (items: Data[]): UPLCTerm =>
  UPLCConst.listOf(constT.data)(items);

const EMPTY_CONTEXT: Data = constr(0n, []);

/**
 * Each case: its label, the script, the context, and the builtins whose
 * application the script exists to step through, in run order. Every script
 * discards its result and returns Unit.
 */
const cases: readonly (readonly [
  string,
  UPLCTerm,
  Data,
  readonly Builtin[],
])[] = [
  [
    "headList (unListData ctx) over one input",
    lam(
      discard(
        app(
          force(builtin("headList") as never),
          app(builtin("unListData") as never, v(0)),
        ),
      ),
    ),
    inputs(1),
    [builtin("unListData"), builtin("headList")],
  ],
  [
    "unListData ctx over five inputs",
    lam(discard(app(builtin("unListData") as never, v(0)))),
    inputs(5),
    [builtin("unListData")],
  ],
  [
    "unConstrData ctx over a sixteen-field record",
    lam(discard(app(builtin("unConstrData") as never, v(0)))),
    constr(
      0n,
      Array.from({ length: 16 }, (_, index) => new DataList([txInInfo(index)])),
    ),
    [builtin("unConstrData")],
  ],
  [
    "listData of two eight-integer lists",
    lam(
      discard(
        app(
          builtin("listData") as never,
          dataList([new DataList(integers(8)), new DataList(integers(8))]),
        ),
      ),
    ),
    EMPTY_CONTEXT,
    [builtin("listData")],
  ],
  [
    "mkCons onto a 64-integer list",
    lam(
      discard(
        app(
          force(builtin("mkCons") as never),
          data(new DataI(0n)),
          dataList(integers(64)),
        ),
      ),
    ),
    EMPTY_CONTEXT,
    [builtin("mkCons")],
  ],
];

const stateCbor = (state: MidgardCekMachineState): Uint8Array =>
  lucidDataToCborIterative(midgardCekMachineStateData(state) as never);

describe("CEK steps of context-reading scripts against the Aiken machine", () => {
  let oracle: AikenStepOracle | undefined;

  // The oracle compiles the on-chain project twice per run, shared by every
  // file that uses it (about 45 s on a developer machine, a few minutes on a
  // loaded CI runner); a file that starts meanwhile waits for it.
  beforeAll(async () => {
    oracle = await makeAikenStepOracle();
  }, 300_000);

  afterAll(() => oracle?.close());

  it.each(cases)(
    "verifies every step of %s",
    { timeout: 600_000 },
    (_, script, context, builtins) => {
      const prepared = prepareMidgardCekProgram(script, context);
      const execution = executeMidgardCekStructuralProgram({
        root: prepared.root,
        material: prepared.material.values(),
        constantWitnesses: prepared.constantWitnesses,
        maxSteps: 400_000,
        executionBudget: MIDGARD_CEK_TX_BUDGET,
      });
      expect(execution.terminalState.mode).toBe("haltSuccess");
      const steps = execution.steps;
      expect(
        steps.flatMap((step) =>
          step.witness.kind.startsWith("executeBuiltin") &&
          "tag" in step.witness
            ? [step.witness.tag]
            : [],
        ),
      ).toEqual(builtins.map((builtin) => BigInt(builtin.builtinTag)));
      const verdicts = oracle!.verify(
        steps.map((step) => ({
          pre: stateCbor(step.pre),
          post: stateCbor(step.post),
          witness: lucidDataToCborIterative(
            midgardCekCoreStepWitnessData(step.witness) as never,
          ),
        })),
      );
      expect(
        verdicts.flatMap((verdict, index) =>
          verdict === "accepted"
            ? []
            : [`step ${index.toString()} (${steps[index]!.witness.kind})`],
        ),
      ).toEqual([]);
      expect(oracle!.hash(steps.map((step) => stateCbor(step.post)))).toEqual(
        steps.map((step) =>
          hashMidgardCekMachineState(step.post).toString("hex"),
        ),
      );
    },
  );
});
