import { createHash } from "node:crypto";

import { hashMidgardCekMachineState } from "@al-ft/midgard-core";

import { encodeMidgardCekCoreStepDataCbor } from "../../src/cek-data.js";
import {
  type MidgardCekExecutionMode,
  type MidgardCekStructuralExecution,
  type MidgardCekStructuralVerdict,
} from "../../src/cek-executor.js";
import { constT, UPLCConst } from "../../src/uplc-constant.js";
import { type UPLCTerm } from "../../src/uplc-term.js";
import {
  MIDGARD_CEK_ADVERSARIAL_SHAPES,
  prepareMidgardCekShape,
} from "./cek-adversarial-shapes.js";
import {
  BASE_ITEMS,
  bData,
  bigInteger,
  bytesConst,
  C,
  chooseData,
  chooseList,
  constrData,
  dataList,
  DIRECT_BOUND,
  equalsData,
  fillTo,
  I,
  LARGE_CONTEXT,
  LARGE_LIST,
  letIn,
  listData,
  listUnder,
  M,
  mapData,
  type MidgardCekDifferentialCase,
  mkPairData,
  program,
  serialiseData,
  unConstrData,
  unListData,
  unMapData,
} from "./cek-differential-builders.js";
import {
  edgeDataCases,
  failureCases,
  typedListCases,
} from "./cek-differential-corpus.edge.js";
import {
  app,
  countdown,
  data,
  dataInt,
  emptyDataList,
  fstPair,
  headList,
  int,
  type MidgardCekPreparedProgram,
  mkCons,
  nullList,
  runMidgardCekProgram,
  seqAll,
  sndPair,
  tailList,
  v,
  walkList,
} from "./cek-uplc-terms.js";
import { builtin } from "./uplc-builtins.js";

/**
 * A corpus of CEK programs for comparing two structural executors step by
 * step: the adversarial shapes at small sizes, Data at the integer, byte and
 * constructor encoding edges, every structural builtin on values that cross
 * the bounded direct-witness size in both directions, typed lists, large and
 * small script contexts, large source constants, and programs that fail.
 */

/** Every structural operation on a Data list bound at index 0. */
const listWorkout = (): UPLCTerm =>
  seqAll(
    headList(v(0)),
    tailList(v(0)),
    nullList(v(0)),
    chooseList(v(0), int(1), int(2)),
    letIn(
      listData(v(0)),
      seqAll(
        chooseData(v(0)),
        unListData(v(0)),
        equalsData(v(0), v(0)),
        headList(unListData(v(0))),
        mkPairData(v(0), dataInt(1n)),
        fstPair(mkPairData(v(0), dataInt(1n))),
        sndPair(mkPairData(dataInt(1n), v(0))),
        listData(tailList(unListData(v(0)))),
      ),
    ),
    letIn(
      constrData(int(0), v(0)),
      seqAll(
        chooseData(v(0)),
        unConstrData(v(0)),
        sndPair(unConstrData(v(0))),
        headList(sndPair(unConstrData(v(0)))),
        constrData(int(200), sndPair(unConstrData(v(0)))),
        constrData(int(bigInteger), tailList(sndPair(unConstrData(v(0))))),
      ),
    ),
    letIn(
      mkCons(mkPairData(dataInt(5n), listData(v(0))), unMapData(data(M([])))),
      seqAll(
        headList(v(0)),
        letIn(
          mapData(v(0)),
          seqAll(chooseData(v(0)), unMapData(v(0)), equalsData(v(0), v(0))),
        ),
      ),
    ),
    walkList(v(0)),
  );

const shapeCases = (): MidgardCekDifferentialCase[] =>
  MIDGARD_CEK_ADVERSARIAL_SHAPES.flatMap((shape) =>
    [3, 9].map((n) => ({
      label: `shape ${shape.label} n=${n}`,
      prepare: () => prepareMidgardCekShape(shape, n),
    })),
  );

/** Lists one item from the direct bound, both sides, through every op. */
const crossingCases = (): MidgardCekDifferentialCase[] =>
  [-3, -2, -1, 0, 1, 2, 3, 5].flatMap((offset) => {
    const target = DIRECT_BOUND + offset;
    const { item, base } = fillTo(BASE_ITEMS, target);
    const grown = mkCons(data(item), dataList(base));
    return [
      program(`list at ${target} bytes`, letIn(grown, listWorkout())),
      program(
        `serialiseData of a list at ${target} bytes`,
        serialiseData(listData(grown)),
      ),
      program(
        `serialiseData of a constructor at ${target + 2} bytes`,
        serialiseData(constrData(int(0), grown)),
      ),
      program(
        `pair list at ${target} bytes`,
        letIn(
          mkCons(
            mkPairData(data(item), dataInt(0n)),
            unMapData(data(M(base.map((key) => [key, I(0n)])))),
          ),
          seqAll(
            headList(v(0)),
            fstPair(headList(v(0))),
            sndPair(headList(v(0))),
            tailList(v(0)),
            letIn(
              mapData(v(0)),
              seqAll(unMapData(v(0)), equalsData(v(0), v(0)), chooseData(v(0))),
            ),
            walkList(v(0)),
          ),
        ),
      ),
    ];
  });

const grownLargeCases = (): MidgardCekDifferentialCase[] => [
  program(
    "Data list grown past the bound at runtime",
    letIn(
      countdown(
        140,
        emptyDataList(),
        mkCons(
          bData(app(builtin("consByteString"), v(1), bytesConst(69))),
          v(0),
        ),
      ),
      listWorkout(),
    ),
  ),
  program(
    "list of integer lists grown past the bound at runtime",
    letIn(
      countdown(
        45,
        UPLCConst.listOf(constT.listOf(constT.int))([]),
        mkCons(
          UPLCConst.listOf(constT.int)([bigInteger, -bigInteger, 7n]),
          v(0),
        ),
      ),
      seqAll(
        headList(v(0)),
        headList(headList(v(0))),
        tailList(v(0)),
        nullList(v(0)),
        chooseList(v(0), int(0), int(1)),
        walkList(v(0)),
      ),
    ),
  ),
];

/** Large Data reaching a script through its context. */
const largeContextCases = (): MidgardCekDifferentialCase[] => {
  const largeConstr = C(3, [LARGE_LIST, I(5n)]);
  const largeMap = M(
    [...BASE_ITEMS, ...listUnder(2_000)].map((key, index) => [
      key,
      I(BigInt(index)),
    ]),
  );
  return [
    program(
      "large Data list context",
      seqAll(
        chooseData(v(0)),
        equalsData(v(0), v(0)),
        letIn(unListData(v(0)), listWorkout()),
      ),
      LARGE_LIST,
    ),
    program(
      "serialiseData of a large list context",
      serialiseData(v(0)),
      LARGE_LIST,
    ),
    program(
      "large constructor context",
      seqAll(
        unConstrData(v(0)),
        fstPair(unConstrData(v(0))),
        letIn(unListData(headList(sndPair(unConstrData(v(0))))), listWorkout()),
      ),
      largeConstr,
    ),
    program(
      "large map context",
      letIn(
        unMapData(v(0)),
        seqAll(
          headList(v(0)),
          sndPair(headList(v(0))),
          tailList(v(0)),
          mapData(tailList(v(0))),
          walkList(v(0)),
        ),
      ),
      largeMap,
    ),
  ];
};

const contextCases = (): MidgardCekDifferentialCase[] => {
  const contextWorkout = seqAll(
    chooseData(v(0)),
    equalsData(v(0), v(0)),
    unConstrData(v(0)),
    letIn(sndPair(unConstrData(v(0))), seqAll(tailList(v(0)), walkList(v(0)))),
    letIn(unListData(headList(sndPair(unConstrData(v(0))))), listWorkout()),
  );
  return [
    program("large script context", contextWorkout, LARGE_CONTEXT),
    program(
      "map inside a large script context",
      letIn(
        unMapData(headList(tailList(sndPair(unConstrData(v(0)))))),
        seqAll(
          headList(v(0)),
          sndPair(headList(tailList(v(0)))),
          mapData(v(0)),
          walkList(v(0)),
        ),
      ),
      LARGE_CONTEXT,
    ),
    program(
      "serialiseData of a large script context",
      serialiseData(v(0)),
      LARGE_CONTEXT,
    ),
    program(
      "small script context",
      seqAll(
        chooseData(v(0)),
        serialiseData(v(0)),
        equalsData(v(0), v(0)),
        letIn(sndPair(unConstrData(v(0))), listWorkout()),
      ),
    ),
  ];
};

export const MIDGARD_CEK_DIFFERENTIAL_CORPUS: readonly MidgardCekDifferentialCase[] =
  [
    ...shapeCases(),
    ...crossingCases(),
    ...grownLargeCases(),
    ...largeContextCases(),
    ...contextCases(),
    ...edgeDataCases(),
    ...typedListCases(),
    ...failureCases(),
  ];

/**
 * A canonical rendering of a step witness: byte strings as hex, integers as
 * decimal, arrays and maps in order, object keys sorted, absent fields
 * omitted. Two witnesses render equally exactly when they carry the same
 * values in the same order.
 */
const canonical = (value: unknown): unknown => {
  if (value instanceof Uint8Array)
    return `h:${Buffer.from(value).toString("hex")}`;
  if (typeof value === "bigint") return `n:${value.toString(10)}`;
  if (Array.isArray(value)) return value.map(canonical);
  if (value instanceof Map) {
    return {
      map: [...value.entries()].map(([key, item]) => [
        canonical(key),
        canonical(item),
      ]),
    };
  }
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .filter((key) => (value as Record<string, unknown>)[key] !== undefined)
        .map((key) => [
          key,
          canonical((value as Record<string, unknown>)[key]),
        ]),
    );
  }
  return value;
};

export type MidgardCekDifferentialOutcome =
  | { readonly throws: true }
  | {
      readonly throws: false;
      readonly steps: number;
      readonly stopReason: string;
      readonly terminal: string;
      readonly digest: string;
    };

/** Every committed byte of a run, in step order, folded into one digest. */
export const runMidgardCekDifferentialCase = (
  prepared: MidgardCekPreparedProgram,
  mode: MidgardCekExecutionMode,
  onStep?: (index: number, stepDigest: string) => void,
): MidgardCekDifferentialOutcome => {
  let run: MidgardCekStructuralExecution | MidgardCekStructuralVerdict;
  try {
    run = runMidgardCekProgram(prepared, mode);
  } catch {
    return { throws: true };
  }
  const hash = createHash("sha256");
  hash.update(hashMidgardCekMachineState(run.initialState));
  const steps = "steps" in run ? run.steps : [];
  steps.forEach((step, index) => {
    const stepHash = createHash("sha256")
      .update(hashMidgardCekMachineState(step.pre))
      .update(hashMidgardCekMachineState(step.post))
      .update(encodeMidgardCekCoreStepDataCbor(step))
      .update(JSON.stringify(canonical(step.witness)))
      .digest();
    onStep?.(index, stepHash.toString("hex"));
    hash.update(stepHash);
  });
  const terminal = Buffer.from(
    hashMidgardCekMachineState(run.terminalState),
  ).toString("hex");
  return {
    throws: false,
    steps: "steps" in run ? run.steps.length : run.stepCount,
    stopReason: run.stopReason,
    terminal,
    digest: hash.update(terminal).digest("hex"),
  };
};
