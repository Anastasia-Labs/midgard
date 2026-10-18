import { type Data, DataI } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  constT,
  Delay,
  Force,
  Lambda,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";

import { encodeMidgardCekPlutusData } from "../../src/cek-constant.js";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionMode,
  type MidgardCekStructuralExecution,
  type MidgardCekStructuralVerdict,
} from "../../src/cek-executor.js";
import { encodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.cardano-flat.js";
import { buildCekProgramFromRawFlat } from "../validation-fixtures.flat-program.js";

/** Small UPLC combinators for building CEK test programs at runtime. */

export const v = (index: number): UPLCTerm => new UPLCVar(index);
export const lam = (body: UPLCTerm): UPLCTerm => new Lambda(body);
export const app = (fn: UPLCTerm, ...args: readonly UPLCTerm[]): UPLCTerm =>
  args.reduce<UPLCTerm>((acc, arg) => new Application(acc, arg), fn);
export const force = (term: UPLCTerm): UPLCTerm => new Force(term);
export const delay = (term: UPLCTerm): UPLCTerm => new Delay(term);
export const int = (value: number | bigint): UPLCTerm => UPLCConst.int(value);
export const data = (value: Data): UPLCTerm => UPLCConst.data(value);
export const dataInt = (value: bigint): UPLCTerm => data(new DataI(value));
export const emptyDataList = (): UPLCTerm => UPLCConst.listOf(constT.data)([]);
export const emptyPairList = (): UPLCTerm =>
  UPLCConst.listOf(constT.pairOf(constT.data, constT.data))([]);

/** Evaluates `kept`, then `discarded`, and yields `kept`. */
export const seq = (discarded: UPLCTerm, kept: UPLCTerm): UPLCTerm =>
  app(lam(lam(v(1))), kept, discarded);

/**
 * `[(\_. ()) term]`: evaluates `term`, then returns Unit, the only value an
 * accepting script may halt with.
 */
export const discard = (term: UPLCTerm): UPLCTerm =>
  app(lam(UPLCConst.unit), term);

/** Evaluates every term, last first, and yields the last. */
export const seqAll = (...terms: readonly UPLCTerm[]): UPLCTerm =>
  terms
    .slice(0, -1)
    .reduceRight<UPLCTerm>(
      (kept, discarded) => seq(discarded, kept),
      terms.at(-1)!,
    );

/** Strict-language fixpoint `\f. (\x. f (\y. x x y)) (\x. f (\y. x x y))`. */
export const fix = (body: UPLCTerm): UPLCTerm => {
  const half = lam(app(v(1), lam(app(v(1), v(1), v(0)))));
  return app(lam(app(half, half)), body);
};

export const ifThenElse = (
  condition: UPLCTerm,
  whenTrue: UPLCTerm,
  whenFalse: UPLCTerm,
): UPLCTerm =>
  force(
    app(
      force(Builtin.ifThenElse),
      condition,
      delay(whenTrue),
      delay(whenFalse),
    ),
  );

export const ifZero = (
  counter: UPLCTerm,
  whenZero: UPLCTerm,
  otherwise: UPLCTerm,
): UPLCTerm =>
  ifThenElse(app(Builtin.equalsInteger, counter, int(0)), whenZero, otherwise);

/**
 * `loop n acc = n == 0 ? acc : loop (n - 1) (step acc)`, applied to `n` and
 * `initial`. `step` is a term with the accumulator at index 0, the counter at
 * index 1 and the loop itself at index 2.
 */
export const countdown = (
  n: number,
  initial: UPLCTerm,
  step: UPLCTerm,
): UPLCTerm =>
  app(
    fix(
      lam(
        lam(
          lam(
            ifZero(
              v(1),
              v(0),
              app(v(2), app(Builtin.subtractInteger, v(1), int(1)), step),
            ),
          ),
        ),
      ),
    ),
    int(n),
    initial,
  );

export const mkCons = (head: UPLCTerm, tail: UPLCTerm): UPLCTerm =>
  app(force(Builtin.mkCons), head, tail);
export const headList = (list: UPLCTerm): UPLCTerm =>
  app(force(Builtin.headList), list);
export const tailList = (list: UPLCTerm): UPLCTerm =>
  app(force(Builtin.tailList), list);
export const nullList = (list: UPLCTerm): UPLCTerm =>
  app(force(Builtin.nullList), list);
export const fstPair = (pair: UPLCTerm): UPLCTerm =>
  app(force(force(Builtin.fstPair)), pair);
export const sndPair = (pair: UPLCTerm): UPLCTerm =>
  app(force(force(Builtin.sndPair)), pair);

/**
 * Walks a list to its end with `tailList`, reading every head with
 * `headList`: `walk l = nullList l ? 0 : walk (tailList l)`.
 */
export const walkList = (list: UPLCTerm): UPLCTerm =>
  app(
    fix(
      lam(
        lam(
          ifThenElse(
            nullList(v(0)),
            int(0),
            seq(headList(v(0)), app(v(1), tailList(v(0)))),
          ),
        ),
      ),
    ),
    list,
  );

export type MidgardCekPreparedProgram = {
  readonly root: Uint8Array;
  readonly material: ReturnType<
    typeof buildMidgardCekExecutionGraph
  >["material"];
  readonly constantWitnesses: ReturnType<
    typeof buildMidgardCekExecutionGraph
  >["constantWitnesses"];
};

/** Compiles `script` and applies it to `context`. */
export const prepareMidgardCekProgram = (
  script: UPLCTerm,
  context: Data,
): MidgardCekPreparedProgram => {
  const program = buildCekProgramFromRawFlat(
    encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], script)),
  );
  const graph = buildMidgardCekExecutionGraph(
    program.termRoot,
    program.material.values(),
    encodeMidgardCekPlutusData(context),
  );
  return {
    root: graph.root,
    material: graph.material,
    constantWitnesses: graph.constantWitnesses,
  };
};

/** The consensus profile's per-transaction execution-unit cap. */
export const MIDGARD_CEK_TX_BUDGET = Object.freeze({
  cpu: 10_000_000_000n,
  memory: 16_500_000n,
});

export function runMidgardCekProgram(
  prepared: MidgardCekPreparedProgram,
  mode: "verdict",
): MidgardCekStructuralVerdict;
export function runMidgardCekProgram(
  prepared: MidgardCekPreparedProgram,
  mode: "proof",
): MidgardCekStructuralExecution;
export function runMidgardCekProgram(
  prepared: MidgardCekPreparedProgram,
  mode: MidgardCekExecutionMode,
): MidgardCekStructuralExecution | MidgardCekStructuralVerdict;
export function runMidgardCekProgram(
  prepared: MidgardCekPreparedProgram,
  mode: MidgardCekExecutionMode,
): MidgardCekStructuralExecution | MidgardCekStructuralVerdict {
  return executeMidgardCekStructuralProgram({
    root: prepared.root,
    material: prepared.material.values(),
    constantWitnesses: prepared.constantWitnesses,
    maxSteps: 0xffff_ffff,
    executionBudget: MIDGARD_CEK_TX_BUDGET,
    mode,
  });
}
