import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core";
import { Builtin, UPLCConst, UPLCProgram } from "@harmoniclabs/uplc";

import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
import { MidgardRedeemerTag, type PhaseAValidatedTx } from "../src/index.js";
import { app, countdown, int, lam, seq, v } from "./helpers/cek-uplc-terms.js";
import { MIDGARD_TX_EX_UNITS_CAP } from "./helpers/plutus-spend-input.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

/**
 * A PlutusV3 program that counts down from `iterations`, applying `step` to
 * an accumulator at each count, then returns unit.
 */
export const countdownProgram = (
  iterations: number,
  step: Builtin = Builtin.addInteger,
) =>
  buildMidgardCanonicalCekProgram(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [1, 1, 0],
        lam(
          seq(
            countdown(iterations, int(0), app(step, v(0), int(1))),
            UPLCConst.unit,
          ),
        ),
      ),
    ),
  );

/** A candidate with its pre-state entry, for one Phase B batch. */
export type WallCapCandidate = {
  readonly candidate: PhaseAValidatedTx;
  readonly preState: readonly (readonly [outRefHex: string, output: Buffer])[];
};

/**
 * A normal transaction that spends one output locked by a PlutusV3 script
 * counting down from `iterations` (applying `step`, `addInteger` by default),
 * plus the plain outputs the candidates in `alsoSpending` spend, declaring the
 * per-transaction ex-units cap. Its script is accepted whenever it runs to
 * the end.
 */
export const plutusCountdownSpend = (
  seed: number,
  iterations: number,
  {
    step,
    alsoSpending = [],
  }: {
    readonly step?: Builtin;
    readonly alsoSpending?: readonly WallCapCandidate[];
  } = {},
): WallCapCandidate => {
  const program = countdownProgram(iterations, step);
  const script = plutusV3ScriptWitness(program.envelopeCbor);
  const locked = outRefFromByte(seed);
  const plainPreState = alsoSpending.flatMap((entry) => entry.preState);
  const spent = [
    locked,
    ...plainPreState.map(([outRefHex]) => Buffer.from(outRefHex, "hex")),
  ].sort(Buffer.compare);
  return {
    candidate: makePhaseBCandidate({
      spent,
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE * BigInt(spent.length))],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: BigInt(spent.indexOf(locked)),
          exUnits: [
            MIDGARD_TX_EX_UNITS_CAP.memory,
            MIDGARD_TX_EX_UNITS_CAP.steps,
          ],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
        ...program.material.values(),
      ]),
    }),
    preState: [
      [
        locked.toString("hex"),
        makeProtectedScriptOutput(
          hashScriptWitness(script),
          FUNDED_OUTPUT_LOVELACE,
        ),
      ],
      ...plainPreState,
    ],
  };
};

/** A normal transaction with no scripts, spending one plain output. */
export const plainSpend = (seed: number): WallCapCandidate => {
  const spent = outRefFromByte(seed);
  return {
    candidate: makePhaseBCandidate({
      spent: [spent],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    }),
    preState: [[spent.toString("hex"), makeOutput(FUNDED_OUTPUT_LOVELACE)]],
  };
};

/** The pre-state of a batch of candidates. */
export const batchPreState = (
  candidates: readonly WallCapCandidate[],
): Map<string, Buffer> =>
  new Map(candidates.flatMap((entry) => entry.preState));
