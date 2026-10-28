import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import {
  MidgardRedeemerTag,
  type PhaseAValidatedTx,
  RejectCodes,
  type ValidationMachineReplayInput,
} from "../src/index.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { ErrorUPLC, UPLCProgram, type UPLCTerm } from "../src/uplc-term.js";
import { app, lam, seq, v } from "./helpers/cek-uplc-terms.js";
import { MIDGARD_TX_EX_UNITS_CAP } from "./helpers/plutus-spend-input.js";
import { builtin } from "./helpers/uplc-builtins.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";
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

/** A PlutusV3 program `\ctx. body`, built at runtime. */
export const plutusProgram = (body: UPLCTerm) =>
  buildCekProgramFromRawFlat(
    encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], lam(body))),
  );

/** Returns unit: accepted. */
export const acceptingProgram = () => plutusProgram(UPLCConst.unit);

/** Halts with `error`: rejected by the script itself. */
export const failingProgram = () => plutusProgram(new ErrorUPLC());

/**
 * Doubles a 75-byte string six times with `appendByteString`, then returns
 * unit. Cardano accepts it; the last append's arguments and result together
 * pass the reference evaluator's 9,215-byte direct payload bound, so proof
 * mode proves it on the staged route.
 */
export const doublingProgram = () => {
  let bytes: UPLCTerm = UPLCConst.byteString(new Uint8Array(75).fill(0xab));
  for (let round = 0; round < 6; round += 1) {
    bytes = app(lam(app(builtin("appendByteString"), v(0), v(0))), bytes);
  }
  return plutusProgram(seq(bytes, UPLCConst.unit));
};

export type PlutusProgram = ReturnType<typeof plutusProgram>;

/** A candidate with its pre-state, for one Phase B batch. */
export type ProgramSpend = {
  readonly candidate: PhaseAValidatedTx;
  readonly preState: Map<string, Buffer>;
};

/**
 * A normal transaction that spends one output locked by `program` as a
 * PlutusV3 script, declaring the per-transaction ex-units cap.
 */
export const plutusProgramSpend = (
  seed: number,
  program: PlutusProgram,
): ProgramSpend => {
  const script = plutusV3ScriptWitness(program.scriptBytes);
  const locked = outRefFromByte(seed);
  return {
    candidate: makePhaseBCandidate({
      spent: [locked],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          exUnits: [
            MIDGARD_TX_EX_UNITS_CAP.memory,
            MIDGARD_TX_EX_UNITS_CAP.steps,
          ],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    }),
    preState: new Map([
      [
        locked.toString("hex"),
        makeProtectedScriptOutput(
          hashScriptWitness(script),
          FUNDED_OUTPUT_LOVELACE,
        ),
      ],
    ]),
  };
};

/**
 * The replay input of `accepted` claimed as rejected by its script, as a
 * challenger whose own evaluation rejected it would claim it: no ledger
 * change, and the UTxO root unchanged.
 */
export const claimedRejected = (
  accepted: ValidationMachineReplayInput,
): ValidationMachineReplayInput => ({
  ...accepted,
  postUtxosRoot: accepted.priorUtxosRoot,
  expectedLedgerOps: [],
  ledgerMutationSteps: [],
  expectedVerdict: "rejected",
  expectedRejectionCode: RejectCodes.PlutusScriptInvalid,
});
