/**
 * A draft that spends one PlutusV3-locked output, for the evaluate path:
 * the script succeeds (or, with `fails`, errors) after reading the context.
 */
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import {
  Application,
  Builtin,
  Delay,
  ErrorUPLC,
  Force,
  getNRequiredForces,
  Lambda,
  UPLCBuiltinTag,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { CML } from "@lucid-evolution/lucid";

import {
  buildMidgardCanonicalCekProgram,
  MidgardRedeemerTag,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

const builtin = (tag: UPLCBuiltinTag): UPLCTerm => {
  let term: UPLCTerm = new Builtin(tag);
  for (let force = 0; force < getNRequiredForces(tag); force += 1) {
    term = new Force(term);
  }
  return term;
};
const apply = (fn: UPLCTerm, ...args: UPLCTerm[]): UPLCTerm =>
  args.reduce<UPLCTerm>((term, arg) => new Application(term, arg), fn);

/** Succeeds when the context's constructor tag is 0, after some arithmetic. */
const contextTagIsZero = (fails: boolean) => {
  const context = new UPLCVar(0);
  const tag = apply(
    builtin(UPLCBuiltinTag.fstPair),
    apply(builtin(UPLCBuiltinTag.unConstrData), context),
  );
  const body = new Lambda(
    new Force(
      apply(
        builtin(UPLCBuiltinTag.ifThenElse),
        apply(
          builtin(UPLCBuiltinTag.equalsInteger),
          apply(
            builtin(UPLCBuiltinTag.multiplyInteger),
            tag,
            UPLCConst.int(fails ? 1n : 0n),
          ),
          UPLCConst.int(fails ? 1n : 0n),
        ),
        new Delay(UPLCConst.unit),
        new Delay(new ErrorUPLC()),
      ),
    ),
  );
  return Buffer.from(UPLCEncoder.compile(new UPLCProgram([1, 1, 0], body)));
};

/** The draft's validity lower bound. */
export const EVALUATE_DRAFT_SLOT = 200_000n;

/** The out-ref of the draft's script-locked input. */
export const EVALUATE_DRAFT_SPENT = outRefFromByte(0x5d);

export const makeEvaluateDraft = (
  exUnits: readonly [bigint, bigint],
  fails = false,
) => {
  const program = buildMidgardCanonicalCekProgram(contextTagIsZero(fails));
  const script = plutusV3ScriptWitness(program.envelopeCbor);
  const spent = EVALUATE_DRAFT_SPENT;
  const spentOutput = encodeMidgardTxOutput({
    address: protectMidgardAddress(
      Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_script(
            CML.ScriptHash.from_hex(hashScriptWitness(script)),
          ),
        )
          .to_address()
          .to_raw_bytes(),
      ),
    ),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  });
  const candidate = makePhaseBCandidate({
    spent: [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    validityIntervalStart: EVALUATE_DRAFT_SLOT,
    scriptWitnesses: [script],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      { tag: MidgardRedeemerTag.Spend, index: 0n, exUnits: [...exUnits] },
    ]),
    scriptLanguages: ["PlutusV3"],
    programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
      ...program.material.values(),
    ]),
  });
  const state = new Map([[spent.toString("hex"), spentOutput]]);
  return { candidate, state };
};
