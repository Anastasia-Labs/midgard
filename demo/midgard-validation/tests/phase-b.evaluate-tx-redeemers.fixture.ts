import {
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import {
  type MidgardCekBuiltinName,
  midgardCekBuiltinTagOfName,
} from "../src/cek-builtin.cardano-tags.js";
import { midgardCekBuiltinForceCount } from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { MidgardRedeemerTag } from "../src/index.js";
import { UPLCConst } from "../src/uplc-constant.js";
import {
  Application,
  Builtin,
  Delay,
  ErrorUPLC,
  Force,
  Lambda,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "../src/uplc-term.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

const builtin = (name: MidgardCekBuiltinName): UPLCTerm => {
  const tag = midgardCekBuiltinTagOfName(name);
  let term: UPLCTerm = new Builtin(tag);
  for (
    let force = 0n;
    force < midgardCekBuiltinForceCount(BigInt(tag));
    force += 1n
  ) {
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
    builtin("fstPair"),
    apply(builtin("unConstrData"), context),
  );
  const body = new Lambda(
    new Force(
      apply(
        builtin("ifThenElse"),
        apply(
          builtin("equalsInteger"),
          apply(
            builtin("multiplyInteger"),
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
  return Buffer.from(
    encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], body)),
  );
};

/** The draft's validity lower bound. */
export const EVALUATE_DRAFT_SLOT = 200_000n;

/** The out-ref of the draft's script-locked input. */
export const EVALUATE_DRAFT_SPENT = outRefFromByte(0x5d);

export const makeEvaluateDraft = (
  exUnits: readonly [bigint, bigint],
  fails = false,
) => {
  return makeEvaluateProgramDraft(exUnits, contextTagIsZero(fails));
};

/** A draft with the caller's complete Flat program as its spending script. */
export const makeEvaluateProgramDraft = (
  exUnits: readonly [bigint, bigint],
  scriptFlat: Uint8Array,
) => {
  const program = buildCekProgramFromRawFlat(scriptFlat);
  const script = plutusV3ScriptWitness(program.scriptBytes);
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
  });
  const state = new Map([[spent.toString("hex"), spentOutput]]);
  return { candidate, state };
};
