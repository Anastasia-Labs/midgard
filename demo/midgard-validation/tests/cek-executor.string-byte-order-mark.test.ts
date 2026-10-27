import { describe, expect, it } from "vitest";

import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { constT, UPLCConst } from "../src/uplc-constant.js";
import {
  Application,
  Delay,
  ErrorUPLC,
  Force,
  UPLCProgram,
  type UPLCTerm,
} from "../src/uplc-term.js";
import { builtin, type BuiltinName } from "./helpers/uplc-builtins.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

const apply = (
  function_: UPLCTerm,
  ...arguments_: readonly UPLCTerm[]
): UPLCTerm =>
  arguments_.reduce<UPLCTerm>(
    (term, argument) => new Application(term, argument),
    function_,
  );

const call = (name: BuiltinName, ...arguments_: readonly UPLCTerm[]) =>
  apply(builtin(name), ...arguments_);

/**
 * Runs a script that returns Unit when `condition` is True and fails
 * otherwise; every step it takes must pass the consensus verifier.
 */
const holds = (condition: UPLCTerm): boolean => {
  const term = new Force(
    apply(
      new Force(builtin("ifThenElse")),
      condition,
      new Delay(UPLCConst.unit),
      new Delay(new ErrorUPLC()),
    ),
  );
  const program = buildCekProgramFromRawFlat(
    encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], term)),
  );
  const execution = executeMidgardCekStructuralProgram({
    root: program.termRoot,
    material: program.material.values(),
    constantWitnesses: program.constantWitnesses,
    maxSteps: 64,
  });
  for (const step of execution.steps) {
    expect(verifyMidgardCekCoreStep(step.pre, step.post, step.witness)).toBe(
      true,
    );
  }
  return execution.terminalState.mode !== "haltError";
};

const MARKED_BYTES = UPLCConst.byteString(
  Uint8Array.from([0xef, 0xbb, 0xbf, 0x41]),
);
const MARKED_TEXT = UPLCConst.str("\uFEFFA");

// A leading byte-order mark is one of the string's characters, as in Plutus.
describe("V1 strings with a leading byte-order mark", () => {
  it("decodeUtf8 keeps the mark", () => {
    const decoded = call("decodeUtf8", MARKED_BYTES);
    expect(holds(call("equalsString", decoded, MARKED_TEXT))).toBe(true);
    expect(holds(call("equalsString", decoded, UPLCConst.str("A")))).toBe(
      false,
    );
    expect(
      holds(
        call("equalsByteString", call("encodeUtf8", decoded), MARKED_BYTES),
      ),
    ).toBe(true);
  });

  it("encodeUtf8 of a marked string round-trips", () => {
    const encoded = call("encodeUtf8", MARKED_TEXT);
    expect(holds(call("equalsByteString", encoded, MARKED_BYTES))).toBe(true);
    expect(
      holds(call("equalsString", call("decodeUtf8", encoded), MARKED_TEXT)),
    ).toBe(true);
  });

  it("a marked string read out of a list keeps the mark", () => {
    const list = UPLCConst.listOf(constT.str)([MARKED_TEXT.value as string]);
    const head = apply(new Force(builtin("headList")), list);
    expect(holds(call("equalsString", head, MARKED_TEXT))).toBe(true);
    expect(holds(call("equalsString", head, UPLCConst.str("A")))).toBe(false);
  });
});
