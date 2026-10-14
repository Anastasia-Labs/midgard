import { type Data, DataB, DataConstr } from "@harmoniclabs/plutus-data";
import { Builtin, UPLCConst, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  executeMidgardCekStructuralProgram,
  MIDGARD_CEK_GATED_SET_V1,
  MidgardCekOutsideGatedSet,
} from "../src/index.js";
import {
  app,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  prepareMidgardCekProgram,
} from "./helpers/cek-uplc-terms.js";

/**
 * The admission screen admits a BLS uncompress failure only when the on-chain
 * verifier can prove its cause: for G1 a wrong length, an invalid header or a
 * point off the curve; for G2 a wrong length or an invalid header. A run
 * failing for any other cause is refused.
 */

const CONTEXT: Data = new DataConstr(0n, []);

const G1_GENERATOR =
  "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb";
const G2_GENERATOR =
  "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8";

/** A compressed encoding: flags `0x80`, then `coordinates` as 48-byte words. */
const compressed = (...coordinates: readonly bigint[]): string => {
  const hex = coordinates
    .map((coordinate) => coordinate.toString(16).padStart(96, "0"))
    .join("");
  return `8${hex.slice(1)}`;
};

const uncompressing = (
  builtin: typeof Builtin.bls12_381_G1_uncompress,
  hex: string,
): UPLCTerm =>
  lam(
    app(
      builtin,
      UPLCConst.byteString(new DataB(Buffer.from(hex, "hex")).bytes),
    ),
  );
const g1 = (hex: string): UPLCTerm =>
  uncompressing(Builtin.bls12_381_G1_uncompress, hex);
const g2 = (hex: string): UPLCTerm =>
  uncompressing(Builtin.bls12_381_G2_uncompress, hex);

const screenedRun = (script: UPLCTerm, mode: "proof" | "verdict") => {
  const prepared = prepareMidgardCekProgram(script, CONTEXT);
  return executeMidgardCekStructuralProgram({
    root: prepared.root,
    material: prepared.material.values(),
    constantWitnesses: prepared.constantWitnesses,
    maxSteps: 0xffff_ffff,
    executionBudget: MIDGARD_CEK_TX_BUDGET,
    mode,
    gatedSet: MIDGARD_CEK_GATED_SET_V1,
  });
};

/** How the run halted, or the arm the screen stopped it at. */
const screenedOutcome = (script: UPLCTerm, mode: "proof" | "verdict") => {
  try {
    return screenedRun(script, mode).terminalState.mode;
  } catch (cause) {
    expect(cause).toBeInstanceOf(MidgardCekOutsideGatedSet);
    return `refused at ${(cause as MidgardCekOutsideGatedSet).arm}`;
  }
};

const admitted: readonly (readonly [string, UPLCTerm, string])[] = [
  ["the G1 generator", g1(G1_GENERATOR), "haltSuccess"],
  ["the G2 generator", g2(G2_GENERATOR), "haltSuccess"],
  ["a G1 encoding of 47 bytes", g1(G1_GENERATOR.slice(2)), "haltError"],
  [
    "a G1 encoding without the compression flag",
    g1(`1${G1_GENERATOR.slice(1)}`),
    "haltError",
  ],
  ["a G1 encoding of a point off the curve", g1(compressed(1n)), "haltError"],
  ["a G2 encoding of 95 bytes", g2(G2_GENERATOR.slice(2)), "haltError"],
  [
    "a G2 encoding without the compression flag",
    g2(`1${G2_GENERATOR.slice(1)}`),
    "haltError",
  ],
  [
    "a G2 encoding of infinity with a coordinate",
    g2(`c${compressed(0n, 1n).slice(1)}`),
    "haltError",
  ],
];

const refused: readonly (readonly [string, UPLCTerm])[] = [
  [
    "a G1 encoding of a point outside the group",
    g1(G1_GENERATOR.replace("586c55e83ff", "586c45e83ff")),
  ],
  ["a G1 encoding of the curve point at x = 0", g1(compressed(0n))],
  ["a G2 encoding of a point off the curve", g2(compressed(0n, 0n))],
  ["a G2 encoding of a point outside the group", g2(compressed(0n, 2n))],
];

describe("CEK admission screen on BLS uncompress failures", () => {
  it.each(admitted)("admits a run uncompressing %s", (_, script, halt) => {
    expect(screenedOutcome(script, "verdict")).toBe(halt);
    expect(screenedOutcome(script, "proof")).toBe(halt);
  });

  it.each(refused)("refuses a run uncompressing %s", (_, script) => {
    expect(screenedOutcome(script, "verdict")).toBe(
      "refused at executeBuiltinFailure",
    );
    expect(screenedOutcome(script, "proof")).toBe(
      "refused at executeBuiltinFailure",
    );
  });

  it("fails every refused run without a set", () => {
    for (const [, script] of refused) {
      const prepared = prepareMidgardCekProgram(script, CONTEXT);
      const outcome = executeMidgardCekStructuralProgram({
        root: prepared.root,
        material: prepared.material.values(),
        constantWitnesses: prepared.constantWitnesses,
        maxSteps: 0xffff_ffff,
        executionBudget: MIDGARD_CEK_TX_BUDGET,
        mode: "verdict",
      });
      expect(outcome.terminalState.mode).toBe("haltError");
    }
  });
});
