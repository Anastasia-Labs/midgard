import {
  Application,
  Builtin,
  Delay,
  ErrorUPLC,
  Force,
  Lambda,
  UPLCBuiltinTag,
  UPLCConst,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { MIDGARD_CEK_CARDANO_BLS_TAGS as TAG } from "../src/cek-builtin.cardano-tags.js";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "../src/cek-executor.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

// SH-2: scripts name the BLS builtins by Cardano's flat tags (Plutus
// `Builtins.hs`: 58/65 compress, 59/66 uncompress, 60/67 hashToGroup). The
// hashToGroup points are RFC 9380 hash_to_curve of "midgard" under the BLS
// signature suites' DSTs, computed independently of the machine.
const hex = (value: string): UPLCTerm =>
  UPLCConst.byteString(Buffer.from(value, "hex") as never);
const ascii = (value: string): UPLCTerm =>
  UPLCConst.byteString(Buffer.from(value, "ascii") as never);
const builtin = (tag: number): UPLCTerm => new Builtin(tag as UPLCBuiltinTag);
const app = (f: UPLCTerm, ...args: UPLCTerm[]): UPLCTerm =>
  args.reduce<UPLCTerm>((acc, arg) => new Application(acc, arg), f);

const G1_GENERATOR =
  "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb";
const G2_GENERATOR =
  "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8";
const G1_HASHED =
  "8148482e08a92312cbd0a869e03ced121bf66f29306e281b0f894701f5e3f1242fa6cfdd3afbb2620cb9bfc50ed7dd52";
const G2_HASHED =
  "89ac93813e1f13d1f1a75d2c790cbc435330bdfcaa9f42a609aa5563b8bab0edbc269b186eb3c3c9a6bdf9fd525c34e7167f474f0e648c0c3eeab5df3eebf3ff7c39f72addeb2d3368a44d56f5dca28fca97fe1184b365e3c8a14e226b6188b1";

/** `(lam ctx (if (equalsByteString actual expected) () error))`. */
const assertEqualBytes = (actual: UPLCTerm, expected: string): Buffer =>
  encodeMidgardCekCardanoFlatProgram(
    new UPLCProgram(
      [1, 1, 0],
      new Lambda(
        new Force(
          app(
            new Force(new Builtin(UPLCBuiltinTag.ifThenElse)),
            app(
              new Builtin(UPLCBuiltinTag.equalsByteString),
              actual,
              hex(expected),
            ),
            new Delay(UPLCConst.unit),
            new Delay(new ErrorUPLC()),
          ),
        ),
      ),
    ),
  );

const halts = (flat: Buffer): string => {
  const program = buildCekProgramFromRawFlat(flat);
  const graph = buildMidgardCekExecutionGraph(
    program.termRoot,
    program.material.values(),
    Buffer.from("d87980", "hex"),
  );
  const run = executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: 100_000,
    executionBudget: { cpu: 10_000_000_000n, memory: 14_000_000n },
    mode: "verdict",
  });
  return `${run.stopReason}/${run.terminalState.mode}`;
};

describe("BLS builtins under Cardano's flat tags", () => {
  const groups = [
    {
      group: "G1",
      compress: TAG.g1Compress,
      uncompress: TAG.g1Uncompress,
      hashToGroup: TAG.g1HashToGroup,
      generator: G1_GENERATOR,
      hashed: G1_HASHED,
      dst: "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_NUL_",
    },
    {
      group: "G2",
      compress: TAG.g2Compress,
      uncompress: TAG.g2Uncompress,
      hashToGroup: TAG.g2HashToGroup,
      generator: G2_GENERATOR,
      hashed: G2_HASHED,
      dst: "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_",
    },
  ] as const;

  it.each(groups)(
    "$group: compress, uncompress and hashToGroup give Cardano's results",
    ({ compress, uncompress, hashToGroup, generator, hashed, dst }) => {
      const roundTrip = app(
        builtin(compress),
        app(builtin(uncompress), hex(generator)),
      );
      const hashedPoint = app(
        builtin(compress),
        app(builtin(hashToGroup), ascii("midgard"), ascii(dst)),
      );
      expect(halts(assertEqualBytes(roundTrip, generator))).toBe(
        "halted/haltSuccess",
      );
      expect(halts(assertEqualBytes(hashedPoint, hashed))).toBe(
        "halted/haltSuccess",
      );
      // The same script against a different point fails.
      expect(halts(assertEqualBytes(hashedPoint, generator))).toBe(
        "halted/haltError",
      );
    },
  );
});
