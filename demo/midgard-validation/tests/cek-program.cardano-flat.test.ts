import { createHash } from "node:crypto";

import { UPLCProgram } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  CardanoFlatRefusal,
  decodeCardanoFlatProgram,
  decodeMidgardCekCardanoFlatProgram,
  encodeMidgardCekCardanoFlatProgram,
} from "../src/cek-program.cardano-flat.js";
import {
  CARDANO_ACCEPTED,
  CARDANO_REFUSED,
  CONSTR_1024_UNITS,
  CONSTR_1025_UNITS,
  flatOfTermBits,
  OUTSIDE_PROFILE,
} from "./cek-program.cardano-flat.vectors.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");
const bytes = (value: string): Buffer => Buffer.from(value, "hex");

describe("raw Flat programs as the Cardano evaluator reads them", () => {
  it("builds the 1,024- and 1,025-field rows the evaluator read", () => {
    const sha256 = (flat: string): string =>
      createHash("sha256").update(bytes(flat)).digest("hex");
    expect(sha256(CONSTR_1024_UNITS)).toBe(
      "c87fc2fd056f8cfd335905c5205c64d1e2e5e1d23337dcc605f058bdabbb5dac",
    );
    expect(sha256(CONSTR_1025_UNITS)).toBe(
      "d85eab721b3eb0c789100a47f883b690a943b2594ac94c37746fc986c2dbdc38",
    );
  });

  it.each(CARDANO_ACCEPTED)(
    "%s decodes to the program Cardano read",
    (_label, flat, _cardano, twin) => {
      const program = decodeMidgardCekCardanoFlatProgram(bytes(flat));
      expect(hex(encodeMidgardCekCardanoFlatProgram(program))).toBe(twin);
      expect(
        hex(
          encodeMidgardCekCardanoFlatProgram(
            decodeMidgardCekCardanoFlatProgram(bytes(twin)),
          ),
        ),
      ).toBe(twin);
      expect(program).toBeInstanceOf(UPLCProgram);
      expect(decodeCardanoFlatProgram(bytes(flat))).toEqual({
        ok: true,
        program,
      });
    },
  );

  it.each(CARDANO_ACCEPTED.slice(0, 3))(
    "%s builds the local root of the program Cardano read",
    (_label, flat, _cardano, twin) => {
      expect(hex(buildCekProgramFromRawFlat(bytes(flat)).termRoot)).toBe(
        hex(buildCekProgramFromRawFlat(bytes(twin)).termRoot),
      );
    },
  );

  it.each(CARDANO_REFUSED)(
    "%s is refused",
    (_label, flat, _cardano, refusal, bitOffset) => {
      expect(decodeCardanoFlatProgram(bytes(flat))).toMatchObject({
        ok: false,
        refusal,
        bitOffset,
      });
      expect(() => decodeMidgardCekCardanoFlatProgram(bytes(flat))).toThrow(
        `Cardano Flat refusal ${refusal} at bit ${bitOffset}:`,
      );
      expect(() => buildCekProgramFromRawFlat(bytes(flat))).toThrow(
        `Cardano Flat refusal ${refusal} at bit ${bitOffset}:`,
      );
    },
  );

  it.each(OUTSIDE_PROFILE)(
    "%s, which Cardano decodes, is outside the profile",
    (_label, flat, _cardano, bitOffset) => {
      expect(decodeCardanoFlatProgram(bytes(flat))).toMatchObject({
        ok: false,
        refusal: "outsideProfile",
        bitOffset,
      });
      let thrown: unknown;
      try {
        buildCekProgramFromRawFlat(bytes(flat));
      } catch (error) {
        thrown = error;
      }
      expect(thrown).toBeInstanceOf(CardanoFlatRefusal);
      expect((thrown as CardanoFlatRefusal).cardanoRefuses).toBe(false);
    },
  );

  it("reads deep nesting without deepening the JavaScript stack", () => {
    // 200,000 nested delays around an error term.
    const flat = flatOfTermBits("0001".repeat(200_000) + "0110");
    expect(decodeCardanoFlatProgram(bytes(flat)).ok).toBe(true);
  });
});
