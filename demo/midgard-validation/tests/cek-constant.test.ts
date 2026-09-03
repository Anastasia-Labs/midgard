import {
  constT,
  type ConstType,
  constTypeEq,
  ConstTyTag,
  eqConstValue,
  UPLCConst as Constant,
  type UPLCConst,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekCanonicalConstant,
  hashMidgardCekConstantWitness,
  midgardCekConstantWitnessFromUplc,
  midgardCekConstantWitnessToUplc,
  parseMidgardCekConstantType,
} from "../src/cek-constant.js";

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

const constantRoot = (constant: UPLCConst): string => {
  const canonical = encodeMidgardCekCanonicalConstant(constant);
  return hex(
    hashMidgardCekConstantWitness({
      typeCbor: canonical.typeCbor,
      payloadCbor: canonical.payloadCbor,
    }),
  );
};

describe("V1 semantic CEK constants", () => {
  it("matches the Aiken integer, boolean, and list vectors", () => {
    const integer = encodeMidgardCekCanonicalConstant(Constant.int(41));
    const boolean = encodeMidgardCekCanonicalConstant(Constant.bool(true));
    const integers = encodeMidgardCekCanonicalConstant(
      Constant.listOf(constT.int)([1n, 2n]),
    );

    expect(hex(integer.typeCbor)).toBe("9f00ff");
    expect(hex(integer.payloadCbor)).toBe("1829");
    expect(constantRoot(Constant.int(41))).toBe(
      "35930512fad9db8f38585195d5363af8826bb2b002028fe85af5874a85ab305c",
    );

    expect(hex(boolean.typeCbor)).toBe("9f04ff");
    expect(hex(boolean.payloadCbor)).toBe("d87a80");
    expect(constantRoot(Constant.bool(true))).toBe(
      "b9d299a51cb8e6181262d67d07662451930d812d4d79348a86d64361f93a889d",
    );

    expect(hex(integers.typeCbor)).toBe("9f0500ff");
    expect(hex(integers.payloadCbor)).toBe("9f0102ff");
    expect(constantRoot(Constant.listOf(constT.int)([1n, 2n]))).toBe(
      "455063152b571b24bc7ba2e98d14120484eea1fe9a4b7210b639fb413f27f0ba",
    );
  });

  it("rejects malformed types and source-encoded BLS values", () => {
    expect(() =>
      parseMidgardCekConstantType([
        ConstTyTag.int,
        ConstTyTag.bool,
      ] as ConstType),
    ).toThrow(/trailing tags/u);
    expect(() =>
      parseMidgardCekConstantType([7] as unknown as ConstType),
    ).toThrow(/unknown type tag/u);

    const sourceBls = {
      type: constT.bls12_381_G1_element,
      value: undefined,
    } as unknown as UPLCConst;
    expect(() => encodeMidgardCekCanonicalConstant(sourceBls)).toThrow(
      /cannot contain encoded BLS constants/u,
    );
  });

  it("decodes and hashes the exact canonical L1 witness", () => {
    const witness = {
      typeCbor: Buffer.from("9f00ff", "hex"),
      payloadCbor: Buffer.from("1829", "hex"),
    };
    expect(decodeMidgardCekConstantWitness(witness).type).toEqual({
      kind: "integer",
    });
    expect(hex(hashMidgardCekConstantWitness(witness))).toBe(
      "35930512fad9db8f38585195d5363af8826bb2b002028fe85af5874a85ab305c",
    );

    expect(() =>
      decodeMidgardCekConstantWitness({
        ...witness,
        payloadCbor: Buffer.from("1817", "hex"),
      }),
    ).toThrow(/not canonical/u);
    expect(() =>
      decodeMidgardCekConstantWitness({
        typeCbor: Buffer.from("9f01ff", "hex"),
        payloadCbor: Buffer.from("01", "hex"),
      }),
    ).toThrow(/does not match/u);
  });

  it("round-trips reference-evaluator scalar and recursive constants", () => {
    const constants = [
      Constant.int(-42),
      Constant.str("Midgard"),
      Constant.unit,
      Constant.bool(false),
      Constant.listOf(constT.int)([1n, -2n, 3n]),
      Constant.pairOf(constT.int, constT.bool)(7n, true),
    ];
    for (const constant of constants) {
      const decoded = midgardCekConstantWitnessToUplc(
        midgardCekConstantWitnessFromUplc(constant),
      );
      expect(constTypeEq(decoded.type, constant.type)).toBe(true);
      expect(eqConstValue(decoded.value, constant.value)).toBe(true);
    }
  });
});
