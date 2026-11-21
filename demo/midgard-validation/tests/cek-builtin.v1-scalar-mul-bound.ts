import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekDirectBuiltin,
  type MidgardCekDirectValueWitness,
  semanticizeMidgardCekDirectConstant,
  verifyMidgardCekDirectBuiltinFailure,
} from "../src/cek-builtin.js";
import { UPLCConst } from "../src/uplc-constant.js";
import {
  direct,
  directBuiltinRoot,
} from "./cek-builtin.v1-builtin-runtime-type-failures.js";

/**
 * G1 and G2 scalarMul (56, 63) fail on a scalar outside [-2^4095, 2^4095 - 1]
 * whatever the point, and Cardano charges the application first. The direct
 * failure verifier admits exactly the failures the L1 arm proves.
 */
describe("V1 scalarMul outside the scalar bound", () => {
  const bound = 2n ** 4095n;
  const point = (tag: bigint, hex: string): MidgardCekDirectValueWitness => ({
    kind: "constant",
    witness: {
      typeCbor: Buffer.from(tag === 56n ? "9f09ff" : "9f0aff", "hex"),
      payloadCbor: Buffer.from(hex, "hex"),
    },
  });
  // The compressed G1 and G2 generators.
  const generator = (tag: bigint): MidgardCekDirectValueWitness =>
    tag === 56n
      ? point(
          tag,
          "583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
        )
      : point(
          tag,
          "5f584093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc510515820c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8ff",
        );
  // The same charges cek-builtin-v1.test.ak pins on L1.
  const charge = (tag: bigint) =>
    tag === 56n
      ? { cpu: 77_000_558n, memory: 18n }
      : { cpu: 159_920_450n, memory: 36n };
  const failureIsProved = (
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitness[],
  ): boolean =>
    verifyMidgardCekDirectBuiltinFailure(
      tag,
      directBuiltinRoot(tag, arguments_),
      arguments_,
    );

  it.each([
    [56n, "2^4095", bound],
    [56n, "-2^4095 - 1", -bound - 1n],
    [63n, "2^4095", bound],
    [63n, "-2^4095 - 1", -bound - 1n],
  ] as const)("tag %s fails, charged, on scalar %s", (tag, _name, scalar) => {
    const arguments_ = [direct(UPLCConst.int(scalar)), generator(tag)];
    expect(evaluateMidgardCekDirectBuiltin(tag, arguments_)).toEqual({
      kind: "failure",
      budget: charge(tag),
    });
    expect(failureIsProved(tag, arguments_)).toBe(true);
  });

  it.each([
    [56n, "2^4095 - 1", bound - 1n],
    [56n, "-2^4095", -bound],
    [63n, "2^4095 - 1", bound - 1n],
    [63n, "-2^4095", -bound],
  ] as const)("tag %s succeeds on scalar %s", (tag, _name, scalar) => {
    const arguments_ = [direct(UPLCConst.int(scalar)), generator(tag)];
    expect(evaluateMidgardCekDirectBuiltin(tag, arguments_).kind).toBe(
      "success",
    );
    expect(failureIsProved(tag, arguments_)).toBe(false);
  });

  it.each([56n, 63n] as const)(
    "tag %s fails on the scalar alone, as L1 reads the point by its type",
    (tag) => {
      const compacted = semanticizeMidgardCekDirectConstant(generator(tag));
      const unchecked = point(
        tag,
        tag === 56n
          ? `5830${"00".repeat(48)}`
          : `5f5840${"00".repeat(64)}5820${"00".repeat(32)}ff`,
      );
      for (const source of [compacted, unchecked]) {
        expect(
          failureIsProved(tag, [direct(UPLCConst.int(bound)), source]),
        ).toBe(true);
        expect(
          failureIsProved(tag, [direct(UPLCConst.int(bound - 1n)), source]),
        ).toBe(false);
      }
    },
  );
});
