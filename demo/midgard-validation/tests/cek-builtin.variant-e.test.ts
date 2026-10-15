import { hashMidgardCekValueNode } from "@al-ft/midgard-core";
import { DataB } from "@harmoniclabs/plutus-data";
import { constT, UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekDirectBuiltin,
  hashMidgardCekDirectArguments,
  verifyMidgardCekDirectBuiltinFailure,
  type MidgardCekDirectValueWitness,
} from "../src/cek-builtin.js";
import {
  encodeMidgardCekPlutusData,
  midgardCekConstantWitnessFromUplc,
} from "../src/cek-constant.js";

const constant = (value: UPLCConst): MidgardCekDirectValueWitness => ({
  kind: "constant",
  witness: midgardCekConstantWitnessFromUplc(value),
});
const failureIsProvable = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const committed = hashMidgardCekDirectArguments(arguments_);
  return verifyMidgardCekDirectBuiltinFailure(
    tag,
    hashMidgardCekValueNode({
      kind: "builtin",
      tag,
      forcesRemaining: 0n,
      argumentsCount: committed.count,
      argumentsRoot: committed.root,
    }),
    arguments_,
  );
};
const scalarLimit = 1n << 4095n;
const groups = [
  {
    name: "G1",
    tag: 56n,
    type: "9f09ff",
    generator:
      "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
  },
  {
    name: "G2",
    tag: 63n,
    type: "9f0aff",
    generator:
      "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
  },
] as const;

describe("Plutus V3 variant E argument bounds", () => {
  it.each([
    [4095, "success"],
    [4096, "success"],
    [4097, "failure"],
  ] as const)("writeBits input length %i gives %s", (length, kind) => {
    const arguments_ = [
      constant(UPLCConst.byteString(new Uint8Array(length))),
      constant(UPLCConst.listOf(constT.int)([0n])),
      constant(UPLCConst.bool(true)),
    ];
    const result = evaluateMidgardCekDirectBuiltin(80n, arguments_);
    expect(result.kind).toBe(kind);
    if (kind === "failure")
      expect(failureIsProvable(80n, arguments_)).toBe(false);
    // These are denotation failures, after the builtin charge.
    expect(result.budget.cpu).toBeGreaterThan(0n);
    expect(result.budget.memory).toBeGreaterThan(0n);
  });

  it.each([-1n, 32776n])(
    "charges the size failure even when writeBits index %s also fails",
    (index) => {
      const arguments_ = [
        constant(UPLCConst.byteString(new Uint8Array(4097))),
        constant(UPLCConst.listOf(constT.int)([index])),
        constant(UPLCConst.bool(true)),
      ];
      const result = evaluateMidgardCekDirectBuiltin(80n, arguments_);
      expect(result.kind).toBe("failure");
      expect(result.budget.cpu).toBeGreaterThan(0n);
      expect(result.budget.memory).toBeGreaterThan(0n);
      expect(failureIsProvable(80n, arguments_)).toBe(false);
    },
  );

  it("does not charge a writeBits size failure before the boolean argument unlifts", () => {
    const arguments_ = [
      constant(UPLCConst.byteString(new Uint8Array(4097))),
      constant(UPLCConst.listOf(constT.int)([0n])),
      constant(UPLCConst.unit),
    ];
    expect(evaluateMidgardCekDirectBuiltin(80n, arguments_)).toEqual({
      kind: "failure",
      budget: { cpu: 0n, memory: 0n },
    });
    expect(failureIsProvable(80n, arguments_)).toBe(false);
  });

  for (const group of groups) {
    it.each([
      ["lower outside", -scalarLimit - 1n, "failure"],
      ["lower bound", -scalarLimit, "success"],
      ["upper bound", scalarLimit - 1n, "success"],
      ["upper outside", scalarLimit, "failure"],
    ] as const)(`${group.name} scalarMul %s`, (_name, scalar, kind) => {
      const point: MidgardCekDirectValueWitness = {
        kind: "constant",
        witness: {
          typeCbor: Buffer.from(group.type, "hex"),
          payloadCbor: encodeMidgardCekPlutusData(
            new DataB(Buffer.from(group.generator, "hex")),
          ),
        },
      };
      const arguments_ = [constant(UPLCConst.int(scalar)), point];
      const result = evaluateMidgardCekDirectBuiltin(group.tag, arguments_);
      expect(result.kind).toBe(kind);
      // C14 owns the corresponding on-chain failure arms; they do not exist yet.
      if (kind === "failure")
        expect(failureIsProvable(group.tag, arguments_)).toBe(false);
      expect(result.budget.cpu).toBeGreaterThan(0n);
      expect(result.budget.memory).toBeGreaterThan(0n);
    });
  }
});
