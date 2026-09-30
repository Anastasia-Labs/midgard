import {
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
} from "@al-ft/midgard-core";
import { DataB, DataI, DataList } from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  hashMidgardCekDirectValueWitness,
  hashMidgardCekRuntimeArguments,
  type MidgardCekDirectValueWitness,
  type MidgardCekRuntimeValueWitness,
  verifyMidgardCekBuiltinTypeFailure,
} from "../src/cek-builtin.js";
import { midgardCekConstantWitnessFromUplc } from "../src/cek-constant.js";

export const hash = (fill: number): Buffer => Buffer.alloc(32, fill);

const integer = (payloadHex: string): MidgardCekRuntimeValueWitness => ({
  kind: "constant",
  witness: {
    typeCbor: Buffer.from("9f00ff", "hex"),
    payloadCbor: Buffer.from(payloadHex, "hex"),
  },
});

const bytes = (payloadHex: string): MidgardCekRuntimeValueWitness => ({
  kind: "constant",
  witness: {
    typeCbor: Buffer.from("9f01ff", "hex"),
    payloadCbor: Buffer.from(payloadHex, "hex"),
  },
});

export const builtinRoot = (
  tag: bigint,
  arguments_: readonly MidgardCekRuntimeValueWitness[],
): Uint8Array => {
  const { root, count } = hashMidgardCekRuntimeArguments(arguments_);
  return hashMidgardCekValueNode({
    kind: "builtin",
    tag,
    forcesRemaining: 0n,
    argumentsCount: count,
    argumentsRoot: root,
  });
};

describe("V1 builtin runtime type failures", () => {
  it("authenticates a closure supplied to addInteger", () => {
    const arguments_: readonly MidgardCekRuntimeValueWitness[] = [
      {
        kind: "lambda",
        body: hash(1),
        environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      },
      integer("01"),
    ];
    expect(
      verifyMidgardCekBuiltinTypeFailure(
        0n,
        builtinRoot(0n, arguments_),
        arguments_,
      ),
    ).toBe(true);
  });

  it("rejects an incongruent mkCons element type", () => {
    const arguments_: readonly MidgardCekRuntimeValueWitness[] = [
      bytes("4101"),
      {
        kind: "constant",
        witness: {
          typeCbor: Buffer.from("9f0500ff", "hex"),
          payloadCbor: Buffer.from("9f01ff", "hex"),
        },
      },
    ];
    expect(
      verifyMidgardCekBuiltinTypeFailure(
        32n,
        builtinRoot(32n, arguments_),
        arguments_,
      ),
    ).toBe(true);
  });

  it("does not misclassify arbitrary control branches", () => {
    const arguments_: readonly MidgardCekRuntimeValueWitness[] = [
      {
        kind: "constant",
        witness: {
          typeCbor: Buffer.from("9f04ff", "hex"),
          payloadCbor: Buffer.from("d87a80", "hex"),
        },
      },
      {
        kind: "delay",
        body: hash(1),
        environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      },
      {
        kind: "constr",
        tag: 0n,
        valuesCount: 0n,
        valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
      },
    ];
    expect(
      verifyMidgardCekBuiltinTypeFailure(
        26n,
        builtinRoot(26n, arguments_),
        arguments_,
      ),
    ).toBe(false);
  });

  it("fails closed on a malformed or mismatched commitment", () => {
    expect(
      verifyMidgardCekBuiltinTypeFailure(0n, hash(9), [
        integer("01"),
        integer("02"),
      ]),
    ).toBe(false);
    expect(
      verifyMidgardCekBuiltinTypeFailure(0n, hash(9), [
        integer("1817"),
        integer("02"),
      ]),
    ).toBe(false);
  });
});

export const direct = (constant: UPLCConst): MidgardCekDirectValueWitness => ({
  kind: "constant",
  witness: midgardCekConstantWitnessFromUplc(constant),
});

export const directByteString = (
  byteLength: number,
): MidgardCekDirectValueWitness =>
  direct(UPLCConst.byteString(new DataB(Buffer.alloc(byteLength)).bytes));

export const directDataList = (count: number): MidgardCekDirectValueWitness =>
  direct(
    UPLCConst.data(
      new DataList(Array.from({ length: count }, () => new DataI(0n))),
    ),
  );

export const runtimeByteString = (
  byteLength: number,
): MidgardCekRuntimeValueWitness => {
  const value = directByteString(byteLength);
  if (value.kind !== "constant") throw new Error("expected a direct constant");
  return { kind: "constant", witness: value.witness };
};

export const directBuiltinRoot = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): Uint8Array => {
  let root: Uint8Array = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT;
  let count = 0n;
  for (const argument of arguments_) {
    count += 1n;
    root = hashMidgardCekSequenceNode({
      head: hashMidgardCekDirectValueWitness(argument),
      tail: root,
      length: count,
    });
  }
  return hashMidgardCekValueNode({
    kind: "builtin",
    tag,
    forcesRemaining: 0n,
    argumentsCount: count,
    argumentsRoot: root,
  });
};
