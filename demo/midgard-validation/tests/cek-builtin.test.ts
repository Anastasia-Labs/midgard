import {
  hashMidgardCekBlsExpressionNodeV1,
  hashMidgardCekSequenceNodeV1,
  hashMidgardCekValueNodeV1,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
} from "@al-ft/midgard-core";
import { DataB, DataConstr } from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekBlsFinalV1,
  evaluateMidgardCekDirectBuiltinV1,
  hashMidgardCekDirectValueWitnessV1,
  hashMidgardCekRuntimeArgumentsV1,
  type MidgardCekBlsExpressionWitnessV1,
  type MidgardCekDirectValueWitnessV1,
  type MidgardCekRuntimeValueWitnessV1,
  verifyMidgardCekBlsFinalV1,
  verifyMidgardCekBuiltinTypeFailureV1,
  verifyMidgardCekDirectBuiltinFailureV1,
  verifyMidgardCekDirectBuiltinV1,
} from "../src/cek-builtin.js";
import {
  decodeMidgardCekConstantWitnessV1,
  midgardCekConstantWitnessFromUplcV1,
} from "../src/cek-constant.js";

const hash = (fill: number): Buffer => Buffer.alloc(32, fill);

const integer = (
  payloadHex: string,
): MidgardCekRuntimeValueWitnessV1 => ({
  kind: "constant",
  witness: {
    typeCbor: Buffer.from("9f00ff", "hex"),
    payloadCbor: Buffer.from(payloadHex, "hex"),
  },
});

const bytes = (
  payloadHex: string,
): MidgardCekRuntimeValueWitnessV1 => ({
  kind: "constant",
  witness: {
    typeCbor: Buffer.from("9f01ff", "hex"),
    payloadCbor: Buffer.from(payloadHex, "hex"),
  },
});

const builtinRoot = (
  tag: bigint,
  arguments_: readonly MidgardCekRuntimeValueWitnessV1[],
): Uint8Array => {
  const { root, count } =
    hashMidgardCekRuntimeArgumentsV1(arguments_);
  return hashMidgardCekValueNodeV1({
    kind: "builtin",
    tag,
    forcesRemaining: 0n,
    argumentsCount: count,
    argumentsRoot: root,
  });
};

describe("V1 builtin runtime type failures", () => {
  it("authenticates a closure supplied to addInteger", () => {
    const arguments_: readonly MidgardCekRuntimeValueWitnessV1[] = [
      {
        kind: "lambda",
        body: hash(1),
        environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      },
      integer("01"),
    ];
    expect(
      verifyMidgardCekBuiltinTypeFailureV1(
        0n,
        builtinRoot(0n, arguments_),
        arguments_,
      ),
    ).toBe(true);
  });

  it("rejects an incongruent mkCons element type", () => {
    const arguments_: readonly MidgardCekRuntimeValueWitnessV1[] = [
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
      verifyMidgardCekBuiltinTypeFailureV1(
        32n,
        builtinRoot(32n, arguments_),
        arguments_,
      ),
    ).toBe(true);
  });

  it("does not misclassify arbitrary control branches", () => {
    const arguments_: readonly MidgardCekRuntimeValueWitnessV1[] = [
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
        environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      },
      {
        kind: "constr",
        tag: 0n,
        valuesCount: 0n,
        valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
      },
    ];
    expect(
      verifyMidgardCekBuiltinTypeFailureV1(
        26n,
        builtinRoot(26n, arguments_),
        arguments_,
      ),
    ).toBe(false);
  });

  it("fails closed on a malformed or mismatched commitment", () => {
    expect(
      verifyMidgardCekBuiltinTypeFailureV1(
        0n,
        hash(9),
        [integer("01"), integer("02")],
      ),
    ).toBe(false);
    expect(
      verifyMidgardCekBuiltinTypeFailureV1(
        0n,
        hash(9),
        [integer("1817"), integer("02")],
      ),
    ).toBe(false);
  });
});

const direct = (constant: UPLCConst): MidgardCekDirectValueWitnessV1 => ({
  kind: "constant",
  witness: midgardCekConstantWitnessFromUplcV1(constant),
});

const directBuiltinRoot = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): Uint8Array => {
  let root: Uint8Array = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1;
  let count = 0n;
  for (const argument of arguments_) {
    count += 1n;
    root = hashMidgardCekSequenceNodeV1({
      head: hashMidgardCekDirectValueWitnessV1(argument),
      tail: root,
      length: count,
    });
  }
  return hashMidgardCekValueNodeV1({
    kind: "builtin",
    tag,
    forcesRemaining: 0n,
    argumentsCount: count,
    argumentsRoot: root,
  });
};

describe("V1 direct builtin execution", () => {
  it("replays a successful integer builtin and its exact budget", () => {
    const arguments_ = [direct(UPLCConst.int(41)), direct(UPLCConst.int(1))];
    const evaluated = evaluateMidgardCekDirectBuiltinV1(0n, arguments_);
    expect(evaluated.kind).toBe("success");
    if (evaluated.kind !== "success") return;
    expect(evaluated.result.kind).toBe("constant");
    if (evaluated.result.kind !== "constant") return;
    const result = decodeMidgardCekConstantWitnessV1(
      evaluated.result.witness,
    );
    expect(result.payload).toMatchObject({ int: 42n });
    expect(
      verifyMidgardCekDirectBuiltinV1(
        0n,
        directBuiltinRoot(0n, arguments_),
        arguments_,
        evaluated.result,
      ),
    ).toBe(true);
    expect(evaluated.budget.cpu).toBeGreaterThan(0n);
    expect(evaluated.budget.memory).toBeGreaterThan(0n);
  });

  it("distinguishes paid division failures from zero-cost shape failures", () => {
    const division = [direct(UPLCConst.int(1)), direct(UPLCConst.int(0))];
    const evaluated = evaluateMidgardCekDirectBuiltinV1(4n, division);
    expect(evaluated.kind).toBe("failure");
    if (evaluated.kind !== "failure") return;
    expect(evaluated.budget.cpu).toBeGreaterThan(0n);
    expect(
      verifyMidgardCekDirectBuiltinFailureV1(
        4n,
        directBuiltinRoot(4n, division),
        division,
      ),
    ).toBe(true);

    const invalidByte = [
      direct(UPLCConst.int(256)),
      direct(UPLCConst.byteString(new DataB(Buffer.alloc(0)).bytes)),
    ];
    const shapeFailure = evaluateMidgardCekDirectBuiltinV1(
      11n,
      invalidByte,
    );
    expect(shapeFailure).toEqual({
      kind: "failure",
      budget: { cpu: 0n, memory: 0n },
    });
  });

  it("selects an opaque control branch without inspecting it", () => {
    const selected = { kind: "opaque", root: hash(7) } as const;
    const arguments_: readonly MidgardCekDirectValueWitnessV1[] = [
      direct(UPLCConst.bool(true)),
      selected,
      { kind: "opaque", root: hash(8) },
    ];
    const evaluated = evaluateMidgardCekDirectBuiltinV1(26n, arguments_);
    expect(evaluated.kind).toBe("success");
    if (evaluated.kind !== "success") return;
    expect(evaluated.result).toEqual(selected);
    expect(
      verifyMidgardCekDirectBuiltinV1(
        26n,
        directBuiltinRoot(26n, arguments_),
        arguments_,
        selected,
      ),
    ).toBe(true);
  });

  it("bridges compressed BLS constants and commits miller-loop expressions", () => {
    const g1: MidgardCekDirectValueWitnessV1 = {
      kind: "constant",
      witness: {
        typeCbor: Buffer.from("9f09ff", "hex"),
        payloadCbor: Buffer.from(
          "583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
          "hex",
        ),
      },
    };
    const g2: MidgardCekDirectValueWitnessV1 = {
      kind: "constant",
      witness: {
        typeCbor: Buffer.from("9f0aff", "hex"),
        payloadCbor: Buffer.from(
          "5f584093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc510515820c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8ff",
          "hex",
        ),
      },
    };

    const added = evaluateMidgardCekDirectBuiltinV1(54n, [g1, g1]);
    expect(added.kind).toBe("success");
    if (added.kind === "success" && added.result.kind === "constant") {
      expect(
        decodeMidgardCekConstantWitnessV1(added.result.witness).type.kind,
      ).toBe("blsG1");
    }

    const miller = evaluateMidgardCekDirectBuiltinV1(68n, [g1, g2]);
    expect(miller.kind).toBe("success");
    if (miller.kind !== "success") return;
    expect(miller.result.kind).toBe("blsMillerLoop");
    if (miller.result.kind !== "blsMillerLoop") return;
    const expressionRoot = miller.result.expressionRoot;
    expect(
      verifyMidgardCekDirectBuiltinV1(
        68n,
        directBuiltinRoot(68n, [g1, g2]),
        [g1, g2],
        miller.result,
      ),
    ).toBe(true);

    if (g1.kind !== "constant" || g2.kind !== "constant") return;
    const leaf: MidgardCekBlsExpressionWitnessV1 = {
      kind: "millerLoop",
      g1: g1.witness,
      g2: g2.witness,
    };
    const finalized = evaluateMidgardCekBlsFinalV1(
      expressionRoot,
      expressionRoot,
      leaf,
      leaf,
    );
    expect(finalized.result.kind).toBe("constant");
    if (finalized.result.kind !== "constant") return;
    expect(
      decodeMidgardCekConstantWitnessV1(finalized.result.witness).payload,
    ).toEqual(new DataConstr(1, []));
    expect(
      verifyMidgardCekBlsFinalV1(
        directBuiltinRoot(70n, [miller.result, miller.result]),
        expressionRoot,
        expressionRoot,
        leaf,
        leaf,
        finalized.result,
      ),
    ).toBe(true);

    let overLimit: MidgardCekBlsExpressionWitnessV1 = leaf;
    let overLimitRoot = expressionRoot;
    for (let index = 1; index < 10; index += 1) {
      overLimit = {
        kind: "multiply",
        left: overLimit,
        right: leaf,
      };
      overLimitRoot = hashMidgardCekBlsExpressionNodeV1({
        kind: "multiply",
        left: overLimitRoot,
        right: expressionRoot,
      });
    }
    expect(() =>
      evaluateMidgardCekBlsFinalV1(
        overLimitRoot,
        expressionRoot,
        overLimit,
        leaf,
      ),
    ).toThrow(/ten-leaf L1 proof reserve/);
  });
});
