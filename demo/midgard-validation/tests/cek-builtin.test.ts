import {
  hashMidgardCekBlsExpressionNode,
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
} from "@al-ft/midgard-core";
import { DataB, DataConstr, DataI, DataList } from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekBlsFinal,
  evaluateMidgardCekDirectBuiltin,
  hashMidgardCekDirectValueWitness,
  hashMidgardCekRuntimeArguments,
  type MidgardCekBlsExpressionWitness,
  type MidgardCekDirectValueWitness,
  type MidgardCekRuntimeValueWitness,
  verifyMidgardCekBlsFinal,
  verifyMidgardCekBuiltinTypeFailure,
  verifyMidgardCekDirectBuiltin,
  verifyMidgardCekDirectBuiltinFailure,
} from "../src/cek-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
  midgardCekConstantWitnessFromUplc,
} from "../src/cek-constant.js";

const hash = (fill: number): Buffer => Buffer.alloc(32, fill);

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

const builtinRoot = (
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

const direct = (constant: UPLCConst): MidgardCekDirectValueWitness => ({
  kind: "constant",
  witness: midgardCekConstantWitnessFromUplc(constant),
});

const directByteString = (byteLength: number): MidgardCekDirectValueWitness =>
  direct(UPLCConst.byteString(new DataB(Buffer.alloc(byteLength)).bytes));

const directDataList = (count: number): MidgardCekDirectValueWitness =>
  direct(
    UPLCConst.data(
      new DataList(Array.from({ length: count }, () => new DataI(0n))),
    ),
  );

const runtimeByteString = (
  byteLength: number,
): MidgardCekRuntimeValueWitness => {
  const value = directByteString(byteLength);
  if (value.kind !== "constant") throw new Error("expected a direct constant");
  return { kind: "constant", witness: value.witness };
};

const directBuiltinRoot = (
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

describe("V1 direct builtin execution", () => {
  it("replays a successful integer builtin and its exact budget", () => {
    const arguments_ = [direct(UPLCConst.int(1)), direct(UPLCConst.int(128))];
    const evaluated = evaluateMidgardCekDirectBuiltin(0n, arguments_);
    expect(evaluated.kind).toBe("success");
    if (evaluated.kind !== "success") return;
    expect(evaluated.result.kind).toBe("constant");
    if (evaluated.result.kind !== "constant") return;
    const result = decodeMidgardCekConstantWitness(evaluated.result.witness);
    expect(result.payload).toMatchObject({ int: 129n });
    expect(
      verifyMidgardCekDirectBuiltin(
        0n,
        directBuiltinRoot(0n, arguments_),
        arguments_,
        evaluated.result,
      ),
    ).toBe(true);
    expect(evaluated.budget).toEqual({ cpu: 101_628n, memory: 3n });
  });

  it("distinguishes paid division failures from zero-cost shape failures", () => {
    const division = [direct(UPLCConst.int(1)), direct(UPLCConst.int(0))];
    const evaluated = evaluateMidgardCekDirectBuiltin(4n, division);
    expect(evaluated.kind).toBe("failure");
    if (evaluated.kind !== "failure") return;
    expect(evaluated.budget.cpu).toBeGreaterThan(0n);
    expect(
      verifyMidgardCekDirectBuiltinFailure(
        4n,
        directBuiltinRoot(4n, division),
        division,
      ),
    ).toBe(true);

    const invalidByte = [
      direct(UPLCConst.int(256)),
      direct(UPLCConst.byteString(new DataB(Buffer.alloc(0)).bytes)),
    ];
    const shapeFailure = evaluateMidgardCekDirectBuiltin(11n, invalidByte);
    expect(shapeFailure).toEqual({
      kind: "failure",
      budget: { cpu: 0n, memory: 0n },
    });
  });

  it("selects an opaque control branch without inspecting it", () => {
    const selected = { kind: "opaque", root: hash(7) } as const;
    const arguments_: readonly MidgardCekDirectValueWitness[] = [
      direct(UPLCConst.bool(true)),
      selected,
      { kind: "opaque", root: hash(8) },
    ];
    const evaluated = evaluateMidgardCekDirectBuiltin(26n, arguments_);
    expect(evaluated.kind).toBe("success");
    if (evaluated.kind !== "success") return;
    expect(evaluated.result).toEqual(selected);
    expect(
      verifyMidgardCekDirectBuiltin(
        26n,
        directBuiltinRoot(26n, arguments_),
        arguments_,
        selected,
      ),
    ).toBe(true);
  });

  it("bridges compressed BLS constants and commits miller-loop expressions", () => {
    const g1: MidgardCekDirectValueWitness = {
      kind: "constant",
      witness: {
        typeCbor: Buffer.from("9f09ff", "hex"),
        payloadCbor: Buffer.from(
          "583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
          "hex",
        ),
      },
    };
    const g2: MidgardCekDirectValueWitness = {
      kind: "constant",
      witness: {
        typeCbor: Buffer.from("9f0aff", "hex"),
        payloadCbor: Buffer.from(
          "5f584093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc510515820c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8ff",
          "hex",
        ),
      },
    };

    const added = evaluateMidgardCekDirectBuiltin(54n, [g1, g1]);
    expect(added.kind).toBe("success");
    if (added.kind === "success" && added.result.kind === "constant") {
      expect(
        decodeMidgardCekConstantWitness(added.result.witness).type.kind,
      ).toBe("blsG1");
    }

    const miller = evaluateMidgardCekDirectBuiltin(68n, [g1, g2]);
    expect(miller.kind).toBe("success");
    if (miller.kind !== "success") return;
    expect(miller.result.kind).toBe("blsMillerLoop");
    if (miller.result.kind !== "blsMillerLoop") return;
    const expressionRoot = miller.result.expressionRoot;
    expect(
      verifyMidgardCekDirectBuiltin(
        68n,
        directBuiltinRoot(68n, [g1, g2]),
        [g1, g2],
        miller.result,
      ),
    ).toBe(true);

    if (g1.kind !== "constant" || g2.kind !== "constant") return;
    const leaf: MidgardCekBlsExpressionWitness = {
      kind: "millerLoop",
      g1: g1.witness,
      g2: g2.witness,
    };
    const finalized = evaluateMidgardCekBlsFinal(
      expressionRoot,
      expressionRoot,
      leaf,
      leaf,
    );
    expect(finalized.result.kind).toBe("constant");
    if (finalized.result.kind !== "constant") return;
    expect(
      decodeMidgardCekConstantWitness(finalized.result.witness).payload,
    ).toEqual(new DataConstr(1, []));
    expect(
      verifyMidgardCekBlsFinal(
        directBuiltinRoot(70n, [miller.result, miller.result]),
        expressionRoot,
        expressionRoot,
        leaf,
        leaf,
        finalized.result,
      ),
    ).toBe(true);

    let overLimit: MidgardCekBlsExpressionWitness = leaf;
    let overLimitRoot = expressionRoot;
    for (let index = 1; index < 10; index += 1) {
      overLimit = {
        kind: "multiply",
        left: overLimit,
        right: leaf,
      };
      overLimitRoot = hashMidgardCekBlsExpressionNode({
        kind: "multiply",
        left: overLimitRoot,
        right: expressionRoot,
      });
    }
    expect(() =>
      evaluateMidgardCekBlsFinal(
        overLimitRoot,
        expressionRoot,
        overLimit,
        leaf,
      ),
    ).toThrow(/ten-leaf L1 proof reserve/);
  });

  it("rejects an over-cap runtime type-failure witness", () => {
    const arguments_ = [
      runtimeByteString(4_608),
      runtimeByteString(4_608),
    ] as const;
    expect(
      arguments_.every(
        (argument) =>
          argument.kind === "constant" &&
          argument.witness.payloadCbor.length <=
            MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
      ),
    ).toBe(true);
    expect(
      verifyMidgardCekBuiltinTypeFailure(
        0n,
        builtinRoot(0n, arguments_),
        arguments_,
      ),
    ).toBe(false);
  });

  it("rejects a non-51 oversized direct result", () => {
    const arguments_ = [
      direct(UPLCConst.bool(true)),
      directByteString(4_608),
      directByteString(1),
    ] as const;
    expect(() => evaluateMidgardCekDirectBuiltin(26n, arguments_)).toThrow(
      /aggregate direct payload bound/,
    );
    expect(
      verifyMidgardCekDirectBuiltin(
        26n,
        directBuiltinRoot(26n, arguments_),
        arguments_,
        directByteString(4_608),
      ),
    ).toBe(false);
  });

  it("rejects an over-cap direct failure witness", () => {
    const arguments_ = [
      directByteString(3_200),
      directByteString(3_200),
      directByteString(3_200),
    ] as const;
    expect(() => evaluateMidgardCekDirectBuiltin(52n, arguments_)).toThrow(
      /aggregate direct payload bound/,
    );
    expect(
      verifyMidgardCekDirectBuiltinFailure(
        52n,
        directBuiltinRoot(52n, arguments_),
        arguments_,
      ),
    ).toBe(false);
  });

  it("accepts under-cap aggregates and does not count semantic constants", () => {
    const runtimeArguments = [
      runtimeByteString(4_400),
      runtimeByteString(4_400),
    ] as const;
    expect(
      verifyMidgardCekBuiltinTypeFailure(
        0n,
        builtinRoot(0n, runtimeArguments),
        runtimeArguments,
      ),
    ).toBe(true);

    const directSuccessArguments = [
      direct(UPLCConst.bool(true)),
      directByteString(4_400),
      directByteString(1),
    ] as const;
    const directSuccess = evaluateMidgardCekDirectBuiltin(
      26n,
      directSuccessArguments,
    );
    expect(directSuccess.kind).toBe("success");
    if (directSuccess.kind !== "success") return;
    expect(
      verifyMidgardCekDirectBuiltin(
        26n,
        directBuiltinRoot(26n, directSuccessArguments),
        directSuccessArguments,
        directSuccess.result,
      ),
    ).toBe(true);

    const directFailureArguments = [
      directByteString(2_800),
      directByteString(2_800),
      directByteString(2_800),
    ] as const;
    expect(
      verifyMidgardCekDirectBuiltinFailure(
        52n,
        directBuiltinRoot(52n, directFailureArguments),
        directFailureArguments,
      ),
    ).toBe(true);

    const semanticBranch = {
      kind: "semanticConstant",
      witness: {
        typeCbor: Buffer.from("9f01ff", "hex"),
        payload: {
          root: hash(12),
          cborLength: BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES + 1),
          memory: 1n,
        },
        memory: 1n,
      },
    } as const;
    const semanticArguments = [
      direct(UPLCConst.bool(true)),
      semanticBranch,
      semanticBranch,
    ] as const;
    const semanticResult = evaluateMidgardCekDirectBuiltin(
      26n,
      semanticArguments,
    );
    expect(semanticResult.kind).toBe("success");
    if (semanticResult.kind !== "success") return;
    expect(semanticResult.result).toEqual(semanticBranch);
    expect(
      verifyMidgardCekDirectBuiltin(
        26n,
        directBuiltinRoot(26n, semanticArguments),
        semanticArguments,
        semanticBranch,
      ),
    ).toBe(true);
  });

  it("keeps tag-51 direct results under the aggregate boundary", () => {
    const arguments_ = [directDataList(0)];
    const input = arguments_[0]!;
    if (input.kind !== "constant") throw new Error("expected direct input");
    expect(input.witness.payloadCbor.length).toBeLessThanOrEqual(
      MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
    );
    const evaluated = evaluateMidgardCekDirectBuiltin(51n, arguments_);
    expect(evaluated.kind).toBe("success");
    if (evaluated.kind !== "success") return;
    expect(evaluated.result.kind).toBe("constant");
    expect(
      verifyMidgardCekDirectBuiltin(
        51n,
        directBuiltinRoot(51n, arguments_),
        arguments_,
        evaluated.result,
      ),
    ).toBe(true);
  });

  it("semanticizes an oversized tag-51 result without weakening argument admission", () => {
    const arguments_ = [directDataList(9_000)];
    const input = arguments_[0]!;
    if (input.kind !== "constant") throw new Error("expected direct input");
    expect(input.witness.payloadCbor.length).toBe(9_002);
    expect(input.witness.payloadCbor.length).toBeLessThanOrEqual(
      MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
    );

    const evaluated = evaluateMidgardCekDirectBuiltin(51n, arguments_);
    expect(evaluated.kind).toBe("success");
    if (evaluated.kind !== "success") return;
    expect(evaluated.result.kind).toBe("semanticConstant");
    if (evaluated.result.kind !== "semanticConstant") return;
    expect(evaluated.result.witness.typeCbor).toEqual(
      Buffer.from("9f01ff", "hex"),
    );
    expect(evaluated.result.witness.payload.cborLength).toBe(9_286n);
    expect(evaluated.budget).toEqual({
      cpu: 9_600_848_754n,
      memory: 90_008n,
    });
    expect(
      verifyMidgardCekDirectBuiltin(
        51n,
        directBuiltinRoot(51n, arguments_),
        arguments_,
        evaluated.result,
      ),
    ).toBe(true);
  });
});
