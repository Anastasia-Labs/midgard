import {
  hashMidgardCekBlsExpressionNode,
  MIDGARD_CEK_MAX_RESOLVED_BUILTIN_TAG,
} from "@al-ft/midgard-core";

import {
  directWitnessPayloadBytes,
  type MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import {
  type CardanoExactValue,
  evaluateCardanoExactBuiltin,
  MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS,
} from "./cek-builtin.cardano-exact.js";
import { MIDGARD_CEK_CARDANO_BLS_TAGS } from "./cek-builtin.cardano-tags.js";
import {
  compactedDividendFailure,
  compactedSourceFailure,
  midgardCekDivisionByZero,
  midgardCekScalarMulOutOfBound,
} from "./cek-builtin.compacted-failures.js";
import {
  evaluatePinnedBuiltin,
  runPinnedBuiltin,
} from "./cek-builtin.pinned-evaluator.js";
import {
  directByteLength,
  directInteger,
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
  type MidgardCekDirectBuiltinEvaluation,
  selectedControlResult,
} from "./cek-builtin.selected-control-result.js";
import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekCanonicalConstant,
  encodeMidgardCekPlutusData,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
  midgardCekConstantMemorySize,
  type MidgardCekConstantWitness,
  midgardCekConstantWitnessFromUplc,
  midgardCekConstantWitnessToUplc,
} from "./cek-constant.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import { midgardCekBuiltinArgumentCount } from "./cek-machine.midgard-cek-builtin-argument-count.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { ConstTyTag, UPLCConst } from "./uplc-constant.js";
import { DataB } from "./uplc-data.js";

/**
 * The constant value of a direct constant witness, as the builtins take it.
 * A BLS point stays its compressed bytes and must uncompress under the L1
 * rule (Cardano's uncompress builtin).
 */
export const directConstantToBuiltinValue = (
  witness: MidgardCekConstantWitness,
): UPLCConst => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  if (decoded.type.kind === "bytes") {
    if (!(decoded.payload instanceof DataB)) {
      throw new Error("V1 byte-string payload is not bytes");
    }
    return UPLCConst.byteString(Uint8Array.from(decoded.payload.bytes));
  }
  if (decoded.type.kind !== "blsG1" && decoded.type.kind !== "blsG2") {
    return midgardCekConstantWitnessToUplc(witness);
  }
  if (!(decoded.payload instanceof DataB)) {
    throw new Error("V1 BLS payload is not bytes");
  }
  const uncompressed = evaluatePinnedBuiltin(
    decoded.type.kind === "blsG1"
      ? MIDGARD_CEK_CARDANO_BLS_TAGS.g1Uncompress
      : MIDGARD_CEK_CARDANO_BLS_TAGS.g2Uncompress,
    [UPLCConst.byteString(Uint8Array.from(decoded.payload.bytes))],
  );
  if (uncompressed.kind === "failure") {
    throw new Error(
      `V1 BLS constant has an invalid encoding: ${uncompressed.message}`,
    );
  }
  return uncompressed.value;
};

const cardanoExactArgument = (reference: UPLCConst): CardanoExactValue => {
  const type = reference.type[0];
  if (type === ConstTyTag.int) {
    return { kind: "integer", value: reference.value as bigint };
  }
  if (type === ConstTyTag.byteStr) {
    return {
      kind: "bytes",
      value: Uint8Array.from(reference.value as Uint8Array),
    };
  }
  if (type === ConstTyTag.bool) {
    return { kind: "bool", value: reference.value as boolean };
  }
  throw new Error("V1 builtin argument has the wrong constant type");
};

const cardanoExactResult = (value: CardanoExactValue): UPLCConst =>
  value.kind === "integer"
    ? UPLCConst.int(value.value)
    : value.kind === "bytes"
      ? UPLCConst.byteString(value.value)
      : UPLCConst.bool(value.value);

const semanticConstantFromCanonical = (
  canonical: ReturnType<typeof encodeMidgardCekCanonicalConstant>,
): MidgardCekDirectValueWitness => {
  const payload = plutusDataFromCborIterative(canonical.payloadCbor);
  const tree = commitMidgardCekDataTree(payload);
  return Object.freeze({
    kind: "semanticConstant" as const,
    witness: Object.freeze({
      typeCbor: canonical.typeCbor,
      payload: Object.freeze({
        root: tree.root,
        cborLength: tree.cborLength,
        memory: tree.memory,
      }),
      memory: midgardCekConstantMemorySize(canonical.type, payload),
    }),
  });
};

export const semanticizeMidgardCekDirectConstant = (
  value: MidgardCekDirectValueWitness,
): MidgardCekDirectValueWitness => {
  if (value.kind !== "constant") return value;
  const decoded = decodeMidgardCekConstantWitness(value.witness);
  const tree = commitMidgardCekDataTree(decoded.payload);
  return Object.freeze({
    kind: "semanticConstant" as const,
    witness: Object.freeze({
      typeCbor: value.witness.typeCbor,
      payload: Object.freeze({
        root: tree.root,
        cborLength: tree.cborLength,
        memory: tree.memory,
      }),
      memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
    }),
  });
};

export const builtinValueToDirectWitness = (
  constant: UPLCConst,
  allowSemantic: boolean = false,
): MidgardCekDirectValueWitness => {
  const tag = constant.type[0];
  if (
    tag !== ConstTyTag.bls12_381_G1_element &&
    tag !== ConstTyTag.bls12_381_G2_element
  ) {
    const canonical = encodeMidgardCekCanonicalConstant(constant);
    const witness = Object.freeze({
      typeCbor: canonical.typeCbor,
      payloadCbor: canonical.payloadCbor,
    });
    if (
      canonical.payloadCbor.length >
      MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES
    ) {
      if (allowSemantic) {
        return semanticConstantFromCanonical(canonical);
      }
    }
    decodeMidgardCekConstantWitness(witness);
    return { kind: "constant", witness };
  }
  // A point is held as its compressed bytes.
  const bytesWitness = midgardCekConstantWitnessFromUplc(
    UPLCConst.byteString(constant.value as Uint8Array),
  );
  const witness = Object.freeze({
    typeCbor: Buffer.from(
      tag === ConstTyTag.bls12_381_G1_element ? "9f09ff" : "9f0aff",
      "hex",
    ),
    payloadCbor: bytesWitness.payloadCbor,
  });
  decodeMidgardCekConstantWitness(witness);
  return { kind: "constant", witness };
};

/**
 * Cardano's builtin `tag` (flat numbering) on constant values, or "failure"
 * where Cardano's builtin fails. It is the one value-level relation of the
 * constant builtins: the direct route computes through it on revealed
 * constants and the staged route on operands bound from their leaves.
 */
export const evaluateMidgardCekBuiltinOnValues = (
  tag: number,
  arguments_: readonly UPLCConst[],
): UPLCConst | "failure" => {
  if (MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS.has(tag)) {
    const result = evaluateCardanoExactBuiltin(
      tag,
      arguments_.map(cardanoExactArgument),
    );
    return result === "failure" ? "failure" : cardanoExactResult(result);
  }
  return runPinnedBuiltin(tag, arguments_);
};

/**
 * The arguments of a direct step of builtin `tag`: those given, except that
 * a division by zero presents a dividend past the direct payload bound by
 * its leaf, as the direct failure arm reads only its type and memory.
 */
export const midgardCekDirectStepArguments = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): readonly MidgardCekDirectValueWitness[] =>
  midgardCekDivisionByZero(tag, arguments_) &&
  directWitnessPayloadBytes(arguments_) >
    BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES)
    ? [semanticizeMidgardCekDirectConstant(arguments_[0]!), arguments_[1]!]
    : arguments_;

const evaluateDirectBuiltinResult = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): MidgardCekDirectValueWitness | "failure" => {
  const control = selectedControlResult(tag, arguments_);
  if (control !== null) return control;
  if (midgardCekDivisionByZero(tag, arguments_)) return "failure";
  if (midgardCekScalarMulOutOfBound(tag, arguments_)) return "failure";
  if (tag === 68n) {
    if (arguments_.length !== 2) {
      throw new Error("BLS millerLoop requires two arguments");
    }
    if (
      arguments_[0]?.kind !== "constant" ||
      arguments_[1]?.kind !== "constant"
    ) {
      throw new Error("BLS millerLoop requires G1 and G2 constants");
    }
    const leftDecoded = decodeMidgardCekConstantWitness(arguments_[0].witness);
    const rightDecoded = decodeMidgardCekConstantWitness(arguments_[1].witness);
    if (
      leftDecoded.type.kind !== "blsG1" ||
      rightDecoded.type.kind !== "blsG2"
    ) {
      throw new Error("BLS millerLoop requires G1 and G2 constants");
    }
    // Both compressed points must uncompress before their expression
    // commitment is admitted, matching the L1 rule.
    directConstantToBuiltinValue(arguments_[0].witness);
    directConstantToBuiltinValue(arguments_[1].witness);
    const left = hashMidgardCekDirectValueWitness(arguments_[0]);
    const right = hashMidgardCekDirectValueWitness(arguments_[1]);
    return {
      kind: "blsMillerLoop",
      expressionRoot: hashMidgardCekBlsExpressionNode({
        kind: "millerLoop",
        g1Value: left,
        g2Value: right,
      }),
    };
  }
  if (tag === 69n) {
    if (
      arguments_.length !== 2 ||
      arguments_[0]?.kind !== "blsMillerLoop" ||
      arguments_[1]?.kind !== "blsMillerLoop"
    ) {
      throw new Error("BLS mulMlResult requires two expression values");
    }
    return {
      kind: "blsMillerLoop",
      expressionRoot: hashMidgardCekBlsExpressionNode({
        kind: "multiply",
        left: arguments_[0].expressionRoot,
        right: arguments_[1].expressionRoot,
      }),
    };
  }
  if (tag === 70n) {
    throw new Error(
      "V1 BLS finalVerify requires its dedicated expression witness",
    );
  }
  if (tag === 51n) {
    // serialiseData writes Cardano's exact Data CBOR: definite maps, chunked
    // byte strings and bignum magnitudes over 64 bytes.
    const [argument] = arguments_;
    if (arguments_.length !== 1 || argument?.kind !== "constant") {
      throw new Error("serialiseData requires one Data constant");
    }
    const decoded = decodeMidgardCekConstantWitness(argument.witness);
    if (decoded.type.kind !== "data") {
      throw new Error("serialiseData requires Data");
    }
    return builtinValueToDirectWitness(
      UPLCConst.byteString(encodeMidgardCekPlutusData(decoded.payload)),
      true,
    );
  }
  if (
    ((tag === 11n || tag === 12n) &&
      arguments_.at(-1)?.kind === "semanticConstant") ||
    ((tag === 82n || tag === 83n) && arguments_[0]?.kind === "semanticConstant")
  ) {
    return compactedSourceFailure(tag, arguments_);
  }
  if (tag >= 3n && tag <= 6n && arguments_[0]?.kind === "semanticConstant") {
    return compactedDividendFailure(arguments_);
  }
  const builtinArguments: UPLCConst[] = [];
  for (const argument of arguments_) {
    if (argument.kind !== "constant") {
      throw new Error("non-control V1 builtin arguments must be constants");
    }
    builtinArguments.push(directConstantToBuiltinValue(argument.witness));
  }
  const result = evaluateMidgardCekBuiltinOnValues(
    Number(tag),
    builtinArguments,
  );
  return result === "failure" ? "failure" : builtinValueToDirectWitness(result);
};

const directFailureIsCharged = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean =>
  // Cardano charges scalarMul (56, 63) before its scalar bound check fails.
  [3n, 4n, 5n, 6n, 21n, 52n, 53n, 56n, 60n, 63n, 67n].includes(tag) ||
  // Cardano's machine refuses an out-of-range width before charging.
  (tag === 73n &&
    directInteger(arguments_[1]!) >= 0n &&
    directInteger(arguments_[1]!) <= 8192n) ||
  (tag === 59n && directByteLength(arguments_[0]!) === 48) ||
  (tag === 66n && directByteLength(arguments_[0]!) === 96);

/**
 * One direct builtin application, evaluated at most once: a step's route
 * choice (`midgardCekDirectBuiltinFits`) and its evaluation
 * (`evaluateMidgardCekDirectBuiltin`) share it and so read one result.
 */
export type MidgardCekDirectBuiltinApplication = {
  readonly tag: bigint;
  readonly arguments: readonly MidgardCekDirectValueWitness[];
  readonly result: () => MidgardCekDirectValueWitness | "failure";
};

export const midgardCekDirectBuiltinApplication = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): MidgardCekDirectBuiltinApplication => {
  let result: MidgardCekDirectValueWitness | "failure" | undefined;
  return Object.freeze({
    tag,
    arguments: arguments_,
    result: () => (result ??= evaluateDirectBuiltinResult(tag, arguments_)),
  });
};

const applicationOf = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  application: MidgardCekDirectBuiltinApplication | undefined,
): MidgardCekDirectBuiltinApplication => {
  if (application === undefined)
    return midgardCekDirectBuiltinApplication(tag, arguments_);
  if (application.tag !== tag || application.arguments !== arguments_)
    throw new Error("V1 builtin application is not this step's");
  return application;
};

/**
 * Whether the direct witness of builtin `tag` over these arguments fits the
 * aggregate direct payload bound with its result. A builtin that fails has no
 * result, so its witness fits when its arguments do. The answer depends only
 * on the tag and the arguments.
 */
export const midgardCekDirectBuiltinFits = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  application?: MidgardCekDirectBuiltinApplication,
): boolean => {
  const bound = BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES);
  if (directWitnessPayloadBytes(arguments_) > bound) return false;
  const result = applicationOf(tag, arguments_, application).result();
  return (
    result === "failure" ||
    directWitnessPayloadBytes([...arguments_, result]) <= bound
  );
};

export const evaluateMidgardCekDirectBuiltin = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  application?: MidgardCekDirectBuiltinApplication,
): MidgardCekDirectBuiltinEvaluation => {
  if (
    tag < 0n ||
    tag > MIDGARD_CEK_MAX_RESOLVED_BUILTIN_TAG ||
    BigInt(arguments_.length) !== midgardCekBuiltinArgumentCount(tag)
  ) {
    throw new Error("V1 builtin has an invalid tag or arity");
  }
  if (
    directWitnessPayloadBytes(arguments_) >
    BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES)
  ) {
    throw new Error(
      "V1 builtin arguments exceed the aggregate direct payload bound",
    );
  }
  const budget = midgardCekDirectBuiltinBudget(tag, arguments_);
  const result = applicationOf(tag, arguments_, application).result();
  if (result === "failure") {
    return Object.freeze({
      kind: "failure",
      budget: directFailureIsCharged(tag, arguments_)
        ? budget
        : Object.freeze({ cpu: 0n, memory: 0n }),
    });
  }
  if (
    directWitnessPayloadBytes([...arguments_, result]) >
    BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES)
  ) {
    if (tag !== 51n) {
      throw new Error(
        "V1 builtin result exceeds aggregate direct payload bound",
      );
    }
    return Object.freeze({
      kind: "success",
      result: semanticizeMidgardCekDirectConstant(result),
      budget,
    });
  }
  return Object.freeze({
    kind: "success",
    result,
    budget,
  });
};
