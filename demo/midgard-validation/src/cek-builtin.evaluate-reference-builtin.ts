import {
  hashMidgardCekBlsExpressionNode,
  MIDGARD_CEK_MAX_BUILTIN_TAG,
} from "@al-ft/midgard-core";
import { DataB } from "@harmoniclabs/plutus-data";
import {
  BnCEK,
  CEKConst,
  CEKError,
  ExBudget,
  PartialBuiltin,
} from "@harmoniclabs/plutus-machine";
import { ConstTyTag, type UPLCBuiltinTag, UPLCConst } from "@harmoniclabs/uplc";

import {
  directWitnessPayloadBytes,
  type MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import {
  type CardanoExactValue,
  evaluateCardanoExactBuiltin,
  MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS,
} from "./cek-builtin.cardano-exact.js";
import {
  directByteLength,
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
import { MIDGARD_CEK_PINNED_PLUTUS_V3_BUILTIN_COSTS } from "./cek-cost.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";

export const runPinnedReferenceBuiltin = (
  tag: number,
  arguments_: readonly CEKConst[],
): CEKConst | CEKError => {
  const builtinTag = tag as UPLCBuiltinTag;
  if (PartialBuiltin.getNRequiredArgsFor(builtinTag) !== arguments_.length) {
    throw new Error("V1 builtin argument count is incomplete");
  }
  const result = new BnCEK(
    MIDGARD_CEK_PINNED_PLUTUS_V3_BUILTIN_COSTS,
    new ExBudget({ cpu: 0, mem: 0 }),
    [],
  ).eval(builtinTag, arguments_);
  if (!(result instanceof CEKConst) && !(result instanceof CEKError)) {
    throw new Error("V1 builtin did not saturate to a constant");
  }
  return result;
};

export const directConstantToReferenceValue = (
  witness: MidgardCekConstantWitness,
): CEKConst => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  if (decoded.type.kind === "bytes") {
    if (!(decoded.payload instanceof DataB)) {
      throw new Error("V1 byte-string payload is not bytes");
    }
    return CEKConst.fromUplc(
      UPLCConst.byteString(Uint8Array.from(decoded.payload.bytes)),
    );
  }
  if (decoded.type.kind !== "blsG1" && decoded.type.kind !== "blsG2") {
    return CEKConst.fromUplc(midgardCekConstantWitnessToUplc(witness));
  }
  if (!(decoded.payload instanceof DataB)) {
    throw new Error("V1 BLS payload is not bytes");
  }
  // Harmonic's crypto parser mutates a `.slice()` while reading mask bits.
  // A Node Buffer slice aliases its source, whereas a plain Uint8Array slice
  // is detached; normalize here so the pinned evaluator sees the canonical
  // compressed point rather than a mask-cleared alias.
  const detachedBytes = Uint8Array.from(decoded.payload.bytes);
  const compressed = CEKConst.fromUplc(UPLCConst.byteString(detachedBytes));
  const uncompressed = runPinnedReferenceBuiltin(
    decoded.type.kind === "blsG1" ? 60 : 67,
    [compressed],
  );
  if (uncompressed instanceof CEKError) {
    throw new Error(
      `V1 BLS constant has an invalid encoding: ${uncompressed.msg ?? "unknown reference error"}`,
    );
  }
  return uncompressed;
};

const cardanoExactArgument = (
  argument: MidgardCekDirectValueWitness,
): CardanoExactValue => {
  if (argument.kind !== "constant") {
    throw new Error("non-control V1 builtin arguments must be constants");
  }
  const reference = directConstantToReferenceValue(argument.witness);
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

const cardanoExactResult = (value: CardanoExactValue): CEKConst =>
  CEKConst.fromUplc(
    value.kind === "integer"
      ? UPLCConst.int(value.value)
      : value.kind === "bytes"
        ? UPLCConst.byteString(value.value)
        : UPLCConst.bool(value.value),
  );

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

const semanticizeDirectConstant = (
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

export const referenceConstantToDirectWitness = (
  result: CEKConst,
  allowSemantic: boolean = false,
): MidgardCekDirectValueWitness => {
  const tag = result.type[0];
  if (
    tag !== ConstTyTag.bls12_381_G1_element &&
    tag !== ConstTyTag.bls12_381_G2_element
  ) {
    const canonical = encodeMidgardCekCanonicalConstant(
      new UPLCConst(result.type, result.value as never),
    );
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
  const compressed = runPinnedReferenceBuiltin(
    tag === ConstTyTag.bls12_381_G1_element ? 59 : 66,
    [result],
  );
  if (compressed instanceof CEKError) {
    throw new Error("reference evaluator could not compress a BLS result");
  }
  const bytesWitness = midgardCekConstantWitnessFromUplc(compressed);
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

const evaluateReferenceBuiltin = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): MidgardCekDirectValueWitness | "failure" => {
  const control = selectedControlResult(tag, arguments_);
  if (control !== null) return control;
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
    // Round-trip both compressed points through the reference evaluator before
    // admitting their expression commitment, matching the L1 rule.
    directConstantToReferenceValue(arguments_[0].witness);
    directConstantToReferenceValue(arguments_[1].witness);
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
    return referenceConstantToDirectWitness(
      CEKConst.fromUplc(
        UPLCConst.byteString(encodeMidgardCekPlutusData(decoded.payload)),
      ),
      true,
    );
  }
  if (MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS.has(Number(tag))) {
    const result = evaluateCardanoExactBuiltin(
      Number(tag),
      arguments_.map(cardanoExactArgument),
    );
    return result === "failure"
      ? "failure"
      : referenceConstantToDirectWitness(cardanoExactResult(result));
  }
  const referenceArguments: CEKConst[] = [];
  for (const argument of arguments_) {
    if (argument.kind !== "constant") {
      throw new Error("non-control V1 builtin arguments must be constants");
    }
    referenceArguments.push(directConstantToReferenceValue(argument.witness));
  }
  const result = runPinnedReferenceBuiltin(Number(tag), referenceArguments);
  if (result instanceof CEKError) return "failure";
  if (!(result instanceof CEKConst)) {
    throw new Error("reference builtin returned a non-constant value");
  }
  return referenceConstantToDirectWitness(result);
};

const directFailureIsCharged = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean =>
  [4n, 5n, 6n, 21n, 52n, 53n, 58n, 65n, 73n].includes(tag) ||
  (tag === 60n && directByteLength(arguments_[0]!) === 48) ||
  (tag === 67n && directByteLength(arguments_[0]!) === 96);

export const evaluateMidgardCekDirectBuiltin = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): MidgardCekDirectBuiltinEvaluation => {
  if (
    tag < 0n ||
    tag > MIDGARD_CEK_MAX_BUILTIN_TAG ||
    BigInt(arguments_.length) !==
      (tag > BigInt(Number.MAX_SAFE_INTEGER)
        ? -1n
        : BigInt(
            // The CEK machine owns the consensus arity table. This local
            // evaluator deliberately relies on the pinned reference type.
            PartialBuiltin.getNRequiredArgsFor(Number(tag) as UPLCBuiltinTag),
          ))
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
  const result = evaluateReferenceBuiltin(tag, arguments_);
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
      result: semanticizeDirectConstant(result),
      budget,
    });
  }
  return Object.freeze({
    kind: "success",
    result,
    budget,
  });
};
