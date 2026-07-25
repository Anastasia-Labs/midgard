import {
  hashMidgardCekBlsExpressionNodeV1,
  hashMidgardCekSequenceNodeV1,
  hashMidgardCekValueNodeV1,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
  MIDGARD_CEK_MAX_BUILTIN_TAG,
} from "@al-ft/midgard-core";
import {
  DataB,
  DataConstr,
  DataList,
} from "@harmoniclabs/plutus-data";
import {
  BnCEK,
  CEKConst,
  CEKError,
  ExBudget,
  PartialBuiltin,
} from "@harmoniclabs/plutus-machine";
import {
  ConstTyTag,
  type UPLCBuiltinTag,
  UPLCConst,
} from "@harmoniclabs/uplc";

import {
  decodeMidgardCekConstantTypeCborV1,
  decodeMidgardCekConstantWitnessV1,
  hashMidgardCekConstantWitnessV1,
  hashMidgardCekSemanticConstantWitnessV1,
  midgardCekConstantMemorySizeV1,
  type MidgardCekConstantTypeV1,
  midgardCekConstantWitnessFromUplcV1,
  midgardCekConstantWitnessToUplcV1,
  type MidgardCekConstantWitnessV1,
  type MidgardCekSemanticConstantWitnessV1,
} from "./cek-constant.js";
import {
  computeMidgardCekBuiltinBudgetV1,
  MIDGARD_CEK_PINNED_PLUTUS_V3_BUILTIN_COSTS_V1,
  type MidgardCekBuiltinBudgetV1,
  normalizeMidgardCekBitwiseCostSizesV1,
} from "./cek-cost.js";

type Bytes = Uint8Array;

export type MidgardCekRuntimeValueWitnessV1 =
  | {
      readonly kind: "constant";
      readonly witness: MidgardCekConstantWitnessV1;
    }
  | {
      readonly kind: "semanticConstant";
      readonly witness: MidgardCekSemanticConstantWitnessV1;
    }
  | {
      readonly kind: "lambda";
      readonly body: Bytes;
      readonly environment: Bytes;
    }
  | {
      readonly kind: "delay";
      readonly body: Bytes;
      readonly environment: Bytes;
    }
  | {
      readonly kind: "constr";
      readonly tag: bigint;
      readonly valuesCount: bigint;
      readonly valuesRoot: Bytes;
    }
  | {
      readonly kind: "builtin";
      readonly tag: bigint;
      readonly forcesRemaining: bigint;
      readonly argumentsCount: bigint;
      readonly argumentsRoot: Bytes;
    }
  | {
      readonly kind: "blsMillerLoop";
      readonly expressionRoot: Bytes;
    };

export type MidgardCekConstantValueWitnessV1 = Extract<
  MidgardCekRuntimeValueWitnessV1,
  { readonly kind: "constant" | "semanticConstant" }
>;

type RuntimeValueKind =
  | "any"
  | "integer"
  | "bytes"
  | "string"
  | "unit"
  | "boolean"
  | "list"
  | "pair"
  | "data"
  | "blsG1"
  | "blsG2"
  | "blsMillerLoop"
  | "listData"
  | "listDataPair"
  | "listInteger";

const sameBytes = (left: Bytes, right: Bytes): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

export const hashMidgardCekRuntimeValueWitnessV1 = (
  value: MidgardCekRuntimeValueWitnessV1,
): Bytes => {
  switch (value.kind) {
    case "constant":
      return hashMidgardCekConstantWitnessV1(value.witness);
    case "semanticConstant":
      return hashMidgardCekSemanticConstantWitnessV1(value.witness);
    case "lambda":
      return hashMidgardCekValueNodeV1({
        kind: "lambda",
        body: value.body,
        environment: value.environment,
      });
    case "delay":
      return hashMidgardCekValueNodeV1({
        kind: "delay",
        body: value.body,
        environment: value.environment,
      });
    case "constr":
      return hashMidgardCekValueNodeV1({
        kind: "constr",
        tag: value.tag,
        valuesCount: value.valuesCount,
        valuesRoot: value.valuesRoot,
      });
    case "builtin":
      return hashMidgardCekValueNodeV1({
        kind: "builtin",
        tag: value.tag,
        forcesRemaining: value.forcesRemaining,
        argumentsCount: value.argumentsCount,
        argumentsRoot: value.argumentsRoot,
      });
    case "blsMillerLoop":
      return hashMidgardCekValueNodeV1({
        kind: "blsMillerLoop",
        expressionRoot: value.expressionRoot,
      });
  }
};

export const hashMidgardCekRuntimeArgumentsV1 = (
  arguments_: readonly MidgardCekRuntimeValueWitnessV1[],
): { readonly root: Bytes; readonly count: bigint } => {
  let root: Bytes = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1;
  let count = 0n;
  for (const argument of arguments_) {
    count += 1n;
    root = hashMidgardCekSequenceNodeV1({
      head: hashMidgardCekRuntimeValueWitnessV1(argument),
      tail: root,
      length: count,
    });
  }
  return Object.freeze({ root, count });
};

const sameConstantType = (
  left: MidgardCekConstantTypeV1,
  right: MidgardCekConstantTypeV1,
): boolean => {
  if (left.kind !== right.kind) return false;
  if (left.kind === "list" && right.kind === "list") {
    return sameConstantType(left.element, right.element);
  }
  if (left.kind === "pair" && right.kind === "pair") {
    return (
      sameConstantType(left.first, right.first) &&
      sameConstantType(left.second, right.second)
    );
  }
  return true;
};

const constantType = (
  value: MidgardCekRuntimeValueWitnessV1,
): MidgardCekConstantTypeV1 | null =>
  value.kind === "constant"
    ? decodeMidgardCekConstantWitnessV1(value.witness).type
    : value.kind === "semanticConstant"
      ? decodeMidgardCekConstantTypeCborV1(value.witness.typeCbor)
    : null;

const matchesKind = (
  value: MidgardCekRuntimeValueWitnessV1,
  kind: RuntimeValueKind,
): boolean => {
  if (kind === "any") return true;
  if (kind === "blsMillerLoop") {
    return value.kind === "blsMillerLoop";
  }
  const type = constantType(value);
  if (type === null) return false;
  switch (kind) {
    case "integer":
    case "bytes":
    case "string":
    case "unit":
    case "boolean":
    case "data":
    case "blsG1":
    case "blsG2":
      return type.kind === kind;
    case "list":
      return type.kind === "list";
    case "pair":
      return type.kind === "pair";
    case "listData":
      return type.kind === "list" && type.element.kind === "data";
    case "listInteger":
      return type.kind === "list" && type.element.kind === "integer";
    case "listDataPair":
      return (
        type.kind === "list" &&
        type.element.kind === "pair" &&
        type.element.first.kind === "data" &&
        type.element.second.kind === "data"
      );
  }
};

const argumentKinds = (tag: number): readonly RuntimeValueKind[] => {
  if (!Number.isInteger(tag) || tag < 0 || tag > 86) {
    throw new Error("V1 builtin tag is outside Plutus V3");
  }
  if (tag <= 9) return ["integer", "integer"];
  if (tag === 10) return ["bytes", "bytes"];
  if (tag === 11) return ["integer", "bytes"];
  if (tag === 12) return ["integer", "integer", "bytes"];
  if (tag === 13) return ["bytes"];
  if (tag === 14) return ["bytes", "integer"];
  if (tag <= 17) return ["bytes", "bytes"];
  if (tag <= 20) return ["bytes"];
  if (tag === 21) return ["bytes", "bytes", "bytes"];
  if (tag <= 23) return ["string", "string"];
  if (tag === 24) return ["string"];
  if (tag === 25) return ["bytes"];
  if (tag === 26) return ["boolean", "any", "any"];
  if (tag === 27) return ["unit", "any"];
  if (tag === 28) return ["string", "any"];
  if (tag <= 30) return ["pair"];
  if (tag === 31) return ["list", "any", "any"];
  if (tag === 32) return ["any", "list"];
  if (tag <= 35) return ["list"];
  if (tag === 36) return ["data", "any", "any", "any", "any", "any"];
  if (tag === 37) return ["integer", "listData"];
  if (tag === 38) return ["listDataPair"];
  if (tag === 39) return ["listData"];
  if (tag === 40) return ["integer"];
  if (tag === 41) return ["bytes"];
  if (tag <= 46) return ["data"];
  if (tag <= 48) return ["data", "data"];
  if (tag <= 50) return ["unit"];
  if (tag === 51) return ["data"];
  if (tag <= 53) return ["bytes", "bytes", "bytes"];
  if (tag === 54 || tag === 57) return ["blsG1", "blsG1"];
  if (tag === 55 || tag === 59) return ["blsG1"];
  if (tag === 56) return ["integer", "blsG1"];
  if (tag === 58) return ["bytes", "bytes"];
  if (tag === 60) return ["bytes"];
  if (tag === 61 || tag === 64) return ["blsG2", "blsG2"];
  if (tag === 62 || tag === 66) return ["blsG2"];
  if (tag === 63) return ["integer", "blsG2"];
  if (tag === 65) return ["bytes", "bytes"];
  if (tag === 67) return ["bytes"];
  if (tag === 68) return ["blsG1", "blsG2"];
  if (tag === 69 || tag === 70) {
    return ["blsMillerLoop", "blsMillerLoop"];
  }
  if (tag <= 72) return ["bytes"];
  if (tag === 73) return ["boolean", "integer", "integer"];
  if (tag === 74) return ["boolean", "bytes"];
  if (tag <= 77) return ["boolean", "bytes", "bytes"];
  if (tag === 78) return ["bytes"];
  if (tag === 79) return ["bytes", "integer"];
  if (tag === 80) return ["bytes", "listInteger", "boolean"];
  if (tag === 81) return ["integer", "integer"];
  if (tag <= 83) return ["bytes", "integer"];
  return ["bytes"];
};

const mkConsIsWellTyped = (
  arguments_: readonly MidgardCekRuntimeValueWitnessV1[],
): boolean => {
  if (arguments_.length !== 2) return false;
  const elementType = constantType(arguments_[0]);
  const listType = constantType(arguments_[1]);
  return (
    elementType !== null &&
    listType?.kind === "list" &&
    sameConstantType(elementType, listType.element)
  );
};

export const verifyMidgardCekBuiltinTypeFailureV1 = (
  tag: bigint,
  builtinValueRoot: Bytes,
  arguments_: readonly MidgardCekRuntimeValueWitnessV1[],
): boolean => {
  try {
    if (
      tag < 0n ||
      tag > MIDGARD_CEK_MAX_BUILTIN_TAG ||
      tag > BigInt(Number.MAX_SAFE_INTEGER)
    ) {
      return false;
    }
    const numericTag = Number(tag);
    const kinds = argumentKinds(numericTag);
    if (arguments_.length !== kinds.length) return false;
    const { root, count } =
      hashMidgardCekRuntimeArgumentsV1(arguments_);
    if (
      !sameBytes(
        builtinValueRoot,
        hashMidgardCekValueNodeV1({
          kind: "builtin",
          tag,
          forcesRemaining: 0n,
          argumentsCount: count,
          argumentsRoot: root,
        }),
      )
    ) {
      return false;
    }
    const wellTyped =
      numericTag === 32
        ? mkConsIsWellTyped(arguments_)
        : arguments_.every((argument, index) =>
            matchesKind(argument, kinds[index]),
          );
    return !wellTyped;
  } catch {
    return false;
  }
};

export type MidgardCekDirectValueWitnessV1 =
  | {
      readonly kind: "constant";
      readonly witness: MidgardCekConstantWitnessV1;
    }
  | {
      readonly kind: "semanticConstant";
      readonly witness: MidgardCekSemanticConstantWitnessV1;
    }
  | { readonly kind: "opaque"; readonly root: Bytes }
  | { readonly kind: "blsMillerLoop"; readonly expressionRoot: Bytes };

export type MidgardCekDirectBuiltinEvaluationV1 =
  | {
      readonly kind: "success";
      readonly result: MidgardCekDirectValueWitnessV1;
      readonly budget: MidgardCekBuiltinBudgetV1;
    }
  | {
      readonly kind: "failure";
      readonly budget: MidgardCekBuiltinBudgetV1;
    };

export const hashMidgardCekDirectValueWitnessV1 = (
  value: MidgardCekDirectValueWitnessV1,
): Bytes => {
  switch (value.kind) {
    case "constant":
      return hashMidgardCekConstantWitnessV1(value.witness);
    case "semanticConstant":
      return hashMidgardCekSemanticConstantWitnessV1(value.witness);
    case "opaque":
      if (value.root.length !== 32) {
        throw new Error("V1 opaque CEK value root must be bytes32");
      }
      return value.root;
    case "blsMillerLoop":
      return hashMidgardCekValueNodeV1({
        kind: "blsMillerLoop",
        expressionRoot: value.expressionRoot,
      });
  }
};

export const hashMidgardCekDirectArgumentsV1 = (
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): { readonly root: Bytes; readonly count: bigint } => {
  let root: Bytes = MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1;
  let count = 0n;
  for (const argument of arguments_) {
    count += 1n;
    root = hashMidgardCekSequenceNodeV1({
      head: hashMidgardCekDirectValueWitnessV1(argument),
      tail: root,
      length: count,
    });
  }
  return Object.freeze({ root, count });
};

const decodedDirectConstant = (
  value: MidgardCekDirectValueWitnessV1,
) => {
  if (value.kind !== "constant") {
    throw new Error("V1 builtin requires a revealed constant");
  }
  return decodeMidgardCekConstantWitnessV1(value.witness);
};

const directValueMemorySize = (
  value: MidgardCekDirectValueWitnessV1,
): bigint => {
  if (value.kind === "opaque") {
    if (value.root.length !== 32) {
      throw new Error("V1 opaque CEK value root must be bytes32");
    }
    return 1n;
  }
  if (value.kind === "blsMillerLoop") {
    if (value.expressionRoot.length !== 32) {
      throw new Error("V1 BLS expression root must be bytes32");
    }
    return 192n;
  }
  if (value.kind === "semanticConstant") {
    return value.witness.memory;
  }
  const decoded = decodeMidgardCekConstantWitnessV1(value.witness);
  return midgardCekConstantMemorySizeV1(decoded.type, decoded.payload);
};

const directBoolean = (value: MidgardCekDirectValueWitnessV1): boolean => {
  const decoded = decodedDirectConstant(value);
  if (
    decoded.type.kind !== "boolean" ||
    !(decoded.payload instanceof DataConstr)
  ) {
    throw new Error("V1 builtin requires a boolean");
  }
  return decoded.payload.constr === 1n;
};

const directByteLength = (
  value: MidgardCekDirectValueWitnessV1,
): number => {
  const decoded = decodedDirectConstant(value);
  if (
    decoded.type.kind !== "bytes" ||
    !(decoded.payload instanceof DataB)
  ) {
    throw new Error("V1 builtin requires a byte string");
  }
  return decoded.payload.bytes.toBuffer().length;
};

export const midgardCekDirectBuiltinCostSizesV1 = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): readonly bigint[] => {
  if (tag === 26n) {
    if (arguments_.length !== 3) {
      throw new Error("ifThenElse requires three arguments");
    }
    directBoolean(arguments_[0]!);
    return Object.freeze([1n, 1n, 1n]);
  }
  if (tag === 27n) {
    if (arguments_.length !== 2) {
      throw new Error("chooseUnit requires two arguments");
    }
    const unit = decodedDirectConstant(arguments_[0]!);
    if (unit.type.kind !== "unit") {
      throw new Error("chooseUnit requires unit");
    }
    return Object.freeze([1n, 1n]);
  }
  if (tag === 28n) {
    if (arguments_.length !== 2) {
      throw new Error("trace requires two arguments");
    }
    const message = decodedDirectConstant(arguments_[0]!);
    if (
      message.type.kind !== "string" ||
      !(message.payload instanceof DataB)
    ) {
      throw new Error("trace requires a string message");
    }
    return Object.freeze([
      BigInt(message.payload.bytes.toBuffer().length),
      1n,
    ]);
  }
  if (tag === 31n) {
    if (arguments_.length !== 3) {
      throw new Error("chooseList requires three arguments");
    }
    return Object.freeze([
      directValueMemorySize(arguments_[0]!),
      1n,
      1n,
    ]);
  }
  if (tag === 36n) {
    if (arguments_.length !== 6) {
      throw new Error("chooseData requires six arguments");
    }
    return Object.freeze([
      directValueMemorySize(arguments_[0]!),
      1n,
      1n,
      1n,
      1n,
      1n,
    ]);
  }
  if (tag >= 75n && tag <= 77n) {
    if (arguments_.length !== 3) {
      throw new Error("bitwise builtin requires three arguments");
    }
    return normalizeMidgardCekBitwiseCostSizesV1(
      directBoolean(arguments_[0]!),
      directValueMemorySize(arguments_[1]!),
      directValueMemorySize(arguments_[2]!),
    );
  }
  return Object.freeze(arguments_.map(directValueMemorySize));
};

export const midgardCekDirectBuiltinBudgetV1 = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): MidgardCekBuiltinBudgetV1 => {
  if (
    tag < 0n ||
    tag > MIDGARD_CEK_MAX_BUILTIN_TAG ||
    tag > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("V1 builtin tag is outside Plutus V3");
  }
  return computeMidgardCekBuiltinBudgetV1(
    Number(tag),
    midgardCekDirectBuiltinCostSizesV1(tag, arguments_),
  );
};

const selectedControlResult = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): MidgardCekDirectValueWitnessV1 | null => {
  if (tag === 26n) {
    if (arguments_.length !== 3) {
      throw new Error("ifThenElse requires three arguments");
    }
    return directBoolean(arguments_[0]!)
      ? arguments_[1]!
      : arguments_[2]!;
  }
  if (tag === 27n) {
    if (arguments_.length !== 2) {
      throw new Error("chooseUnit requires two arguments");
    }
    const unit = decodedDirectConstant(arguments_[0]!);
    if (unit.type.kind !== "unit") {
      throw new Error("chooseUnit requires unit");
    }
    return arguments_[1]!;
  }
  if (tag === 28n) {
    if (arguments_.length !== 2) {
      throw new Error("trace requires two arguments");
    }
    const message = decodedDirectConstant(arguments_[0]!);
    if (message.type.kind !== "string") {
      throw new Error("trace requires a string");
    }
    return arguments_[1]!;
  }
  if (tag === 31n) {
    if (arguments_.length !== 3) {
      throw new Error("chooseList requires three arguments");
    }
    const source = decodedDirectConstant(arguments_[0]!);
    if (
      source.type.kind !== "list" ||
      !(source.payload instanceof DataList)
    ) {
      throw new Error("chooseList requires a list");
    }
    return source.payload.list.length === 0
      ? arguments_[1]!
      : arguments_[2]!;
  }
  if (tag === 36n) {
    if (arguments_.length !== 6) {
      throw new Error("chooseData requires six arguments");
    }
    const source = decodedDirectConstant(arguments_[0]!);
    if (source.type.kind !== "data") {
      throw new Error("chooseData requires Data");
    }
    const selected =
      source.payload instanceof DataConstr
        ? 1
        : source.payload.constructor.name === "DataMap"
          ? 2
          : source.payload instanceof DataList
            ? 3
            : source.payload.constructor.name === "DataI"
              ? 4
              : source.payload instanceof DataB
                ? 5
                : -1;
    if (selected < 1) {
      throw new Error("chooseData received an unknown Data variant");
    }
    return arguments_[selected]!;
  }
  return null;
};

const runPinnedReferenceBuiltin = (
  tag: number,
  arguments_: readonly CEKConst[],
): CEKConst | CEKError => {
  const builtin = new PartialBuiltin(tag as UPLCBuiltinTag);
  for (const argument of arguments_) builtin.apply(argument);
  if (builtin.nMissingArgs !== 0) {
    throw new Error("V1 builtin argument count is incomplete");
  }
  return new BnCEK(
    MIDGARD_CEK_PINNED_PLUTUS_V3_BUILTIN_COSTS_V1,
    new ExBudget({ cpu: 0, mem: 0 }),
    [],
  ).eval(builtin);
};

const directConstantToReferenceValue = (
  witness: MidgardCekConstantWitnessV1,
): CEKConst => {
  const decoded = decodeMidgardCekConstantWitnessV1(witness);
  if (decoded.type.kind === "bytes") {
    if (!(decoded.payload instanceof DataB)) {
      throw new Error("V1 byte-string payload is not bytes");
    }
    const payloadBytes = decoded.payload.bytes;
    const ByteStringConstructor = payloadBytes.constructor as new (
      bytes: Uint8Array,
    ) => typeof payloadBytes;
    return CEKConst.fromUplc(
      UPLCConst.byteString(
        new ByteStringConstructor(
          Uint8Array.from(payloadBytes.toBuffer()),
        ),
      ),
    );
  }
  if (
    decoded.type.kind !== "blsG1" &&
    decoded.type.kind !== "blsG2"
  ) {
    return CEKConst.fromUplc(
      midgardCekConstantWitnessToUplcV1(witness),
    );
  }
  if (!(decoded.payload instanceof DataB)) {
    throw new Error("V1 BLS payload is not bytes");
  }
  const payloadBytes = decoded.payload.bytes;
  const ByteStringConstructor = payloadBytes.constructor as new (
    bytes: Uint8Array,
  ) => typeof payloadBytes;
  // Harmonic's crypto parser mutates a `.slice()` while reading mask bits.
  // A Node Buffer slice aliases its source, whereas a plain Uint8Array slice
  // is detached; normalize here so the pinned evaluator sees the canonical
  // compressed point rather than a mask-cleared alias.
  const detachedBytes = new ByteStringConstructor(
    Uint8Array.from(payloadBytes.toBuffer()),
  );
  const compressed = CEKConst.fromUplc(
    UPLCConst.byteString(detachedBytes),
  );
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

const referenceConstantToDirectWitness = (
  result: CEKConst,
): MidgardCekConstantWitnessV1 => {
  const tag = result.type[0];
  if (
    tag !== ConstTyTag.bls12_381_G1_element &&
    tag !== ConstTyTag.bls12_381_G2_element
  ) {
    return midgardCekConstantWitnessFromUplcV1(result);
  }
  const compressed = runPinnedReferenceBuiltin(
    tag === ConstTyTag.bls12_381_G1_element ? 59 : 66,
    [result],
  );
  if (compressed instanceof CEKError) {
    throw new Error("reference evaluator could not compress a BLS result");
  }
  const bytesWitness = midgardCekConstantWitnessFromUplcV1(compressed);
  const witness = Object.freeze({
    typeCbor: Buffer.from(
      tag === ConstTyTag.bls12_381_G1_element ? "9f09ff" : "9f0aff",
      "hex",
    ),
    payloadCbor: bytesWitness.payloadCbor,
  });
  decodeMidgardCekConstantWitnessV1(witness);
  return witness;
};

const evaluateReferenceBuiltin = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): MidgardCekDirectValueWitnessV1 | "failure" => {
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
    const leftDecoded = decodeMidgardCekConstantWitnessV1(
      arguments_[0].witness,
    );
    const rightDecoded = decodeMidgardCekConstantWitnessV1(
      arguments_[1].witness,
    );
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
    const left = hashMidgardCekDirectValueWitnessV1(arguments_[0]);
    const right = hashMidgardCekDirectValueWitnessV1(arguments_[1]);
    return {
      kind: "blsMillerLoop",
      expressionRoot: hashMidgardCekBlsExpressionNodeV1({
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
      expressionRoot: hashMidgardCekBlsExpressionNodeV1({
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
  const referenceArguments: CEKConst[] = [];
  for (const argument of arguments_) {
    if (argument.kind !== "constant") {
      throw new Error(
        "non-control V1 builtin arguments must be constants",
      );
    }
    referenceArguments.push(
      directConstantToReferenceValue(argument.witness),
    );
  }
  const result = runPinnedReferenceBuiltin(
    Number(tag),
    referenceArguments,
  );
  if (result instanceof CEKError) return "failure";
  if (!(result instanceof CEKConst)) {
    throw new Error("reference builtin returned a non-constant value");
  }
  return {
    kind: "constant",
    witness: referenceConstantToDirectWitness(result),
  };
};

const directFailureIsCharged = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): boolean =>
  [4n, 5n, 6n, 52n, 53n, 58n, 65n, 73n].includes(tag) ||
  (tag === 60n && directByteLength(arguments_[0]!) === 48) ||
  (tag === 67n && directByteLength(arguments_[0]!) === 96);

export const evaluateMidgardCekDirectBuiltinV1 = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): MidgardCekDirectBuiltinEvaluationV1 => {
  if (
    tag < 0n ||
    tag > MIDGARD_CEK_MAX_BUILTIN_TAG ||
    BigInt(arguments_.length) !==
      (tag > BigInt(Number.MAX_SAFE_INTEGER)
        ? -1n
        : BigInt(
            // The CEK machine owns the consensus arity table. This local
            // evaluator deliberately relies on the pinned reference type.
            PartialBuiltin.getNRequiredArgsFor(
              Number(tag) as UPLCBuiltinTag,
            ),
          ))
  ) {
    throw new Error("V1 builtin has an invalid tag or arity");
  }
  const budget = midgardCekDirectBuiltinBudgetV1(tag, arguments_);
  const result = evaluateReferenceBuiltin(tag, arguments_);
  if (result === "failure") {
    return Object.freeze({
      kind: "failure",
      budget: directFailureIsCharged(tag, arguments_)
        ? budget
        : Object.freeze({ cpu: 0n, memory: 0n }),
    });
  }
  return Object.freeze({ kind: "success", result, budget });
};

export const verifyMidgardCekDirectBuiltinV1 = (
  tag: bigint,
  builtinValueRoot: Bytes,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
  result: MidgardCekDirectValueWitnessV1,
): boolean => {
  try {
    const committed = hashMidgardCekDirectArgumentsV1(arguments_);
    if (
      !sameBytes(
        builtinValueRoot,
        hashMidgardCekValueNodeV1({
          kind: "builtin",
          tag,
          forcesRemaining: 0n,
          argumentsCount: committed.count,
          argumentsRoot: committed.root,
        }),
      )
    ) {
      return false;
    }
    const evaluated = evaluateMidgardCekDirectBuiltinV1(tag, arguments_);
    return (
      evaluated.kind === "success" &&
      sameBytes(
        hashMidgardCekDirectValueWitnessV1(evaluated.result),
        hashMidgardCekDirectValueWitnessV1(result),
      )
    );
  } catch {
    return false;
  }
};

export const verifyMidgardCekDirectBuiltinFailureV1 = (
  tag: bigint,
  builtinValueRoot: Bytes,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): boolean => {
  try {
    const committed = hashMidgardCekDirectArgumentsV1(arguments_);
    if (
      !sameBytes(
        builtinValueRoot,
        hashMidgardCekValueNodeV1({
          kind: "builtin",
          tag,
          forcesRemaining: 0n,
          argumentsCount: committed.count,
          argumentsRoot: committed.root,
        }),
      )
    ) {
      return false;
    }
    return (
      evaluateMidgardCekDirectBuiltinV1(tag, arguments_).kind === "failure"
    );
  } catch {
    return false;
  }
};

export type MidgardCekBlsExpressionWitnessV1 =
  | {
      readonly kind: "millerLoop";
      readonly g1: MidgardCekConstantWitnessV1;
      readonly g2: MidgardCekConstantWitnessV1;
    }
  | {
      readonly kind: "multiply";
      readonly left: MidgardCekBlsExpressionWitnessV1;
      readonly right: MidgardCekBlsExpressionWitnessV1;
    };

type EvaluatedBlsExpressionV1 = {
  readonly root: Bytes;
  readonly value: CEKConst;
  readonly leaves: number;
  readonly depth: number;
};

const evaluateBlsExpressionV1 = (
  expression: MidgardCekBlsExpressionWitnessV1,
): EvaluatedBlsExpressionV1 => {
  if (expression.kind === "millerLoop") {
    const g1Decoded = decodeMidgardCekConstantWitnessV1(expression.g1);
    const g2Decoded = decodeMidgardCekConstantWitnessV1(expression.g2);
    if (
      g1Decoded.type.kind !== "blsG1" ||
      g2Decoded.type.kind !== "blsG2"
    ) {
      throw new Error("BLS expression leaf requires G1 and G2 constants");
    }
    const g1 = directConstantToReferenceValue(expression.g1);
    const g2 = directConstantToReferenceValue(expression.g2);
    const value = runPinnedReferenceBuiltin(68, [g1, g2]);
    if (value instanceof CEKError) {
      throw new Error("reference evaluator rejected a BLS expression leaf");
    }
    return Object.freeze({
      root: hashMidgardCekBlsExpressionNodeV1({
        kind: "millerLoop",
        g1Value: hashMidgardCekConstantWitnessV1(expression.g1),
        g2Value: hashMidgardCekConstantWitnessV1(expression.g2),
      }),
      value,
      leaves: 1,
      depth: 1,
    });
  }
  const left = evaluateBlsExpressionV1(expression.left);
  const right = evaluateBlsExpressionV1(expression.right);
  const value = runPinnedReferenceBuiltin(69, [left.value, right.value]);
  if (value instanceof CEKError) {
    throw new Error("reference evaluator rejected a BLS expression product");
  }
  return Object.freeze({
    root: hashMidgardCekBlsExpressionNodeV1({
      kind: "multiply",
      left: left.root,
      right: right.root,
    }),
    value,
    leaves: left.leaves + right.leaves,
    depth: Math.max(left.depth, right.depth) + 1,
  });
};

export type MidgardCekBlsFinalEvaluationV1 = {
  readonly leftRoot: Bytes;
  readonly rightRoot: Bytes;
  readonly result: MidgardCekDirectValueWitnessV1;
  readonly budget: MidgardCekBuiltinBudgetV1;
};

export const evaluateMidgardCekBlsFinalV1 = (
  expectedLeftRoot: Bytes,
  expectedRightRoot: Bytes,
  leftExpression: MidgardCekBlsExpressionWitnessV1,
  rightExpression: MidgardCekBlsExpressionWitnessV1,
): MidgardCekBlsFinalEvaluationV1 => {
  if (expectedLeftRoot.length !== 32 || expectedRightRoot.length !== 32) {
    throw new Error("BLS finalVerify expected roots must be bytes32");
  }
  const left = evaluateBlsExpressionV1(leftExpression);
  const right = evaluateBlsExpressionV1(rightExpression);
  if (
    !sameBytes(left.root, expectedLeftRoot) ||
    !sameBytes(right.root, expectedRightRoot)
  ) {
    throw new Error("BLS finalVerify expression root mismatch");
  }
  if (
    left.leaves + right.leaves > 10 ||
    left.depth > 10 ||
    right.depth > 10
  ) {
    throw new Error(
      "BLS finalVerify expression exceeds the ten-leaf L1 proof reserve",
    );
  }
  const result = runPinnedReferenceBuiltin(70, [left.value, right.value]);
  if (result instanceof CEKError) {
    throw new Error("reference evaluator rejected BLS finalVerify");
  }
  const arguments_: readonly MidgardCekDirectValueWitnessV1[] = [
    { kind: "blsMillerLoop", expressionRoot: left.root },
    { kind: "blsMillerLoop", expressionRoot: right.root },
  ];
  return Object.freeze({
    leftRoot: left.root,
    rightRoot: right.root,
    result: {
      kind: "constant",
      witness: referenceConstantToDirectWitness(result),
    },
    budget: midgardCekDirectBuiltinBudgetV1(70n, arguments_),
  });
};

export const verifyMidgardCekBlsFinalV1 = (
  builtinValueRoot: Bytes,
  expectedLeftRoot: Bytes,
  expectedRightRoot: Bytes,
  leftExpression: MidgardCekBlsExpressionWitnessV1,
  rightExpression: MidgardCekBlsExpressionWitnessV1,
  result: MidgardCekDirectValueWitnessV1,
): boolean => {
  try {
    const arguments_: readonly MidgardCekDirectValueWitnessV1[] = [
      {
        kind: "blsMillerLoop",
        expressionRoot: expectedLeftRoot,
      },
      {
        kind: "blsMillerLoop",
        expressionRoot: expectedRightRoot,
      },
    ];
    const committed = hashMidgardCekDirectArgumentsV1(arguments_);
    if (
      !sameBytes(
        builtinValueRoot,
        hashMidgardCekValueNodeV1({
          kind: "builtin",
          tag: 70n,
          forcesRemaining: 0n,
          argumentsCount: committed.count,
          argumentsRoot: committed.root,
        }),
      )
    ) {
      return false;
    }
    const evaluated = evaluateMidgardCekBlsFinalV1(
      expectedLeftRoot,
      expectedRightRoot,
      leftExpression,
      rightExpression,
    );
    return sameBytes(
      hashMidgardCekDirectValueWitnessV1(evaluated.result),
      hashMidgardCekDirectValueWitnessV1(result),
    );
  } catch {
    return false;
  }
};
