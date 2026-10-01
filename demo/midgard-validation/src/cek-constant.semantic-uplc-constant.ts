import {
  hashMidgardCekBlobChunk,
  hashMidgardCekValueNode,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";
import {
  constT,
  type ConstType,
  type ConstValue,
  UPLCConst,
} from "@harmoniclabs/uplc";

import {
  decodeMidgardCekConstantTypeCbor,
  decodeMidgardCekConstantWitness,
  midgardConstantTypeToTags,
} from "./cek-constant.payload-matches-type.js";
import {
  type MidgardCekCanonicalConstant,
  type MidgardCekConstantType,
  type MidgardCekConstantWitness,
  type MidgardCekSemanticConstantWitness,
  parseMidgardCekConstantType,
  semanticData,
} from "./cek-constant.semantic-data.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { encodeMidgardCekPlutusData } from "./plutus-data-iterative.encode.js";
import {
  midgardCekByteStringMemorySize,
  midgardCekDataMemorySize,
  midgardCekIntegerMemorySize,
} from "./plutus-data-iterative.memory.js";

const semanticUplcConstant = (
  type: MidgardCekConstantType,
  payload: Data,
): UPLCConst => {
  switch (type.kind) {
    case "integer": {
      if (!(payload instanceof DataI)) {
        throw new Error("V1 integer payload is not DataI");
      }
      return UPLCConst.int(payload.int);
    }
    case "bytes": {
      if (!(payload instanceof DataB)) {
        throw new Error("V1 byte-string payload is not DataB");
      }
      return UPLCConst.byteString(payload.bytes);
    }
    case "string": {
      if (!(payload instanceof DataB)) {
        throw new Error("V1 string payload is not DataB");
      }
      return UPLCConst.str(
        new TextDecoder("utf-8", { fatal: true }).decode(payload.bytes),
      );
    }
    case "unit":
      return UPLCConst.unit;
    case "boolean": {
      if (!(payload instanceof DataConstr)) {
        throw new Error("V1 boolean payload is not DataConstr");
      }
      return UPLCConst.bool(payload.constr === 1n);
    }
    case "list": {
      if (!(payload instanceof DataList)) {
        throw new Error("V1 list payload is not DataList");
      }
      const elementType = midgardConstantTypeToTags(type.element);
      return UPLCConst.listOf(elementType)(
        payload.list.map(
          (item) => semanticUplcConstant(type.element, item).value,
        ) as never,
      );
    }
    case "pair": {
      if (
        !(payload instanceof DataConstr) ||
        payload.constr !== 0n ||
        payload.fields.length !== 2
      ) {
        throw new Error("V1 pair payload is malformed");
      }
      return UPLCConst.pairOf(
        midgardConstantTypeToTags(type.first),
        midgardConstantTypeToTags(type.second),
      )(
        semanticUplcConstant(type.first, payload.fields[0]).value,
        semanticUplcConstant(type.second, payload.fields[1]).value,
      );
    }
    case "data":
      return UPLCConst.data(payload);
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
      throw new Error(
        "V1 BLS constants require their dedicated runtime proof nodes",
      );
  }
};

/**
 * Reconstructs the exact Harmonic UPLC constant consumed by the pinned
 * reference evaluator from the canonical semantic witness checked on L1.
 */
export const midgardCekConstantWitnessToUplc = (
  witness: MidgardCekConstantWitness,
): UPLCConst => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  return semanticUplcConstant(decoded.type, decoded.payload);
};

/**
 * Converts a reference-evaluator constant back into the canonical L1
 * witness. The ordinary witness decoder remains authoritative for the direct
 * one-step payload bound.
 */
export const midgardCekConstantWitnessFromUplc = (constant: {
  readonly type: ConstType;
  readonly value: ConstValue;
}): MidgardCekConstantWitness => {
  const canonical = encodeMidgardCekCanonicalConstant(
    new UPLCConst(constant.type, constant.value as never),
  );
  const witness = Object.freeze({
    typeCbor: canonical.typeCbor,
    payloadCbor: canonical.payloadCbor,
  });
  decodeMidgardCekConstantWitness(witness);
  return witness;
};

export const hashMidgardCekConstantWitness = (
  witness: MidgardCekConstantWitness,
): Uint8Array => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  const semantic = commitMidgardCekDataTree(decoded.payload);
  return hashMidgardCekValueNode({
    kind: "constant",
    typeRoot: hashMidgardCekBlobChunk(witness.typeCbor),
    payloadRoot: semantic.root,
    payloadLength: BigInt(encodeMidgardCekPlutusData(decoded.payload).length),
    semanticRoot: semantic.root,
    memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
  });
};

export const hashMidgardCekSemanticConstantWitness = (
  witness: MidgardCekSemanticConstantWitness,
): Uint8Array => {
  if (witness.typeCbor.length > 64) {
    throw new Error("V1 semantic constant type exceeds its bound");
  }
  decodeMidgardCekConstantTypeCbor(witness.typeCbor);
  if (
    witness.payload.root.length !== 32 ||
    witness.payload.cborLength < 0n ||
    witness.payload.memory < 0n ||
    witness.memory < 0n
  ) {
    throw new Error("V1 semantic constant summary is invalid");
  }
  return hashMidgardCekValueNode({
    kind: "constant",
    typeRoot: hashMidgardCekBlobChunk(witness.typeCbor),
    payloadRoot: witness.payload.root,
    payloadLength: witness.payload.cborLength,
    semanticRoot: witness.payload.root,
    memory: witness.memory,
  });
};

/**
 * ExMemory size of the semantic payload committed by a constant witness.
 * Lists and pairs sum element sizes without an extra container charge.
 */
export const midgardCekConstantMemorySize = (
  type: MidgardCekConstantType,
  payload: Data,
): bigint => {
  switch (type.kind) {
    case "integer":
      if (!(payload instanceof DataI)) {
        throw new Error("V1 integer payload is not DataI");
      }
      return midgardCekIntegerMemorySize(payload.int);
    case "bytes":
    case "string":
      if (!(payload instanceof DataB)) {
        throw new Error("V1 byte payload is not DataB");
      }
      return midgardCekByteStringMemorySize(payload.bytes);
    case "unit":
    case "boolean":
      if (!(payload instanceof DataConstr)) {
        throw new Error("V1 scalar payload is not DataConstr");
      }
      return 1n;
    case "list":
      if (!(payload instanceof DataList)) {
        throw new Error("V1 list payload is not DataList");
      }
      return payload.list.reduce(
        (total, item) =>
          total + midgardCekConstantMemorySize(type.element, item),
        0n,
      );
    case "pair":
      if (
        !(payload instanceof DataConstr) ||
        payload.constr !== 0n ||
        payload.fields.length !== 2
      ) {
        throw new Error("V1 pair payload is malformed");
      }
      return (
        midgardCekConstantMemorySize(type.first, payload.fields[0]) +
        midgardCekConstantMemorySize(type.second, payload.fields[1])
      );
    case "data":
      return midgardCekDataMemorySize(payload);
    case "blsG1":
      return 48n;
    case "blsG2":
      return 96n;
    case "blsMillerLoopResult":
      return 192n;
  }
};

/**
 * Canonical semantic representation consumed by both the off-chain CEK and
 * the L1 builtin verifier. Raw Flat is intentionally not the runtime payload:
 * script identity commits the canonical program envelope instead.
 */
const encodeCanonicalConstant = (
  typeTags: ConstType,
  value: ConstValue,
): MidgardCekCanonicalConstant => {
  const type = parseMidgardCekConstantType(typeTags);
  return Object.freeze({
    type,
    typeCbor: encodeMidgardCekPlutusData(
      new DataList(typeTags.map((tag) => new DataI(BigInt(tag)))),
    ),
    payloadCbor: encodeMidgardCekPlutusData(semanticData(type, value)),
  });
};

export const encodeMidgardCekCanonicalConstant = (
  constant: UPLCConst,
): MidgardCekCanonicalConstant =>
  encodeCanonicalConstant(constant.type, constant.value);

/**
 * `encodeMidgardCekCanonicalConstant(UPLCConst.data(data))` without building
 * the `UPLCConst`, whose constructor stringifies the whole value recursively.
 */
export const encodeMidgardCekCanonicalDataConstant = (
  data: Data,
): MidgardCekCanonicalConstant => encodeCanonicalConstant(constT.data, data);

export const midgardCekUplcConstantMemorySize = (
  constant: UPLCConst,
): bigint => {
  const canonical = encodeMidgardCekCanonicalConstant(constant);
  return midgardCekConstantMemorySize(
    canonical.type,
    plutusDataFromCborIterative(canonical.payloadCbor),
  );
};
