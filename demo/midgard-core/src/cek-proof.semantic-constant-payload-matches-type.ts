import {
  commitSemanticData,
  semanticIntegerMemory,
} from "./cek-proof.commit-semantic-data.js";
import {
  isSemanticConstr,
  isSemanticList,
  type SemanticConstantType,
  type SemanticDataValue,
} from "./cek-proof.program-material-task.js";

export const semanticConstantPayloadMatchesType = (
  type: SemanticConstantType,
  value: SemanticDataValue,
): boolean => {
  if (type.kind === "integer") return typeof value === "bigint";
  if (type.kind === "bytes") return typeof value === "string";
  if (type.kind === "string") {
    if (typeof value !== "string") return false;
    try {
      const bytes = Buffer.from(value, "hex");
      const decoded = new TextDecoder("utf-8", { fatal: true }).decode(bytes);
      return Buffer.from(decoded, "utf8").equals(bytes);
    } catch {
      return false;
    }
  }
  if (type.kind === "unit") {
    return (
      isSemanticConstr(value) &&
      value.constructor === 0n &&
      value.fields.length === 0
    );
  }
  if (type.kind === "boolean") {
    return (
      isSemanticConstr(value) &&
      (value.constructor === 0n || value.constructor === 1n) &&
      value.fields.length === 0
    );
  }
  if (type.kind === "list") {
    return (
      isSemanticList(value) &&
      value.every((item) =>
        semanticConstantPayloadMatchesType(type.element, item),
      )
    );
  }
  if (type.kind === "pair") {
    return (
      isSemanticConstr(value) &&
      value.constructor === 0n &&
      value.fields.length === 2 &&
      semanticConstantPayloadMatchesType(type.first, value.fields[0]!) &&
      semanticConstantPayloadMatchesType(type.second, value.fields[1]!)
    );
  }
  if (type.kind === "data") return true;
  if (type.kind === "blsG1") {
    return typeof value === "string" && Buffer.from(value, "hex").length === 48;
  }
  if (type.kind === "blsG2") {
    return typeof value === "string" && Buffer.from(value, "hex").length === 96;
  }
  return false;
};

export const semanticConstantMemory = (
  type: SemanticConstantType,
  value: SemanticDataValue,
): bigint => {
  if (type.kind === "integer") {
    if (typeof value !== "bigint") {
      throw new Error("CEK integer payload is not an integer");
    }
    return semanticIntegerMemory(value);
  }
  if (type.kind === "bytes" || type.kind === "string") {
    if (typeof value !== "string") {
      throw new Error("CEK bytes payload is not bytes");
    }
    return BigInt(Math.max(1, Buffer.from(value, "hex").length));
  }
  if (type.kind === "unit" || type.kind === "boolean") return 1n;
  if (type.kind === "list") {
    if (!isSemanticList(value)) {
      throw new Error("CEK list payload is not a list");
    }
    return value.reduce<bigint>(
      (total, item) => total + semanticConstantMemory(type.element, item),
      0n,
    );
  }
  if (type.kind === "pair") {
    if (
      !isSemanticConstr(value) ||
      value.constructor !== 0n ||
      value.fields.length !== 2
    ) {
      throw new Error("CEK pair payload is not a pair");
    }
    return (
      semanticConstantMemory(type.first, value.fields[0]!) +
      semanticConstantMemory(type.second, value.fields[1]!)
    );
  }
  if (type.kind === "data") return commitSemanticData(value).memory;
  if (type.kind === "blsG1") return 48n;
  if (type.kind === "blsG2") return 96n;
  return 192n;
};
