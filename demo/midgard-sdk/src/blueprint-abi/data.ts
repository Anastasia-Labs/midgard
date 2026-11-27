import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  type BlueprintAbi,
  blueprintDefinitionName,
  type BlueprintSchema,
  isOpaqueData,
  parseBlueprintAbi,
} from "./abi.js";

const NESTING_BOUND = 2_048;

const dataKind = (value: unknown): string => {
  if (value instanceof Constr) return "constructor";
  if (Array.isArray(value)) return "list";
  if (typeof value === "bigint") return "integer";
  if (typeof value === "string") return "bytes";
  return typeof value;
};

const isCanonicalBytes = (value: unknown): value is string =>
  typeof value === "string" &&
  value.length % 2 === 0 &&
  /^[0-9a-f]*$/u.test(value);

const validateAnyPlutusData = (
  value: unknown,
  path: string,
  depth: number,
): void => {
  if (depth > NESTING_BOUND) {
    throw new Error(`${path} exceeds the Aiken data nesting bound`);
  }
  if (typeof value === "bigint" || isCanonicalBytes(value)) return;
  if (value instanceof Constr) {
    value.fields.forEach((field, index) =>
      validateAnyPlutusData(field, `${path}.${index.toString()}`, depth + 1),
    );
    return;
  }
  if (Array.isArray(value)) {
    value.forEach((item, index) =>
      validateAnyPlutusData(item, `${path}[${index.toString()}]`, depth + 1),
    );
    return;
  }
  if (value instanceof Map) {
    for (const [key, item] of value.entries()) {
      validateAnyPlutusData(key, `${path}.key`, depth + 1);
      validateAnyPlutusData(item, `${path}.value`, depth + 1);
    }
    return;
  }
  throw new Error(`${path} must be Plutus Data, got ${dataKind(value)}`);
};

const validate = (
  abi: BlueprintAbi,
  value: unknown,
  schema: BlueprintSchema,
  path: string,
  depth: number,
): void => {
  if (depth > NESTING_BOUND) {
    throw new Error(`${path} exceeds the Aiken data nesting bound`);
  }
  if (schema.$ref !== undefined) {
    const definition = abi.ref(blueprintDefinitionName(schema.$ref)).schema;
    validate(abi, value, definition, path, depth + 1);
    return;
  }
  if (isOpaqueData(schema)) {
    validateAnyPlutusData(value, path, depth + 1);
    return;
  }
  if (schema.anyOf !== undefined) {
    const failures: string[] = [];
    for (const variant of schema.anyOf) {
      try {
        validate(abi, value, variant, path, depth + 1);
        return;
      } catch (cause) {
        failures.push(cause instanceof Error ? cause.message : String(cause));
      }
    }
    throw new Error(
      `${path} does not match any Aiken constructor: ${failures.join("; ")}`,
    );
  }
  switch (schema.dataType) {
    case "bytes":
      if (!isCanonicalBytes(value)) {
        throw new Error(`${path} must be canonical lowercase bytes`);
      }
      return;
    case "integer":
      if (typeof value !== "bigint") {
        throw new Error(`${path} must be an integer, got ${dataKind(value)}`);
      }
      return;
    case "list": {
      const items = schema.items;
      if (Array.isArray(items)) {
        throw new Error(`${path} uses unsupported Aiken schema type tuple`);
      }
      if (!Array.isArray(value) || items === undefined) {
        throw new Error(`${path} must be an Aiken list`);
      }
      value.forEach((item, index) =>
        validate(
          abi,
          item,
          items as BlueprintSchema,
          `${path}[${index.toString()}]`,
          depth + 1,
        ),
      );
      return;
    }
    case "constructor": {
      if (
        !(value instanceof Constr) ||
        schema.index === undefined ||
        value.index !== schema.index
      ) {
        throw new Error(
          `${path} must be constructor ${schema.index?.toString() ?? "?"}, got ${dataKind(value)}`,
        );
      }
      const fields = schema.fields ?? [];
      if (value.fields.length !== fields.length) {
        throw new Error(
          `${path} constructor ${schema.index.toString()} requires ${fields.length.toString()} fields, got ${value.fields.length.toString()}`,
        );
      }
      fields.forEach((field, index) =>
        validate(
          abi,
          value.fields[index],
          field,
          `${path}.${field.title ?? index.toString()}`,
          depth + 1,
        ),
      );
      return;
    }
    case undefined:
    default:
      throw new Error(
        `${path} uses unsupported Aiken schema type ${String(schema.dataType)}`,
      );
  }
};

/**
 * Decodes `cbor` as Plutus Data and checks it is exactly a value of the
 * blueprint definition `definitionName`: canonical lowercase hex within
 * `maxBytes`, well-formed with no trailing bytes, and every constructor
 * index, arity, integer, byte string and list the definition declares.
 * Opaque `Data` positions accept any Plutus Data. Returns the decoded value.
 */
export const parseExactAikenDataCbor = ({
  blueprint,
  definitionName,
  cbor,
  maxBytes,
}: {
  readonly blueprint: unknown;
  readonly definitionName: string;
  readonly cbor: string;
  readonly maxBytes: number;
}): unknown => {
  if (
    cbor.length === 0 ||
    cbor.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(cbor)
  ) {
    throw new Error("Aiken data CBOR must be non-empty lowercase hex");
  }
  if (!Number.isSafeInteger(maxBytes) || maxBytes <= 0) {
    throw new Error("Aiken data maxBytes must be a positive safe integer");
  }
  const byteLength = cbor.length / 2;
  if (byteLength > maxBytes) {
    throw new Error(
      `Aiken data CBOR is ${byteLength.toString()} bytes, exceeding ${maxBytes.toString()}`,
    );
  }
  // Parse through CML as well as Lucid. CML rejects malformed/trailing CBOR,
  // but we intentionally do not require its definite-array re-encoding:
  // Lucid's generated Aiken ABI uses valid indefinite constructor/list arrays.
  canonicalPlutusDataCbor(cbor);
  const parsed = Data.from(cbor);
  const ref = parseBlueprintAbi(blueprint).ref(definitionName);
  validate(ref.abi, parsed, ref.schema, definitionName, 0);
  return parsed;
};
