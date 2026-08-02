import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

type AikenSchema = {
  readonly $ref?: string;
  readonly anyOf?: readonly AikenSchema[];
  readonly dataType?: "bytes" | "constructor" | "integer" | "list";
  readonly description?: string;
  readonly index?: number;
  readonly fields?: readonly AikenSchema[];
  readonly items?: AikenSchema;
  readonly title?: string;
};

type AikenBlueprint = {
  readonly definitions: Readonly<Record<string, AikenSchema>>;
};

const requireBlueprint = (value: unknown): AikenBlueprint => {
  if (typeof value !== "object" || value === null) {
    throw new Error("Aiken blueprint must be an object");
  }
  const definitions = (value as { readonly definitions?: unknown }).definitions;
  if (
    typeof definitions !== "object" ||
    definitions === null ||
    Array.isArray(definitions)
  ) {
    throw new Error("Aiken blueprint must contain definitions");
  }
  return {
    definitions: definitions as Readonly<Record<string, AikenSchema>>,
  };
};

const definitionNameFromRef = (ref: string): string => {
  const prefix = "#/definitions/";
  if (!ref.startsWith(prefix)) {
    throw new Error(`Unsupported Aiken schema reference ${ref}`);
  }
  return ref.slice(prefix.length).replace(/~1/gu, "/").replace(/~0/gu, "~");
};

const dataKind = (value: unknown): string => {
  if (value instanceof Constr) return "constructor";
  if (Array.isArray(value)) return "list";
  if (typeof value === "bigint") return "integer";
  if (typeof value === "string") return "bytes";
  return typeof value;
};

const validateAnyPlutusData = (
  value: unknown,
  path: string,
  depth: number,
): void => {
  if (depth > 2_048) {
    throw new Error(`${path} exceeds the Aiken data nesting bound`);
  }
  if (typeof value === "bigint") return;
  if (
    typeof value === "string" &&
    value.length % 2 === 0 &&
    /^[0-9a-f]*$/u.test(value)
  ) {
    return;
  }
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

const validate = ({
  value,
  schema,
  definitions,
  path,
  depth,
}: {
  readonly value: unknown;
  readonly schema: AikenSchema;
  readonly definitions: AikenBlueprint["definitions"];
  readonly path: string;
  readonly depth: number;
}): void => {
  if (depth > 2_048) {
    throw new Error(`${path} exceeds the Aiken data nesting bound`);
  }
  if (schema.$ref !== undefined) {
    const definitionName = definitionNameFromRef(schema.$ref);
    const definition = definitions[definitionName];
    if (definition === undefined) {
      throw new Error(
        `${path} references missing Aiken definition ${definitionName}`,
      );
    }
    validate({
      value,
      schema: definition,
      definitions,
      path,
      depth: depth + 1,
    });
    return;
  }
  if (schema.anyOf !== undefined) {
    const failures: string[] = [];
    for (const variant of schema.anyOf) {
      try {
        validate({
          value,
          schema: variant,
          definitions,
          path,
          depth: depth + 1,
        });
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
      if (
        typeof value !== "string" ||
        value.length % 2 !== 0 ||
        !/^[0-9a-f]*$/u.test(value)
      ) {
        throw new Error(`${path} must be canonical lowercase bytes`);
      }
      return;
    case "integer":
      if (typeof value !== "bigint") {
        throw new Error(`${path} must be an integer, got ${dataKind(value)}`);
      }
      return;
    case "list":
      if (!Array.isArray(value) || schema.items === undefined) {
        throw new Error(`${path} must be an Aiken list`);
      }
      value.forEach((item, index) =>
        validate({
          value: item,
          schema: schema.items!,
          definitions,
          path: `${path}[${index.toString()}]`,
          depth: depth + 1,
        }),
      );
      return;
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
        validate({
          value: value.fields[index],
          schema: field,
          definitions,
          path: `${path}.${field.title ?? index.toString()}`,
          depth: depth + 1,
        }),
      );
      return;
    }
    default:
      if (
        schema.title === "Data" &&
        schema.description === "Any Plutus data."
      ) {
        validateAnyPlutusData(value, path, depth + 1);
        return;
      }
      throw new Error(
        `${path} uses unsupported Aiken schema type ${String(schema.dataType)}`,
      );
  }
};

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
  const parsedBlueprint = requireBlueprint(blueprint);
  const definition = parsedBlueprint.definitions[definitionName];
  if (definition === undefined) {
    throw new Error(`Aiken blueprint is missing definition ${definitionName}`);
  }
  validate({
    value: parsed,
    schema: definition,
    definitions: parsedBlueprint.definitions,
    path: definitionName,
    depth: 0,
  });
  return parsed;
};
