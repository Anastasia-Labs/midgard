#!/usr/bin/env node

import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const defaultRegistryPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-format-registry-v1.json",
);
const defaultBlueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");
const defaultSdkModulePath = resolve(
  repoRoot,
  "demo/midgard-sdk/dist/index.js",
);

// Redirection hooks for the behavioral self-test. Each names ONE input; the
// self-test seeds a mismatch into exactly one of them and leaves the other two
// real, so a run that passed only because two synthesized projections agreed
// with each other cannot happen here. Precedence is flag, then environment
// variable, then the real repository path.
const underTestArgument = (flag) => {
  const match = process.argv
    .slice(2)
    .find((argument) => argument.startsWith(`--${flag}=`));
  return match === undefined ? null : resolve(match.slice(flag.length + 3));
};
const inputPath = (flag, environmentVariable, fallback) =>
  underTestArgument(flag) ??
  (process.env[environmentVariable] === undefined
    ? fallback
    : resolve(process.env[environmentVariable]));

const isRecord = (value) =>
  value !== null && typeof value === "object" && !Array.isArray(value);

const contractError = (message) =>
  new Error(`HeaderV1 ABI contract: ${message}`);

const requireRecord = (value, label) => {
  if (!isRecord(value)) throw contractError(`${label} must be an object`);
  return value;
};

const requireString = (value, label) => {
  if (typeof value !== "string" || value.length === 0) {
    throw contractError(`${label} must be a non-empty string`);
  }
  return value;
};

const requireArray = (value, label, length) => {
  if (
    !Array.isArray(value) ||
    (length !== undefined && value.length !== length)
  ) {
    const expected =
      length === undefined ? "an array" : `an array of length ${length}`;
    throw contractError(`${label} must be ${expected}`);
  }
  return value;
};

const FIELD_COUNT = 25;

/**
 * Validate the registry-owned machine contract without consulting any source
 * projection.  The registry contract is the only hand-maintained ABI
 * authority; SDK and blueprint shapes are checked against it below.
 */
export const assertHeaderV1AbiContract = (value) => {
  const contract = requireRecord(value, "contract");
  requireString(contract.schema, "contract.schema");
  if (contract.schema !== "midgard-header-v1-abi-v1") {
    throw contractError("contract.schema must be midgard-header-v1-abi-v1");
  }
  if (contract.name !== "HeaderV1") {
    throw contractError("contract.name must be HeaderV1");
  }
  if (contract.version !== 1) {
    throw contractError("contract.version must be 1");
  }
  if (contract.blueprintDefinition !== "midgard/ledger_state/HeaderV1") {
    throw contractError(
      "contract.blueprintDefinition must be midgard/ledger_state/HeaderV1",
    );
  }

  const constructor = requireRecord(
    contract.constructor,
    "contract.constructor",
  );
  if (constructor.tag !== 0) {
    throw contractError("contract.constructor.tag must be 0");
  }
  if (constructor.arity !== FIELD_COUNT) {
    throw contractError(
      `contract.constructor.arity must be ${FIELD_COUNT.toString()}`,
    );
  }

  const fields = requireArray(contract.fields, "contract.fields", FIELD_COUNT);
  const names = [];
  const sdkNames = [];
  const groups = [];
  const types = [];
  const aikenRefs = [];
  for (const [index, fieldValue] of fields.entries()) {
    const label = `contract.fields[${index.toString()}]`;
    const field = requireRecord(fieldValue, label);
    if (field.index !== index) {
      throw contractError(`${label}.index must be ${index.toString()}`);
    }
    names.push(requireString(field.name, `${label}.name`));
    sdkNames.push(requireString(field.sdkName, `${label}.sdkName`));
    groups.push(requireString(field.group, `${label}.group`));
    const type = requireString(field.type, `${label}.type`);
    if (!/^(?:integer|bytes(?:28|32))$/u.test(type)) {
      throw contractError(`${label}.type is not a supported structural type`);
    }
    types.push(type);
    aikenRefs.push(requireString(field.aikenRef, `${label}.aikenRef`));
  }

  const exactFields = requireArray(
    contract.exactFields,
    "contract.exactFields",
    FIELD_COUNT,
  );
  if (JSON.stringify(exactFields) !== JSON.stringify(names)) {
    throw contractError(
      "contract.exactFields must equal the indexed field names",
    );
  }
  if (new Set(names).size !== names.length) {
    throw contractError("contract field names must be unique");
  }
  if (new Set(sdkNames).size !== sdkNames.length) {
    throw contractError("contract SDK field names must be unique");
  }
  const constructorTags = requireArray(
    contract.constructorTags,
    "contract.constructorTags",
    1,
  );
  if (constructorTags[0] !== constructor.tag) {
    throw contractError(
      "contract.constructorTags must identify constructor tag 0",
    );
  }

  const contractGroups = requireArray(contract.groups, "contract.groups", 3);
  const groupNames = new Set();
  let coveredFields = 0;
  for (const [groupIndex, groupValue] of contractGroups.entries()) {
    const group = requireRecord(
      groupValue,
      `contract.groups[${groupIndex.toString()}]`,
    );
    const name = requireString(
      group.name,
      `contract.groups[${groupIndex.toString()}].name`,
    );
    if (groupNames.has(name)) {
      throw contractError(`contract.groups contains duplicate group ${name}`);
    }
    groupNames.add(name);
    if (!Number.isSafeInteger(group.start) || group.start !== coveredFields) {
      throw contractError(
        `contract.groups[${groupIndex.toString()}].start must continue at ${coveredFields.toString()}`,
      );
    }
    if (!Number.isSafeInteger(group.count) || group.count <= 0) {
      throw contractError(
        `contract.groups[${groupIndex.toString()}].count must be a positive integer`,
      );
    }
    for (
      let index = group.start;
      index < group.start + group.count;
      index += 1
    ) {
      if (index >= FIELD_COUNT || groups[index] !== name) {
        throw contractError(
          `contract.fields[${index.toString()}].group must be ${name}`,
        );
      }
    }
    coveredFields += group.count;
  }
  if (coveredFields !== FIELD_COUNT) {
    throw contractError(
      `contract.groups must cover all ${FIELD_COUNT.toString()} fields`,
    );
  }

  return {
    contract,
    names,
    sdkNames,
    groups,
    types,
    aikenRefs,
    constructorTag: constructor.tag,
    constructorArity: constructor.arity,
  };
};

const decodeJsonPointerReference = (reference) => {
  const prefix = "#/definitions/";
  if (typeof reference !== "string" || !reference.startsWith(prefix)) {
    throw new Error(`unsupported blueprint reference ${String(reference)}`);
  }
  return reference
    .slice(prefix.length)
    .replaceAll("~1", "/")
    .replaceAll("~0", "~");
};

const blueprintSchemaKind = (schema, definitions, resolving = new Set()) => {
  const value = requireRecord(schema, "blueprint schema");
  if (value.$ref !== undefined) {
    const name = decodeJsonPointerReference(value.$ref);
    if (resolving.has(name)) {
      throw new Error(`recursive blueprint reference ${name}`);
    }
    const definition = definitions[name];
    if (definition === undefined) {
      throw new Error(`missing blueprint definition ${name}`);
    }
    return blueprintSchemaKind(
      definition,
      definitions,
      new Set([...resolving, name]),
    );
  }
  if (value.anyOf !== undefined) return "constructor";
  if (value.dataType === "bytes") return "bytes";
  if (value.dataType === "integer") return "integer";
  if (value.dataType === "list") return "list";
  if (value.dataType === "map") return "map";
  if (value.dataType === "constructor") return "constructor";
  throw new Error(
    `unsupported blueprint schema kind ${String(value.dataType)}`,
  );
};

/** Extract HeaderV1's constructor shape from the generated Aiken blueprint. */
export const extractHeaderV1Blueprint = (blueprint) => {
  const value = requireRecord(blueprint, "blueprint");
  const definitions = requireRecord(value.definitions, "blueprint.definitions");
  const definition = requireRecord(
    definitions["midgard/ledger_state/HeaderV1"],
    "blueprint HeaderV1 definition",
  );
  const variants = requireArray(
    definition.anyOf,
    "blueprint HeaderV1 variants",
  );
  const constructor = variants.find((candidate) => candidate?.index === 0);
  if (constructor === undefined) {
    throw new Error("blueprint HeaderV1 constructor 0 is missing");
  }
  const fields = requireArray(constructor.fields, "blueprint HeaderV1 fields");
  return {
    definition: "midgard/ledger_state/HeaderV1",
    constructorTag: constructor.index,
    constructorArity: fields.length,
    fields: fields.map((field, index) => {
      const entry = requireRecord(
        field,
        `blueprint HeaderV1 field ${index.toString()}`,
      );
      return {
        index,
        name: requireString(
          entry.title,
          `blueprint HeaderV1 field ${index.toString()}.title`,
        ),
        aikenRef:
          entry.$ref === undefined
            ? null
            : decodeJsonPointerReference(entry.$ref),
        type: blueprintSchemaKind(entry, definitions),
      };
    }),
  };
};

const schemaKind = (schema) => {
  const value = requireRecord(schema, "SDK schema");
  if (value.$ref !== undefined) return "data";
  if (value.dataType === "integer") return "integer";
  if (value.dataType === "bytes") {
    const min = value.minLength;
    const max = value.maxLength;
    if (Number.isSafeInteger(min) && Number.isSafeInteger(max) && min === max) {
      return `bytes${min.toString()}`;
    }
    return "bytes";
  }
  if (value.anyOf !== undefined || value.dataType === "constructor") {
    return "constructor";
  }
  if (value.dataType === "list") return "list";
  if (value.dataType === "map") return "map";
  return "data";
};

/** Extract HeaderV1's constructor shape from the SDK's runtime Data schema. */
export const extractHeaderV1SdkSchema = (schema) => {
  const value = requireRecord(schema, "SDK HeaderV1Schema");
  const variants =
    value.anyOf ?? (value.dataType === "constructor" ? [value] : null);
  if (variants === null && isRecord(value.fields)) {
    const entries = Object.entries(value.fields);
    return {
      constructorTag: 0,
      constructorArity: entries.length,
      fields: entries.map(([name, field], index) => ({
        index,
        name: requireString(
          name,
          `SDK HeaderV1 field ${index.toString()}.name`,
        ),
        type: schemaKind(field),
      })),
    };
  }
  const constructors = requireArray(variants, "SDK HeaderV1 constructors");
  const constructor = constructors.find((candidate) => candidate?.index === 0);
  if (constructor === undefined) {
    throw new Error("SDK HeaderV1 constructor 0 is missing");
  }
  const fields = requireArray(constructor.fields, "SDK HeaderV1 fields");
  return {
    constructorTag: constructor.index,
    constructorArity: fields.length,
    fields: fields.map((field, index) => {
      const entry = requireRecord(
        field,
        `SDK HeaderV1 field ${index.toString()}`,
      );
      return {
        index,
        name: requireString(
          entry.title,
          `SDK HeaderV1 field ${index.toString()}.title`,
        ),
        type: schemaKind(entry),
      };
    }),
  };
};

const compare = (actual, expected, label) => {
  if (actual !== expected) {
    throw new Error(
      `HeaderV1 ABI ${label} mismatch: expected ${expected}, got ${actual}`,
    );
  }
};

/** Compare both structural projections to the registry-owned contract. */
export const verifyHeaderV1Abi = ({
  contract: rawContract,
  blueprint,
  sdkSchema,
}) => {
  const normalized = assertHeaderV1AbiContract(rawContract);
  const actualBlueprint = extractHeaderV1Blueprint(blueprint);
  compare(
    actualBlueprint.definition,
    normalized.contract.blueprintDefinition,
    "blueprint definition",
  );
  compare(
    actualBlueprint.constructorTag,
    normalized.constructorTag,
    "Aiken constructor tag",
  );
  compare(
    actualBlueprint.constructorArity,
    normalized.constructorArity,
    "Aiken constructor arity",
  );
  for (const [index, field] of actualBlueprint.fields.entries()) {
    compare(field.index, index, `Aiken field ${index.toString()} index`);
    compare(
      field.name,
      normalized.names[index],
      `Aiken field ${index.toString()} name`,
    );
    compare(
      field.aikenRef,
      normalized.aikenRefs[index],
      `Aiken field ${index.toString()} definition`,
    );
    compare(
      field.type,
      normalized.types[index].replace(/(?:28|32)$/u, ""),
      `Aiken field ${index.toString()} type`,
    );
  }

  const actualSdk = extractHeaderV1SdkSchema(sdkSchema);
  compare(
    actualSdk.constructorTag,
    normalized.constructorTag,
    "SDK constructor tag",
  );
  compare(
    actualSdk.constructorArity,
    normalized.constructorArity,
    "SDK constructor arity",
  );
  for (const [index, field] of actualSdk.fields.entries()) {
    compare(field.index, index, `SDK field ${index.toString()} index`);
    compare(
      field.name,
      normalized.sdkNames[index],
      `SDK field ${index.toString()} name`,
    );
    compare(
      field.type,
      normalized.types[index],
      `SDK field ${index.toString()} type`,
    );
  }

  return normalized;
};

const loadJson = (path, label) => {
  if (!existsSync(path)) throw new Error(`${label} does not exist: ${path}`);
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch (error) {
    throw new Error(`${label} is invalid JSON: ${error.message}`);
  }
};

/**
 * The only path that compares the registry-owned contract against BOTH real
 * projections: the blueprint the Aiken compiler generated and the Data schema
 * the built SDK exports at runtime.  Nothing here is synthesized from the
 * contract under test.
 */
export const run = async () => {
  const registryPath = inputPath(
    "registry-under-test",
    "MIDGARD_FORMAT_REGISTRY_PATH",
    defaultRegistryPath,
  );
  const registry = loadJson(registryPath, "format registry");
  const row = registry.formats?.find((candidate) => candidate?.id === "L01");
  if (!row?.canonicalForms?.[0]) {
    throw new Error("format registry L01 must provide canonicalForms[0]");
  }
  const contract = row.canonicalForms[0];
  const blueprintPath = inputPath(
    "blueprint-under-test",
    "MIDGARD_REAL_BLUEPRINT_PATH",
    defaultBlueprintPath,
  );
  const sdkModulePath = inputPath(
    "sdk-module-under-test",
    "MIDGARD_SDK_MODULE_PATH",
    defaultSdkModulePath,
  );
  if (!existsSync(sdkModulePath)) {
    throw new Error(
      `SDK runtime module does not exist: ${sdkModulePath}; build @al-ft/midgard-sdk first`,
    );
  }
  const sdk = await import(pathToFileURL(sdkModulePath).href);
  const normalized = verifyHeaderV1Abi({
    contract,
    blueprint: loadJson(blueprintPath, "Aiken blueprint"),
    sdkSchema: sdk.HeaderV1Schema,
  });
  // Provenance, not evidence: this line names the three inputs that were
  // actually read so a passing run can be attributed. It is not a test name
  // and must never be cited as one — the format-registry gate rejects any row
  // that cites an output literal of a script it names.
  process.stdout.write(
    [
      `header-v1-abi: PASS constructor ${normalized.constructorTag} arity ${normalized.constructorArity}`,
      `  registry:  ${registryPath}`,
      `  blueprint: ${blueprintPath}`,
      `  sdk:       ${sdkModulePath}`,
      "",
    ].join("\n"),
  );
  return normalized;
};

const invokedDirectly =
  process.argv[1] !== undefined &&
  pathToFileURL(resolve(process.argv[1])).href === import.meta.url;
if (invokedDirectly) {
  try {
    await run();
  } catch (error) {
    process.stderr.write(
      `HeaderV1 ABI verification failed: ${error.message}\n`,
    );
    process.exitCode = 1;
  }
}
