/**
 * The one reader of a compiled Aiken blueprint (CIP-57 `plutus.json`).
 *
 * The blueprint is the on-chain ABI as the compiler emitted it. Everything here
 * fails closed: a missing validator title, a missing definition or a `$ref`
 * that is not a `#/definitions/<name>` pointer throws rather than degrading to
 * a skipped check. Definitions are read lazily, on lookup, so parsing an 11 MB
 * blueprint costs one pass over its validators and nothing more.
 */

/** One node of a blueprint schema, as the Aiken compiler emits it. */
export type BlueprintSchema = {
  readonly $ref?: string;
  readonly title?: string;
  readonly description?: string;
  readonly dataType?: string;
  readonly anyOf?: readonly BlueprintSchema[];
  readonly index?: number;
  readonly fields?: readonly BlueprintSchema[];
  /** One schema for a list, one per position for a tuple. */
  readonly items?: BlueprintSchema | readonly BlueprintSchema[];
  readonly keys?: BlueprintSchema;
  readonly values?: BlueprintSchema;
};

/** One `validator main(...)` parameter, in declaration order. */
export type BlueprintParameter = {
  readonly title: string;
  /** The parameter's `schema.$ref`, when its schema is a reference. */
  readonly $ref?: string;
};

export type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  /** Absent in the JSON means zero declared, never "unknown". */
  readonly parameters: readonly BlueprintParameter[];
  readonly datum?: BlueprintSchema;
  readonly redeemer?: BlueprintSchema;
};

/** One constructor of a sum-type definition. */
export type BlueprintConstructor = {
  readonly title: string;
  readonly index: number;
  /** Field titles in declared order, which is the Plutus Data field order. */
  readonly fieldTitles: readonly string[];
};

/** A definition the blueprint is known to contain. */
export type BlueprintRef = {
  readonly abi: BlueprintAbi;
  /** The decoded definition name, e.g. `midgard/ledger_state/HeaderV1`. */
  readonly name: string;
  readonly schema: BlueprintSchema;
  /** Every constructor, by index; throws unless each is titled and indexed. */
  readonly constructors: () => readonly BlueprintConstructor[];
  /** The constructor with this Aiken title; throws when there is none. */
  readonly constructorTitled: (title: string) => BlueprintConstructor;
};

export type BlueprintAbi = {
  readonly validators: readonly BlueprintValidator[];
  /** The one validator with this title; throws on none or several. */
  readonly validator: (title: string) => BlueprintValidator;
  /** A definition by name or by `#/definitions/` pointer; throws when absent. */
  readonly ref: (nameOrPointer: string) => BlueprintRef;
};

const DEFINITIONS_POINTER = "#/definitions/";

/**
 * Aiken's `Data` and every opaque alias of it (`cardano/transaction/Redeemer`)
 * carry this description and no shape: any Plutus Data inhabits them.
 */
export const isOpaqueData = (schema: BlueprintSchema): boolean =>
  schema.dataType === undefined &&
  schema.anyOf === undefined &&
  schema.description === "Any Plutus data.";

/**
 * Decodes a `#/definitions/<name>` pointer (RFC 6901 escaping) to the
 * definition name. Any other `$ref` form is refused.
 */
export const blueprintDefinitionName = (pointer: string): string => {
  const encoded = pointer.startsWith(DEFINITIONS_POINTER)
    ? pointer.slice(DEFINITIONS_POINTER.length)
    : "";
  if (encoded === "" || encoded.includes("/") || /~(?![01])/u.test(encoded)) {
    throw new Error(
      `Unsupported blueprint $ref ${JSON.stringify(pointer)}: only ${DEFINITIONS_POINTER}<name> pointers are read`,
    );
  }
  return encoded.replace(/~1/gu, "/").replace(/~0/gu, "~");
};

const isRecord = (value: unknown): value is Readonly<Record<string, unknown>> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const schemaRef = (schema: unknown): string | undefined => {
  if (!isRecord(schema) || schema["$ref"] === undefined) return undefined;
  const pointer = schema["$ref"];
  if (typeof pointer !== "string") {
    throw new Error("Blueprint $ref must be a string");
  }
  blueprintDefinitionName(pointer);
  return pointer;
};

const parseParameter = (
  parameter: unknown,
  path: string,
): BlueprintParameter => {
  if (!isRecord(parameter) || typeof parameter["title"] !== "string") {
    throw new Error(`${path}.title must be a string`);
  }
  // An SDK-normalized parameter record re-enters as `schemaRef` after it has
  // crossed an adapter; it names the same pointer as the raw `schema.$ref`.
  const $ref =
    schemaRef(parameter["schema"]) ??
    (typeof parameter["schemaRef"] === "string"
      ? schemaRef({ $ref: parameter["schemaRef"] })
      : undefined);
  return $ref === undefined
    ? { title: parameter["title"] }
    : { title: parameter["title"], $ref };
};

const argumentSchema = (
  argument: unknown,
  path: string,
): BlueprintSchema | undefined => {
  if (argument === undefined) return undefined;
  if (!isRecord(argument) || !isRecord(argument["schema"])) {
    throw new Error(`${path}.schema must be an object`);
  }
  schemaRef(argument["schema"]);
  return argument["schema"] as BlueprintSchema;
};

const parseValidator = (
  validator: unknown,
  index: number,
): BlueprintValidator => {
  const path = `validators[${index.toString()}]`;
  if (typeof validator !== "object" || validator === null) {
    throw new Error(`${path} must be an object`);
  }
  const candidate = validator as Readonly<Record<string, unknown>>;
  if (typeof candidate["title"] !== "string") {
    throw new Error(`${path}.title must be a string`);
  }
  if (typeof candidate["compiledCode"] !== "string") {
    throw new Error(`${path}.compiledCode must be a string`);
  }
  const rawParameters = candidate["parameters"] ?? [];
  if (!Array.isArray(rawParameters)) {
    throw new Error(`${path}.parameters must be an array when present`);
  }
  const datum = argumentSchema(candidate["datum"], `${path}.datum`);
  const redeemer = argumentSchema(candidate["redeemer"], `${path}.redeemer`);
  return {
    title: candidate["title"],
    compiledCode: candidate["compiledCode"],
    parameters: rawParameters.map((parameter: unknown, parameterIndex) =>
      parseParameter(
        parameter,
        `${path}.parameters[${parameterIndex.toString()}]`,
      ),
    ),
    ...(datum === undefined ? {} : { datum }),
    ...(redeemer === undefined ? {} : { redeemer }),
  };
};

const parseConstructor = (
  name: string,
  constructor: BlueprintSchema,
): BlueprintConstructor => {
  if (
    typeof constructor.title !== "string" ||
    !Number.isSafeInteger(constructor.index)
  ) {
    throw new Error(
      `Blueprint definition "${name}" carries an untitled or unindexed constructor.`,
    );
  }
  const title = constructor.title;
  return {
    title,
    index: constructor.index!,
    fieldTitles: (constructor.fields ?? []).map((field, position) => {
      if (typeof field.title !== "string") {
        throw new Error(
          `Blueprint constructor "${name}.${title}" field ${position.toString()} has no title.`,
        );
      }
      return field.title;
    }),
  };
};

const makeRef = (
  abi: BlueprintAbi,
  name: string,
  schema: BlueprintSchema,
): BlueprintRef => {
  const constructors = (): readonly BlueprintConstructor[] => {
    if (!Array.isArray(schema.anyOf)) {
      throw new Error(
        `Blueprint definition "${name}" declares no constructors.`,
      );
    }
    return schema.anyOf
      .map((constructor) => parseConstructor(name, constructor))
      .sort((left, right) => left.index - right.index);
  };
  return {
    abi,
    name,
    schema,
    constructors,
    constructorTitled: (title) => {
      const found = constructors().find(
        (constructor) => constructor.title === title,
      );
      if (found === undefined) {
        throw new Error(
          `Blueprint definition "${name}" declares no constructor "${title}".`,
        );
      }
      return found;
    },
  };
};

/**
 * Parses a blueprint's validators eagerly and its definitions on lookup.
 * `validators[]` is required; an absent `definitions` section reads as empty,
 * so every lookup in it throws.
 */
export const parseBlueprintAbi = (json: unknown): BlueprintAbi => {
  if (typeof json !== "object" || json === null) {
    throw new Error("Blueprint must be a JSON object");
  }
  const candidate = json as Readonly<Record<string, unknown>>;
  if (!Array.isArray(candidate["validators"])) {
    throw new Error("Blueprint must contain validators[]");
  }
  const rawDefinitions = candidate["definitions"] ?? {};
  if (!isRecord(rawDefinitions)) {
    throw new Error("Blueprint definitions must be an object when present");
  }
  const validators = candidate["validators"].map(
    (validator: unknown, index: number) => parseValidator(validator, index),
  );
  const abi: BlueprintAbi = {
    validators,
    validator: (title) => {
      const matches = validators.filter(
        (validator) => validator.title === title,
      );
      if (matches.length !== 1) {
        throw new Error(
          `Blueprint must contain exactly one validator with title "${title}"; found ${matches.length.toString()}.`,
        );
      }
      return matches[0]!;
    },
    ref: (nameOrPointer) => {
      const name = nameOrPointer.startsWith("#")
        ? blueprintDefinitionName(nameOrPointer)
        : nameOrPointer;
      const schema = Object.prototype.hasOwnProperty.call(rawDefinitions, name)
        ? rawDefinitions[name]
        : undefined;
      if (schema === undefined) {
        throw new Error(`Blueprint is missing definition "${name}"`);
      }
      if (!isRecord(schema)) {
        throw new Error(`Blueprint definition "${name}" must be an object`);
      }
      return makeRef(abi, name, schema as BlueprintSchema);
    },
  };
  return abi;
};
