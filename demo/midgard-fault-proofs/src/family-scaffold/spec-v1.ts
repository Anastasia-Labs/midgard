/**
 * Explicit family-scaffold specification (Goal task `Q02`).
 *
 * The generator creates boilerplate only, so it must be *told* every schema it
 * emits. This module is the strict, fail-closed parser for that instruction:
 * unknown keys, missing test classes, implicit state shapes, and unnamed fields
 * are rejected rather than defaulted. A spec that does not name a positive
 * test, a valid-block negative test, a mutation test, and a maximum/adversarial
 * proof-fit test cannot be generated at all — those are `GOAL_SPEC.md` §9.1
 * outputs 4 and 5 and §3 invariant 9.
 */
export const FRAUD_PROOF_FAMILY_SCAFFOLD_SPEC_V1_SCHEMA_VERSION =
  "midgard-fraud-proof-family-scaffold-v1" as const;

export type ScaffoldFieldTypeV1 =
  | "hash32"
  | "midgard_inputs_hash"
  | "int"
  | "bytes";

export type ScaffoldFieldV1 = {
  readonly name: string;
  readonly type: ScaffoldFieldTypeV1;
  readonly doc: string;
};

export type ScaffoldStepV1 = {
  readonly index: number;
  /** Normative rule statement this step advances (§9.1 output 1). */
  readonly rule: string;
  /** `null` only for step 1, which carries no input state. */
  readonly inputState: readonly ScaffoldFieldV1[] | null;
  /** `null` only for the terminal step, which produces the proof token. */
  readonly outputState: readonly ScaffoldFieldV1[] | null;
  /**
   * Extra redeemer arguments beyond the positional indices every step carries.
   * `null` only for step 1, whose args are `NativeTxInclusionArgs`.
   */
  readonly argsFields: readonly ScaffoldFieldV1[] | null;
};

export const SCAFFOLD_TEST_CLASSES_V1 = [
  "positive",
  "validBlockNegative",
  "mutation",
  "maximumFit",
] as const;
export type ScaffoldTestClassV1 = (typeof SCAFFOLD_TEST_CLASSES_V1)[number];

export type ScaffoldTestsV1 = Readonly<
  Record<ScaffoldTestClassV1, readonly string[]>
>;

export type FraudProofFamilyScaffoldSpecV1 = {
  readonly schemaVersion: typeof FRAUD_PROOF_FAMILY_SCAFFOLD_SPEC_V1_SCHEMA_VERSION;
  /** kebab-case family identifier, e.g. `zero-input`. */
  readonly family: string;
  /** Goal task identifier that owns the closure, e.g. `Q14`. */
  readonly taskId: string;
  /** Normative violation identifier (§9.1 output 1). */
  readonly violationId: string;
  /** Catalogue category name, e.g. `zeroInput` (§9.1 output 6). */
  readonly catalogueCategory: string;
  readonly summary: string;
  readonly steps: readonly ScaffoldStepV1[];
  readonly tests: ScaffoldTestsV1;
};

export type FamilyScaffoldRejectionCodeV1 =
  | "not_an_object"
  | "unknown_key"
  | "missing_key"
  | "invalid_schema_version"
  | "invalid_family_name"
  | "invalid_task_id"
  | "invalid_identifier"
  | "invalid_text"
  | "invalid_step_sequence"
  | "invalid_state_shape"
  | "unknown_field_type"
  | "duplicate_field"
  | "empty_test_class"
  | "duplicate_test_name";

export class FamilyScaffoldRejectionV1 extends Error {
  readonly code: FamilyScaffoldRejectionCodeV1;
  readonly path: string;

  constructor(code: FamilyScaffoldRejectionCodeV1, path: string) {
    super(`${code}: ${path}`);
    this.name = "FamilyScaffoldRejectionV1";
    this.code = code;
    this.path = path;
  }
}

const reject = (code: FamilyScaffoldRejectionCodeV1, path: string): never => {
  throw new FamilyScaffoldRejectionV1(code, path);
};

const KEBAB = /^[a-z][a-z0-9]*(-[a-z0-9]+)*$/u;
const SNAKE = /^[a-z][a-z0-9]*(_[a-z0-9]+)*$/u;
const CAMEL = /^[a-z][A-Za-z0-9]*$/u;
const TASK_ID = /^Q\d{2}$/u;

const FIELD_TYPES: readonly ScaffoldFieldTypeV1[] = [
  "hash32",
  "midgard_inputs_hash",
  "int",
  "bytes",
];

const asObject = (value: unknown, path: string): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    reject("not_an_object", path);
  }
  return value as Record<string, unknown>;
};

const requireExactKeys = (
  record: Record<string, unknown>,
  keys: readonly string[],
  path: string,
): void => {
  for (const key of keys) {
    if (!(key in record)) {
      reject("missing_key", `${path}.${key}`);
    }
  }
  for (const key of Object.keys(record)) {
    if (!keys.includes(key)) {
      reject("unknown_key", `${path}.${key}`);
    }
  }
};

const requireText = (value: unknown, path: string): string => {
  if (typeof value !== "string" || value.trim().length === 0) {
    reject("invalid_text", path);
  }
  return (value as string).trim();
};

const parseField = (value: unknown, path: string): ScaffoldFieldV1 => {
  const record = asObject(value, path);
  requireExactKeys(record, ["name", "type", "doc"], path);
  const name = requireText(record.name, `${path}.name`);
  if (!SNAKE.test(name)) {
    reject("invalid_identifier", `${path}.name`);
  }
  const type = record.type;
  if (
    typeof type !== "string" ||
    !FIELD_TYPES.includes(type as ScaffoldFieldTypeV1)
  ) {
    reject("unknown_field_type", `${path}.type`);
  }
  return {
    name,
    type: type as ScaffoldFieldTypeV1,
    doc: requireText(record.doc, `${path}.doc`),
  };
};

const parseFields = (
  value: unknown,
  path: string,
): readonly ScaffoldFieldV1[] | null => {
  if (value === null) {
    return null;
  }
  if (!Array.isArray(value)) {
    reject("invalid_state_shape", path);
  }
  const fields = (value as readonly unknown[]).map((entry, index) =>
    parseField(entry, `${path}[${index.toString()}]`),
  );
  const names = new Set<string>();
  for (const field of fields) {
    if (names.has(field.name)) {
      reject("duplicate_field", `${path}.${field.name}`);
    }
    names.add(field.name);
  }
  return fields;
};

const parseStep = (
  value: unknown,
  position: number,
  stepCount: number,
): ScaffoldStepV1 => {
  const path = `steps[${position.toString()}]`;
  const record = asObject(value, path);
  requireExactKeys(
    record,
    ["index", "rule", "inputState", "outputState", "argsFields"],
    path,
  );
  const index = record.index;
  if (index !== position + 1) {
    reject("invalid_step_sequence", `${path}.index`);
  }
  const isFirst = position === 0;
  const isTerminal = position === stepCount - 1;
  const inputState = parseFields(record.inputState, `${path}.inputState`);
  const outputState = parseFields(record.outputState, `${path}.outputState`);
  const argsFields = parseFields(record.argsFields, `${path}.argsFields`);

  // Step 1 carries no input state and consumes `NativeTxInclusionArgs`; every
  // later step must declare the exact state it consumes; only the terminal step
  // may omit an output state.
  if (isFirst && inputState !== null) {
    reject("invalid_state_shape", `${path}.inputState`);
  }
  if (!isFirst && (inputState === null || inputState.length === 0)) {
    reject("invalid_state_shape", `${path}.inputState`);
  }
  if (isFirst && argsFields !== null) {
    reject("invalid_state_shape", `${path}.argsFields`);
  }
  if (!isFirst && argsFields === null) {
    reject("invalid_state_shape", `${path}.argsFields`);
  }
  if (isTerminal && outputState !== null) {
    reject("invalid_state_shape", `${path}.outputState`);
  }
  if (!isTerminal && (outputState === null || outputState.length === 0)) {
    reject("invalid_state_shape", `${path}.outputState`);
  }
  return {
    index: index as number,
    rule: requireText(record.rule, `${path}.rule`),
    inputState,
    outputState,
    argsFields,
  };
};

const parseTestClass = (
  record: Record<string, unknown>,
  testClass: ScaffoldTestClassV1,
  seen: Set<string>,
): readonly string[] => {
  const raw = record[testClass];
  if (!Array.isArray(raw) || raw.length === 0) {
    reject("empty_test_class", `tests.${testClass}`);
  }
  return (raw as readonly unknown[]).map((entry, index) => {
    const name = requireText(entry, `tests.${testClass}[${index.toString()}]`);
    if (!SNAKE.test(name)) {
      reject("invalid_identifier", `tests.${testClass}[${index.toString()}]`);
    }
    if (seen.has(name)) {
      reject("duplicate_test_name", `tests.${testClass}.${name}`);
    }
    seen.add(name);
    return name;
  });
};

const parseTests = (value: unknown): ScaffoldTestsV1 => {
  const record = asObject(value, "tests");
  requireExactKeys(record, [...SCAFFOLD_TEST_CLASSES_V1], "tests");
  const seen = new Set<string>();
  // Written out one class at a time so the four mandatory §9.1/§3-invariant-9
  // test classes stay a compile-time record rather than a widened index type.
  return {
    positive: parseTestClass(record, "positive", seen),
    validBlockNegative: parseTestClass(record, "validBlockNegative", seen),
    mutation: parseTestClass(record, "mutation", seen),
    maximumFit: parseTestClass(record, "maximumFit", seen),
  };
};

/**
 * Strict spec parser. Every rejection is a typed code, so a family task can
 * distinguish "spec is wrong" from "generation failed".
 */
export const parseFraudProofFamilyScaffoldSpecV1 = (
  value: unknown,
): FraudProofFamilyScaffoldSpecV1 => {
  const record = asObject(value, "spec");
  requireExactKeys(
    record,
    [
      "schemaVersion",
      "family",
      "taskId",
      "violationId",
      "catalogueCategory",
      "summary",
      "steps",
      "tests",
    ],
    "spec",
  );
  if (
    record.schemaVersion !== FRAUD_PROOF_FAMILY_SCAFFOLD_SPEC_V1_SCHEMA_VERSION
  ) {
    reject("invalid_schema_version", "spec.schemaVersion");
  }
  const family = requireText(record.family, "spec.family");
  if (!KEBAB.test(family)) {
    reject("invalid_family_name", "spec.family");
  }
  const taskId = requireText(record.taskId, "spec.taskId");
  if (!TASK_ID.test(taskId)) {
    reject("invalid_task_id", "spec.taskId");
  }
  const catalogueCategory = requireText(
    record.catalogueCategory,
    "spec.catalogueCategory",
  );
  if (!CAMEL.test(catalogueCategory)) {
    reject("invalid_identifier", "spec.catalogueCategory");
  }
  const rawSteps = record.steps;
  if (!Array.isArray(rawSteps) || rawSteps.length < 2) {
    reject("invalid_step_sequence", "spec.steps");
  }
  const steps = (rawSteps as readonly unknown[]).map((step, position) =>
    parseStep(step, position, (rawSteps as readonly unknown[]).length),
  );
  return {
    schemaVersion: FRAUD_PROOF_FAMILY_SCAFFOLD_SPEC_V1_SCHEMA_VERSION,
    family,
    taskId,
    violationId: requireText(record.violationId, "spec.violationId"),
    catalogueCategory,
    summary: requireText(record.summary, "spec.summary"),
    steps,
    tests: parseTests(record.tests),
  };
};

export type FamilyScaffoldNamesV1 = {
  readonly family: string;
  /** `zero-input` -> `zero_input` */
  readonly aikenModule: string;
  /** `zero-input` -> `ZeroInput` */
  readonly pascal: string;
  /** `zero-input` -> `zeroInput` */
  readonly camel: string;
  /** `zero-input` -> `ZERO_INPUT` */
  readonly screamingSnake: string;
};

export const familyScaffoldNamesV1 = (
  family: string,
): FamilyScaffoldNamesV1 => {
  const parts = family.split("-");
  const pascal = parts
    .map((part) => `${part.charAt(0).toUpperCase()}${part.slice(1)}`)
    .join("");
  return {
    family,
    aikenModule: parts.join("_"),
    pascal,
    camel: `${pascal.charAt(0).toLowerCase()}${pascal.slice(1)}`,
    screamingSnake: parts.join("_").toUpperCase(),
  };
};

export const stepModuleNameV1 = (index: number): string =>
  `step_${index.toString().padStart(2, "0")}`;

export const stepFileNameV1 = (index: number): string =>
  `step-${index.toString().padStart(2, "0")}`;

export const stepPascalNameV1 = (index: number): string =>
  `Step${index.toString().padStart(2, "0")}`;
