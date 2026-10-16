import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { CoreStepEvidenceSchema } from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import { encodeMidgardCekCoreStepDataCbor } from "../src/cek-data.js";
import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionStep,
} from "../src/cek-executor.js";
import {
  buildMidgardCanonicalCekProgram,
  encodeMidgardCekCardanoFlatProgram,
} from "../src/cek-program.js";
import * as vectors from "./fixtures/cek-core-step-v1.vectors.mjs";

/**
 * The SDK's `CoreStepEvidenceSchema` against the core-step evidence the
 * TypeScript machine emits, over every step of the V1 core-step golden
 * vectors.
 *
 * A byte round trip through the schema catches a field of the wrong type or
 * arity, but not two fields of the same type in swapped places. So each
 * decoded step is also compared field by field, by name, with the witness the
 * TypeScript machine produced for it: the schema's snake_case names against
 * the witness's camelCase ones.
 */

type GoldenProgram = {
  readonly label: string;
  readonly steps: readonly {
    readonly kind: string;
    readonly evidenceCbor: string;
  }[];
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL("./fixtures/cek-core-step-v1.generated.json", import.meta.url),
    ),
    "utf8",
  ),
) as { readonly programs: readonly GoldenProgram[] };

const hex = (value: Uint8Array): string => Buffer.from(value).toString("hex");

/** The schema field names the TypeScript witness spells differently. */
const TS_FIELD_NAMES: Readonly<Record<string, string>> = {
  left: "leftExpression",
  right: "rightExpression",
};

const camelCase = (name: string): string =>
  TS_FIELD_NAMES[name] ??
  name.replace(/_([a-z0-9])/g, (_match, letter: string) =>
    letter.toUpperCase(),
  );

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

/**
 * The integers and byte strings a value holds, sorted. Field names, variant
 * names and `kind` tags are left out, so a value the schema read and the
 * TypeScript value it was encoded from give the same list.
 */
const leaves = (value: unknown): string[] => {
  const walk = (item: unknown): string[] => {
    if (typeof item === "bigint") {
      return [`i${item}`];
    }
    if (item instanceof Uint8Array) {
      return [`b${hex(item)}`];
    }
    if (typeof item === "string") {
      // A schema variant without fields decodes to its name.
      return /^([0-9a-f]{2})*$/.test(item) ? [`b${item}`] : [];
    }
    if (item === null) {
      return [];
    }
    if (Array.isArray(item)) {
      return item.flatMap(walk);
    }
    if (item instanceof Constr) {
      return (item.fields as unknown[]).flatMap(walk);
    }
    if (isRecord(item)) {
      return Object.entries(item)
        .filter(([key]) => key !== "kind")
        .flatMap(([, field]) => walk(field));
    }
    throw new Error(`unexpected witness value ${String(item)}`);
  };
  return walk(value).sort();
};

/**
 * Asserts that `decoded`, a witness the schema read, names the same fields as
 * `witness`, the TypeScript witness it was encoded from: the same constructor,
 * the same field names, and under each name the same value (compared exactly
 * for an integer or byte string, by its integers and byte strings otherwise).
 */
const expectSameNamedFields = (
  decoded: unknown,
  witness: Record<string, unknown>,
  path: string,
): void => {
  const { kind, ...tsFields } = witness;
  const [variant, schemaFields] =
    typeof decoded === "string"
      ? [decoded, {}]
      : (Object.entries(decoded as Record<string, unknown>)[0] ?? ["", {}]);
  expect(
    typeof kind === "string" && variant.toLowerCase() === kind.toLowerCase(),
    `${path}: ${variant} vs ${String(kind)}`,
  ).toBe(true);
  expect(isRecord(schemaFields), path).toBe(true);
  const fields = schemaFields as Record<string, unknown>;
  const names = Object.keys(fields);
  expect(Object.keys(tsFields).sort(), path).toEqual(
    names.map(camelCase).sort(),
  );
  for (const name of names) {
    const schemaValue = fields[name];
    const tsValue = tsFields[camelCase(name)];
    const fieldPath = `${path}.${variant}.${name}`;
    if (typeof schemaValue === "bigint") {
      expect(tsValue, fieldPath).toBe(schemaValue);
    } else if (tsValue instanceof Uint8Array) {
      expect(hex(tsValue), fieldPath).toBe(schemaValue);
    } else {
      expect(leaves(schemaValue), fieldPath).toEqual(leaves(tsValue));
    }
  }
};

const execute = (
  definition: vectors.CekCoreStepProgramV1,
): readonly MidgardCekExecutionStep[] => {
  const program = buildMidgardCanonicalCekProgram(
    vectors.compileMidgardCekGoldenProgram(
      definition.term(),
      encodeMidgardCekCardanoFlatProgram,
    ),
  );
  const graph = buildMidgardCekExecutionGraph(
    program.envelope,
    program.material.values(),
    encodeMidgardCekPlutusData(definition.context()),
  );
  return executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: definition.maxSteps,
    executionBudget: definition.executionBudget,
  }).steps;
};

const witnessConstructor = (evidenceCbor: string): bigint => {
  const evidence = Data.from(evidenceCbor);
  if (!(evidence instanceof Constr)) {
    throw new Error("core-step evidence is not a constructor");
  }
  const witness: unknown = evidence.fields[2];
  if (!(witness instanceof Constr)) {
    throw new Error("core-step witness is not a constructor");
  }
  return BigInt(witness.index);
};

describe("SDK CoreStepEvidenceSchema against the core-step golden vectors", () => {
  const steps = golden.programs.flatMap((program) => program.steps);

  it("decodes and re-encodes every step byte-for-byte", () => {
    for (const step of steps) {
      expect(
        Data.to(
          Data.from(step.evidenceCbor, CoreStepEvidenceSchema),
          CoreStepEvidenceSchema,
        ),
        step.kind,
      ).toBe(step.evidenceCbor);
    }
  });

  it("covers all 42 witness constructors", () => {
    const constructors = new Set(
      steps.map((step) => witnessConstructor(step.evidenceCbor)),
    );
    expect([...constructors].sort((a, b) => Number(a - b))).toEqual(
      Array.from({ length: 42 }, (_, index) => BigInt(index)),
    );
  });

  it("names every witness field as the machine does, for all 42 constructors", () => {
    const checked = new Set<bigint>();
    let checkedSteps = 0;
    for (const program of golden.programs) {
      const definition = vectors.CEK_CORE_STEP_PROGRAMS.find(
        (candidate) => candidate.label === program.label,
      );
      expect(definition, program.label).toBeDefined();
      if (definition === undefined) {
        continue;
      }
      const executed = execute(definition);
      expect(executed.map((step) => step.witness.kind)).toEqual(
        program.steps.map((step) => step.kind),
      );
      executed.forEach((step, index) => {
        const evidenceCbor = program.steps[index]?.evidenceCbor ?? "";
        expect(hex(encodeMidgardCekCoreStepDataCbor(step))).toBe(evidenceCbor);
        expectSameNamedFields(
          Data.from(evidenceCbor, CoreStepEvidenceSchema).witness,
          step.witness as unknown as Record<string, unknown>,
          `${program.label}[${index}]`,
        );
        checked.add(witnessConstructor(evidenceCbor));
        checkedSteps += 1;
      });
    }
    expect(checkedSteps).toBe(steps.length);
    expect(checked.size).toBe(42);
  });
});
