/**
 * Family scaffold generator entry point (Goal task `Q02`).
 *
 * `GOAL_SPEC.md` §9.2 acceptance:
 *
 * > Shared generator may create boilerplate only; generated families retain
 * > explicit schemas/tests and no dynamic "accept any" dispatch.
 *
 * The generator is deliberately dumb: it turns an explicit, strictly parsed
 * family specification into skeleton files whose every schema is spelled out
 * and whose every rule check is `todo`. It then re-reads its own output through
 * four fail-closed gates before returning it:
 *
 * 1. `assertNoPermissiveDispatchV1` — no emitted line contains an accept-by-
 *    default construct (`permissive-dispatch-v1.ts`);
 * 2. `assertGeneratedTestsFailLoudV1` — no emitted test can report green before
 *    the owning family task implements it, and no `fail`-annotated Aiken test
 *    may sit in a module whose bodies are still `todo` (such a test passes
 *    vacuously);
 * 3. `assertDeclaredSurfaceIsExplicitV1` — every state field and every declared
 *    test name in the spec is present verbatim in the emitted artifacts, so the
 *    generator cannot quietly drop a schema or a required test class; and
 * 4. `assertClosureChecklistUnclaimedV1` — the emitted §9.1 checklist carries no
 *    claimed row.
 *
 * A generator that cannot pass its own gates emits nothing.
 */
import { existsSync } from "node:fs";
import { mkdir, writeFile } from "node:fs/promises";
import { dirname, isAbsolute, join, normalize, resolve, sep } from "node:path";

import {
  emitAikenStepTypesModuleV1,
  emitAikenStepValidatorV1,
  emitClosureChecklistV1,
  emitSdkFamilyModuleV1,
  emitSdkFamilyTestModuleV1,
  SCAFFOLD_CLOSURE_ROW_STATUS_V1,
} from "./emit-v1.js";
import {
  assertNoPermissiveDispatchV1,
  type ScaffoldArtifactV1,
} from "./permissive-dispatch-v1.js";
import {
  type FraudProofFamilyScaffoldSpecV1,
  parseFraudProofFamilyScaffoldSpecV1,
  SCAFFOLD_TEST_CLASSES_V1,
} from "./spec-v1.js";

export const FAMILY_SCAFFOLD_GENERATOR_V1_ID =
  "midgard-fraud-proof-family-scaffold-generator-v1" as const;

export type FamilyScaffoldPlanV1 = {
  readonly generator: typeof FAMILY_SCAFFOLD_GENERATOR_V1_ID;
  readonly spec: FraudProofFamilyScaffoldSpecV1;
  readonly artifacts: readonly ScaffoldArtifactV1[];
};

export type ScaffoldGuardCodeV1 =
  | "vacuous_fail_test"
  | "test_can_report_green"
  | "missing_declared_test"
  | "missing_declared_field"
  | "claimed_closure_row"
  | "unparsable_closure_checklist";

export class ScaffoldGuardRejectionV1 extends Error {
  readonly code: ScaffoldGuardCodeV1;
  readonly path: string;
  readonly detail: string;

  constructor(code: ScaffoldGuardCodeV1, path: string, detail: string) {
    super(`${code}: ${path} — ${detail}`);
    this.name = "ScaffoldGuardRejectionV1";
    this.code = code;
    this.path = path;
    this.detail = detail;
  }
}

const AIKEN_FAIL_TEST = /^\s*test\s+\w+\(\)\s*fail\b/u;
const AIKEN_TEST = /^\s*test\s+\w+\(\)/u;
const AIKEN_TODO = /todo\s+@"/u;
const TS_IT = /^\s*it\(/u;
const TS_UNIMPLEMENTED = /SCAFFOLD_UNIMPLEMENTED/u;

/**
 * Gate 2. An emitted test must be incapable of reporting green.
 *
 * Aiken: a `test … () fail { … }` whose body reaches `todo` *passes*, because
 * `todo` aborts and the annotation expects an abort. A generator that emitted
 * `fail` annotations onto unimplemented bodies would therefore ship a family
 * whose valid-block negative "passes" while asserting nothing — precisely the
 * accept-anything outcome §9.2 forbids. So no `fail` annotation may appear in a
 * module that still contains `todo`.
 *
 * TypeScript: every generated `it(` must throw `SCAFFOLD_UNIMPLEMENTED`.
 */
export const assertGeneratedTestsFailLoudV1 = (
  artifacts: readonly ScaffoldArtifactV1[],
): void => {
  for (const artifact of artifacts) {
    const lines = artifact.contents.split("\n");
    if (artifact.language === "aiken") {
      const hasTodo = AIKEN_TODO.test(artifact.contents);
      const hasTest = lines.some((line) => AIKEN_TEST.test(line));
      for (const [index, line] of lines.entries()) {
        if (hasTodo && AIKEN_FAIL_TEST.test(line)) {
          throw new ScaffoldGuardRejectionV1(
            "vacuous_fail_test",
            `${artifact.path}:${(index + 1).toString()}`,
            "a `fail` test in a module that still contains `todo` passes vacuously",
          );
        }
      }
      if (hasTest && !hasTodo) {
        throw new ScaffoldGuardRejectionV1(
          "test_can_report_green",
          artifact.path,
          "a generated Aiken test module must reach `todo` so it cannot report green before the family task implements it",
        );
      }
      continue;
    }
    if (artifact.language !== "typescript") {
      continue;
    }
    const itCount = lines.filter((line) => TS_IT.test(line)).length;
    if (itCount === 0) {
      continue;
    }
    const unimplementedCount = lines.filter((line) =>
      TS_UNIMPLEMENTED.test(line),
    ).length;
    if (unimplementedCount < itCount) {
      throw new ScaffoldGuardRejectionV1(
        "test_can_report_green",
        artifact.path,
        `${itCount.toString()} generated tests but only ${unimplementedCount.toString()} SCAFFOLD_UNIMPLEMENTED throws`,
      );
    }
  }
};

/**
 * Gate 3. Every schema field and every declared test name the spec names must
 * survive into the emitted artifacts verbatim. This is the "retain explicit
 * schemas/tests" half of the acceptance text, checked rather than asserted.
 */
export const assertDeclaredSurfaceIsExplicitV1 = ({
  spec,
  artifacts,
}: {
  readonly spec: FraudProofFamilyScaffoldSpecV1;
  readonly artifacts: readonly ScaffoldArtifactV1[];
}): void => {
  const aiken = artifacts.filter((artifact) => artifact.language === "aiken");
  const typescript = artifacts.filter(
    (artifact) => artifact.language === "typescript",
  );
  const aikenText = aiken.map((artifact) => artifact.contents).join("\n");
  const typescriptText = typescript
    .map((artifact) => artifact.contents)
    .join("\n");

  for (const step of spec.steps) {
    for (const field of [
      ...(step.inputState ?? []),
      ...(step.argsFields ?? []),
    ]) {
      if (!aikenText.includes(field.name)) {
        throw new ScaffoldGuardRejectionV1(
          "missing_declared_field",
          `steps[${(step.index - 1).toString()}].${field.name}`,
          "declared field is absent from the emitted Aiken schemas",
        );
      }
      if (!typescriptText.includes(field.name)) {
        throw new ScaffoldGuardRejectionV1(
          "missing_declared_field",
          `steps[${(step.index - 1).toString()}].${field.name}`,
          "declared field is absent from the emitted off-chain schemas",
        );
      }
    }
  }

  for (const testClass of SCAFFOLD_TEST_CLASSES_V1) {
    for (const name of spec.tests[testClass]) {
      if (!aikenText.includes(name)) {
        throw new ScaffoldGuardRejectionV1(
          "missing_declared_test",
          `tests.${testClass}.${name}`,
          "declared test name is absent from the emitted Aiken step modules",
        );
      }
    }
  }
};

/** Gate 4. A generated checklist may not pre-claim any §9.1 output. */
export const assertClosureChecklistUnclaimedV1 = (
  artifact: ScaffoldArtifactV1,
): void => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(artifact.contents);
  } catch {
    throw new ScaffoldGuardRejectionV1(
      "unparsable_closure_checklist",
      artifact.path,
      "emitted checklist is not valid JSON",
    );
  }
  const rows = (parsed as { closureOutputs?: unknown }).closureOutputs;
  if (!Array.isArray(rows) || rows.length === 0) {
    throw new ScaffoldGuardRejectionV1(
      "unparsable_closure_checklist",
      artifact.path,
      "emitted checklist has no closureOutputs rows",
    );
  }
  for (const row of rows as readonly { status?: unknown }[]) {
    if (row.status !== SCAFFOLD_CLOSURE_ROW_STATUS_V1) {
      throw new ScaffoldGuardRejectionV1(
        "claimed_closure_row",
        artifact.path,
        `row status must be ${SCAFFOLD_CLOSURE_ROW_STATUS_V1}, saw ${String(
          row.status,
        )}`,
      );
    }
  }
};

/**
 * Emitters are injectable so the gates can be exercised against deliberately
 * bad output. Production callers use the default set.
 */
export type ScaffoldEmittersV1 = {
  readonly emitAikenStepTypesModule: typeof emitAikenStepTypesModuleV1;
  readonly emitAikenStepValidator: typeof emitAikenStepValidatorV1;
  readonly emitSdkFamilyModule: typeof emitSdkFamilyModuleV1;
  readonly emitSdkFamilyTestModule: typeof emitSdkFamilyTestModuleV1;
  readonly emitClosureChecklist: typeof emitClosureChecklistV1;
};

export const FAMILY_SCAFFOLD_EMITTERS_V1: ScaffoldEmittersV1 = {
  emitAikenStepTypesModule: emitAikenStepTypesModuleV1,
  emitAikenStepValidator: emitAikenStepValidatorV1,
  emitSdkFamilyModule: emitSdkFamilyModuleV1,
  emitSdkFamilyTestModule: emitSdkFamilyTestModuleV1,
  emitClosureChecklist: emitClosureChecklistV1,
};

/**
 * Generates one family scaffold from an explicit specification.
 *
 * `spec` is `unknown` on purpose: the strict parser is part of the contract, so
 * a caller cannot bypass it by asserting a type.
 */
export const generateFraudProofFamilyScaffoldV1 = ({
  spec,
  emitters = FAMILY_SCAFFOLD_EMITTERS_V1,
}: {
  readonly spec: unknown;
  readonly emitters?: ScaffoldEmittersV1;
}): FamilyScaffoldPlanV1 => {
  const parsed = parseFraudProofFamilyScaffoldSpecV1(spec);
  const artifacts: ScaffoldArtifactV1[] = [];
  for (const step of parsed.steps) {
    artifacts.push(emitters.emitAikenStepTypesModule({ spec: parsed, step }));
    artifacts.push(emitters.emitAikenStepValidator({ spec: parsed, step }));
  }
  artifacts.push(emitters.emitSdkFamilyModule(parsed));
  artifacts.push(emitters.emitSdkFamilyTestModule(parsed));
  const checklist = emitters.emitClosureChecklist(parsed);
  artifacts.push(checklist);

  assertNoPermissiveDispatchV1(artifacts);
  assertGeneratedTestsFailLoudV1(artifacts);
  assertDeclaredSurfaceIsExplicitV1({ spec: parsed, artifacts });
  assertClosureChecklistUnclaimedV1(checklist);

  return {
    generator: FAMILY_SCAFFOLD_GENERATOR_V1_ID,
    spec: parsed,
    artifacts,
  };
};

export type ScaffoldWriteCodeV1 = "target_exists" | "path_escapes_root";

export class ScaffoldWriteRejectionV1 extends Error {
  readonly code: ScaffoldWriteCodeV1;
  readonly path: string;

  constructor(code: ScaffoldWriteCodeV1, path: string) {
    super(`${code}: ${path}`);
    this.name = "ScaffoldWriteRejectionV1";
    this.code = code;
    this.path = path;
  }
}

export type FamilyScaffoldWriteResultV1 = {
  readonly repoRoot: string;
  readonly dryRun: boolean;
  readonly written: readonly string[];
};

/**
 * Writes a plan to disk. Fail-closed twice: an artifact path that escapes the
 * repository root is rejected, and an existing target is *never* overwritten —
 * regenerating over a family a task has already implemented would silently
 * replace real proof logic with `todo`.
 */
export const writeFraudProofFamilyScaffoldV1 = async ({
  plan,
  repoRoot,
  dryRun = false,
}: {
  readonly plan: FamilyScaffoldPlanV1;
  readonly repoRoot: string;
  readonly dryRun?: boolean;
}): Promise<FamilyScaffoldWriteResultV1> => {
  const root = resolve(repoRoot);
  const targets = plan.artifacts.map((artifact) => {
    if (isAbsolute(artifact.path)) {
      throw new ScaffoldWriteRejectionV1("path_escapes_root", artifact.path);
    }
    const absolute = normalize(join(root, artifact.path));
    if (absolute !== root && !absolute.startsWith(`${root}${sep}`)) {
      throw new ScaffoldWriteRejectionV1("path_escapes_root", artifact.path);
    }
    return { artifact, absolute };
  });

  for (const target of targets) {
    if (existsSync(target.absolute)) {
      throw new ScaffoldWriteRejectionV1("target_exists", target.artifact.path);
    }
  }
  if (dryRun) {
    return {
      repoRoot: root,
      dryRun: true,
      written: targets.map((target) => target.artifact.path),
    };
  }
  for (const target of targets) {
    await mkdir(dirname(target.absolute), { recursive: true });
    await writeFile(target.absolute, target.artifact.contents, "utf8");
  }
  return {
    repoRoot: root,
    dryRun: false,
    written: targets.map((target) => target.artifact.path),
  };
};
