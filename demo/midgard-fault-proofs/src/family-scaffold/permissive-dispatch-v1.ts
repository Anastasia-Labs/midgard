/**
 * Permissive-dispatch scanner for generated fraud-proof family scaffolds
 * (Goal task `Q02`).
 *
 * `GOAL_SPEC.md` §9.2 acceptance:
 *
 * > Shared generator may create boilerplate only; generated families retain
 * > explicit schemas/tests and no dynamic "accept any" dispatch.
 *
 * The anti-goal is a generator whose output accepts anything it did not
 * explicitly reject. This module is the executable form of that constraint: it
 * scans emitted artifact text for constructs that accept-by-default, and the
 * generator refuses to return a scaffold whose own output trips any of them.
 *
 * Every rule is a *default-accept* construct. Explicit checks, explicit
 * rejections, and phantom type parameters that carry no acceptance decision
 * (`ct.StepDatum<Data>` for a step with no input state, and its off-chain twin
 * `faultProofStepDatumSchema(Data.Any())`) are deliberately not flagged: they
 * are the shape the deployed families already use, and treating them as
 * findings would train reviewers to ignore the scanner.
 *
 * The calibration is checked, not asserted: the suite scans every shipped
 * fraud-proof artifact under `onchain/aiken` and `demo/midgard-sdk/src/
 * fraud-proof` and requires zero findings, while a table-driven control
 * requires every rule to fire on a sample of the construct it names. So the
 * scanner is neither vacuous nor noisy.
 *
 * Three constructs are default-accept in shape but not in effect, and each is
 * excluded by a guard narrow enough to name it rather than by a loosened
 * pattern (owner-adjudicated 2026-08-18): a wildcard arm whose block opens with
 * a refusing `expect`, an indexed lookup whose fallback is a quoted string
 * bound to a `*Name`-shaped diagnostic key, and one whose fallback is zero
 * under an equality test against a non-zero quantity. Each guard is paired
 * with hostile negative controls in the suite: an unguarded arm, an arm whose
 * `expect` is a binding or follows other statements, a fallback that feeds a
 * verdict or a handler, and a default that widens acceptance all still fire.
 *
 * Known bound: the rules are line-based, not parsed. `ak_opaque_data_parameter`
 * therefore accepts `List<Data>`/`Option<Data>`-style opaque carriers, which are
 * decoded explicitly by their callers. A genuinely opaque canonical field —
 * `native-tx/types.ak`'s `mint: Option<Data>` is the current example — is a
 * codec-hardening question for the owning family, not a scaffold dispatch
 * finding, and is out of this module's scope.
 */
export type ScaffoldArtifactLanguageV1 = "aiken" | "typescript" | "json";

export type PermissiveDispatchCategoryV1 =
  | "accept_any_dispatch"
  | "silent_fallback"
  | "permissive_schema"
  | "unenforced_test";

export type PermissiveDispatchFindingV1 = {
  readonly ruleId: string;
  readonly category: PermissiveDispatchCategoryV1;
  readonly path: string;
  readonly line: number;
  readonly excerpt: string;
  readonly explanation: string;
};

export type ScaffoldArtifactV1 = {
  readonly path: string;
  readonly language: ScaffoldArtifactLanguageV1;
  readonly contents: string;
};

type Rule = {
  readonly ruleId: string;
  readonly category: PermissiveDispatchCategoryV1;
  readonly languages: readonly ScaffoldArtifactLanguageV1[];
  readonly explanation: string;
  /** Matches a single logical line (comments already stripped). */
  readonly pattern: RegExp;
  /**
   * Optional guard applied to the match. `before` and `after` are the artifact
   * text immediately preceding and following the match, across line
   * boundaries, so a construct that a formatter wrapped onto its own line is
   * still judged in context. `after` is what lets a guard read the statement a
   * match introduces — an arm's body, or the fallback value of a `??`.
   */
  readonly accept?: (before: string, after: string) => boolean;
};

const AIKEN = ["aiken"] as const;
const TYPESCRIPT = ["typescript"] as const;
const BOTH = ["aiken", "typescript"] as const;

/**
 * `Data.Any()` and `<Data>` are permitted only as the phantom state parameter
 * of a step datum. Anywhere else they widen a real schema.
 */
const isStepDatumPhantom = (before: string): boolean =>
  /faultProofStepDatumSchema\(\s*$/u.test(before) ||
  /StepDatum$/u.test(before) ||
  /StepDatum<\s*$/u.test(before) ||
  // The off-chain twin of the on-chain `data: Option<Data>` thread slot, as
  // declared once in `fraud-proof/computation-threads.ts`.
  /\bdata:\s*Data\.Nullable\(\s*$/u.test(before);

/**
 * `List<Data>`, `Option<Data>`, `Pairs<Data>` and friends are opaque *carriers*
 * whose contents the surrounding code decodes explicitly; they are not a widened
 * acceptance decision. Only a bare `<Data>` type parameter on a family's own
 * type is.
 */
const isOpaqueDataContainer = (before: string): boolean =>
  /\b(List|Option|Pairs|Dict|Set)\s*$/u.test(before);

const isPermittedOpaqueData = (before: string): boolean =>
  isStepDatumPhantom(before) || isOpaqueDataContainer(before);

/**
 * A wildcard arm is not a default-accept when it opens a block whose *first*
 * statement is a refusing `expect`: the arm then rejects every case that
 * assertion does not admit, which is the enumerate-and-refuse shape the rule
 * exists to require. `native-binding-fixture-v1.ak`'s field-8 arm is the
 * shipped example — fields 6 and 7 are explicit arms and `_ -> { expect
 * field_index == 8 ... }` refuses everything that is not 8.
 *
 * Two conditions carry the narrowing, and both are load-bearing. The `expect`
 * must assert a *condition*, not bind a pattern: `expect x = decode(d)`
 * destructures without refusing anything and `expect True` refuses nothing at
 * all, so a comparison operator is required. And it must be the arm's *first*
 * statement — an `expect` reached only after other work does not gate the arm.
 */
const isRefusingCatchAllArm = (_before: string, after: string): boolean =>
  /^[^\S\n]*\{\s*expect[^\S\n]+[^\n=<>!]*(?:==|!=|>=|<=|>|<)[^\n]*\n/u.test(
    after,
  );

/**
 * `NAMES[verdict] ?? "unknown"` supplies a *diagnostic label*, never a
 * decision: the fallback is a quoted string and the value lands in a
 * `*Name`-shaped key that only ever reaches a human reading evidence. Both
 * halves are required, so a fallback that feeds a verdict, a handler, or any
 * other dispatch decision keeps firing — either its target is not
 * label-shaped (`verdict: VERDICTS[k] ?? "accept"`) or its fallback is not a
 * literal string (`registry[k] ?? fallbackHandler`).
 */
const isLabelOnlyFallback = (before: string, after: string): boolean =>
  /\b[A-Za-z_$][\w$]*(?:Name|Label|Description)\s*:\s*$/u.test(before) &&
  /^\s*"[^"\n]*"\s*[,;)\]]/u.test(after);

/**
 * `(utxo.assets[unit] ?? 0n) === 1n` defaults an absent quantity to zero and
 * then requires a non-zero one, so an absent value can only ever *fail* the
 * test. That is the conservative direction — the opposite of a default-accept.
 * A widening default (`?? 1n`, `?? true`) or a comparison the default itself
 * satisfies (`?? 0n) === 0n`) keeps firing.
 */
const isConservativeZeroDefault = (_before: string, after: string): boolean => {
  const match = /^\s*0n?\s*\)\s*===\s*(\d+)n?\b/u.exec(after);
  return match !== null && /[1-9]/u.test(match[1] ?? "");
};

const isNonDispatchingIndexedFallback = (
  before: string,
  after: string,
): boolean =>
  isLabelOnlyFallback(before, after) ||
  isConservativeZeroDefault(before, after);

export const PERMISSIVE_DISPATCH_RULES_V1: readonly Rule[] = [
  {
    ruleId: "ak_catch_all_arm_true",
    category: "accept_any_dispatch",
    languages: AIKEN,
    explanation:
      "a wildcard match arm that returns True accepts every unenumerated case",
    pattern: /(^|\s)_\s*(\w+\s*)?->\s*True\s*$/u,
  },
  {
    ruleId: "ak_catch_all_arm",
    category: "accept_any_dispatch",
    languages: AIKEN,
    explanation:
      "a wildcard match arm hides unenumerated cases; enumerate every constructor and `fail`, `False`, or `None` on the rest",
    pattern: /(^|\s)_\s*(\w+\s*)?->(?!\s*(fail|False|None)\b)/u,
    accept: isRefusingCatchAllArm,
  },
  {
    ruleId: "ak_always_true_predicate",
    category: "accept_any_dispatch",
    languages: AIKEN,
    explanation: "a predicate whose entire body is True validates nothing",
    pattern: /->\s*Bool\s*\{\s*True\s*\}/u,
  },
  {
    ruleId: "ak_or_else_true",
    category: "silent_fallback",
    languages: AIKEN,
    explanation:
      "an `or_else(True)` fallback turns a missing value into acceptance",
    pattern: /or_else\(\s*True\s*\)/u,
  },
  {
    ruleId: "ak_expect_wildcard",
    category: "accept_any_dispatch",
    languages: AIKEN,
    explanation:
      "`expect _ = ...` discards the shape check it appears to perform",
    pattern: /expect\s+_\s*=/u,
  },
  {
    ruleId: "ak_opaque_data_parameter",
    category: "permissive_schema",
    languages: AIKEN,
    explanation:
      "an opaque `Data` type parameter outside the step-datum phantom slot accepts any datum shape",
    pattern: /<\s*Data\s*>/u,
    accept: isPermittedOpaqueData,
  },
  {
    ruleId: "ak_vacuous_test",
    category: "unenforced_test",
    languages: AIKEN,
    explanation: "a test whose body is the literal True asserts nothing",
    pattern: /^\s*test\s+\w+\(\)\s*\{\s*True\s*\}/u,
  },
  {
    ruleId: "ts_default_case",
    category: "accept_any_dispatch",
    languages: TYPESCRIPT,
    explanation:
      "a switch `default:` branch handles families the generator never declared; use an exhaustive union with a never check",
    pattern: /^\s*default\s*:/u,
  },
  {
    ruleId: "ts_catch_accepts",
    category: "silent_fallback",
    languages: TYPESCRIPT,
    explanation: "a catch block that returns true swallows a real rejection",
    pattern: /catch[\s\S]{0,80}?return\s+true\s*;/u,
  },
  {
    ruleId: "ts_nullish_true",
    category: "silent_fallback",
    languages: TYPESCRIPT,
    explanation: "`?? true` turns an absent value into acceptance",
    pattern: /\?\?\s*true\b/u,
  },
  {
    ruleId: "ts_dynamic_registry_dispatch",
    category: "accept_any_dispatch",
    languages: TYPESCRIPT,
    explanation:
      "an indexed registry lookup with a fallback dispatches to an unenumerated handler",
    pattern: /\w+\[[^\]]+\]\s*\?\?/u,
    accept: isNonDispatchingIndexedFallback,
  },
  {
    ruleId: "ts_any_type",
    category: "permissive_schema",
    languages: TYPESCRIPT,
    explanation: "`any` erases the explicit schema the family must retain",
    pattern: /(:\s*any\b|as\s+any\b|<any>)/u,
  },
  {
    ruleId: "ts_data_any",
    category: "permissive_schema",
    languages: TYPESCRIPT,
    explanation:
      "`Data.Any()` outside the step-datum phantom slot accepts any Plutus datum",
    pattern: /Data\.Any\(\)/u,
    accept: isStepDatumPhantom,
  },
  {
    ruleId: "ts_skipped_test",
    category: "unenforced_test",
    languages: TYPESCRIPT,
    explanation:
      "a skipped test lets an unimplemented family report a green suite",
    pattern: /\b(it|test|describe)\.skip\s*\(/u,
  },
  {
    ruleId: "ts_always_true_predicate",
    category: "accept_any_dispatch",
    languages: BOTH,
    explanation:
      "an arrow function that returns the literal true validates nothing",
    pattern: /=>\s*true\s*;/u,
  },
];

const stripComments = (
  line: string,
  language: ScaffoldArtifactLanguageV1,
): string => {
  if (language === "aiken") {
    return line.replace(/\/\/.*$/u, "");
  }
  if (language === "typescript") {
    return line.replace(/\/\/.*$/u, "").replace(/^\s*\*.*$/u, "");
  }
  return line;
};

/**
 * Scans one emitted artifact. Comments are stripped first so documentation may
 * name a forbidden construct without tripping its own rule.
 */
export const scanForPermissiveDispatchV1 = (
  artifact: ScaffoldArtifactV1,
): readonly PermissiveDispatchFindingV1[] => {
  if (artifact.language === "json") {
    return [];
  }
  const findings: PermissiveDispatchFindingV1[] = [];
  const lines = artifact.contents.split("\n");
  // Comment stripping only removes a trailing comment or blanks a whole line,
  // so an index inside a stripped line is still the index inside its raw line.
  let lineStart = 0;
  for (const [index, rawLine] of lines.entries()) {
    const offset = lineStart;
    lineStart += rawLine.length + 1;
    const line = stripComments(rawLine, artifact.language);
    if (line.trim().length === 0) {
      continue;
    }
    for (const rule of PERMISSIVE_DISPATCH_RULES_V1) {
      if (!rule.languages.includes(artifact.language)) {
        continue;
      }
      const pattern = new RegExp(rule.pattern.source, rule.pattern.flags);
      const match = pattern.exec(line);
      if (match === null) {
        continue;
      }
      const globalIndex = offset + match.index;
      const before = artifact.contents.slice(
        Math.max(0, globalIndex - 96),
        globalIndex,
      );
      const matchEnd = globalIndex + match[0].length;
      const after = artifact.contents.slice(matchEnd, matchEnd + 160);
      if (rule.accept?.(before, after) === true) {
        continue;
      }
      findings.push({
        ruleId: rule.ruleId,
        category: rule.category,
        path: artifact.path,
        line: index + 1,
        excerpt: rawLine.trim().slice(0, 160),
        explanation: rule.explanation,
      });
    }
  }
  return findings;
};

export class PermissiveDispatchRejectionV1 extends Error {
  readonly findings: readonly PermissiveDispatchFindingV1[];

  constructor(findings: readonly PermissiveDispatchFindingV1[]) {
    super(
      `Refusing to emit a family scaffold with permissive dispatch:\n${findings
        .map(
          (finding) =>
            `  ${finding.path}:${finding.line.toString()} ${finding.ruleId} (${
              finding.category
            }) — ${finding.explanation}: ${finding.excerpt}`,
        )
        .join("\n")}`,
    );
    this.name = "PermissiveDispatchRejectionV1";
    this.findings = findings;
  }
}

/** Fail-closed gate the generator applies to its own output. */
export const assertNoPermissiveDispatchV1 = (
  artifacts: readonly ScaffoldArtifactV1[],
): void => {
  const findings = artifacts.flatMap((artifact) =>
    scanForPermissiveDispatchV1(artifact),
  );
  if (findings.length > 0) {
    throw new PermissiveDispatchRejectionV1(findings);
  }
};
