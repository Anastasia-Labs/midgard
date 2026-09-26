/**
 * Holds the validator scenario registry to the blueprint and the family
 * registry it describes. The blueprint is the one the package's global setup
 * already refused when stale; an absent blueprint fails here rather than
 * passing on an empty validator set.
 */
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { FAMILY_APPLICATION_REGISTRY } from "../src/workflow/family-application-registry.js";
import {
  FAMILY_SCENARIOS,
  UNMAPPED_FAMILIES,
  UNMAPPED_FAMILY_COUNT,
  UNMAPPED_VALIDATOR_COUNT,
  UNMAPPED_VALIDATORS,
  VALIDATOR_SCENARIOS,
  type ValidatorScenario,
  type ValidatorScenarioPair,
} from "./support/validator-scenario-registry.js";

const REPOSITORY_ROOT = fileURLToPath(new URL("../../../", import.meta.url));
const BLUEPRINT_PATH = join(REPOSITORY_ROOT, "onchain/aiken/plutus.json");

/** `<module>.<validator>`: a blueprint title without its handler suffix. */
const blueprintValidators = (): string[] => {
  if (!existsSync(BLUEPRINT_PATH)) {
    throw new Error(
      `${BLUEPRINT_PATH} is absent; build it with \`pnpm --dir demo deployment:build preprod-testing\``,
    );
  }
  const blueprint = JSON.parse(readFileSync(BLUEPRINT_PATH, "utf8")) as {
    readonly validators: readonly { readonly title: string }[];
  };
  return [
    ...new Set(
      blueprint.validators.map((validator) =>
        validator.title.split(".").slice(0, 2).join("."),
      ),
    ),
  ].sort();
};

const readQuoted = (text: string, start: number): string | undefined => {
  const quote = text[start];
  if (quote === undefined || !`"'\``.includes(quote)) return undefined;
  let value = "";
  for (let index = start + 1; index < text.length; index += 1) {
    const character = text[index];
    if (character === quote) return value;
    if (character === "\\") {
      value += text[index + 1] ?? "";
      index += 1;
    } else {
      value += character;
    }
  }
  return undefined;
};

/**
 * The titles of the runnable tests a file declares: `it`/`test` calls and
 * their `.each(table)(title)` forms. A skipped or todo test is not a
 * scenario, so those are left out.
 */
const declaredTestTitles = (text: string): Set<string> => {
  const titles = new Set<string>();
  const call =
    /\b(?:it|test)((?:\.(?:skip|only|concurrent|sequential|fails|todo))*)(\.each)?\(/gu;
  for (const match of text.matchAll(call)) {
    let index = match.index + match[0].length;
    if (match[2] !== undefined) {
      let depth = 1;
      while (index < text.length && depth > 0) {
        const character = text[index];
        if (character === "(") depth += 1;
        else if (character === ")") depth -= 1;
        else if (character !== undefined && `"'\``.includes(character)) {
          index += (readQuoted(text, index)?.length ?? 0) + 1;
        }
        index += 1;
      }
      if (text[index] !== "(") continue;
      index += 1;
    }
    while (/\s/u.test(text[index] ?? "")) index += 1;
    const title = readQuoted(text, index);
    if (title !== undefined && !/\.(?:skip|todo)\b/u.test(match[1] ?? "")) {
      titles.add(title);
    }
  }
  return titles;
};

const titlesByFile = new Map<string, Set<string> | undefined>();
const titlesIn = (file: string): Set<string> | undefined => {
  if (!titlesByFile.has(file)) {
    const path = join(REPOSITORY_ROOT, file);
    titlesByFile.set(
      file,
      existsSync(path)
        ? declaredTestTitles(readFileSync(path, "utf8"))
        : undefined,
    );
  }
  return titlesByFile.get(file);
};

const missingScenario = (scenario: ValidatorScenario): string | undefined => {
  const titles = titlesIn(scenario.file);
  if (titles === undefined) return `${scenario.file} does not exist`;
  return titles.has(scenario.test)
    ? undefined
    : `${scenario.file} declares no runnable test "${scenario.test}"`;
};

const pairProblems = (key: string, pair: ValidatorScenarioPair): string[] => [
  ...(pair.passing.length === 0 ? [`${key}: no passing scenario`] : []),
  ...(pair.failing.length === 0 ? [`${key}: no failing scenario`] : []),
  ...[...pair.passing, ...pair.failing].flatMap((scenario) => {
    const problem = missingScenario(scenario);
    return problem === undefined ? [] : [`${key}: ${problem}`];
  }),
];

/** Keys listed more than once across the mapped and unmapped sets. */
const duplicates = (keys: readonly string[]): string[] =>
  keys.filter((key, index) => keys.indexOf(key) !== index);

describe("validator scenario registry", () => {
  const unmappedValidators = UNMAPPED_VALIDATORS.flatMap(
    (group) => group.validators,
  );
  const unmappedFamilies: string[] = UNMAPPED_FAMILIES.flatMap(
    (group) => group.families,
  );

  it("reads the test titles it checks against, including table tests", () => {
    // Guards the parser: if it stopped seeing titles, every reference would
    // be reported missing and the failure would blame the registry.
    const titles = declaredTestTitles(
      [
        'it("plain", () => {});',
        "it.each([[1], [2]])(\n  `table %s`,\n  () => {},\n);",
        'it.skip("skipped", () => {});',
        'it.todo("todo");',
        'test.concurrent("concurrent", () => {});',
      ].join("\n"),
    );
    expect([...titles].sort()).toEqual(["concurrent", "plain", "table %s"]);
  });

  it("covers every validator in the blueprint, mapped or listed with a reason", () => {
    const validators = blueprintValidators();
    expect(validators.length).toBeGreaterThan(0);
    const known = [...Object.keys(VALIDATOR_SCENARIOS), ...unmappedValidators];
    expect(duplicates(known)).toEqual([]);
    expect(validators.filter((name) => !known.includes(name))).toEqual([]);
    expect(known.filter((name) => !validators.includes(name))).toEqual([]);
  });

  it("covers every family in FAMILY_APPLICATION_REGISTRY, mapped or listed with a reason", () => {
    const families = Object.keys(FAMILY_APPLICATION_REGISTRY).sort();
    const known = [...Object.keys(FAMILY_SCENARIOS), ...unmappedFamilies];
    expect(duplicates(known)).toEqual([]);
    expect(families.filter((name) => !known.includes(name))).toEqual([]);
    expect(known.filter((name) => !families.includes(name))).toEqual([]);
  });

  it("names a passing and a failing scenario that exist for every mapping", () => {
    const problems = [
      ...Object.entries(VALIDATOR_SCENARIOS),
      ...Object.entries(FAMILY_SCENARIOS),
    ].flatMap(([key, pair]) =>
      pair === undefined ? [`${key}: no entry`] : pairProblems(key, pair),
    );
    expect(problems).toEqual([]);
  });

  it("gives every unmapped group a reason and keeps the gap from growing", () => {
    expect(
      [...UNMAPPED_VALIDATORS, ...UNMAPPED_FAMILIES].filter(
        (group) => group.reason.trim() === "",
      ),
    ).toEqual([]);
    // Mapping a validator removes it from the list and lowers the count.
    // Adding one to the list means raising the count in the same change.
    expect({
      validators: unmappedValidators.length,
      families: unmappedFamilies.length,
    }).toEqual({
      validators: UNMAPPED_VALIDATOR_COUNT,
      families: UNMAPPED_FAMILY_COUNT,
    });
  });
});
