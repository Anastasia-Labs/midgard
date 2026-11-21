// Tests for the Midgard ESLint plugin (eslint-plugin-midgard/).
//
// Every rule must have fixtures with `// ruleid:` and `// ok:` cases, and must
// report exactly the lines its `ruleid` annotations point at: a rule without
// fixtures, or one that has stopped firing, fails here. The baseline ratchet
// is tested on a synthetic baseline, and the real baseline's shape (a reason
// on every entry, only real rules and files) is checked too.

import assert from "node:assert/strict";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { Linter } from "eslint";
import tseslint from "typescript-eslint";

import {
  BASELINE_PATH,
  baselineProblems,
  fingerprint,
  pruneBaseline,
} from "./eslint-plugin-midgard/baseline.mjs";
import midgard, { PLUGIN_NAME } from "./eslint-plugin-midgard/index.mjs";

const demoRoot = join(dirname(fileURLToPath(import.meta.url)), "../..");
const fixturesDirectory = join(
  dirname(fileURLToPath(import.meta.url)),
  "eslint-plugin-midgard/fixtures",
);
const ruleNames = Object.keys(midgard.rules).sort();
const ruleIds = ruleNames.map((name) => `${PLUGIN_NAME}/${name}`);

const ANNOTATION = /^\s*\/\/\s*(ruleid|ok):\s*(\S+)\s*$/u;
const FIXTURE_PATH = /^\s*\/\/\s*fixture-path:\s*(\S+)\s*$/u;

// A fixture is `<rule>.ts` or `<rule>.<variant>.ts`. Its first line may name
// the demo-relative path it is linted as (`// fixture-path: <path>`), for
// rules that only apply to some files.
const fixtures = readdirSync(fixturesDirectory)
  .filter((name) => name.endsWith(".ts"))
  .sort()
  .map((name) => {
    const source = readFileSync(join(fixturesDirectory, name), "utf8");
    const lines = source.split("\n");
    const path =
      FIXTURE_PATH.exec(lines[0])?.[1] ??
      `scripts/lib/eslint-plugin-midgard/fixtures/${name}`;
    const expected = { ruleid: [], ok: [] };
    const annotated = new Set();
    lines.forEach((line, index) => {
      const match = ANNOTATION.exec(line);
      if (match === null) return;
      annotated.add(match[2]);
      // The annotation names the line after it (1-based: index + 2).
      expected[match[1]].push(index + 2);
    });
    return {
      name,
      rule: name.split(".")[0],
      path,
      source,
      annotated,
      expected,
    };
  });

const lint = (source, path, ruleId, baseline = {}) =>
  new Linter({ configType: "flat", cwd: demoRoot }).verify(
    source,
    [
      {
        files: ["**/*.ts"],
        languageOptions: {
          parser: tseslint.parser,
          ecmaVersion: "latest",
          sourceType: "module",
        },
        plugins: { [PLUGIN_NAME]: midgard },
        settings: { midgard: { root: demoRoot, baseline } },
        rules: { [ruleId]: "error" },
      },
    ],
    { filename: join(demoRoot, path) },
  );

test("every rule has fixtures with ruleid and ok cases", () => {
  for (const rule of ruleNames) {
    const own = fixtures.filter((fixture) => fixture.rule === rule);
    assert.ok(own.length > 0, `${rule}: no fixture in ${fixturesDirectory}`);
    assert.ok(
      own.some((fixture) => fixture.expected.ruleid.length > 0),
      `${rule}: no \`// ruleid: ${PLUGIN_NAME}/${rule}\` case`,
    );
    assert.ok(
      own.some((fixture) => fixture.expected.ok.length > 0),
      `${rule}: no \`// ok: ${PLUGIN_NAME}/${rule}\` case`,
    );
  }
  for (const fixture of fixtures) {
    assert.ok(
      ruleNames.includes(fixture.rule),
      `${fixture.name}: no rule named ${fixture.rule}`,
    );
    assert.deepEqual(
      [...fixture.annotated],
      [`${PLUGIN_NAME}/${fixture.rule}`],
      `${fixture.name}: annotations must name ${PLUGIN_NAME}/${fixture.rule} only`,
    );
  }
});

for (const fixture of fixtures) {
  test(`${fixture.name} reports exactly its ruleid lines`, () => {
    const ruleId = `${PLUGIN_NAME}/${fixture.rule}`;
    const messages = lint(fixture.source, fixture.path, ruleId);
    const fatal = messages.filter((message) => message.fatal);
    assert.deepEqual(fatal, [], `${fixture.name} does not parse`);
    for (const message of messages) {
      assert.equal(message.ruleId, ruleId);
      assert.match(
        message.message,
        /Fix:/u,
        `${fixture.name}:${message.line} message says how to fix it`,
      );
    }
    assert.deepEqual(
      [...new Set(messages.map((message) => message.line))].sort(
        (a, b) => a - b,
      ),
      fixture.expected.ruleid,
      `${fixture.name}: reported lines vs ruleid lines`,
    );
    assert.equal(
      messages.length,
      fixture.expected.ruleid.length,
      `${fixture.name}: one report per ruleid line`,
    );
  });
}

test("the workspace config enables every rule as an error, with the baseline", async () => {
  const { default: config } = await import(join(demoRoot, "eslint.config.mjs"));
  const entry = config.find((item) => item.plugins?.[PLUGIN_NAME] === midgard);
  assert.ok(entry, "eslint.config.mjs does not register the midgard plugin");
  assert.equal(entry.settings.midgard.root, demoRoot);
  assert.ok(entry.settings.midgard.baseline !== undefined);
  assert.equal(entry.files, undefined, "the rules run on every file");
  for (const ruleId of ruleIds) {
    assert.equal(entry.rules[ruleId], "error", `${ruleId} is not an error`);
  }
});

test("the real baseline gives every entry a reason and names only real rules and files", () => {
  const baseline = JSON.parse(readFileSync(BASELINE_PATH, "utf8"));
  assert.deepEqual(
    baselineProblems(baseline, {
      ruleIds,
      fileExists: (file) => existsSync(join(demoRoot, file)),
    }),
    [],
  );
});

test("baselineProblems refuses entries without a reason, and unknown rules and files", () => {
  const problems = baselineProblems(
    {
      "midgard/no-such-rule": {
        "a.ts": { reason: "a reason that is long enough", sites: ["x"] },
      },
      "midgard/locale-compare-explicit-locale": {
        "b.ts": { reason: "short", sites: ["  padded  "] },
        "c.ts": { reason: "a reason that is long enough", sites: [], note: 1 },
      },
    },
    { ruleIds, fileExists: (file) => file !== "a.ts" },
  );
  assert.deepEqual(problems, [
    "midgard/no-such-rule: not a rule of the midgard plugin",
    "midgard/no-such-rule a.ts: file does not exist",
    "midgard/locale-compare-explicit-locale b.ts: every entry needs a reason (20+ characters)",
    "midgard/locale-compare-explicit-locale b.ts: sites must be a non-empty list of trimmed, whitespace-collapsed source lines",
    "midgard/locale-compare-explicit-locale c.ts: sites must be a non-empty list of trimmed, whitespace-collapsed source lines",
    "midgard/locale-compare-explicit-locale c.ts: unknown key(s) note",
  ]);
});

test("a baselined site is suppressed once per listing, and a stale one fails", () => {
  const ruleId = `${PLUGIN_NAME}/locale-compare-explicit-locale`;
  const path = "midgard-sdk/src/example.ts";
  const repeated = "export const x = a.localeCompare(b);";
  const source = [
    "declare const a: string;",
    "declare const b: string;",
    repeated,
    "export const y = a.localeCompare(b);",
    `      ${repeated.replace("x =", "z =")}`,
  ].join("\n");
  assert.deepEqual(
    lint(source, path, ruleId).map((message) => message.line),
    [3, 4, 5],
  );
  const baseline = {
    [ruleId]: {
      [path]: {
        reason: "synthetic entry for the ratchet test",
        sites: [
          repeated,
          fingerprint(`      ${repeated.replace("x =", "z =")}`),
          "export const gone = a.localeCompare(b);",
        ],
      },
    },
  };
  const messages = lint(source, path, ruleId, baseline);
  assert.deepEqual(
    messages.map((message) => [message.line, message.messageId]),
    [
      [1, "staleBaseline"],
      [4, "bareLocaleCompare"],
    ],
  );
  assert.match(messages[0].message, /no longer occur.*Fix:.*prune-baseline/su);
  // The baseline is keyed by file: the same line elsewhere is not excused.
  assert.deepEqual(
    lint(source, "midgard-sdk/src/other.ts", ruleId, baseline).map(
      (message) => message.line,
    ),
    [3, 4, 5],
  );
});

test("pruneBaseline only removes sites, never adds a rule, file or site", () => {
  const baseline = {
    "midgard/r": {
      "a.ts": { reason: "reason a", sites: ["one", "one", "two"] },
      "b.ts": { reason: "reason b", sites: ["gone"] },
    },
  };
  const actual = {
    "midgard/r": { "a.ts": ["one", "three"], "c.ts": ["new"] },
    "midgard/s": { "a.ts": ["new"] },
  };
  assert.deepEqual(pruneBaseline(baseline, actual), {
    pruned: { "midgard/r": { "a.ts": { reason: "reason a", sites: ["one"] } } },
    removed: 3,
  });
  assert.deepEqual(pruneBaseline(baseline, {}), { pruned: {}, removed: 4 });
});
