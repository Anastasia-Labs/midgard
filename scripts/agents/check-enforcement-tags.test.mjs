import assert from "node:assert/strict";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  KNOWN_ENFORCERS,
  LINE_BUDGETS,
  agentFacingFiles,
  checkEnforcementTags,
  resolveTag,
  tagsIn,
  trackedFiles,
} from "./check-enforcement-tags.mjs";
import {
  fixtureRepository,
  isolateGit,
  runScript,
  temporaryDirectory,
} from "./fixture-repository.mjs";

isolateGit();

const here = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(here, "../..");
const script = join(here, "check-enforcement-tags.mjs");

// Every enforcer a fixture rule can name, so each tag kind can resolve.
const enforcers = {
  "demo/eslint.config.mjs":
    'export default [{ rules: { "no-restricted-imports": ["error"] } }];\n',
  ".github/workflows/ci.yml":
    "name: Unit CI\njobs:\n  test:\n    steps:\n      - name: Run the unit tests\n        run: true\n",
  ".githooks/pre-commit": "#!/bin/sh\n",
  "scripts/guard.mjs": "export {};\n",
  "onchain/aiken/lib/midgard/sample.ak":
    "pub fn id(x) {\n  x\n}\n\ntest id_is_identity() {\n  id(1) == 1\n}\n",
  "onchain/aiken/lib/midgard/untested.ak": "pub fn id(x) {\n  x\n}\n",
  "src/config.ts": "export const loadConfig = () => ({});\n",
};

const check = (rules, extra = {}) =>
  checkEnforcementTags(
    fixtureRepository({
      ...enforcers,
      "docs/agents/rules.md": `# Rules\n\n${rules}\n`,
      ...extra,
    }),
  ).findings;

test("a rule tagged with an enforcer that exists passes, for every tag kind", () => {
  assert.deepEqual(
    check(
      [
        "- Never import a sibling's src. [eslint: no-restricted-imports]",
        "- Always run the unit tests. [ci: ci/Run the unit tests]",
        "- Always run them in CI. [ci: Unit CI/Run the unit tests]",
        "- Never commit the build output. [hook: pre-commit]",
        "- Always run the guard. [script: scripts/guard.mjs]",
        "- Never break identity. [aiken-test: midgard/sample]",
        "- Configuration must load. [runtime: loadConfig]",
        "- Always ask before deleting data. [review]",
        "",
        "Read the guide before you start.",
      ].join("\n"),
    ),
    [],
  );
});

test("a rule without a tag is a finding", () => {
  assert.deepEqual(check("You must never skip the tests."), [
    "docs/agents/rules.md:3: rule has no enforcement tag",
  ]);
});

test("a list item in a rules section is a rule even without a directive word", () => {
  const findings = checkEnforcementTags(
    fixtureRepository({
      ...enforcers,
      "docs/agents/guide.md":
        "# Guide\n\n## Hard rules\n\n- Keep the fixtures small.\n",
    }),
  ).findings;
  assert.deepEqual(findings, [
    "docs/agents/guide.md:5: rule has no enforcement tag",
  ]);
});

test("a document title naming rules does not make every list item a rule", () => {
  assert.deepEqual(
    checkEnforcementTags(
      fixtureRepository({
        ...enforcers,
        "docs/agents/guide.md": "# Reset Rules\n\n- The index lists files.\n",
      }),
    ).findings,
    [],
  );
});

test("a directive word inside inline code does not make a rule", () => {
  assert.deepEqual(check("Set `MUST_RUN=1` to run it."), []);
});

test("a rule with two tags is a finding", () => {
  assert.match(
    check("Never skip the guard. [script: scripts/guard.mjs] [review]")[0],
    /rule has 2 tags; use exactly one/u,
  );
});

test("an unknown tag kind is a finding", () => {
  assert.deepEqual(check("Never skip it. [lint: something]"), [
    "docs/agents/rules.md:3: [lint: something]: unknown tag kind [lint]",
  ]);
});

test("each tag kind that does not resolve is a finding", () => {
  const cases = [
    ["[eslint: no-console]", /ESLint rule no-console is not configured/u],
    ["[ci: ci/Some other step]", /has no step named "Some other step"/u],
    ["[ci: missing/Run the unit tests]", /no tracked workflow missing\.yml/u],
    ["[hook: pre-push]", /\.githooks\/pre-push is not a tracked hook/u],
    [
      "[script: scripts/missing.mjs]",
      /scripts\/missing\.mjs is not a tracked file/u,
    ],
    ["[aiken-test: midgard/untested]", /defines no test/u],
    ["[aiken-test: midgard/absent]", /no tracked Aiken module/u],
    ["[runtime: loadMissing]", /no tracked source file defines loadMissing/u],
    ["[review: someone]", /\[review\] takes no value/u],
  ];
  for (const [tag, reason] of cases) {
    const findings = check(`Never skip it. ${tag}`);
    assert.equal(findings.length, 1, `${tag}: ${findings.join("; ")}`);
    assert.match(findings[0], reason, tag);
  }
});

test("a script cited by a tag must be tracked, not only present", () => {
  const findings = checkEnforcementTags(
    fixtureRepository(
      { "docs/agents/rules.md": "Always run it. [script: scripts/new.mjs]\n" },
      { "scripts/new.mjs": "export {};\n" },
    ),
  ).findings;
  assert.match(findings[0], /scripts\/new\.mjs is not a tracked file/u);
});

test("a runtime symbol mentioned only in Markdown does not resolve", () => {
  const findings = check("Configuration must load. [runtime: onlyInDocs]", {
    "docs/notes.md": "const onlyInDocs = 1\n",
  });
  assert.match(findings[0], /no tracked source file defines onlyInDocs/u);
});

test("a [review] rule that a known check enforces is under-claimed", () => {
  const findings = check("- Never bypass localUPLCEval. [review]");
  assert.equal(findings.length, 1);
  assert.match(findings[0], /under-claimed \[review\]/u);
});

test("a file over its line budget is a finding", () => {
  const body = Array.from({ length: 125 }, (_, index) => `Line ${index}.`).join(
    "\n\n",
  );
  const findings = checkEnforcementTags(
    fixtureRepository({ "docs/agents/long.md": `# Long\n\n${body}\n` }),
  ).findings;
  assert.equal(findings.length, 1);
  assert.match(
    findings[0],
    /docs\/agents\/long\.md: \d+ lines is over its budget of 120/u,
  );
});

test("the files checked are the agent-facing ones", () => {
  assert.deepEqual(
    agentFacingFiles(
      new Set([
        "AGENTS.md",
        "demo/AGENTS.md",
        "docs/agents/verification.md",
        "docs/agents/nested/deep.md",
        ".agents/skills/build/SKILL.md",
        ".agents/skills/build/references/notes.md",
        "docs/README.md",
      ]),
    ),
    [
      ".agents/skills/build/SKILL.md",
      "AGENTS.md",
      "demo/AGENTS.md",
      "docs/agents/verification.md",
    ],
  );
});

test("a tag-shaped Markdown link is not a tag", () => {
  assert.deepEqual(tagsIn("See [review](review.md) and [script: x]"), [
    { whole: "[script: x]", kind: "script", value: "x" },
  ]);
});

test("the command line exits 1 on findings and 0 when clean", () => {
  const dirty = fixtureRepository({
    "docs/agents/rules.md": "You must run it.\n",
  });
  const failed = runScript(script, ["--root", dirty]);
  assert.equal(failed.status, 1, failed.stderr);
  assert.match(failed.stderr, /rule has no enforcement tag/u);

  const clean = fixtureRepository({
    "docs/agents/rules.md": "You must run it. [review]\n",
  });
  const passed = runScript(script, ["--root", clean]);
  assert.equal(passed.status, 0, passed.stderr);
});

test("could not look is exit 2, never a pass", () => {
  const notARepository = runScript(script, [
    "--root",
    temporaryDirectory("midgard-agent-plain-"),
  ]);
  assert.equal(notARepository.status, 2, notARepository.stderr);
  assert.match(notARepository.stderr, /could not look/u);

  const nothingTracked = runScript(script, ["--root", fixtureRepository({})]);
  assert.equal(nothingTracked.status, 2, nothingTracked.stderr);
  assert.match(nothingTracked.stderr, /no agent-facing files are tracked/u);
});

test("this repository's agent-facing files pass", () => {
  assert.deepEqual(checkEnforcementTags(repositoryRoot).findings, []);
});

test("every known enforcer resolves in this repository", () => {
  const context = {
    root: repositoryRoot,
    tracked: trackedFiles(repositoryRoot),
  };
  for (const { tag } of KNOWN_ENFORCERS) {
    const [parsed] = tagsIn(tag);
    assert.equal(resolveTag(parsed, context), undefined, tag);
  }
});

test("every line budget names a tracked agent-facing file", () => {
  const files = agentFacingFiles(trackedFiles(repositoryRoot));
  for (const path of Object.keys(LINE_BUDGETS))
    assert.ok(files.includes(path), `${path} has a budget but is not checked`);
});
