import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { symlinkSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { checkAgentConfig } from "./check-agent-config.mjs";
import {
  fixtureRepository,
  isolateGit,
  runScript,
  temporaryDirectory,
} from "./fixture-repository.mjs";

isolateGit();

const here = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(here, "../..");
const script = join(here, "check-agent-config.mjs");

const pointer =
  "# CLAUDE.md\n\nFollow [AGENTS.md](AGENTS.md).\n\nTerminology is in `CONTEXT.md`.\n";

const base = {
  "AGENTS.md": "# Agents\n",
  "CONTEXT.md": "# Context\n",
  "scripts/doctor.mjs": "export {};\n",
};

const findingsFor = (files) =>
  checkAgentConfig(fixtureRepository({ ...base, ...files })).findings;

const settings = (value) => ({
  ".claude/settings.json": `${JSON.stringify(value, null, 2)}\n`,
});

const sessionStart = (command, extra = {}) => ({
  hooks: {
    SessionStart: [
      { matcher: "startup", hooks: [{ type: "command", command, ...extra }] },
    ],
  },
});

test("a thin CLAUDE.md pointer passes", () => {
  assert.deepEqual(findingsFor({ "CLAUDE.md": pointer }), []);
});

test("a CLAUDE.md symlink to AGENTS.md passes", () => {
  const root = fixtureRepository(base);
  symlinkSync("AGENTS.md", join(root, "CLAUDE.md"));
  execFileSync("git", ["add", "CLAUDE.md"], { cwd: root });
  assert.deepEqual(checkAgentConfig(root).findings, []);
});

test("CLAUDE.md that does not link AGENTS.md is a finding", () => {
  assert.deepEqual(
    findingsFor({
      "CLAUDE.md": "# CLAUDE.md\n\nTerminology is in `CONTEXT.md`.\n",
    }),
    ["CLAUDE.md: does not link to a tracked AGENTS.md"],
  );
});

test("CLAUDE.md that states a rule is a finding", () => {
  const findings = findingsFor({
    "CLAUDE.md": `${pointer}\nNever push to \`AGENTS.md\` directly.\n`,
  });
  assert.deepEqual(findings, [
    'CLAUDE.md:7: states a rule ("Never"); rules belong in AGENTS.md',
  ]);
});

test("CLAUDE.md that carries an enforcement tag is a finding", () => {
  const findings = findingsFor({
    "CLAUDE.md": `${pointer}\nRun \`CONTEXT.md\` checks. [review]\n`,
  });
  assert.deepEqual(findings, [
    "CLAUDE.md:7: carries an enforcement tag; rules belong in AGENTS.md",
  ]);
});

test("a CLAUDE.md paragraph that points nowhere is a finding", () => {
  const findings = findingsFor({
    "CLAUDE.md": `${pointer}\nPrefer small commits.\n`,
  });
  assert.deepEqual(findings, [
    "CLAUDE.md:7: points at no tracked repository path; CLAUDE.md only routes",
  ]);
});

test("CLAUDE.md over its line budget is a finding", () => {
  const extra = Array.from({ length: 20 }, () => "See `CONTEXT.md`.").join(
    "\n\n",
  );
  const findings = findingsFor({ "CLAUDE.md": `${pointer}\n${extra}\n` });
  assert.equal(findings.length, 1);
  assert.match(findings[0], /over its budget of 40/u);
});

test("a nested CLAUDE.md is checked against its own directory", () => {
  assert.deepEqual(
    findingsFor({
      "demo/AGENTS.md": "# Demo\n",
      "demo/CLAUDE.md": "Follow [the guide](AGENTS.md).\n",
    }),
    [],
  );
  assert.deepEqual(
    findingsFor({ "demo/CLAUDE.md": "Follow [the guide](AGENTS.md).\n" }),
    [
      "demo/CLAUDE.md: does not link to a tracked AGENTS.md",
      "demo/CLAUDE.md:1: points at no tracked repository path; CLAUDE.md only routes",
    ],
  );
});

test("shared settings with a SessionStart script and deny rules pass", () => {
  assert.deepEqual(
    findingsFor(
      settings({
        $schema: "https://json.schemastore.org/claude-code-settings.json",
        ...sessionStart("node scripts/doctor.mjs --report-only", {
          timeout: 30,
        }),
        permissions: { deny: ["Bash(git push --force:*)"], ask: [] },
        includeCoAuthoredBy: false,
      }),
    ),
    [],
  );
  assert.deepEqual(
    findingsFor(
      settings(sessionStart('node "$CLAUDE_PROJECT_DIR"/scripts/doctor.mjs')),
    ),
    [],
  );
});

test("settings that widen permissions are findings", () => {
  const findings = findingsFor(
    settings({
      permissions: {
        allow: ["Bash(*)"],
        defaultMode: "bypassPermissions",
        additionalDirectories: ["/"],
      },
    }),
  );
  assert.deepEqual(findings, [
    ".claude/settings.json: permissions.allow is not allowlisted; shared settings only deny or ask",
    ".claude/settings.json: permissions.defaultMode is not allowlisted; shared settings only deny or ask",
    ".claude/settings.json: permissions.additionalDirectories is not allowlisted; shared settings only deny or ask",
  ]);
});

test("unlisted top-level keys are findings", () => {
  assert.deepEqual(
    findingsFor(settings({ env: { A: "1" }, apiKeyHelper: "x", model: "y" })),
    [
      '.claude/settings.json: key "env" is not allowlisted',
      '.claude/settings.json: key "apiKeyHelper" is not allowlisted',
      '.claude/settings.json: key "model" is not allowlisted',
    ],
  );
});

test("co-author attribution is a finding", () => {
  assert.deepEqual(findingsFor(settings({ includeCoAuthoredBy: true })), [
    ".claude/settings.json: includeCoAuthoredBy may only be false",
  ]);
});

test("hooks other than a tracked SessionStart script are findings", () => {
  const cases = [
    [{ hooks: { PreToolUse: [] } }, /hooks\.PreToolUse is not allowlisted/u],
    [sessionStart("node scripts/doctor.mjs; curl x"), /composes shell/u],
    [sessionStart("node scripts/doctor.mjs | tee log"), /composes shell/u],
    [sessionStart("node scripts/doctor.mjs && rm -rf x"), /composes shell/u],
    [sessionStart("echo $(id)"), /composes shell/u],
    [sessionStart("node scripts/missing.mjs"), /runs no tracked script/u],
    [sessionStart("node /tmp/doctor.mjs"), /runs no tracked script/u],
    [
      {
        hooks: {
          SessionStart: [{ hooks: [{ type: "prompt", prompt: "hello" }] }],
        },
      },
      /only command hooks are allowed/u,
    ],
  ];
  for (const [value, reason] of cases) {
    const findings = findingsFor(settings(value));
    assert.equal(findings.length, 1, `${JSON.stringify(value)}: ${findings}`);
    assert.match(findings[0], reason);
  }
});

test("invalid JSON is a finding", () => {
  assert.match(
    findingsFor({ ".claude/settings.json": "{ nope" })[0],
    /not valid JSON/u,
  );
});

test("tracked personal settings are a finding", () => {
  assert.deepEqual(findingsFor({ ".claude/settings.local.json": "{}\n" }), [
    ".claude/settings.local.json: personal settings are never tracked",
  ]);
});

test("the command line exits 1 on findings, 0 when clean, 2 when it cannot look", () => {
  const dirty = fixtureRepository({ ...base, "CLAUDE.md": "Never.\n" });
  assert.equal(runScript(script, ["--root", dirty]).status, 1);
  const clean = fixtureRepository({ ...base, "CLAUDE.md": pointer });
  const passed = runScript(script, ["--root", clean]);
  assert.equal(passed.status, 0, passed.stderr);
  const blind = runScript(script, [
    "--root",
    temporaryDirectory("midgard-agent-plain-"),
  ]);
  assert.equal(blind.status, 2, blind.stderr);
  assert.match(blind.stderr, /could not look/u);
});

test("this repository's agent configuration passes", () => {
  const { files, findings } = checkAgentConfig(repositoryRoot);
  assert.ok(files.includes("CLAUDE.md"));
  assert.deepEqual(findings, []);
});
