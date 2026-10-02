// The pre-push preflight runs these tooling tests from a git hook, which
// exports GIT_DIR and GIT_INDEX_FILE. A test that runs `git init` in a
// temporary directory with that environment re-initialises the outer
// repository instead; from a linked worktree, whose git directory does not end
// in `/.git`, git guesses bare and writes core.bare = true into the shared
// configuration, breaking every worktree. Each test that creates a repository
// must drop the inherited GIT_* variables first.

import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, relative } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const scripts = dirname(fileURLToPath(import.meta.url));

const testFiles = (directory) =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name);
    if (entry.isDirectory())
      return entry.name === "node_modules" ? [] : testFiles(path);
    return entry.name.endsWith(".test.mjs") ? [path] : [];
  });

const runsGitInit = (source) => /["']init["']/u.test(source);

const isolatesGit = (source) =>
  /key\.startsWith\(["']GIT_["']\)/u.test(source) ||
  /\bisolateGit\(/u.test(source);

test("every tooling test that creates a repository drops the hook's GIT_* environment", () => {
  const offenders = testFiles(scripts)
    .filter((path) => {
      const source = readFileSync(path, "utf8");
      return runsGitInit(source) && !isolatesGit(source);
    })
    .map((path) => relative(scripts, path));
  assert.deepEqual(offenders, []);
});

test("the guard recognises a git init and both isolation forms", () => {
  assert.equal(runsGitInit('git(repo, "init", "--quiet")'), true);
  assert.equal(runsGitInit('spawnSync("git", ["status"])'), false);
  assert.equal(
    isolatesGit('if (key.startsWith("GIT_")) delete process.env[key];'),
    true,
  );
  assert.equal(isolatesGit("const { git } = isolateGit();"), true);
  assert.equal(isolatesGit("const env = { ...process.env };"), false);
});
