import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const hook = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../.githooks/pre-push",
);
const env = {
  ...process.env,
  GIT_CONFIG_NOSYSTEM: "1",
  GIT_CONFIG_GLOBAL: "/dev/null",
  GIT_AUTHOR_NAME: "t",
  GIT_AUTHOR_EMAIL: "t@example.invalid",
  GIT_COMMITTER_NAME: "t",
  GIT_COMMITTER_EMAIL: "t@example.invalid",
  MIDGARD_SKIP_HOOKS: "",
  MIDGARD_SKIP_PREFLIGHT: "",
};

// Fixture setup must not address the repository whose hook launched the suite.
const localVars = spawnSync("git", ["rev-parse", "--local-env-vars"], {
  encoding: "utf8",
});
assert.equal(localVars.status, 0, localVars.stderr);
for (const name of localVars.stdout.trim().split("\n")) delete env[name];

const git = (cwd, ...args) => {
  const run = spawnSync("git", args, { cwd, env, encoding: "utf8" });
  assert.equal(run.status, 0, run.stderr);
  return run.stdout.trim();
};

for (const extended of [false, true]) {
  test(`preflight isolates nested Git fixtures with ${extended ? "all hook paths" : "GIT_DIR"} exported`, () => {
    const repo = mkdtempSync(join(tmpdir(), "pre-push-git-environment-"));
    try {
      git(repo, "init", "--quiet");
      mkdirSync(join(repo, "scripts"));
      const fixture = join(repo, "nested.git");
      writeFileSync(
        join(repo, "scripts/preflight.mjs"),
        `import { spawnSync } from "node:child_process";
import { writeFileSync } from "node:fs";
const run = (...args) => {
  const result = spawnSync("git", args, { encoding: "utf8" });
  if (result.status !== 0) throw new Error(result.stderr);
  return result.stdout.trim();
};
run("init", "--quiet", "--bare", ${JSON.stringify(fixture)});
writeFileSync("fixture-result.json", JSON.stringify({
  gitDir: run("-C", ${JSON.stringify(fixture)}, "rev-parse", "--absolute-git-dir"),
  bare: run("-C", ${JSON.stringify(fixture)}, "config", "--local", "core.bare"),
}));
`,
      );
      git(repo, "add", "--", "scripts");
      git(repo, "commit", "--quiet", "-m", "fixture");
      const head = git(repo, "rev-parse", "HEAD");
      const configBefore = readFileSync(join(repo, ".git/config"), "utf8");
      const run = spawnSync("bash", [hook, "origin", "git@example:x"], {
        cwd: repo,
        encoding: "utf8",
        env: {
          ...env,
          GIT_DIR: join(repo, ".git"),
          ...(extended
            ? {
                GIT_COMMON_DIR: join(repo, ".git"),
                GIT_WORK_TREE: repo,
                GIT_INDEX_FILE: join(repo, ".git/index"),
                GIT_OBJECT_DIRECTORY: join(repo, ".git/objects"),
                GIT_PREFIX: "",
              }
            : {}),
        },
        input: `refs/heads/b ${head} refs/heads/b ${"0".repeat(40)}\n`,
      });
      assert.equal(
        readFileSync(join(repo, ".git/config"), "utf8"),
        configBefore,
        "nested git init --bare must not change the parent repository config",
      );
      assert.equal(run.status, 0, run.stderr);
      assert.deepEqual(
        JSON.parse(readFileSync(join(repo, "fixture-result.json"), "utf8")),
        { gitDir: fixture, bare: "true" },
        "preflight must create and address its separate fixture repository",
      );
    } finally {
      rmSync(repo, { recursive: true, force: true });
    }
  });
}
