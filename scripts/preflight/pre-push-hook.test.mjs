// .githooks/pre-push against a stub preflight in a temporary repository.

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
  "../../.githooks/pre-push",
);

const ENV = {
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
// A real push exports repository-local variables; fixture setup must not use
// the repository whose hook launched this test suite.
for (const name of spawnSync("git", ["rev-parse", "--local-env-vars"], {
  encoding: "utf8",
})
  .stdout.trim()
  .split("\n")) {
  delete ENV[name];
}

// The stub records its arguments and exits with $STUB_EXIT.
const STUB = `import { appendFileSync } from "node:fs";
appendFileSync("ran.log", process.argv.slice(2).join(" ") + "\\n");
process.exit(Number(process.env.STUB_EXIT ?? "0"));
`;

const withRepo = (body) => {
  const repo = mkdtempSync(join(tmpdir(), "pre-push-hook-test-"));
  try {
    spawnSync("git", ["init", "--quiet"], { cwd: repo, env: ENV });
    mkdirSync(join(repo, "scripts"));
    writeFileSync(join(repo, "scripts/preflight.mjs"), STUB);
    writeFileSync(join(repo, ".gitignore"), "ran.log\n");
    spawnSync("git", ["add", "--", "scripts", ".gitignore"], {
      cwd: repo,
      env: ENV,
    });
    spawnSync("git", ["commit", "--quiet", "-m", "x"], { cwd: repo, env: ENV });
    const head = spawnSync("git", ["rev-parse", "HEAD"], {
      cwd: repo,
      encoding: "utf8",
      env: ENV,
    }).stdout.trim();
    const push = ({ sha = head, env = {} } = {}) => {
      const run = spawnSync("bash", [hook, "origin", "git@example:x"], {
        cwd: repo,
        encoding: "utf8",
        env: { ...ENV, ...env },
        input: `refs/heads/b ${sha} refs/heads/b ${"0".repeat(40)}\n`,
      });
      const ran = spawnSync("cat", ["ran.log"], {
        cwd: repo,
        encoding: "utf8",
      }).stdout;
      rmSync(join(repo, "ran.log"), { force: true });
      return { ...run, ran };
    };
    return body(push, repo);
  } finally {
    rmSync(repo, { recursive: true, force: true });
  }
};

test("a failed preflight blocks the push and names the escape", () =>
  withRepo((push) => {
    const run = push({ env: { STUB_EXIT: "1" } });
    assert.equal(run.status, 1);
    assert.equal(run.ran, "--pre-push\n");
    assert.match(run.stderr, /MIDGARD_SKIP_PREFLIGHT=1 git push/u);
  }));

test("passed, skipped-for-capability and could-not-run all let the push go", () =>
  withRepo((push) => {
    assert.equal(push({ env: { STUB_EXIT: "0" } }).status, 0);
    const skipped = push({ env: { STUB_EXIT: "3" } });
    assert.equal(skipped.status, 0);
    assert.match(skipped.stderr, /could not run/u);
    const usage = push({ env: { STUB_EXIT: "2" } });
    assert.equal(usage.status, 0);
    assert.match(usage.stderr, /could not run \(exit 2/u);
  }));

test("the escapes skip preflight entirely", () =>
  withRepo((push) => {
    for (const env of [
      { MIDGARD_SKIP_PREFLIGHT: "1", STUB_EXIT: "1" },
      { MIDGARD_SKIP_HOOKS: "1", STUB_EXIT: "1" },
    ]) {
      const run = push({ env });
      assert.equal(run.status, 0);
      assert.equal(run.ran, "");
    }
  }));

test("a push of something other than HEAD is not judged", () =>
  withRepo((push) => {
    const other = push({ sha: "1".repeat(40), env: { STUB_EXIT: "1" } });
    assert.equal(other.status, 0);
    assert.equal(other.ran, "");
    assert.match(other.stderr, /not the checked-out HEAD/u);
    const deletion = push({ sha: "0".repeat(40), env: { STUB_EXIT: "1" } });
    assert.equal(deletion.status, 0);
    assert.equal(deletion.ran, "");
  }));

for (const extended of [false, true]) {
  test(`preflight isolates nested Git fixtures with ${extended ? "all hook paths" : "GIT_DIR"} exported`, () =>
    withRepo((push, repo) => {
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
      const configBefore = readFileSync(join(repo, ".git/config"), "utf8");
      const run = push({
        env: {
          GIT_DIR: join(repo, ".git"),
          ...(extended
            ? {
                GIT_WORK_TREE: repo,
                GIT_INDEX_FILE: join(repo, ".git/index"),
                GIT_OBJECT_DIRECTORY: join(repo, ".git/objects"),
                GIT_PREFIX: "",
              }
            : {}),
        },
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
    }));
}
