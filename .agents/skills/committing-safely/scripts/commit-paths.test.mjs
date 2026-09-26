// Tests for commit-paths.mjs. Every test runs in a throwaway git repository
// under the OS temp directory, with the user's global and system git config
// switched off; nothing here touches the repository this file lives in.
//
//   node --test .agents/skills/committing-safely/scripts/commit-paths.test.mjs

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  chmodSync,
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { after, describe, test } from "node:test";
import { fileURLToPath } from "node:url";

const SCRIPT = fileURLToPath(new URL("./commit-paths.mjs", import.meta.url));
// Read-only: the real hook and pin checker are copied INTO throwaway repos.
const SOURCE_REPO = resolve(dirname(SCRIPT), "../../../..");
const TMP = realpathSync(tmpdir());
const made = [];
after(() => {
  for (const dir of made) rmSync(dir, { recursive: true, force: true });
});

const baseEnv = (extra = {}) => {
  const env = { ...process.env };
  for (const key of Object.keys(env)) {
    if (key.startsWith("GIT_") || key.startsWith("MIDGARD_")) delete env[key];
  }
  return {
    ...env,
    GIT_CONFIG_GLOBAL: "/dev/null",
    GIT_CONFIG_NOSYSTEM: "1",
    GIT_AUTHOR_NAME: "Test Author",
    GIT_AUTHOR_EMAIL: "author@example.invalid",
    GIT_COMMITTER_NAME: "Test Author",
    GIT_COMMITTER_EMAIL: "author@example.invalid",
    ...extra,
  };
};

const makeRepo = (files = {}) => {
  const dir = realpathSync(mkdtempSync(join(TMP, "commit-paths-test-")));
  made.push(dir);
  assert.ok(
    dir.startsWith(TMP),
    "test repositories live under the OS temp dir",
  );
  const git = (args, options = {}) => {
    const result = spawnSync("git", args, {
      cwd: options.cwd ?? dir,
      env: baseEnv(options.env),
      encoding: "utf8",
      input: options.input,
    });
    if (options.allowFail !== true && result.status !== 0) {
      throw new Error(`git ${args.join(" ")} failed: ${result.stderr}`);
    }
    return result;
  };
  const write = (path, content) => {
    mkdirSync(dirname(join(dir, path)), { recursive: true });
    writeFileSync(join(dir, path), content);
  };
  const read = (path) => readFileSync(join(dir, path), "utf8");
  git(["init", "-q", "-b", "main"]);
  git(["config", "core.hooksPath", join(dir, ".git/hooks")]);
  for (const [path, content] of Object.entries({
    "README.md": "readme\n",
    ...files,
  })) {
    write(path, content);
  }
  git(["add", "--", "."]);
  git(["commit", "-q", "--no-verify", "-m", "Initial"]);
  const commitPaths = (args, { env, cwd } = {}) => {
    const result = spawnSync(process.execPath, [SCRIPT, ...args], {
      cwd: cwd ?? dir,
      env: baseEnv(env),
      encoding: "utf8",
    });
    return {
      code: result.status,
      stdout: result.stdout,
      stderr: result.stderr,
    };
  };
  const head = () => git(["rev-parse", "HEAD"]).stdout.trim();
  const committedPaths = (rev = "HEAD") =>
    git(["show", "--name-only", "--format=", rev])
      .stdout.split("\n")
      .filter(Boolean)
      .sort();
  const status = () => git(["status", "--porcelain=v1"]).stdout;
  return { dir, git, write, read, commitPaths, head, committedPaths, status };
};

const makeTool = (repo, name, body) => {
  const path = join(repo.dir, ".tools", name);
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, `#!/usr/bin/env node\n${body}\n`);
  chmodSync(path, 0o755);
  // Keep the tool out of `git status` so it is not mistaken for foreign work.
  writeFileSync(join(repo.dir, ".git/info/exclude"), ".tools/\n");
  return path;
};

const statusWithout = (repo, path) =>
  repo
    .status()
    .split("\n")
    .filter((line) => line.length > 0 && !line.endsWith(` ${path}`))
    .join("\n");

describe("commits exactly the named paths", () => {
  test("a file another session staged is not swept into the commit", () => {
    const repo = makeRepo({ "a.txt": "a\n", "b.txt": "b\n" });
    repo.write("a.txt", "a mine\n");
    repo.write("b.txt", "b theirs\n");
    repo.git(["add", "--", "b.txt"]);
    const before = repo.head();

    const result = repo.commitPaths(["-m", "Change a", "--", "a.txt"]);

    assert.equal(result.code, 0, result.stderr);
    assert.equal(repo.git(["rev-parse", "HEAD^"]).stdout.trim(), before);
    assert.deepEqual(repo.committedPaths(), ["a.txt"]);
    assert.equal(
      repo.git(["diff", "--cached", "--name-only"]).stdout.trim(),
      "b.txt",
    );
    assert.equal(
      repo.git(["show", ":b.txt"]).stdout,
      "b theirs\n",
      "the foreign staged content is still staged",
    );
    assert.match(result.stdout, /Change a/u);
    assert.match(result.stdout, /a\.txt \| 2/u);
  });

  test("unstaged edits, partial staging and untracked files elsewhere survive untouched", () => {
    const repo = makeRepo({ "a.txt": "a\n", "c.txt": "c\n", "d.txt": "d\n" });
    repo.write("a.txt", "a mine\n");
    repo.write("c.txt", "c theirs, unstaged\n");
    repo.write("d.txt", "d staged\n");
    repo.git(["add", "--", "d.txt"]);
    repo.write("d.txt", "d staged then edited again\n");
    repo.write("u.txt", "untracked\n");
    const before = statusWithout(repo, "a.txt");
    const stagedD = repo.git(["show", ":d.txt"]).stdout;

    const result = repo.commitPaths(["-m", "Change a", "--", "a.txt"]);

    assert.equal(result.code, 0, result.stderr);
    assert.equal(statusWithout(repo, "a.txt"), before);
    assert.equal(repo.read("c.txt"), "c theirs, unstaged\n");
    assert.equal(repo.read("d.txt"), "d staged then edited again\n");
    assert.equal(repo.git(["show", ":d.txt"]).stdout, stagedD);
    assert.equal(repo.read("u.txt"), "untracked\n");
  });

  test("the real index is clean for committed paths afterwards", () => {
    const repo = makeRepo({ "a.txt": "a\n", "gone.txt": "bye\n" });
    repo.write("a.txt", "a changed\n");
    repo.write("new.txt", "brand new\n");
    rmSync(join(repo.dir, "gone.txt"));

    const result = repo.commitPaths([
      "-m",
      "Change, add and delete",
      "--",
      "a.txt",
      "new.txt",
      "gone.txt",
    ]);

    assert.equal(result.code, 0, result.stderr);
    assert.deepEqual(repo.committedPaths(), ["a.txt", "gone.txt", "new.txt"]);
    assert.equal(repo.status(), "", "no reverse-staged or leftover entries");
  });

  test("a path already staged with the same content commits and stays clean", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("a.txt", "a staged by me\n");
    repo.git(["add", "--", "a.txt"]);

    const result = repo.commitPaths(["-m", "Change a", "--", "a.txt"]);

    assert.equal(result.code, 0, result.stderr);
    assert.equal(repo.status(), "");
  });

  test("paths are resolved against the current directory", () => {
    const repo = makeRepo({ "sub/a.txt": "a\n" });
    repo.write("sub/a.txt", "a changed\n");

    const result = repo.commitPaths(["-m", "Change a", "--", "a.txt"], {
      cwd: join(repo.dir, "sub"),
    });

    assert.equal(result.code, 0, result.stderr);
    assert.deepEqual(repo.committedPaths(), ["sub/a.txt"]);
  });

  test("--dry-run reports the stat and commits nothing", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("a.txt", "a changed\n");
    const before = repo.head();

    const result = repo.commitPaths([
      "--dry-run",
      "-m",
      "Change a",
      "--",
      "a.txt",
    ]);

    assert.equal(result.code, 0, result.stderr);
    assert.match(result.stdout, /dry run: would commit/u);
    assert.equal(repo.head(), before);
    assert.equal(repo.status(), " M a.txt\n");
  });
});

describe("refusals leave HEAD, the index and the tree alone", () => {
  const refused = (repo, args, pattern, code = 1) => {
    const before = { head: repo.head(), status: repo.status() };
    const result = repo.commitPaths(args);
    assert.equal(result.code, code, `${result.stdout}\n${result.stderr}`);
    assert.match(result.stderr, pattern);
    assert.equal(repo.head(), before.head, "HEAD did not move");
    assert.equal(repo.status(), before.status, "index and tree unchanged");
  };

  test("onchain/aiken/plutus.json is refused, by path and by patch", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("onchain/aiken/plutus.json", "{}\n");
    refused(
      repo,
      ["-m", "Blueprint", "--", "onchain/aiken/plutus.json"],
      /plutus\.json .*never committed/u,
    );
    repo.write("a.txt", "a changed\n");
    refused(
      repo,
      ["-m", "Blueprint", "--", "a.txt", "onchain/aiken/plutus.json"],
      /never committed/u,
    );
    repo.git(["add", "-N", "--", "onchain/aiken/plutus.json"]);
    const patch = repo.git(["diff", "--", "onchain/aiken/plutus.json"]).stdout;
    repo.git(["rm", "-q", "--cached", "--", "onchain/aiken/plutus.json"]);
    writeFileSync(join(repo.dir, ".git/blueprint.patch"), patch);
    refused(
      repo,
      ["-m", "Blueprint", "--patch", ".git/blueprint.patch"],
      /never committed/u,
    );
  });

  test("no paths is a usage error", () => {
    const repo = makeRepo();
    refused(repo, ["-m", "Nothing"], /no paths given/u, 2);
    refused(repo, ["-m", "Nothing", "--"], /no paths given/u, 2);
  });

  test("a message is required", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("a.txt", "changed\n");
    refused(repo, ["--", "a.txt"], /message is required/u, 2);
  });

  test("a path with no change is refused", () => {
    const repo = makeRepo({ "a.txt": "a\n", "b.txt": "b\n" });
    repo.write("a.txt", "a changed\n");
    refused(
      repo,
      ["-m", "Change", "--", "a.txt", "b.txt"],
      /no change against HEAD in: b\.txt/u,
    );
  });

  test("a path that does not exist anywhere is refused", () => {
    const repo = makeRepo();
    refused(repo, ["-m", "Ghost", "--", "ghost.txt"], /did not match/u);
  });

  test("a directory, the repository root and outside paths are refused", () => {
    const repo = makeRepo({ "dir/a.txt": "a\n" });
    repo.write("dir/a.txt", "changed\n");
    refused(repo, ["-m", "Dir", "--", "dir"], /is a directory/u);
    refused(repo, ["-m", "Root", "--", "."], /repository root/u);
    refused(
      repo,
      ["-m", "Out", "--", "../elsewhere.txt"],
      /outside the repository/u,
    );
  });

  test("a different version of the path staged in the real index is refused", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("a.txt", "someone else's staged version\n");
    repo.git(["add", "--", "a.txt"]);
    repo.write("a.txt", "my worktree version\n");
    refused(
      repo,
      ["-m", "Change a", "--", "a.txt"],
      /different staged version of: a\.txt/u,
    );
    assert.equal(
      repo.git(["show", ":a.txt"]).stdout,
      "someone else's staged version\n",
    );
  });

  test("a tool attribution trailer is refused", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("a.txt", "changed\n");
    refused(
      repo,
      [
        "-m",
        "Change a",
        "-m",
        "Co-authored-by: Codex <codex@example.invalid>",
        "--",
        "a.txt",
      ],
      /attribution/u,
    );
  });

  test("an in-progress merge is refused", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("a.txt", "changed\n");
    writeFileSync(join(repo.dir, ".git/MERGE_HEAD"), `${repo.head()}\n`);
    refused(
      repo,
      ["-m", "Change", "--", "a.txt"],
      /MERGE_HEAD is in progress/u,
    );
  });

  test("an inherited GIT_INDEX_FILE is refused", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("a.txt", "changed\n");
    const result = spawnSync(
      process.execPath,
      [SCRIPT, "-m", "x", "--", "a.txt"],
      {
        cwd: repo.dir,
        env: {
          ...baseEnv(),
          GIT_INDEX_FILE: join(repo.dir, ".git/other-index"),
        },
        encoding: "utf8",
      },
    );
    assert.equal(result.status, 1);
    assert.match(result.stderr, /GIT_INDEX_FILE is set/u);
  });
});

describe("--patch commits only the selected hunks", () => {
  const lines = Array.from({ length: 20 }, (_, i) => `line ${String(i + 1)}`);

  test("the unselected hunk stays unstaged in the tree", () => {
    const repo = makeRepo({ "f.txt": `${lines.join("\n")}\n` });
    const edited = [...lines];
    edited[1] = "line 2 MINE";
    edited[17] = "line 18 THEIRS";
    repo.write("f.txt", `${edited.join("\n")}\n`);
    const full = repo.git(["diff", "-U1", "HEAD", "--", "f.txt"]).stdout;
    const hunks = full.split(/^(?=@@ )/mu);
    assert.equal(hunks.length, 3, "header plus two hunks");
    writeFileSync(join(repo.dir, ".git/mine.patch"), hunks[0] + hunks[1]);

    const result = repo.commitPaths([
      "-m",
      "Change line 2",
      "--patch",
      ".git/mine.patch",
      "--",
      "f.txt",
    ]);

    assert.equal(result.code, 0, result.stderr);
    const committed = repo.git(["show", "HEAD:f.txt"]).stdout;
    assert.match(committed, /line 2 MINE/u);
    assert.doesNotMatch(committed, /THEIRS/u);
    assert.match(repo.read("f.txt"), /line 18 THEIRS/u);
    assert.equal(repo.git(["diff", "--cached", "--name-only"]).stdout, "");
    const remaining = repo.git(["diff", "--", "f.txt"]).stdout;
    assert.match(remaining, /\+line 18 THEIRS/u);
    assert.doesNotMatch(remaining, /MINE/u);
  });

  test("paths that do not match the patch are refused", () => {
    const repo = makeRepo({ "f.txt": "f\n", "g.txt": "g\n" });
    repo.write("f.txt", "f changed\n");
    writeFileSync(
      join(repo.dir, ".git/f.patch"),
      repo.git(["diff", "HEAD", "--", "f.txt"]).stdout,
    );
    const result = repo.commitPaths([
      "-m",
      "x",
      "--patch",
      ".git/f.patch",
      "--",
      "g.txt",
    ]);
    assert.equal(result.code, 1);
    assert.match(result.stderr, /patch touches \[f\.txt\]/u);
  });
});

describe("formatting of the committed content", () => {
  test("a demo TypeScript file with no prettier installed is 'could not check', distinct from unformatted", () => {
    const repo = makeRepo({ "demo/src/x.ts": "export const x = 1;\n" });
    repo.write("demo/src/x.ts", "export const x = 2;\n");
    const before = repo.head();

    const unchecked = repo.commitPaths(["-m", "Bump x", "--", "demo/src/x.ts"]);
    assert.equal(unchecked.code, 3, unchecked.stderr);
    assert.match(unchecked.stderr, /could not check formatting/u);
    assert.match(unchecked.stderr, /prettier is not installed/u);
    assert.equal(repo.head(), before);

    const allowed = repo.commitPaths([
      "--allow-unchecked",
      "-m",
      "Bump x",
      "--",
      "demo/src/x.ts",
    ]);
    assert.equal(allowed.code, 0, allowed.stderr);
    assert.match(allowed.stderr, /WITHOUT a formatting check/u);
  });

  test("prettier output that differs from the committed content is refused", () => {
    const repo = makeRepo({ "demo/src/x.ts": "export const x = 1;\n" });
    repo.write("demo/src/x.ts", "export const x = 2;\n");
    const identity = makeTool(
      repo,
      "prettier-identity",
      "process.stdin.pipe(process.stdout);",
    );
    const rewriting = makeTool(
      repo,
      "prettier-rewrite",
      "let s='';process.stdin.on('data',d=>s+=d).on('end',()=>process.stdout.write(s+'// reformatted\\n'));",
    );
    const before = repo.head();

    const bad = repo.commitPaths(["-m", "Bump x", "--", "demo/src/x.ts"], {
      env: { MIDGARD_PRETTIER_BIN: rewriting },
    });
    assert.equal(bad.code, 1, bad.stderr);
    assert.match(bad.stderr, /demo\/src\/x\.ts \(prettier\)/u);
    assert.equal(repo.head(), before);
    assert.equal(
      repo.read("demo/src/x.ts"),
      "export const x = 2;\n",
      "never rewritten",
    );

    const good = repo.commitPaths(["-m", "Bump x", "--", "demo/src/x.ts"], {
      env: { MIDGARD_PRETTIER_BIN: identity },
    });
    assert.equal(good.code, 0, good.stderr);
  });

  const realPrettier = join(SOURCE_REPO, "demo/node_modules/.bin/prettier");
  test(
    "demo's real prettier refuses unformatted TypeScript",
    {
      skip: existsSync(realPrettier)
        ? false
        : "demo/node_modules is not installed in this checkout",
    },
    () => {
      const repo = makeRepo({ "demo/src/x.ts": "export const x = 1;\n" });
      repo.write("demo/src/x.ts", "export const   x=2\n");
      const env = { MIDGARD_PRETTIER_BIN: realPrettier };
      const bad = repo.commitPaths(["-m", "Bump x", "--", "demo/src/x.ts"], {
        env,
      });
      assert.equal(bad.code, 1, bad.stderr);
      repo.write("demo/src/x.ts", "export const x = 2;\n");
      const good = repo.commitPaths(["-m", "Bump x", "--", "demo/src/x.ts"], {
        env,
      });
      assert.equal(good.code, 0, good.stderr);
    },
  );

  const aikenRepo = () => {
    const repo = makeRepo({ "onchain/aiken/lib/a.ak": "fn a() {\n  1\n}\n" });
    const pin = "  AIKEN_FORK_VERSION: aiken v0.0.0+pinned\n";
    repo.write(".github/workflows/aiken-ci.yml", `env:\n${pin}`);
    repo.write(".github/workflows/midgard-node-ci.yml", `env:\n${pin}`);
    mkdirSync(join(repo.dir, "onchain/aiken/scripts"), { recursive: true });
    copyFileSync(
      join(SOURCE_REPO, "onchain/aiken/scripts/pinned-compiler.mjs"),
      join(repo.dir, "onchain/aiken/scripts/pinned-compiler.mjs"),
    );
    repo.git(["add", "--", "."]);
    repo.git(["commit", "-q", "--no-verify", "-m", "Pin"]);
    // A stand-in compiler: --version reports FAKE_AIKEN_VERSION; `fmt --stdin`
    // echoes its input, adding the trailing spaces the real formatter emits
    // (FAKE_AIKEN_MODE=trailing) or a real change (FAKE_AIKEN_MODE=change).
    const aiken = makeTool(
      repo,
      "aiken",
      [
        "const [cmd, flag] = process.argv.slice(2);",
        "if (cmd === '--version') { console.log(process.env.FAKE_AIKEN_VERSION); process.exit(0); }",
        "if (cmd !== 'fmt' || flag !== '--stdin') process.exit(9);",
        "let s=''; process.stdin.on('data', d => s += d).on('end', () => {",
        "  if (process.env.FAKE_AIKEN_MODE === 'change') s += '// moved\\n';",
        "  else s = s.split('\\n').map(l => l.length ? l + '  ' : l).join('\\n');",
        "  process.stdout.write(s);",
        "});",
      ].join("\n"),
    );
    repo.write("onchain/aiken/lib/a.ak", "fn a() {\n  2\n}\n");
    return { repo, aiken };
  };

  test("an .ak file passes when only the formatter's trailing-space artifact differs", () => {
    const { repo, aiken } = aikenRepo();
    const result = repo.commitPaths(
      ["-m", "Two", "--", "onchain/aiken/lib/a.ak"],
      {
        env: {
          MIDGARD_AIKEN_BIN: aiken,
          FAKE_AIKEN_VERSION: "aiken v0.0.0+pinned",
          FAKE_AIKEN_MODE: "trailing",
        },
      },
    );
    assert.equal(result.code, 0, result.stderr);
  });

  test("an unformatted .ak file is refused", () => {
    const { repo, aiken } = aikenRepo();
    const result = repo.commitPaths(
      ["-m", "Two", "--", "onchain/aiken/lib/a.ak"],
      {
        env: {
          MIDGARD_AIKEN_BIN: aiken,
          FAKE_AIKEN_VERSION: "aiken v0.0.0+pinned",
          FAKE_AIKEN_MODE: "change",
        },
      },
    );
    assert.equal(result.code, 1, result.stderr);
    assert.match(result.stderr, /a\.ak \(aiken fmt, CI-normalized\)/u);
  });

  test("an aiken that is not the pinned fork is refused, not skipped", () => {
    const { repo, aiken } = aikenRepo();
    const result = repo.commitPaths(
      ["-m", "Two", "--", "onchain/aiken/lib/a.ak"],
      {
        env: {
          MIDGARD_AIKEN_BIN: aiken,
          FAKE_AIKEN_VERSION: "aiken v1.1.22",
        },
      },
    );
    assert.equal(result.code, 1, result.stderr);
    assert.match(result.stderr, /not the pinned fork/u);
  });

  test("a missing aiken is 'could not check'", () => {
    const { repo } = aikenRepo();
    const result = repo.commitPaths(
      ["-m", "Two", "--", "onchain/aiken/lib/a.ak"],
      {
        env: { MIDGARD_AIKEN_BIN: join(repo.dir, ".tools/no-such-aiken") },
      },
    );
    assert.equal(result.code, 3, result.stderr);
    assert.match(result.stderr, /could not run aiken/u);
  });
});

describe("concurrent commits", () => {
  test("a commit landing mid-run is never reverted: HEAD moves only from the base it read", () => {
    const repo = makeRepo({
      "demo/src/x.ts": "export const x = 1;\n",
      "other.txt": "other\n",
    });
    repo.write("demo/src/x.ts", "export const x = 2;\n");
    // Runs inside the formatting check, after the temporary index was built
    // from HEAD: another session commits other.txt in the meantime.
    const racing = makeTool(
      repo,
      "prettier-racing",
      [
        "const { execFileSync } = require('node:child_process');",
        "const fs = require('node:fs');",
        "const root = execFileSync('git', ['rev-parse', '--show-toplevel']).toString().trim();",
        "fs.writeFileSync(root + '/other.txt', 'changed by the other session\\n');",
        "execFileSync('git', ['commit', '-q', '--no-verify', '-m', 'Other session', '--', 'other.txt'], { cwd: root });",
        "process.stdin.pipe(process.stdout);",
      ].join("\n"),
    );

    const result = repo.commitPaths(["-m", "Bump x", "--", "demo/src/x.ts"], {
      env: { MIDGARD_PRETTIER_BIN: racing },
    });

    assert.equal(result.code, 1, result.stderr);
    assert.match(result.stderr, /HEAD moved away/u);
    assert.equal(
      repo.git(["log", "-1", "--format=%s"]).stdout.trim(),
      "Other session",
    );
    assert.equal(
      repo.git(["show", "HEAD:other.txt"]).stdout,
      "changed by the other session\n",
    );
    assert.equal(repo.read("demo/src/x.ts"), "export const x = 2;\n");
  });
});

describe("git hooks", () => {
  test("neither the repository pre-commit hook nor pre-commit.local runs", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    const hooks = join(repo.dir, ".git/hooks");
    copyFileSync(
      join(SOURCE_REPO, ".githooks/pre-commit"),
      join(hooks, "pre-commit"),
    );
    chmodSync(join(hooks, "pre-commit"), 0o755);
    // Stands in for the Nix shim; it only leaves a marker.
    writeFileSync(
      join(hooks, "pre-commit.local"),
      '#!/usr/bin/env bash\ntouch "$(git rev-parse --git-common-dir)/local-ran"\n',
    );
    chmodSync(join(hooks, "pre-commit.local"), 0o755);
    const marker = join(repo.dir, ".git/local-ran");

    // Control: a plain commit in the main checkout reaches pre-commit.local.
    repo.write("a.txt", "plain commit\n");
    repo.git(["add", "--", "a.txt"]);
    repo.git(["commit", "-q", "-m", "Plain"]);
    assert.ok(
      existsSync(marker),
      "control: the hook chain runs pre-commit.local",
    );
    rmSync(marker);

    // MIDGARD_SKIP_HOOKS=1 exits before the chain reaches pre-commit.local.
    repo.write("a.txt", "skip hooks\n");
    repo.git(["add", "--", "a.txt"]);
    repo.git(["commit", "-q", "-m", "Skipped"], {
      env: { MIDGARD_SKIP_HOOKS: "1" },
    });
    assert.ok(!existsSync(marker));

    // A linked worktree: the hook looks for <worktree>/.git/hooks/pre-commit.local,
    // and there .git is a file, so the shim never runs.
    const linked = join(repo.dir, ".git/linked-worktree");
    repo.git(["worktree", "add", "-q", "-b", "side", linked]);
    writeFileSync(join(linked, "a.txt"), "from the worktree\n");
    repo.git(["add", "--", "a.txt"], { cwd: linked });
    repo.git(["commit", "-q", "-m", "Worktree"], { cwd: linked });
    assert.ok(
      !existsSync(marker),
      "pre-commit.local is skipped in a linked worktree",
    );

    repo.write("a.txt", "via commit-paths\n");
    const result = repo.commitPaths(["-m", "Via script", "--", "a.txt"]);
    assert.equal(result.code, 0, result.stderr);
    assert.ok(!existsSync(marker), "commit-paths runs no hooks");
  });
});

describe("negative self-tests: the assertions above can fail", () => {
  test("a sweeping commit is caught by the same check that passes for commit-paths", () => {
    const repo = makeRepo({ "a.txt": "a\n", "b.txt": "b\n" });
    repo.write("a.txt", "a mine\n");
    repo.write("b.txt", "b theirs\n");
    repo.git(["add", "--", "b.txt"]);
    repo.git(["commit", "-q", "--no-verify", "-a", "-m", "Sweep"]);
    assert.notDeepEqual(
      repo.committedPaths(),
      ["a.txt"],
      "git commit -a swept the foreign staged file",
    );
    assert.deepEqual(repo.committedPaths(), ["a.txt", "b.txt"]);
  });

  test("a temporary-index commit without the resync leaves the path reverse-staged", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    repo.write("a.txt", "a changed\n");
    const index = join(repo.dir, ".git/tmp-index");
    const env = { GIT_INDEX_FILE: index };
    repo.git(["read-tree", "HEAD"], { env });
    repo.git(["add", "--", "a.txt"], { env });
    repo.git(["commit", "-q", "--no-verify", "-m", "No resync"], { env });
    assert.notEqual(
      repo.status(),
      "",
      "the status check detects a missing resync",
    );
    assert.equal(
      repo.git(["diff", "--cached", "--name-only"]).stdout.trim(),
      "a.txt",
    );
  });

  test("the script's own refusal path is reachable: a no-op commit fails", () => {
    const repo = makeRepo({ "a.txt": "a\n" });
    const result = repo.commitPaths(["-m", "Nothing changed", "--", "a.txt"]);
    assert.notEqual(result.code, 0);
  });
});
