// Exercises .githooks/{install,pre-commit,post-commit} in a throwaway
// repository with its own git config, never this repository's. Proves that
// `install` makes every checkout run its own copy of the hooks, that the Nix
// pre-commit shim runs only where its config exists, and that the Graphify
// post-commit refresh is a no-op unless opted in.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  chmodSync,
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  realpathSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const hooksSource = join(repositoryRoot, ".githooks");

const hasCommand = (name) =>
  spawnSync("sh", ["-c", `command -v ${name}`]).status === 0;

// A git environment that cannot see the developer's global or system config,
// or any GIT_* variables inherited from a hook this test runs under.
const sandbox = () => {
  const base = realpathSync(mkdtempSync(join(tmpdir(), "midgard-githooks-")));
  const home = join(base, "home");
  const bin = join(base, "bin");
  mkdirSync(home);
  mkdirSync(bin);
  const globalConfig = join(base, "gitconfig");
  writeFileSync(
    globalConfig,
    "[user]\n\tname = Hook Test\n\temail = hooks@example.invalid\n[init]\n\tdefaultBranch = main\n",
  );
  const env = Object.fromEntries(
    Object.entries(process.env).filter(([key]) => !key.startsWith("GIT_")),
  );
  Object.assign(env, {
    HOME: home,
    PATH: `${bin}:${process.env.PATH}`,
    GIT_CONFIG_GLOBAL: globalConfig,
    GIT_CONFIG_NOSYSTEM: "1",
  });
  delete env.MIDGARD_SKIP_HOOKS;
  delete env.MIDGARD_GRAPHIFY_SKIP_POST_COMMIT;
  delete env.MIDGARD_GRAPHIFY_STATE_ROOT;
  const run = (cwd, command, args, extraEnv = {}) =>
    spawnSync(command, args, {
      cwd,
      env: { ...env, ...extraEnv },
      encoding: "utf8",
    });
  const git = (cwd, ...args) => {
    const result = run(cwd, "git", args);
    assert.equal(result.status, 0, `git ${args.join(" ")}: ${result.stderr}`);
    return result.stdout.trim();
  };
  return { base, bin, run, git };
};

// A repository whose .githooks are copies of this repository's, deliberately
// NOT executable, with a Nix-style shim already installed as the common
// pre-commit hook. The shim records the directory it ran in.
const repositoryWithHooks = ({ base, git }) => {
  const main = join(base, "main");
  mkdirSync(join(main, ".githooks"), { recursive: true });
  git(main, "init", "-q");
  for (const name of readdirSync(hooksSource)) {
    copyFileSync(join(hooksSource, name), join(main, ".githooks", name));
    chmodSync(join(main, ".githooks", name), 0o644);
  }
  const shimLog = join(base, "shim.log");
  writeFileSync(
    join(main, ".git/hooks/pre-commit"),
    `#!/bin/sh\npwd >> "${shimLog}"\n`,
    { mode: 0o755 },
  );
  return { main, shimLog };
};

const commit = (context, cwd, name, extraEnv = {}) => {
  writeFileSync(join(cwd, name), `${name}\n`);
  context.git(cwd, "add", "--", name);
  return context.run(
    cwd,
    "git",
    ["commit", "-q", "-m", `add ${name}`],
    extraEnv,
  );
};

test("install points every checkout at its own .githooks copy", () => {
  const context = sandbox();
  try {
    const { main, shimLog } = repositoryWithHooks(context);
    const installed = context.run(main, "bash", [".githooks/install"]);
    assert.equal(installed.status, 0, installed.stderr);

    assert.equal(context.git(main, "config", "core.hooksPath"), ".githooks");
    for (const name of readdirSync(join(main, ".githooks"))) {
      assert.ok(
        statSync(join(main, ".githooks", name)).mode & 0o100,
        `${name} not executable`,
      );
    }
    assert.ok(existsSync(join(main, ".git/hooks/pre-commit.local")));
    assert.ok(!existsSync(join(main, ".git/hooks/pre-commit")));
    // Track the hooks, so a worktree checked out later has its own copy.
    context.git(main, "add", ".githooks");
    context.git(main, "commit", "-q", "--no-verify", "-m", "hooks");

    // Running it twice changes nothing and keeps the shim.
    const again = context.run(main, "bash", [".githooks/install"]);
    assert.equal(again.status, 0, again.stderr);
    assert.ok(existsSync(join(main, ".git/hooks/pre-commit.local")));

    // The main checkout has the Nix config, so the shim runs there.
    writeFileSync(join(main, ".pre-commit-config.yaml"), "repos: []\n");
    const first = commit(context, main, "one.txt");
    assert.equal(first.status, 0, first.stderr);
    assert.deepEqual(readFileSync(shimLog, "utf8").trim().split("\n"), [main]);

    // A linked worktree has no config: the shim is skipped, visibly, and the
    // commit still succeeds.
    const linked = join(context.base, "linked");
    context.git(main, "worktree", "add", "-q", "-b", "side", linked);
    const second = commit(context, linked, "two.txt");
    assert.equal(second.status, 0, second.stderr);
    assert.match(second.stderr, /pre-commit\.local .* skipped/u);
    assert.deepEqual(readFileSync(shimLog, "utf8").trim().split("\n"), [main]);

    // The linked worktree runs ITS copy: change it there and only there.
    writeFileSync(
      join(linked, ".githooks/pre-commit"),
      "#!/bin/sh\necho LINKED-COPY >&2\n",
      { mode: 0o755 },
    );
    const third = commit(context, linked, "three.txt");
    assert.match(third.stderr, /LINKED-COPY/u);
    const fourth = commit(context, main, "four.txt");
    assert.equal(fourth.status, 0, fourth.stderr);
    assert.doesNotMatch(fourth.stderr, /LINKED-COPY/u);
  } finally {
    rmSync(context.base, { recursive: true, force: true });
  }
});

test("the graph refresh is a no-op unless opted in", async (t) => {
  const context = sandbox();
  try {
    const { main } = repositoryWithHooks(context);
    const installed = context.run(main, "bash", [".githooks/install"]);
    assert.equal(installed.status, 0, installed.stderr);
    // Stands in for graphify: records the call and creates the output
    // directory the real extractor would.
    const graphifyLog = join(context.base, "graphify.log");
    writeFileSync(
      join(context.bin, "graphify"),
      `#!/bin/sh\necho "$@" >> "${graphifyLog}"\nmkdir -p "$MIDGARD_GRAPHIFY_STATE_ROOT/graphify-out"\n`,
      { mode: 0o755 },
    );

    // No state directory under HOME and no opt-in variable: nothing at all.
    const plain = commit(context, main, "one.txt");
    assert.equal(plain.status, 0, plain.stderr);
    assert.doesNotMatch(plain.stderr, /graphify/u);
    assert.ok(!existsSync(graphifyLog));

    if (!hasCommand("setsid") || !hasCommand("flock")) {
      t.skip("could not check the opted-in refresh: setsid or flock missing");
      return;
    }
    const stateRoot = join(context.base, "graph-state");
    const optedIn = commit(context, main, "two.txt", {
      MIDGARD_GRAPHIFY_STATE_ROOT: stateRoot,
    });
    assert.equal(optedIn.status, 0, optedIn.stderr);
    assert.match(optedIn.stderr, /refresh dispatched/u);
    const stamp = join(stateRoot, "graphify-out/indexed-commit");
    for (let attempt = 0; attempt < 100 && !existsSync(stamp); attempt += 1) {
      await new Promise((settle) => setTimeout(settle, 50));
    }
    assert.equal(
      readFileSync(stamp, "utf8").trim(),
      context.git(main, "rev-parse", "HEAD"),
    );
  } finally {
    rmSync(context.base, { recursive: true, force: true });
  }
});
