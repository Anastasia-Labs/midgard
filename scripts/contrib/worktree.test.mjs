import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import { validateTestDatabasePrefix } from "../../demo/midgard-test-support/database-identity.js";
import { worktreeIdentity } from "../lib/worktree-identity.mjs";
import {
  checkoutDatabasePrefixes,
  dropTestDatabases,
  invocationDatabasePrefix,
} from "./databases.mjs";
import {
  createWorktree,
  removeWorktree,
  unsavedWork,
  worktreeRoot,
} from "./worktree.mjs";

// A pre-push hook exports GIT_DIR; `git init` under it would re-initialise
// the outer repository (see scripts/tooling-git-isolation.test.mjs).
const env = Object.fromEntries(
  Object.entries(process.env).filter(([key]) => !key.startsWith("GIT_")),
);
const git = (cwd, ...args) =>
  execFileSync("git", args, { cwd, env, encoding: "utf8" }).trim();
const commit = (cwd, file, message) => {
  writeFileSync(join(cwd, file), message);
  git(cwd, "add", file);
  git(
    cwd,
    "-c",
    "user.name=Fixture",
    "-c",
    "user.email=fixture@example.invalid",
    "-c",
    "core.hooksPath=/dev/null",
    "commit",
    "-qm",
    message,
  );
};

// A main checkout with one commit, inside a parent that also holds worktrees.
const repository = (t) => {
  const parent = realpathSync(mkdtempSync(join(tmpdir(), "midgard-worktree-")));
  t.after(() => rmSync(parent, { recursive: true, force: true }));
  const main = join(parent, "main");
  mkdirSync(main);
  git(main, "init", "-q", "-b", "trunk");
  commit(main, "README", "base");
  const add = (name, ...args) => {
    const path = join(parent, name);
    git(main, "worktree", "add", "-q", ...args, path);
    return path;
  };
  return { parent, main, add };
};

const noDrop = async () => ({ status: "unavailable" });

test("the worktree root comes from the environment, then git config, then beside the main checkout", (t) => {
  const { parent, main } = repository(t);
  assert.equal(worktreeRoot(main, env), parent);
  git(main, "config", "midgard.worktreeRoot", "/srv/lanes");
  assert.equal(worktreeRoot(main, env), "/srv/lanes");
  assert.equal(
    worktreeRoot(main, { ...env, MIDGARD_WORKTREE_ROOT: "/elsewhere" }),
    "/elsewhere",
  );
});

test("create adds a branch worktree under the root and runs setup there", async (t) => {
  const { parent, main } = repository(t);
  const calls = [];
  const result = await createWorktree(main, {
    branch: "lane/tooling",
    env,
    setup: async (path, options) => {
      calls.push([path, options.packageName]);
      return { ready: true };
    },
    packageName: "midgard-node",
  });
  const path = join(parent, "midgard-tooling");
  assert.deepEqual(calls, [[path, "midgard-node"]]);
  assert.equal(result.path, path);
  assert.equal(git(path, "symbolic-ref", "--short", "HEAD"), "lane/tooling");
  await assert.rejects(
    createWorktree(main, { branch: "lane/tooling", env, setup: noDrop }),
    /already exists/u,
  );
});

test("remove refuses the main checkout", async (t) => {
  const { main } = repository(t);
  await assert.rejects(
    removeWorktree(main, { env, drop: noDrop }),
    /is the main checkout/u,
  );
});

test("uncommitted work blocks removal; the blueprint contrib placed does not", (t) => {
  const { add } = repository(t);
  const lane = add("lane", "-b", "lane");
  git(lane, "branch", "kept"); // its commits are shared
  mkdirSync(join(lane, "onchain/aiken"), { recursive: true });
  writeFileSync(join(lane, "onchain/aiken/plutus.json"), "{}");
  assert.deepEqual(unsavedWork(lane, env), []);
  writeFileSync(join(lane, "notes.txt"), "draft");
  assert.match(
    unsavedWork(lane, env).join(),
    /uncommitted change.*notes\.txt/u,
  );
  rmSync(join(lane, "notes.txt"));
  writeFileSync(join(lane, "README"), "edited");
  assert.match(unsavedWork(lane, env).join(), /uncommitted change.*README/u);
});

test("commits on no other branch or remote block removal until merged elsewhere", (t) => {
  const { main, add } = repository(t);
  const lane = add("lane", "-b", "lane");
  commit(lane, "work", "lane work");
  assert.match(unsavedWork(lane, env).join(), /1 commit\(s\) on lane/u);
  git(main, "merge", "-q", "--ff-only", "lane");
  assert.deepEqual(unsavedWork(lane, env), []);
});

test("a detached worktree's own commits block removal", (t) => {
  const { add } = repository(t);
  const lane = add("lane", "--detach");
  commit(lane, "work", "detached work");
  assert.match(unsavedWork(lane, env).join(), /on a detached HEAD/u);
});

test("remove drops only the worktree's hashed prefixes, then removes it; --force overrides", async (t) => {
  const { main, add } = repository(t);
  const lane = add("lane", "-b", "lane");
  commit(lane, "work", "lane work");
  const { hash } = worktreeIdentity(lane);
  const dropped = [];
  const drop = async (root, prefixes) => {
    dropped.push(...prefixes);
    return { status: "dropped", databases: [], schemas: [] };
  };
  await assert.rejects(
    removeWorktree(lane, { env, drop }),
    /refusing to remove .*1 commit/u,
  );
  assert.equal(existsSync(lane), true);
  assert.deepEqual(dropped, []);
  const result = await removeWorktree(lane, { env, drop, force: true });
  assert.equal(existsSync(lane), false);
  assert.match(result.forced.join(), /1 commit/u);
  assert.deepEqual(dropped, [
    `midgard_test_${hash}`,
    `midgard_tools_test_${hash}`,
    `midgard_contrib_${hash}`,
  ]);
  assert.match(
    git(main, "branch", "--list", "lane"),
    /lane/u,
    "the branch is kept",
  );
});

// A stand-in for pg.Client that serves a fixed catalogue and records queries.
const fakeClient = (names, { refuse = false } = {}) => {
  const queries = [];
  return {
    queries,
    connect: async () => {
      if (refuse)
        throw Object.assign(new Error("connect ECONNREFUSED"), {
          code: "ECONNREFUSED",
        });
    },
    query: async (sql) => {
      queries.push(sql);
      return sql.startsWith("SELECT datname")
        ? { rows: names.databases.map((name) => ({ name })) }
        : sql.startsWith("SELECT nspname")
          ? { rows: names.schemas.map((name) => ({ name })) }
          : { rows: [] };
    },
    end: async () => {},
  };
};

test("database cleanup drops names under the prefix boundary and nothing else", async () => {
  const client = fakeClient({
    databases: [
      "midgard_test_0123abcd_w1",
      "midgard_test_0123abcd",
      "midgard_test_0123abcde_w1",
      "midgard_test_w1",
      "postgres",
    ],
    schemas: ["midgard_contrib_0123abcd_f00d_watcher", "public"],
  });
  const result = await dropTestDatabases(
    "/unused",
    ["midgard_test_0123abcd", "midgard_contrib_0123abcd"],
    { env: {}, client },
  );
  assert.deepEqual(result.databases, [
    "midgard_test_0123abcd_w1",
    "midgard_test_0123abcd",
  ]);
  assert.deepEqual(result.schemas, ["midgard_contrib_0123abcd_f00d_watcher"]);
  assert.deepEqual(client.queries.slice(2), [
    'DROP DATABASE IF EXISTS "midgard_test_0123abcd_w1" WITH (FORCE)',
    'DROP DATABASE IF EXISTS "midgard_test_0123abcd" WITH (FORCE)',
    'DROP SCHEMA IF EXISTS "midgard_contrib_0123abcd_f00d_watcher" CASCADE',
  ]);
});

test("database cleanup refuses a prefix that is not scoped to one checkout", async () => {
  for (const prefix of ["midgard_test", "midgard_contrib", "postgres"])
    await assert.rejects(
      dropTestDatabases("/unused", [prefix], {
        env: {},
        client: fakeClient({ databases: [], schemas: [] }),
      }),
      /without a checkout hash/u,
    );
});

test("database cleanup reports a stopped server instead of failing", async () => {
  const result = await dropTestDatabases("/unused", ["midgard_test_0123abcd"], {
    env: {},
    client: fakeClient({}, { refuse: true }),
  });
  assert.equal(result.status, "unavailable");
});

test("contrib's prefixes are ones the suites accept and teardown drops", (t) => {
  const { main } = repository(t);
  const prefix = invocationDatabasePrefix(main, "0f1e2d3c");
  assert.equal(validateTestDatabasePrefix(prefix), prefix);
  assert.ok(
    checkoutDatabasePrefixes(main).some((owner) =>
      prefix.startsWith(`${owner}_`),
    ),
  );
});
