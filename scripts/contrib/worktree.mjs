import { execFileSync } from "node:child_process";
import { existsSync, realpathSync } from "node:fs";
import { basename, dirname, resolve } from "node:path";

import { listCheckouts } from "./blueprint.mjs";
import { runDirectory } from "./build.mjs";
import { checkoutDatabasePrefixes, dropTestDatabases } from "./databases.mjs";
import { workspacePackages } from "./files.mjs";
import { runProcess } from "./process.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import { withResource } from "./resources.mjs";
import { prepareArtifacts } from "./tests.mjs";

// One way to start and finish a lane's checkout. `create` adds a linked
// worktree and sets it up; `setup` installs the workspace, readies the
// blueprint and builds every prerequisite dist the suites need; `remove`
// drops the checkout's own test databases and removes it, refusing work that
// exists nowhere else.

// A hook exports GIT_DIR and friends; with them, `git -C <other checkout>`
// would act on the hook's repository instead.
const gitEnv = (env) =>
  Object.fromEntries(
    Object.entries(env).filter(([key]) => !key.startsWith("GIT_")),
  );

const git = (cwd, args, env = process.env) =>
  execFileSync("git", args, {
    cwd,
    env: gitEnv(env),
    encoding: "utf8",
  }).trim();

/**
 * Where new worktrees go: MIDGARD_WORKTREE_ROOT, else `git config
 * midgard.worktreeRoot`, else beside the main checkout.
 */
export const worktreeRoot = (root, env = process.env) => {
  if (env.MIDGARD_WORKTREE_ROOT) return resolve(env.MIDGARD_WORKTREE_ROOT);
  let configured = "";
  try {
    configured = git(root, ["config", "--get", "midgard.worktreeRoot"], env);
  } catch {
    // Unset: `git config --get` exits 1.
  }
  return configured
    ? resolve(root, configured)
    : dirname(listCheckouts(root)[0]);
};

/** Every workspace package with a test script. */
const testedPackages = (root) =>
  workspacePackages(root)
    .filter((pkg) => pkg.scripts?.test)
    .map((pkg) => pkg.name);

export const setupWorktree = async (
  root,
  { packageName, signal, env = process.env } = {},
) =>
  withResource(
    `workspace:${realpathSync(root)}`,
    async (ownedEnv) => {
      const install = await runProcess({
        ...pinnedPnpm(resolve(root, "demo"), ["install", "--frozen-lockfile"]),
        env: ownedEnv,
        signal,
        logPath: resolve(runDirectory(), "install.log"),
      });
      if (install.exitCode !== 0)
        throw new Error(
          `pnpm install --frozen-lockfile failed; log: ${install.logPath}`,
        );
      const prepared = await prepareArtifacts(
        root,
        packageName ? [packageName] : testedPackages(root),
        { signal, env: ownedEnv },
      );
      return { schema: "midgard-worktree-setup/v1", root, ...prepared };
    },
    { signal, env },
  );

export const createWorktree = async (
  root,
  {
    branch,
    base,
    packageName,
    signal,
    env = process.env,
    setup = setupWorktree,
  },
) => {
  if (!branch) throw new Error("--branch is required");
  const path = resolve(worktreeRoot(root, env), `midgard-${basename(branch)}`);
  if (existsSync(path)) throw new Error(`${path} already exists`);
  let exists = true;
  try {
    git(
      root,
      ["rev-parse", "--verify", "--quiet", `refs/heads/${branch}`],
      env,
    );
  } catch {
    exists = false;
  }
  if (exists && base)
    throw new Error(`branch ${branch} already exists; drop --base to use it`);
  git(
    root,
    exists
      ? ["worktree", "add", path, branch]
      : ["worktree", "add", "-b", branch, path, base ?? "HEAD"],
    env,
  );
  return { path, ...(await setup(path, { packageName, signal, env })) };
};

/**
 * Why removing `target` would lose work, or an empty list. Uncommitted work
 * is any tracked change or untracked file except the blueprint contrib itself
 * places; unshared work is any commit on HEAD that no other local branch and
 * no remote-tracking branch contains (removing a detached worktree orphans
 * them; on a branch they survive only on that one local ref).
 */
export const unsavedWork = (target, env = process.env) => {
  const reasons = [];
  const changes = git(
    target,
    ["status", "--porcelain=v1", "--untracked-files=all"],
    env,
  )
    .split("\n")
    .filter((line) => line && line !== "?? onchain/aiken/plutus.json");
  if (changes.length)
    reasons.push(
      `${changes.length} uncommitted change(s), first: ${changes[0].trim()}`,
    );
  let branch = "";
  try {
    branch = git(target, ["symbolic-ref", "--quiet", "--short", "HEAD"], env);
  } catch {
    // Detached HEAD.
  }
  const unshared = Number(
    git(
      target,
      [
        "rev-list",
        "--count",
        "HEAD",
        "--not",
        ...(branch ? [`--exclude=${branch}`] : []),
        "--branches",
        "--remotes",
      ],
      env,
    ),
  );
  if (unshared)
    reasons.push(
      `${unshared} commit(s) on ${branch || "a detached HEAD"} are on no other branch and no remote; push or merge them`,
    );
  return reasons;
};

export const removeWorktree = async (
  root,
  { force = false, env = process.env, drop = dropTestDatabases } = {},
) => {
  const target = realpathSync(root);
  const checkouts = listCheckouts(target).map((path) =>
    existsSync(path) ? realpathSync(path) : path,
  );
  if (checkouts[0] === target)
    throw new Error(
      `${target} is the main checkout; only worktrees are removed`,
    );
  if (!checkouts.includes(target))
    throw new Error(`${target} is not a worktree of this repository`);
  const reasons = unsavedWork(target, env);
  if (reasons.length && !force)
    throw new Error(
      `refusing to remove ${target}: ${reasons.join("; ")}; or pass --force`,
    );
  const databases = await drop(target, checkoutDatabasePrefixes(target), {
    env,
  });
  // Our own check above is the stronger one; git's would also refuse the
  // untracked blueprint.
  git(checkouts[0], ["worktree", "remove", "--force", target], env);
  return {
    schema: "midgard-worktree-remove/v1",
    removed: target,
    ...(reasons.length ? { forced: reasons } : {}),
    databases,
  };
};
