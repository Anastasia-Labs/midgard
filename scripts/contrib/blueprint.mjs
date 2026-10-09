import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, realpathSync, statSync } from "node:fs";
import { resolve } from "node:path";

import {
  blueprintSourceHash,
  buildRecordPath,
  checkBlueprintStamp,
} from "../../demo/scripts/lib/blueprint-stamp.mjs";
import { pinnedAikenVersion } from "../../onchain/aiken/scripts/pinned-compiler.mjs";
import {
  main as syncBlueprintFrom,
  profileMismatch,
  selectedProfile,
} from "../sync-blueprint-from.mjs";
import { runDirectory } from "./build.mjs";
import { runProcess } from "./process.mjs";
import { pinnedPnpm } from "./pnpm.mjs";
import { withResource } from "./resources.mjs";

// One answer to "this checkout needs a blueprint": nobody decides between
// copying and building. A blueprint is ready when its stamp is fresh (same
// sources, pinned compiler, untouched since its record) AND it was built for
// the profile this checkout selects. When it is not, copy a ready one from
// another checkout of the repository (scripts/sync-blueprint-from.mjs, which
// re-verifies everything after the copy), and build only when none is.

const blueprintOf = (root) => resolve(root, "onchain/aiken/plutus.json");

/** `{ ready: true }`, or `{ ready: false, reason }` saying why not. */
export const blueprintReadiness = (root) => {
  const stamp = checkBlueprintStamp({ root });
  if (stamp.status !== "fresh") return { ready: false, reason: stamp.detail };
  const mismatch = profileMismatch(buildRecordPath(blueprintOf(root)), root);
  return mismatch === null
    ? { ready: true }
    : { ready: false, reason: mismatch };
};

/** Every checkout of this repository, the main one first, as git lists them. */
export const listCheckouts = (root) =>
  execFileSync("git", ["worktree", "list", "--porcelain"], {
    cwd: root,
    encoding: "utf8",
  })
    .split("\n")
    .filter((line) => line.startsWith("worktree "))
    .map((line) => line.slice("worktree ".length));

/**
 * Checkouts whose build record already names this tree's sources, compiler
 * and profile, newest build first after the main checkout. Only a cheap
 * pre-filter: the copy itself re-verifies every input.
 */
export const syncCandidates = (root, checkouts = listCheckouts(root)) => {
  const here = realpathSync(root);
  const wanted = {
    sourceHash: blueprintSourceHash(root),
    compiler: pinnedAikenVersion(root),
    profile: selectedProfile(root).name,
  };
  const matching = checkouts.flatMap((checkout, index) => {
    if (!existsSync(checkout) || realpathSync(checkout) === here) return [];
    const record = buildRecordPath(blueprintOf(checkout));
    try {
      const built = JSON.parse(readFileSync(record, "utf8"));
      if (
        built.sourceHash !== wanted.sourceHash ||
        built.compiler !== wanted.compiler ||
        built.profile?.name !== wanted.profile
      )
        return [];
      return [{ checkout, main: index === 0, mtime: statSync(record).mtimeMs }];
    } catch {
      return [];
    }
  });
  return matching
    .sort((a, b) => Number(b.main) - Number(a.main) || b.mtime - a.mtime)
    .map(({ checkout }) => checkout);
};

const deploymentBuild = async (root, profile, { signal, env }) => {
  const logPath = resolve(runDirectory(), "deployment-build.log");
  const step = await withResource(
    "memory-heavy-build",
    (ownedEnv) =>
      runProcess({
        ...pinnedPnpm(resolve(root, "demo"), ["deployment:build", profile]),
        env: ownedEnv,
        signal,
        logPath,
      }),
    { signal, env },
  );
  return { exitCode: step.exitCode, logPath };
};

/**
 * Make this checkout's blueprint ready: leave a ready one alone, else copy
 * one from another checkout, else build it. Returns what was done; throws
 * when even the build leaves it unready.
 */
export const ensureBlueprint = async (
  root,
  {
    signal,
    env = process.env,
    checkouts,
    sync = syncBlueprintFrom,
    build = deploymentBuild,
  } = {},
) => {
  const before = blueprintReadiness(root);
  if (before.ready) return { action: "none" };
  const refused = [];
  for (const source of syncCandidates(root, checkouts)) {
    const messages = [];
    const record = (text) => messages.push(text.trim());
    if (sync([source], { root, stdout: record, stderr: record }) === 0)
      return { action: "copied", from: source, stale: before.reason };
    refused.push({ source, detail: messages.join(" ") });
  }
  const profile = selectedProfile(root).name;
  const built = await build(root, profile, { signal, env });
  const after = blueprintReadiness(root);
  if (built.exitCode !== 0 || !after.ready)
    throw new Error(
      `pnpm --dir demo deployment:build ${profile} did not leave a ready blueprint` +
        (built.exitCode === 0
          ? `: ${after.reason}`
          : ` (exit ${built.exitCode})`) +
        `; log: ${built.logPath}`,
    );
  return {
    action: "built",
    profile,
    stale: before.reason,
    logPath: built.logPath,
    ...(refused.length ? { refused } : {}),
  };
};
