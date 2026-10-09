#!/usr/bin/env node

// Fails when a hand-kept registry names a file that no longer exists.
//
// A deletion or rename that misses a registry leaves it pointing at nothing,
// and the failure comes late or never: a contrib gate's "missing owner" only
// when that gate runs (it once blocked a push), a preflight trigger or a
// traced-refusal input that silently never matches again, a duration table
// that keeps sharding by a file CI no longer runs. This is the one check for
// all of them; preflight runs it on every push and Repo Tools CI on every
// pull request.
//
// Each registry lists entries of two kinds: a path (a file, or a directory
// holding at least one file) or a glob in preflight's dialect (derive.mjs),
// which must match at least one file. Files are those git tracks or would
// add (untracked, not ignored) that exist in the working tree.
//
// Registries not listed here, and why:
// - demo/module-size-exceptions.json: its own validator
//   (demo/scripts/check-module-size-exceptions.mjs) reads every listed file to
//   compare its cap, so a deleted file already fails it.
// - docs/module-size-refactor/{extractions,verification,metrics,
//   preflight-results}.json and docs/exec-plans/**: dated records of a past
//   run at a named commit, not lists anything acts on now.
// - the scenario titles in the validator scenario registry: its own Vitest
//   test checks them; this check covers only the files, which it can read
//   without a blueprint.
//
// Usage: node scripts/ci/check-registry-paths.mjs
// Exit 0 when every entry names something, 1 when one does not.

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, statSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { GATES } from "../contrib/gates.mjs";
import { packageByName } from "../contrib/files.mjs";
import {
  buildRegistry,
  FULL_RUN,
  VERIFICATION_ONLY,
} from "../preflight/registry.mjs";
import { globToRegExp } from "../preflight/derive.mjs";
import { tracedRefusalInputs } from "./traced-refusal-inputs.mjs";

const json = (root, path) =>
  JSON.parse(readFileSync(resolve(root, path), "utf8"));

const isGlob = (pattern) => /[*?{]/u.test(pattern);
const entry = (pattern, label = pattern) =>
  isGlob(pattern)
    ? { glob: pattern, label }
    : { path: pattern.replace(/\/$/u, ""), label };

// The files every glob and directory is judged against: what git tracks or
// would add, as the working tree has it. A hook's GIT_* would point git at
// another index.
export const repositoryFiles = (root) => {
  const run = spawnSync(
    "git",
    ["ls-files", "-z", "--cached", "--others", "--exclude-standard"],
    {
      cwd: root,
      encoding: "utf8",
      maxBuffer: 64 * 1024 * 1024,
      env: Object.fromEntries(
        Object.entries(process.env).filter(([key]) => !key.startsWith("GIT_")),
      ),
    },
  );
  if (run.status !== 0)
    throw new Error(`git ls-files failed: ${(run.stderr ?? "").trim()}`);
  return [...new Set(run.stdout.split("\0").filter(Boolean))].filter((path) =>
    existsSync(resolve(root, path)),
  );
};

const SCENARIO_REGISTRY =
  /^demo\/midgard-fault-proofs\/tests\/support\/validator-scenario-registry(?:\.[\w-]+)?\.ts$/u;
const DURATION_TABLE =
  /^demo\/[^/]+\/tests\/support\/ci-file-durations\.json$/u;

/**
 * The registries, each with the file that holds it and its entries. An
 * entry's label is what the registry says, for the report.
 */
export const REGISTRIES = [
  {
    name: "contrib gates",
    source: "scripts/contrib/gates.mjs",
    entries: (root) =>
      Object.entries(GATES).flatMap(([gate, lanes]) =>
        lanes.flatMap(([name, patterns]) => {
          const directory = packageByName(root, name).directory;
          return patterns.map((pattern) =>
            entry(`${directory}/${pattern}`, `${gate}: ${name} ${pattern}`),
          );
        }),
      ),
  },
  {
    name: "preflight triggers",
    source: "scripts/preflight/registry.mjs",
    entries: (root) => [
      ...buildRegistry(root).checks.flatMap((check) =>
        (check.triggers ?? []).map((trigger) =>
          entry(trigger, `${check.id}: ${trigger}`),
        ),
      ),
      ...FULL_RUN.map((path) => entry(path, `FULL_RUN: ${path}`)),
      ...VERIFICATION_ONLY.map((path) =>
        entry(path, `VERIFICATION_ONLY: ${path}`),
      ),
    ],
  },
  {
    name: "traced-refusal inputs",
    source: "scripts/ci/traced-refusal-inputs.mjs",
    entries: () => tracedRefusalInputs.map((path) => entry(path)),
  },
  {
    name: "retained modules",
    source: "docs/module-size-refactor/retained-modules.json",
    entries: (root) =>
      json(root, "docs/module-size-refactor/retained-modules.json").modules.map(
        ({ file }) => entry(file),
      ),
  },
  {
    name: "CI file durations",
    sources: DURATION_TABLE,
    entries: (root, source) => {
      const directory = source.replace(/\/tests\/support\/[^/]+$/u, "");
      return Object.keys(json(root, source).files).map((file) =>
        entry(`${directory}/${file}`, file),
      );
    },
  },
  {
    name: "validator scenario registry",
    sources: SCENARIO_REGISTRY,
    // The registry is TypeScript; each scenario names its file as a string
    // literal, `file: "<repository path>"`.
    entries: (root, source) =>
      [
        ...readFileSync(resolve(root, source), "utf8").matchAll(
          /\bfile:\s*"([^"]+)"/gu,
        ),
      ].map(([, file]) => entry(file)),
  },
];

/**
 * Every registry entry that names nothing, as
 * `{ registry, source, label, reason }`. `files` is the repository's file
 * list (see repositoryFiles).
 */
export const deadEntries = (
  root,
  { registries = REGISTRIES, files = repositoryFiles(root) } = {},
) => {
  const present = new Set(files);
  const directories = new Set(
    files.flatMap((file) =>
      file
        .split("/")
        .slice(0, -1)
        .map((_, index, parts) => parts.slice(0, index + 1).join("/")),
    ),
  );
  const dead = [];
  const counts = {};
  for (const registry of registries) {
    const sources = registry.sources
      ? files.filter((file) => registry.sources.test(file))
      : [registry.source];
    counts[registry.name] = 0;
    if (sources.length === 0 || !sources.every((s) => present.has(s))) {
      dead.push({
        registry: registry.name,
        source: registry.source ?? String(registry.sources),
        label: "(the registry itself)",
        reason: "the file that holds it is gone; update this check",
      });
      continue;
    }
    for (const source of sources) {
      for (const item of registry.entries(root, source)) {
        counts[registry.name] += 1;
        const found = item.glob
          ? files.some((file) => globToRegExp(item.glob).test(file))
          : present.has(item.path) || directories.has(item.path);
        if (!found)
          dead.push({
            registry: registry.name,
            source,
            label: item.label,
            reason: item.glob ? "matches no file" : "does not exist",
          });
      }
    }
  }
  return { dead, counts };
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  const root = fileURLToPath(new URL("../..", import.meta.url));
  const { dead, counts } = deadEntries(root);
  for (const item of dead)
    console.error(
      `${item.source}: ${item.label}: ${item.reason}; remove the entry or point it at the file's new path`,
    );
  const total = Object.values(counts).reduce((sum, count) => sum + count, 0);
  console.log(
    dead.length === 0
      ? `check-registry-paths: ${String(total)} entries in ${String(Object.keys(counts).length)} registries all name existing files`
      : `check-registry-paths: ${String(dead.length)} of ${String(total)} entries name nothing`,
  );
  process.exitCode = dead.length === 0 ? 0 : 1;
}
