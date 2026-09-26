#!/usr/bin/env node

// Whether `onchain/aiken/plutus.json` still describes the tree it sits in.
//
// The blueprint is untracked build output. Nothing rebuilds it when a
// validator, an env module, or the compiler changes, so a suite can load a
// blueprint compiled from last week's sources — or by stock Aiken — and report
// hundreds of reds (or greens) that say nothing about the code under test.
// `deployment-profiles.mjs build` therefore records, beside the blueprint, a
// hash of every compiler input and the compiler's own `--version`; this module
// computes that stamp and judges an existing one against the tree and the pin.
//
// CLI: `node demo/scripts/lib/blueprint-stamp.mjs [blueprint]` exits 0 when the
// stamp is fresh, 1 when it is stale or missing, 3 when it could not be judged.

import { createHash } from "node:crypto";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { pinnedAikenVersion } from "../../../onchain/aiken/scripts/pinned-compiler.mjs";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../..",
);

// Everything `aiken build` reads. `build/` and `scripts/` are not inputs.
const sourceDirectories = ["lib", "validators", "env"];
const sourceFiles = ["aiken.toml", "aiken.lock"];

export const buildRecordPath = (blueprintPath) =>
  `${blueprintPath}.deployment.json`;

const listFiles = (directory, found) => {
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) {
      listFiles(path, found);
    } else if (entry.isFile()) {
      found.push(path);
    }
  }
  return found;
};

/**
 * sha256 over every compiler input, each framed by its project-relative path
 * and length so that moving bytes between files changes the hash.
 */
export const blueprintSourceHash = (root = repositoryRoot) => {
  const project = resolve(root, "onchain/aiken");
  const files = [
    ...sourceDirectories.flatMap((directory) =>
      listFiles(resolve(project, directory), []),
    ),
    ...sourceFiles.map((file) => resolve(project, file)),
  ]
    .map((path) => ({ path, name: relative(project, path) }))
    .sort((left, right) =>
      left.name < right.name ? -1 : left.name > right.name ? 1 : 0,
    );
  const hash = createHash("sha256");
  for (const { path, name } of files) {
    const contents = readFileSync(path);
    hash.update(`${name}\0${String(contents.length)}\0`);
    hash.update(contents);
  }
  return hash.digest("hex");
};

export const blueprintHash = (blueprintPath) =>
  createHash("sha256").update(readFileSync(blueprintPath)).digest("hex");

const rebuildCommand = (profileName) =>
  `pnpm --dir demo deployment:build ${profileName ?? "<profile>"}`;

/**
 * @returns {{ status: "fresh" | "stale" | "missing" | "unknown", detail: string, fix: string | null, blueprintAbsent?: true }}
 */
export const checkBlueprintStamp = ({
  root = repositoryRoot,
  blueprintPath = resolve(root, "onchain/aiken/plutus.json"),
} = {}) => {
  if (!existsSync(blueprintPath)) {
    return {
      status: "missing",
      blueprintAbsent: true,
      detail: `no blueprint at ${blueprintPath}`,
      fix: rebuildCommand(),
    };
  }
  const recordPath = buildRecordPath(blueprintPath);
  if (!existsSync(recordPath)) {
    return {
      status: "missing",
      detail: `${blueprintPath} has no build record (${recordPath}); it was not built by deployment-profiles.mjs, so nothing says which sources or compiler produced it`,
      fix: rebuildCommand(),
    };
  }
  let record;
  let expectedCompiler;
  let sourceHash;
  let currentBlueprintHash;
  try {
    record = JSON.parse(readFileSync(recordPath, "utf8"));
    expectedCompiler = pinnedAikenVersion(root);
    sourceHash = blueprintSourceHash(root);
    currentBlueprintHash = blueprintHash(blueprintPath);
  } catch (error) {
    return {
      status: "unknown",
      detail: `could not judge ${blueprintPath}: ${error instanceof Error ? error.message : String(error)}`,
      fix: null,
    };
  }
  const fix = rebuildCommand(record?.profile?.name);
  const mismatches = [];
  if (record.blueprintHash !== currentBlueprintHash) {
    mismatches.push(
      "the blueprint was modified after its build record was written",
    );
  }
  if (record.sourceHash === undefined || record.compiler === undefined) {
    mismatches.push("the build record predates source and compiler stamping");
  } else {
    if (record.sourceHash !== sourceHash) {
      mismatches.push(
        "onchain/aiken sources (lib, validators, env, aiken.toml, aiken.lock) changed since it was built",
      );
    }
    if (record.compiler !== expectedCompiler) {
      mismatches.push(
        `it was built by '${record.compiler}', not the pinned '${expectedCompiler}'`,
      );
    }
  }
  if (mismatches.length > 0) {
    return {
      status: "stale",
      detail: `${blueprintPath} is stale: ${mismatches.join("; ")}`,
      fix,
    };
  }
  return {
    status: "fresh",
    detail: `${blueprintPath} matches its sources and the pinned compiler`,
    fix: null,
  };
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  const requested = process.argv[2];
  const verdict = checkBlueprintStamp(
    requested === undefined ? {} : { blueprintPath: resolve(requested) },
  );
  const stream = verdict.status === "fresh" ? console.log : console.error;
  stream(`blueprint ${verdict.status}: ${verdict.detail}`);
  if (verdict.fix !== null) {
    console.error(`Rebuild it: ${verdict.fix}`);
  }
  process.exitCode =
    verdict.status === "fresh" ? 0 : verdict.status === "unknown" ? 3 : 1;
}
