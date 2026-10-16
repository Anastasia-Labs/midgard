#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import { readdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { exUnitsSummary } from "./guard-focused-selector.mjs";
import { aikenBinary } from "../../../scripts/lib/aiken.mjs";

const usage =
  "usage: node scripts/run-focused-check.mjs <module> <test-name> [<test-name> ...]";

// A module selector is an Aiken module's source path relative to its source
// root, without the `.ak` suffix. Aiken accepts hyphens and dots inside those
// path segments (`midgard/bounded-blob-v1.test.ak`), so the allowlist is a
// `/`-separated path of dot-separated words drawn from lowercase letters,
// digits, underscores and hyphens: exactly the characters this tree's module
// paths use, and nothing more.
const moduleSegment = String.raw`[a-z0-9_-]+(?:\.[a-z0-9_-]+)*`;
const validModule = new RegExp(`^${moduleSegment}(?:/${moduleSegment})*$`, "u");
// Test names are Aiken identifiers.
const validTest = /^[a-z0-9_]+$/u;

// Aiken compiles every `.ak` file under these roots; `env/` holds environment
// modules that are substituted by `--env` rather than addressed by name.
const sourceRoots = ["lib", "validators"];

const collectModules = (directory, prefix, found) => {
  let entries;
  try {
    entries = readdirSync(directory, { withFileTypes: true });
  } catch {
    return found;
  }
  for (const entry of entries) {
    if (entry.isDirectory()) {
      collectModules(
        resolve(directory, entry.name),
        `${prefix}${entry.name}/`,
        found,
      );
    } else if (entry.isFile() && entry.name.endsWith(".ak")) {
      found.push(`${prefix}${entry.name.slice(0, -".ak".length)}`);
    }
  }
  return found;
};

// Aiken derives a module's name from its source path with hyphens folded to
// underscores, so `midgard/bounded-blob-v1.test.ak` reports as
// `midgard/bounded_blob_v1.test`. Accept either spelling.
const toAikenModule = (candidate) => candidate.replaceAll("-", "_");

// Runs the named tests of one module and returns the exit status the CLI
// reports, Aiken's report (stdout) and the diagnostics (stderr).
export const focusedCheck = ({
  projectDirectory,
  moduleName,
  testNames,
  environment,
}) => {
  const stderr = [];
  const fail = (status, ...lines) => ({
    status,
    stdout: "",
    stderr: [...stderr, ...lines].join("\n") + "\n",
  });
  if (
    moduleName === undefined ||
    testNames.length === 0 ||
    !validModule.test(moduleName) ||
    testNames.some((testName) => !validTest.test(testName)) ||
    new Set(testNames).size !== testNames.length
  ) {
    return fail(2, usage);
  }

  const modulesByAlias = new Map();
  for (const root of sourceRoots) {
    for (const found of collectModules(
      resolve(projectDirectory, root),
      "",
      [],
    )) {
      for (const alias of [found, toAikenModule(found)]) {
        if (!modulesByAlias.has(alias)) {
          modulesByAlias.set(alias, found);
        }
      }
    }
  }

  const sourceModule = modulesByAlias.get(moduleName);
  if (sourceModule === undefined) {
    return fail(
      2,
      `focused Aiken check: no module '${moduleName}' under ${sourceRoots
        .map((root) => `${root}/`)
        .join(" or ")} in ${projectDirectory}`,
    );
  }
  const aikenModule = toAikenModule(sourceModule);
  // `aiken check -m` splits its selector on the first `.` to separate the
  // module from the `{test}` list, so a module whose own name contains a `.`
  // cannot be spelled out in full. Aiken matches the module part as a
  // substring, so the prefix up to the first `.` still selects it; the
  // module-identity assertion below rejects whatever an over-broad prefix
  // drags in.
  const moduleSelector = aikenModule.split(".")[0];

  if (environment !== undefined && !/^[a-z0-9_-]+$/u.test(environment)) {
    return fail(2, "MIDGARD_AIKEN_ENV contains an invalid environment name");
  }

  const args = [
    "check",
    ...testNames.flatMap((testName) => [
      "-m",
      `${moduleSelector}.{${testName}}`,
    ]),
    "-e",
    "--plain-numbers",
  ];
  if (environment !== undefined) {
    args.push("--env", environment);
  }

  const result = spawnSync(aikenBinary(), args, {
    cwd: projectDirectory,
    encoding: "utf8",
    maxBuffer: 16 * 1024 * 1024,
  });
  if (result.stderr) stderr.push(result.stderr.trimEnd());
  const stdout = result.stdout ?? "";

  let report;
  try {
    report = JSON.parse(stdout);
  } catch {
    const lines = [];
    if (result.error !== undefined) lines.push(result.error.message);
    if (result.status !== null && result.status !== 0) {
      lines.push(`Aiken exited with status ${result.status}`);
    }
    lines.push("Aiken did not return its structured test report");
    return { ...fail(1, ...lines), stdout };
  }

  const collectedModules = Array.isArray(report?.modules)
    ? report.modules.map((entry) => entry?.name)
    : [];
  if (collectedModules.length !== 1 || collectedModules[0] !== aikenModule) {
    return {
      ...fail(
        1,
        `focused Aiken check expected results from module '${aikenModule}' only; collected=[${collectedModules.join(", ")}]`,
      ),
      stdout,
    };
  }

  const summary = report?.summary;
  if (
    summary?.total !== testNames.length ||
    summary?.passed !== testNames.length ||
    summary?.failed !== 0
  ) {
    return {
      ...fail(
        1,
        `focused Aiken check expected exactly ${String(testNames.length)} passing test(s); collected=${String(summary?.total)}, passed=${String(summary?.passed)}, failed=${String(summary?.failed)}`,
      ),
      stdout,
    };
  }
  if (result.status !== null && result.status !== 0) {
    return {
      ...fail(result.status, `Aiken exited with status ${result.status}`),
      stdout,
    };
  }
  // stdout is Aiken's report, which the execution-ledger measurer parses; the
  // summary goes to stderr.
  return {
    status: 0,
    stdout,
    stderr:
      [...stderr, JSON.stringify({ exUnits: exUnitsSummary(report) })].join(
        "\n",
      ) + "\n",
  };
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  const [, , moduleName, ...testNames] = process.argv;
  const outcome = focusedCheck({
    projectDirectory: resolve(dirname(fileURLToPath(import.meta.url)), ".."),
    moduleName,
    testNames,
    environment: process.env.MIDGARD_AIKEN_ENV,
  });
  process.stderr.write(outcome.stderr);
  process.stdout.write(outcome.stdout);
  process.exitCode = outcome.status;
}
