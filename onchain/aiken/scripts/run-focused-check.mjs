#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const usage =
  "usage: node scripts/run-focused-check.mjs <module> <test-name> [<test-name> ...]";
const [, , moduleName, ...testNames] = process.argv;
const validModule = /^[a-z0-9_/]+$/u;
const validTest = /^[a-z0-9_]+$/u;

if (
  moduleName === undefined ||
  testNames.length === 0 ||
  !validModule.test(moduleName) ||
  testNames.some((testName) => !validTest.test(testName)) ||
  new Set(testNames).size !== testNames.length
) {
  console.error(usage);
  process.exit(2);
}

const projectDirectory = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const aikenBinary = process.env.MIDGARD_AIKEN_BIN ?? "aiken";
const environment = process.env.MIDGARD_AIKEN_ENV;
if (environment !== undefined && !/^[a-z0-9_-]+$/u.test(environment)) {
  console.error("MIDGARD_AIKEN_ENV contains an invalid environment name");
  process.exit(2);
}

const args = [
  "check",
  ...testNames.flatMap((testName) => ["-m", `${moduleName}.{${testName}}`]),
  "-e",
  "--plain-numbers",
];
if (environment !== undefined) {
  args.push("--env", environment);
}

const result = spawnSync(aikenBinary, args, {
  cwd: projectDirectory,
  encoding: "utf8",
  maxBuffer: 16 * 1024 * 1024,
});

if (result.stderr) {
  process.stderr.write(result.stderr);
}
if (result.stdout) {
  process.stdout.write(result.stdout);
}
let report;
try {
  report = JSON.parse(result.stdout);
} catch {
  if (result.error !== undefined) {
    console.error(result.error.message);
  }
  if (result.status !== null && result.status !== 0) {
    console.error(`Aiken exited with status ${result.status}`);
  }
  console.error("Aiken did not return its structured test report");
  process.exit(1);
}

const summary = report?.summary;
if (
  summary?.total !== testNames.length ||
  summary?.passed !== testNames.length ||
  summary?.failed !== 0
) {
  console.error(
    `focused Aiken check expected exactly ${String(testNames.length)} passing test(s); collected=${String(summary?.total)}, passed=${String(summary?.passed)}, failed=${String(summary?.failed)}`,
  );
  process.exit(1);
}
if (result.status !== null && result.status !== 0) {
  console.error(`Aiken exited with status ${result.status}`);
  process.exit(result.status);
}
