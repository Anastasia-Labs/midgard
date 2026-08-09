#!/usr/bin/env node

/**
 * The one way an execution ledger takes a reading.
 *
 * Two ledgers pin execution units in this tree — `verify-carriage-exec-ledger-v1.mjs`
 * (the #574 Phase-4 §8 carriage rows) and `verify-q1x-exec-ledger-v1.mjs` (the
 * #575 Q1x family rows) — and what they assert about their numbers differs a
 * lot. How they *obtain* a number must not: a reading taken two ways is two
 * measurements, and the whole point of pinning to the unit is that the pin and
 * the re-take mean the same thing. This module is that shared half, and it is
 * the only part of the two verifiers that was ever byte-identical.
 *
 * Readings are taken through `run-focused-check.mjs` rather than by spawning
 * Aiken directly. That script already asserts the collected module is the one
 * asked for and that every selected test passed, so a report assembled from a
 * different module — or one containing a failure — cannot be accepted here.
 * That guard is the reason this indirection exists at all; calling Aiken
 * straight would give a caller a way to publish a number for a test that did
 * not run.
 */

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const focusedCheckPath = resolve(scriptDirectory, "run-focused-check.mjs");
const projectDirectory = resolve(scriptDirectory, "..");

/**
 * @param {string} moduleName Aiken module selector, as `run-focused-check.mjs` takes it.
 * @param {string[]} testNames Every test whose reading the caller wants, in one invocation.
 * @returns {Map<string, {mem: number, cpu: number}>} title → execution units.
 */
export const measureModule = (moduleName, testNames) => {
  const result = spawnSync(
    process.execPath,
    [focusedCheckPath, moduleName, ...testNames],
    {
      cwd: projectDirectory,
      encoding: "utf8",
      maxBuffer: 64 * 1024 * 1024,
    },
  );
  if (result.status !== 0) {
    if (result.stderr) {
      process.stderr.write(result.stderr);
    }
    throw new Error(
      `focused check for '${moduleName}' exited with status ${String(result.status)}`,
    );
  }
  let report;
  try {
    report = JSON.parse(result.stdout);
  } catch {
    throw new Error(
      `focused check for '${moduleName}' did not return a structured report`,
    );
  }
  const readings = new Map();
  for (const module of report.modules ?? []) {
    for (const test of module.tests ?? []) {
      readings.set(test.title, {
        mem: test.execution_units.mem,
        cpu: test.execution_units.cpu,
      });
    }
  }
  return readings;
};
