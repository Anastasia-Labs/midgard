#!/usr/bin/env node

/**
 * Pins the #576 lane-B (Q31 `reference-input-no-idx`) family-rebind execution
 * figures against a fresh `aiken check` measurement at the GOAL_SPEC §3.3
 * 13.2M-mem basis.
 *
 * **Why this exists.** The Q31 rebind moves the family off the counted
 * `field_commitment_from_items` re-hash of a reproduced item list and onto the
 * §8.8 door: step-02 reads field 1 (reference inputs) by arithmetic at field
 * 0's shared 38-byte stride (§5.3/§10.5), and step-04 reads field 2's
 * authenticated item count (§5.2). Neither is a walk whose cost grows with the
 * disputed transaction, so the lane introduces no new unbounded walk and the
 * ledger carries no `basisFit:"exceeds"` row. What still has to be asserted is
 * that the two door-open steps *fit* the basis with margin, and that a re-take
 * cannot silently turn a `within` row into one that does not — a cost claim
 * nothing can falsify is not a measurement.
 *
 * Aiken tests cannot assert their own execution units, so the pin lives one
 * level up: a ledger of expected readings next to this verifier, which takes
 * the readings through `run-focused-check.mjs` (so a row for a test that did
 * not run, or one that did not pass, cannot be published) and compares.
 *
 * **What is checked.**
 *
 *   1. Every row's raw reading matches the ledger to the unit.
 *   2. Every row the ledger judges `within` is measured within the basis on
 *      both axes. A fresh reading that contradicts that judgement is a
 *      structural failure, not a drift.
 *   3. The neutralisation selectors still run and still pass, so the rows are a
 *      measurement of a validator that discriminates rather than one that
 *      returns `True` for anything.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-q31-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-q31-exec-ledger-v1.mjs --update
 *
 * `--update` rewrites the raw rows from the measurement and is the only way to
 * record a legitimate re-take. It absorbs measurement drift and nothing else:
 * a selector that did not run, a row that did not run, or a fresh reading that
 * contradicts a `within` judgement fails in update mode too, and the ledger is
 * not rewritten when it does.
 */

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { measureModule } from "./exec-ledger-measure-v1.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const ledgerPath = resolve(scriptDirectory, "native-tx-q31-exec-ledger-v1.json");

const update = process.argv.includes("--update");
const ledger = JSON.parse(readFileSync(ledgerPath, "utf8"));

// A **drift** is a measured number that no longer matches the recorded one —
// what `--update` exists to absorb. A **structural** failure is anything else:
// a selector or row that did not run, or a `within` judgement the measurement
// contradicts. None of those is a number to re-pin, so `--update` must not
// swallow one.
const failures = [];
const drifts = [];
const fail = (message) => failures.push(message);
const drifted = (message) => drifts.push(message);

const { memoryUnits, cpuUnits } = ledger.basis;
let rowCount = 0;

for (const entry of ledger.modules) {
  const rowNames = Object.keys(entry.rows);
  const neutralisation = entry.neutralisation ?? [];
  // The neutralisation selectors run in the same invocation as the rows, so a
  // re-take cannot quietly drop one and still publish the rows it guards. An
  // empty list would disarm check #3 silently, so it is refused.
  if (neutralisation.length === 0) {
    fail(`${entry.module}: ledger entry declares no neutralisation selectors`);
    continue;
  }
  const readings = measureModule(entry.module, [...rowNames, ...neutralisation]);
  for (const name of neutralisation) {
    if (!readings.has(name)) {
      fail(`${entry.module}: neutralisation selector '${name}' did not run`);
    }
  }
  for (const [name, expected] of Object.entries(entry.rows)) {
    const actual = readings.get(name);
    if (actual === undefined) {
      fail(`${entry.module}: row '${name}' did not run`);
      continue;
    }
    rowCount += 1;

    // The judgement check runs against the *fresh* reading in both modes, and
    // before any write: a re-take that pushes a `within` row past the basis is
    // a structural change to the lane's feasibility claim, not a number to
    // re-pin, so `--update` must not launder it.
    const within = actual.mem <= memoryUnits && actual.cpu <= cpuUnits;
    if (expected.basisFit === "within" && !within) {
      fail(
        `${entry.module}: '${name}' is recorded 'within' but measured ` +
          `mem=${String(actual.mem)} cpu=${String(actual.cpu)} exceeds the ` +
          `basis mem=${String(memoryUnits)} cpu=${String(cpuUnits)}`,
      );
      continue;
    }
    if (expected.basisFit !== "within") {
      fail(
        `${entry.module}: '${name}' has unexpected basisFit ` +
          `'${String(expected.basisFit)}' — this lane records only 'within' rows`,
      );
      continue;
    }

    if (update) {
      entry.rows[name] = { ...expected, mem: actual.mem, cpu: actual.cpu };
      continue;
    }
    if (actual.mem !== expected.mem || actual.cpu !== expected.cpu) {
      drifted(
        `${entry.module}: '${name}' drifted — ledger mem=${String(expected.mem)} cpu=${String(expected.cpu)}, ` +
          `measured mem=${String(actual.mem)} cpu=${String(actual.cpu)}`,
      );
    }
  }
}

// Structural failures are fatal in **both** modes, and are checked before
// anything is written: a `--update` run that could not find a selector, or one
// whose fresh reading broke a feasibility judgement, must not leave a rewritten
// ledger behind as evidence that it succeeded.
if (failures.length > 0) {
  for (const failure of failures) {
    console.error(failure);
  }
  console.error(
    `\nQ31 reference-input-no-idx execution ledger: ${String(failures.length)} structural failure(s). ` +
      (update
        ? "The ledger was NOT rewritten. `--update` absorbs measurement drift and nothing else."
        : "These are not re-takeable numbers; resolve them in the source or in the ledger."),
  );
  process.exit(1);
}

if (update) {
  writeFileSync(ledgerPath, `${JSON.stringify(ledger, null, 2)}\n`);
  process.stdout.write(
    `${JSON.stringify({ status: "updated", ledger: ledgerPath }, null, 2)}\n`,
  );
  process.exit(0);
}

if (drifts.length > 0) {
  for (const drift of drifts) {
    console.error(drift);
  }
  console.error(
    `\nQ31 reference-input-no-idx execution ledger: ${String(drifts.length)} drift(s). ` +
      "If the re-take is legitimate, re-run with --update.",
  );
  process.exit(1);
}

process.stdout.write(
  `${JSON.stringify(
    {
      status: "pass",
      rows: rowCount,
      basis: { memoryUnits, cpuUnits },
    },
    null,
    2,
  )}\n`,
);
