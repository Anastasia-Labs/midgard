#!/usr/bin/env node

/**
 * Pins every execution figure `docs/spec/midgard-tx.md` §8.10 publishes for the
 * §8 carriage ladder against a fresh `aiken check` measurement.
 *
 * **Why this exists.** §8.10's rows were hand-transcribed from a check run into
 * a Markdown table, and nothing in the repo asserted them: a grep for the
 * published figures returned zero hits outside the document itself. That is the
 * gate-that-cannot-fail shape — the numbers could drift arbitrarily far from the
 * validator without any suite going red, and a cost claim nothing can falsify is
 * not a measurement. The byte-level constants in the same section are pinned
 * (`MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES_V1` and its neighbours are asserted
 * in the emulator suite); the execution figures now are too.
 *
 * Aiken tests cannot assert their own execution units — the units are the
 * check *report's* observation of the test, not a value in scope — so the pin
 * has to live one level up. This is that level: a ledger of expected readings
 * next to a verifier that takes the readings and compares.
 *
 * **What is checked.** Three things, because publishing a table is three claims:
 *
 *   1. Every raw row matches to the unit. These are absolute readings, and
 *      §8.10 quotes them as absolutes precisely so the deltas can be recomputed.
 *   2. Every derived figure §8.10 publishes is the subtraction it says it is.
 *      A control row and a measured row that both drifted by the same amount
 *      would leave the delta right and the readings wrong; checking both closes
 *      that.
 *   3. The neutralisation selectors still run and still pass. Each measured
 *      family has a fixture-shape assertion and a tampered-input rejection; the
 *      measurement is of a validator that discriminates, and without these the
 *      rows could be a measurement of something that returns `True` for
 *      anything.
 *
 * Two conditions sit underneath all three, and neither is data the ledger gets
 * to supply about itself: the basis the read budget is computed against is
 * `exec-ledger-within-basis-v1.mjs`'s exported GOAL_SPEC §3.3 constant rather
 * than a number read out of this ledger, and the run must have measured at
 * least one row. A basis read from the file being judged, or a pin that
 * measured nothing, is a gate that cannot fail (#519, #523).
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-carriage-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-carriage-exec-ledger-v1.mjs --update
 *
 * `--update` rewrites the ledger from the measurement. It is how a legitimate
 * re-take is recorded — and it is the only way, so the spec table and the ledger
 * move together or the check that follows fails.
 *
 * **`--update` is not a bypass.** It absorbs measurement drift and nothing else:
 * a selector that did not run, a referenced claim that does not exist, or a
 * binding axis the fresh measurement contradicts fails in update mode too, and
 * the ledger is not rewritten when it does.
 */

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { measureModule } from "./exec-ledger-measure-v1.mjs";
import { GOAL_SPEC_EXECUTION_BASIS_V1 } from "./exec-ledger-within-basis-v1.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const ledgerPath = resolve(
  scriptDirectory,
  "native-tx-carriage-exec-ledger-v1.json",
);

const update = process.argv.includes("--update");
const ledger = JSON.parse(readFileSync(ledgerPath, "utf8"));

// Two classes of complaint. A **drift** is a measured number that no longer
// matches the recorded one — the thing `--update` exists to absorb. A
// **structural** failure is anything else: a selector that did not run, a
// referenced claim that does not exist, a derived binding axis the measurement
// contradicts. None of those is a number to re-pin, so `--update` must not
// swallow one. See the note above the exit sequence at the foot of this file.
const failures = [];
const drifts = [];
const fail = (message) => failures.push(message);
const drifted = (message) => drifts.push(message);

const measured = new Map();

for (const entry of ledger.modules) {
  const rowNames = Object.keys(entry.rows);
  const neutralisation = entry.neutralisation ?? [];
  // The neutralisation selectors are run in the same invocation as the rows.
  // Running them separately would let a re-take quietly drop one and still
  // publish the rows it guards. An empty list would disarm check #3 silently,
  // so a module with no selectors is refused rather than waved through.
  if (neutralisation.length === 0) {
    fail(`${entry.module}: ledger entry declares no neutralisation selectors`);
    continue;
  }
  const readings = measureModule(entry.module, [
    ...rowNames,
    ...neutralisation,
  ]);
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
    measured.set(name, actual);
    if (update) {
      entry.rows[name] = actual;
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

for (const derived of ledger.derived) {
  const from = measured.get(derived.from);
  const minus = measured.get(derived.minus);
  if (from === undefined || minus === undefined) {
    fail(`derived '${derived.claim}': one of its rows did not run`);
    continue;
  }
  const mem = from.mem - minus.mem;
  const cpu = from.cpu - minus.cpu;
  if (update) {
    derived.mem = mem;
    derived.cpu = cpu;
    continue;
  }
  if (mem !== derived.mem || cpu !== derived.cpu) {
    drifted(
      `derived '${derived.claim}' drifted — ledger mem=${String(derived.mem)} cpu=${String(derived.cpu)}, ` +
        `measured mem=${String(mem)} cpu=${String(cpu)}`,
    );
  }
}

// The basis the read budget is computed against is a constant of the *program*,
// not a number read out of the file whose figures it validates. Read from the
// ledger — as this file did until #577's round-2 review — raising
// `basis.memoryUnits` in `native-tx-carriage-exec-ledger-v1.json` moves the
// published per-step read budget without any gate noticing, which is the
// gate-that-cannot-fail shape of #519/#523 dressed as a data-driven check. So
// the constant is the authority and the ledger's own declaration is checked
// against it.
const basisMemory = GOAL_SPEC_EXECUTION_BASIS_V1.memoryUnits;
const basisCpu = GOAL_SPEC_EXECUTION_BASIS_V1.cpuUnits;
if (
  ledger.basis?.memoryUnits !== basisMemory ||
  ledger.basis?.cpuUnits !== basisCpu
) {
  fail(
    `ledger basis mem=${String(ledger.basis?.memoryUnits)} cpu=${String(ledger.basis?.cpuUnits)} ` +
      `is not the GOAL_SPEC §3.3 basis §8.10 is published at, mem=${String(basisMemory)} ` +
      `cpu=${String(basisCpu)}`,
  );
}

// A run that measured nothing is a run that passed for free. The per-module
// selector guard cannot see it — a ledger with no modules has no module to
// guard — and while the `derived` lookups above happen to fail in that state
// today, they do so as a side effect of a claim's rows not resolving, which is
// a confusing way to report "this ledger measured nothing" and stops being true
// the moment `derived` is ever emptied alongside `modules`.
if (measured.size === 0) {
  fail(
    "the ledger produced no measured rows at all — an execution pin that " +
      "measures nothing passes for free",
  );
}

// §8.10 publishes a per-step read budget and names an axis as binding. Both are
// derived from two of the rows above, so both are checked rather than restated:
// a re-take that moved the binding axis without moving the prose is exactly the
// error the section's previous revision made.
const budget = ledger.readBudget;
const openCost = ledger.derived.find(
  (entry) => entry.claim === budget.openCost,
);
const perRead = ledger.derived.find((entry) => entry.claim === budget.perRead);
if (openCost === undefined || perRead === undefined) {
  fail("read budget references a derived claim that does not exist");
} else {
  const byMemory = (basisMemory - openCost.mem) / perRead.mem;
  const byCpu = (basisCpu - openCost.cpu) / perRead.cpu;
  const bindingAxis = byCpu <= byMemory ? "cpu" : "memory";
  const round = (value) => Math.round(value * 100) / 100;
  if (update) {
    budget.byMemory = round(byMemory);
    budget.byCpu = round(byCpu);
    budget.bindingAxis = bindingAxis;
  } else {
    if (round(byMemory) !== budget.byMemory || round(byCpu) !== budget.byCpu) {
      drifted(
        `read budget drifted — ledger memory=${String(budget.byMemory)} cpu=${String(budget.byCpu)}, ` +
          `measured memory=${String(round(byMemory))} cpu=${String(round(byCpu))}`,
      );
    }
    if (bindingAxis !== budget.bindingAxis) {
      fail(
        `read budget's binding axis is '${bindingAxis}', ledger says '${budget.bindingAxis}'`,
      );
    }
  }
}

// Structural failures are fatal in **both** modes, and they are checked before
// anything is written: a `--update` run that could not find a neutralisation
// selector must not leave a rewritten ledger behind as evidence that it
// succeeded. This file's own header refuses that shape for the spec table; it
// had not been applied to the flag itself until #575's round-2 review.
if (failures.length > 0) {
  for (const failure of failures) {
    console.error(failure);
  }
  console.error(
    `\n§8.10 carriage execution ledger: ${String(failures.length)} structural failure(s). ` +
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
    `\n§8.10 carriage execution ledger: ${String(drifts.length)} drift(s). ` +
      "If the re-take is legitimate, re-run with --update and move " +
      "docs/spec/midgard-tx.md §8.10 in the same commit.",
  );
  process.exit(1);
}

process.stdout.write(
  `${JSON.stringify(
    {
      status: "pass",
      rows: measured.size,
      derived: ledger.derived.length,
      readBudget: {
        byMemory: budget.byMemory,
        byCpu: budget.byCpu,
        bindingAxis: budget.bindingAxis,
      },
    },
    null,
    2,
  )}\n`,
);
