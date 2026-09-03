#!/usr/bin/env node

/**
 * The one way a **within-basis** execution ledger is judged.
 *
 * `exec-ledger-measure-v1.mjs` beside this file is the one way such a ledger
 * takes a reading; this is the one way it decides what the reading means. The
 * split is the same in kind: a reading taken two ways is two measurements, and
 * a judgement made two ways is two gates, of which the weaker one is the one
 * that will be trusted.
 *
 * **Why this exists.** A *within-basis* ledger is the simplest shape a lane's
 * execution exit criterion takes: every row is an absolute reading, every row
 * is judged `within` the GOAL_SPEC §3.3 basis, and nothing is derived from
 * anything else. Lanes that publish that shape have nothing lane-specific to
 * assert beyond their own ledger filename and their own lane label — which is
 * exactly why the shape kept being retyped, and why #577's Q21 lane was written
 * against this module instead — and, in the same change, why #576's Q31 lane
 * was moved onto it rather than left as a second copy. AGENTS.md *Engineering
 * Principles*: do not reimplement common functionality without a clear reason,
 * and do not accept a stopgap meant to be replaced later.
 *
 * **What it checks.**
 *
 *   1. The ledger's declared basis is the basis the caller says the lane was
 *      judged at. The basis is a constant of the lane, not a number read out of
 *      the hand-edited file whose judgements it validates: without this, raising
 *      `basis.memoryUnits` in the ledger makes every `within` verdict vacuous
 *      while the gate stays green.
 *   2. Every row's raw reading matches the ledger to the unit.
 *   3. Every row the ledger judges `within` is measured within the basis on both
 *      axes, and no row carries any other judgement. A fresh reading that
 *      contradicts a recorded judgement is a structural failure, not a drift.
 *   4. Every measured module declares neutralisation selectors, and they all
 *      ran, in the same invocation as the rows it guards.
 *   5. The run measured at least one row at all. An execution pin that measures
 *      nothing passes for free — the "gate that cannot fail" shape of issues
 *      #519 and #523 — and the per-module guard in check 4 cannot see it,
 *      because a ledger with no modules has no module to guard.
 *
 * **Who consumes it.** The two lanes whose ledgers *are* this shape:
 * `verify-q21-exec-ledger-v1.mjs` (#577, lane C) and
 * `verify-q31-exec-ledger-v1.mjs` (#576, lane B), the second moved onto this
 * module in the same change that wrote it. Each supplies its ledger path, its
 * label and its basis and asserts nothing else, because nothing else about the
 * shape is lane-specific.
 *
 * **What it deliberately does not cover.** The two richer ledgers in this tree
 * — `verify-carriage-exec-ledger-v1.mjs` (#574 §8.10) and
 * `verify-q1x-exec-ledger-v1.mjs` (#575) — are a different shape, not a lazier
 * spelling of this one. Their rows carry no `basisFit` at all, and cannot: a
 * raw row there is a reading of `fixture + step`, and the judgement is made
 * against `derived` subtractions of a measured row and its control, because the
 * fixture is harness work an L1 step never does. On top of that they publish a
 * `fits`/`exceeds` vocabulary, declared infeasibilities with errata
 * cross-references, the Q1X-F6 growth bands, and a binding-axis read budget.
 * None of that is expressible here, and bending either ledger into this shape
 * would mean rewriting the rows themselves — so this module asserts one shape
 * well rather than every shape badly. What all four ledgers *do* share is the
 * basis, and all four now take it from `GOAL_SPEC_EXECUTION_BASIS_V1` below
 * rather than from the file being judged, which is the one defect the two
 * richer verifiers would otherwise have been left carrying.
 *
 * Structural failures are fatal in **both** modes and are decided before
 * anything is written, so a `--update` run that could not find a selector, or
 * one whose fresh reading broke a feasibility judgement, cannot leave a
 * rewritten ledger behind as evidence that it succeeded. `--update` absorbs
 * measurement drift and nothing else.
 */

import { readFileSync, writeFileSync } from "node:fs";

import { measureModule } from "./exec-ledger-measure-v1.mjs";

/**
 * The one execution-budget basis every flat-reversion lane is judged at.
 *
 * GOAL_SPEC §3.3 *Execution fit*, owner amendment 2026-08-08: **13,200,000
 * memory units** — the 20% reserve off the Cardano mainnet `maxTxExUnits`
 * memory cap of 16,500,000 — never the 11,200,000 figure earlier benches
 * derived from the stale Conway 14,000,000 cap. The cpu axis is the same 20%
 * reserve off 10,000,000,000. Both are carried in the repo as the supported
 * floor `MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits` /
 * `...CpuUnits` (`docs/consensus-profile-v1.md` §10).
 *
 * It is exported, and it is exported from *here*, because a basis retyped per
 * lane is a basis per lane. The pair was spelled out as bare literals in the
 * Q21 verifier and in this module's own self-test, and read as data out of four
 * separate ledger `basis` blocks, with nothing tying any of them to §3.3 — so a
 * lane could be judged at a number nobody had noticed was different, which is
 * the same failure as reading the basis out of the ledger being judged, one
 * level further out. Lanes name this constant now; §3.3 moves in one place or
 * not at all.
 */
export const GOAL_SPEC_EXECUTION_BASIS_V1 = Object.freeze({
  memoryUnits: 13_200_000,
  cpuUnits: 8_000_000_000,
});

/**
 * @param {object} options
 * @param {string} options.ledgerPath Absolute path to the lane's ledger JSON.
 * @param {string} options.lane Human label for the lane, used in messages.
 * @param {{memoryUnits: number, cpuUnits: number}} options.declaredBasis The basis the lane's rows were judged at.
 * @param {boolean} options.update Whether to rewrite the raw rows from the measurement.
 * @param {(module: string, tests: string[]) => Map<string, {mem: number, cpu: number}>} [options.measure] Reading source; defaults to the shared measurer, injectable for this module's self-test.
 * @param {{write: (chunk: string) => unknown}} [options.stdout]
 * @param {{write: (chunk: string) => unknown}} [options.stderr]
 * @returns {number} The process exit code the caller should exit with.
 */
export const checkWithinBasisExecLedger = ({
  ledgerPath,
  lane,
  declaredBasis,
  update,
  measure = measureModule,
  stdout = process.stdout,
  stderr = process.stderr,
}) => {
  const ledger = JSON.parse(readFileSync(ledgerPath, "utf8"));

  // A **drift** is a measured number that no longer matches the recorded one —
  // what `--update` exists to absorb. A **structural** failure is anything else:
  // a selector or row that did not run, a basis that is not the one the lane
  // declares, or a `within` judgement the measurement contradicts. None of those
  // is a number to re-pin, so `--update` must not swallow one.
  const failures = [];
  const drifts = [];
  const fail = (message) => failures.push(message);
  const drifted = (message) => drifts.push(message);

  const { memoryUnits, cpuUnits } = ledger.basis ?? {};
  if (
    memoryUnits !== declaredBasis.memoryUnits ||
    cpuUnits !== declaredBasis.cpuUnits
  ) {
    fail(
      `ledger basis mem=${String(memoryUnits)} cpu=${String(cpuUnits)} is not the ` +
        `basis this lane is judged at, mem=${String(declaredBasis.memoryUnits)} ` +
        `cpu=${String(declaredBasis.cpuUnits)}`,
    );
  }

  let rowCount = 0;
  // An absent or mistyped `modules` key is refused by name rather than left to
  // throw out of the loop below. Either way the run exits nonzero, but a gate
  // whose failure mode is an unhandled `TypeError` tells its reader nothing
  // about what was wrong with the ledger.
  const modules = Array.isArray(ledger.modules) ? ledger.modules : null;
  if (modules === null) {
    fail("the ledger declares no `modules` array");
  }

  for (const entry of modules ?? []) {
    // Every field this loop reads is named before it is used, for the reason the
    // `modules` guard above gives: a malformed entry must be refused by name.
    // An entry with no `module` string would otherwise render as the literal
    // `undefined:` in every message below, and one with no `rows` object would
    // throw a raw `TypeError` out of `Object.keys` — a stack trace naming this
    // file rather than a complaint naming the ledger that is actually wrong.
    // `rows` must be a plain object: an array passes `typeof … === "object"`
    // and would silently be read as rows named "0", "1", ….
    const module =
      typeof entry.module === "string" ? entry.module : "<unnamed ledger entry>";
    const rows =
      typeof entry.rows === "object" &&
      entry.rows !== null &&
      !Array.isArray(entry.rows)
        ? entry.rows
        : null;
    if (rows === null) {
      fail(`${module}: ledger entry declares no \`rows\` object`);
      continue;
    }
    const rowNames = Object.keys(rows);
    const neutralisation = Array.isArray(entry.neutralisation)
      ? entry.neutralisation
      : [];
    // The neutralisation selectors run in the same invocation as the rows, so a
    // re-take cannot quietly drop one and still publish the rows it guards. An
    // empty list would disarm check 4 silently, so it is refused.
    if (neutralisation.length === 0) {
      fail(`${module}: ledger entry declares no neutralisation selectors`);
      continue;
    }
    const readings = measure(module, [...rowNames, ...neutralisation]);
    for (const name of neutralisation) {
      if (!readings.has(name)) {
        fail(`${module}: neutralisation selector '${name}' did not run`);
      }
    }
    for (const [name, expected] of Object.entries(rows)) {
      const actual = readings.get(name);
      if (actual === undefined) {
        fail(`${module}: row '${name}' did not run`);
        continue;
      }
      rowCount += 1;

      // The judgement check runs against the *fresh* reading in both modes, and
      // before any write: a re-take that pushes a `within` row past the basis is
      // a structural change to the lane's feasibility claim, not a number to
      // re-pin, so `--update` must not launder it.
      const within =
        actual.mem <= declaredBasis.memoryUnits &&
        actual.cpu <= declaredBasis.cpuUnits;
      if (expected.basisFit === "within" && !within) {
        fail(
          `${module}: '${name}' is recorded 'within' but measured ` +
            `mem=${String(actual.mem)} cpu=${String(actual.cpu)} exceeds the ` +
            `basis mem=${String(declaredBasis.memoryUnits)} cpu=${String(declaredBasis.cpuUnits)}`,
        );
        continue;
      }
      if (expected.basisFit !== "within") {
        fail(
          `${module}: '${name}' has unexpected basisFit ` +
            `'${String(expected.basisFit)}' — a within-basis ledger records only 'within' rows`,
        );
        continue;
      }

      if (update) {
        rows[name] = { ...expected, mem: actual.mem, cpu: actual.cpu };
        continue;
      }
      if (actual.mem !== expected.mem || actual.cpu !== expected.cpu) {
        drifted(
          `${module}: '${name}' drifted — ledger mem=${String(expected.mem)} cpu=${String(expected.cpu)}, ` +
            `measured mem=${String(actual.mem)} cpu=${String(actual.cpu)}`,
        );
      }
    }
  }

  // Two ways to reach here having judged nothing: a ledger whose `modules` array
  // is empty, and one whose every module entry carries an empty `rows` object.
  // Both walk the loop above without a single comparison and would otherwise
  // reach the success write reporting `rows: 0` — a pin that passes because it
  // measured nothing.
  if (rowCount === 0) {
    fail(
      "the ledger produced no measured rows at all — an execution pin that " +
        "measures nothing passes for free",
    );
  }

  if (failures.length > 0) {
    for (const failure of failures) {
      stderr.write(`${failure}\n`);
    }
    stderr.write(
      `\n${lane} execution ledger: ${String(failures.length)} structural failure(s). ` +
        (update
          ? "The ledger was NOT rewritten. `--update` absorbs measurement drift and nothing else."
          : "These are not re-takeable numbers; resolve them in the source or in the ledger.") +
        "\n",
    );
    return 1;
  }

  if (update) {
    writeFileSync(ledgerPath, `${JSON.stringify(ledger, null, 2)}\n`);
    stdout.write(
      `${JSON.stringify({ status: "updated", ledger: ledgerPath }, null, 2)}\n`,
    );
    return 0;
  }

  if (drifts.length > 0) {
    for (const drift of drifts) {
      stderr.write(`${drift}\n`);
    }
    stderr.write(
      `\n${lane} execution ledger: ${String(drifts.length)} drift(s). ` +
        "If the re-take is legitimate, re-run with --update.\n",
    );
    return 1;
  }

  stdout.write(
    `${JSON.stringify(
      {
        status: "pass",
        rows: rowCount,
        basis: {
          memoryUnits: declaredBasis.memoryUnits,
          cpuUnits: declaredBasis.cpuUnits,
        },
      },
      null,
      2,
    )}\n`,
  );
  return 0;
};
