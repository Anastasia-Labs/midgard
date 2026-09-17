#!/usr/bin/env node

/**
 * Pins the #633 tag-0 structural predicate's execution cost against a fresh
 * `aiken check` measurement at the GOAL_SPEC §3.3 13.2M-mem basis — and, unlike
 * every other ledger in this directory, pins it as an **infeasibility**.
 *
 * **Why this exists.** Decision 0005 amendment A3 gates wave item 9 — giving
 * `ledger_output_v1.parse_script_ref`'s tag-0 arm a canonical native-script
 * structural predicate — on an R3-style ExUnits measurement taken *before* any
 * on-chain commitment. That measurement failed: the largest payload the arm can
 * ever see costs about 81x the memory basis. The item dropped out of the #617
 * wave on the strength of these numbers, so these numbers are exactly the kind
 * of claim the #519/#523 gate-that-cannot-fail doctrine says must be falsifiable.
 * A verdict that dropped an owner-ruled wave item on an unpinned measurement is
 * a verdict nobody can re-check when the follow-up ruling is written.
 *
 * Aiken tests cannot observe their own execution units — the units are the
 * check *report's* reading of the test, not a value in scope — so the pin lives
 * one level up, taken through `run-focused-check.mjs` (so a row for a test that
 * did not run, or one that did not pass, cannot be published) and compared.
 *
 * **Why it is not `exec-ledger-within-basis-v1.mjs`.** That module is the one
 * way a *within-basis* ledger is judged, and it admits only `within` rows by
 * construction: a row recorded any other way is a structural failure there. It
 * is the right shape for a lane whose exit criterion is "this fits". This lane's
 * exit criterion is the opposite one, and bending the shared module to carry
 * `exceeds` would weaken the four lanes that depend on its strictness. So this
 * file reuses the two halves that must not be retyped — `measureModule`, the one
 * way a reading is taken, and `GOAL_SPEC_EXECUTION_BASIS_V1`, the one place §3.3
 * lives — and makes its own judgement, following the two-sided pattern
 * `verify-q1x-exec-ledger-v1.mjs` established for the same situation: a
 * `basisFit` that must be declared, an `exceeds` row that must declare its
 * exception, and movement across the basis failing in either direction. The
 * within-basis label here is spelled `within` rather than Q1x's `fits`, matching
 * the `exec-ledger-within-basis-v1.mjs` this file is contrasted with above.
 *
 * **What is checked.**
 *
 *   1. The ledger's declared basis is `GOAL_SPEC_EXECUTION_BASIS_V1`, taken
 *      from the shared module and never from the ledger being judged. A basis
 *      read out of the hand-edited file whose verdicts it decides is not a
 *      basis, and here it would be worse than usual: raising it is how an
 *      infeasibility quietly becomes a feasibility.
 *   2. Every row's raw reading matches the ledger to the unit.
 *   3. Every row sits on the side of the basis the ledger says it does. A
 *      `within` row that stops fitting is a regression; an `exceeds` row that
 *      starts fitting is a stale recorded infeasibility, and it is the one this
 *      ledger exists to catch — if a future compiler or a cheaper driver brings
 *      the worst case under the basis, wave item 9 comes back and this gate is
 *      what says so. Both directions fail, in both modes.
 *   4. Every `exceeds` row declares its exception: `infeasibility` prose saying
 *      what cannot be done and `ruling` naming who owns the resolution. Without
 *      that, "exceeds" becomes an acceptable steady state and this file degrades
 *      from an infeasibility pin into a no-movement check — the criticism #575's
 *      round-2 review raised against the Q1x ledger.
 *   5. Every row declares the `nodes` its vector presents to the scan. That
 *      number is the divisor in every derivation below, and it is pinned in the
 *      measured modules themselves by shape-guard selectors, so a vector that
 *      shrank cannot be re-pinned here without going red there first.
 *   6. Every derived claim is recomputed from the fresh readings in exact
 *      integer arithmetic rather than restated. The four node counts this lane
 *      publishes — where the basis is exhausted on the deep shape, on the wide
 *      shape, under `inspect_native_script`, and under a bare
 *      `cbor.deserialise` — are two-point linear extrapolations off rows in this
 *      ledger, and a reader who cannot recompute them is taking them on trust.
 *   7. Every measured module declares neutralisation selectors and they all ran,
 *      in the same invocation as the rows they guard.
 *   8. Every `derivedRows` entry — a cost published for a vector this lane
 *      cannot drive inside the tree run — is recomputed here from the fresh
 *      curve, never read from the file, and is held to everything a measured
 *      row is held to: declared `nodes`, a declared `basisFit` the arithmetic
 *      must agree with in both directions, and a declared `infeasibility` plus
 *      `ruling` when it exceeds. It must additionally carry `derivation` prose
 *      recording how the figure was obtained, what direct readings it was
 *      checked against, and how to reproduce one. A derived row may not share a
 *      name with a measured row, and its `nodes` must lie past the curve it is
 *      projected from, so it can never silently become an interpolation.
 *   9. The run measured at least one row and derived at least one claim. An
 *      execution pin that measures nothing passes for free — the "gate that
 *      cannot fail" shape of issues #519 and #523.
 *
 * **Why one row is derived rather than measured.** Host memory tracks the
 * evaluator's recursion depth while execution units track node count, so the
 * deep worst case -- 5,446 nested containers -- peaks around 12.6 GB of host
 * memory, while its wide twin, one node smaller but at stack depth 1, peaks at
 * 0.6 GB. A live deep test took the whole-tree `aiken check` to a 14.62 GB peak
 * and aborted it with SIGABRT against the 16 GiB guardrail cap even with
 * `RAYON_NUM_THREADS=1`; with that one test absent the same run is green at
 * 10.97 GB. The ruling was to keep the tree runnable and derive the deep
 * figure, so the headline MEASURED worst case is the wide one (71.9x over the
 * basis) and the deep one is a `derivedRows` entry recomputed here from the
 * deep curve. The verdict does not lean on which it is: the live wide row alone
 * fails A3's precondition, and so does the bare `cbor.deserialise` floor. Each
 * module is still measured in its own invocation, because Aiken evaluates a
 * module's selected tests in PARALLEL and `run-focused-check.mjs` puts every
 * selector it is handed into one process; see the ledger's `note`.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-native-script-scan-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-native-script-scan-exec-ledger-v1.mjs --update
 *
 * `--update` rewrites the raw rows, the recomputed derived integers and each
 * derived row's recomputed `mem` from the measurement, and is the only way to
 * record a legitimate re-take. It never rewrites `basisFit`, `infeasibility`,
 * `ruling`, `nodes`, `derivation` prose or any `claim` prose:
 * those are judgements, and a re-take must not be able to launder one into the
 * ledger. It is not a bypass — a selector that did not run, a row that did not
 * run, a missing exception declaration, or a fresh reading that contradicts a
 * `basisFit` fails in update mode too, and the ledger is not rewritten when it
 * does.
 */

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { measureModule } from "./exec-ledger-measure-v1.mjs";
import { GOAL_SPEC_EXECUTION_BASIS_V1 } from "./exec-ledger-within-basis-v1.mjs";

const lane = "native-script-scan #633 tag-0 structural predicate";

/**
 * The first node count at which a two-point linear extrapolation of `mem`
 * against `nodes` passes `basis`.
 *
 * Integer arithmetic throughout, and with BigInt, because the cpu readings run
 * to 3.6e11 and the intermediate product below runs higher still: a derived
 * integer that depends on float rounding is a derived integer two readers can
 * disagree about. Solving `memA + (n - nodesA) * dm / dn <= basis` for the
 * largest integer `n`, with `dm > 0` and `dn > 0`, gives
 * `n = nodesA + floor((basis - memA) * dn / dm)`; the answer published is the
 * first `n` past it.
 *
 * @param {{nodes: number, mem: number}} low
 * @param {{nodes: number, mem: number}} high
 * @param {number} basis
 * @returns {{largestWithinBasisNodes: number, basisExhaustedAtNodes: number}}
 */
const extrapolateBasisExhaustion = (low, high, basis) => {
  const dn = BigInt(high.nodes - low.nodes);
  const dm = BigInt(high.mem - low.mem);
  if (dn <= 0n || dm <= 0n) {
    throw new RangeError(
      "a two-point cost extrapolation needs the higher row to carry both more nodes and more memory",
    );
  }
  if (BigInt(low.mem) > BigInt(basis)) {
    throw new RangeError(
      "a two-point cost extrapolation needs its lower row to be within the basis",
    );
  }
  const largest =
    BigInt(low.nodes) + (BigInt(basis) - BigInt(low.mem)) * dn / dm;
  return {
    largestWithinBasisNodes: Number(largest),
    basisExhaustedAtNodes: Number(largest + 1n),
  };
};

/**
 * The memory cost a two-point linear extrapolation of `mem` against `nodes`
 * predicts at `nodes`.
 *
 * The inverse of `extrapolateBasisExhaustion`, for the one row this lane cannot
 * measure in-tree: driving the predicate over the full deep vector peaks around
 * 12.6 GB of host memory and aborts the whole-tree `aiken check` at the
 * guardrail cap, so its cost is derived from the deep curve rows -- which do
 * run -- rather than read off a live test. Exact integer arithmetic, and
 * extrapolation only: `nodes` must lie beyond the curve it is projected from,
 * so this can never quietly become an interpolation between two rows that
 * happen to bracket it.
 *
 * @param {{nodes: number, mem: number}} low
 * @param {{nodes: number, mem: number}} high
 * @param {number} nodes
 * @returns {number}
 */
const extrapolateMemAtNodes = (low, high, nodes) => {
  const dn = BigInt(high.nodes - low.nodes);
  const dm = BigInt(high.mem - low.mem);
  if (dn <= 0n || dm <= 0n) {
    throw new RangeError(
      "a two-point cost extrapolation needs the higher row to carry both more nodes and more memory",
    );
  }
  if (nodes <= high.nodes) {
    throw new RangeError(
      `a derived row must be an extrapolation past the curve it is derived from: ` +
        `nodes=${String(nodes)} is not beyond the higher row's ${String(high.nodes)}`,
    );
  }
  return Number(
    BigInt(low.mem) + (BigInt(nodes) - BigInt(low.nodes)) * dm / dn,
  );
};

/**
 * @param {object} options
 * @param {string} options.ledgerPath
 * @param {{memoryUnits: number, cpuUnits: number}} options.declaredBasis
 * @param {boolean} options.update
 * @param {(module: string, tests: string[]) => Map<string, {mem: number, cpu: number}>} [options.measure]
 * @param {{write: (chunk: string) => unknown}} [options.stdout]
 * @param {{write: (chunk: string) => unknown}} [options.stderr]
 * @returns {number} The process exit code the caller should exit with.
 */
const checkNativeScriptScanExecLedger = ({
  ledgerPath,
  declaredBasis,
  update,
  measure = measureModule,
  stdout = process.stdout,
  stderr = process.stderr,
}) => {
  const ledger = JSON.parse(readFileSync(ledgerPath, "utf8"));

  // A **drift** is a measured number that no longer matches the recorded one —
  // what `--update` exists to absorb. A **structural** failure is anything else:
  // a selector or row that did not run, a basis that is not the one this lane is
  // judged at, a `basisFit` the measurement contradicts, or an `exceeds` row
  // that declares no exception. None of those is a number to re-pin.
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

  const modules = Array.isArray(ledger.modules) ? ledger.modules : null;
  if (modules === null) {
    fail("the ledger declares no `modules` array");
  }

  let rowCount = 0;
  // Fresh readings, keyed by row name across every module, so the derivations
  // below can pair rows that were measured in different invocations — which the
  // two worst cases must be.
  const measured = new Map();

  for (const entry of modules ?? []) {
    // Every field is named before it is used: a malformed entry must be refused
    // by name rather than render as `undefined:` in every message below or throw
    // a raw `TypeError` naming this file instead of the ledger that is wrong.
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
    const neutralisation = Array.isArray(entry.neutralisation)
      ? entry.neutralisation
      : [];
    // An empty list would disarm check 7 silently, so it is refused.
    if (neutralisation.length === 0) {
      fail(`${module}: ledger entry declares no neutralisation selectors`);
      continue;
    }
    const readings = measure(module, [...Object.keys(rows), ...neutralisation]);
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

      if (!Number.isInteger(expected.nodes) || expected.nodes <= 0) {
        fail(
          `${module}: '${name}' declares no positive integer \`nodes\` — the ` +
            "divisor every derivation in this ledger uses",
        );
        continue;
      }
      if (measured.has(name)) {
        fail(`row name '${name}' appears in more than one module`);
        continue;
      }
      measured.set(name, { nodes: expected.nodes, mem: actual.mem });

      // Judged against the *fresh* reading in both modes and before any write: a
      // re-take that moves a row across the basis is a structural change to this
      // lane's feasibility claim, not a number to re-pin.
      const within =
        actual.mem <= declaredBasis.memoryUnits &&
        actual.cpu <= declaredBasis.cpuUnits;
      if (expected.basisFit !== "within" && expected.basisFit !== "exceeds") {
        fail(
          `${module}: '${name}' has unexpected basisFit ` +
            `'${String(expected.basisFit)}' — every row is recorded 'within' or 'exceeds' ` +
            "so that movement across the basis fails in either direction",
        );
        continue;
      }
      if (expected.basisFit === "within" && !within) {
        fail(
          `${module}: '${name}' is recorded 'within' but measured ` +
            `mem=${String(actual.mem)} cpu=${String(actual.cpu)} exceeds the ` +
            `basis mem=${String(declaredBasis.memoryUnits)} cpu=${String(declaredBasis.cpuUnits)}`,
        );
        continue;
      }
      if (expected.basisFit === "exceeds" && within) {
        fail(
          `${module}: '${name}' is recorded 'exceeds' but measured ` +
            `mem=${String(actual.mem)} cpu=${String(actual.cpu)}, which is WITHIN the ` +
            `basis — a recorded infeasibility that became feasible is not drift. ` +
            "Wave item 9 may be back; re-read the ledger's note and #633.",
        );
        continue;
      }
      // An `exceeds` row with nowhere to point is how "exceeds" becomes a
      // steady state. It has to name what cannot be done and who owns undoing
      // it. The Q1x ledger requires a spec erratum here; this lane's
      // infeasibility is owned by an owner ruling rather than by a normative
      // section, so it requires the ruling.
      if (expected.basisFit === "exceeds") {
        if (
          typeof expected.infeasibility !== "string" ||
          expected.infeasibility.trim() === ""
        ) {
          fail(
            `${module}: '${name}' exceeds the basis and declares no \`infeasibility\``,
          );
          continue;
        }
        if (
          typeof expected.ruling !== "string" ||
          expected.ruling.trim() === ""
        ) {
          fail(
            `${module}: '${name}' exceeds the basis and declares no \`ruling\` ` +
              "cross-reference naming who owns the resolution",
          );
          continue;
        }
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

  // The derivations. A missing block fails rather than skips: these four node
  // counts are what the verdict is quoted as, everywhere from the predicate's
  // doc comment to the follow-up ticket.
  const derived = Array.isArray(ledger.derived) ? ledger.derived : null;
  if (derived === null) {
    fail("the ledger declares no `derived` array");
  }
  let derivedCount = 0;
  for (const claim of derived ?? []) {
    const label =
      typeof claim.claim === "string" ? claim.claim : "<unnamed derivation>";
    const low = measured.get(claim.from);
    const high = measured.get(claim.to);
    if (low === undefined || high === undefined) {
      fail(
        `${label}: derivation names rows '${String(claim.from)}' and ` +
          `'${String(claim.to)}', and at least one is not a measured row here`,
      );
      continue;
    }
    let recomputed;
    try {
      recomputed = extrapolateBasisExhaustion(
        low,
        high,
        declaredBasis.memoryUnits,
      );
    } catch (error) {
      fail(`${label}: ${error.message}`);
      continue;
    }
    derivedCount += 1;
    if (update) {
      claim.largestWithinBasisNodes = recomputed.largestWithinBasisNodes;
      claim.basisExhaustedAtNodes = recomputed.basisExhaustedAtNodes;
      continue;
    }
    if (
      claim.largestWithinBasisNodes !== recomputed.largestWithinBasisNodes ||
      claim.basisExhaustedAtNodes !== recomputed.basisExhaustedAtNodes
    ) {
      drifted(
        `${label}: recorded largestWithinBasisNodes=${String(claim.largestWithinBasisNodes)} ` +
          `basisExhaustedAtNodes=${String(claim.basisExhaustedAtNodes)}, recomputed from the ` +
          `fresh readings of '${claim.from}' and '${claim.to}' as ` +
          `${String(recomputed.largestWithinBasisNodes)} / ${String(recomputed.basisExhaustedAtNodes)}`,
      );
    }
  }

  // Derived ROWS: costs this lane publishes for a vector it cannot drive
  // in-tree. They are held to every discipline a measured row is held to —
  // declared `nodes`, a declared `basisFit` that the arithmetic must agree
  // with in both directions, and a declared exception when it exceeds — and to
  // one more: the figure itself is recomputed here, so it can never be edited
  // into the ledger by hand. What separates them from a measured row is only
  // that `mem` comes from the curve instead of from the evaluator, and the
  // ledger labels them so no reader can mistake one for the other.
  let derivedRowCount = 0;
  const derivedRows = Array.isArray(ledger.derivedRows) ? ledger.derivedRows : [];
  for (const row of derivedRows) {
    const name = typeof row.row === "string" ? row.row : "<unnamed derived row>";
    if (measured.has(name)) {
      fail(
        `derived row '${name}' collides with a measured row of the same name — ` +
          "a figure is either read from a live test or derived from the curve, never both",
      );
      continue;
    }
    const low = measured.get(row.from);
    const high = measured.get(row.to);
    if (low === undefined || high === undefined) {
      fail(
        `derived row '${name}' names rows '${String(row.from)}' and ` +
          `'${String(row.to)}', and at least one is not a measured row here`,
      );
      continue;
    }
    if (!Number.isInteger(row.nodes) || row.nodes <= 0) {
      fail(`derived row '${name}' declares no positive integer \`nodes\``);
      continue;
    }
    let recomputedMem;
    try {
      recomputedMem = extrapolateMemAtNodes(low, high, row.nodes);
    } catch (error) {
      fail(`derived row '${name}': ${error.message}`);
      continue;
    }
    derivedRowCount += 1;

    const within = recomputedMem <= declaredBasis.memoryUnits;
    if (row.basisFit !== "within" && row.basisFit !== "exceeds") {
      fail(
        `derived row '${name}' has unexpected basisFit '${String(row.basisFit)}'`,
      );
      continue;
    }
    if (row.basisFit === "within" && !within) {
      fail(
        `derived row '${name}' is recorded 'within' but the curve derives ` +
          `mem=${String(recomputedMem)}, over the basis mem=${String(declaredBasis.memoryUnits)}`,
      );
      continue;
    }
    if (row.basisFit === "exceeds" && within) {
      fail(
        `derived row '${name}' is recorded 'exceeds' but the curve derives ` +
          `mem=${String(recomputedMem)}, which is WITHIN the basis — a recorded ` +
          "infeasibility that became feasible is not drift. Wave item 9 may be back; " +
          "re-read the ledger's note and #633.",
      );
      continue;
    }
    if (row.basisFit === "exceeds") {
      if (
        typeof row.infeasibility !== "string" ||
        row.infeasibility.trim() === ""
      ) {
        fail(
          `derived row '${name}' exceeds the basis and declares no \`infeasibility\``,
        );
        continue;
      }
      if (typeof row.ruling !== "string" || row.ruling.trim() === "") {
        fail(
          `derived row '${name}' exceeds the basis and declares no \`ruling\` ` +
            "cross-reference naming who owns the resolution",
        );
        continue;
      }
    }
    // A derived row's whole claim to being evidence is that a reader can see
    // where the number came from and reproduce the direct reading. Without that
    // prose it is an unsourced figure wearing a gate's clothes.
    if (typeof row.derivation !== "string" || row.derivation.trim() === "") {
      fail(
        `derived row '${name}' declares no \`derivation\` prose — a derived ` +
          "figure must record how it was obtained, what direct readings it was " +
          "checked against, and how to reproduce one",
      );
      continue;
    }

    if (update) {
      row.mem = recomputedMem;
      continue;
    }
    if (row.mem !== recomputedMem) {
      drifted(
        `derived row '${name}': recorded mem=${String(row.mem)}, recomputed from ` +
          `the fresh readings of '${row.from}' and '${row.to}' as ${String(recomputedMem)}`,
      );
    }
  }

  // Two ways to reach here having judged nothing: an empty `modules` array, and
  // one whose every entry carries an empty `rows` object. Both walk the loops
  // above without a comparison and would otherwise reach the success write
  // reporting `rows: 0`.
  if (rowCount === 0) {
    fail(
      "the ledger produced no measured rows at all — an execution pin that " +
        "measures nothing passes for free",
    );
  }
  if (derivedCount === 0) {
    fail(
      "the ledger recomputed no derived claim at all — the node counts this " +
        "lane publishes would then be prose",
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
        derived: derivedCount,
        derivedRows: derivedRowCount,
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

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

// Set rather than `process.exit(code)`: the checker is synchronous and has
// nothing pending, so letting the process end on its own cannot truncate the
// report it just wrote.
process.exitCode = checkNativeScriptScanExecLedger({
  ledgerPath: resolve(scriptDirectory, "native-script-scan-exec-ledger-v1.json"),
  declaredBasis: GOAL_SPEC_EXECUTION_BASIS_V1,
  update: process.argv.includes("--update"),
});
