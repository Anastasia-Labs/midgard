#!/usr/bin/env node

/**
 * The Q1x lane's rolling execution re-measurement — issue #575, the first
 * Phase-5 family-rebind wave of the #565 flat field-hash reversion.
 *
 * **Why this exists.** #565 slots measurements as *lane exit criteria*: they
 * ride the lane that builds the thing they measure, so a cost regression
 * surfaces in the building lane rather than weeks later in the Phase-7 batch.
 * That only works if the numbers are asserted somewhere a suite can go red. A
 * table in a report is not a measurement; a ledger next to a verifier is.
 *
 * Aiken tests cannot observe their own execution units — the units are the
 * check *report's* reading of the test, not a value in scope — so the pin lives
 * one level up, exactly as `verify-carriage-exec-ledger-v1.mjs` (the #574
 * Phase-4 exit ledger) does for the §8 carriage rows. This file is that file's
 * twin for the Q1x family steps; the two share a shape on purpose, so a reader
 * who has understood one has understood both.
 *
 * **What is checked.**
 *
 *   1. Every raw row matches to the unit, at the GOAL_SPEC §3.3 declared basis
 *      of 13,200,000 memory units — taken from
 *      `exec-ledger-within-basis-v1.mjs`'s exported constant, never from the
 *      ledger being judged, and the ledger's own `basis` block is checked
 *      against it. A basis read out of the hand-edited file whose verdicts it
 *      decides is not a basis. The run must also have measured at least one row.
 *   2. Every derived figure is the subtraction it claims to be. A control row
 *      and a measured row that both drifted by the same amount would leave the
 *      delta right and the readings wrong.
 *   3. Every measured module's neutralisation selectors still run and still
 *      pass, so the rows are readings of a validator that discriminates rather
 *      than of one that returns `True` for anything.
 *   4. Every derived figure sits on the side of the basis the ledger says it
 *      does — `basisFit: "fits"` that stops fitting is a regression, and
 *      `basisFit: "exceeds"` that starts fitting is a stale recorded
 *      infeasibility. Both fail. The headroom the lane claims is recomputed
 *      rather than restated.
 *   5. Every `basisFit: "exceeds"` row **declares its exception**: prose in
 *      `infeasibility` saying what is unfaultable and who owns the resolution,
 *      and an `errata` cross-reference into `docs/spec/midgard-tx.md`. Without
 *      that, "exceeds" becomes an acceptable steady state and this file
 *      degrades from a fits-the-basis check into a no-movement check — the
 *      exact criticism #575's round-2 review raised against it.
 *   6. Neither the `derived` block nor the `q1xF6` block may be absent: a
 *      missing exit criterion fails rather than skips.
 *
 * **The Q1X-F6 block** is the reason this ledger exists at all. #551 recorded
 * that Q10 step-04 and Q11 step-02 could not be evaluated at the admissible
 * 296 spend-input cardinality: the retired idiom reproduced the whole
 * authenticated collection in the step, so execution memory grew with the
 * transaction's input count and passed the ledger's cap somewhere above 40
 * inputs. Under the door the step hashes the field preimage once and reads one
 * item by §5.3 arithmetic. `q1xF6` asserts the property that claim rests on —
 * that the cost of the *read* does not grow with cardinality — by measuring the
 * same step at both ends of the admissible range and bounding the difference.
 * A rebind that reintroduced a per-item cost would widen that gap and fail
 * here, which is what makes the signal falsifiable rather than asserted.
 *
 * **What this ledger covers, and what it deliberately does not.** It covers the
 * *shapes* the #575 rebind introduced, not the lifecycle. Concretely: the two
 * arithmetic-access steps #551 measured the defect at (Q10 step-04, Q11
 * step-02), and **both** of the wave's genuinely unbounded walks (Q17 step-06
 * over field 6, Q16 step-04 over field 7) — the second added at the round-2
 * review's request, since measuring only the steps the rebind made flat and
 * none of the steps it made linear is not a measurement of the rebind.
 *
 * It does **not** carry a row per lifecycle step, and that is a scoping
 * decision rather than an omission. Per-step lifecycle re-measurement across
 * the whole correction path is GOAL_SPEC §3.3's *maturity fit* threshold, whose
 * evidence artifact is
 * `docs/exec-plans/evidence/canonical-v1-proof-family-q1x-v1.json` and whose
 * Q10/Q11 output-5 cells `GOAL_PROGRESS.md` deliberately holds OPEN for the
 * Phase-7 re-measurement against the Phase-6 blueprint. Taking those numbers
 * here, against a pre-blueprint build, would produce figures Phase 7 must
 * discard and re-take; what a *lane* ledger can honestly assert is that no
 * shape the lane introduced is unmeasured, which is what the four families
 * above establish.
 *
 * This is therefore the **provisional** signal #575 owes. Formal closure is
 * Phase 7's, against the Phase-6 blueprint, per #563's evidence standard.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-q1x-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> node scripts/verify-q1x-exec-ledger-v1.mjs --update
 *
 * `--update` rewrites the ledger from the measurement, and is the only way to
 * record a legitimate re-take. It rewrites raw rows, derived figures and the
 * Q1X-F6 growth readings only — never `basisFit`, never `infeasibility`, never
 * `errata`, never the Q1X-F6 bands. Those are judgements, and a re-take must
 * not be able to launder one into the ledger.
 *
 * **`--update` is not a bypass.** It absorbs measurement drift and nothing
 * else: a selector that did not run, a missing block, a judgement the fresh
 * measurement contradicts, or a Q1X-F6 band the fresh reading falls outside all
 * fail in update mode too, and the ledger is not rewritten when they do. Until
 * #575's round-2 review this file exited 0 on `--update` before looking at the
 * failure list at all, which made the flag a way to make any complaint go away.
 */

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { measureModule } from "./exec-ledger-measure-v1.mjs";
import { GOAL_SPEC_EXECUTION_BASIS_V1 } from "./exec-ledger-within-basis-v1.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const ledgerPath = resolve(scriptDirectory, "native-tx-q1x-exec-ledger-v1.json");

const update = process.argv.includes("--update");
const ledger = JSON.parse(readFileSync(ledgerPath, "utf8"));

// Two classes of complaint, and the difference is the whole point of the split.
//
//   * a **drift** is a measured number that no longer matches the recorded one.
//     It is the thing `--update` exists to absorb, and absorbing it is
//     legitimate: the ledger is a pin, and a re-take moves the pin.
//   * a **structural** failure is anything else — a selector that did not run, a
//     block that is missing, a judgement (`basisFit`, `infeasibility`, a
//     Q1X-F6 band) that is absent or contradicted by the measurement. None of
//     those is a number to re-pin, and `--update` must not launder one.
//
// Before #575's round-2 review this file exited 0 on `--update` *before*
// looking at either list, so a run that could not find a neutralisation
// selector at all still rewrote the ledger and reported success. That is the
// same "gate that cannot fail" shape the empty-`neutralisation` guard below
// already refuses, so it is refused the same way: structural failures are fatal
// in both modes and the ledger is not written when there are any.
const failures = [];
const drifts = [];
const fail = (message) => failures.push(message);
const drifted = (message) => drifts.push(message);

const measured = new Map();

for (const entry of ledger.modules) {
  const rowNames = Object.keys(entry.rows);
  const neutralisation = entry.neutralisation ?? [];
  // The neutralisation selectors run in the same invocation as the rows.
  // Running them separately would let a re-take quietly drop one and still
  // publish the rows it guards; an empty list would disarm check #3 in silence,
  // so a module that declares none is refused rather than waved through.
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

// The derived block and the Q1X-F6 block are this lane's *exit criterion*, not
// optional decoration. Reading them with `?? []` and `!== undefined` — as this
// script did until #575's review — makes deleting or misspelling a key a silent
// pass: the verifier finds nothing to check and exits 0, reporting `derived: 0`
// to a CI step nobody reads the body of. That is the same "gate that cannot
// fail" shape the empty-`neutralisation` guard above already refuses, so the
// two are refused the same way and for the same stated reason.
if (!Array.isArray(ledger.derived) || ledger.derived.length === 0) {
  fail(
    "ledger declares no `derived` figures — the basis and Q1X-F6 checks are " +
      "computed from them, so an absent or empty block would disarm every " +
      "assertion below rather than pass them",
  );
}

for (const derived of ledger.derived ?? []) {
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

// Every published *step cost* must fit the declared basis. This is the lane's
// exit criterion in its plainest form: a family step that does not fit 13.2M
// mem cannot be evaluated on L1 at the 20% reserve, whatever its delta against
// a control says.
//
// It is the derived figures that are checked, not the raw rows. A raw row is a
// reading of `fixture + step`, and the fixture is test-harness work — building
// a block, encoding 296 inputs, hashing a preimage — that an L1 step never
// does, because on L1 those bytes arrive in the redeemer already built. The
// control row exists precisely so the harness can be subtracted out; asserting
// the basis against the raw row would be asserting it against a number the
// validator is not responsible for.
//
// The basis is a constant of the *program*, not a number read out of the file
// whose judgements it validates. Read from the ledger — as this file did until
// #577's round-2 review — raising `basis.memoryUnits` in
// `native-tx-q1x-exec-ledger-v1.json` makes every `fits` verdict below vacuous
// while this gate stays green, which is the gate-that-cannot-fail shape of
// #519/#523 dressed as a data-driven check. So the constant is the authority and
// the ledger's own declaration is checked against it.
const basisMemory = GOAL_SPEC_EXECUTION_BASIS_V1.memoryUnits;
const basisCpu = GOAL_SPEC_EXECUTION_BASIS_V1.cpuUnits;
if (
  ledger.basis?.memoryUnits !== basisMemory ||
  ledger.basis?.cpuUnits !== basisCpu
) {
  fail(
    `ledger basis mem=${String(ledger.basis?.memoryUnits)} cpu=${String(ledger.basis?.cpuUnits)} ` +
      `is not the GOAL_SPEC §3.3 basis this lane is judged at, mem=${String(basisMemory)} ` +
      `cpu=${String(basisCpu)}`,
  );
}

// And a run that judged nothing is a run that passed for free. The per-module
// selector guard cannot see it: a ledger with no modules has no module to guard.
// The `derived` checks below already refuse most of this shape by name, but they
// do so as a side effect of a claim's rows not resolving, which is a confusing
// way to report "this ledger measured nothing" — and it would stop being true
// the moment a derived block ever became optional.
if (measured.size === 0) {
  fail(
    "the ledger produced no measured rows at all — an execution pin that " +
      "measures nothing passes for free",
  );
}

const derivedByClaim = new Map(
  (ledger.derived ?? []).map((entry) => [entry.claim, entry]),
);
// Each derived figure declares which side of the basis it is on, and the check
// runs in **both** directions. A row that claims to fit and does not is the
// regression this file exists to catch. A row that is recorded as exceeding —
// a step this wave measured and found infeasible at its admissible cardinality
// — and then quietly starts fitting is just as much a drift: it means either
// the step or the measurement changed, and the recorded infeasibility (with the
// issue that owns it) is now stale. Neither may pass in silence, and a row that
// declares nothing is refused rather than defaulted, because the default is
// what would let a new over-basis row in unnoticed.
// The shape an `errata` cross-reference has to have: the spec file, a section,
// and an erratum letter. Matching on shape rather than on an exact string keeps
// the check from going stale when a second over-basis row lands under a
// different erratum, while still refusing a bare "see the spec".
const erratumReference =
  /docs\/spec\/midgard-tx\.md\s+§[\d.]+\s+erratum\s+E\d/u;

for (const derived of ledger.derived ?? []) {
  const fits = derived.mem <= basisMemory && derived.cpu <= basisCpu;
  if (derived.basisFit !== "fits" && derived.basisFit !== "exceeds") {
    fail(
      `derived '${derived.claim}' declares no \`basisFit\` — it must say "fits" ` +
        `or "exceeds" so that movement across the basis is a failure in either direction`,
    );
    continue;
  }
  if (derived.basisFit === "fits" && !fits) {
    fail(
      `derived '${derived.claim}' is recorded as fitting the declared basis but exceeds it — ` +
        `mem=${String(derived.mem)}/${String(basisMemory)}, cpu=${String(derived.cpu)}/${String(basisCpu)}`,
    );
  }
  if (derived.basisFit === "exceeds") {
    // An over-basis row is an *exception*, and an exception has to be declared
    // somewhere a reader of the spec will meet it — otherwise "exceeds" becomes
    // an acceptable steady state and this file degrades into a no-movement
    // check. So two things are required of it, and neither is rewritten by
    // `--update`: prose saying what is unfaultable and who owns the resolution,
    // and a cross-reference to the erratum that carries the limit normatively.
    if (typeof derived.infeasibility !== "string") {
      fail(
        `derived '${derived.claim}' is recorded as exceeding the basis but names no ` +
          "`infeasibility` — an over-basis step must say what is unfaultable and who owns it",
      );
    }
    if (
      typeof derived.errata !== "string" ||
      !erratumReference.test(derived.errata)
    ) {
      fail(
        `derived '${derived.claim}' is recorded as exceeding the basis but names no \`errata\` ` +
          "cross-reference matching " +
          String(erratumReference) +
          " — an over-basis step is an amendment-level exception to the declared basis and " +
          "must be declared in docs/spec/midgard-tx.md, not only here",
      );
    }
    if (fits) {
      fail(
        `derived '${derived.claim}' is recorded as exceeding the declared basis but now fits it — ` +
          `mem=${String(derived.mem)}/${String(basisMemory)}, cpu=${String(derived.cpu)}/${String(basisCpu)}. ` +
          "Re-take the row and retire the recorded infeasibility in the same commit.",
      );
    }
  }
}

// The Q1X-F6 signal (#551). Two readings of the same step at the two ends of
// the admissible spend-input range, and the claim that the second is not
// materially dearer than the first. `maxMemoryGrowth`/`maxCpuGrowth` are the
// tolerance the lane commits to; a per-item cost re-entering the step would
// blow through them long before it reached the basis, so the regression is
// caught while it is still cheap to fix.
const f6 = ledger.q1xF6;
if (f6 === undefined || f6 === null) {
  fail(
    "ledger declares no `q1xF6` block — it is the #551 signal this whole file " +
      "exists to assert, so its absence is a failure and not a reason to skip",
  );
} else {
  const low = derivedByClaim.get(f6.lowCardinalityClaim);
  const high = derivedByClaim.get(f6.highCardinalityClaim);
  if (low === undefined || high === undefined) {
    fail("q1xF6: one of its derived claims does not exist");
  } else {
    const memoryGrowth = high.mem - low.mem;
    const cpuGrowth = high.cpu - low.cpu;
    const memoryHeadroom = basisMemory - high.mem;
    const cpuHeadroom = basisCpu - high.cpu;
    if (update) {
      f6.memoryGrowth = memoryGrowth;
      f6.cpuGrowth = cpuGrowth;
      f6.memoryHeadroom = memoryHeadroom;
      f6.cpuHeadroom = cpuHeadroom;
    } else if (
      memoryGrowth !== f6.memoryGrowth ||
      cpuGrowth !== f6.cpuGrowth ||
      memoryHeadroom !== f6.memoryHeadroom ||
      cpuHeadroom !== f6.cpuHeadroom
    ) {
      drifted(
        `q1xF6 drifted — ledger growth mem=${String(f6.memoryGrowth)} cpu=${String(f6.cpuGrowth)}, ` +
          `measured mem=${String(memoryGrowth)} cpu=${String(cpuGrowth)}`,
      );
    }
    // The bands are checked in **both** modes, against the freshly measured
    // growth. They are judgements, `--update` does not rewrite them, and a
    // re-take that lands outside one is precisely the event they exist to
    // catch — so absorbing the readings while ignoring the bands would be the
    // laundering this file is written to prevent.
    {
      // The band is two-sided on both axes. One-sided was the wrong shape: the
      // measured growth is *negative* on both axes, and a one-sided bound
      // absorbs that silently — which is how a −112,081 mem artifact sat inside
      // a 400,000 tolerance for a whole wave without anyone having to explain
      // it. A band forces the artifact to stay the size it was measured at, so
      // a change in the *compilation* of these two programs shows up as a
      // failure rather than as unexplained slack. See the ledger's
      // `deltaAttribution` for what the artifact is.
      const bands = [
        ["memory", memoryGrowth, f6.minMemoryGrowth, f6.maxMemoryGrowth],
        ["cpu", cpuGrowth, f6.minCpuGrowth, f6.maxCpuGrowth],
      ];
      for (const [axis, growth, min, max] of bands) {
        if (typeof min !== "number" || typeof max !== "number") {
          fail(
            `q1xF6: the ${axis} band is not declared as two numbers — a missing ` +
              "bound is an unasserted axis, not a permissive one",
          );
          continue;
        }
        if (growth > max) {
          fail(
            `q1xF6: ${axis} grew by ${String(growth)} across the admissible spend-input range, ` +
              `band [${String(min)}, ${String(max)}] — the read is scaling with cardinality again`,
          );
        }
        if (growth < min) {
          fail(
            `q1xF6: ${axis} fell by ${String(-growth)} across the admissible spend-input range, ` +
              `below the band [${String(min)}, ${String(max)}]. The step cannot get cheaper with ` +
              "cardinality, so this is the control-subtraction artifact moving — re-attribute it " +
              "before re-taking the band.",
          );
        }
      }
    }
  }
}

// Structural failures are fatal in **both** modes, and they are checked before
// anything is written: a `--update` run that could not find a selector, or that
// contradicts a recorded judgement, must not leave a rewritten ledger behind as
// evidence that it succeeded.
if (failures.length > 0) {
  for (const failure of failures) {
    console.error(failure);
  }
  console.error(
    `\nQ1x family execution ledger: ${String(failures.length)} structural failure(s). ` +
      (update
        ? "The ledger was NOT rewritten. `--update` absorbs measurement drift and nothing " +
          "else — a missing selector, a missing block, or a judgement the measurement " +
          "contradicts has to be resolved in the source or in the ledger's declared fields."
        : "These are not re-takeable numbers; resolve them in the source or in the " +
          "ledger's declared fields."),
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
    `\nQ1x family execution ledger: ${String(drifts.length)} drift(s). ` +
      "If the re-take is legitimate, re-run with --update and move the lane's " +
      "reported numbers in the same commit.",
  );
  process.exit(1);
}

process.stdout.write(
  `${JSON.stringify(
    {
      status: "pass",
      // The authoritative pair, not the ledger's `basis` block: that block
      // carries a paragraph of provenance prose, and echoing the number this
      // run actually judged against is the point of printing it at all.
      basis: { memoryUnits: basisMemory, cpuUnits: basisCpu },
      rows: measured.size,
      derived: (ledger.derived ?? []).length,
      q1xF6:
        f6 === undefined
          ? null
          : {
              memoryGrowth: f6.memoryGrowth,
              cpuGrowth: f6.cpuGrowth,
              memoryHeadroom: f6.memoryHeadroom,
              cpuHeadroom: f6.cpuHeadroom,
            },
    },
    null,
    2,
  )}\n`,
);
