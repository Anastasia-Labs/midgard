#!/usr/bin/env node

/**
 * The hostile self-test for the #628 frontier gate.
 *
 * A gate is only worth the trust placed in it if someone has watched it go red.
 * Issues #519 and #523 are both cases of a gate that could not fail, and the
 * #628 ledger this file guards is the sole evidence behind a sign-off figure —
 * "the L2 arm's marginal cost is within basis" — so every judgement the
 * verifier makes is mutated here and required to fail.
 *
 * The fixture ledger is synthetic and the measurement is stubbed. That is
 * deliberate: this file tests the VERIFIER, not Aiken, and a self-test that had
 * to spawn a compiler would be too slow to run on every change and would fail
 * for reasons that have nothing to do with the logic under test.
 *
 * Run from `onchain/aiken/`:
 *
 *   node --test scripts/verify-transition-trace-descriptor-exec-ledger-v1.test.mjs
 */

import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import {
  GOAL_SPEC_EXECUTION_BASIS_V1,
} from "./exec-ledger-within-basis-v1.mjs";
import {
  checkTransitionTraceDescriptorExecLedger,
} from "./verify-transition-trace-descriptor-exec-ledger-v1.mjs";

const basis = GOAL_SPEC_EXECUTION_BASIS_V1;

const exampleModule = "midgard/example.test";

/** The readings the stub measurement returns unless a case overrides them. */
const passingReadings = {
  small_arm: { mem: 1_000_000, cpu: 400_000_000 },
  small_baseline: { mem: 200_000, cpu: 80_000_000 },
  big_arm: { mem: 14_000_000, cpu: 900_000_000 },
  big_baseline: { mem: 300_000, cpu: 90_000_000 },
  guard_rejects: { mem: 10_000, cpu: 1_000_000 },
};

const ledgerWith = (overrides = {}) => ({
  basis: { memoryUnits: basis.memoryUnits, cpuUnits: basis.cpuUnits },
  modules: [
    {
      module: exampleModule,
      neutralisation: ["guard_rejects"],
      rows: {
        small_arm: {
          kind: "arm",
          sizeBytes: 100,
          mem: 1_000_000,
          cpu: 400_000_000,
          basisFit: "within",
        },
        small_baseline: {
          kind: "baseline",
          sizeBytes: 100,
          mem: 200_000,
          cpu: 80_000_000,
          basisFit: "within",
        },
        big_arm: {
          kind: "arm",
          sizeBytes: 200,
          mem: 14_000_000,
          cpu: 900_000_000,
          basisFit: "exceeds",
          infeasibility: "the whole test does not fit the basis at 200 bytes",
          ruling: "#628 review major; owner ruling pending",
        },
        big_baseline: {
          kind: "baseline",
          sizeBytes: 200,
          mem: 300_000,
          cpu: 90_000_000,
          basisFit: "within",
        },
      },
    },
  ],
  marginal: [
    {
      claim: "small arm net of its fixtures",
      arm: "small_arm",
      baseline: "small_baseline",
      marginalMem: 800_000,
      marginalCpu: 320_000_000,
      netBasisFit: "within",
    },
    {
      claim: "big arm net of its fixtures",
      arm: "big_arm",
      baseline: "big_baseline",
      marginalMem: 13_700_000,
      marginalCpu: 810_000_000,
      netBasisFit: "exceeds",
      infeasibility: "even net of fixtures the arm does not fit",
      ruling: "#628 review major; owner ruling pending",
    },
  ],
  crossings: [
    {
      claim: "example crossing",
      dimension: "example bytes",
      withinRow: "small_arm",
      exceedsRow: "big_arm",
      withinSizeBytes: 100,
      exceedsSizeBytes: 200,
      bracketWidthBytes: 100,
    },
  ],
  derived: [],
  ...overrides,
});

const stubMeasure = (readings) => (moduleName, tests) => {
  assert.equal(moduleName, exampleModule);
  const collected = new Map();
  for (const name of tests) {
    if (readings[name] !== undefined) {
      collected.set(name, readings[name]);
    }
  }
  return collected;
};

const silent = { write: () => {} };

/**
 * Run the checker over a temporary copy of a ledger and return the exit code
 * along with whatever was left on disk, so update-mode cases can assert that a
 * refusal did not rewrite the file.
 */
const run = (ledger, { update = false, readings = passingReadings } = {}) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-628-frontier-selftest-"));
  const ledgerPath = join(directory, "ledger.json");
  writeFileSync(ledgerPath, `${JSON.stringify(ledger, null, 2)}\n`);
  try {
    const code = checkTransitionTraceDescriptorExecLedger({
      ledgerPath,
      declaredBasis: basis,
      update,
      measure: stubMeasure(readings),
      stdout: silent,
      stderr: silent,
    });
    return { code, onDisk: JSON.parse(readFileSync(ledgerPath, "utf8")) };
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

/** Deep clone so a mutation in one case cannot leak into the next. */
const clone = (value) => JSON.parse(JSON.stringify(value));

test("an unmutated ledger whose readings match passes", () => {
  assert.equal(run(ledgerWith()).code, 0);
});

test("a perturbed mem figure is drift and goes red", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows.small_arm.mem += 1;
  assert.equal(run(ledger).code, 1);
});

test("a perturbed cpu figure is drift and goes red", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows.big_arm.cpu -= 1;
  assert.equal(run(ledger).code, 1);
});

test("--update absorbs a perturbed mem figure and rewrites it", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows.small_arm.mem += 1;
  const { code, onDisk } = run(ledger, { update: true });
  assert.equal(code, 0);
  assert.equal(onDisk.modules[0].rows.small_arm.mem, 1_000_000);
});

test("a basisFit flipped from within to exceeds goes red", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows.small_arm.basisFit = "exceeds";
  ledger.modules[0].rows.small_arm.infeasibility = "claimed";
  ledger.modules[0].rows.small_arm.ruling = "claimed";
  assert.equal(run(ledger).code, 1);
});

test("a basisFit flipped from exceeds to within goes red", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows.big_arm.basisFit = "within";
  assert.equal(run(ledger).code, 1);
});

test("--update cannot launder a flipped basisFit and leaves the file alone", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows.big_arm.basisFit = "within";
  const { code, onDisk } = run(ledger, { update: true });
  assert.equal(code, 1);
  assert.equal(onDisk.modules[0].rows.big_arm.basisFit, "within");
  assert.equal(onDisk.modules[0].rows.big_arm.mem, 14_000_000);
});

test("an exceeds row stripped of its ruling goes red", () => {
  const ledger = ledgerWith();
  delete ledger.modules[0].rows.big_arm.ruling;
  assert.equal(run(ledger).code, 1);
  assert.equal(run(ledger, { update: true }).code, 1);
});

test("an exceeds row stripped of its infeasibility goes red", () => {
  const ledger = ledgerWith();
  delete ledger.modules[0].rows.big_arm.infeasibility;
  assert.equal(run(ledger).code, 1);
});

test("a blank ruling is treated as no ruling", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows.big_arm.ruling = "   ";
  assert.equal(run(ledger).code, 1);
});

test("a row stripped of its kind goes red", () => {
  const ledger = ledgerWith();
  delete ledger.modules[0].rows.small_baseline.kind;
  assert.equal(run(ledger).code, 1);
});

test("a row stripped of its sizeBytes goes red", () => {
  const ledger = ledgerWith();
  delete ledger.modules[0].rows.small_arm.sizeBytes;
  assert.equal(run(ledger).code, 1);
});

test("a ledger basis other than GOAL_SPEC §3.3 goes red", () => {
  const ledger = ledgerWith();
  ledger.basis.memoryUnits = basis.memoryUnits * 2;
  assert.equal(run(ledger).code, 1);
});

test("a module that declares no neutralisation selectors goes red", () => {
  const ledger = ledgerWith();
  ledger.modules[0].neutralisation = [];
  assert.equal(run(ledger).code, 1);
});

test("a neutralisation selector that did not run goes red", () => {
  const readings = clone(passingReadings);
  delete readings.guard_rejects;
  assert.equal(run(ledgerWith(), { readings }).code, 1);
});

test("a row that did not run goes red", () => {
  const readings = clone(passingReadings);
  delete readings.big_arm;
  assert.equal(run(ledgerWith(), { readings }).code, 1);
});

test("a ledger with no rows at all goes red", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows = {};
  assert.equal(run(ledger).code, 1);
});

test("a ledger with no modules array goes red", () => {
  const ledger = ledgerWith();
  delete ledger.modules;
  assert.equal(run(ledger).code, 1);
});

test("a marginal figure perturbed by one unit is drift and goes red", () => {
  const ledger = ledgerWith();
  ledger.marginal[0].marginalMem += 1;
  assert.equal(run(ledger).code, 1);
});

test("a marginal netBasisFit flipped to within goes red", () => {
  const ledger = ledgerWith();
  ledger.marginal[1].netBasisFit = "within";
  assert.equal(run(ledger).code, 1);
});

test("a marginal netBasisFit flipped to exceeds goes red", () => {
  const ledger = ledgerWith();
  ledger.marginal[0].netBasisFit = "exceeds";
  ledger.marginal[0].infeasibility = "claimed";
  ledger.marginal[0].ruling = "claimed";
  assert.equal(run(ledger).code, 1);
});

test("a marginal claim whose net figure exceeds without a ruling goes red", () => {
  const ledger = ledgerWith();
  delete ledger.marginal[1].ruling;
  assert.equal(run(ledger).code, 1);
});

test("a baseline that is not cheaper than its arm goes red", () => {
  const readings = clone(passingReadings);
  readings.small_baseline = { mem: 1_000_000, cpu: 400_000_000 };
  const ledger = ledgerWith();
  ledger.modules[0].rows.small_baseline.mem = 1_000_000;
  ledger.modules[0].rows.small_baseline.cpu = 400_000_000;
  assert.equal(run(ledger, { readings }).code, 1);
});

test("pairing an arm against an arm rather than a baseline goes red", () => {
  const ledger = ledgerWith();
  ledger.marginal[0].baseline = "big_arm";
  assert.equal(run(ledger).code, 1);
});

test("a baseline taken at a different width than its arm goes red", () => {
  const ledger = ledgerWith();
  ledger.modules[0].rows.small_baseline.sizeBytes = 101;
  assert.equal(run(ledger).code, 1);
});

test("a ledger with no marginal block goes red", () => {
  const ledger = ledgerWith();
  ledger.marginal = [];
  assert.equal(run(ledger).code, 1);
});

test("a crossing whose ends do not straddle the basis goes red", () => {
  const ledger = ledgerWith();
  ledger.crossings[0].exceedsRow = "small_baseline";
  assert.equal(run(ledger).code, 1);
});

test("a crossing with a third arm row between its ends goes red", () => {
  const readings = clone(passingReadings);
  readings.middle_arm = { mem: 2_000_000, cpu: 500_000_000 };
  const ledger = ledgerWith();
  ledger.modules[0].rows.middle_arm = {
    kind: "arm",
    sizeBytes: 150,
    mem: 2_000_000,
    cpu: 500_000_000,
    basisFit: "within",
  };
  assert.equal(run(ledger, { readings }).code, 1);
});

test("a crossing whose recorded bracket width is wrong is drift and goes red", () => {
  const ledger = ledgerWith();
  ledger.crossings[0].bracketWidthBytes = 99;
  assert.equal(run(ledger).code, 1);
});

test("a crossing read across two different modules goes red", () => {
  const readings = clone(passingReadings);
  const ledger = ledgerWith();
  ledger.modules[0].rows.big_arm.basisFit = "within";
  ledger.modules[0].rows.big_arm.mem = 1_500_000;
  delete ledger.modules[0].rows.big_arm.infeasibility;
  delete ledger.modules[0].rows.big_arm.ruling;
  readings.big_arm = { mem: 1_500_000, cpu: 900_000_000 };
  ledger.marginal[1].netBasisFit = "within";
  delete ledger.marginal[1].infeasibility;
  delete ledger.marginal[1].ruling;
  ledger.marginal[1].marginalMem = 1_200_000;
  ledger.marginal[1].marginalCpu = 810_000_000;
  // With nothing over the basis left, the crossing cannot be recomputed.
  assert.equal(run(ledger, { readings }).code, 1);
});

test("a ledger with no crossing at all goes red", () => {
  const ledger = ledgerWith();
  ledger.crossings = [];
  assert.equal(run(ledger).code, 1);
});

test("an interpolated point without a NOT MEASURED label goes red", () => {
  const ledger = ledgerWith();
  ledger.derived = [
    {
      claim: "example interpolation",
      dimension: "other bytes",
      from: "small_arm",
      to: "big_arm",
      label: "derived",
      derivation: "two-point linear interpolation of mem against sizeBytes",
      largestWithinBasisBytes: 193,
      basisExhaustedAtBytes: 194,
    },
  ];
  assert.equal(run(ledger).code, 1);
});

test("an interpolated point with a correct recomputation passes", () => {
  const ledger = ledgerWith();
  ledger.derived = [
    {
      claim: "example interpolation",
      dimension: "other bytes",
      from: "small_arm",
      to: "big_arm",
      label: "INTERPOLATED, NOT MEASURED",
      derivation: "two-point linear interpolation of mem against sizeBytes",
      largestWithinBasisBytes: 193,
      basisExhaustedAtBytes: 194,
    },
  ];
  assert.equal(run(ledger).code, 0);
});

test("an interpolated point whose integers are wrong is drift and goes red", () => {
  const ledger = ledgerWith();
  ledger.derived = [
    {
      claim: "example interpolation",
      dimension: "other bytes",
      from: "small_arm",
      to: "big_arm",
      label: "INTERPOLATED, NOT MEASURED",
      derivation: "two-point linear interpolation of mem against sizeBytes",
      largestWithinBasisBytes: 194,
      basisExhaustedAtBytes: 195,
    },
  ];
  assert.equal(run(ledger).code, 1);
});

test("an interpolated point standing on a dimension that has a measured crossing goes red", () => {
  const ledger = ledgerWith();
  ledger.derived = [
    {
      claim: "example interpolation",
      dimension: "example bytes",
      from: "small_arm",
      to: "big_arm",
      label: "INTERPOLATED, NOT MEASURED",
      derivation: "two-point linear interpolation of mem against sizeBytes",
      largestWithinBasisBytes: 193,
      basisExhaustedAtBytes: 194,
    },
  ];
  assert.equal(run(ledger).code, 1);
});

test("an interpolated point without derivation prose goes red", () => {
  const ledger = ledgerWith();
  ledger.derived = [
    {
      claim: "example interpolation",
      dimension: "other bytes",
      from: "small_arm",
      to: "big_arm",
      label: "INTERPOLATED, NOT MEASURED",
      derivation: "",
      largestWithinBasisBytes: 193,
      basisExhaustedAtBytes: 194,
    },
  ];
  assert.equal(run(ledger).code, 1);
});
