#!/usr/bin/env node

// Verifies Q60 (fault-proof-plan decision row D-S12): a committed block
// header's `end_time` is anchored to the commit transaction's bounded validity
// window, and the adjacent accepted/rejected bounds around that anchor are
// covered by tests that really execute.
//
// Two things this gate refuses to do, both of them defect classes this
// repository has already been bitten by:
//
//  1. It never counts declarations. Every published count comes from a runner
//     report — `aiken check` for the on-chain half and Vitest for the off-chain
//     half — so a renamed, deleted, skipped or throwing test cannot be counted
//     as a pass. Issue #519 finding V-1 is the precedent.
//
//  2. It never trusts a selector to be about what its name suggests. Aiken's
//     `-m` flag is a glob over test NAMES, not module names: before this task
//     the F05 manifest's Q60 selector `state_queue` collected six tests, none
//     of which touched the commit `end_time` bound at all. Each cited test is
//     therefore resolved to an exact (module, test-name) pair and must have
//     been collected from the module the artifact says it lives in.
//
// The artifact's numeric claims are checked structurally first (phase 1) and
// its citations executed second (phase 2), so the count contract is only
// published once both agree.
//
//  3. It never lets the family shrink back to the shape the Q60 audit caught it
//     in. Every fixture in the original family sat AT the commit window's
//     inclusive upper bound or beyond it, so replacing the rule's equality with
//     interval membership (`lower <= end_time <= upper`) passed all 21 tests
//     while three published claims asserted the family excluded exactly that.
//     The fixtures that close the gap are named in REQUIRED_AIKEN_SELECTORS and
//     REQUIRED_VITEST_TITLES below rather than read out of the artifact, so a
//     later edit cannot drop a claim and its evidence together and stay green,
//     and the artifact's recorded mutation experiment is checked for the shape
//     of a real measurement (two arms, mutant fails, real passes).
//
// usage: node demo/scripts/verify-canonical-v1-q60-commit-end-time-bound.mjs [--json] [--self-test]

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  aikenCompilerVersion,
  forkAikenBinary,
  aikenModuleName,
  aikenPublishedCommand,
  deriveAikenOutcome,
  deriveVitestOutcome,
  runAikenCheck,
  runVitest,
  vitestPublishedCommand,
} from "./lib/runner-reports.mjs";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");
const aikenProjectDirectory = "onchain/aiken";
const aikenProjectRoot = resolve(repositoryRoot, aikenProjectDirectory);
const artifactPath =
  "docs/exec-plans/evidence/canonical-v1-q60-commit-end-time-bound-v1.json";

const argv = process.argv.slice(2);
const emitJson = argv.includes("--json");
const selfTestMode = argv.includes("--self-test");

// Seeded defects for the self-test at the bottom. Each one perturbs the plan
// the gate is about to check, so the gate's own rejection path is exercised
// against a real run rather than asserted in prose.
const FAULTS = new Set([
  "accepted-beyond-bound",
  "drop-due-class",
  "phantom-aiken-selector",
  "phantom-vitest-title",
  "disposition-contradicts-rule",
  "drop-membership-witness",
  "drop-required-vitest-title",
  "forge-surviving-mutant",
]);
const fault = process.env.MIDGARD_Q60_FAULT;
if (fault !== undefined && !FAULTS.has(fault)) {
  console.error(`unknown MIDGARD_Q60_FAULT ${fault}`);
  process.exit(2);
}

// The fixtures that separate the anchor from interval membership. Named here,
// in the gate, and not read out of the artifact: an artifact edit can remove a
// class and its expected count in one stroke, and before the Q60 audit that
// would have left a green gate over a family that proved strictly less than it
// claimed. Removing any of these must fail instead.
const REQUIRED_AIKEN_SELECTORS = [
  "state_queue_commit_end_time_rejects_an_end_strictly_inside_the_commit_window",
  "state_queue_commit_end_time_never_accepts_any_value_inside_the_window",
];
const REQUIRED_VITEST_TITLES = [
  "rejects a header end_time strictly below the bound",
  "rejects every header end_time strictly inside the commit validity interval",
];

const evidence = JSON.parse(
  await readFile(resolve(repositoryRoot, artifactPath), "utf8"),
);

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-q60-commit-end-time-bound.v1",
  "unexpected Q60 artifact schema",
);
assert.deepEqual(evidence.goalIds, ["Q60"]);
assert.equal(evidence.decisionRow, "D-S12");

// ---------------------------------------------------------------------------
// Phase 0 — apply the seeded defect, if any.
// ---------------------------------------------------------------------------

const boundaryClasses = evidence.aiken.boundaryClasses.map((entry) => ({
  ...entry,
}));
const dueEventClasses = evidence.aiken.dueEventClasses.map((entry) => ({
  ...entry,
}));
const vitestTitles = [...evidence.vitest.titles];
const mutationExperiment = structuredClone(
  evidence.aiken.mutationExperiment ?? null,
);

if (fault === "drop-membership-witness") {
  const index = boundaryClasses.findIndex((entry) =>
    REQUIRED_AIKEN_SELECTORS.includes(entry.selector),
  );
  boundaryClasses.splice(index, 1);
}
if (fault === "drop-required-vitest-title") {
  const index = vitestTitles.indexOf(REQUIRED_VITEST_TITLES[0]);
  vitestTitles.splice(index, 1);
}
if (fault === "forge-surviving-mutant") {
  const mutantRun = mutationExperiment.runs.find(
    (entry) => entry.arm === "mutant",
  );
  mutantRun.passed = mutantRun.collected;
  mutantRun.failed = 0;
  mutantRun.failedSelectors = [];
}

if (fault === "accepted-beyond-bound") {
  const target = boundaryClasses.find(
    (entry) => entry.disposition === "accepted",
  );
  target.headerEndTimeMs = target.commitInclusiveUpperBoundMs + 1;
}
if (fault === "drop-due-class") {
  dueEventClasses.pop();
}
if (fault === "phantom-aiken-selector") {
  boundaryClasses[0].selector =
    "state_queue_commit_end_time_this_test_does_not_exist";
}
if (fault === "phantom-vitest-title") {
  vitestTitles.push("this vitest title does not exist");
}
if (fault === "disposition-contradicts-rule") {
  const target = boundaryClasses.find(
    (entry) => entry.disposition === "rejected",
  );
  target.headerEndTimeMs = target.commitInclusiveUpperBoundMs;
}

// ---------------------------------------------------------------------------
// Phase 1 — structural and numeric checks on what the artifact claims.
//
// The rule under test is an equality: a commit may carry exactly the inclusive
// upper bound of its own validity window. So every accepted case must sit ON
// the bound and every rejected case must sit off it. That makes "0 accepted
// cases permit end_time beyond the commit validity bound" a derived fact rather
// than an assertion the artifact is trusted about.
// ---------------------------------------------------------------------------

const failures = [];

const acceptedCases = boundaryClasses.filter(
  (entry) => entry.disposition === "accepted",
);
const rejectedCases = boundaryClasses.filter(
  (entry) => entry.disposition === "rejected",
);

for (const entry of boundaryClasses) {
  if (!["accepted", "rejected"].includes(entry.disposition)) {
    failures.push(
      `ERR_Q60_BAD_DISPOSITION: ${entry.class} has disposition ${String(entry.disposition)}`,
    );
  }
  if (
    !Number.isSafeInteger(entry.headerEndTimeMs) ||
    !Number.isSafeInteger(entry.commitInclusiveUpperBoundMs)
  ) {
    failures.push(
      `ERR_Q60_NON_INTEGER_VECTOR: ${entry.class} does not publish safe-integer millisecond values`,
    );
  }
}

const acceptedBeyondBound = acceptedCases.filter(
  (entry) => entry.headerEndTimeMs > entry.commitInclusiveUpperBoundMs,
);
if (acceptedBeyondBound.length !== 0) {
  failures.push(
    `ERR_Q60_ACCEPTED_BEYOND_BOUND: ${String(acceptedBeyondBound.length)} accepted case(s) carry an end_time past the commit validity bound — ${acceptedBeyondBound
      .map(
        (entry) =>
          `${entry.class} (end_time ${String(entry.headerEndTimeMs)} > bound ${String(entry.commitInclusiveUpperBoundMs)})`,
      )
      .join("; ")}`,
  );
}

for (const entry of acceptedCases) {
  if (entry.headerEndTimeMs !== entry.commitInclusiveUpperBoundMs) {
    failures.push(
      `ERR_Q60_ACCEPTED_OFF_ANCHOR: ${entry.class} is published as accepted but its end_time ${String(entry.headerEndTimeMs)} is not the inclusive upper bound ${String(entry.commitInclusiveUpperBoundMs)}`,
    );
  }
}
for (const entry of rejectedCases) {
  if (entry.headerEndTimeMs === entry.commitInclusiveUpperBoundMs) {
    failures.push(
      `ERR_Q60_REJECTED_ON_ANCHOR: ${entry.class} is published as rejected but its end_time equals the inclusive upper bound ${String(entry.commitInclusiveUpperBoundMs)}, which the rule accepts`,
    );
  }
}

// The two languages must be pinned to the same vector: Cardano's
// `invalid_hereafter` is exclusive, so validTo is one millisecond past the
// inclusive bound the header carries.
const vector = evidence.vector;
if (
  vector.commitValidity.transactionValidToMs !==
  vector.commitValidity.inclusiveUpperBoundMs + 1
) {
  failures.push(
    "ERR_Q60_VECTOR_INCOHERENT: transactionValidToMs must be one millisecond past the inclusive upper bound",
  );
}
if (
  vector.acceptedHeaderEndTimeMs !== vector.commitValidity.inclusiveUpperBoundMs
) {
  failures.push(
    "ERR_Q60_VECTOR_INCOHERENT: acceptedHeaderEndTimeMs must equal the inclusive upper bound",
  );
}
if (
  vector.immediatelyAboveBoundHeaderEndTimeMs !==
  vector.commitValidity.inclusiveUpperBoundMs + 1
) {
  failures.push(
    "ERR_Q60_VECTOR_INCOHERENT: immediatelyAboveBoundHeaderEndTimeMs must be the bound plus one millisecond",
  );
}
if (
  vector.farFutureHeaderEndTimeMs <= vector.commitValidity.inclusiveUpperBoundMs
) {
  failures.push(
    "ERR_Q60_VECTOR_INCOHERENT: farFutureHeaderEndTimeMs must be past the inclusive upper bound",
  );
}

// Declared class multiset must match the manifest's contract before anything
// is executed. Execution then proves each declaration really ran.
const expected = evidence.expectedCounts;
const declaredByClass = new Map();
for (const entry of boundaryClasses) {
  declaredByClass.set(entry.class, (declaredByClass.get(entry.class) ?? 0) + 1);
}
for (const className of [
  "lowerBoundControl",
  "maximumAcceptedEndTime",
  "immediatelyAboveBoundRejection",
  "farFutureRejection",
  "strictlyInsideWindowRejection",
]) {
  const declared = declaredByClass.get(className) ?? 0;
  if (declared !== expected[className]) {
    failures.push(
      `ERR_Q60_CLASS_COUNT: ${className} declares ${String(declared)} case(s), contract requires ${String(expected[className])}`,
    );
  }
}
const declaredDueClasses = dueEventClasses.map((entry) => entry.class);
if (declaredDueClasses.length !== expected.dueEventClasses) {
  failures.push(
    `ERR_Q60_DUE_CLASS_COUNT: ${String(declaredDueClasses.length)} due-event class(es) declared, contract requires ${String(expected.dueEventClasses)}`,
  );
}
for (const required of ["deposit", "withdrawal", "forced"]) {
  if (!declaredDueClasses.includes(required)) {
    failures.push(
      `ERR_Q60_DUE_CLASS_MISSING: the ${required} due-event class is not covered`,
    );
  }
}
if (expected.acceptedCasesPermittingEndTimeBeyondBound !== 0) {
  failures.push(
    "ERR_Q60_CONTRACT: the count contract must require 0 accepted cases beyond the bound",
  );
}

// The membership-exclusion fixtures must be cited by name, and at least one of
// them must sit strictly INSIDE the commit window. A case at or beyond the
// bound is satisfied by interval membership too, so a family without a strictly
// interior case cannot tell the two rules apart no matter how many cases it has.
const citedSelectorNames = new Set([
  ...boundaryClasses.map((entry) => entry.selector),
  ...dueEventClasses.map((entry) => entry.selector),
  ...evidence.aiken.supportingSelectors.map((entry) => entry.selector),
]);
for (const selector of REQUIRED_AIKEN_SELECTORS) {
  if (!citedSelectorNames.has(selector)) {
    failures.push(
      `ERR_Q60_REQUIRED_SELECTOR_MISSING: ${selector} is required to separate the commit anchor from interval membership but is not cited by the artifact`,
    );
  }
}
for (const title of REQUIRED_VITEST_TITLES) {
  if (!vitestTitles.includes(title)) {
    failures.push(
      `ERR_Q60_REQUIRED_TITLE_MISSING: the off-chain case "${title}" is required to separate \`=== validTo - 1\` from \`<= validTo - 1\` but is not cited by the artifact`,
    );
  }
}

const strictlyInsideCases = boundaryClasses.filter(
  (entry) =>
    entry.headerEndTimeMs > entry.commitInclusiveLowerBoundMs &&
    entry.headerEndTimeMs < entry.commitInclusiveUpperBoundMs,
);
if (strictlyInsideCases.length === 0) {
  failures.push(
    "ERR_Q60_NO_INTERIOR_CASE: no boundary case places header_end_time strictly inside the commit window, so the family cannot distinguish the anchor from interval membership",
  );
}
for (const entry of strictlyInsideCases) {
  if (entry.disposition !== "rejected") {
    failures.push(
      `ERR_Q60_INTERIOR_ACCEPTED: ${entry.class} places end_time ${String(entry.headerEndTimeMs)} strictly inside the commit window but publishes it as accepted; the rule accepts only the inclusive upper bound`,
    );
  }
}
if (
  vector.strictlyInsideHeaderEndTimeMs <=
    vector.commitValidity.inclusiveLowerBoundMs ||
  vector.strictlyInsideHeaderEndTimeMs >=
    vector.commitValidity.inclusiveUpperBoundMs
) {
  failures.push(
    "ERR_Q60_VECTOR_INCOHERENT: strictlyInsideHeaderEndTimeMs must lie strictly between the inclusive lower and upper bounds",
  );
}

// The recorded mutation experiment must have the shape of a real measurement:
// two arms over the same tree, the mutant killed by the required fixtures, the
// real rule passing clean. A survived mutant is the defect, not the evidence.
if (mutationExperiment === null) {
  failures.push(
    "ERR_Q60_NO_MUTATION_EXPERIMENT: the artifact must record the measured interval-membership mutation experiment",
  );
} else {
  const runs = mutationExperiment.runs ?? [];
  const arms = new Map(runs.map((entry) => [entry.arm, entry]));
  if (runs.length !== 2 || !arms.has("mutant") || !arms.has("real")) {
    failures.push(
      "ERR_Q60_MUTATION_ARMS: the mutation experiment must record exactly two runs, one `mutant` and one `real`",
    );
  } else {
    for (const [arm, run] of arms) {
      if (
        !Number.isSafeInteger(run.collected) ||
        !Number.isSafeInteger(run.passed) ||
        !Number.isSafeInteger(run.failed) ||
        run.collected <= 0
      ) {
        failures.push(
          `ERR_Q60_MUTATION_COUNTS: the ${arm} arm must publish safe-integer counts over a nonzero collection`,
        );
      } else if (run.passed + run.failed !== run.collected) {
        failures.push(
          `ERR_Q60_MUTATION_COUNTS: the ${arm} arm publishes passed ${String(run.passed)} + failed ${String(run.failed)} which does not total collected ${String(run.collected)}`,
        );
      }
    }
    const mutantRun = arms.get("mutant");
    const realRun = arms.get("real");
    if (mutantRun.collected !== realRun.collected) {
      failures.push(
        `ERR_Q60_MUTATION_ARMS: the arms collected ${String(mutantRun.collected)} and ${String(realRun.collected)} tests; a mutation experiment must run the same collection against both predicates`,
      );
    }
    if (mutantRun.failed < 1) {
      failures.push(
        "ERR_Q60_MUTANT_SURVIVED: the recorded mutant arm reports no failing test, so the family does not kill the interval-membership mutant",
      );
    }
    if (realRun.failed !== 0 || realRun.passed !== realRun.collected) {
      failures.push(
        `ERR_Q60_MUTATION_REAL_ARM_FAILED: the real arm must pass clean but reports ${String(realRun.failed)} failure(s)`,
      );
    }
    const killers = new Set(mutantRun.failedSelectors ?? []);
    for (const selector of REQUIRED_AIKEN_SELECTORS) {
      if (!killers.has(selector)) {
        failures.push(
          `ERR_Q60_MUTATION_KILLER: ${selector} is not recorded among the tests that fail under the mutant, so it is not doing the work the artifact credits it with`,
        );
      }
    }
    for (const selector of killers) {
      if (!citedSelectorNames.has(selector)) {
        failures.push(
          `ERR_Q60_MUTATION_KILLER: the mutant arm cites ${selector} as failing but the artifact does not cite that selector anywhere`,
        );
      }
    }
    const priorFamily = mutationExperiment.preRemediation;
    if (
      priorFamily !== undefined &&
      (!Number.isSafeInteger(priorFamily.collected) ||
        !Number.isSafeInteger(priorFamily.failedUnderMutant))
    ) {
      failures.push(
        "ERR_Q60_MUTATION_COUNTS: preRemediation must publish safe-integer counts",
      );
    }
  }
}

if (failures.length > 0) {
  for (const failure of failures) {
    console.error(failure);
  }
  process.exit(1);
}

// ---------------------------------------------------------------------------
// Phase 2 — execute every cited test and take the counts from the reports.
// ---------------------------------------------------------------------------

const aikenModule = aikenModuleName(evidence.aiken.modulePath);
const citedAiken = [
  ...boundaryClasses.map((entry) => ({
    kind: "boundary",
    key: entry.class,
    selector: entry.selector,
  })),
  ...dueEventClasses.map((entry) => ({
    kind: "dueEvent",
    key: entry.class,
    selector: entry.selector,
  })),
  ...evidence.aiken.supportingSelectors.map((entry) => ({
    kind: "supporting",
    key: entry.selector,
    selector: entry.selector,
  })),
];

const declaredAiken = citedAiken.map(({ selector }) => ({
  module: aikenModule,
  selector,
}));

const aikenSelectors = declaredAiken.map(
  ({ module, selector }) => `${module.split(".")[0]}.{${selector}}`,
);

// The stock v1.1.22 that compiles the blueprint must never execute this family;
// the patched fork is the test compiler. Local runs name it with
// MIDGARD_AIKEN_BIN and .github/workflows/aiken-ci.yml names it with
// MIDGARD_FORK_AIKEN_BIN, so both are honoured — but identity is measured, not
// assumed, so whichever binary is resolved must actually report the pinned fork
// version or the gate fails closed instead of running under the wrong compiler.
const testCompiler = evidence.aiken.testCompiler;
let aikenOutcome;
let aikenCompiler;
try {
  const binary = process.env.MIDGARD_AIKEN_BIN ?? forkAikenBinary();
  const version = aikenCompilerVersion(binary);
  aikenCompiler = version;
  if (!version.startsWith(testCompiler.requiredVersionPrefix)) {
    console.error(
      `ERR_Q60_WRONG_TEST_COMPILER: ${binary} reports "${version}" but this family must execute under "${testCompiler.requiredVersionPrefix}"; set ${testCompiler.binaryEnvironmentVariable} to the patched fork`,
    );
    process.exit(1);
  }
  const { report, status } = runAikenCheck({
    projectRoot: aikenProjectRoot,
    selectors: aikenSelectors,
    binary,
  });
  aikenOutcome = deriveAikenOutcome({
    label: "Q60 on-chain commit end_time bound",
    report,
    status,
    declared: declaredAiken,
  });
} catch (error) {
  console.error(
    `${error.code ?? "ERR_AIKEN"}: ${error instanceof Error ? error.message : String(error)}`,
  );
  process.exit(1);
}

const measuredSelectors = new Set(
  aikenOutcome.measured.map(({ selector }) => selector),
);

let vitestOutcome;
try {
  const { report, status } = runVitest({
    packageRoot: resolve(repositoryRoot, evidence.vitest.packageDirectory),
    testFile: evidence.vitest.testFile,
  });
  vitestOutcome = deriveVitestOutcome({
    label: "Q60 off-chain commit end_time mirror",
    report,
    status,
    requiredTitles: vitestTitles,
  });
} catch (error) {
  console.error(
    `${error.code ?? "ERR_VITEST"}: ${error instanceof Error ? error.message : String(error)}`,
  );
  process.exit(1);
}

// ---------------------------------------------------------------------------
// Phase 3 — publish counts taken from the measured results only.
// ---------------------------------------------------------------------------

const measuredCounts = {
  lowerBoundControl: 0,
  maximumAcceptedEndTime: 0,
  immediatelyAboveBoundRejection: 0,
  farFutureRejection: 0,
  strictlyInsideWindowRejection: 0,
  dueEventClasses: 0,
};
for (const entry of boundaryClasses) {
  if (measuredSelectors.has(entry.selector)) {
    measuredCounts[entry.class] += 1;
  }
}
for (const entry of dueEventClasses) {
  if (measuredSelectors.has(entry.selector)) {
    measuredCounts.dueEventClasses += 1;
  }
}

const countFailures = [];
for (const [className, requiredCount] of [
  ["lowerBoundControl", expected.lowerBoundControl],
  ["maximumAcceptedEndTime", expected.maximumAcceptedEndTime],
  ["immediatelyAboveBoundRejection", expected.immediatelyAboveBoundRejection],
  ["farFutureRejection", expected.farFutureRejection],
  [
    "strictlyInsideWindowRejection",
    expected.strictlyInsideWindowRejection,
  ],
  ["dueEventClasses", expected.dueEventClasses],
]) {
  if (measuredCounts[className] !== requiredCount) {
    countFailures.push(
      `ERR_Q60_MEASURED_COUNT: ${className} measured ${String(measuredCounts[className])} passing case(s), contract requires ${String(requiredCount)}`,
    );
  }
}

// Citation is not execution. The membership-exclusion fixtures must appear in
// the runner report as collected and passing, not merely be named above.
for (const selector of REQUIRED_AIKEN_SELECTORS) {
  if (!measuredSelectors.has(selector)) {
    countFailures.push(
      `ERR_Q60_REQUIRED_SELECTOR_NOT_MEASURED: ${selector} did not appear as a passing test in the Aiken runner report`,
    );
  }
}
// The off-chain half needs no equivalent loop here, and adding one would be a
// check that cannot fail: REQUIRED_VITEST_TITLES is asserted to be a subset of
// vitestTitles in phase 1, vitestTitles is handed to deriveVitestOutcome as its
// requiredTitles, and that helper throws ERR_FOCUSED_CHECK_TITLE_NOT_COLLECTED
// for any title the runner did not collect and ERR_FOCUSED_CHECK_FAILED /
// ERR_FOCUSED_CHECK_NOT_EXECUTED unless every collected test actually passed.
// Reaching this line already means both required titles ran and passed.

if (countFailures.length > 0) {
  for (const failure of countFailures) {
    console.error(failure);
  }
  process.exit(1);
}

// ---------------------------------------------------------------------------
// Self-test — the gate must reject each seeded defect and must still accept the
// untouched artifact, so it is demonstrably neither a gate that always passes
// nor a gate that always fails.
// ---------------------------------------------------------------------------

if (selfTestMode) {
  const runSeeded = (seededFault) =>
    spawnSync(process.execPath, [fileURLToPath(import.meta.url)], {
      cwd: repositoryRoot,
      encoding: "utf8",
      env: { ...process.env, MIDGARD_Q60_FAULT: seededFault },
      maxBuffer: 64 * 1024 * 1024,
    });

  const seeded = [
    ["accepted-beyond-bound", /ERR_Q60_ACCEPTED_BEYOND_BOUND/u],
    ["drop-due-class", /ERR_Q60_DUE_CLASS_(COUNT|MISSING)/u],
    ["phantom-aiken-selector", /ERR_AIKEN_SELECTOR_NOT_COLLECTED/u],
    ["phantom-vitest-title", /ERR_FOCUSED_CHECK_TITLE_NOT_COLLECTED/u],
    ["disposition-contradicts-rule", /ERR_Q60_REJECTED_ON_ANCHOR/u],
    ["drop-membership-witness", /ERR_Q60_REQUIRED_SELECTOR_MISSING/u],
    ["drop-required-vitest-title", /ERR_Q60_REQUIRED_TITLE_MISSING/u],
    ["forge-surviving-mutant", /ERR_Q60_MUTANT_SURVIVED/u],
  ];
  for (const [seededFault, expectedDiagnostic] of seeded) {
    const result = runSeeded(seededFault);
    assert.notEqual(
      result.status,
      0,
      `Q60 gate accepted the seeded defect ${seededFault}`,
    );
    assert.match(
      result.stderr,
      expectedDiagnostic,
      `Q60 gate rejected ${seededFault} without its specific diagnostic (stderr: ${result.stderr})`,
    );
  }
  console.log(
    `Q60 self-test: ${String(seeded.length)} seeded defects all rejected with their specific diagnostics`,
  );
}

const report = {
  status: "PASS",
  goalIds: evidence.goalIds,
  decisionRow: evidence.decisionRow,
  direction: evidence.direction.chosen,
  vector: {
    commitInclusiveLowerBoundMs: vector.commitValidity.inclusiveLowerBoundMs,
    commitInclusiveUpperBoundMs: vector.commitValidity.inclusiveUpperBoundMs,
    transactionValidToMs: vector.commitValidity.transactionValidToMs,
    acceptedHeaderEndTimeMs: vector.acceptedHeaderEndTimeMs,
    immediatelyAboveBoundHeaderEndTimeMs:
      vector.immediatelyAboveBoundHeaderEndTimeMs,
    farFutureHeaderEndTimeMs: vector.farFutureHeaderEndTimeMs,
  },
  measuredCounts,
  acceptedCasesPermittingEndTimeBeyondBound: acceptedBeyondBound.length,
  membershipExclusion: {
    requiredAikenSelectors: REQUIRED_AIKEN_SELECTORS,
    requiredVitestTitles: REQUIRED_VITEST_TITLES,
    strictlyInsideCases: strictlyInsideCases.length,
    mutationExperiment: {
      mutant: mutationExperiment.mutant.id,
      mutantArm: mutationExperiment.runs.find(
        (entry) => entry.arm === "mutant",
      ),
      realArm: mutationExperiment.runs.find((entry) => entry.arm === "real"),
    },
  },
  aiken: {
    command: aikenPublishedCommand({
      projectDirectory: aikenProjectDirectory,
      selectors: aikenSelectors,
    }),
    compiler: aikenCompiler,
    collected: aikenOutcome.collected,
    passed: aikenOutcome.passed,
    citedSelectors: declaredAiken.length,
  },
  vitest: {
    command: vitestPublishedCommand({
      packageDirectory: evidence.vitest.packageDirectory,
      testFile: evidence.vitest.testFile,
    }),
    collected: vitestOutcome.collected,
    passed: vitestOutcome.passed,
    citedTitles: vitestTitles.length,
  },
  blueprintImpact: evidence.blueprintImpact,
  openParentActions: evidence.openParentActions,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q60 commit end_time bound: PASS (${String(aikenOutcome.passed)}/${String(aikenOutcome.collected)} Aiken selectors, ${String(vitestOutcome.passed)}/${String(vitestOutcome.collected)} Vitest tests, ` +
      `${String(measuredCounts.lowerBoundControl)} lower-bound control, ${String(measuredCounts.maximumAcceptedEndTime)} maximum accepted end_time, ` +
      `${String(measuredCounts.immediatelyAboveBoundRejection)} immediately-above-bound rejection, ${String(measuredCounts.farFutureRejection)} far-future rejection, ` +
      `${String(measuredCounts.strictlyInsideWindowRejection)} strictly-inside-window rejection, ` +
      `${String(measuredCounts.dueEventClasses)} due-event classes, ${String(acceptedBeyondBound.length)} accepted cases beyond the bound; ` +
      `interval-membership mutant killed ${String(report.membershipExclusion.mutationExperiment.mutantArm.failed)}/${String(report.membershipExclusion.mutationExperiment.mutantArm.collected)} vs real ${String(report.membershipExclusion.mutationExperiment.realArm.passed)}/${String(report.membershipExclusion.mutationExperiment.realArm.collected)})`,
  );
}
