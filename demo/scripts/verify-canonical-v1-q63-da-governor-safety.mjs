#!/usr/bin/env node

// Verifies the Q63 (decision row D-DA5) DA-governor safety artifact against the
// final tree.
//
// Q63 binds numbers it is not allowed to invent: the governed threshold floors
// come from the F04 economics decision record
// (`docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md` §4)
// and nowhere else. This gate therefore re-reads F04 at the exact cited lines
// and fails closed if the quoted floor text drifted, so a future edit to the
// decision record cannot leave the validator silently enforcing a number the
// owner never accepted.
//
// Every published count is recomputed from an executed runner report. Nothing
// here reads test source or counts declarations: the cited Aiken selectors run
// in one `aiken check -e` invocation and the cited Vitest titles run through the
// SDK package's own Vitest CLI. A selector that collects nothing, a test that
// fails, a test that never executed, and a citation the runner never collected
// each fail this gate with a distinct diagnostic.
//
// Q63's acceptance has three clauses. (a) governed lower bounds and (b)
// owner-set drain protection are delivered and measured here. (c) the
// partial-attestation rescue/refund path is NOT: it needs new `MintRedeemer`
// and `SpendRedeemer` constructors, and both enums live in Q62's leased
// `onchain/aiken/lib/midgard/da-attestation-types.ak`. Those groups are
// recorded OPEN with their blocker and this gate exits non-zero while any
// acceptance group is still open, so an incomplete Q63 can never read as a
// green gate.
//
// usage: node demo/scripts/verify-canonical-v1-q63-da-governor-safety.mjs [--json]

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { runFixtureMode } from "./lib/runner-fixtures.mjs";
import {
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

// Fixture mode runs one seeded defect and nothing else, so the negative
// self-tests at the bottom can spawn this very gate and observe its real exit
// code. It must run before any artifact is read.
//
// The fixture package root only supplies a Vitest CLI for the seeded synthetic
// projects; it is deliberately not the package this gate verifies. `midgard-sdk`
// resolves a Vitest whose "no matching file" path exits before writing a JSON
// report, which collapses the distinct `ERR_FOCUSED_CHECK_NO_FILES` diagnostic
// into `ERR_FOCUSED_CHECK_NO_REPORT` and would blunt the self-test.
runFixtureMode({
  argv: process.argv.slice(2),
  packageRoot: resolve(repositoryRoot, "demo/midgard-validation"),
});

const emitJson = process.argv.slice(2).includes("--json");

const readRepositoryFile = async (relativePath) =>
  readFile(resolve(repositoryRoot, relativePath), "utf8");

const evidence = JSON.parse(
  await readRepositoryFile(
    "docs/exec-plans/evidence/canonical-v1-q63-da-governor-safety-v1.json",
  ),
);

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-q63-da-governor-safety.v1",
  "unexpected Q63 evidence schema",
);
assert.equal(evidence.version, 1);
assert.deepEqual(evidence.goalIds, ["Q63"]);
assert.equal(evidence.decisionRow, "D-DA5");

// ---------------------------------------------------------------------------
// F04 is the only source of Q63's numbers. Re-read it at the cited lines.
// ---------------------------------------------------------------------------

const decisionLines = (
  await readRepositoryFile(evidence.decisionSource.path)
).split(/\r?\n/u);

assert.ok(
  Array.isArray(evidence.decisionSource.boundValues) &&
    evidence.decisionSource.boundValues.length > 0,
  "the artifact must name the F04 values Q63 binds",
);
for (const bound of evidence.decisionSource.boundValues) {
  assert.equal(
    decisionLines[bound.line - 1],
    bound.quote,
    `F04 ${evidence.decisionSource.path}:${String(bound.line)} no longer reads as the quoted ${bound.name} source, so Q63's bound value is unprovenanced`,
  );
}

// ---------------------------------------------------------------------------
// Phase 1 — collect what the artifact declares. Nothing here decides whether a
// check passed; the declarations are only the plan the runners execute.
// ---------------------------------------------------------------------------

const declaredAikenChecks = [];
const declaredVitestFiles = new Map();
const groupPlans = [];

assert.ok(
  Array.isArray(evidence.groups) && evidence.groups.length > 0,
  "the artifact declares no measured groups",
);

for (const group of evidence.groups) {
  assert.equal(
    group.disposition,
    "PASS",
    `group ${group.id} is published as a measured group but is not PASS`,
  );
  assert.ok(
    typeof group.claim === "string" && group.claim.length > 0,
    `group ${group.id} must state the claim it proves`,
  );
  assert.ok(
    Array.isArray(group.evidence) && group.evidence.length > 0,
    `group ${group.id} has no executable evidence (prose-only closure is forbidden)`,
  );

  const plan = { id: group.id, expected: group.expected, aiken: 0, vitest: 0 };
  for (const item of group.evidence) {
    if (item.kind === "aiken") {
      assert.ok(
        Array.isArray(item.selectors) && item.selectors.length > 0,
        `group ${group.id} cites ${item.module} with no selectors`,
      );
      const module = aikenModuleName(item.module);
      for (const selector of item.selectors) {
        assert.ok(
          /^[a-z0-9_]+$/u.test(selector),
          `selector ${selector} is not a focused-check-safe name`,
        );
        declaredAikenChecks.push({ module, selector, source: item.module });
        plan.aiken += 1;
      }
    } else if (item.kind === "vitest") {
      assert.ok(
        Array.isArray(item.titles) && item.titles.length > 0,
        `group ${group.id} cites ${item.file} with no test titles`,
      );
      const packageDirectory = item.file.split("/").slice(0, 2).join("/");
      const testFile = item.file.split("/").slice(2).join("/");
      const existing = declaredVitestFiles.get(item.file) ?? {
        file: item.file,
        packageDirectory,
        testFile,
        titles: [],
      };
      existing.titles.push(...item.titles);
      declaredVitestFiles.set(item.file, existing);
      plan.vitest += item.titles.length;
    } else {
      throw new Error(
        `unknown evidence kind ${String(item.kind)} in group ${group.id}`,
      );
    }
  }

  // Each language must independently carry the group's full cardinality: the
  // floors are a cross-language pair, so one side alone is not the claim.
  assert.equal(
    plan.aiken,
    group.expected,
    `group ${group.id} cites ${String(plan.aiken)} on-chain selectors but claims ${String(group.expected)}`,
  );
  assert.equal(
    plan.vitest,
    group.expected,
    `group ${group.id} cites ${String(plan.vitest)} off-chain titles but claims ${String(group.expected)}`,
  );
  groupPlans.push(plan);
}

// ---------------------------------------------------------------------------
// Phase 2 — execute.
// ---------------------------------------------------------------------------

const aikenSelectors = declaredAikenChecks.map(({ selector }) => selector);
assert.equal(
  new Set(aikenSelectors).size,
  aikenSelectors.length,
  "the artifact cites the same Aiken selector twice",
);

const aikenOutcome = deriveAikenOutcome({
  label: "Q63 DA-governor safety on-chain selectors",
  declared: declaredAikenChecks,
  ...runAikenCheck({
    projectRoot: aikenProjectRoot,
    selectors: aikenSelectors,
  }),
});
assert.equal(
  aikenOutcome.passed,
  declaredAikenChecks.length,
  "every cited on-chain selector must be measured as passing",
);

const vitestOutcomes = new Map();
for (const declaration of declaredVitestFiles.values()) {
  const outcome = deriveVitestOutcome({
    label: `Q63 DA-governor safety ${declaration.file}`,
    requiredTitles: declaration.titles,
    ...runVitest({
      packageRoot: resolve(repositoryRoot, declaration.packageDirectory),
      testFile: declaration.testFile,
    }),
  });
  vitestOutcomes.set(declaration.file, outcome);
}

const vitestChecksExecuted = [...declaredVitestFiles.values()].reduce(
  (total, declaration) => total + declaration.titles.length,
  0,
);
const executedChecks = aikenOutcome.passed + vitestChecksExecuted;

const groupTotal = (id) => {
  const plan = groupPlans.find((candidate) => candidate.id === id);
  return plan === undefined ? 0 : plan.expected;
};

const recomputedSummary = {
  groups: evidence.groups.length,
  openGroups: evidence.openGroups.length,
  governedFloorDrainInvariants: groupTotal("governed-floor-drain-invariants"),
  validBoundaryControls: groupTotal("valid-boundary-controls"),
  belowFloorDrainRejectionClasses: groupTotal("below-floor-drain-rejections"),
  partialAttestationRescuePaths: groupTotal("partial-attestation-rescue-path"),
  rescueTheftDuplicateReplayRejectionClasses: groupTotal(
    "rescue-theft-duplicate-replay-rejections",
  ),
  aikenChecksExecuted: aikenOutcome.passed,
  vitestChecksExecuted,
  executedChecks,
  runners: [
    aikenPublishedCommand({
      projectDirectory: aikenProjectDirectory,
      selectors: aikenSelectors,
    }),
    ...[...declaredVitestFiles.values()].map((declaration) =>
      vitestPublishedCommand({
        packageDirectory: declaration.packageDirectory,
        testFile: declaration.testFile,
      }),
    ),
  ],
};

assert.deepEqual(
  evidence.summary,
  recomputedSummary,
  "the recorded summary disagrees with what the runners measured",
);

// ---------------------------------------------------------------------------
// Structural claims: Q63 is a governed safety/liveness fix, not a catalogue
// family, and F04 is a decision record that authorises numbers only.
// ---------------------------------------------------------------------------

assert.deepEqual(
  evidence.structural.standaloneCatalogueIds,
  [],
  "Q63 must not introduce a standalone catalogue ID",
);
assert.equal(
  evidence.structural.implementationOrLiveClaimsFromF04,
  0,
  "no implementation or live-readiness claim may be derived solely from F04",
);
assert.equal(evidence.parentIntegration.owner, "parent");
assert.ok(
  evidence.parentIntegration.pendingEdits.length > 0,
  "the parent handoff must list the edits it owns",
);

// Open acceptance groups must name a real blocker rather than simply vanish.
for (const group of evidence.openGroups) {
  assert.equal(
    group.disposition,
    "OPEN",
    `group ${group.id} sits in openGroups without an OPEN disposition`,
  );
  assert.ok(
    typeof group.blockedOn?.reason === "string" &&
      group.blockedOn.reason.length > 0,
    `open group ${group.id} must state why it is blocked`,
  );
  assert.ok(
    typeof group.blockedOn?.leasedPath === "string" &&
      group.blockedOn.leasedPath.length > 0,
    `open group ${group.id} must name the path whose lease blocks it`,
  );
}

assert.equal(
  evidence.acceptanceComplete,
  evidence.openGroups.length === 0,
  "acceptanceComplete must agree with whether any acceptance group is still open",
);

// ---------------------------------------------------------------------------
// Negative self-tests: spawn this gate against seeded fixtures and require a
// non-zero exit carrying the specific diagnostic, then positive controls so
// the rejections cannot be a gate that rejects everything.
// ---------------------------------------------------------------------------

const runSelfTest = (flag) =>
  spawnSync(process.execPath, [fileURLToPath(import.meta.url), flag], {
    cwd: repositoryRoot,
    encoding: "utf8",
    maxBuffer: 128 * 1024 * 1024,
  });

const selfTests = [
  [
    "--vitest-fixture=failing",
    /ERR_FOCUSED_CHECK_FAILED: .*executes and fails/su,
  ],
  [
    "--vitest-fixture=zero-collection",
    /ERR_FOCUSED_CHECK_ZERO_COLLECTION: .*collected 0 tests/su,
  ],
  [
    "--vitest-fixture=skipped",
    /ERR_FOCUSED_CHECK_NOT_EXECUTED: .*never executed/su,
  ],
  [
    "--vitest-fixture=renamed-title",
    /ERR_FOCUSED_CHECK_TITLE_NOT_COLLECTED: .*executes and passes/su,
  ],
  [
    "--vitest-fixture=missing-file",
    /ERR_FOCUSED_CHECK_NO_FILES: .*matched no test file/su,
  ],
  ["--aiken-fixture=failing", /ERR_AIKEN_CHECK_FAILED: .*selftest_probe/su],
  [
    "--aiken-fixture=zero-collection",
    /ERR_AIKEN_ZERO_COLLECTION: .*collected 0 tests/su,
  ],
  [
    "--aiken-fixture=missing-selector",
    /ERR_AIKEN_SELECTOR_NOT_COLLECTED: .*selftest_probe_absent/su,
  ],
  [
    "--aiken-fixture=module-mismatch",
    /ERR_AIKEN_SELECTOR_MODULE_MISMATCH: .*selftest\/elsewhere/su,
  ],
];
for (const [flag, expectedDiagnostic] of selfTests) {
  const selfTest = runSelfTest(flag);
  assert.notEqual(
    selfTest.status,
    0,
    `Q63 DA-governor safety gate accepted the seeded defect ${flag}`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `Q63 DA-governor safety gate rejected ${flag} without its specific diagnostic`,
  );
}
for (const [flag, expectedStdout] of [
  ["--vitest-fixture=passing", /vitest fixture passing: 1\/1 passed/u],
  ["--aiken-fixture=passing", /aiken fixture passing: 1\/1 passed/u],
]) {
  const control = runSelfTest(flag);
  assert.equal(
    control.status,
    0,
    `Q63 DA-governor safety gate rejected a passing fixture (${flag}): ${control.stderr}`,
  );
  assert.match(control.stdout, expectedStdout);
}

const openGroupIds = evidence.openGroups.map((group) => group.id);
const status = openGroupIds.length === 0 ? "PASS" : "INCOMPLETE";

const report = {
  status,
  goalIds: evidence.goalIds,
  decisionRow: evidence.decisionRow,
  governedFloorDrainInvariants: recomputedSummary.governedFloorDrainInvariants,
  validBoundaryControls: recomputedSummary.validBoundaryControls,
  belowFloorDrainRejectionClasses:
    recomputedSummary.belowFloorDrainRejectionClasses,
  partialAttestationRescuePaths:
    recomputedSummary.partialAttestationRescuePaths,
  rescueTheftDuplicateReplayRejectionClasses:
    recomputedSummary.rescueTheftDuplicateReplayRejectionClasses,
  aikenChecksExecuted: aikenOutcome.passed,
  aikenSelectorsCollected: aikenOutcome.collected,
  vitestChecksExecuted,
  executedChecks,
  standaloneCatalogueIds: evidence.structural.standaloneCatalogueIds.length,
  implementationOrLiveClaimsFromF04:
    evidence.structural.implementationOrLiveClaimsFromF04,
  openGroups: openGroupIds,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q63 DA-governor safety: ${status} (${String(recomputedSummary.governedFloorDrainInvariants)} governed floor/drain invariants, ${String(recomputedSummary.validBoundaryControls)} valid-boundary controls, ${String(recomputedSummary.belowFloorDrainRejectionClasses)} below-floor/drain rejection classes, ${String(recomputedSummary.partialAttestationRescuePaths)} rescue paths, ${String(recomputedSummary.rescueTheftDuplicateReplayRejectionClasses)} rescue rejection classes; ${String(executedChecks)} runner-executed checks, ${String(evidence.structural.standaloneCatalogueIds.length)} standalone catalogue IDs, ${String(evidence.structural.implementationOrLiveClaimsFromF04)} F04-derived implementation/live claims)`,
  );
}

if (openGroupIds.length > 0) {
  console.error(
    `Q63_INCOMPLETE: acceptance clause (c) is unmet — ${openGroupIds.join(", ")} remain OPEN, blocked on the ${evidence.openGroups[0].blockedOn.leasedPath} lease held by ${evidence.openGroups[0].blockedOn.leaseHolder}`,
  );
  process.exit(1);
}
