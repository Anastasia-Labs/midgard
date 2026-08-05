#!/usr/bin/env node
// Verifies the Q10/Q11/Q12/Q14 binding of GOAL_SPEC.md 9.1 outputs 5 to 9.
//
// The rule enforced here: a per-family output may be published LOCAL_PASS only
// when the artifact's claim is re-derivable from the tree, and an OPEN cell
// must say why it is open and who owns it. Every catalogue index, category id,
// blueprint title, builder name, CLI verb, module resumability shape, vitest
// title and proof-fit threshold this artifact names is re-read out of its
// source file, so the artifact cannot drift away from the code.
//
// Issue #533 (finding V-2 of #519). Existence used to be accepted in place of
// passage for every test this artifact cites: the shared DA-first gate's
// `{tests: 32, passed: 32, failed: 0}` was validated by `assert.equal(passed,
// tests)` — two fields of the same JSON object, true by construction — with the
// denominator itself substituted by counting `it("` occurrences in the suite
// source, and each output-9 emulator lifecycle was credited to a family purely
// because `it("<title>"` appeared in the file. A throwing body, an `it.skip`,
// or a deleted assertion left all of it untouched.
//
// Every published count and every cited lifecycle now comes from a Vitest JSON
// report produced by spawning the owning package's own runner: a test that
// fails, never executes, or is no longer collected under its cited title fails
// this gate closed with a distinct diagnostic. Source scanning survives only
// where it backs no count — the builder, verb, resumability and proof-fit
// structural claims.
//
// usage: node demo/scripts/verify-canonical-v1-proof-family-q1x.mjs [--json]

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { runFixtureMode } from "./lib/runner-fixtures.mjs";
import {
  deriveVitestOutcome,
  runVitest,
  vitestPublishedCommand,
} from "./lib/runner-reports.mjs";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");

// Fixture mode runs one seeded defect and nothing else, so the negative
// self-tests at the bottom can spawn this very gate and observe its real exit
// code. It must run before any artifact is read.
runFixtureMode({
  argv: process.argv.slice(2),
  packageRoot: resolve(repositoryRoot, "demo/midgard-fault-proofs"),
});

const emitJson = process.argv.slice(2).includes("--json");

const fileCache = new Map();
const readRepositoryFile = async (relativePath) => {
  const cached = fileCache.get(relativePath);
  if (cached !== undefined) {
    return cached;
  }
  const contents = await readFile(
    resolve(repositoryRoot, relativePath),
    "utf8",
  );
  fileCache.set(relativePath, contents);
  return contents;
};

const evidence = JSON.parse(
  await readRepositoryFile(
    "docs/exec-plans/evidence/canonical-v1-proof-family-q1x-v1.json",
  ),
);

const GOAL_IDS = ["Q10", "Q11", "Q12", "Q14"];
const BOUND_OUTPUTS = [5, 6, 7, 8, 9];
const L1_MAX_TX_SIZE = 16_384;

// ## Identity

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-proof-family-q1x.v1",
  "evidence schema identity drifted",
);
assert.equal(evidence.version, 1, "evidence version drifted");
assert.deepEqual(evidence.goalIds, GOAL_IDS, "bound goal ids drifted");
assert.equal(evidence.issue, 482, "issue binding drifted");

// Outputs 1 to 4 are inherited from issue #481 and must never be re-claimed.
assert.deepEqual(
  evidence.inheritedOutputs.outputs,
  [1, 2, 3, 4],
  "outputs 1 to 4 are the inherited set",
);
assert.equal(
  evidence.inheritedOutputs.closedBy,
  481,
  "outputs 1 to 4 were closed at issue #481",
);
for (const goalId of GOAL_IDS) {
  assert.ok(
    typeof evidence.inheritedOutputs.measured[goalId] === "string",
    `inherited measurement for ${goalId} is missing`,
  );
}

// ## Proof-fit thresholds must match GOAL_SPEC.md 3.3 and the helper in use

assert.equal(
  evidence.proofFitThresholds.byteFit.limit,
  L1_MAX_TX_SIZE,
  "the L1 byte-fit limit drifted from 16,384",
);
assert.equal(
  evidence.proofFitThresholds.executionFit.reservePercent,
  20,
  "GOAL_SPEC.md 3.3 item 2 requires a 20% execution reserve",
);
let thresholdEnforcementChecks = 0;
for (const enforcement of evidence.proofFitThresholds.enforcedBy) {
  const source = await readRepositoryFile(enforcement.file);
  assert.ok(
    source.includes(`const ${enforcement.helper} = ({`),
    `${enforcement.file} no longer declares the proof-fit helper ${enforcement.helper}`,
  );
  // The byte-fit check must be a non-negative L1 margin, not a raw byte count
  // against the emulator's relaxed ceiling.
  assert.ok(
    source.includes("measurement.l1ByteMargin") &&
      source.includes("toBeGreaterThanOrEqual(0)"),
    `${enforcement.file} does not assert a non-negative l1ByteMargin`,
  );
  // The reserve must be applied; asserting against the raw limit is a failure.
  assert.ok(
    source.includes("const EXECUTION_RESERVE_FRACTION = 20n") &&
      source.includes("100n - EXECUTION_RESERVE_FRACTION"),
    `${enforcement.file} does not apply the 20% execution reserve`,
  );
  assert.ok(
    source.includes("measurement.executionMemory <= memoryCeiling") &&
      source.includes("measurement.executionSteps <= stepCeiling"),
    `${enforcement.file} does not check both execution memory and execution steps against the reserve ceiling`,
  );
  for (const goalId of enforcement.families) {
    assert.ok(
      GOAL_IDS.includes(goalId),
      `proof-fit enforcement names unknown goal id ${goalId}`,
    );
  }
  thresholdEnforcementChecks += 1;
}
assert.ok(
  thresholdEnforcementChecks > 0,
  "no proof-fit enforcement site was checked, so the threshold claim proves nothing",
);

// ## Catalogue order is the single source of the append-only ids

const catalogueSource = await readRepositoryFile(
  "demo/midgard-sdk/src/fraud-proof/catalogue.ts",
);
const orderBlock = catalogueSource.slice(
  catalogueSource.indexOf("FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = ["),
);
const orderedCategories = [
  ...orderBlock.slice(0, orderBlock.indexOf("]")).matchAll(/"([a-zA-Z]+)"/gu),
].map((match) => match[1]);
assert.ok(
  orderedCategories.length > 0,
  "could not read FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
);

const contractsSource = await readRepositoryFile(
  "demo/midgard-sdk/src/fraud-proof/contracts.ts",
);
const binSource = await readRepositoryFile(
  "demo/midgard-fault-proofs/src/bin.ts",
);
const evidenceBuilderSource = await readRepositoryFile(
  evidence.sharedDaFirstEvidenceGate.module,
);

// ## Per-family checks

assert.deepEqual(
  evidence.families.map((family) => family.goalId),
  GOAL_IDS,
  "the four families must be listed exactly once, in order",
);

let familyChecks = 0;
let measuredStages = 0;
let maximumStages = 0;
let worstByteMargin = Number.POSITIVE_INFINITY;
let worstMaximumByteMargin = Number.POSITIVE_INFINITY;
let lowestExhaustionLevel = Number.POSITIVE_INFINITY;

// ## The adversarial depth bound
//
// The maximum-fixture arithmetic below is only as good as the marginal cost it
// extrapolates, so that constant is checked against the two-depth measurement
// it comes from before any family is allowed to use it. The distinction
// between the MPF library's own CBOR cost and the cost in the complete signed
// transaction is enforced here too: substituting the smaller number would
// halve the reported exposure.
const bound = evidence.adversarialDepthBound;
assert.ok(
  bound !== undefined && typeof bound === "object",
  "the maximum fixture's depth bound must be recorded, not left implicit",
);
const ADVERSARIAL_BRANCH_LEVELS = bound.deepBranchLevels;
assert.ok(
  Number.isInteger(ADVERSARIAL_BRANCH_LEVELS) &&
    ADVERSARIAL_BRANCH_LEVELS > bound.shallowBranchLevels,
  "the depth bound must be measured at two distinct depths",
);
assert.equal(
  bound.proofTransactionBranchLevelBytes,
  (bound.deepStep01Bytes - bound.shallowStep01Bytes) /
    (bound.deepBranchLevels - bound.shallowBranchLevels),
  "the pinned per-level transaction cost is not the difference between the two measured depths",
);
assert.ok(
  bound.proofTransactionBranchLevelBytes > bound.mpfBranchProofStepCborBytes,
  "the transaction-level cost must exceed the MPF CBOR cost; if they were equal the Plutus-data expansion would have been silently dropped",
);
assert.equal(
  bound.byteCeiling,
  ADVERSARIAL_BRANCH_LEVELS +
    Math.floor(
      (L1_MAX_TX_SIZE - bound.deepStep01Bytes) /
        bound.proofTransactionBranchLevelBytes,
    ),
  "the published byte ceiling is not the one its own measured transaction implies",
);
assert.equal(
  bound.bindingEnvelope,
  "bytes",
  "the binding envelope must be named explicitly",
);
assert.ok(
  bound.byteCeiling < bound.executionMemoryCeiling &&
    bound.byteCeiling < bound.executionStepCeiling,
  "byte fit is published as the binding envelope but is not the smallest of the three measured ceilings",
);
assert.equal(
  bound.referenceAdversaryBranchLevelReach,
  Math.floor(bound.referenceAdversaryLog2Work / 4),
  "the reference adversary's reach is not 2^(4i) work per level",
);
assert.equal(
  bound.log2WorkToExhaustEnvelope,
  4 * bound.byteCeiling,
  "the work needed to exhaust the envelope is not 4 bits per forced level",
);
assert.equal(
  bound.envelopeExhaustibleByReferenceAdversary,
  bound.byteCeiling < bound.referenceAdversaryBranchLevelReach,
  "the exhaustibility claim disagrees with the ceilings it is drawn from",
);

const maximumSuite = evidence.maximumFixtureSuite;
assert.ok(
  maximumSuite !== undefined,
  "the maximum-fixture suite must be named so the runner can execute it",
);
for (const goalId of GOAL_IDS) {
  assert.ok(
    maximumSuite.titles.includes(maximumSuite.familyTitles[goalId]),
    `the maximum-fixture suite does not declare a lifecycle title for ${goalId}`,
  );
}

// Declarations only: which titles each suite is claimed to contain. Nothing
// here decides whether any of them passed; the runner does that below.
const declaredVitestFiles = new Map();
const declareVitest = (file, titles) => {
  const existing = declaredVitestFiles.get(file) ?? {
    file,
    packageDirectory: file.split("/").slice(0, 2).join("/"),
    testFile: file.split("/").slice(2).join("/"),
    titles: [],
  };
  existing.titles.push(...titles);
  declaredVitestFiles.set(file, existing);
  return existing;
};

for (const family of evidence.families) {
  const where = `${family.goalId} (${family.family})`;

  // -- output 6: catalogue identity and deployed step titles
  assert.equal(
    orderedCategories.indexOf(family.catalogueCategory),
    family.categoryIndex,
    `${where} catalogue index drifted from the append-only order`,
  );
  const derivedId = family.categoryIndex.toString(16).padStart(8, "0");
  assert.equal(
    family.categoryId,
    derivedId,
    `${where} category id ${family.categoryId} is not the 4-byte big-endian encoding of index ${String(family.categoryIndex)}`,
  );
  assert.equal(
    family.blueprintTitles.length,
    family.stepCount,
    `${where} declares ${String(family.stepCount)} steps but lists ${String(family.blueprintTitles.length)} blueprint titles`,
  );
  assert.ok(
    contractsSource.includes(`${family.titlesConstant} = {`),
    `${where} names titles constant ${family.titlesConstant}, which contracts.ts does not declare`,
  );
  for (const title of family.blueprintTitles) {
    assert.ok(
      contractsSource.includes(`"${title}"`),
      `${where} claims blueprint title ${title}, which contracts.ts does not reference`,
    );
  }

  // -- output 7: the DA-first builder must exist and route through the gate
  assert.ok(
    evidenceBuilderSource.includes(`export const ${family.daFirstBuilder} =`),
    `${where} claims builder ${family.daFirstBuilder}, which is not exported from ${evidence.sharedDaFirstEvidenceGate.module}`,
  );

  // -- output 8: exactly one prepare verb, one submit verb per step, and a
  //    resumable hand-off on every non-terminal step.
  assert.ok(
    binSource.includes(`"${family.prepareVerb}"`),
    `${where} claims prepare verb ${family.prepareVerb}, which bin.ts does not accept`,
  );
  assert.equal(
    family.submitVerbs.length,
    family.stepCount,
    `${where} must expose exactly one submit verb per step`,
  );
  for (const verb of family.submitVerbs) {
    assert.ok(
      binSource.includes(`"${verb}"`),
      `${where} claims submit verb ${verb}, which bin.ts does not accept`,
    );
  }
  assert.equal(
    family.resumableStepModules.length,
    family.stepCount - 1,
    `${where} must declare a resumable hand-off for every non-terminal step`,
  );
  for (const modulePath of family.resumableStepModules) {
    const moduleSource = await readRepositoryFile(modulePath);
    assert.ok(
      moduleSource.includes("nextThreadOutRef"),
      `${modulePath} is claimed resumable but does not return nextThreadOutRef`,
    );
  }
  const terminalSource = await readRepositoryFile(family.terminalStepModule);
  assert.ok(
    !terminalSource.includes("nextThreadOutRef"),
    `${family.terminalStepModule} is the terminal step and must not return nextThreadOutRef`,
  );
  assert.ok(
    terminalSource.includes("fraudProofUnit"),
    `${family.terminalStepModule} is the terminal step and must return the permanent fraudProofUnit`,
  );

  // -- output 9: the measured lifecycle must exist by exact title
  const emulatorSource = await readRepositoryFile(family.emulator.file);
  assert.ok(
    family.emulator.titles.includes(family.emulator.measuredLifecycleTitle),
    `${where} measured lifecycle title is not among its declared titles`,
  );
  // The cited lifecycles are executed below; citing them here only schedules
  // the run. A title the runner never collects, or collects and does not pass,
  // fails this gate closed.
  declareVitest(family.emulator.file, family.emulator.titles);
  if (family.emulator.hasValidBlockNegative) {
    assert.ok(
      typeof family.emulator.validBlockNegativeTitle === "string" &&
        family.emulator.titles.includes(
          family.emulator.validBlockNegativeTitle,
        ),
      `${where} claims a valid-block negative but does not name a declared title for it`,
    );
    assert.ok(
      typeof family.emulator.validBlockNegativeScope === "string" &&
        family.emulator.validBlockNegativeScope.length > 0,
      `${where} must state the exact scope of its valid-block negative rather than implying full coverage`,
    );
  }
  // The measured lifecycle must actually record proof fit for this family.
  assert.ok(
    emulatorSource.includes(`stage: \`${family.proofFitLabel} \${stage}\``),
    `${family.emulator.file} does not apply the proof-fit helper under the ${family.proofFitLabel} label`,
  );

  // -- output 5: the recorded measurement must be self-consistent
  const fit = family.measuredProofFit;
  assert.equal(
    fit.fixture,
    "minimal",
    `${where} records measuredProofFit fixture "${String(fit.fixture)}"; that block is the minimal baseline and the adversarial one belongs in maximumProofFit`,
  );
  const stepStages = Array.from(
    { length: family.stepCount },
    (_unused, index) => `step-0${String(index + 1)}`,
  );
  const expectedStages = ["init", ...stepStages, "remove"];
  assert.deepEqual(
    fit.stages.map((stage) => stage.stage),
    expectedStages,
    `${where} must measure init, every step, and removal`,
  );
  for (const stage of fit.stages) {
    assert.equal(
      stage.l1ByteMargin,
      L1_MAX_TX_SIZE - stage.bytes,
      `${where} stage ${stage.stage} margin ${String(stage.l1ByteMargin)} does not equal 16384 - ${String(stage.bytes)}`,
    );
    assert.ok(
      stage.l1ByteMargin >= 0,
      `${where} stage ${stage.stage} exceeds the L1 envelope`,
    );
    measuredStages += 1;
  }
  const computedWorst = Math.min(
    ...fit.stages.map((stage) => stage.l1ByteMargin),
  );
  assert.equal(
    fit.worstByteMargin,
    computedWorst,
    `${where} worstByteMargin disagrees with its own stage list`,
  );
  assert.equal(
    fit.stages.find((stage) => stage.l1ByteMargin === computedWorst).stage,
    fit.worstByteMarginStage,
    `${where} worstByteMarginStage disagrees with its own stage list`,
  );
  for (const bytes of fit.removalTransactionBytes) {
    assert.ok(
      bytes <= L1_MAX_TX_SIZE,
      `${where} removal transaction of ${String(bytes)} bytes exceeds the L1 envelope`,
    );
  }
  worstByteMargin = Math.min(worstByteMargin, computedWorst);

  // -- output 5, adversarial half: the maximum fixture. Every number below is
  //    recomputed from the recorded stages and the pinned marginal cost, so the
  //    ceiling that keeps this cell OPEN cannot be asserted by hand.
  const maximum = family.maximumProofFit;
  assert.ok(
    maximum !== undefined,
    `${where} must record a maximumProofFit block; a minimal-only measurement is what left output 5 unowned`,
  );
  assert.equal(
    maximum.fixture,
    "maximum",
    `${where} maximumProofFit must declare fixture "maximum"`,
  );
  assert.equal(
    maximum.branchLevels,
    ADVERSARIAL_BRANCH_LEVELS,
    `${where} maximum fixture branch levels drifted from the constructed depth`,
  );
  // Removal carries no membership proof, so it is depth-invariant and is
  // measured but not byte-pinned; the proof-carrying path is what is pinned.
  assert.deepEqual(
    maximum.stages.map((stage) => stage.stage),
    ["init", ...stepStages],
    `${where} maximum fixture must measure init and every step`,
  );
  assert.equal(
    maximum.removalMeasured,
    true,
    `${where} maximum fixture must still measure the removal transactions`,
  );
  for (const stage of maximum.stages) {
    assert.equal(
      stage.l1ByteMargin,
      L1_MAX_TX_SIZE - stage.bytes,
      `${where} maximum stage ${stage.stage} margin ${String(stage.l1ByteMargin)} does not equal 16384 - ${String(stage.bytes)}`,
    );
    assert.ok(
      stage.l1ByteMargin >= 0,
      `${where} maximum stage ${stage.stage} exceeds the L1 envelope`,
    );
    maximumStages += 1;
  }
  const maximumWorst = Math.min(
    ...maximum.stages.map((stage) => stage.l1ByteMargin),
  );
  assert.equal(
    maximum.worstByteMargin,
    maximumWorst,
    `${where} maximum worstByteMargin disagrees with its own stage list`,
  );
  assert.equal(
    maximum.stages.find((stage) => stage.l1ByteMargin === maximumWorst).stage,
    maximum.worstByteMarginStage,
    `${where} maximum worstByteMarginStage disagrees with its own stage list`,
  );
  const largestStageBytes = Math.max(
    ...maximum.stages.map((stage) => stage.bytes),
  );
  assert.equal(
    maximum.largestStageBytes,
    largestStageBytes,
    `${where} maximum largestStageBytes disagrees with its own stage list`,
  );
  assert.ok(
    maximum.worstByteMargin < fit.worstByteMargin,
    `${where} the maximum fixture must be tighter than the minimal one; ${String(maximum.worstByteMargin)} is not below ${String(fit.worstByteMargin)}`,
  );
  const derivedCeiling =
    maximum.branchLevels +
    Math.floor(
      (L1_MAX_TX_SIZE - largestStageBytes) /
        bound.proofTransactionBranchLevelBytes,
    );
  assert.equal(
    maximum.envelopeExhaustionBranchLevel,
    derivedCeiling,
    `${where} envelopeExhaustionBranchLevel ${String(maximum.envelopeExhaustionBranchLevel)} is not the level derived from its own measured bytes and the pinned marginal cost (${String(derivedCeiling)})`,
  );
  assert.equal(
    maximum.log2WorkToExhaustEnvelope,
    4 * derivedCeiling,
    `${where} log2WorkToExhaustEnvelope must be 4 times the exhaustion level, because forcing level i is a fixed-target search over i nibbles`,
  );
  // The claim that keeps output 5 OPEN, restated per family so no family can
  // be quietly promoted while the exposure stands.
  assert.ok(
    maximum.envelopeExhaustionBranchLevel <
      bound.referenceAdversaryBranchLevelReach,
    `${where} the L1 envelope is no longer exhaustible by the reference adversary; finding Q1X-F5 must be re-stated rather than left stale`,
  );
  lowestExhaustionLevel = Math.min(
    lowestExhaustionLevel,
    maximum.envelopeExhaustionBranchLevel,
  );
  worstMaximumByteMargin = Math.min(worstMaximumByteMargin, maximumWorst);
  familyChecks += 1;
}

// The maximum-fixture suite is scheduled here; the runner below decides whether
// any of it passed.
declareVitest(maximumSuite.file, maximumSuite.titles);

// ## Shared DA-first evidence gate

const gate = evidence.sharedDaFirstEvidenceGate;
assert.ok(
  evidenceBuilderSource.includes(`export const ${gate.admissionGate} =`),
  `the shared admission gate ${gate.admissionGate} is no longer exported`,
);
for (const assertion of gate.assertions) {
  assert.ok(
    evidenceBuilderSource.includes(assertion) ||
      (
        await readRepositoryFile(
          "demo/midgard-fault-proofs/src/evidence/canonical-block-evidence-v1.ts",
        )
      ).includes(assertion),
    `the DA-first gate no longer applies ${assertion}`,
  );
}
const gateDeclaration = declareVitest(gate.suite, gate.familyGateTitles);

// ---------------------------------------------------------------------------
// Execute. Each cited suite is run once by its own package's Vitest CLI, and
// every published count below is read out of the resulting JSON report.
// ---------------------------------------------------------------------------

const vitestOutcomes = new Map();
for (const declaration of declaredVitestFiles.values()) {
  vitestOutcomes.set(
    declaration.file,
    deriveVitestOutcome({
      label: `Q1x proof family ${declaration.file}`,
      requiredTitles: declaration.titles,
      ...runVitest({
        packageRoot: resolve(repositoryRoot, declaration.packageDirectory),
        testFile: declaration.testFile,
      }),
    }),
  );
}
assert.deepEqual(
  evidence.runners,
  [...declaredVitestFiles.values()].map((declaration) =>
    vitestPublishedCommand({
      packageDirectory: declaration.packageDirectory,
      testFile: declaration.testFile,
    }),
  ),
  "published runner commands drifted from the commands this gate executes",
);

const gateOutcome = vitestOutcomes.get(gate.suite);
assert.equal(
  gate.command,
  vitestPublishedCommand({
    packageDirectory: gateDeclaration.packageDirectory,
    testFile: gateDeclaration.testFile,
  }),
  "the gate command recorded in the artifact is not the command this gate runs",
);
assert.equal(
  gate.measured.tests,
  gateOutcome.collected,
  "published canonical-evidence test count is not the number the runner collected",
);
assert.equal(
  gate.measured.passed,
  gateOutcome.passed,
  "published canonical-evidence pass count is not the number the runner passed",
);
assert.equal(
  gate.measured.failed,
  gateOutcome.collected - gateOutcome.passed,
  "published canonical-evidence failure count is not the number the runner did not pass",
);

const emulatorLifecyclesExecuted = evidence.families.reduce(
  (total, family) => total + family.emulator.titles.length,
  0,
);

// The maximum-fixture suite's own passage, read out of the runner report rather
// than from the artifact that cites it.
const maximumOutcome = vitestOutcomes.get(maximumSuite.file);
assert.equal(
  maximumOutcome.collected,
  maximumSuite.titles.length,
  "the maximum-fixture suite collected a different number of tests than the artifact cites",
);

// ## Output status matrix

assert.deepEqual(
  evidence.outputStatus.map((row) => row.output),
  BOUND_OUTPUTS,
  "outputs 5 to 9 must be listed exactly once, in order",
);
let localPassCells = 0;
let openCells = 0;
for (const row of evidence.outputStatus) {
  for (const goalId of GOAL_IDS) {
    const status = row[goalId];
    assert.ok(
      ["LOCAL_PASS", "OPEN"].includes(status),
      `output ${String(row.output)} / ${goalId} has an unknown status ${String(status)}`,
    );
    if (status === "LOCAL_PASS") {
      localPassCells += 1;
    } else {
      openCells += 1;
    }
  }
  assert.ok(
    typeof row.evidence === "string" && row.evidence.length > 0,
    `output ${String(row.output)} must carry evidence text`,
  );
  assert.ok(
    typeof row.owner === "string" && row.owner.length > 0,
    `output ${String(row.output)} must name an owner`,
  );
  if (GOAL_IDS.some((goalId) => row[goalId] === "OPEN")) {
    assert.ok(
      typeof row.whyOpen === "string" && row.whyOpen.length > 0,
      `output ${String(row.output)} has an OPEN cell and must say why it is open`,
    );
  }
}
// Output 5 must remain OPEN for every family while the maximum fixture's own
// measurement says the envelope is exhaustible, or while an adversarial axis is
// still unexercised. Publishing LOCAL_PASS on the strength of a fixture that
// merely fits at the depth someone chose to build would be the exact defect
// this program has been bitten by: a passage claim an adversary can falsify.
const output5 = evidence.outputStatus.find((row) => row.output === 5);
const unexercisedAxes = evidence.adversarialAxes.filter((axis) =>
  axis.measuredToday.includes("unexercised"),
);
const output5MayClose =
  !evidence.adversarialDepthBound.envelopeExhaustibleByReferenceAdversary &&
  unexercisedAxes.length === 0;
for (const goalId of GOAL_IDS) {
  assert.equal(
    output5[goalId],
    "OPEN",
    `output 5 / ${goalId} may not be LOCAL_PASS while the L1 envelope is exhaustible at branch level ${String(
      evidence.adversarialDepthBound.byteCeiling,
    )} and ${String(unexercisedAxes.length)} adversarial axis/axes remain unexercised`,
  );
}
assert.equal(
  output5MayClose,
  false,
  "the conditions that keep output 5 OPEN have been cleared; the cells must be re-decided deliberately rather than left OPEN by inertia",
);

// Adversarial axes must be enumerated, not hand-waved.
assert.ok(
  Array.isArray(evidence.adversarialAxes) &&
    evidence.adversarialAxes.length > 0,
  "the adversarial axes that output 5 still owes must be enumerated explicitly",
);
for (const axis of evidence.adversarialAxes) {
  for (const key of ["axis", "growsWith", "affects", "measuredToday"]) {
    assert.ok(
      typeof axis[key] === "string" && axis[key].length > 0,
      `adversarial axis is missing ${key}`,
    );
  }
}

// No LIVE_PASS may ever be claimed from a family-local artifact.
const statusValues = [];
const collectStatuses = (node) => {
  if (Array.isArray(node)) {
    for (const item of node) {
      collectStatuses(item);
    }
    return;
  }
  if (node !== null && typeof node === "object") {
    for (const [key, value] of Object.entries(node)) {
      if (key === "status" || GOAL_IDS.includes(key)) {
        statusValues.push(value);
      }
      collectStatuses(value);
    }
  }
};
collectStatuses(evidence);
assert.ok(
  !statusValues.includes("LIVE_PASS"),
  "a family-local artifact must never record a LIVE_PASS status; that belongs to Q57/QG3",
);

// ## Residual findings must be owned, never silenced

assert.ok(
  Array.isArray(evidence.residualFindings),
  "residualFindings must be an explicit list, even when empty",
);
for (const finding of evidence.residualFindings) {
  for (const key of ["id", "severity", "finding", "owner"]) {
    assert.ok(
      typeof finding[key] === "string" && finding[key].length > 0,
      `residual finding is missing ${key}`,
    );
  }
}
// A family whose emulator journey has no valid-block negative must be named in
// the residual findings; a silent omission is not acceptable.
const residualText = evidence.residualFindings
  .map((finding) => finding.finding)
  .join(" ");
for (const family of evidence.families) {
  if (!family.emulator.hasValidBlockNegative) {
    assert.ok(
      residualText.includes(family.goalId),
      `${family.goalId} has no emulator valid-block negative and must be named in residualFindings`,
    );
  }
}

// ## The summary is recomputed, so it cannot lie

const recomputed = {
  families: evidence.families.length,
  outputsBound: evidence.outputStatus.length,
  localPassCells,
  openCells,
  measuredProofFitStages: measuredStages,
  measuredMaximumProofFitStages: maximumStages,
  worstByteMarginAcrossFamilies: worstByteMargin,
  worstMaximumByteMarginAcrossFamilies: worstMaximumByteMargin,
  lowestEnvelopeExhaustionBranchLevel: lowestExhaustionLevel,
  canonicalEvidenceSuiteTests: gateOutcome.passed,
  emulatorLifecyclesExecuted,
  maximumFixtureLifecyclesExecuted: maximumOutcome.passed,
  residualFindings: evidence.residualFindings.length,
};
assert.deepEqual(
  evidence.summary,
  recomputed,
  "the recorded summary disagrees with the rows it summarizes",
);

// ## Parent integration is declared, never performed here

assert.equal(
  evidence.parentIntegration.owner,
  "parent",
  "matrix, manifest and ledger integration is parent-owned",
);
assert.ok(
  evidence.parentIntegration.pendingEdits.length > 0,
  "the parent-owned edits this artifact supports must be listed explicitly",
);

// ---------------------------------------------------------------------------
// Negative self-tests: spawn this gate against seeded fixtures and require a
// non-zero exit carrying the specific diagnostic. Without these, a future edit
// could reintroduce existence-as-passage and nothing would notice.
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
];
for (const [flag, expectedDiagnostic] of selfTests) {
  const selfTest = runSelfTest(flag);
  assert.notEqual(
    selfTest.status,
    0,
    `Q1x proof-family gate accepted the seeded defect ${flag}`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `Q1x proof-family gate rejected ${flag} without its specific diagnostic`,
  );
}
// Positive control: the same harness must still accept a real passing run, so
// the rejections above cannot be a gate that rejects everything.
const control = runSelfTest("--vitest-fixture=passing");
assert.equal(
  control.status,
  0,
  `Q1x proof-family gate rejected a passing fixture: ${control.stderr}`,
);
assert.match(control.stdout, /vitest fixture passing: 1\/1 passed/u);

const report = {
  status: "PASS",
  families: evidence.summary.families,
  outputsBound: evidence.summary.outputsBound,
  localPassCells: evidence.summary.localPassCells,
  openCells: evidence.summary.openCells,
  measuredProofFitStages: evidence.summary.measuredProofFitStages,
  measuredMaximumProofFitStages: evidence.summary.measuredMaximumProofFitStages,
  worstByteMarginAcrossFamilies: evidence.summary.worstByteMarginAcrossFamilies,
  worstMaximumByteMarginAcrossFamilies:
    evidence.summary.worstMaximumByteMarginAcrossFamilies,
  lowestEnvelopeExhaustionBranchLevel:
    evidence.summary.lowestEnvelopeExhaustionBranchLevel,
  canonicalEvidenceSuiteTests: evidence.summary.canonicalEvidenceSuiteTests,
  emulatorLifecyclesExecuted: evidence.summary.emulatorLifecyclesExecuted,
  maximumFixtureLifecyclesExecuted:
    evidence.summary.maximumFixtureLifecyclesExecuted,
  residualFindings: evidence.summary.residualFindings,
  familyChecks,
  thresholdEnforcementChecks,
  vitestSuitesExecuted: vitestOutcomes.size,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q10/Q11/Q12/Q14 outputs 5-9: PASS (${String(evidence.summary.localPassCells)} LOCAL_PASS cells, ${String(
      evidence.summary.openCells,
    )} OPEN cells, ${String(evidence.summary.measuredProofFitStages)} minimal + ${String(
      evidence.summary.measuredMaximumProofFitStages,
    )} maximum proof-fit stages, worst L1 byte margin ${String(
      evidence.summary.worstByteMarginAcrossFamilies,
    )} minimal / ${String(
      evidence.summary.worstMaximumByteMarginAcrossFamilies,
    )} maximum, envelope exhausted at branch level ${String(
      evidence.summary.lowestEnvelopeExhaustionBranchLevel,
    )})`,
  );
}
