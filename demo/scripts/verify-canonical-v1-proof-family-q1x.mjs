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

// ## Published-chunk proof carriage (issue #545): the remediation of Q1X-F5
//
// Q1X-F5 is a measured fact about the redeemer-carried route and stays asserted
// above, unchanged. What is checked here is the remediation that lets a family's
// output-5 cell close in spite of it: a carriage whose step transaction does not
// grow with proof depth, measured end to end for the families that claim it.
// Every number is recomputed from the recorded stages, and the depth-invariance
// claim is recomputed from the two depths it was measured at.
const carriage = evidence.chunkedProofCarriage;
assert.ok(
  carriage !== undefined && typeof carriage === "object",
  "the published-chunk carriage that remediates Q1X-F5 must be recorded, not asserted in prose",
);
assert.equal(
  carriage.remediates,
  "Q1X-F5",
  "the carriage block must name the finding it remediates",
);
assert.ok(
  carriage.onchainSeam.directRouteRetained === true,
  "the redeemer-carried route must be retained; removing it would strand every prover whose proof already fits",
);
assert.ok(
  carriage.branchLevels > bound.byteCeiling,
  `the carriage must be measured PAST the direct route's envelope ceiling; ${String(carriage.branchLevels)} is not above ${String(bound.byteCeiling)}`,
);
let chunkedStages = 0;
for (const goalId of carriage.measuredFamilies) {
  assert.ok(
    GOAL_IDS.includes(goalId),
    `carriage names unknown goal id ${goalId}`,
  );
  const stages = carriage.stages[goalId];
  assert.ok(
    Array.isArray(stages) && stages.length > 0,
    `${goalId} claims a measured carriage journey but records no stages`,
  );
  assert.ok(
    stages.some((stage) => stage.stage === "publish-chunks"),
    `${goalId} carriage journey must measure the publication transaction, not only the step that references it`,
  );
  for (const stage of stages) {
    assert.equal(
      stage.l1ByteMargin,
      L1_MAX_TX_SIZE - stage.bytes,
      `${goalId} carriage stage ${stage.stage} margin ${String(stage.l1ByteMargin)} does not equal 16384 - ${String(stage.bytes)}`,
    );
    assert.ok(
      stage.l1ByteMargin >= 0,
      `${goalId} carriage stage ${stage.stage} exceeds the L1 envelope`,
    );
    chunkedStages += 1;
  }
  assert.ok(
    carriage.titles.includes(carriage.familyTitles[goalId]),
    `the carriage suite does not declare a lifecycle title for ${goalId}`,
  );
}
assert.deepEqual(
  [...carriage.measuredFamilies, ...carriage.unmeasuredFamilies].sort(),
  [...GOAL_IDS].sort(),
  "every family must be recorded as either carriage-measured or explicitly not",
);
// The claim that makes the remediation a remediation: depth costs the step
// transaction nothing. Recomputed, not asserted.
const invariance = carriage.depthInvariance;
assert.ok(
  invariance.deepBranchLevels > invariance.shallowBranchLevels,
  "depth invariance must be measured at two distinct depths",
);
assert.equal(
  invariance.stepTransactionBytesPerBranchLevel,
  (invariance.deepStep01Bytes - invariance.shallowStep01Bytes) /
    (invariance.deepBranchLevels - invariance.shallowBranchLevels),
  "the published per-level cost of the chunked step transaction is not the difference between its own two measured depths",
);
assert.equal(
  invariance.stepTransactionBytesPerBranchLevel,
  0,
  "the chunked step transaction still grows with proof depth, so it does not remediate Q1X-F5",
);
assert.equal(
  invariance.directRouteBytesForTheSameLevels,
  (invariance.deepBranchLevels - invariance.shallowBranchLevels) *
    bound.proofTransactionBranchLevelBytes,
  "the direct-route comparison is not the pinned marginal cost times the level difference",
);
assert.ok(
  invariance.deepProofCborBytes > invariance.shallowProofCborBytes,
  "the deeper proof must actually be larger; if it were not, the invariance would be measuring nothing",
);
assert.ok(
  invariance.deepBranchLevels >= bound.referenceAdversaryBranchLevelReach,
  "depth invariance must be measured out to the depth the reference adversary reaches",
);
declareVitest(carriage.suite, carriage.titles);

// ## Spend-input preimage cardinality (issue #549): the SECOND adversarial axis
//
// The depth axis was exercised, bounded, and then remediated. This one is
// exercised and bounded and NOT remediated, and the block below is what makes
// that a derived conclusion rather than a claim: the admissible cardinality is
// recomputed from the consensus profile's own numbers, the reserve ceiling from
// the ledger's own cap, and the verdict from the two measured cardinalities per
// family. A future edit that quietly raised the measured ceiling, lowered the
// admissible one, or flipped the verdict without the numbers moving fails here.
const cardinality = evidence.spendInputCardinalityBound;
assert.ok(
  cardinality !== undefined && typeof cardinality === "object",
  "the spend-input cardinality bound must be recorded, not asserted in prose",
);
for (const goalId of cardinality.affectsGoalIds) {
  assert.ok(
    GOAL_IDS.includes(goalId),
    `the spend-input cardinality bound names unknown goal id ${goalId}`,
  );
}
assert.equal(
  cardinality.admissiblePreimageBytes,
  2 * L1_MAX_TX_SIZE,
  "the spend-inputs preimage field bound is twice the preserved L1 envelope",
);
assert.equal(
  cardinality.admissibleByPreimageBytes,
  Math.floor(
    (cardinality.admissiblePreimageBytes -
      cardinality.preimageArrayHeaderBytes) /
      cardinality.preimageBytesPerInput,
  ),
  "the cardinality the field-bytes bound admits is not the one its own bytes imply",
);
assert.equal(
  cardinality.admissibleCardinality,
  Math.min(
    cardinality.admissibleItemCountGuardrail,
    cardinality.admissibleByPreimageBytes,
    cardinality.cardanoScriptSpendShapeCardinality,
  ),
  "the admissible cardinality must be the SMALLEST of the constraints it lists; taking any other would understate what a proof has to survive",
);
assert.equal(
  cardinality.executionMemoryReserveCeiling,
  (cardinality.executionMemoryCap *
    (100 - evidence.proofFitThresholds.executionFit.reservePercent)) /
    100,
  "the reserve ceiling is not the 20% reserve applied to the ledger's own memory cap",
);
assert.ok(
  cardinality.executionMemoryCap < cardinality.consensusProfileMemoryFloor,
  "the ceiling the boundary is measured against must be the conservative one; measuring against a cap above the consensus profile's capability floor would overstate what fits",
);
assert.equal(
  cardinality.bindingEnvelope,
  "executionMemory",
  "the binding envelope of this axis must be named explicitly",
);
let cardinalityFamilyChecks = 0;
let largestFittingCardinality = 0;
for (const goalId of cardinality.affectsGoalIds) {
  const measured = cardinality.measured[goalId];
  assert.ok(
    measured !== undefined,
    `${goalId} is named as affected by the cardinality axis but records no measurement`,
  );
  assert.equal(
    measured.firstOverReserveCardinality,
    measured.largestFittingCardinality + 1,
    `${goalId} must measure the boundary as an adjacent PAIR; ${String(measured.firstOverReserveCardinality)} does not follow ${String(measured.largestFittingCardinality)}`,
  );
  assert.ok(
    measured.largestFittingExecutionMemory <=
      cardinality.executionMemoryReserveCeiling,
    `${goalId} claims ${String(measured.largestFittingCardinality)} inputs fit, but its measured memory exceeds the reserve ceiling`,
  );
  assert.ok(
    measured.firstOverReserveExecutionMemory >
      cardinality.executionMemoryReserveCeiling,
    `${goalId} claims ${String(measured.firstOverReserveCardinality)} inputs exceed the reserve, but its measured memory does not`,
  );
  // The adjacent case must be a real evaluation that fails the RELEASE policy,
  // not one the ledger rejected outright: an over-cap number here would prove
  // nothing about the 20% reserve.
  assert.ok(
    measured.firstOverReserveExecutionMemory < cardinality.executionMemoryCap,
    `${goalId}'s first over-reserve measurement is above the ledger's own cap, so it does not isolate the 20% reserve`,
  );
  if (measured.witnessPublicationBytes !== undefined) {
    assert.equal(
      measured.witnessPublicationL1ByteMargin,
      L1_MAX_TX_SIZE - measured.witnessPublicationBytes,
      `${goalId} witness publication margin does not equal 16384 - ${String(measured.witnessPublicationBytes)}`,
    );
    // The correction issue #482's expectation needed: publication is not the
    // constraint, so it must be recorded as comfortably inside the envelope.
    assert.ok(
      measured.witnessPublicationL1ByteMargin > 0,
      `${goalId} publishes its spend-inputs witness outside the L1 envelope, which would make the publication the binding constraint after all`,
    );
  }
  largestFittingCardinality = Math.max(
    largestFittingCardinality,
    measured.largestFittingCardinality,
  );
  cardinalityFamilyChecks += 1;
}
assert.ok(
  cardinalityFamilyChecks > 0,
  "the cardinality axis must be measured for at least one family it affects",
);
assert.equal(
  cardinality.admissibleCardinalityExceedsMeasuredCeiling,
  cardinality.admissibleCardinality > largestFittingCardinality,
  "the cardinality verdict disagrees with the numbers it is drawn from",
);
// A fit claim would have to be a fit MEASUREMENT: if the admissible shape ever
// becomes buildable, this artifact must say so with a measurement rather than
// by deleting the finding.
assert.equal(
  cardinality.buildsAtAdmissibleCardinality,
  !cardinality.admissibleCardinalityExceedsMeasuredCeiling,
  "buildsAtAdmissibleCardinality disagrees with the measured ceiling",
);
assert.equal(
  cardinality.remediableByCarriage,
  false,
  "a carriage remediation of this axis would have to be recorded as a remediation block, exactly as chunkedProofCarriage is for Q1X-F5",
);
assert.ok(
  cardinality.titles.includes(cardinality.derivationTitle) &&
    cardinality.titles.includes(cardinality.buildsAtAdmissibleCardinalityTitle),
  "the cardinality suite must declare both the derivation and the admissible-shape lifecycle it is cited for",
);
declareVitest(cardinality.suite, cardinality.titles);

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

// The carriage suite's own passage, likewise read out of the runner report.
const carriageOutcome = vitestOutcomes.get(carriage.suite);
assert.equal(
  carriageOutcome.collected,
  carriage.titles.length,
  "the carriage suite collected a different number of tests than the artifact cites",
);
assert.equal(
  carriageOutcome.passed,
  carriage.titles.length,
  "the carriage suite did not pass every lifecycle the artifact cites, so no output-5 cell may close on it",
);

// The cardinality suite's own passage. It decides no cell — the axis it
// measures is unsettled — but a finding that says "measured and does not fit"
// is only worth the measurement, so the suite must have run and passed.
const cardinalityOutcome = vitestOutcomes.get(cardinality.suite);
assert.equal(
  cardinalityOutcome.collected,
  cardinality.titles.length,
  "the cardinality suite collected a different number of tests than the artifact cites",
);
assert.equal(
  cardinalityOutcome.passed,
  cardinality.titles.length,
  "the cardinality suite did not pass every lifecycle the artifact cites, so finding Q1X-F6's numbers are unowned",
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
// Output 5 is decided PER FAMILY, and the decision is derived here rather than
// read from the artifact. A family may be LOCAL_PASS only when BOTH hold:
//
//   1. every adversarial axis that affects it is either exercised or remediated
//      by a mechanism this gate has just re-derived, and
//   2. that remediation is measured end to end for THAT family.
//
// Publishing LOCAL_PASS on the strength of a fixture that merely fits at the
// depth someone chose to build, or on a remediation measured for a sibling
// family, would be the exact defect this program has been bitten by: a passage
// claim an adversary can falsify.
const output5 = evidence.outputStatus.find((row) => row.output === 5);
for (const axis of evidence.adversarialAxes) {
  assert.ok(
    Array.isArray(axis.affectsGoalIds) && axis.affectsGoalIds.length > 0,
    `adversarial axis "${axis.axis}" must name the families it affects, so output 5 can be decided per family`,
  );
  for (const goalId of axis.affectsGoalIds) {
    assert.ok(
      GOAL_IDS.includes(goalId),
      `adversarial axis "${axis.axis}" names unknown goal id ${goalId}`,
    );
  }
}
// Every axis names the bound block that decides whether being exercised is
// enough. An axis whose own bound records that the worst ADMISSIBLE instance
// does not fit is not settled by having been measured — the measurement is the
// exposure. Reading the wrong bound is how an unsettled axis would slip through,
// so the mapping is explicit and an unknown bound is a hard failure.
const axisBoundClearsTheAxis = (axis) => {
  switch (axis.boundedBy) {
    case "adversarialDepthBound":
      return !evidence.adversarialDepthBound
        .envelopeExhaustibleByReferenceAdversary;
    case "spendInputCardinalityBound":
      return !cardinality.admissibleCardinalityExceedsMeasuredCeiling;
    default:
      throw new Error(
        `adversarial axis "${axis.axis}" names bound ${String(axis.boundedBy)}, which this gate does not know how to re-derive`,
      );
  }
};
// The depth axis is the one Q1X-F5 records. It counts as settled for a family
// only through a remediation block this gate validated above, and only where
// that block records a measurement for the family itself.
const axisIsSettledFor = (axis, goalId) => {
  if (!axis.affectsGoalIds.includes(goalId)) {
    return true;
  }
  if (!axis.measuredToday.includes("unexercised")) {
    if (axis.remediatedBy === undefined || axis.remediatedBy === null) {
      // Exercised and bounded. Whether that settles the axis is decided by the
      // axis's own bound, not by an unrelated one.
      return axisBoundClearsTheAxis(axis);
    }
    assert.equal(
      axis.remediatedBy,
      "chunkedProofCarriage",
      `adversarial axis "${axis.axis}" claims a remediation this gate does not know how to verify`,
    );
    return carriage.measuredFamilies.includes(goalId);
  }
  return false;
};
for (const goalId of GOAL_IDS) {
  const unsettled = evidence.adversarialAxes.filter(
    (axis) => !axisIsSettledFor(axis, goalId),
  );
  const mayClose = unsettled.length === 0;
  assert.equal(
    output5[goalId],
    mayClose ? "LOCAL_PASS" : "OPEN",
    mayClose
      ? `output 5 / ${goalId} has no unsettled adversarial axis left, so the cell must be re-decided deliberately rather than left OPEN by inertia`
      : `output 5 / ${goalId} may not be LOCAL_PASS while ${String(unsettled.length)} adversarial axis/axes (${unsettled
          .map((axis) => axis.axis)
          .join(", ")}) remain unsettled for it`,
  );
}
// At least one cell must still be OPEN or still be LOCAL_PASS by measurement,
// never by an empty axis list.
assert.ok(
  evidence.adversarialAxes.length > 1,
  "collapsing the axis list to one entry would make the per-family rule vacuous",
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
  assert.ok(
    typeof axis.boundedBy === "string" && axis.boundedBy.length > 0,
    `adversarial axis "${axis.axis}" must name the bound block that decides it`,
  );
}

// A finding that claims remediation must name the block that carries it, and
// that block must have been validated above. Q1X-F5's measurement stays
// recorded either way: a remediated finding is restated, never deleted.
for (const finding of evidence.residualFindings) {
  if (finding.status === undefined) {
    continue;
  }
  assert.equal(
    finding.status,
    "remediated-by-carriage",
    `residual finding ${finding.id} carries an unknown status ${String(finding.status)}`,
  );
  assert.equal(
    finding.id,
    carriage.remediates,
    `residual finding ${finding.id} claims a remediation the carriage block does not name`,
  );
  assert.ok(
    finding.finding.includes(
      String(evidence.adversarialDepthBound.proofTransactionBranchLevelBytes),
    ),
    `residual finding ${finding.id} was remediated but dropped the measurement it was built on`,
  );
}

// The cardinality axis's finding must carry the numbers, not a verdict. A
// finding that said only "does not fit" would be unfalsifiable prose, which is
// exactly what this axis had before it was measured.
const cardinalityFinding = evidence.residualFindings.find((entry) =>
  entry.finding.includes("spend-input preimage cardinality"),
);
assert.ok(
  cardinalityFinding !== undefined,
  "the spend-input cardinality axis must be owned by a residual finding",
);
assert.equal(
  cardinalityFinding.severity,
  cardinality.admissibleCardinalityExceedsMeasuredCeiling
    ? "defect"
    : "observation",
  `residual finding ${cardinalityFinding.id} does not carry the severity its own measurement implies`,
);
for (const number of [
  cardinality.admissibleCardinality,
  ...cardinality.affectsGoalIds.map(
    (goalId) => cardinality.measured[goalId].largestFittingCardinality,
  ),
  ...cardinality.affectsGoalIds.map(
    (goalId) => cardinality.measured[goalId].firstOverReserveCardinality,
  ),
]) {
  assert.ok(
    cardinalityFinding.finding.includes(String(number)),
    `residual finding ${cardinalityFinding.id} dropped the measured cardinality ${String(number)} it was built on`,
  );
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
  chunkedCarriageStages: chunkedStages,
  chunkedCarriageLifecyclesExecuted: carriageOutcome.passed,
  spendInputCardinalityLifecyclesExecuted: cardinalityOutcome.passed,
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
  chunkedCarriageStages: evidence.summary.chunkedCarriageStages,
  chunkedCarriageLifecyclesExecuted:
    evidence.summary.chunkedCarriageLifecyclesExecuted,
  spendInputCardinalityLifecyclesExecuted:
    evidence.summary.spendInputCardinalityLifecyclesExecuted,
  admissibleSpendInputCardinality: cardinality.admissibleCardinality,
  largestFittingSpendInputCardinality: largestFittingCardinality,
  residualFindings: evidence.summary.residualFindings,
  familyChecks,
  cardinalityFamilyChecks,
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
    )}, spend-input cardinality admissible ${String(
      cardinality.admissibleCardinality,
    )} against a measured ceiling of ${String(largestFittingCardinality)})`,
  );
}
