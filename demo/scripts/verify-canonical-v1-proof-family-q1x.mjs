#!/usr/bin/env node
// Verifies the Q10/Q11/Q12/Q14 binding of GOAL_SPEC.md 9.1 outputs 5 to 9.
//
// The rule enforced here: a per-family output may be published LOCAL_PASS only
// when the artifact's claim is re-derivable from the tree, and an OPEN cell
// must say why it is open and who owns it. Every catalogue index, category id,
// blueprint title, builder name, CLI verb, module resumability shape, vitest
// title and proof-fit threshold this artifact names is re-read out of its
// source file, so the artifact cannot drift away from the code. Existence is
// never accepted in place of passage: the byte and execution thresholds are
// re-checked against the helper the tests actually apply.
//
// usage: node demo/scripts/verify-canonical-v1-proof-family-q1x.mjs [--json]

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const emitJson = process.argv.slice(2).includes("--json");
const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repositoryRoot = resolve(demoRoot, "..");

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
let worstByteMargin = Number.POSITIVE_INFINITY;

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
  for (const title of family.emulator.titles) {
    assert.ok(
      emulatorSource.includes(`it("${title}"`),
      `${where} claims emulator test "${title}", which ${family.emulator.file} does not declare`,
    );
  }
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
    `${where} records fixture "${String(fit.fixture)}"; only a "minimal" fixture may be published while output 5 is OPEN`,
  );
  const expectedStages = [
    "init",
    ...Array.from(
      { length: family.stepCount },
      (_unused, index) => `step-0${String(index + 1)}`,
    ),
    "remove",
  ];
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
  familyChecks += 1;
}

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
const gateSuiteSource = await readRepositoryFile(gate.suite);
assert.equal(
  gate.measured.passed,
  gate.measured.tests,
  "the canonical-evidence suite must be measured fully passing",
);
assert.equal(gate.measured.failed, 0, "canonical-evidence failures must be 0");
const gateSuiteTitleCount = [...gateSuiteSource.matchAll(/\n\s+it\("/gu)]
  .length;
assert.equal(
  gateSuiteTitleCount,
  gate.measured.tests,
  `${gate.suite} declares ${String(gateSuiteTitleCount)} tests but the artifact records ${String(gate.measured.tests)}`,
);
for (const title of gate.familyGateTitles) {
  assert.ok(
    gateSuiteSource.includes(`it("${title}"`),
    `${gate.suite} no longer declares the family gate test "${title}"`,
  );
}

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
// Output 5 must remain OPEN for every family while only a minimal fixture is
// measured. Publishing LOCAL_PASS there would be the exact defect this
// program has been bitten by: a passage claim on an existence-grade fixture.
const output5 = evidence.outputStatus.find((row) => row.output === 5);
for (const goalId of GOAL_IDS) {
  assert.equal(
    output5[goalId],
    "OPEN",
    `output 5 / ${goalId} may not be LOCAL_PASS while every measured fixture is "minimal"`,
  );
}

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
  worstByteMarginAcrossFamilies: worstByteMargin,
  canonicalEvidenceSuiteTests: gate.measured.tests,
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

const report = {
  status: "PASS",
  families: evidence.summary.families,
  outputsBound: evidence.summary.outputsBound,
  localPassCells: evidence.summary.localPassCells,
  openCells: evidence.summary.openCells,
  measuredProofFitStages: evidence.summary.measuredProofFitStages,
  worstByteMarginAcrossFamilies: evidence.summary.worstByteMarginAcrossFamilies,
  canonicalEvidenceSuiteTests: evidence.summary.canonicalEvidenceSuiteTests,
  residualFindings: evidence.summary.residualFindings,
  familyChecks,
  thresholdEnforcementChecks,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q10/Q11/Q12/Q14 outputs 5-9: PASS (${String(evidence.summary.localPassCells)} LOCAL_PASS cells, ${String(
      evidence.summary.openCells,
    )} OPEN cells, ${String(evidence.summary.measuredProofFitStages)} measured proof-fit stages, worst L1 byte margin ${String(
      evidence.summary.worstByteMarginAcrossFamilies,
    )})`,
  );
}
