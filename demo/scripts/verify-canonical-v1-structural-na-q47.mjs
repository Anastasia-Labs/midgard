#!/usr/bin/env node
// Verifies the Q47 structural-N/A disposition (GOAL_SPEC.md 9.1).
//
// The rule enforced here: a structural N/A may be recorded only when (a) every
// violation variant it disclaims resolves to at least one EXECUTABLE selector
// that really passes, (b) the standalone family surface it claims not to need
// genuinely does not exist, and (c) no closure output is marked N/A without a
// named owner and a justification. Prose is never sufficient.
//
// Issue #533 (finding V-2 of #519). This gate used to publish its passage
// claims by reading the artifact back to itself — `assert.equal(measured.passed,
// measured.selectors)` compares two fields of the same JSON object and holds
// for `{selectors: 8, passed: 8}` whether or not aiken ever ran — and backed
// "N executable checks" with `^test <name>(` / `it("<title>"` declaration
// regexes over test source. A throwing test body, an `it.skip`, or a selector
// that collects nothing left every one of those numbers untouched.
//
// Every published count now comes from a runner report instead. All thirteen
// cited on-chain selectors are executed in one `aiken check -e` invocation
// (compilation dominates its cost, so batching is both faster and exactly as
// strict) and the TypeScript twins by spawning the owning package's own Vitest
// CLI.
//
// Issue #538. That invocation used to be a single compiler while the artifact
// claimed passage "under both stock v1.1.22 and fork v1.1.23", so the gate was
// made to take the second measurement rather than assert it.
//
// Issue #579 (owner ruling A, 2026-08-13). The dual-compiler leg is RETIRED
// with the stock compiler itself: stock v1.1.22 no longer compiles, formats,
// builds or executes anything in this repository, and the one measured
// divergence between the two showed the stock artifact was the WRONG one (see
// the retirement record in `.github/workflows/aiken-ci.yml`). Asserting that
// the cited selectors also pass under a compiler the owner ruled unsound is not
// a property worth gating on. What #538 actually bought is untouched and is the
// point of the leg that remains: this gate still EXECUTES every selector it
// cites, under a compiler whose identity it reads from the binary that produced
// the result, so it cannot publish a passage claim it never ran.
//
// The artifact's `measured` blocks are pins asserted against those runner
// results, never operands of each other. Source scanning survives only where it
// backs no count at all: the constructor/entry-point structural claims, and the
// exhaustiveness check that the Q47 module declares no selector the artifact
// omits.
//
// usage: node demo/scripts/verify-canonical-v1-structural-na-q47.mjs [--json]

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readdir, readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { runFixtureMode } from "./lib/runner-fixtures.mjs";
import {
  aikenCompilerVersion,
  aikenModuleName,
  aikenPublishedCommand,
  deriveAikenOutcome,
  deriveVitestOutcome,
  forkAikenBinary,
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
    "docs/exec-plans/evidence/canonical-v1-structural-na-q47-v1.json",
  ),
);

// ## Identity

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-structural-na-q47.v1",
  "evidence schema identity drifted",
);
assert.equal(evidence.version, 1, "evidence version drifted");
assert.deepEqual(evidence.goalIds, ["Q47"], "Q47 is the only goal id bound");
assert.equal(evidence.issue, 482, "issue binding drifted");

// ## The shared-family claim must match the tree

const proofSource = await readRepositoryFile(
  evidence.structuralClaim.constructorHome,
);
assert.equal(
  evidence.structuralClaim.sharedFamily,
  "transitionTrace",
  "Q47 must disclaim in favour of the transitionTrace family",
);
for (const constructor of evidence.structuralClaim.faultConstructors) {
  assert.ok(
    proofSource.includes(`  ${constructor} {`),
    `fault constructor ${constructor} is no longer declared in ${evidence.structuralClaim.constructorHome}`,
  );
}
for (const witnessEnum of evidence.structuralClaim.witnessEnums) {
  assert.ok(
    proofSource.includes(`pub type ${witnessEnum} {`),
    `witness enum ${witnessEnum} is no longer declared in ${evidence.structuralClaim.constructorHome}`,
  );
}
for (const entryPoint of evidence.structuralClaim.publicEntryPoints) {
  assert.ok(
    proofSource.includes(`pub fn ${entryPoint}(`),
    `public entry point ${entryPoint} is no longer exported`,
  );
}
// The window semantics the selectors depend on must still be the exclusive
// start / inclusive end pair the artifact records.
assert.ok(
  proofSource.includes("header.start_time < inclusion_time") &&
    proofSource.includes("inclusion_time <= header.end_time"),
  "the (start_time, end_time] publication window changed; every Q47 boundary vector is invalidated",
);
// The category id the artifact pins must still be the transition-trace one.
assert.equal(
  evidence.structuralClaim.catalogueCategoryId,
  "00000004",
  "transitionTrace category id drifted",
);
assert.ok(
  proofSource.includes(
    `const transition_trace_fraud_category_id = #"${evidence.structuralClaim.catalogueCategoryId}"`,
  ),
  "the on-chain transition-trace category id no longer matches the pinned id",
);
const catalogueOrder = await readRepositoryFile(
  "demo/midgard-sdk/src/fraud-proof/catalogue.ts",
);
const orderBlock = catalogueOrder.slice(
  catalogueOrder.indexOf("FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = ["),
);
const orderedCategories = [
  ...orderBlock.slice(0, orderBlock.indexOf("]")).matchAll(/"([a-zA-Z]+)"/gu),
].map((match) => match[1]);
assert.equal(
  orderedCategories.indexOf("transitionTrace"),
  evidence.structuralClaim.catalogueCategoryIndex,
  "transitionTrace catalogue index drifted from the pinned append-only index",
);

// ---------------------------------------------------------------------------
// Phase 1 — collect what the artifact declares. Nothing here decides whether a
// check passed; the declarations are only the plan the runners execute.
// ---------------------------------------------------------------------------

const selectorSafe = (name) =>
  assert.ok(
    /^[a-z0-9_]+$/u.test(name),
    `selector ${String(name)} is not a focused-check-safe name`,
  );

// The artifact names its modules in compiler form minus the `.test` suffix the
// compiler appends for a `*.test.ak` file; pinning the derived name here keeps
// the declared module honest and gives `deriveAikenOutcome` the exact module a
// selector must have been collected from.
const declaredModuleOf = ({ module, file }) => {
  const derived = aikenModuleName(file);
  assert.equal(
    derived,
    `${module}.test`,
    `${file} compiles to module ${derived}, which is not the ${module} the artifact declares`,
  );
  return derived;
};

const REQUIRED_VARIANTS = [
  "omittedDueDeposit",
  "omittedDueWithdrawal",
  "omittedDueForcedTransaction",
  "outOfWindowDeposit",
  "outOfWindowWithdrawal",
  "outOfWindowForcedTransaction",
];
assert.deepEqual(
  evidence.variantMatrix.map((row) => row.variant),
  REQUIRED_VARIANTS,
  "the six omitted/out-of-window variants must be enumerated exactly once, in order",
);

const variantChecks = new Map();
for (const row of evidence.variantMatrix) {
  assert.ok(
    proofSource.includes(`    ${row.constructor} {`),
    `variant ${row.variant} names constructor ${row.constructor}, which is not declared in proof.ak`,
  );
  assert.ok(
    proofSource.includes(`transition_trace.${row.rootDomain}`),
    `variant ${row.variant} names root domain ${row.rootDomain}, which proof.ak does not use`,
  );
  assert.ok(
    proofSource.includes(`header.${row.headerRoot}`) &&
      proofSource.includes(`header.${row.headerCount}`),
    `variant ${row.variant} names header fields ${row.headerRoot}/${row.headerCount}, which proof.ak does not read`,
  );
  assert.ok(
    row.aiken.selectors.length > 0,
    `variant ${row.variant} has no executable selector; a structural N/A may not rest on prose`,
  );
  const module = declaredModuleOf(row.aiken);
  for (const selector of row.aiken.selectors) {
    selectorSafe(selector);
  }
  variantChecks.set(
    row.variant,
    row.aiken.selectors.map((selector) => ({ module, selector })),
  );
}

const inherited = evidence.inheritedEventWindowGuard;
const inheritedModule = declaredModuleOf(inherited);
for (const selector of inherited.selectors) {
  selectorSafe(selector);
}
const inheritedChecks = inherited.selectors.map((selector) => ({
  module: inheritedModule,
  selector,
}));

const q47 = evidence.q47AikenSelectors;
const q47Module = declaredModuleOf(q47);
for (const selector of q47.selectors) {
  selectorSafe(selector.name);
  assert.ok(
    typeof selector.claim === "string" && selector.claim.length > 0,
    `selector ${selector.name} must state what it proves`,
  );
  assert.ok(
    ["positive", "adjacentValidNegative", "control"].includes(selector.class),
    `selector ${selector.name} has an unknown class ${String(selector.class)}`,
  );
}
const q47Checks = q47.selectors.map((selector) => ({
  module: q47Module,
  selector: selector.name,
}));
const selectorClassCounts = q47.selectors.reduce((counts, selector) => {
  counts[selector.class] = (counts[selector.class] ?? 0) + 1;
  return counts;
}, {});
assert.deepEqual(
  selectorClassCounts,
  { positive: 4, adjacentValidNegative: 2, control: 2 },
  "Q47 requires four positives, two adjacent valid-block negatives and two controls",
);

// Exhaustiveness, not passage: a selector living in the Q47 module that the
// artifact does not list would make the published set incomplete. This scan
// backs no count — every count below comes from the runner.
const q47ModuleSource = await readRepositoryFile(q47.file);
assert.deepEqual(
  [...q47ModuleSource.matchAll(/^test\s+([a-z0-9_]+)\(/gmu)]
    .map((match) => match[1])
    .sort(),
  q47Checks.map(({ selector }) => selector).sort(),
  `${q47.file} declares a different selector set than the artifact claims`,
);

const twins = evidence.q47TypeScriptTwins;
const twinsPackageDirectory = twins.file.split("/").slice(0, 2).join("/");
const twinsTestFile = twins.file.split("/").slice(2).join("/");

// ---------------------------------------------------------------------------
// Phase 2 — execute. One `aiken check` invocation covers every cited on-chain
// selector; the TypeScript twins are run by their own package's Vitest CLI.
// ---------------------------------------------------------------------------

const declaredAikenChecks = [];
const seenChecks = new Set();
for (const check of [
  ...inheritedChecks,
  ...q47Checks,
  ...[...variantChecks.values()].flat(),
]) {
  const key = `${check.module}#${check.selector}`;
  if (!seenChecks.has(key)) {
    seenChecks.add(key);
    declaredAikenChecks.push(check);
  }
}
const aikenSelectors = [
  ...new Set(declaredAikenChecks.map(({ selector }) => selector)),
];
// Issue #538. The artifact claimed these selectors pass "under both stock
// v1.1.22 and fork v1.1.23" while this gate spawned whatever `aiken` resolved to
// — one compiler, and by default the stock one. A dual-compiler claim needs two
// measurements, so both are taken here: the released authority compiler and the
// patched fork `.github/workflows/aiken-ci.yml` uses to execute the suite. Each
// compiler's identity is read from the binary itself and pinned, so a shadowed
// or stale build cannot supply a result under the other's name.
// RETIRED 2026-08-13 (#579, owner ruling A, confirmed by Ruling 1 on the
// escalation): the stock arm of this gate is gone with the stock compiler
// itself. Stock v1.1.22 has no remaining role — it does not compile, format,
// build or execute anything — so requiring the cited selectors to ALSO pass
// under it would assert agreement with a compiler the owner ruled unsound. The
// measured divergence that ended it: `user_events/tx_order_v1.mint` built to
// stock `81fa79f4…` 9,851 bytes versus fork `b5541807…` 10,071 bytes, and the
// stock side is the WRONG artifact — stock keys generated expect-decoders by
// local type name only (issue #521) and `MintRedeemer` is shared by 12 modules.
// That evidence is preserved as the retirement record in
// `.github/workflows/aiken-ci.yml`, not as a recurring gate.
//
// What survives unchanged is the property this gate was raised for (#538): it
// still EXECUTES every selector it cites, under a compiler whose identity it
// measures rather than describes, so it cannot publish a claim it did not run.
const forkAiken = forkAikenBinary();
const measuredCompilerVersions = {
  fork: aikenCompilerVersion(forkAiken),
};
const runDeclaredSelectors = (binary, role) =>
  deriveAikenOutcome({
    label: `Q47 structural N/A on-chain selectors (${role})`,
    declared: declaredAikenChecks,
    ...runAikenCheck({
      projectRoot: aikenProjectRoot,
      selectors: aikenSelectors,
      binary,
    }),
  });
const measuredKeys = (outcome) =>
  outcome.measured
    .map(({ module, selector }) => `${module}#${selector}`)
    .sort();

const forkOutcome = runDeclaredSelectors(forkAiken, "pinned fork");
assert.equal(
  forkOutcome.passed,
  declaredAikenChecks.length,
  "every cited on-chain selector must be measured as passing under the pinned fork",
);
const forkMeasuredSelectors = new Set(measuredKeys(forkOutcome));
const passedIn = (measured) => (checks) =>
  checks.filter(({ module, selector }) => measured.has(`${module}#${selector}`))
    .length;
// One compiler, one measured set: every per-variant count below is the pinned
// fork's, so this is the only "measured passing" predicate there is.
const forkPassed = passedIn(forkMeasuredSelectors);

// The compiler identities the artifact publishes are the ones just spawned, and
// the fork pin is bound to the exact revision CI installs — a fork built from a
// different commit is a different compiler and may not answer to this pin.
const workflowSource = await readRepositoryFile(
  ".github/workflows/aiken-ci.yml",
);
assert.equal(
  evidence.compilers.fork.version,
  measuredCompilerVersions.fork,
  "published patched-fork identity is not the compiler this gate spawned",
);
assert.equal(
  forkAiken,
  process.env[evidence.compilers.fork.binaryEnv] ??
    evidence.compilers.fork.binaryDefault,
  "the patched fork this gate spawned is not the one the published selection rule names",
);
assert.ok(
  evidence.compilers.fork.version.endsWith(
    `+${evidence.compilers.fork.rev.slice(0, 7)}`,
  ),
  "the patched fork reported a build that is not the revision the artifact pins",
);
assert.ok(
  workflowSource.includes(`AIKEN_FORK_REV: ${evidence.compilers.fork.rev}`),
  "the pinned fork revision is not the one .github/workflows/aiken-ci.yml installs",
);

const twinsRun = runVitest({
  packageRoot: resolve(repositoryRoot, twinsPackageDirectory),
  testFile: twinsTestFile,
});
const twinsOutcome = deriveVitestOutcome({
  label: `Q47 TypeScript twins ${twins.file}`,
  requiredTitles: twins.titles,
  ...twinsRun,
});
const collectedTwinTitles = twinsRun.report.testResults
  .flatMap((file) => file.assertionResults ?? [])
  .map((assertion) => assertion.title);

// ---------------------------------------------------------------------------
// Phase 3 — the artifact's published numbers are pins, checked against what the
// runners measured. No `measured` field is ever compared to another field of
// the same object.
// ---------------------------------------------------------------------------

for (const [variant, checks] of variantChecks) {
  assert.equal(
    forkPassed(checks),
    checks.length,
    `variant ${variant} cites ${String(checks.length)} selector(s) but only ${String(forkPassed(checks))} were measured passing`,
  );
}

const inheritedPassed = forkPassed(inheritedChecks);
assert.equal(
  inherited.measured.selectors,
  inheritedChecks.length,
  "published inherited selector count drifted from the artifact's selector list",
);
assert.equal(
  inherited.measured.passed,
  inheritedPassed,
  "published inherited pass count is not the number of inherited selectors the runner passed",
);
assert.equal(
  inherited.measured.failed,
  inheritedChecks.length - inheritedPassed,
  "published inherited failure count is not the number the runner did not pass",
);
assert.equal(
  inherited.measured.stockPassed,
  inheritedPassed,
  "published inherited released-compiler pass count is not what that compiler passed",
);
assert.equal(
  inherited.measured.forkPassed,
  forkPassed(inheritedChecks),
  "published inherited patched-fork pass count is not what that compiler passed",
);

const q47Passed = forkPassed(q47Checks);
assert.equal(
  q47.measured.selectors,
  q47Checks.length,
  "published Q47 selector count drifted from the artifact's selector list",
);
assert.equal(
  q47.measured.passed,
  q47Passed,
  "published Q47 pass count is not the number of Q47 selectors the runner passed",
);
assert.equal(
  q47.measured.failed,
  q47Checks.length - q47Passed,
  "published Q47 failure count is not the number the runner did not pass",
);
assert.equal(
  q47.measured.stockPassed,
  q47Passed,
  "published Q47 released-compiler pass count is not what that compiler passed",
);
assert.equal(
  q47.measured.forkPassed,
  forkPassed(q47Checks),
  "published Q47 patched-fork pass count is not what that compiler passed",
);

assert.equal(
  twins.measured.tests,
  twinsOutcome.collected,
  "published twin test count is not the number of tests the runner collected",
);
assert.equal(
  twins.measured.passed,
  twinsOutcome.passed,
  "published twin pass count is not the number of tests the runner passed",
);
assert.equal(
  twins.measured.failed,
  twinsOutcome.collected - twinsOutcome.passed,
  "published twin failure count is not the number the runner did not pass",
);
assert.deepEqual(
  collectedTwinTitles,
  twins.titles,
  `${twins.file} ran a different test title list than the artifact claims`,
);

assert.deepEqual(
  evidence.runners,
  [
    aikenPublishedCommand({
      projectDirectory: aikenProjectDirectory,
      selectors: aikenSelectors,
      command: evidence.compilers.fork.binaryDefault,
    }),
    vitestPublishedCommand({
      packageDirectory: twinsPackageDirectory,
      testFile: twinsTestFile,
    }),
  ],
  "published runner commands drifted from the commands this gate executes",
);
assert.equal(
  twins.command,
  vitestPublishedCommand({
    packageDirectory: twinsPackageDirectory,
    testFile: twinsTestFile,
  }),
  "the twin command recorded in the artifact is not the command this gate runs",
);

// ## The standalone surface must genuinely not exist

const inventory = evidence.standaloneInventory;
for (const key of [
  "aikenValidatorDirectories",
  "aikenLibDirectories",
  "catalogueCategories",
  "prepareCommands",
  "submitCommands",
]) {
  assert.deepEqual(
    inventory[key],
    [],
    `standalone inventory ${key} must be empty for a structural N/A`,
  );
}
assert.ok(
  !orderedCategories.some((category) => /q47/iu.test(category)),
  "a standalone q47 catalogue category appeared; the structural N/A no longer holds",
);
const searchRoots = [
  "onchain/aiken/validators/fraud-proofs",
  "onchain/aiken/lib/midgard/fraud-proofs",
  "demo/midgard-fault-proofs/src",
  "demo/midgard-sdk/src/fraud-proof",
];
let inventoryChecks = 0;
for (const root of searchRoots) {
  const entries = await readdir(resolve(repositoryRoot, root), {
    recursive: true,
  });
  for (const entry of entries) {
    const normalized = entry.replaceAll("\\", "/");
    for (const forbidden of inventory.forbiddenPathSubstrings) {
      assert.ok(
        !`${root}/${normalized}`.includes(forbidden),
        `found ${root}/${normalized}, which matches the forbidden standalone-surface pattern ${forbidden}`,
      );
    }
    inventoryChecks += 1;
  }
}
assert.ok(
  inventoryChecks > 0,
  "the standalone-surface scan inspected zero paths, so it proves nothing",
);

// ## Closure outputs

assert.equal(
  evidence.closureOutputs.length,
  10,
  "GOAL_SPEC.md 9.1 has exactly ten closure outputs",
);
assert.deepEqual(
  evidence.closureOutputs.map((row) => row.output),
  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
  "closure outputs must be listed 1..10 in order",
);
for (const row of evidence.closureOutputs) {
  assert.ok(
    ["LOCAL_PASS", "N/A", "PARENT_PENDING"].includes(row.status),
    `closure output ${String(row.output)} has an unknown status ${String(row.status)}`,
  );
  if (row.status === "LOCAL_PASS") {
    assert.ok(
      typeof row.evidence === "string" && row.evidence.length > 0,
      `closure output ${String(row.output)} claims LOCAL_PASS with no evidence`,
    );
  } else {
    assert.ok(
      typeof row.justification === "string" && row.justification.length > 0,
      `closure output ${String(row.output)} is not LOCAL_PASS and must carry a justification`,
    );
    assert.ok(
      typeof row.owner === "string" && row.owner.length > 0,
      `closure output ${String(row.output)} is not LOCAL_PASS and must name an owner`,
    );
  }
}
// Outputs 1-4 are the ones a structural disposition genuinely owns; they may
// never be waived.
for (const output of [1, 2, 3, 4]) {
  const row = evidence.closureOutputs.find((entry) => entry.output === output);
  assert.equal(
    row.status,
    "LOCAL_PASS",
    `closure output ${String(output)} is owned by this disposition and cannot be waived`,
  );
}
// Output 4's evidence prose states pass ratios in words. Prose that states a
// number is still a published count, so the numbers are parsed back out and
// held to the same runner results as the `measured` blocks.
const output4 = evidence.closureOutputs.find((row) => row.output === 4);
const prosePassRatios = [
  ...output4.evidence.matchAll(/(\d+) of (\d+) passing/gu),
].map((match) => [Number(match[1]), Number(match[2])]);
assert.deepEqual(
  prosePassRatios,
  [
    [q47Passed, q47Checks.length],
    [inheritedPassed, inheritedChecks.length],
  ],
  "closure output 4 states pass ratios that the runners did not measure",
);
assert.ok(
  /0 failures/u.test(output4.evidence) &&
    q47Passed === q47Checks.length &&
    inheritedPassed === inheritedChecks.length,
  "closure output 4 claims zero failures, which the runner results do not support",
);
// The dual-compiler sentence is a published claim like any other, so it holds
// only while both compilers really were run over the identical selector set.
assert.ok(
  /executed under both compilers and the two passed the identical set/u.test(
    output4.evidence,
  ) &&
    forkPassed(q47Checks) === q47Passed &&
    forkPassed(inheritedChecks) === inheritedPassed,
  "closure output 4 claims both compilers passed the same selectors, which the runner results do not support",
);

// No LIVE_PASS may ever be claimed from a family-local artifact. Only a
// *status* is checked, so the note may still say the artifact disclaims it.
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
      if (key === "status" || key === "disposition") {
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
  assert.ok(
    typeof finding.id === "string" && finding.id.length > 0,
    "each residual finding needs an id",
  );
  assert.ok(
    typeof finding.owner === "string" && finding.owner.length > 0,
    `residual finding ${String(finding.id)} must name an owner`,
  );
}

// ## The summary is recomputed from measured results, so it cannot lie

const executedChecks = forkOutcome.passed + twinsOutcome.passed;
const recomputed = {
  variants: evidence.variantMatrix.length,
  inheritedSelectors: inheritedPassed,
  q47AikenSelectors: q47Passed,
  q47TypeScriptTwins: twinsOutcome.passed,
  executedChecks,
  closureOutputs: evidence.closureOutputs.length,
  localPass: evidence.closureOutputs.filter(
    (row) => row.status === "LOCAL_PASS",
  ).length,
  structuralNa: evidence.closureOutputs.filter((row) => row.status === "N/A")
    .length,
  parentPending: evidence.closureOutputs.filter(
    (row) => row.status === "PARENT_PENDING",
  ).length,
  standalonePaths:
    inventory.aikenValidatorDirectories.length +
    inventory.aikenLibDirectories.length +
    inventory.prepareCommands.length +
    inventory.submitCommands.length,
  standaloneCategories: inventory.catalogueCategories.length,
};
assert.deepEqual(
  evidence.summary,
  recomputed,
  "the recorded summary disagrees with what the runners measured",
);
assert.equal(evidence.summary.standalonePaths, 0, "standalone paths must be 0");
assert.equal(
  evidence.summary.standaloneCategories,
  0,
  "standalone categories must be 0",
);

// ## Parent integration is declared, never performed here

assert.equal(
  evidence.parentIntegration.owner,
  "parent",
  "matrix and ledger integration is parent-owned",
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
  // Issue #538: a per-compiler claim needs the named compiler, so an
  // unresolvable binary must fail the gate rather than fall through to PATH.
  [
    "--compiler-fixture=missing",
    /ERR_AIKEN_BINARY_UNAVAILABLE: .*MIDGARD_FORK_AIKEN_BIN/su,
  ],
];
for (const [flag, expectedDiagnostic] of selfTests) {
  const selfTest = runSelfTest(flag);
  assert.notEqual(
    selfTest.status,
    0,
    `Q47 structural-N/A gate accepted the seeded defect ${flag}`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `Q47 structural-N/A gate rejected ${flag} without its specific diagnostic`,
  );
}
// Positive controls: the same harness must still accept real passing runs, so
// the rejections above cannot be a gate that rejects everything.
for (const [flag, expectedStdout] of [
  ["--vitest-fixture=passing", /vitest fixture passing: 1\/1 passed/u],
  ["--aiken-fixture=passing", /aiken fixture passing: 1\/1 passed/u],
  // The identity is read from the spawned binary, not from a constant …
  [
    "--compiler-fixture=stub-version",
    /compiler fixture stub-version: 1\/1 passed/u,
  ],
  // … and the pinned fork really runs a real project under its own name. The
  // `dual` fixture this replaces required stock to agree, which ruling A
  // retired; divergence detection needs no second compiler and stays covered by
  // the `--aiken-fixture=*` negatives above.
  ["--compiler-fixture=fork", /compiler fixture fork: 1\/1 passed/u],
]) {
  const control = runSelfTest(flag);
  assert.equal(
    control.status,
    0,
    `Q47 structural-N/A gate rejected a passing fixture (${flag}): ${control.stderr}`,
  );
  assert.match(control.stdout, expectedStdout);
}

const report = {
  status: "PASS",
  variants: evidence.summary.variants,
  inheritedSelectors: evidence.summary.inheritedSelectors,
  q47AikenSelectors: evidence.summary.q47AikenSelectors,
  q47TypeScriptTwins: evidence.summary.q47TypeScriptTwins,
  closureOutputs: evidence.summary.closureOutputs,
  localPass: evidence.summary.localPass,
  structuralNa: evidence.summary.structuralNa,
  parentPending: evidence.summary.parentPending,
  standalonePaths: evidence.summary.standalonePaths,
  standaloneCategories: evidence.summary.standaloneCategories,
  executedChecks,
  compilers: measuredCompilerVersions,
  aikenSelectorsCollected: forkOutcome.collected,
  twinTestsCollected: twinsOutcome.collected,
  inventoryPathsScanned: inventoryChecks,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q47 structural N/A: PASS (${String(evidence.summary.variants)} variants, ${String(
      executedChecks,
    )} runner-executed checks, ${String(inventoryChecks)} paths scanned, 0 standalone paths, 0 standalone categories)`,
  );
  console.log(
    `Q47 on-chain leg: ${String(forkOutcome.passed)}/${String(
      declaredAikenChecks.length,
    )} cited selectors executed and passed under ${measuredCompilerVersions.fork}`,
  );
}
