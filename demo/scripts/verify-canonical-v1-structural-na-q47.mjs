#!/usr/bin/env node
// Verifies the Q47 structural-N/A disposition (GOAL_SPEC.md 9.1).
//
// The rule enforced here: a structural N/A may be recorded only when (a) every
// violation variant it disclaims resolves to at least one EXECUTABLE selector
// that exists in the tree, (b) the standalone family surface it claims not to
// need genuinely does not exist, and (c) no closure output is marked N/A
// without a named owner and a justification. Prose is never sufficient, and an
// existence assertion is never accepted where a passage claim is published:
// every selector and vitest title named by the artifact is re-read out of its
// source file and matched, so the artifact cannot drift away from the tree.
//
// usage: node demo/scripts/verify-canonical-v1-structural-na-q47.mjs [--json]

import assert from "node:assert/strict";
import { readdir, readFile } from "node:fs/promises";
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

// ## Every one of the six variants must resolve to an existing selector

const aikenSelectorNames = async (file) => {
  const source = await readRepositoryFile(file);
  return new Set(
    [...source.matchAll(/^test\s+([a-z0-9_]+)\(/gmu)].map((match) => match[1]),
  );
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

let variantSelectorChecks = 0;
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
  const declared = await aikenSelectorNames(row.aiken.file);
  for (const selector of row.aiken.selectors) {
    assert.ok(
      /^[a-z0-9_]+$/u.test(selector),
      `selector ${selector} is not a focused-check-safe name`,
    );
    assert.ok(
      declared.has(selector),
      `variant ${row.variant} claims Aiken selector ${selector}, which is not declared in ${row.aiken.file}`,
    );
    variantSelectorChecks += 1;
  }
}

// ## Inherited Q21 guard and the new Q47 selector set

const inherited = evidence.inheritedEventWindowGuard;
const inheritedDeclared = await aikenSelectorNames(inherited.file);
assert.equal(
  inherited.selectors.length,
  inherited.measured.selectors,
  "inherited selector list length must equal the measured selector count",
);
assert.equal(
  inherited.measured.passed,
  inherited.measured.selectors,
  "every inherited selector must be measured passing",
);
assert.equal(inherited.measured.failed, 0, "inherited failures must be zero");
for (const selector of inherited.selectors) {
  assert.ok(
    inheritedDeclared.has(selector),
    `inherited selector ${selector} is not declared in ${inherited.file}`,
  );
}

const q47 = evidence.q47AikenSelectors;
const q47Declared = await aikenSelectorNames(q47.file);
assert.equal(
  q47.selectors.length,
  q47.measured.selectors,
  "Q47 selector list length must equal the measured selector count",
);
assert.equal(
  q47.measured.passed,
  q47.measured.selectors,
  "every Q47 selector must be measured passing",
);
assert.equal(q47.measured.failed, 0, "Q47 failures must be zero");
// The module must contain EXACTLY the claimed selectors: an unlisted extra
// selector would make the measured count unverifiable.
assert.deepEqual(
  [...q47Declared].sort(),
  q47.selectors.map((selector) => selector.name).sort(),
  `${q47.file} declares a different selector set than the artifact claims`,
);
for (const selector of q47.selectors) {
  assert.ok(
    /^[a-z0-9_]+$/u.test(selector.name),
    `selector ${selector.name} is not a focused-check-safe name`,
  );
  assert.ok(
    typeof selector.claim === "string" && selector.claim.length > 0,
    `selector ${selector.name} must state what it proves`,
  );
  assert.ok(
    ["positive", "adjacentValidNegative", "control"].includes(selector.class),
    `selector ${selector.name} has an unknown class ${String(selector.class)}`,
  );
}
const selectorClassCounts = q47.selectors.reduce((counts, selector) => {
  counts[selector.class] = (counts[selector.class] ?? 0) + 1;
  return counts;
}, {});
assert.deepEqual(
  selectorClassCounts,
  { positive: 4, adjacentValidNegative: 2, control: 2 },
  "Q47 requires four positives, two adjacent valid-block negatives and two controls",
);

// ## TypeScript twins

const twins = evidence.q47TypeScriptTwins;
const twinSource = await readRepositoryFile(twins.file);
assert.equal(
  twins.titles.length,
  twins.measured.tests,
  "twin title list length must equal the measured test count",
);
assert.equal(
  twins.measured.passed,
  twins.measured.tests,
  "every twin must be measured passing",
);
assert.equal(twins.measured.failed, 0, "twin failures must be zero");
const declaredTitles = [...twinSource.matchAll(/\n\s+it\("([^"]+)"/gu)].map(
  (match) => match[1],
);
assert.deepEqual(
  declaredTitles,
  twins.titles,
  `${twins.file} declares a different test title list than the artifact claims`,
);
assert.ok(
  !/\b(it|test|describe)\.skip\s*\(/u.test(twinSource),
  "no Q47 twin may be skipped",
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

// ## The summary is recomputed, so it cannot lie

const recomputed = {
  variants: evidence.variantMatrix.length,
  inheritedSelectors: inherited.selectors.length,
  q47AikenSelectors: q47.selectors.length,
  q47TypeScriptTwins: twins.titles.length,
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
  "the recorded summary disagrees with the rows it summarizes",
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

const executableChecks =
  variantSelectorChecks +
  inherited.selectors.length +
  q47.selectors.length +
  twins.titles.length;

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
  executableChecks,
  inventoryPathsScanned: inventoryChecks,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q47 structural N/A: PASS (${String(evidence.summary.variants)} variants, ${String(
      executableChecks,
    )} executable checks, ${String(inventoryChecks)} paths scanned, 0 standalone paths, 0 standalone categories)`,
  );
}
