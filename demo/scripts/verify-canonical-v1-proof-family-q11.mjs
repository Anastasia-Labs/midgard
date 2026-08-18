#!/usr/bin/env node
// Verifies the Q11 (`no-input`) binding of GOAL_SPEC.md 9.1 outputs 1 to 10.
//
// The rule enforced here is the one the per-family closure contract states: an
// output may be published LOCAL_PASS only when the artifact's claim is
// re-derivable from this tree, an OPEN cell must say why it is open and who owns
// it, and an N/A cell must justify itself. Nothing is taken on the artifact's
// word. Every rule anchor, catalogue index, codec field, spec section, on-chain
// selector, builder name, CLI verb, emulator marker and matrix cell it names is
// re-read out of its own source file, and every count it publishes is derived
// from a runner report this gate produced: one batched `aiken check -e`
// invocation under the pinned fork compiler, and one Vitest run per cited suite.
//
// Two disciplines are inherited deliberately from the sibling gates.
//
// From verify-canonical-v1-proof-family-q1x.mjs (issue #533, finding V-2 of
// #519): existence is never accepted in place of passage. A count of `test`
// declarations in an Aiken module or of `it(` lines in a suite is not a count of
// checks that passed — nine throwing bodies, an `it.skip`, or a selector that
// collects nothing leave such a count untouched — so source scanning survives
// here only where it backs no count: the structural claims about roles, fields,
// verbs, markers and matrix text.
//
// From verify-canonical-v1-q60-commit-end-time-bound.mjs: a gate that publishes
// an on-chain result has to know which compiler produced it. The identity is
// measured from the spawned binary and required to match the fork pin EXACTLY,
// not by prefix, because a second fork build of the same 1.1.23 base reports a
// different `+<rev>` suffix and would supply this family's result under a
// compiler .github/workflows/aiken-ci.yml does not pin.
//
// One discipline is NOT inherited from the Q10 twin, because this family's shape
// forbids it. Q10's emulator suite carries only double-spend journeys, so a
// whole-file marker scan is sound there. This family's lifecycle shares
// submit-init-emulator-ledger-rules.test.ts with two sibling ledger-rule
// families, and two of the stages Q10 claims from its markers — the
// `slashingApproach` route — are asserted by those siblings and NOT by this
// family's journey. A whole-file scan would therefore have let a sibling satisfy
// this family's claim. Every marker here is required inside this family's own
// `it` block, the two absent markers are required to be absent from that block
// AND present elsewhere in the file (so the gap is a real per-family omission
// rather than a renamed symbol), and the count of lifecycles the file executes is
// published separately from the count this family owns.
//
// Outputs 5 to 8 are bound family-locally in the SHARED four-family artifact
// docs/exec-plans/evidence/canonical-v1-proof-family-q1x-v1.json, whose own gate
// demo/scripts/verify-canonical-v1-proof-family-q1x.mjs is the executable
// authority for those cells and runs in the same battery. That gate spawns five
// Vitest suites, three of them emulator suites driving the complete correction
// path at forced branch level 22 and at the admissible 296-input Cardano spend
// shape. Re-running them from here would double the battery's cost and measure
// nothing the shared gate does not already measure, so this gate DELEGATES their
// passage and asserts the delegation instead: it re-reads the shared artifact's
// Q11 cells, requires each to be LOCAL_PASS with no open cell anywhere in that
// artifact, Q11 present in both carriage remediations' measured-family lists and
// the tier-routed remediation naming this family's own binding stage, and then
// re-checks in this tree that the concrete Q11 surfaces those cells describe
// still exist and are still wired. If the shared artifact's Q11 cells are ever
// re-opened, this gate fails closed rather than keeping four rows.
//
// usage: node demo/scripts/verify-canonical-v1-proof-family-q11.mjs [--json]

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { runFixtureMode } from "./lib/runner-fixtures.mjs";
import {
  aikenCompilerVersion,
  aikenModuleName,
  aikenPublishedCommand,
  aikenSelectorPattern,
  deriveAikenOutcome,
  deriveVitestOutcome,
  forkAikenBinary,
  runAikenCheck,
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

const GOAL_ID = "Q11";
const FAMILY = "no-input";
const OUTPUTS = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const DELEGATED_OUTPUTS = [5, 6, 7, 8];
const ARTIFACT =
  "docs/exec-plans/evidence/canonical-v1-proof-family-q11-v1.json";
const SHARED_ARTIFACT =
  "docs/exec-plans/evidence/canonical-v1-proof-family-q1x-v1.json";
const SHARED_GATE = "demo/scripts/verify-canonical-v1-proof-family-q1x.mjs";
const AIKEN_PROJECT_DIRECTORY = "onchain/aiken";
const aikenProjectRoot = resolve(repositoryRoot, AIKEN_PROJECT_DIRECTORY);

// `.github/workflows/aiken-ci.yml` AIKEN_FORK_VERSION. The stock v1.1.22 that
// used to compile the blueprint ships a live unsound-codegen defect and is
// retired from every role; this is the only compiler entitled to produce this
// family's on-chain result.
const PINNED_FORK_COMPILER = "aiken v1.1.23+2a78108";

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

const artifactText = await readRepositoryFile(ARTIFACT);
const evidence = JSON.parse(artifactText);

// ---------------------------------------------------------------------------
// Identity
// ---------------------------------------------------------------------------

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-proof-family-q11.v1",
  "evidence schema identity drifted",
);
assert.equal(evidence.version, 1, "evidence version drifted");
assert.deepEqual(evidence.goalIds, [GOAL_ID], "bound goal ids drifted");
assert.equal(evidence.family, FAMILY, "bound family drifted");
assert.ok(
  Array.isArray(evidence.prescribedBy) && evidence.prescribedBy.length > 0,
  "the artifact must name what prescribes it, so a reader can check the surface against its contract",
);
assert.ok(
  typeof evidence.integrationIssue === "string" &&
    evidence.integrationIssue.length > 0,
  "the integrating owner must be stated; the manifest row prescribes these surfaces without naming an issue and an invented number would be an unmeasured claim",
);

// The live half of a family's closure belongs exclusively to the Q57 sweep and
// QG3 (GOAL_SPEC.md 9.1). A family-local artifact may never record it, so the
// check is on the artifact's raw text rather than on a walk of status keys: a
// status smuggled into a prose field would pass the walk.
assert.ok(
  !artifactText.includes("LIVE_PASS"),
  "a family-local artifact must never record a LIVE_PASS status anywhere, in a status field or in prose; live status belongs to Q57/QG3",
);

// ---------------------------------------------------------------------------
// Compiler identity — measured, never assumed
// ---------------------------------------------------------------------------

// Local runs name the fork with MIDGARD_AIKEN_BIN and the workflow's family
// steps do the same; MIDGARD_FORK_AIKEN_BIN is honoured as the fallback name for
// the same role. Whichever is resolved must report the pin itself.
const aikenBinaryPath = process.env.MIDGARD_AIKEN_BIN ?? forkAikenBinary();
assert.equal(
  evidence.compiler.requiredVersion,
  PINNED_FORK_COMPILER,
  "the artifact's pinned compiler is not the one .github/workflows/aiken-ci.yml pins; the pin may not be relaxed from the artifact side",
);
const measuredCompiler = aikenCompilerVersion(aikenBinaryPath);
assert.equal(
  measuredCompiler,
  PINNED_FORK_COMPILER,
  `ERR_Q11_WRONG_TEST_COMPILER: ${aikenBinaryPath} reports "${measuredCompiler}" but this family must execute under exactly "${PINNED_FORK_COMPILER}"; set MIDGARD_AIKEN_BIN (or MIDGARD_FORK_AIKEN_BIN) to the patched fork`,
);
assert.equal(
  evidence.compiler.measuredVersion,
  measuredCompiler,
  "the artifact's recorded compiler identity is not the one the resolved binary reports",
);

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

// A GitHub-flavoured table row splits into a leading empty cell, one cell per
// column, and a trailing empty cell, so column N of the header addresses column
// N of every row below it.
const tableCells = (line) => line.split("|").map((cell) => cell.trim());

const columnIndex = ({ file, headerLine, header }) => {
  const index = headerLine.indexOf(header);
  assert.ok(
    index > 0,
    `${file} has no ${header} column, so no cell of it can be measured`,
  );
  return index;
};

// Declarations only: which titles each suite is claimed to contain. Nothing here
// decides whether any of them passed; the runner does that further down. The
// declaration order is the published-command order, so output 2's suite is
// declared before output 3's and output 9's.
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

const assertRecordedMeasurement = ({ label, recorded, outcome }) => {
  assert.deepEqual(
    recorded,
    {
      collected: outcome.collected,
      passed: outcome.passed,
      failed: outcome.collected - outcome.passed,
    },
    `${label}: the published counts are not the ones the runner produced`,
  );
};

// ---------------------------------------------------------------------------
// Output 1 — normative rule and violation identifier
// ---------------------------------------------------------------------------

const rule = evidence.output1RuleAndViolationIdentifier;
assert.ok(
  typeof rule.rule === "string" && rule.rule.length > 0,
  "output 1 must state the rule, not only its identifier",
);

// The identifier is a declaration in the validation package, not a string this
// artifact chose.
const identifierSource = await readRepositoryFile(
  rule.identifierDeclaration.file,
);
assert.ok(
  identifierSource.includes(rule.identifierDeclaration.declaration),
  `${rule.identifierDeclaration.file} no longer declares ${rule.identifierDeclaration.declaration}`,
);
assert.ok(
  rule.identifierDeclaration.declaration.includes(rule.violationIdentifier),
  `the declared identifier does not carry ${rule.violationIdentifier}`,
);

// The enforcement sites are counted from source because they back a published
// number, and each recorded line must really be one of them: a stale line
// reference is exactly the drift this artifact records against the matrices.
const phaseBSource = await readRepositoryFile(rule.localEnforcement.file);
const phaseBLines = phaseBSource.split(/\r?\n/u);
const enforcementCall = rule.localEnforcement.call;
const measuredEnforcementLines = phaseBLines.flatMap((line, index) =>
  line.includes(enforcementCall) ? [index + 1] : [],
);
assert.deepEqual(
  measuredEnforcementLines,
  rule.localEnforcement.measuredSiteLines,
  `${rule.localEnforcement.file} enforcement sites for ${enforcementCall} drifted from the recorded lines`,
);
assert.equal(
  rule.localEnforcement.measuredSiteCount,
  measuredEnforcementLines.length,
  "the published local-enforcement site count is not the number of sites in source",
);
assert.ok(
  measuredEnforcementLines.length > 0,
  "a rule with no local enforcement site has no violation identifier to bind",
);

// The deployed identifier side: the append-only id is the positional index
// encoded over FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT bytes, so it is re-derived
// from the order constant rather than read from a constant. Inserting a category
// rather than appending one silently re-numbers every family after it — and this
// family sits at index 1, so it is the first one such an insertion would move.
const catalogueBinding = rule.catalogueBinding;
const catalogueSource = await readRepositoryFile(catalogueBinding.file);
const orderBlock = catalogueSource.slice(
  catalogueSource.indexOf(`${catalogueBinding.orderConstant} = [`),
);
const orderedCategories = [
  ...orderBlock.slice(0, orderBlock.indexOf("]")).matchAll(/"([a-zA-Z]+)"/gu),
].map((match) => match[1]);
assert.ok(
  orderedCategories.length > 0,
  `could not read ${catalogueBinding.orderConstant} from ${catalogueBinding.file}`,
);
assert.equal(
  orderedCategories.length,
  catalogueBinding.measuredCategoryCount,
  "the published catalogue category count is not the length of the append-only order",
);
assert.equal(
  orderedCategories.indexOf(catalogueBinding.category),
  catalogueBinding.categoryIndex,
  `${catalogueBinding.category} is not at the recorded index of the append-only order`,
);
const idHexDigits = 2 * catalogueBinding.categoryIdByteCount;
assert.ok(
  catalogueSource.includes(
    `FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT = ${String(catalogueBinding.categoryIdByteCount)}`,
  ),
  `${catalogueBinding.file} does not declare a ${String(catalogueBinding.categoryIdByteCount)}-byte category id`,
);
assert.equal(
  catalogueBinding.categoryId,
  catalogueBinding.categoryIndex.toString(16).padStart(idHexDigits, "0"),
  `category id ${catalogueBinding.categoryId} is not the ${String(catalogueBinding.categoryIdByteCount)}-byte big-endian encoding of index ${String(catalogueBinding.categoryIndex)}`,
);

// The registered-families lifecycle suite is not cited for this family, and the
// exclusion is measured rather than asserted: a no-input lifecycle added there
// must be cited, so its appearance fails this gate until it is.
const notCited = rule.registeredFamiliesSuiteNotCited;
const registeredFamiliesSource = await readRepositoryFile(notCited.file);
assert.ok(
  !registeredFamiliesSource.includes(catalogueBinding.category),
  `${notCited.file} now mentions ${catalogueBinding.category}; if it carries a no-input lifecycle it must be cited and run rather than excluded`,
);
assert.ok(
  typeof notCited.reason === "string" && notCited.reason.length > 0,
  "an uncited suite must say why it is uncited",
);

// ---------------------------------------------------------------------------
// Output 2 — canonical evidence schema and strict codec agreement
// ---------------------------------------------------------------------------

const codec = evidence.output2SchemaAndCodecAgreement;

// Every field list is parsed out of its own source file and compared to its
// counterpart. Positional agreement is the property that matters: these records
// reach the chain as Plutus data, where a swapped pair of same-typed fields
// type-checks on both sides of the boundary and mis-binds silently. Three of the
// four records measured here open with the same two `Int` fields, which is
// exactly the shape where such a swap goes unnoticed.
const recordFieldNames = (block) =>
  [...block.matchAll(/^ {2}([a-z_][a-z0-9_]*):/gmu)].map((match) => match[1]);

const aikenRecordFields = async ({ module, type }) => {
  const source = await readRepositoryFile(module);
  const header = `pub type ${type} {`;
  const start = source.indexOf(header);
  assert.ok(start >= 0, `${module} no longer declares ${type}`);
  const body = source.slice(start + header.length);
  const end = body.indexOf("\n}");
  assert.ok(end > 0, `${module} declaration of ${type} is unterminated`);
  return recordFieldNames(body.slice(0, end));
};

const typescriptSchemaFields = async ({ module, schema }) => {
  const source = await readRepositoryFile(module);
  const header = `${schema} = Data.Object({`;
  const start = source.indexOf(header);
  assert.ok(start >= 0, `${module} no longer declares ${schema}`);
  const body = source.slice(start + header.length);
  const end = body.indexOf("});");
  assert.ok(end > 0, `${module} declaration of ${schema} is unterminated`);
  return recordFieldNames(body.slice(0, end));
};

const assertRecordParity = async (record, label) => {
  const aikenFields = await aikenRecordFields({
    module: record.aikenModule,
    type: record.aikenType,
  });
  const typescriptFields = await typescriptSchemaFields({
    module: record.typescriptModule,
    schema: record.typescriptSchema,
  });
  assert.deepEqual(
    aikenFields,
    typescriptFields,
    `${label}: ${record.aikenType} is not field-for-field identical across the boundary; the two sides disagree in name or order`,
  );
  assert.deepEqual(
    aikenFields,
    record.fields,
    `${label}: the published field list is not the one the two sources declare`,
  );
  assert.equal(
    record.fieldCount,
    aikenFields.length,
    `${label}: the published field count is not the number of fields the sources declare`,
  );
  return aikenFields;
};

// The shared inclusion record step-01 rides. It is Q00/Q01-owned and consumed,
// not edited, by this family — which is why the two alias bindings that connect
// it to the family are measured below rather than assumed.
await assertRecordParity(
  codec.sharedInclusionRecord,
  "output 2 shared inclusion record",
);

// A re-export is what makes the shared record THIS family's record. If either
// alias is rewritten into a family-local re-declaration, the parity above stops
// binding here and this gate says so.
assert.ok(
  Array.isArray(codec.aliasBindings) && codec.aliasBindings.length > 0,
  "output 2 must measure the alias bindings that make the shared inclusion record this family's own",
);
for (const alias of codec.aliasBindings) {
  const aliasSource = await readRepositoryFile(alias.file);
  assert.ok(
    aliasSource.includes(alias.declaration),
    `${alias.file} no longer carries the alias binding ${JSON.stringify(alias.declaration)}, so the shared inclusion record is no longer reached the way this artifact records`,
  );
}

// The family's own per-step Args records. Every one of them is measured, so a
// step whose args drifted cannot hide behind the shared record's parity.
assert.ok(
  Array.isArray(codec.familyArgsRecords) && codec.familyArgsRecords.length > 0,
  "output 2 must measure the family's own step args records, not only the shared inclusion record",
);
assert.equal(
  codec.measuredFamilyArgsRecordCount,
  codec.familyArgsRecords.length,
  "the published family args record count is not the length of the measured list",
);
for (const record of codec.familyArgsRecords) {
  assert.ok(
    typeof record.step === "string" && record.step.length > 0,
    "each family args record must name the step it belongs to",
  );
  await assertRecordParity(record, `output 2 ${record.step} args`);
}

const codecDeclaration = declareVitest(codec.suite, codec.requiredTitles);

// Structural, backing no count: the cited suite must really drive THIS family.
// Its five double-spend-specific titles are deliberately not re-cited here, so
// something has to establish that the four titles that are cited are not a
// sibling's.
const codecSuiteSource = await readRepositoryFile(codec.suite);
for (const marker of codec.suiteFamilyMarkers) {
  assert.ok(
    codecSuiteSource.includes(marker),
    `${codec.suite} no longer contains ${marker}, so it can no longer be cited as a suite that drives this family`,
  );
}

// ---------------------------------------------------------------------------
// Output 3 — flat field-hash / preimage-grammar binding
// ---------------------------------------------------------------------------

const binding = evidence.output3FlatFieldHashBinding;
const specSource = await readRepositoryFile(binding.authority);
for (const heading of binding.authoritySections) {
  assert.ok(
    specSource.includes(`\n${heading}\n`),
    `${binding.authority} no longer carries the normative section ${heading} this binding is anchored to`,
  );
}
assert.ok(
  binding.authoritySections.length > 0,
  "the format authority must be anchored to sections, not cited as a whole file",
);

// The bound field is parsed out of the spec's own field table rather than
// quoted, so a re-numbered field or a renamed commitment slot fails here.
const fieldTableStart = specSource.indexOf("### 2.5 The nine committed fields");
assert.ok(
  fieldTableStart > 0,
  `${binding.authority} no longer carries the committed-field table`,
);
const fieldTable = specSource
  .slice(fieldTableStart, specSource.indexOf("\n## 3.", fieldTableStart))
  .split(/\r?\n/u);
const fieldRow = fieldTable
  .map((line) => tableCells(line))
  .find(
    (cells) =>
      cells.length > 3 && cells[1] === String(binding.boundField.index),
  );
assert.ok(
  fieldRow !== undefined,
  `${binding.authority} committed-field table has no row for field index ${String(binding.boundField.index)}`,
);
assert.equal(
  fieldRow[2],
  binding.boundField.field,
  `field ${String(binding.boundField.index)} is not ${binding.boundField.field} in the spec's own table`,
);
assert.equal(
  fieldRow[3],
  `\`${binding.boundField.commitmentSlot}\``,
  `field ${String(binding.boundField.index)} does not commit through ${binding.boundField.commitmentSlot} in the spec's own table`,
);

// The witness-binding suite is cited because it covers the door THIS family's
// binding step builds through. That is a structural claim about the builder, so
// it is read out of the builder rather than asserted: a step-02 rewritten to
// construct its opening some other way would leave the citation pointing at a
// path the family no longer takes.
const door = binding.builderDoorBinding;
const doorSource = await readRepositoryFile(door.module);
for (const symbol of [door.planner, door.constructor, door.redeemerField]) {
  assert.ok(
    doorSource.includes(symbol),
    `${door.module} no longer references ${symbol}, so the cited field-opening suite is no longer the surface this family's binding step routes through`,
  );
}

const bindingDeclarations = binding.suites.map((suite) =>
  declareVitest(suite.file, suite.requiredTitles),
);
assert.ok(
  bindingDeclarations.length > 0,
  "output 3 must cite at least one executed suite; the spec anchor alone is prose",
);

// ---------------------------------------------------------------------------
// Output 4 — Aiken proof steps with positive and valid-block negative tests
// ---------------------------------------------------------------------------

const onchain = evidence.output4OnchainSelectors;
assert.equal(
  onchain.projectDirectory,
  AIKEN_PROJECT_DIRECTORY,
  "the on-chain project directory drifted",
);

// A negative selector must be declared `test <name>() fail`; a positive must not
// be. Reading the declaration form is what keeps a role label from becoming
// decoration: a "valid-block negative" that is really an ordinary assertion
// would otherwise pass by name alone.
const aikenTestDeclarations = (source) =>
  [...source.matchAll(/^test\s+([a-z0-9_]+)\s*\(\)\s*(fail\b)?\s*\{/gmu)].map(
    (match) => ({
      selector: match[1],
      failsByDeclaration: match[2] !== undefined,
    }),
  );

const ROLE_MUST_FAIL = {
  positive: false,
  validBlockNegative: true,
  additionalNegative: true,
};

const declaredAiken = [];
let positiveSelectors = 0;
let validBlockNegativeSelectors = 0;
let additionalNegativeSelectors = 0;

for (const module of onchain.modules) {
  const where = `${module.module} (${module.source})`;
  assert.equal(
    aikenModuleName(module.source),
    module.module,
    `${where} declares a module name that is not the one aiken derives from its path`,
  );
  const source = await readRepositoryFile(module.source);
  const declarations = aikenTestDeclarations(source);
  // The declared set must be EXACTLY the module's `test` declarations. A
  // selector added to one of these four modules and left uncited would
  // otherwise sit outside every count this artifact publishes — which is how
  // the two witness-faithful step-03 exclusion arms landed by #582 are held in
  // the census rather than absorbed into the one selector they replaced.
  assert.deepEqual(
    declarations.map(({ selector }) => selector),
    module.selectors.map(({ selector }) => selector),
    `${where} declares a different set of tests, in a different order, than the artifact cites`,
  );
  assert.equal(
    module.selectorCount,
    module.selectors.length,
    `${where} publishes a selector count that is not the length of its own selector list`,
  );
  const declarationBySelector = new Map(
    declarations.map((declaration) => [declaration.selector, declaration]),
  );
  let modulePositives = 0;
  let moduleValidBlockNegatives = 0;
  for (const { selector, role, claim } of module.selectors) {
    assert.ok(
      /^[a-z0-9_]+$/u.test(selector),
      `${where} selector ${selector} is not a focused-check-safe name`,
    );
    assert.ok(
      typeof claim === "string" && claim.length > 0,
      `${where} selector ${selector} must state what it proves`,
    );
    const mustFail = ROLE_MUST_FAIL[role];
    assert.ok(
      mustFail !== undefined,
      `${where} selector ${selector} carries unknown role ${String(role)}`,
    );
    assert.equal(
      declarationBySelector.get(selector).failsByDeclaration,
      mustFail,
      `${where} selector ${selector} is declared as a ${declarationBySelector.get(selector).failsByDeclaration ? "failure" : "success"} test, which contradicts its recorded role ${role}`,
    );
    if (role === "positive") {
      modulePositives += 1;
      positiveSelectors += 1;
    } else if (role === "validBlockNegative") {
      moduleValidBlockNegatives += 1;
      validBlockNegativeSelectors += 1;
    } else {
      additionalNegativeSelectors += 1;
    }
    declaredAiken.push({ module: module.module, selector });
  }
  // GOAL_SPEC.md 9.1 output 4 asks for positive AND valid-block negative tests
  // for the family's proof steps, so the requirement is checked per step rather
  // than in aggregate: a family could otherwise satisfy it with four positives
  // in one step and four negatives in another.
  assert.ok(
    modulePositives > 0,
    `${where} has no positive selector, so output 4 is unmet for that step`,
  );
  assert.ok(
    moduleValidBlockNegatives > 0,
    `${where} has no valid-block negative selector, so output 4 is unmet for that step`,
  );
}
assert.equal(
  onchain.declaredTotal,
  declaredAiken.length,
  "the published selector total is not the number of selectors the modules declare",
);

// `aiken check -m <module>.{<test>}` names exactly one test, so the batch is
// exact and the collected count is checked against the declared one below.
const aikenSelectors = declaredAiken.map((citation) =>
  aikenSelectorPattern(citation),
);
const aikenCommand = aikenPublishedCommand({
  projectDirectory: AIKEN_PROJECT_DIRECTORY,
  selectors: aikenSelectors,
});

// ---------------------------------------------------------------------------
// Outputs 5 to 8 — delegated to the shared Q1x gate
// ---------------------------------------------------------------------------

const delegation = evidence.delegatedOutputs;
assert.deepEqual(
  delegation.outputs,
  DELEGATED_OUTPUTS,
  "the delegated set must be exactly outputs 5 to 8; outputs 1 to 4, 9 and 10 are measured here",
);
assert.equal(
  delegation.artifact,
  SHARED_ARTIFACT,
  "the delegation must name the shared artifact that binds these cells",
);
assert.equal(
  delegation.gate,
  SHARED_GATE,
  "the delegation must name the gate that EXECUTES these cells, not only the artifact that records them",
);
// The executing gate must exist. A delegation to a missing gate is a citation.
await readRepositoryFile(SHARED_GATE);
assert.ok(
  typeof delegation.rationale === "string" && delegation.rationale.length > 0,
  "a delegated output must say why it is delegated rather than re-measured",
);

const shared = JSON.parse(await readRepositoryFile(SHARED_ARTIFACT));
assert.equal(
  shared.schema,
  "midgard.canonical-v1-proof-family-q1x.v1",
  "the shared artifact's schema identity drifted, so its cells cannot be read as this family's",
);
assert.ok(
  shared.goalIds.includes(GOAL_ID),
  `the shared artifact no longer binds ${GOAL_ID}`,
);

// The delegation contract may not be relaxed from the artifact side: LOCAL_PASS
// is the only status that carries a cell, and an artifact with any open cell
// left in it is not a settled authority for four of this family's rows.
const assertedCells = delegation.assertedCells;
assert.equal(
  assertedCells.requiredStatus,
  "LOCAL_PASS",
  "the delegation may only accept LOCAL_PASS in the shared artifact's cells",
);
assert.equal(
  assertedCells.requiredOpenCells,
  0,
  "the delegation may not accept a shared artifact that still has an open cell",
);
assert.deepEqual(
  assertedCells.requiredCarriageMembership,
  [
    "chunkedProofCarriage.measuredFamilies",
    "spendInputTierRoutedCarriage.measuredFamilies",
  ],
  "both carriage remediations must be required to have measured THIS family; a remediation measured for a sibling settles nothing here",
);
const sharedCellStatus = new Map();
for (const output of assertedCells.outputs) {
  const row = shared.outputStatus.find((entry) => entry.output === output);
  assert.ok(
    row !== undefined,
    `the shared artifact has no output-${String(output)} row to delegate to`,
  );
  assert.equal(
    row[GOAL_ID],
    assertedCells.requiredStatus,
    `the shared artifact's output-${String(output)} cell for ${GOAL_ID} is ${String(row[GOAL_ID])}, so this artifact may not publish it as ${assertedCells.requiredStatus}`,
  );
  sharedCellStatus.set(output, row[GOAL_ID]);
}
for (const output of DELEGATED_OUTPUTS) {
  assert.ok(
    sharedCellStatus.has(output),
    `delegated output ${String(output)} is not among the cells this gate asserts in the shared artifact`,
  );
}
assert.equal(
  shared.summary.openCells,
  assertedCells.requiredOpenCells,
  "the shared artifact reports open cells, so it is not the settled authority these four rows rest on",
);
assert.ok(
  shared.chunkedProofCarriage.measuredFamilies.includes(GOAL_ID),
  `${GOAL_ID} is not in the shared artifact's chunkedProofCarriage.measuredFamilies, so the depth axis is not remediated for this family`,
);
assert.ok(
  shared.spendInputTierRoutedCarriage.measuredFamilies.includes(GOAL_ID),
  `${GOAL_ID} is not in the shared artifact's spendInputTierRoutedCarriage.measuredFamilies, so the spend-input cardinality axis is not remediated for this family`,
);
// Membership alone would let a remediation measured on some other step of the
// chain settle the axis. This family's field-0 door is step-02, and that is the
// stage the cardinality axis reaches, so the stage itself is asserted.
assert.equal(
  shared.spendInputTierRoutedCarriage.bindingStages[GOAL_ID],
  assertedCells.requiredBindingStage,
  `the shared artifact routes ${GOAL_ID}'s tier-2 carriage through ${String(
    shared.spendInputTierRoutedCarriage.bindingStages[GOAL_ID],
  )}, not the recorded binding stage ${String(assertedCells.requiredBindingStage)}`,
);

// The concrete surfaces those four cells describe, re-checked in this tree. The
// shared artifact's own record for this family is the cross-check: two artifacts
// that disagree about the family's catalogue index, blueprint titles, builder,
// verbs or step modules cannot both be describing it.
const sharedFamily = shared.families.find(
  (family) => family.goalId === GOAL_ID,
);
assert.ok(
  sharedFamily !== undefined,
  `the shared artifact has no ${GOAL_ID} family entry`,
);
assert.equal(
  sharedFamily.family,
  FAMILY,
  "the shared artifact records a different family name for this goal id",
);
assert.equal(
  sharedFamily.stepCount,
  onchain.modules.length,
  "the shared artifact's step count disagrees with the number of step modules measured here",
);
assert.equal(sharedFamily.catalogueCategory, catalogueBinding.category);
assert.equal(sharedFamily.categoryIndex, catalogueBinding.categoryIndex);
assert.equal(sharedFamily.categoryId, catalogueBinding.categoryId);

const surfaces = delegation.concreteSurfaces;

// -- output 6: catalogue identifier and deployed first-step identity
const output6 = surfaces.output6;
assert.deepEqual(
  output6.blueprintTitles,
  sharedFamily.blueprintTitles,
  "the recorded blueprint titles disagree with the shared artifact's",
);
assert.equal(
  output6.titlesConstant,
  sharedFamily.titlesConstant,
  "the recorded titles constant disagrees with the shared artifact's",
);
assert.equal(
  output6.blueprintTitles.length,
  sharedFamily.stepCount,
  "the family must list one blueprint title per deployed step",
);
assert.equal(
  output6.firstStepTitle,
  output6.blueprintTitles[0],
  "the deployed first-step identity must be the first blueprint title of the chain",
);
const contractsSource = await readRepositoryFile(output6.titlesModule);
assert.ok(
  contractsSource.includes(`${output6.titlesConstant} = {`),
  `${output6.titlesModule} does not declare ${output6.titlesConstant}`,
);
assert.ok(
  contractsSource.includes(`${output6.chainBuilder} = `) ||
    contractsSource.includes(`${output6.chainBuilder} =`),
  `${output6.titlesModule} does not declare the chain builder ${output6.chainBuilder}`,
);
for (const title of output6.blueprintTitles) {
  assert.ok(
    contractsSource.includes(`"${title}"`),
    `${output6.titlesModule} does not reference blueprint title ${title}`,
  );
}
// Declaring the titles is not using them: each one must be applied by the chain
// builder, which is what turns a title into a deployed applied script.
for (const reference of output6.titleReferences) {
  assert.ok(
    contractsSource.includes(reference),
    `${output6.titlesModule} never applies ${reference}, so that step's deployed identity is unbuilt`,
  );
}
assert.equal(
  output6.titleReferences.length,
  sharedFamily.stepCount,
  "every deployed step's title must be applied, one reference per step",
);
// Applied script hashes are deliberately unpinned here: blueprint regeneration
// is parent-only (IG1) and re-pins them, so a family-local copy would become a
// second authority that goes stale on the next regeneration.
assert.equal(
  output6.blueprintHashesPinned,
  false,
  "applied script hashes must not be pinned in a family-local artifact; blueprint regeneration is parent-only",
);

// -- output 7: the DA-first builder and its admission gate
const output7 = surfaces.output7;
const builderSource = await readRepositoryFile(output7.module);
const builderExport = `export const ${output7.builder} =`;
const builderStart = builderSource.indexOf(builderExport);
assert.ok(
  builderStart >= 0,
  `${output7.module} does not export the DA-first builder ${output7.builder}`,
);
assert.ok(
  builderSource.includes(`export const ${output7.admissionGate} =`),
  `${output7.module} does not export the shared admission gate ${output7.admissionGate}`,
);
// The builder's OWN body must route through the gate. A gate exported from the
// same module proves nothing about the builder beside it.
const afterBuilder = builderSource.slice(builderStart + builderExport.length);
const nextExport = afterBuilder.indexOf("\nexport const ");
const builderBody =
  nextExport === -1 ? afterBuilder : afterBuilder.slice(0, nextExport);
assert.ok(
  builderBody.includes(`${output7.admissionGate}(`),
  `${output7.builder} does not route through ${output7.admissionGate}, so it may emit proof material from unadmitted evidence`,
);
assert.equal(
  output7.builder,
  sharedFamily.daFirstBuilder,
  "the recorded DA-first builder disagrees with the shared artifact's",
);

// -- output 8: one resumable prepare/submit chain, no hidden state
const output8 = surfaces.output8;
const binSource = await readRepositoryFile(output8.module);
for (const verb of [
  output8.prepareVerb,
  output8.initVerb,
  ...output8.submitVerbs,
]) {
  assert.ok(
    binSource.includes(`"${verb}"`),
    `${output8.module} does not accept the verb ${verb}`,
  );
}
assert.equal(
  output8.prepareVerb,
  sharedFamily.prepareVerb,
  "the recorded prepare verb disagrees with the shared artifact's",
);
assert.deepEqual(
  output8.submitVerbs,
  sharedFamily.submitVerbs,
  "the recorded submit verbs disagree with the shared artifact's",
);
assert.equal(
  output8.submitVerbs.length,
  sharedFamily.stepCount,
  "the family must expose exactly one submit verb per deployed step",
);
assert.deepEqual(
  output8.resumableStepModules,
  sharedFamily.resumableStepModules,
  "the recorded resumable step modules disagree with the shared artifact's",
);
assert.equal(
  output8.resumableStepModules.length,
  sharedFamily.stepCount - 1,
  "every non-terminal step must hand off resumably",
);
for (const modulePath of output8.resumableStepModules) {
  const moduleSource = await readRepositoryFile(modulePath);
  assert.ok(
    moduleSource.includes("nextThreadOutRef"),
    `${modulePath} is claimed resumable but does not return nextThreadOutRef, so the next step is not addressable from the previous submission alone`,
  );
}
assert.equal(
  output8.terminalStepModule,
  sharedFamily.terminalStepModule,
  "the recorded terminal step module disagrees with the shared artifact's",
);
const terminalSource = await readRepositoryFile(output8.terminalStepModule);
assert.ok(
  !terminalSource.includes("nextThreadOutRef"),
  `${output8.terminalStepModule} is the terminal step and must not return nextThreadOutRef`,
);
assert.ok(
  terminalSource.includes("fraudProofUnit"),
  `${output8.terminalStepModule} is the terminal step and must return the permanent fraudProofUnit`,
);

// ---------------------------------------------------------------------------
// Output 9 — emulator lifecycle, measured locally
// ---------------------------------------------------------------------------

const lifecycle = evidence.output9EmulatorLifecycle;
assert.equal(
  lifecycle.suite,
  sharedFamily.emulator.file,
  "the emulator suite measured here is not the one the shared artifact binds for this family",
);
assert.deepEqual(
  [...lifecycle.requiredTitles].sort(),
  [...sharedFamily.emulator.titles].sort(),
  "the lifecycle titles measured here disagree with the shared artifact's for this family",
);
assert.ok(
  lifecycle.requiredTitles.includes(lifecycle.measuredLifecycleTitle),
  "the named measured lifecycle is not among the titles this gate requires",
);
assert.equal(
  lifecycle.familyLifecycleCount,
  lifecycle.requiredTitles.length,
  "the published family lifecycle count is not the number of lifecycles this family requires by name",
);
assert.equal(
  lifecycle.hasValidBlockNegative,
  sharedFamily.emulator.hasValidBlockNegative,
  "the two artifacts disagree about whether this family has an emulator valid-block negative",
);
// A missing emulator valid-block negative is a gap, not a silence: it must be
// justified here and owned in the residual findings below.
if (!lifecycle.hasValidBlockNegative) {
  assert.ok(
    typeof lifecycle.validBlockNegativeNote === "string" &&
      lifecycle.validBlockNegativeNote.length > 0,
    "a family with no emulator valid-block negative must say so explicitly",
  );
}
const lifecycleSource = await readRepositoryFile(lifecycle.suite);

// This suite is SHARED with sibling ledger-rule families, so the file's own
// lifecycle inventory is measured and published. It is what makes the difference
// between "this gate ran four lifecycles" and "this family owns four" legible,
// and it is cross-checked against the runner's collected count below.
const suiteDeclaredTitles = [
  ...lifecycleSource.matchAll(/^ {2}it\(\s*"([^"]+)"/gmu),
].map((match) => match[1]);
assert.deepEqual(
  suiteDeclaredTitles,
  lifecycle.suiteLifecycleTitles,
  `${lifecycle.suite} declares a different set of lifecycles, in a different order, than the artifact records`,
);
assert.ok(
  suiteDeclaredTitles.includes(lifecycle.measuredLifecycleTitle),
  `${lifecycle.suite} no longer declares this family's lifecycle ${lifecycle.measuredLifecycleTitle}`,
);

// The marker scan is scoped to THIS family's own `it` block. A whole-file scan
// of a suite shared with three sibling lifecycles would let a sibling's
// assertions satisfy this family's claims — which is exactly how a slashing
// stage this journey does not assert would have been published as measured.
const blockStart = lifecycleSource.indexOf(
  `it("${lifecycle.measuredLifecycleTitle}"`,
);
assert.ok(
  blockStart >= 0,
  `${lifecycle.suite} has no \`it(\` block for ${lifecycle.measuredLifecycleTitle}, so its markers cannot be scoped to this family`,
);
const afterLifecycle = lifecycleSource.slice(blockStart);
const nextLifecycle = afterLifecycle.indexOf("\n  it(");
const lifecycleBlock =
  nextLifecycle === -1
    ? afterLifecycle
    : afterLifecycle.slice(0, nextLifecycle);
// Structural markers only. They establish that the journey this gate runs really
// asserts the stages the row claims; the PASSAGE comes from the runner report.
for (const marker of lifecycle.sourceMarkers) {
  assert.ok(
    lifecycleBlock.includes(marker),
    `${lifecycle.suite} lifecycle "${lifecycle.measuredLifecycleTitle}" no longer contains ${marker}, so the stage it backs is no longer asserted by THIS family's journey`,
  );
}
// The absent markers are the recorded gap. They must be absent from this
// family's block and present elsewhere in the file: absent-and-gone-everywhere
// would make the gap vacuous, and present-in-the-block would make the residual
// finding false.
for (const marker of lifecycle.absentSourceMarkers ?? []) {
  assert.ok(
    !lifecycleBlock.includes(marker),
    `${lifecycle.suite} lifecycle "${lifecycle.measuredLifecycleTitle}" now asserts ${marker}; the gap recorded for it must be closed rather than kept`,
  );
  assert.ok(
    lifecycleSource.includes(marker),
    `${marker} no longer appears anywhere in ${lifecycle.suite}, so recording this family's journey as missing it is no longer a measured comparison against a sibling`,
  );
}
assert.ok(
  Array.isArray(lifecycle.assertedStages) &&
    lifecycle.assertedStages.length > 0,
  "the emulator lifecycle must enumerate the stages it asserts",
);
const lifecycleDeclaration = declareVitest(
  lifecycle.suite,
  lifecycle.requiredTitles,
);

// ---------------------------------------------------------------------------
// Output 10 — the parent-owned matrix rows, MEASURED
// ---------------------------------------------------------------------------

// Output 10 is not asserted, it is derived. Both files are parent-owned (the
// manifest's Q11 row lists them under pathsMustNotTouch), so this gate reads
// what they say today from the header-derived Status column and lets the
// measurement decide the cell. A parent edit that lands the rows flips this
// derivation instead of requiring a prose change here.
const matrices = evidence.output10MatrixRows;
assert.equal(
  matrices.owner,
  "parent",
  "the coverage and catalogue matrices are parent-owned",
);

const measureMatrixRow = async (row) => {
  const lines = (await readRepositoryFile(row.file)).split(/\r?\n/u);
  const headerCells = tableCells(lines[row.headerLine - 1]);
  assert.ok(
    /^\|[\s-]+\|/u.test(lines[row.separatorLine - 1]),
    `${row.file} line ${String(row.separatorLine)} is not the table separator this row belongs to`,
  );
  const labelIndex = columnIndex({
    file: row.file,
    headerLine: headerCells,
    header: row.labelColumnHeader,
  });
  const statusIndex = columnIndex({
    file: row.file,
    headerLine: headerCells,
    header: "Status",
  });
  const rowCells = tableCells(lines[row.line - 1]);
  assert.equal(
    rowCells[labelIndex],
    row.rowLabel,
    `${row.file} line ${String(row.line)} is not the ${row.rowLabel} row any more`,
  );
  assert.equal(
    rowCells[statusIndex],
    row.measuredStatusCell,
    `${row.file} line ${String(row.line)} Status cell drifted from the measured text`,
  );
  if (
    row.measuredRemainingCell !== null &&
    row.measuredRemainingCell !== undefined
  ) {
    const remainingIndex = columnIndex({
      file: row.file,
      headerLine: headerCells,
      header: "Remaining",
    });
    assert.equal(
      rowCells[remainingIndex],
      row.measuredRemainingCell,
      `${row.file} line ${String(row.line)} Remaining cell drifted from the measured text`,
    );
  }
  return rowCells[statusIndex];
};

let matrixRowsMeasured = 0;
for (const row of matrices.rows) {
  const statusCell = await measureMatrixRow(row);
  assert.equal(
    row.recordsLocalPass,
    statusCell.includes("LOCAL_PASS"),
    `${row.file} line ${String(row.line)} records a local status that disagrees with recordsLocalPass`,
  );
  matrixRowsMeasured += 1;
}
assert.ok(
  matrixRowsMeasured > 0,
  "output 10 must measure at least one matrix row, or its status is a claim",
);
// A positive control for the derivation above. Without it, "no row records a
// local status" could equally mean the Status column was being read from the
// wrong place, and this cell would stay OPEN for the wrong reason.
const comparison = matrices.comparisonRow;
const comparisonCells = tableCells(
  (await readRepositoryFile(comparison.file)).split(/\r?\n/u)[
    comparison.line - 1
  ],
);
assert.ok(
  comparisonCells.includes(comparison.rowLabel),
  `${comparison.file} line ${String(comparison.line)} is not the ${comparison.rowLabel} row this derivation is calibrated against`,
);
assert.ok(
  comparisonCells.includes(comparison.measuredStatusCell),
  `${comparison.file} line ${String(comparison.line)} Status cell drifted from the calibration text`,
);
assert.ok(
  comparison.measuredStatusCell.includes("LOCAL_PASS"),
  "the calibration row must be one that DOES record a local status, or the derivation cannot be shown to work",
);

const matricesRecordLocalPass = matrices.rows.every(
  (row) => row.recordsLocalPass,
);

// ---------------------------------------------------------------------------
// Execute. One batched `aiken check -e` under the pinned fork, then each cited
// suite once by its own package's Vitest CLI. Every count published below is
// read out of the resulting reports.
// ---------------------------------------------------------------------------

const aikenStartedAt = Date.now();
const aikenOutcome = deriveAikenOutcome({
  label: `${GOAL_ID} ${FAMILY} on-chain selectors`,
  declared: declaredAiken,
  ...runAikenCheck({
    projectRoot: aikenProjectRoot,
    selectors: aikenSelectors,
    binary: aikenBinaryPath,
  }),
});
const aikenWallSeconds = (Date.now() - aikenStartedAt) / 1000;
assert.equal(
  aikenOutcome.passed,
  declaredAiken.length,
  "every declared on-chain selector must be measured as passing",
);
// Exact selection: `-m <module>.{<test>}` names one test, so a run that
// collected more than the declared set was not the run these counts describe.
assert.equal(
  aikenOutcome.collected,
  declaredAiken.length,
  "the batched run collected tests the artifact did not declare",
);
assert.deepEqual(
  onchain.measured,
  {
    declared: declaredAiken.length,
    collected: aikenOutcome.collected,
    passed: aikenOutcome.passed,
    failed: aikenOutcome.collected - aikenOutcome.passed,
    positive: positiveSelectors,
    validBlockNegative: validBlockNegativeSelectors,
    additionalNegative: additionalNegativeSelectors,
  },
  "the published on-chain counts are not the ones this gate measured",
);

const vitestOutcomes = new Map();
let slowestVitestSuite = { file: null, seconds: 0 };
for (const declaration of declaredVitestFiles.values()) {
  const startedAt = Date.now();
  vitestOutcomes.set(
    declaration.file,
    deriveVitestOutcome({
      label: `${GOAL_ID} ${FAMILY} ${declaration.file}`,
      requiredTitles: declaration.titles,
      ...runVitest({
        packageRoot: resolve(repositoryRoot, declaration.packageDirectory),
        testFile: declaration.testFile,
      }),
    }),
  );
  const seconds = (Date.now() - startedAt) / 1000;
  if (seconds > slowestVitestSuite.seconds) {
    slowestVitestSuite = { file: declaration.file, seconds };
  }
}

// The published commands are recomputed from the declarations this gate just
// executed, so a runner list that drifted from the runs is a failure rather than
// a stale line in an artifact.
assert.deepEqual(
  evidence.runners,
  [
    aikenCommand,
    ...[...declaredVitestFiles.values()].map((declaration) =>
      vitestPublishedCommand({
        packageDirectory: declaration.packageDirectory,
        testFile: declaration.testFile,
      }),
    ),
  ],
  "published runner commands drifted from the commands this gate executes",
);

assertRecordedMeasurement({
  label: `output 2 ${codec.suite}`,
  recorded: codec.measured,
  outcome: vitestOutcomes.get(codecDeclaration.file),
});
for (const suite of binding.suites) {
  assertRecordedMeasurement({
    label: `output 3 ${suite.file}`,
    recorded: suite.measured,
    outcome: vitestOutcomes.get(suite.file),
  });
}
const lifecycleOutcome = vitestOutcomes.get(lifecycleDeclaration.file);
assertRecordedMeasurement({
  label: `output 9 ${lifecycle.suite}`,
  recorded: lifecycle.measured,
  outcome: lifecycleOutcome,
});
// The shared suite's declared inventory and its runner report must be the same
// size, or one of the two is not describing the run these counts come from.
assert.equal(
  suiteDeclaredTitles.length,
  lifecycleOutcome.collected,
  `${lifecycle.suite} declares ${String(suiteDeclaredTitles.length)} lifecycles but the runner collected ${String(lifecycleOutcome.collected)} tests`,
);
assert.ok(
  lifecycle.familyLifecycleCount <= lifecycleOutcome.passed,
  "this family cannot own more lifecycles than the suite executed",
);

// ---------------------------------------------------------------------------
// Output status matrix
// ---------------------------------------------------------------------------

assert.deepEqual(
  evidence.outputStatus.map((row) => row.output),
  OUTPUTS,
  "outputs 1 to 10 must be listed exactly once, in order",
);
const LOCALLY_MEASURED_OUTPUTS = OUTPUTS.filter(
  (output) => !DELEGATED_OUTPUTS.includes(output) && output !== 10,
);
let localPassOutputs = 0;
let openOutputs = 0;
let notApplicableOutputs = 0;
const notApplicable = [];
for (const row of evidence.outputStatus) {
  assert.ok(
    ["LOCAL_PASS", "OPEN", "N/A"].includes(row.status),
    `output ${String(row.output)} has an unknown status ${String(row.status)}`,
  );
  assert.ok(
    typeof row.title === "string" && row.title.length > 0,
    `output ${String(row.output)} must carry the contract's title for it`,
  );
  assert.ok(
    typeof row.evidence === "string" && row.evidence.length > 0,
    `output ${String(row.output)} must carry evidence text`,
  );
  assert.ok(
    typeof row.owner === "string" && row.owner.length > 0,
    `output ${String(row.output)} must name an owner`,
  );
  if (row.status === "OPEN") {
    assert.ok(
      typeof row.whyOpen === "string" && row.whyOpen.length > 0,
      `output ${String(row.output)} is OPEN and must say why it is open`,
    );
    openOutputs += 1;
  } else if (row.status === "N/A") {
    // GOAL_SPEC.md 9.1's closing rule: a structurally enforced rule closes with
    // an adversarial executable proof of that fact and a precise matrix N/A;
    // prose alone is insufficient.
    assert.ok(
      typeof row.whyNotApplicable === "string" &&
        row.whyNotApplicable.length > 0,
      `output ${String(row.output)} is N/A and must justify why it does not apply`,
    );
    notApplicable.push(row.output);
    notApplicableOutputs += 1;
  } else {
    localPassOutputs += 1;
  }
  // A delegated row must name where it is bound; a locally measured one must
  // not, or the reader cannot tell which gate owns its passage.
  if (DELEGATED_OUTPUTS.includes(row.output)) {
    assert.equal(
      row.delegatedTo,
      SHARED_ARTIFACT,
      `output ${String(row.output)} is delegated and must name the artifact that binds it`,
    );
  } else {
    assert.equal(
      row.delegatedTo,
      undefined,
      `output ${String(row.output)} is measured by this gate and must not be published as delegated`,
    );
  }
}
assert.deepEqual(
  evidence.notApplicableOutputs,
  notApplicable,
  "the N/A inventory is not the set of rows this artifact marks N/A",
);
assert.ok(
  typeof evidence.notApplicableNote === "string" &&
    evidence.notApplicableNote.length > 0,
  "the N/A inventory must be justified even when it is empty, so an output cannot be dropped from the ten by silence",
);
assert.equal(
  localPassOutputs + openOutputs + notApplicableOutputs,
  OUTPUTS.length,
  "every output must be accounted for exactly once",
);

// Reaching this line means every runner this gate spawned passed, so each
// locally measured output is decided by that measurement rather than by the
// artifact's word: an under-claim is as much a drift as an over-claim.
for (const output of LOCALLY_MEASURED_OUTPUTS) {
  const row = evidence.outputStatus.find((entry) => entry.output === output);
  assert.equal(
    row.status,
    "LOCAL_PASS",
    `output ${String(output)} is measured passing by this gate and must be published LOCAL_PASS`,
  );
}
// The delegated rows are decided by the shared artifact's cells, re-read above.
for (const output of DELEGATED_OUTPUTS) {
  const row = evidence.outputStatus.find((entry) => entry.output === output);
  assert.equal(
    row.status,
    sharedCellStatus.get(output),
    `output ${String(output)} must publish exactly the status the shared artifact records for ${GOAL_ID}`,
  );
}
// Output 10 is derived from the matrix measurement, never asserted.
const output10 = evidence.outputStatus.find((row) => row.output === 10);
assert.equal(
  output10.status,
  matricesRecordLocalPass ? "LOCAL_PASS" : "OPEN",
  matricesRecordLocalPass
    ? "both parent-owned matrices now record this family's local status, so output 10 must be re-decided deliberately rather than left OPEN by inertia"
    : "output 10 may not be published closed while a parent-owned matrix row still does not record this family's local status",
);
if (!matricesRecordLocalPass) {
  assert.equal(
    output10.owner,
    "parent",
    "an output 10 left OPEN on parent-owned files must be owned by the parent",
  );
  // The edit this gate refuses to perform must be handed over explicitly.
  const pendingText = evidence.parentIntegration.pendingEdits.join(" ");
  for (const row of matrices.rows) {
    assert.ok(
      pendingText.includes(row.file),
      `output 10 is OPEN on ${row.file} but the parent handoff does not list the edit`,
    );
  }
}

// ---------------------------------------------------------------------------
// Residual findings must be owned, never silenced
// ---------------------------------------------------------------------------

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
  assert.ok(
    ["defect", "gap", "observation"].includes(finding.severity),
    `residual finding ${finding.id} carries unknown severity ${finding.severity}`,
  );
}
const residualText = evidence.residualFindings
  .map((finding) => finding.finding)
  .join(" ");
// The honest residuals this family carries must be named rather than absorbed
// into a LOCAL_PASS: the parent-owned output-10 pendency, the absent emulator
// valid-block negative, and every stage this family's journey is measured NOT to
// assert.
if (!matricesRecordLocalPass) {
  for (const row of matrices.rows) {
    assert.ok(
      residualText.includes(row.file),
      `output 10 is OPEN on ${row.file} and that pendency must be owned by a residual finding`,
    );
  }
}
if (!lifecycle.hasValidBlockNegative) {
  assert.ok(
    residualText.includes("valid-block negative"),
    "this family has no emulator valid-block negative and that gap must be owned by a residual finding",
  );
}
for (const marker of lifecycle.absentSourceMarkers ?? []) {
  assert.ok(
    residualText.includes(marker),
    `this family's journey is measured not to assert ${marker} and that gap must be owned by a residual finding`,
  );
}
// The delegation is a real dependency and must be stated as one, so a reader of
// the four delegated rows knows what re-opens them.
assert.ok(
  residualText.includes(SHARED_ARTIFACT),
  "the four delegated outputs rest on the shared artifact and that dependency must be owned by a residual finding",
);

// ---------------------------------------------------------------------------
// The summary is recomputed, so it cannot lie
// ---------------------------------------------------------------------------

const vitestTestsPassed = [...vitestOutcomes.values()].reduce(
  (total, outcome) => total + outcome.passed,
  0,
);
const vitestTitlesRequired = [...declaredVitestFiles.values()].reduce(
  (total, declaration) => total + declaration.titles.length,
  0,
);
const recomputed = {
  outputs: evidence.outputStatus.length,
  localPass: localPassOutputs,
  open: openOutputs,
  notApplicable: notApplicableOutputs,
  delegatedOutputs: DELEGATED_OUTPUTS.length,
  aikenModules: onchain.modules.length,
  aikenSelectorsDeclared: declaredAiken.length,
  aikenSelectorsPassed: aikenOutcome.passed,
  aikenPositiveSelectors: positiveSelectors,
  aikenValidBlockNegativeSelectors: validBlockNegativeSelectors,
  aikenAdditionalNegativeSelectors: additionalNegativeSelectors,
  vitestSuitesExecuted: vitestOutcomes.size,
  vitestTestsPassed,
  vitestTitlesRequired,
  emulatorLifecyclesExecuted: lifecycleOutcome.passed,
  // Runner-derived like the rest: `requiredTitles` are the titles
  // deriveVitestOutcome refused to accept as absent, and every collected test in
  // that report passed. The count is published beside the whole-file one because
  // this family shares its suite with two sibling ledger-rule families.
  emulatorFamilyLifecycles: lifecycle.requiredTitles.length,
  residualFindings: evidence.residualFindings.length,
};
assert.deepEqual(
  evidence.summary,
  recomputed,
  "the recorded summary disagrees with the rows it summarizes",
);

// ---------------------------------------------------------------------------
// Parent integration is declared, never performed here
// ---------------------------------------------------------------------------

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
  // The compiler leg of this gate: an unavailable binary must fail closed rather
  // than falling through to whatever `aiken` happens to be first on PATH.
  [
    "--compiler-fixture=missing",
    /ERR_AIKEN_BINARY_UNAVAILABLE: .*could not report its version/su,
  ],
];
for (const [flag, expectedDiagnostic] of selfTests) {
  const selfTest = runSelfTest(flag);
  assert.notEqual(
    selfTest.status,
    0,
    `${GOAL_ID} proof-family gate accepted the seeded defect ${flag}`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `${GOAL_ID} proof-family gate rejected ${flag} without its specific diagnostic`,
  );
}
// Positive controls: the same harness must still accept real passing runs, so
// the rejections above cannot be a gate that rejects everything. The compiler
// control reads an identity out of a stub binary, which is the property the hard
// pin above depends on — that the version is measured from the spawned binary
// rather than supplied by a constant in this gate.
for (const [flag, expectedStdout] of [
  ["--vitest-fixture=passing", /vitest fixture passing: 1\/1 passed/u],
  ["--aiken-fixture=passing", /aiken fixture passing: 1\/1 passed/u],
  [
    "--compiler-fixture=stub-version",
    /compiler fixture stub-version: 1\/1 passed/u,
  ],
]) {
  const control = runSelfTest(flag);
  assert.equal(
    control.status,
    0,
    `${GOAL_ID} proof-family gate rejected a passing fixture (${flag}): ${control.stderr}`,
  );
  assert.match(control.stdout, expectedStdout);
}

const report = {
  status: "PASS",
  goalId: GOAL_ID,
  family: FAMILY,
  compiler: measuredCompiler,
  outputs: evidence.summary.outputs,
  localPass: evidence.summary.localPass,
  open: evidence.summary.open,
  notApplicable: evidence.summary.notApplicable,
  delegatedOutputs: evidence.summary.delegatedOutputs,
  delegatedTo: SHARED_ARTIFACT,
  delegationExecutedBy: SHARED_GATE,
  aikenModules: evidence.summary.aikenModules,
  aikenSelectorsDeclared: evidence.summary.aikenSelectorsDeclared,
  aikenSelectorsPassed: evidence.summary.aikenSelectorsPassed,
  aikenPositiveSelectors: evidence.summary.aikenPositiveSelectors,
  aikenValidBlockNegativeSelectors:
    evidence.summary.aikenValidBlockNegativeSelectors,
  aikenAdditionalNegativeSelectors:
    evidence.summary.aikenAdditionalNegativeSelectors,
  aikenWallSeconds,
  vitestSuitesExecuted: evidence.summary.vitestSuitesExecuted,
  vitestTestsPassed: evidence.summary.vitestTestsPassed,
  vitestTitlesRequired: evidence.summary.vitestTitlesRequired,
  slowestVitestSuite,
  emulatorLifecyclesExecuted: evidence.summary.emulatorLifecyclesExecuted,
  emulatorFamilyLifecycles: evidence.summary.emulatorFamilyLifecycles,
  matrixRowsMeasured,
  matricesRecordLocalPass,
  residualFindings: evidence.summary.residualFindings,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `${GOAL_ID} ${FAMILY} outputs 1-10: PASS (${String(evidence.summary.localPass)} LOCAL_PASS, ${String(
      evidence.summary.open,
    )} OPEN, ${String(evidence.summary.notApplicable)} N/A; ${String(
      evidence.summary.aikenSelectorsPassed,
    )}/${String(evidence.summary.aikenSelectorsDeclared)} on-chain selectors across ${String(
      evidence.summary.aikenModules,
    )} step modules under ${measuredCompiler} (${String(
      evidence.summary.aikenPositiveSelectors,
    )} positive, ${String(
      evidence.summary.aikenValidBlockNegativeSelectors,
    )} valid-block negative, ${String(
      evidence.summary.aikenAdditionalNegativeSelectors,
    )} further negative), ${String(evidence.summary.vitestTestsPassed)} tests over ${String(
      evidence.summary.vitestSuitesExecuted,
    )} suites with ${String(evidence.summary.vitestTitlesRequired)} required titles, ${String(
      evidence.summary.emulatorLifecyclesExecuted,
    )} emulator lifecycles (${String(
      evidence.summary.emulatorFamilyLifecycles,
    )} this family's), outputs 5-8 delegated to the Q1x gate, output 10 OPEN on ${String(
      matrixRowsMeasured,
    )} parent-owned matrix rows)`,
  );
}
