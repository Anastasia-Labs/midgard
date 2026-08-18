#!/usr/bin/env node
// Verifies the Q14 (`zero-input`) binding of GOAL_SPEC.md 9.1 outputs 1 to 10.
//
// The rule enforced here is the one the per-family closure contract states: an
// output may be published LOCAL_PASS only when the artifact's claim is
// re-derivable from this tree, an OPEN cell must say why it is open and who owns
// it, and an N/A cell must justify itself. Nothing is taken on the artifact's
// word. Every rule anchor, catalogue index, codec field, spec table row,
// on-chain selector, builder name, CLI verb, emulator marker and matrix cell it
// names is re-read out of its own source file, and every count it publishes is
// derived from a runner report this gate produced: one batched `aiken check -e`
// invocation under the pinned fork compiler, and one Vitest run per cited suite.
//
// Four disciplines are inherited deliberately from the sibling gates.
//
// From verify-canonical-v1-proof-family-q1x.mjs (issue #533, finding V-2 of
// #519): existence is never accepted in place of passage. A count of `test`
// declarations in an Aiken module or of `it(` lines in a suite is not a count of
// checks that passed — eight throwing bodies, an `it.skip`, or a selector that
// collects nothing leave such a count untouched — so source scanning survives
// here only where it backs no count: the structural claims about roles, scopes,
// fields, verbs, markers and matrix text.
//
// From verify-canonical-v1-q60-commit-end-time-bound.mjs: a gate that publishes
// an on-chain result has to know which compiler produced it. The identity is
// measured from the spawned binary and required to match the fork pin EXACTLY,
// not by prefix, because a second fork build of the same 1.1.23 base reports a
// different `+<rev>` suffix and would supply this family's result under a
// compiler .github/workflows/aiken-ci.yml does not pin.
//
// From verify-canonical-v1-proof-family-q11.mjs and its Q12 twin: this family's
// emulator coverage shares submit-init-emulator-ledger-rules.test.ts with two
// sibling ledger-rule families, so every marker is required inside the family's
// OWN `it` block, and the count of blocks the file executes is published
// separately from the count this family owns.
//
// From verify-canonical-v1-proof-family-q11.mjs: this family opens one of the
// nine section-2.5 committed fields through a section-8.8 door, so output 3 is
// measured as a bound field rather than as an absence.
//
// Four things are NOT inherited, because this family's shape forbids it.
//
// 1. This family owns TWO blocks in the shared emulator suite, not one: an
//    end-to-end lifecycle AND a dedicated negative. Both are required by name,
//    each one's markers are scoped to its own `it` block, and the two counts are
//    published separately — a lifecycle and a refusal answer different
//    questions, and adding them would report two lifecycles where one exists.
//    The negative's adversarial assertion is MEASURED rather than assumed: the
//    `.rejects.toThrow` occurrences inside the block are counted, the refusal
//    message is required inside the block, and the module that RAISES that
//    message is resolved by sweeping the family's own on-chain and off-chain
//    modules plus the shared door. That sweep is what lets this gate publish
//    where the refusal comes from instead of implying the chain refused.
//
// 2. The delegation contract differs from Q10's and Q11's in one place, and its
//    reason differs from Q12's. The shared artifact's #545 depth remediation has
//    measured this family; its #612 spend-input tier-routing remediation has
//    NOT. Copying the siblings' "must appear in BOTH remediations" assertion
//    would produce a row that can never go green; dropping the axis silently
//    would hide the obligation if the axis ever grew. So membership is DERIVED
//    from the axis's own scope list (`spendInputCardinalityBound.affectsGoalIds`)
//    and required to agree in both directions. Q12 could back that absence with
//    "this family opens no field at all"; this family DOES open the spend-inputs
//    field, so the source-side reason is narrower and is measured instead: the
//    finalizing step pins the opened field's AUTHENTICATED ITEM COUNT to zero,
//    so a proof that finalizes carries the one-byte empty preimage and the
//    axis's admissible-cardinality stress cannot reach it. The sweep publishes
//    every `spend_inputs*` and field-opening token in all four family-owned
//    Aiken modules, requires the count pin to be present, and requires the
//    axis's own cardinality surfaces to be absent from this family while still
//    present elsewhere in the tree.
//
// 3. The selector census carries the step/module-helper scope split its Q12
//    sibling introduced, but the scope is DERIVED here rather than declared: for
//    every selector this gate resolves whether the declaration reaches
//    `main.spend`, directly or through a module-local helper that does, and
//    requires the recorded scope to agree. A label is not a measurement, and
//    output 4's per-step requirement is stated on the step-scoped positives.
//
// 4. The nested codec this family's redeemer carries — `FieldOpeningV1` — is
//    tracked on BOTH sides, so its constructor order is bound POSITIONALLY
//    rather than as a set. Q12's enum could only be set-compared because its
//    Aiken side lives in a fetched third-party package; this one does not, and a
//    wire-order enum compared as a set would accept a swap of Constr 0 and 1.
//
// Outputs 5 to 8 are bound family-locally in the SHARED four-family artifact
// docs/exec-plans/evidence/canonical-v1-proof-family-q1x-v1.json, whose own gate
// demo/scripts/verify-canonical-v1-proof-family-q1x.mjs is the executable
// authority for those cells and runs in the same battery. That gate spawns five
// Vitest suites, three of them emulator suites driving the complete correction
// path at forced branch level 22 and at the admissible 296-input Cardano spend
// shape. Re-running them from here would double the battery's cost and measure
// nothing the shared gate does not already measure, so this gate DELEGATES their
// passage and asserts the delegation instead. If the shared artifact's Q14 cells
// are ever re-opened, this gate fails closed rather than keeping four rows.
//
// usage: node demo/scripts/verify-canonical-v1-proof-family-q14.mjs [--json]

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

const GOAL_ID = "Q14";
const FAMILY = "zero-input";
const OUTPUTS = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const DELEGATED_OUTPUTS = [5, 6, 7, 8];
const ARTIFACT =
  "docs/exec-plans/evidence/canonical-v1-proof-family-q14-v1.json";
const SHARED_ARTIFACT =
  "docs/exec-plans/evidence/canonical-v1-proof-family-q1x-v1.json";
const SHARED_GATE = "demo/scripts/verify-canonical-v1-proof-family-q1x.mjs";
const AIKEN_PROJECT_DIRECTORY = "onchain/aiken";
const aikenProjectRoot = resolve(repositoryRoot, AIKEN_PROJECT_DIRECTORY);

// The spec sections output 3 parses rather than quotes.
const COMMITTED_FIELD_TABLE_HEADING = "### 2.5 The nine committed fields";
const COMMITTED_FIELD_TABLE_END = "\n## 3.";
const BODY_FIELD_TABLE_HEADING = "### 2.1 `NativeTxBodyCompact`";
const BODY_FIELD_TABLE_END = "\n### 2.2 ";
const IDENTITY_SECTION_HEADING =
  "## 3. Transaction identity (unchanged derivation)";
const ITEM_COUNT_SECTION_HEADING = "### 5.2 Item count";
const COMMITTED_FIELD_COUNT = 9;

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
  "midgard.canonical-v1-proof-family-q14.v1",
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
  `ERR_Q14_WRONG_TEST_COMPILER: ${aikenBinaryPath} reports "${measuredCompiler}" but this family must execute under exactly "${PINNED_FORK_COMPILER}"; set MIDGARD_AIKEN_BIN (or MIDGARD_FORK_AIKEN_BIN) to the patched fork`,
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

// A spec table cell that must be exactly one backticked token, unwrapped.
const backtickedCell = ({ cell, where }) => {
  assert.ok(
    /^`[^`]+`$/u.test(cell),
    `${where}: ${JSON.stringify(cell)} is not the single backticked token this table column is parsed as`,
  );
  return cell.slice(1, -1);
};

// A commented-out call is not a call. Every structural read of Aiken source in
// this gate runs over the comment-stripped text, so a marker, a handler reach or
// a token sweep can never be satisfied by prose.
const withoutAikenComments = (source) =>
  source
    .split(/\r?\n/u)
    .filter((line) => !line.trim().startsWith("//"))
    .join("\n");

// Declarations only: which titles each suite is claimed to contain. Nothing here
// decides whether any of them passed; the runner does that further down. The
// declaration order is the published-command order, so output 2's suites are
// declared before output 3's, output 6's re-check and output 9's.
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
// Output 1 — normative rule and violation identifiers
// ---------------------------------------------------------------------------

const rule = evidence.output1RuleAndViolationIdentifier;
assert.ok(
  typeof rule.rule === "string" && rule.rule.length > 0,
  "output 1 must state the rule, not only its identifiers",
);

// The identifier's declaration is DERIVED from the recorded key and code string
// rather than quoted, so a renamed key or a re-lettered code fails here instead
// of being carried by a stale literal.
const identifierSource = await readRepositoryFile(
  rule.identifierDeclarationFile,
);
assert.ok(
  Array.isArray(rule.localEnforcement) && rule.localEnforcement.length > 0,
  "output 1 must measure at least one local enforcement site, or the rule has no violation identifier to bind",
);
let measuredEnforcementSites = 0;
const measuredIdentifiers = [];
const enforcementFiles = new Set();
const enforcementOpeners = new Set();
for (const enforcement of rule.localEnforcement) {
  const where = `${enforcement.identifier} (${enforcement.file})`;
  assert.ok(
    identifierSource.includes(
      `${enforcement.identifierKey}: ${JSON.stringify(enforcement.identifier)},`,
    ),
    `${rule.identifierDeclarationFile} no longer declares ${enforcement.identifierKey} as ${enforcement.identifier}`,
  );
  assert.equal(
    enforcement.call,
    `RejectCodes.${enforcement.identifierKey}`,
    `${where}: the counted token is not the RejectCodes member of the declared key`,
  );
  const source = await readRepositoryFile(enforcement.file);
  const sourceLines = source.split(/\r?\n/u);
  const measuredLines = sourceLines.flatMap((line, index) =>
    line.includes(enforcement.call) ? [index + 1] : [],
  );
  // Every occurrence of the token in the file is an enforcement site, so the
  // published number is the enforcement count rather than a count of mentions —
  // and a new site added anywhere in the file fails this gate until it is
  // recorded.
  assert.deepEqual(
    measuredLines,
    enforcement.measuredSites.map((site) => site.line),
    `${where}: enforcement sites for ${enforcement.call} drifted from the recorded lines`,
  );
  assert.equal(
    enforcement.measuredSiteCount,
    measuredLines.length,
    `${where}: the published site count is not the number of sites in source`,
  );
  assert.ok(
    measuredLines.length > 0,
    `${where}: a rule with no local enforcement site has no violation identifier to bind`,
  );
  // The token alone would also match a comparison or a log line, so each site's
  // rejection opener is required at or just above it. This family's enforcement
  // call is a SINGLE line, so the opener shares the token's line and the check is
  // that the line BEGINS the rejection call — requiring a multi-line shape the
  // source does not have would be a gate tuned to a sibling's formatting.
  for (const site of enforcement.measuredSites) {
    assert.ok(
      site.openerLine <= site.line && site.line - site.openerLine <= 3,
      `${where}: the recorded opener line ${String(site.openerLine)} is not within the call that carries the token at line ${String(site.line)}`,
    );
    assert.ok(
      sourceLines[site.openerLine - 1]
        .trim()
        .startsWith(enforcement.callOpener),
      `${where}: line ${String(site.openerLine)} does not open the ${enforcement.callOpener} that makes line ${String(site.line)} an enforcement site`,
    );
  }
  if (enforcement.enclosingDeclaration !== undefined) {
    assert.ok(
      source.includes(enforcement.enclosingDeclaration),
      `${where}: ${enforcement.file} no longer declares ${enforcement.enclosingDeclaration}`,
    );
  }
  measuredEnforcementSites += measuredLines.length;
  measuredIdentifiers.push(enforcement.identifier);
  enforcementFiles.add(enforcement.file);
  enforcementOpeners.add(enforcement.callOpener);
}
assert.equal(
  rule.measuredEnforcementSiteCount,
  measuredEnforcementSites,
  "the published total enforcement site count is not the sum of the measured sites",
);
assert.deepEqual(
  rule.violationIdentifiers,
  measuredIdentifiers,
  "the published identifier list is not the set of identifiers whose enforcement was measured",
);

// The other side of the enforcement-versus-mention discipline. This family's
// identifier appears once more in the validation package, in a cross-check
// comparison rather than a rejection, and that occurrence is measured to be
// exactly that: it lives outside every counted file, it carries the token, and
// its line does not open a rejection call. Without this, "the count is an
// enforcement count" would rest only on the openers of the sites that WERE
// counted.
assert.ok(
  Array.isArray(rule.measuredNonEnforcementMentions),
  "output 1 must publish the non-enforcement mentions it measured, even when the list is empty",
);
for (const mention of rule.measuredNonEnforcementMentions) {
  assert.ok(
    !enforcementFiles.has(mention.file),
    `${mention.file} is a counted enforcement file, so a mention in it cannot be published as a non-enforcement occurrence`,
  );
  const mentionLines = (await readRepositoryFile(mention.file)).split(/\r?\n/u);
  assert.equal(
    mentionLines[mention.line - 1].trim(),
    mention.text,
    `${mention.file} line ${String(mention.line)} is not the non-enforcement mention this artifact records`,
  );
  assert.ok(
    mention.text.includes(mention.call),
    `${mention.file} line ${String(mention.line)} does not carry ${mention.call}, so it is not a mention of this rule's identifier at all`,
  );
  for (const opener of enforcementOpeners) {
    assert.ok(
      !mention.text.startsWith(opener),
      `${mention.file} line ${String(mention.line)} opens ${opener}: it is an enforcement site and may not be published as a mere mention`,
    );
  }
}

// The deployed identifier side: the append-only id is the positional index
// encoded over FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT bytes, so it is re-derived
// from the order constant rather than read from a constant. Inserting a category
// rather than appending one silently re-numbers every family after it, and this
// family sits at index 5 with five earlier insertion points ahead of it.
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
// exclusion is measured rather than asserted: a zero-input lifecycle added there
// must be cited, so its appearance fails this gate until it is.
const notCited = rule.registeredFamiliesSuiteNotCited;
const registeredFamiliesSource = await readRepositoryFile(notCited.file);
assert.ok(
  !registeredFamiliesSource.includes(catalogueBinding.category),
  `${notCited.file} now mentions ${catalogueBinding.category}; if it carries a zero-input lifecycle it must be cited and run rather than excluded`,
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
// type-checks on both sides of the boundary and mis-binds silently. This
// family's own args record opens with a same-typed pair.
const recordFieldNames = (block, indent) =>
  [
    ...block.matchAll(
      new RegExp(`^ {${String(indent)}}([a-z_][a-z0-9_]*):`, "gmu"),
    ),
  ].map((match) => match[1]);

const aikenRecordFields = async ({ module, type }) => {
  const source = await readRepositoryFile(module);
  const header = `pub type ${type} {`;
  const start = source.indexOf(header);
  assert.ok(start >= 0, `${module} no longer declares ${type}`);
  const body = source.slice(start + header.length);
  const end = body.indexOf("\n}");
  assert.ok(end > 0, `${module} declaration of ${type} is unterminated`);
  return recordFieldNames(body.slice(0, end), 2);
};

const typescriptSchemaFields = async ({ module, schema }) => {
  const source = await readRepositoryFile(module);
  const header = `${schema} = Data.Object({`;
  const start = source.indexOf(header);
  assert.ok(start >= 0, `${module} no longer declares ${schema}`);
  const body = source.slice(start + header.length);
  const end = body.indexOf("});");
  assert.ok(end > 0, `${module} declaration of ${schema} is unterminated`);
  return recordFieldNames(body.slice(0, end), 2);
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
// not edited, by this family — which is why the alias bindings that connect it to
// the family are measured below rather than assumed.
await assertRecordParity(
  codec.sharedInclusionRecord,
  "output 2 shared inclusion record",
);

// A re-export is what makes the shared record THIS family's record. If any alias
// is rewritten into a family-local re-declaration, the parity above stops binding
// here and this gate says so.
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

// The family's own records. This family owns exactly one step `Args`, so the
// inter-step `State` that actually crosses its step boundary is measured with the
// same machinery: stopping at args would leave the family's distinctive datum —
// the section-2.5 anchor the terminal step re-opens the field against —
// unmeasured.
assert.ok(
  Array.isArray(codec.familyArgsRecords) && codec.familyArgsRecords.length > 0,
  "output 2 must measure the family's own step args records, not only the shared inclusion record",
);
assert.equal(
  codec.measuredFamilyArgsRecordCount,
  codec.familyArgsRecords.length,
  "the published family args record count is not the length of the measured list",
);
assert.ok(
  Array.isArray(codec.familyStateRecords) &&
    codec.familyStateRecords.length > 0,
  "output 2 must measure the inter-step state record this family's steps exchange",
);
assert.equal(
  codec.measuredFamilyStateRecordCount,
  codec.familyStateRecords.length,
  "the published family state record count is not the length of the measured list",
);
for (const record of [
  ...codec.familyArgsRecords,
  ...codec.familyStateRecords,
]) {
  assert.ok(
    typeof record.step === "string" && record.step.length > 0,
    "each family record must name the step it belongs to",
  );
  await assertRecordParity(
    record,
    `output 2 ${record.step} ${record.aikenType}`,
  );
}

// One field of the args record is not a scalar but the section-8 carriage enum,
// so field-name parity alone would not bind it: the constructors and their WIRE
// ORDER have to agree, and each arm's own record has to agree field-for-field.
// Both sides of this enum are tracked here, so the comparison is positional
// rather than set-based — a set comparison would accept a swap of Constr 0 and
// Constr 1, which is precisely the mis-binding a redeemer enum can suffer.
const opening = codec.fieldOpeningEnum;
const aikenEnumArms = async ({ module, type }) => {
  const source = withoutAikenComments(await readRepositoryFile(module));
  const header = `pub type ${type} {`;
  const start = source.indexOf(header);
  assert.ok(start >= 0, `${module} no longer declares ${type}`);
  const body = source.slice(start + header.length);
  const end = body.indexOf("\n}");
  assert.ok(end > 0, `${module} declaration of ${type} is unterminated`);
  const block = body.slice(0, end);
  return [
    ...block.matchAll(
      /^ {2}([A-Z][A-Za-z0-9]*) \{\n((?: {4}[^\n]*\n)*) {2}\}/gmu,
    ),
  ].map((match) => ({
    constructor: match[1],
    fields: recordFieldNames(match[2], 4),
  }));
};
const aikenOpeningArms = await aikenEnumArms({
  module: opening.aikenModule,
  type: opening.aikenType,
});
const openingSource = await readRepositoryFile(opening.typescriptModule);
const openingHeader = `${opening.typescriptSchema} = Data.Enum([`;
const openingStart = openingSource.indexOf(openingHeader);
assert.ok(
  openingStart >= 0,
  `${opening.typescriptModule} no longer declares ${opening.typescriptSchema} as a Data.Enum`,
);
const openingBody = openingSource.slice(openingStart + openingHeader.length);
const openingBlock = openingBody.slice(0, openingBody.indexOf("]);"));
assert.ok(
  openingBlock.length > 0,
  `${opening.typescriptModule} declaration of ${opening.typescriptSchema} is unterminated`,
);
const typescriptOpeningArms = [
  ...openingBlock.matchAll(
    /Data\.Object\(\{\s*([A-Z][A-Za-z0-9]*):\s*([A-Za-z0-9]+Schema)\s*\}\)/gu,
  ),
].map((match) => ({ constructor: match[1], schema: match[2] }));
assert.deepEqual(
  typescriptOpeningArms.map((arm) => arm.constructor),
  aikenOpeningArms.map((arm) => arm.constructor),
  `${opening.aikenModule} and ${opening.typescriptModule} disagree about the constructors of ${opening.aikenType} or about their order; the order is wire format, so a swap re-binds Constr 0 to the other arm`,
);
assert.deepEqual(
  aikenOpeningArms.map((arm) => arm.constructor),
  opening.constructors.map((arm) => arm.constructor),
  "the published carriage-enum constructor order is not the one the two sources declare",
);
assert.equal(
  opening.measuredConstructorCount,
  aikenOpeningArms.length,
  "the published carriage-enum constructor count is not the number the sources declare",
);
for (const [position, arm] of opening.constructors.entries()) {
  assert.equal(
    arm.wireIndex,
    position,
    `${arm.constructor} is published at wire index ${String(arm.wireIndex)} but is declared at position ${String(position)}`,
  );
  assert.deepEqual(
    aikenOpeningArms[position].fields,
    arm.fields,
    `${arm.constructor}: the published field list is not the one ${opening.aikenModule} declares`,
  );
  assert.equal(
    typescriptOpeningArms[position].schema,
    arm.typescriptSchema,
    `${arm.constructor}: the SDK enum arm is not built from ${arm.typescriptSchema}`,
  );
  const armFields = await typescriptSchemaFields({
    module: opening.typescriptModule,
    schema: arm.typescriptSchema,
  });
  assert.deepEqual(
    armFields,
    arm.fields,
    `${arm.constructor}: ${arm.typescriptSchema} is not field-for-field identical to the Aiken arm, in the same order`,
  );
}
// Which arm this family uses is not a preference: fields 0 to 5 are the body's,
// and a step that reads a body field must not be handed a witness set to ignore.
const usedArm = opening.constructors.find(
  (arm) => arm.constructor === opening.familyArm,
);
assert.ok(
  usedArm !== undefined,
  `the arm this family uses (${String(opening.familyArm)}) is not one of the measured constructors`,
);

assert.ok(
  Array.isArray(codec.suites) && codec.suites.length > 0,
  "output 2 must cite at least one executed suite; a parsed field list alone is not passage",
);
for (const suite of codec.suites) {
  declareVitest(suite.file, suite.requiredTitles);
  // Structural, backing no count: a cited suite must really drive THIS family.
  // Titles that belong to a sibling's coverage are deliberately not cited, so
  // something has to establish that the cited ones are not a sibling's either.
  for (const marker of suite.suiteFamilyMarkers ?? []) {
    const suiteSource = await readRepositoryFile(suite.file);
    assert.ok(
      suiteSource.includes(marker),
      `${suite.file} no longer contains ${marker}, so it can no longer be cited as a suite that drives this family`,
    );
  }
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
// This family's verdict is the field's ITEM COUNT, and the count exists in
// exactly one place under the flat scheme, so the section that says so is not
// optional for it.
assert.ok(
  binding.authoritySections.includes(ITEM_COUNT_SECTION_HEADING),
  `a family whose rule is decided from an authenticated item count must anchor ${ITEM_COUNT_SECTION_HEADING}`,
);
assert.ok(
  binding.authoritySections.includes(IDENTITY_SECTION_HEADING),
  `the anchor the door opens against is the transaction id, so ${IDENTITY_SECTION_HEADING} must be among the anchored sections`,
);

// The whole committed-field inventory is parsed out of the spec's own table
// rather than a single row of it: the field this family opens is identified
// POSITIONALLY, so the positions of all nine are what make index 0 mean what the
// artifact says it means. A re-numbered field or a renamed commitment slot fails
// here.
const fieldTableStart = specSource.indexOf(COMMITTED_FIELD_TABLE_HEADING);
assert.ok(
  fieldTableStart > 0,
  `${binding.authority} no longer carries the committed-field table`,
);
const parsedCommittedFields = specSource
  .slice(
    fieldTableStart,
    specSource.indexOf(COMMITTED_FIELD_TABLE_END, fieldTableStart),
  )
  .split(/\r?\n/u)
  .map((line) => tableCells(line))
  .filter((cells) => cells.length > 3 && /^\d+$/u.test(cells[1]))
  .map((cells) => ({
    index: Number(cells[1]),
    field: cells[2],
    commitmentSlot: backtickedCell({
      cell: cells[3],
      where: `${binding.authority} ${COMMITTED_FIELD_TABLE_HEADING}`,
    }),
  }));
assert.equal(
  parsedCommittedFields.length,
  COMMITTED_FIELD_COUNT,
  `${binding.authority} committed-field table no longer has exactly ${String(COMMITTED_FIELD_COUNT)} rows`,
);
assert.deepEqual(
  parsedCommittedFields.map((field) => field.index),
  parsedCommittedFields.map((_field, position) => position),
  `${binding.authority} committed-field indices are not the fixed positional 0..${String(COMMITTED_FIELD_COUNT - 1)} the spec calls normative`,
);
assert.deepEqual(
  parsedCommittedFields,
  binding.committedFieldInventory,
  "the published committed-field inventory is not the one the spec's own table declares",
);

// The field this family opens, bound positionally against that inventory.
assert.ok(
  Array.isArray(binding.openedCommittedFields),
  "output 3 must publish the list of committed fields this family opens, even when it is empty",
);
assert.equal(
  binding.opensNoCommittedField,
  binding.openedCommittedFields.length === 0,
  "opensNoCommittedField disagrees with the published list of opened fields",
);
assert.equal(
  binding.opensNoCommittedField,
  false,
  "this family opens a committed field; publishing it as opening none would delete the binding output 3 rests on",
);
const openedIndices = [];
for (const opened of binding.openedCommittedFields) {
  const row = parsedCommittedFields.find(
    (field) => field.index === opened.index,
  );
  assert.ok(
    row !== undefined,
    `output 3 claims to open committed field ${String(opened.index)}, which the spec's table does not declare`,
  );
  assert.equal(
    row.field,
    opened.field,
    `field ${String(opened.index)} is not ${opened.field} in the spec's own table`,
  );
  assert.equal(
    row.commitmentSlot,
    opened.commitmentSlot,
    `field ${String(opened.index)} does not commit through ${opened.commitmentSlot} in the spec's own table`,
  );
  openedIndices.push(opened.index);
}
assert.equal(
  openedIndices.length,
  1,
  "this family opens exactly one committed field; a second opening changes the carriage and cardinality disposition and must be re-decided deliberately",
);
const boundFieldIndex = openedIndices[0];

// The commitment slot names a position of the compact body, so the body's own
// twelve-row table is parsed too and the slot is required to sit where the
// artifact says. This is the join the two spec tables make: field 0 of the
// section-2.5 inventory commits through position 0 of the section-2.1 body.
const bodyTableStart = specSource.indexOf(BODY_FIELD_TABLE_HEADING);
assert.ok(
  bodyTableStart > 0,
  `${binding.authority} no longer carries the compact-body field table`,
);
const parsedBodyFields = specSource
  .slice(
    bodyTableStart,
    specSource.indexOf(BODY_FIELD_TABLE_END, bodyTableStart),
  )
  .split(/\r?\n/u)
  .map((line) => tableCells(line))
  .filter((cells) => cells.length > 3 && /^\d+$/u.test(cells[1]))
  .map((cells) => ({
    position: Number(cells[1]),
    field: backtickedCell({
      cell: cells[2],
      where: `${binding.authority} ${BODY_FIELD_TABLE_HEADING}`,
    }),
    type: cells[3],
  }));
assert.equal(
  parsedBodyFields.length,
  binding.bodyFieldCount,
  `${binding.authority} compact-body table no longer has the published number of fields`,
);
assert.deepEqual(
  parsedBodyFields.map((field) => field.position),
  parsedBodyFields.map((_field, position) => position),
  `${binding.authority} compact-body field positions are not the declaration and wire order the spec states`,
);
const boundSlot = binding.boundBodySlot;
const bodyRow = parsedBodyFields.find(
  (field) => field.position === boundSlot.position,
);
assert.ok(
  bodyRow !== undefined,
  `${binding.authority} compact-body table has no row at position ${String(boundSlot.position)}`,
);
assert.equal(
  bodyRow.field,
  boundSlot.field,
  `position ${String(boundSlot.position)} of the compact body is not ${boundSlot.field} in the spec's own table`,
);
assert.equal(
  bodyRow.type,
  boundSlot.type,
  `position ${String(boundSlot.position)} of the compact body is not typed ${boundSlot.type} in the spec's own table`,
);
// The two tables must agree, and the agreement is derived rather than asserted:
// the commitment slot of the opened field is `body.<the body row's own name>`.
assert.equal(
  binding.openedCommittedFields[0].commitmentSlot,
  `body.${bodyRow.field}`,
  "the opened field's commitment slot is not the body position this artifact binds it to; the two spec tables would then be describing different slots",
);
assert.equal(
  boundSlot.position,
  boundFieldIndex,
  "for fields 0 to 5 the committed-field index and the compact-body position are the same number; publishing different ones would hide a re-numbering",
);

// The positional identity, bound in all three places it exists. Section 4
// removed field-index domain separation, so the empty preimage of every field
// hashes to the same value and the INDEX is the only thing that makes this an
// opening of field 0 rather than of some other empty field. That makes the
// on-chain constant and its off-chain twin part of the binding, not trivia.
const indexConstants = binding.fieldIndexConstants;
const aikenIndexSource = await readRepositoryFile(indexConstants.aiken.module);
assert.ok(
  aikenIndexSource.includes(indexConstants.aiken.declaration),
  `${indexConstants.aiken.module} no longer declares ${indexConstants.aiken.declaration}`,
);
assert.equal(
  indexConstants.aiken.declaration,
  `pub const ${indexConstants.aiken.constant}: Int = ${String(boundFieldIndex)}`,
  "the recorded Aiken field-index declaration is not the constant this family's opened field index requires",
);
const indexTableSource = await readRepositoryFile(
  indexConstants.typescript.module,
);
const indexTableHeader = `${indexConstants.typescript.table} = Object.freeze({`;
const indexTableStart = indexTableSource.indexOf(indexTableHeader);
assert.ok(
  indexTableStart >= 0,
  `${indexConstants.typescript.module} no longer declares ${indexConstants.typescript.table}`,
);
const indexTableBody = indexTableSource.slice(
  indexTableStart + indexTableHeader.length,
);
const parsedIndexEntries = [
  ...indexTableBody
    .slice(0, indexTableBody.indexOf("});"))
    .matchAll(/^ {2}([A-Za-z][A-Za-z0-9]*): (\d+),$/gmu),
].map((match) => ({ name: match[1], value: Number(match[2]) }));
assert.equal(
  parsedIndexEntries.length,
  COMMITTED_FIELD_COUNT,
  `${indexConstants.typescript.table} no longer names exactly ${String(COMMITTED_FIELD_COUNT)} fields, so it is not the off-chain twin of the section-2.5 table`,
);
assert.deepEqual(
  parsedIndexEntries.map((entry) => entry.value),
  parsedIndexEntries.map((_entry, position) => position),
  `${indexConstants.typescript.table} is not the positional table the spec calls normative; a named index that is not its own position would let a builder open one field while naming another`,
);
assert.deepEqual(
  parsedIndexEntries,
  indexConstants.typescript.entries,
  `the published ${indexConstants.typescript.table} entries are not the ones the module declares`,
);
const boundIndexEntry = parsedIndexEntries.find(
  (entry) => entry.name === indexConstants.typescript.boundEntry,
);
assert.ok(
  boundIndexEntry !== undefined,
  `${indexConstants.typescript.table} has no ${String(indexConstants.typescript.boundEntry)} entry`,
);
assert.equal(
  boundIndexEntry.value,
  boundFieldIndex,
  `${indexConstants.typescript.boundEntry} is not index ${String(boundFieldIndex)} off-chain, so the two sides open different fields`,
);
// And the arm the enum measurement recorded is derived from that index rather
// than trusted: fields below the witness-set boundary are the body's.
const witnessBoundaryEntry = parsedIndexEntries.find(
  (entry) => entry.name === indexConstants.typescript.witnessBoundaryEntry,
);
assert.ok(
  witnessBoundaryEntry !== undefined,
  `${indexConstants.typescript.table} has no ${String(indexConstants.typescript.witnessBoundaryEntry)} entry to derive the witness-set boundary from`,
);
assert.equal(
  indexConstants.typescript.firstWitnessSetFieldIndex,
  witnessBoundaryEntry.value,
  "the published witness-set boundary is not the index the table gives its first witness-set field",
);
assert.ok(
  boundFieldIndex < witnessBoundaryEntry.value,
  `field ${String(boundFieldIndex)} is at or past the witness-set boundary, so this family's opening cannot be the body arm this artifact records`,
);
assert.equal(
  opening.familyArm,
  opening.constructors[0].constructor,
  "a body-field opening must use the body arm, which is Constr 0 of the carriage enum",
);

// The verdict itself, measured in the validator that reaches it. The pinned
// assertion is what makes this family's rule an authenticated COUNT read rather
// than a hash comparison against the shared empty-field commitment — the
// distinction the module's own third selector exists to defend.
const countPin = binding.authenticatedCountAssertion;
const countPinSource = withoutAikenComments(
  await readRepositoryFile(countPin.module),
);
assert.ok(
  countPinSource.includes(countPin.assertion),
  `${countPin.module} no longer carries ${JSON.stringify(countPin.assertion)}, so this family's verdict is no longer the door's authenticated item count`,
);
assert.equal(
  countPin.requiredItemCount,
  0,
  "the zero-input rule finalizes only on an empty field; any other required count is a different rule",
);
assert.ok(
  countPin.assertion.includes(countPin.countFunction),
  "the recorded assertion does not call the count function it names",
);
assert.ok(
  countPin.assertion
    .trim()
    .endsWith(`== ${String(countPin.requiredItemCount)}`),
  "the recorded assertion does not pin the count to the required value",
);
for (const symbol of countPin.doorSymbols) {
  assert.ok(
    countPinSource.includes(symbol),
    `${countPin.module} no longer references ${symbol}, so the count it asserts on is no longer read through the section-8.8 door`,
  );
}

// The cited suites cover the code path this family's steps take. That is a
// structural claim about the builders, so it is read out of them: a step-02
// rewritten to construct its opening some other way, or a step-01 that stopped
// re-deriving the anchor, would leave the citations pointing at paths the family
// no longer walks.
const door = binding.builderDoorBinding;
const doorSource = await readRepositoryFile(door.module);
for (const symbol of [
  door.planner,
  door.constructor,
  door.redeemerField,
  door.fieldIndexSymbol,
  door.itemCountField,
]) {
  assert.ok(
    doorSource.includes(symbol),
    `${door.module} no longer references ${symbol}, so the cited field-opening suite is no longer the surface this family's binding step routes through`,
  );
}
assert.equal(
  door.fieldIndexSymbol,
  `${indexConstants.typescript.table}.${indexConstants.typescript.boundEntry}`,
  "the builder must name the positional index table rather than a bare integer, and the recorded symbol is not that name",
);
const anchor = binding.builderAnchorBinding;
const anchorSource = await readRepositoryFile(anchor.module);
for (const symbol of anchor.symbols) {
  assert.ok(
    anchorSource.includes(symbol),
    `${anchor.module} no longer references ${symbol}, so this family's first step no longer anchors the transaction the door is opened against`,
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
// decoration: a "valid-block negative" that is really an ordinary assertion would
// otherwise pass by name alone.
const aikenTestDeclarations = (source) =>
  [...source.matchAll(/^test\s+([a-z0-9_]+)\s*\(\)\s*(fail\b)?\s*\{/gmu)].map(
    (match) => ({
      selector: match[1],
      failsByDeclaration: match[2] !== undefined,
    }),
  );

// Top-level declarations of an Aiken module, sliced so each one's body can be
// searched independently. This is what turns the scope label into a measurement:
// a selector is step-scoped only if its own body reaches the module's
// `main.spend` handler, directly or through a module-local helper that does.
const HANDLER_CALL = "main.spend";
const aikenModuleReach = (source) => {
  const stripped = withoutAikenComments(source);
  const starts = [
    ...stripped.matchAll(
      /^(?:pub )?(test|fn|const|type|validator)\s+([A-Za-z][A-Za-z0-9_]*)/gmu,
    ),
  ].map((match) => ({
    keyword: match[1],
    name: match[2],
    index: match.index,
  }));
  const declarations = starts.map((declaration, position) => ({
    ...declaration,
    body: stripped.slice(
      declaration.index,
      position + 1 < starts.length ? starts[position + 1].index : undefined,
    ),
  }));
  const calls = (body, name) => new RegExp(`\\b${name}\\s*\\(`, "u").test(body);
  const helpers = declarations.filter(
    (declaration) => declaration.keyword === "fn",
  );
  const reaching = new Set(
    helpers
      .filter((helper) => helper.body.includes(HANDLER_CALL))
      .map((helper) => helper.name),
  );
  let grew = true;
  while (grew) {
    grew = false;
    for (const helper of helpers) {
      if (reaching.has(helper.name)) {
        continue;
      }
      for (const reached of reaching) {
        if (calls(helper.body, reached)) {
          reaching.add(helper.name);
          grew = true;
          break;
        }
      }
    }
  }
  const reachTokens = (body) => {
    const tokens = [];
    if (body.includes(HANDLER_CALL)) {
      tokens.push(HANDLER_CALL);
    }
    for (const helper of [...reaching].sort()) {
      if (calls(body, helper)) {
        tokens.push(helper);
      }
    }
    return tokens;
  };
  return {
    handlerReachingHelpers: [...reaching].sort(),
    tests: new Map(
      declarations
        .filter((declaration) => declaration.keyword === "test")
        .map((declaration) => [
          declaration.name,
          reachTokens(declaration.body),
        ]),
    ),
  };
};

const ROLE_MUST_FAIL = {
  positive: false,
  validBlockNegative: true,
  additionalNegative: true,
};
const SELECTOR_SCOPES = ["step", "moduleHelper"];

const declaredAiken = [];
let positiveSelectors = 0;
let validBlockNegativeSelectors = 0;
let additionalNegativeSelectors = 0;
let stepScopedSelectors = 0;
let moduleHelperSelectors = 0;

for (const module of onchain.modules) {
  const where = `${module.module} (${module.source})`;
  assert.equal(
    aikenModuleName(module.source),
    module.module,
    `${where} declares a module name that is not the one aiken derives from its path`,
  );
  const source = await readRepositoryFile(module.source);
  const declarations = aikenTestDeclarations(source);
  // The declared set must be EXACTLY the module's `test` declarations. A selector
  // added to one of these modules and left uncited would otherwise sit outside
  // every count this artifact publishes.
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
  const reach = aikenModuleReach(source);
  assert.deepEqual(
    reach.handlerReachingHelpers,
    module.measuredHandlerReachingHelpers,
    `${where}: the published set of module-local helpers that reach ${HANDLER_CALL} is not the one this module declares`,
  );
  let moduleStepScopedPositives = 0;
  let moduleValidBlockNegatives = 0;
  for (const {
    selector,
    role,
    scope,
    handlerReach,
    claim,
  } of module.selectors) {
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
    assert.ok(
      SELECTOR_SCOPES.includes(scope),
      `${where} selector ${selector} carries unknown scope ${String(scope)}`,
    );
    assert.equal(
      declarationBySelector.get(selector).failsByDeclaration,
      mustFail,
      `${where} selector ${selector} is declared as a ${declarationBySelector.get(selector).failsByDeclaration ? "failure" : "success"} test, which contradicts its recorded role ${role}`,
    );
    // The scope is DERIVED, not read: a selector is step-scoped exactly when its
    // own body reaches this module's `main.spend`, and the token by which it
    // reaches it is published and re-measured.
    const measuredReach = reach.tests.get(selector) ?? [];
    assert.deepEqual(
      measuredReach,
      handlerReach,
      `${where} selector ${selector}: the published route to ${HANDLER_CALL} is not the one its body takes`,
    );
    assert.equal(
      scope === "step",
      measuredReach.length > 0,
      `${where} selector ${selector} is recorded ${scope} but ${measuredReach.length > 0 ? "does" : "does not"} reach ${HANDLER_CALL}`,
    );
    // A module-helper selector calls a module-local function directly instead of
    // driving `main.spend`, so it can never be one of the family's negatives: a
    // `fail`-declared helper unit case would prove something about the helper,
    // not about a valid block the family must refuse to convict.
    if (scope === "moduleHelper") {
      assert.equal(
        role,
        "positive",
        `${where} selector ${selector} is a module-helper unit case and may not be counted as a ${role}; only a step-scoped selector drives the handler a negative has to be judged against`,
      );
      moduleHelperSelectors += 1;
    } else {
      stepScopedSelectors += 1;
    }
    if (role === "positive") {
      positiveSelectors += 1;
      if (scope === "step") {
        moduleStepScopedPositives += 1;
      }
    } else if (role === "validBlockNegative") {
      moduleValidBlockNegatives += 1;
      validBlockNegativeSelectors += 1;
    } else {
      additionalNegativeSelectors += 1;
    }
    declaredAiken.push({ module: module.module, selector });
  }
  assert.equal(
    module.stepScopedPositiveCount,
    moduleStepScopedPositives,
    `${where} publishes a step-scoped positive count that is not the number its own selector list carries`,
  );
  // GOAL_SPEC.md 9.1 output 4 asks for positive AND valid-block negative tests
  // for the family's proof STEPS, so the requirement is checked per step rather
  // than in aggregate — and on step-scoped positives rather than on positives, so
  // that a module-local unit case added later cannot satisfy it with the real
  // proof-step positives deleted.
  assert.ok(
    moduleStepScopedPositives > 0,
    `${where} has no step-scoped positive selector — one that drives its own main.spend handler — so output 4 is unmet for that step`,
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
assert.equal(
  stepScopedSelectors + moduleHelperSelectors,
  declaredAiken.length,
  "the scope split does not account for every declared selector exactly once",
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
// is the only status that carries a cell, and an artifact with any open cell left
// in it is not a settled authority for four of this family's rows.
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

// The carriage disposition. This family rides ONE of the shared artifact's two
// remediations, and which one is not a choice this gate hard-codes: the
// spend-input axis publishes its own scope, so membership in its remediation is
// derived from that scope and required to agree in BOTH directions. Demanding
// this family in the tier-routed list would be a row that can never go green;
// skipping the axis would hide the obligation if the axis ever grew to cover it.
assert.deepEqual(
  assertedCells.requiredCarriageMembership,
  ["chunkedProofCarriage.measuredFamilies"],
  "the depth remediation is the one that binds this family and must be required by name",
);
assert.deepEqual(
  assertedCells.requiredCarriageNonMembership,
  ["spendInputTierRoutedCarriage.measuredFamilies"],
  "the spend-input tier-routing remediation does not reach this family and its ABSENCE must be asserted rather than left unmentioned",
);
assert.ok(
  shared.chunkedProofCarriage.measuredFamilies.includes(GOAL_ID),
  `${GOAL_ID} is not in the shared artifact's chunkedProofCarriage.measuredFamilies, so the membership-proof depth axis is not remediated for this family`,
);
assert.equal(
  assertedCells.spendInputAxisScopeKey,
  "spendInputCardinalityBound.affectsGoalIds",
  "the axis disposition must be derived from the axis's own scope list, not from a constant in this artifact",
);
const spendInputAxisScope = shared.spendInputCardinalityBound.affectsGoalIds;
assert.deepEqual(
  spendInputAxisScope,
  assertedCells.measuredSpendInputAxisScope,
  "the recorded scope of the spend-input cardinality axis is not the one the shared artifact publishes",
);
const spendInputAxisApplies = spendInputAxisScope.includes(GOAL_ID);
assert.equal(
  assertedCells.spendInputAxisApplies,
  spendInputAxisApplies,
  "the recorded axis disposition is not the one derived from the shared artifact's scope list",
);
assert.equal(
  shared.spendInputTierRoutedCarriage.measuredFamilies.includes(GOAL_ID),
  spendInputAxisApplies,
  spendInputAxisApplies
    ? `the spend-input cardinality axis now covers ${GOAL_ID} but the #612 tier-routed remediation has not measured it, so this family's output-5 row may not stand on the depth remediation alone`
    : `the #612 tier-routed remediation now measures ${GOAL_ID} while the cardinality axis does not claim to cover it; the axis disposition recorded here must be re-decided deliberately rather than left recording an absence`,
);
assert.equal(
  Object.hasOwn(shared.spendInputTierRoutedCarriage.bindingStages, GOAL_ID),
  spendInputAxisApplies,
  "the tier-routed remediation's binding-stage list disagrees with the axis scope about this family",
);
assert.ok(
  typeof assertedCells.carriageDispositionNote === "string" &&
    assertedCells.carriageDispositionNote.length > 0,
  "a family that rides one remediation and not the other must say which, and why",
);

// The source-side half of the same claim, and the place this family's contract
// departs from its Q12 sibling's. Q12 could back the axis's absence with "no step
// of this family opens a field at all"; this family DOES open one, so the absence
// rests on something narrower and it is measured rather than asserted: the
// finalizing step pins the opened field's authenticated item count to zero, so a
// proof that finalizes carries the one-byte empty preimage and cannot grow with
// spend-input cardinality. The sweep publishes every field-opening and
// `spend_inputs*` token in all four family-owned Aiken modules, so a new opening
// or a relaxed count pin lands here rather than passing unseen.
const FIELD_OPENING_TOKENS = [
  "FieldOpening",
  "field_opening",
  "_opening",
  "inputs_preimage",
];
const sweep = delegation.spendInputAxisSourceSweep;
const familyOwnedAikenModules = [
  ...new Set(
    [
      ...onchain.modules.map((module) => module.source),
      ...codec.familyArgsRecords.map((record) => record.aikenModule),
      ...codec.familyStateRecords.map((record) => record.aikenModule),
      ...codec.aliasBindings.map((alias) => alias.file),
    ].filter((path) => path.endsWith(".ak") && path.includes(`/${FAMILY}/`)),
  ),
].sort();
// The two validator modules and the two library modules of a two-step family.
assert.ok(
  familyOwnedAikenModules.length >= 2 * onchain.modules.length,
  "the family-owned Aiken module sweep did not resolve both the validator and the library module of every step, so its result would be weaker than it reads",
);
assert.equal(
  sweep.measuredModuleCount,
  familyOwnedAikenModules.length,
  "the published sweep module count is not the number of family-owned Aiken modules resolved from this artifact's own citations",
);
assert.equal(
  sweep.minimumModuleCount,
  2 * onchain.modules.length,
  "the sweep's own floor must be the validator and library module of every step",
);
const measuredSweep = [];
for (const modulePath of familyOwnedAikenModules) {
  const moduleSource = withoutAikenComments(
    await readRepositoryFile(modulePath),
  );
  measuredSweep.push({
    module: modulePath,
    fieldOpeningTokens: FIELD_OPENING_TOKENS.map((token) => ({
      token,
      count: moduleSource.split(token).length - 1,
    })).filter((entry) => entry.count > 0),
    spendInputTokens: [
      ...[...moduleSource.matchAll(/spend_inputs[a-z_]*/gu)]
        .map((match) => match[0])
        .reduce(
          (counts, token) => counts.set(token, (counts.get(token) ?? 0) + 1),
          new Map(),
        )
        .entries(),
    ]
      .map(([token, count]) => ({ token, count }))
      .sort((left, right) => left.token.localeCompare(right.token)),
  });
}
assert.deepEqual(
  measuredSweep,
  sweep.modules,
  "the published per-module token sweep is not the one these four modules carry",
);
// Exactly one module may hold the door, and it must be the finalizing validator
// that carries the count pin — an opening in the first step would put a preimage
// on the carriage of a step the axis does reach.
const measuredOpeningModules = measuredSweep
  .filter((module) =>
    module.spendInputTokens.some(
      (entry) => entry.token === countPin.viewConstructorToken,
    ),
  )
  .map((module) => module.module);
assert.deepEqual(
  measuredOpeningModules,
  sweep.openingModules,
  "the published set of modules that open the spend-inputs field is not the measured one",
);
assert.deepEqual(
  measuredOpeningModules,
  [countPin.module],
  "the module that opens the spend-inputs field is not the one output 3 measured the count pin in",
);
if (!spendInputAxisApplies) {
  // The axis's own cardinality surfaces. A family the axis does not reach must
  // not name them, and the tokens must still exist where the shared artifact says
  // they do — absent-and-gone-everywhere would make this check vacuous.
  for (const surface of sweep.axisSurfaceTokens) {
    for (const module of familyOwnedAikenModules) {
      const moduleSource = await readRepositoryFile(module);
      assert.ok(
        !moduleSource.includes(surface.token),
        `${module} now names ${surface.token}: this family is recorded outside the spend-input cardinality axis, so a cardinality surface here re-opens the axis and the delegation must be re-decided`,
      );
    }
    const elsewhere = await readRepositoryFile(surface.presentAt);
    assert.ok(
      elsewhere.includes(surface.token),
      `${surface.token} no longer appears in ${surface.presentAt}, so recording this family as free of it is no longer a measured comparison`,
    );
  }
  assert.ok(
    sweep.axisSurfaceTokens.length > 0,
    "the axis-absence claim must name at least one cardinality surface it measured absent, or it rests on nothing",
  );
}

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
  contractsSource.includes(`${output6.chainBuilder} = `),
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
// And the applied PARAMETER ORDER is bound by a passing test rather than by a
// source read, on a suite this gate already runs: a re-ordered parameter list
// still reads correctly in source and still compiles.
declareVitest(output6.suite, output6.requiredTitles);
assert.ok(
  output6.requiredTitles.length > 0,
  "the deployed first-step identity must be bound by at least one executed title, not by a source read alone",
);
// Applied script hashes are deliberately unpinned here: blueprint regeneration is
// parent-only (IG1) and re-pins them, so a family-local copy would become a
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
// Output 9 — emulator coverage, measured locally
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
  "the coverage titles measured here disagree with the shared artifact's for this family",
);
assert.ok(
  lifecycle.requiredTitles.includes(lifecycle.measuredLifecycleTitle),
  "the named measured lifecycle is not among the titles this gate requires",
);
assert.equal(
  lifecycle.measuredLifecycleTitle,
  sharedFamily.emulator.measuredLifecycleTitle,
  "the two artifacts disagree about which title is this family's end-to-end lifecycle",
);
assert.equal(
  lifecycle.hasValidBlockNegative,
  sharedFamily.emulator.hasValidBlockNegative,
  "the two artifacts disagree about whether this family has an emulator valid-block negative",
);
// A missing emulator valid-block negative is a gap, not a silence: it must be
// justified here and owned in the residual findings below. This family HAS one,
// so the requirement runs the other way — the title must be named, must be
// required, and must be the one the shared artifact records.
if (lifecycle.hasValidBlockNegative) {
  assert.equal(
    lifecycle.validBlockNegativeTitle,
    sharedFamily.emulator.validBlockNegativeTitle,
    "the two artifacts disagree about which title is this family's emulator valid-block negative",
  );
  assert.ok(
    lifecycle.requiredTitles.includes(lifecycle.validBlockNegativeTitle),
    "the emulator valid-block negative must be among the titles this gate requires by name, or it is a citation rather than a measurement",
  );
  assert.ok(
    typeof lifecycle.validBlockNegativeScope === "string" &&
      lifecycle.validBlockNegativeScope.length > 0,
    "the emulator valid-block negative must state what it does and does not cover",
  );
} else {
  assert.ok(
    typeof lifecycle.validBlockNegativeNote === "string" &&
      lifecycle.validBlockNegativeNote.length > 0,
    "a family with no emulator valid-block negative must say so explicitly",
  );
}
const lifecycleSource = await readRepositoryFile(lifecycle.suite);

// This suite is SHARED with sibling ledger-rule families, so the file's own
// block inventory is measured and published. It is what makes the difference
// between "this gate ran four blocks" and "this family owns two" legible, and it
// is cross-checked against the runner's collected count below.
const suiteDeclaredTitles = [
  ...lifecycleSource.matchAll(/^ {2}it\(\s*"([^"]+)"/gmu),
].map((match) => match[1]);
assert.deepEqual(
  suiteDeclaredTitles,
  lifecycle.suiteBlockTitles,
  `${lifecycle.suite} declares a different set of blocks, in a different order, than the artifact records`,
);

// One `it` block of the shared suite, addressed by its title. Every marker scan
// below is scoped to one of these: a whole-file scan of a suite shared with two
// sibling families would let a sibling's assertions satisfy this family's claims.
const suiteBlock = (title) => {
  const start = lifecycleSource.indexOf(`it("${title}"`);
  assert.ok(
    start >= 0,
    `${lifecycle.suite} has no \`it(\` block for ${title}, so its markers cannot be scoped to this family`,
  );
  const after = lifecycleSource.slice(start);
  const next = after.indexOf("\n  it(");
  return next === -1 ? after : after.slice(0, next);
};

// The candidate modules a refusal message may be raised from: this family's four
// Aiken modules, its three off-chain modules, and the shared off-chain door. The
// sweep over this fixed set is what lets the artifact say WHERE a refusal comes
// from instead of implying the chain refused.
const REFUSAL_ORIGIN_CANDIDATES = [
  ...familyOwnedAikenModules,
  ...lifecycle.refusalOriginCandidates,
];
assert.ok(
  lifecycle.refusalOriginCandidates.length > 0,
  "the refusal-origin sweep must name the off-chain modules it searches, or a measured origin list means nothing",
);

const ONCHAIN_ORIGIN = new Set(familyOwnedAikenModules);
let familyLifecycleBlocks = 0;
let familyValidBlockNegativeBlocks = 0;
let measuredAdversarialAssertions = 0;
for (const block of lifecycle.familyBlocks) {
  assert.ok(
    ["lifecycle", "validBlockNegative"].includes(block.role),
    `emulator block ${block.title} carries unknown role ${String(block.role)}`,
  );
  assert.ok(
    lifecycle.requiredTitles.includes(block.title),
    `emulator block ${block.title} is measured here but is not among the titles this gate requires by name`,
  );
  assert.ok(
    suiteDeclaredTitles.includes(block.title),
    `${lifecycle.suite} no longer declares ${block.title}`,
  );
  const blockSource = suiteBlock(block.title);
  // Structural markers only. They establish that the journey this gate runs
  // really asserts the stages the row claims; the PASSAGE comes from the runner
  // report.
  for (const marker of block.sourceMarkers) {
    assert.ok(
      blockSource.includes(marker),
      `${lifecycle.suite} block "${block.title}" no longer contains ${marker}, so the stage it backs is no longer asserted by THIS family's own block`,
    );
  }
  // The absent markers are recorded gaps. They must be absent from this block and
  // present elsewhere in the file: absent-and-gone-everywhere would make the gap
  // vacuous, and present-in-the-block would make the residual finding false.
  for (const marker of block.absentSourceMarkers ?? []) {
    assert.ok(
      !blockSource.includes(marker),
      `${lifecycle.suite} block "${block.title}" now asserts ${marker}; the gap recorded for it must be closed rather than kept`,
    );
    assert.ok(
      lifecycleSource.includes(marker),
      `${marker} no longer appears anywhere in ${lifecycle.suite}, so recording this block as missing it is no longer a measured comparison against a sibling`,
    );
  }
  assert.ok(
    Array.isArray(block.assertedStages) && block.assertedStages.length > 0,
    `emulator block ${block.title} must enumerate the stages it asserts`,
  );
  // The adversarial assertion, measured. A negative whose refusal is not asserted
  // is a journey that submits nothing and passes.
  const adversarial = block.adversarialAssertion;
  const measuredForms = blockSource.split(adversarial.form).length - 1;
  assert.equal(
    measuredForms,
    adversarial.measuredCount,
    `${lifecycle.suite} block "${block.title}" carries ${String(measuredForms)} ${adversarial.form} assertions, not the ${String(adversarial.measuredCount)} this artifact records`,
  );
  assert.ok(
    adversarial.measuredCount > 0,
    `${lifecycle.suite} block "${block.title}" asserts no refusal at all, so nothing in it is adversarial`,
  );
  assert.ok(
    blockSource.includes(adversarial.messageFragment),
    `${lifecycle.suite} block "${block.title}" no longer expects the refusal ${JSON.stringify(adversarial.messageFragment)}`,
  );
  // Where the refusal is raised is measured, not assumed: the fragment is looked
  // for in every candidate module, and the resulting list decides whether the
  // refusal came from the chain or from a builder ahead of it.
  const measuredOrigins = [];
  for (const candidate of REFUSAL_ORIGIN_CANDIDATES) {
    if (
      (await readRepositoryFile(candidate)).includes(
        adversarial.messageFragment,
      )
    ) {
      measuredOrigins.push(candidate);
    }
  }
  assert.deepEqual(
    measuredOrigins,
    adversarial.measuredOriginModules,
    `${lifecycle.suite} block "${block.title}": the modules that raise ${JSON.stringify(adversarial.messageFragment)} are not the ones this artifact records`,
  );
  assert.ok(
    measuredOrigins.length > 0,
    `${lifecycle.suite} block "${block.title}" expects a refusal no module in this family or its shared door raises, so the assertion cannot be attributed`,
  );
  assert.equal(
    adversarial.raisedOnChain,
    measuredOrigins.some((origin) => ONCHAIN_ORIGIN.has(origin)),
    `${lifecycle.suite} block "${block.title}": the recorded refusal origin disagrees with the measured one`,
  );
  measuredAdversarialAssertions += adversarial.measuredCount;
  if (block.role === "lifecycle") {
    familyLifecycleBlocks += 1;
  } else {
    familyValidBlockNegativeBlocks += 1;
  }
}
assert.equal(
  lifecycle.familyBlockCount,
  lifecycle.familyBlocks.length,
  "the published family block count is not the length of the measured list",
);
assert.equal(
  lifecycle.familyBlockCount,
  lifecycle.requiredTitles.length,
  "every title this gate requires by name must be measured as one of this family's blocks, and no other",
);
assert.equal(
  lifecycle.familyLifecycleCount,
  familyLifecycleBlocks,
  "the published lifecycle count is not the number of blocks measured as lifecycles",
);
assert.equal(
  lifecycle.familyValidBlockNegativeCount,
  familyValidBlockNegativeBlocks,
  "the published negative count is not the number of blocks measured as valid-block negatives",
);
assert.equal(
  lifecycle.familyLifecycleCount + lifecycle.familyValidBlockNegativeCount,
  lifecycle.familyBlockCount,
  "a family block is either a lifecycle or a negative, and the two counts must account for every one exactly once",
);
assert.ok(
  familyLifecycleBlocks > 0,
  "output 9 asks for an emulator lifecycle; a negative alone does not carry it",
);
assert.equal(
  familyValidBlockNegativeBlocks > 0,
  lifecycle.hasValidBlockNegative,
  "hasValidBlockNegative disagrees with the blocks measured as valid-block negatives",
);
assert.equal(
  lifecycle.measuredAdversarialAssertions,
  measuredAdversarialAssertions,
  "the published total of adversarial assertions is not the sum measured inside this family's blocks",
);
// The suite-wide disposition this family's own measurement produces: every
// refusal the file asserts is raised off-chain. It is derived from the blocks
// above rather than stated, and it is what keeps the negative from being read as
// an on-chain rejection.
assert.equal(
  lifecycle.everyRefusalRaisedOffChain,
  lifecycle.familyBlocks.every(
    (block) => block.adversarialAssertion.raisedOnChain === false,
  ),
  "the recorded off-chain-refusal disposition is not the one this family's measured blocks produce",
);
const lifecycleDeclaration = declareVitest(
  lifecycle.suite,
  lifecycle.requiredTitles,
);

// ---------------------------------------------------------------------------
// Output 10 — the parent-owned matrix rows, MEASURED
// ---------------------------------------------------------------------------

// Output 10 is not asserted, it is derived. Both files are parent-owned (the
// manifest's Q14 row lists them under pathsMustNotTouch), so this gate reads what
// they say today from the header-derived Status column and lets the measurement
// decide the cell. A parent edit that lands the rows flips this derivation
// instead of requiring a prose change here.
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
  assert.equal(
    row.recordsLocalPass,
    rowCells[statusIndex].includes("LOCAL_PASS"),
    `${row.file} line ${String(row.line)} records a local status that disagrees with recordsLocalPass`,
  );
  return rowCells[statusIndex];
};

let matrixRowsMeasured = 0;
for (const row of matrices.rows) {
  await measureMatrixRow(row);
  matrixRowsMeasured += 1;
}
assert.ok(
  matrixRowsMeasured > 0,
  "output 10 must measure at least one matrix row, or its status is a claim",
);
// A positive control for the derivation above, run through the SAME header-derived
// machinery rather than string-matched. Without it, "neither row records a local
// status" could equally mean the Status column was being read from the wrong
// place, and this cell would stay OPEN for the wrong reason.
const comparison = matrices.comparisonRow;
await measureMatrixRow(comparison);
assert.ok(
  comparison.recordsLocalPass &&
    comparison.measuredStatusCell.includes("LOCAL_PASS"),
  "the calibration row must be one that DOES record a local status, or the derivation cannot be shown to work",
);

const matricesRecordLocalPass = matrices.rows.every(
  (row) => row.recordsLocalPass,
);

// ---------------------------------------------------------------------------
// Execute. One batched `aiken check -e` under the pinned fork, then each cited
// suite once by its own package's Vitest CLI. Every count published below is read
// out of the resulting reports.
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
// Exact selection: `-m <module>.{<test>}` names one test, so a run that collected
// more than the declared set was not the run these counts describe.
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
    stepScoped: stepScopedSelectors,
    moduleHelper: moduleHelperSelectors,
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
// executed, so a runner list that drifted from the runs is a failure rather than a
// stale line in an artifact.
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

for (const suite of codec.suites) {
  assertRecordedMeasurement({
    label: `output 2 ${suite.file}`,
    recorded: suite.measured,
    outcome: vitestOutcomes.get(suite.file),
  });
}
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
  `${lifecycle.suite} declares ${String(suiteDeclaredTitles.length)} blocks but the runner collected ${String(lifecycleOutcome.collected)} tests`,
);
assert.ok(
  lifecycle.familyBlockCount <= lifecycleOutcome.passed,
  "this family cannot own more emulator blocks than the suite executed",
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
  // A delegated row must name where it is bound; a locally measured one must not,
  // or the reader cannot tell which gate owns its passage.
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

// Reaching this line means every runner this gate spawned passed, so each locally
// measured output is decided by that measurement rather than by the artifact's
// word: an under-claim is as much a drift as an over-claim.
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

// Ownership is CLAIMED, not inferred from prose. An earlier revision of this
// gate asked only whether some finding's text contained a token, and an external
// mutation sweep showed that to be vacuous twice over: deleting the finding that
// owns the output-10 pendency still passed, because two unrelated
// stale-citation findings happen to name the same two matrix files, and deleting
// the finding that owns the off-chain-refusal limit still passed, because an
// unrelated SDK-docstring finding happens to contain the words "off-chain". A
// substring of the whole list is therefore not evidence that anything is owned.
// Each finding now declares exactly what it owns, every claim it declares must
// appear in its OWN text so a bare label cannot claim a gap the finding does not
// discuss, and no two findings may claim the same token — which is what makes
// deleting the owner fail instead of falling through to a neighbour.
const ownedClaims = new Map();
for (const finding of evidence.residualFindings) {
  assert.ok(
    Array.isArray(finding.owns) && finding.owns.length > 0,
    `residual finding ${finding.id} must declare what it owns, so deleting it fails this gate rather than falling through to a neighbouring finding that happens to mention the same words`,
  );
  for (const claim of finding.owns) {
    assert.ok(
      typeof claim === "string" && claim.length > 0,
      `residual finding ${finding.id} declares an empty claim`,
    );
    assert.ok(
      finding.finding.includes(claim),
      `residual finding ${finding.id} claims to own ${JSON.stringify(claim)} but does not discuss it; a claim must be backed by the finding's own text`,
    );
    assert.equal(
      ownedClaims.get(claim),
      undefined,
      `residual findings ${String(ownedClaims.get(claim))} and ${finding.id} both claim ${JSON.stringify(claim)}; a shared claim means neither is the owner and deleting either would pass`,
    );
    ownedClaims.set(claim, finding.id);
  }
}
const requireOwned = (claim, why) => {
  assert.ok(ownedClaims.has(claim), `${why} (unowned claim: ${claim})`);
};

// The honest residuals this family carries must be named rather than absorbed
// into a LOCAL_PASS: the parent-owned output-10 pendency, every assertion its
// journeys are measured NOT to make, the axis it does not ride, the selectors that
// do not drive a handler, and the refusals that are raised ahead of the chain
// rather than by it.
if (!matricesRecordLocalPass) {
  for (const row of matrices.rows) {
    requireOwned(
      row.file,
      `output 10 is OPEN on ${row.file} and that pendency must be owned by a residual finding`,
    );
  }
}
if (!lifecycle.hasValidBlockNegative) {
  requireOwned(
    "emulator valid-block negative",
    "this family has no emulator valid-block negative and that gap must be owned by a residual finding",
  );
}
if (lifecycle.everyRefusalRaisedOffChain) {
  // Anchored on the MEASURED origin modules rather than on the words "off-chain":
  // the origins are values this gate derived by sweeping the family's modules, so
  // a finding that owns them is necessarily discussing this measurement.
  for (const block of lifecycle.familyBlocks) {
    for (const origin of block.adversarialAssertion.measuredOriginModules) {
      requireOwned(
        origin,
        `this family's block "${block.title}" is measured to have its refusal raised off-chain by ${origin}, and that limit on what the emulator observes must be owned by a residual finding rather than read as an on-chain rejection`,
      );
    }
  }
}
for (const block of lifecycle.familyBlocks) {
  for (const marker of block.absentSourceMarkers ?? []) {
    requireOwned(
      marker,
      `this family's block "${block.title}" is measured not to assert ${marker} and that gap must be owned by a residual finding`,
    );
  }
}
// The delegation is a real dependency and must be stated as one, so a reader of
// the four delegated rows knows what re-opens them.
requireOwned(
  SHARED_ARTIFACT,
  "the four delegated outputs rest on the shared artifact and that dependency must be owned by a residual finding",
);
if (!spendInputAxisApplies) {
  requireOwned(
    "spendInputTierRoutedCarriage",
    "this family is deliberately absent from one of the two carriage remediations and that disposition must be owned by a residual finding, not left as an unexplained absence",
  );
}
if (moduleHelperSelectors > 0) {
  requireOwned(
    "module-helper",
    "selectors that do not drive the family's own handler are counted in the census and that must be owned by a residual finding, so a reader of the positive count is not misled",
  );
}
// The census this gate measures is not the one the manifest row predicts, and a
// gate that quietly out-measured its own task row would leave the row to be
// believed. The drift is owned rather than corrected here: the manifest is
// parent-owned.
requireOwned(
  "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json",
  "the manifest row for this family is parent-owned and its predicted counts are not the measured ones; that drift must be owned by a residual finding",
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
  // Published beside the role split because the per-step requirement above is
  // stated on the step-scoped selectors, and because the scope of every selector
  // is derived from whether its body reaches this family's own handler.
  aikenStepScopedSelectors: stepScopedSelectors,
  aikenModuleHelperSelectors: moduleHelperSelectors,
  // Derived from the parsed spec table, not asserted: this family opens exactly
  // one of the nine committed fields.
  committedFieldsOpened: binding.openedCommittedFields.length,
  vitestSuitesExecuted: vitestOutcomes.size,
  vitestTestsPassed,
  vitestTitlesRequired,
  emulatorBlocksExecuted: lifecycleOutcome.passed,
  // Runner-derived like the rest: `requiredTitles` are the titles
  // deriveVitestOutcome refused to accept as absent, and every collected test in
  // that report passed. These counts are published beside the whole-file one
  // because this family shares its suite with two sibling ledger-rule families,
  // and split because a lifecycle and a refusal answer different questions.
  emulatorFamilyBlocks: lifecycle.familyBlockCount,
  emulatorFamilyLifecycles: lifecycle.familyLifecycleCount,
  emulatorFamilyValidBlockNegatives: lifecycle.familyValidBlockNegativeCount,
  emulatorAdversarialAssertions: measuredAdversarialAssertions,
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
// Positive controls: the same harness must still accept real passing runs, so the
// rejections above cannot be a gate that rejects everything. The compiler control
// reads an identity out of a stub binary, which is the property the hard pin above
// depends on — that the version is measured from the spawned binary rather than
// supplied by a constant in this gate.
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
  spendInputAxisApplies,
  spendInputAxisScope,
  spendInputAxisSweptModules: sweep.measuredModuleCount,
  aikenModules: evidence.summary.aikenModules,
  aikenSelectorsDeclared: evidence.summary.aikenSelectorsDeclared,
  aikenSelectorsPassed: evidence.summary.aikenSelectorsPassed,
  aikenPositiveSelectors: evidence.summary.aikenPositiveSelectors,
  aikenValidBlockNegativeSelectors:
    evidence.summary.aikenValidBlockNegativeSelectors,
  aikenAdditionalNegativeSelectors:
    evidence.summary.aikenAdditionalNegativeSelectors,
  aikenStepScopedSelectors: evidence.summary.aikenStepScopedSelectors,
  aikenModuleHelperSelectors: evidence.summary.aikenModuleHelperSelectors,
  aikenWallSeconds,
  committedFieldsOpened: evidence.summary.committedFieldsOpened,
  openedCommittedFieldIndex: boundFieldIndex,
  vitestSuitesExecuted: evidence.summary.vitestSuitesExecuted,
  vitestTestsPassed: evidence.summary.vitestTestsPassed,
  vitestTitlesRequired: evidence.summary.vitestTitlesRequired,
  slowestVitestSuite,
  emulatorBlocksExecuted: evidence.summary.emulatorBlocksExecuted,
  emulatorFamilyLifecycles: evidence.summary.emulatorFamilyLifecycles,
  emulatorFamilyValidBlockNegatives:
    evidence.summary.emulatorFamilyValidBlockNegatives,
  emulatorAdversarialAssertions: evidence.summary.emulatorAdversarialAssertions,
  everyRefusalRaisedOffChain: lifecycle.everyRefusalRaisedOffChain,
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
    )} positive of which ${String(
      evidence.summary.aikenStepScopedSelectors -
        (evidence.summary.aikenValidBlockNegativeSelectors +
          evidence.summary.aikenAdditionalNegativeSelectors),
    )} step-scoped, ${String(
      evidence.summary.aikenValidBlockNegativeSelectors,
    )} valid-block negative, ${String(
      evidence.summary.aikenAdditionalNegativeSelectors,
    )} further negative, ${String(
      evidence.summary.aikenModuleHelperSelectors,
    )} module-helper), ${String(evidence.summary.vitestTestsPassed)} tests over ${String(
      evidence.summary.vitestSuitesExecuted,
    )} suites with ${String(evidence.summary.vitestTitlesRequired)} required titles, ${String(
      evidence.summary.emulatorBlocksExecuted,
    )} emulator blocks (${String(
      evidence.summary.emulatorFamilyLifecycles,
    )} lifecycle and ${String(
      evidence.summary.emulatorFamilyValidBlockNegatives,
    )} valid-block negative this family's, ${String(
      evidence.summary.emulatorAdversarialAssertions,
    )} adversarial assertions all raised off-chain), committed field ${String(
      boundFieldIndex,
    )} of 9 opened through the §8.8 door with its authenticated item count pinned to 0, outputs 5-8 delegated to the Q1x gate with the spend-input cardinality axis measured out of scope across ${String(
      sweep.measuredModuleCount,
    )} family-owned Aiken modules, output 10 ${
      matricesRecordLocalPass ? "LOCAL_PASS" : "OPEN"
    } on ${String(matrixRowsMeasured)} parent-owned matrix rows)`,
  );
}
