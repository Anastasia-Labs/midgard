#!/usr/bin/env node
// Verifies the Q39 (`fabricated-deposit`) binding of GOAL_SPEC.md 9.1 outputs 1
// to 10.
//
// The rule enforced here is the one the per-family closure contract states: an
// output may be published LOCAL_PASS only when the artifact's claim is
// re-derivable from this tree, an OPEN cell must say why it is open and who owns
// it, and an N/A cell must justify itself. Nothing is taken on the artifact's
// word. Every rule anchor, catalogue index, codec field, enum arm, on-chain
// selector, builder name, CLI verb, emulator marker and matrix cell it names is
// re-read out of its own source file, and every count it publishes is derived
// from a runner report this gate produced: one batched `aiken check -e`
// invocation under the pinned fork compiler, the fork's own format normalizer
// over the family's eight modules, one Vitest run per cited suite, and one
// adversarial proof-fit sweep computed at run time.
//
// Four disciplines are inherited deliberately from the sibling gates.
//
// From verify-canonical-v1-proof-family-q1x.mjs (issue #533, finding V-2 of
// #519): existence is never accepted in place of passage. A count of `test`
// declarations in an Aiken module or of `it(` lines in a suite is not a count of
// checks that passed — eight throwing bodies, an `it.skip`, or a selector that
// collects nothing leave such a count untouched — so source scanning survives
// here only where it backs no count: the structural claims about roles, scopes,
// fields, arms, markers and token absence.
//
// From verify-canonical-v1-q60-commit-end-time-bound.mjs: a gate that publishes
// an on-chain result has to know which compiler produced it. The identity is
// measured from the spawned binary and required to match the fork pin EXACTLY,
// not by prefix, because a second fork build of the same 1.1.23 base reports a
// different `+<rev>` suffix and would supply this family's result under a
// compiler .github/workflows/aiken-ci.yml does not pin.
//
// From verify-canonical-v1-proof-family-q14.mjs: the selector census carries the
// step/module-helper scope split, and the scope is DERIVED rather than declared —
// for every selector this gate resolves whether the declaration reaches
// `main.spend`, directly or through a module-local helper that does, and requires
// the published route to be the one its body takes. A label is not a
// measurement, and output 4's per-step requirement is stated on the step-scoped
// positives.
//
// From verify-canonical-v1-proof-family-q12.mjs: this family opens NONE of the
// nine section-2.5 committed fields, so output 3 is measured as an ABSENCE plus
// the anchoring that does bind, rather than as a bound field.
//
// Five things are NOT inherited, because this family's shape forbids it.
//
// 1. Nothing is delegated. This family is not one of the four bound by the
//    shared Q1x artifact, so output 5's adversarial proof fit is measured HERE,
//    at run time, and no cell of this artifact may cite a sibling artifact's
//    numbers. The sweep reads `max_deposit_count_v1` out of the Aiken source,
//    builds a real deposits MPF at five cardinalities up to that bound, proves
//    EVERY leaf at each rung and keeps the BYTE-maximising proof rather than the
//    step-maximising one — they are different leaves, and selecting on steps
//    publishes an optimistic worst case — then encodes this family's real step
//    redeemers and thread datums through the SDK's own codec and checks the
//    published margins.
//
// 2. The codec the sweep measures through is a BUILT artifact, so it is anchored
//    before it is trusted. The two Aiken-measured leaf constants are read out of
//    the tracked family suite, decoded through the loaded schemas, re-encoded
//    through the family's own byte helpers, and required to reproduce the same
//    bytes. A stale or missing build fails the sweep at its first step with a
//    diagnostic naming the build command, instead of publishing sizes taken
//    under a codec nobody checked.
//
// 3. Four of the ten cells are OPEN, and every one of those statuses is DERIVED
//    from a measurement of the surface it names rather than asserted: the count
//    of the family's blueprint titles present in the frozen blueprint and its
//    membership in the append-only catalogue order (output 6), the count of CLI
//    verbs in the parent-owned `bin.ts` (output 8), the emulator suite's measured
//    skip (output 9), and the two parent-owned matrix cells (output 10). The
//    #617 regeneration wave and the parent's matrix edits therefore close those
//    cells without an edit to the artifact's prose — and a cell that closed
//    while the artifact still recorded OPEN fails here.
//
// 4. Output 9's emulator suite is measured through a purpose-built derivation.
//    The shared `deriveVitestOutcome` refuses a skipped test outright, which is
//    right for every suite that claims passage; this suite claims a BOUNDARY, so
//    the derivation requires exactly one collected test whose assertion status is
//    exactly `skipped`, zero failures, exit 0, all four structural markers inside
//    the block's own body, and the eight-title absence the block asserts to be
//    the blueprint's measured state — with a control title required present in
//    the same read, so "the family is missing" can never be satisfied by a failed
//    or empty blueprint.
//
// 5. Every absence this gate measures carries a positive control in the same
//    read: the blueprint's control title for the missing family titles, a
//    settled sibling's CLI verb for the missing verbs, and a settled sibling's
//    matrix row for the missing matrix status. An absence measured without one is
//    indistinguishable from a read that failed.
//
// usage: node demo/scripts/verify-canonical-v1-proof-family-q39.mjs [--json]

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { createRequire } from "node:module";
import { resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

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

const GOAL_ID = "Q39";
const FAMILY = "fabricated-deposit";
const OUTPUTS = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const ARTIFACT =
  "docs/exec-plans/evidence/canonical-v1-proof-family-q39-v1.json";
const SHARED_ARTIFACT =
  "docs/exec-plans/evidence/canonical-v1-proof-family-q1x-v1.json";
const MANIFEST =
  "docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json";
const GATE = "demo/scripts/verify-canonical-v1-proof-family-q39.mjs";
const AIKEN_PROJECT_DIRECTORY = "onchain/aiken";
const aikenProjectRoot = resolve(repositoryRoot, AIKEN_PROJECT_DIRECTORY);
const FORMAT_SCRIPT = "scripts/verify-normalized-format.mjs";
const CATALOGUE = "demo/midgard-sdk/src/fraud-proof/catalogue.ts";
const LEDGER_STATE = "onchain/aiken/lib/midgard/ledger-state.ak";
const BLUEPRINT = "onchain/aiken/plutus.json";
const FAMILY_SUITE =
  "demo/midgard-fault-proofs/tests/fabricated-deposit.test.ts";
const SDK_TWIN_SUITE = "demo/midgard-sdk/tests/fabricated-deposit-v1.test.ts";
const EMULATOR_SUITE =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-fabricated-deposit.test.ts";

// `.github/workflows/aiken-ci.yml` AIKEN_FORK_VERSION. The stock v1.1.22 that
// used to compile the blueprint ships a live unsound-codegen defect and is
// retired from every role; this is the only compiler entitled to produce this
// family's on-chain result, and the only formatter entitled to normalize its
// modules.
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
  "midgard.canonical-v1-proof-family-q39.v1",
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
    evidence.integrationIssue.includes("#617"),
  "the integrating owner must be stated: four of this family's ten cells are OPEN pending the #617 regeneration wave, and an unnamed owner leaves them unowned",
);
assert.ok(
  typeof evidence.note === "string" && evidence.note.length > 0,
  "the artifact must explain its own shape, including how it differs from its siblings",
);

// The live half of a family's closure belongs exclusively to the Q57 sweep and
// QG3 (GOAL_SPEC.md 9.1). A family-local artifact may never record it, so the
// check is on the artifact's raw text rather than on a walk of status keys: a
// status smuggled into a prose field would pass the walk.
assert.ok(
  !artifactText.includes("LIVE_PASS"),
  "a family-local artifact must never record a LIVE_PASS status anywhere, in a status field or in prose; live status belongs to Q57/QG3",
);

// This family delegates nothing. A delegation block would mean some cell of this
// artifact rests on a sibling artifact's numbers, which is exactly what the
// manifest row forbids for a family the shared Q1x artifact does not bind.
assert.equal(
  evidence.delegatedOutputs,
  undefined,
  "this family is not bound by the shared Q1x artifact, so it may not declare delegated outputs; output 5 is measured here",
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
assert.equal(
  evidence.compiler.binaryEnvironmentVariable,
  "MIDGARD_AIKEN_BIN",
  "the artifact must name the environment variable this gate resolves the compiler through",
);
assert.equal(
  evidence.compiler.binaryEnvironmentFallback,
  "MIDGARD_FORK_AIKEN_BIN",
  "the artifact must name the fallback variable sibling gates use for the same role",
);
const measuredCompiler = aikenCompilerVersion(aikenBinaryPath);
assert.equal(
  measuredCompiler,
  PINNED_FORK_COMPILER,
  `ERR_Q39_WRONG_TEST_COMPILER: ${aikenBinaryPath} reports "${measuredCompiler}" but this family must execute under exactly "${PINNED_FORK_COMPILER}"; set MIDGARD_AIKEN_BIN (or MIDGARD_FORK_AIKEN_BIN) to the patched fork`,
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

const columnIndex = ({ file, headerCells, header }) => {
  const index = headerCells.indexOf(header);
  assert.ok(
    index > 0,
    `${file} has no ${header} column, so no cell of it can be measured`,
  );
  return index;
};

// A GFM table's header is the row immediately above its delimiter row. Resolving
// it that way rather than by looking for a known column name means this gate can
// measure a cell in any of these files' tables without being told the table's
// shape, and cannot silently walk past a header it did not recognise.
const TABLE_DELIMITER = /^\|[\s:|-]+\|\s*$/u;

// A commented-out call is not a call. Every structural read of Aiken source in
// this gate runs over the comment-stripped text, so a marker, a handler reach or
// a token sweep can never be satisfied by prose.
const withoutAikenComments = (source) =>
  source
    .split(/\r?\n/u)
    .filter((line) => !line.trim().startsWith("//"))
    .join("\n");

const requireSourceMarker = ({ source, marker, where }) => {
  assert.ok(
    source.includes(marker),
    `${where} no longer contains ${JSON.stringify(marker)}`,
  );
};

// Declarations only: which titles each suite is claimed to contain. Nothing here
// decides whether any of them passed; the runner does that further down. The
// declaration order is the published-command order, so output 1's suite is
// declared before output 2's byte twin.
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

const declaredTitles = (source) =>
  [...source.matchAll(/^ {2}it\(\s*"((?:[^"\\]|\\.)*)"/gmu)].map(
    (match) => match[1],
  );

// Field names of an Aiken record, in declaration order, read out of the type's
// own body. Doc comments sit at the same indent and start with `/`, so they never
// enter the field list; nested records sit one indent deeper.
const recordFieldNames = (body, indent) =>
  [
    ...body.matchAll(new RegExp(`^ {${String(indent)}}([a-z0-9_]+):`, "gmu")),
  ].map((match) => match[1]);

const aikenTypeBody = ({ source, type, where }) => {
  const opener = `pub type ${type} {`;
  const start = source.indexOf(opener);
  assert.ok(start >= 0, `${where} no longer declares ${opener}`);
  const end = source.indexOf("\n}", start);
  assert.ok(end > start, `${where}: ${type}'s declaration is unterminated`);
  return source.slice(start + opener.length, end);
};

const aikenRecordFields = async ({ module, type }) => {
  const source = withoutAikenComments(await readRepositoryFile(module));
  return recordFieldNames(aikenTypeBody({ source, type, where: module }), 2);
};

// Constructor names of an Aiken enum, in declaration order. Order IS wire
// format: each of these crosses the boundary as a Plutus constructor index.
const aikenEnumArms = async ({ module, type }) => {
  const source = withoutAikenComments(await readRepositoryFile(module));
  return [
    ...aikenTypeBody({ source, type, where: module }).matchAll(
      /^ {2}([A-Z][A-Za-z0-9_]*)/gmu,
    ),
  ].map((match) => match[1]);
};

const typescriptSchemaFields = async ({ module, schema }) => {
  const source = await readRepositoryFile(module);
  const opener = `${schema} = Data.Object({`;
  const start = source.indexOf(opener);
  assert.ok(start >= 0, `${module} no longer declares ${opener}`);
  const end = source.indexOf("});", start);
  assert.ok(end > start, `${module}: ${schema}'s declaration is unterminated`);
  return recordFieldNames(source.slice(start + opener.length, end), 2);
};

// Arm names of a `Data.Enum([...])`, in declaration order. A literal arm carries
// its name in `Data.Literal("...")`; a record arm carries it as the single key of
// the wrapping `Data.Object`, one indent inside the enum's list.
const typescriptEnumArms = async ({ module, schema }) => {
  const source = await readRepositoryFile(module);
  const opener = `${schema} = Data.Enum([`;
  const start = source.indexOf(opener);
  assert.ok(start >= 0, `${module} no longer declares ${opener}`);
  const end = source.indexOf("\n]);", start);
  assert.ok(end > start, `${module}: ${schema}'s declaration is unterminated`);
  return [
    ...source
      .slice(start + opener.length, end)
      .matchAll(
        /Data\.Literal\("([A-Z][A-Za-z0-9_]*)"\)|^ {4}([A-Z][A-Za-z0-9_]*):/gmu,
      ),
  ].map((match) => match[1] ?? match[2]);
};

const hexBytes = (hex) => {
  assert.ok(
    typeof hex === "string" && /^(?:[0-9a-f]{2})*$/u.test(hex),
    `expected lowercase hex, measured ${JSON.stringify(hex)}`,
  );
  return hex.length / 2;
};

// ---------------------------------------------------------------------------
// Output 1 — normative rule, violation identifier, derived catalogue identity
// ---------------------------------------------------------------------------

const rule = evidence.output1RuleAndViolationIdentifier;
assert.ok(
  typeof rule.rule === "string" && rule.rule.length > 200,
  "output 1 must state the rule in full — both arms and the inclusion window — not only its identifiers",
);
assert.deepEqual(
  rule.violationIdentifiers,
  [FAMILY],
  "output 1's violation identifier set drifted from the family it names",
);

for (const declaration of rule.identifierDeclarations) {
  const source = await readRepositoryFile(declaration.file);
  requireSourceMarker({
    source,
    marker: `${declaration.declaration} = ${JSON.stringify(declaration.value)}`,
    where: `output 1 identifier declaration in ${declaration.file}:`,
  });
  assert.equal(
    declaration.value,
    FAMILY,
    `${declaration.file} declares ${declaration.declaration} as ${declaration.value}, which is not this family's violation identifier`,
  );
}
assert.ok(
  rule.identifierDeclarations.length >= 2,
  "the violation identifier must be measured on both sides of the boundary the family spans",
);

// The deployed category identifier is DERIVED from the append-only order rather
// than read from a constant, because the constant is what a reservation can
// drift from. `fabricatedDeposit` is measured ABSENT from the order, so the next
// append slot is the order's own length, encoded big-endian over the width the
// catalogue declares.
const catalogueSource = await readRepositoryFile(CATALOGUE);
const orderOpener = "FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = [";
const orderStart = catalogueSource.indexOf(orderOpener);
assert.ok(orderStart >= 0, `${CATALOGUE} no longer declares ${orderOpener}`);
const orderEnd = catalogueSource.indexOf("\n]", orderStart);
assert.ok(
  orderEnd > orderStart,
  `${CATALOGUE}: the category order is unterminated`,
);
const catalogueOrder = [
  ...catalogueSource
    .slice(orderStart + orderOpener.length, orderEnd)
    .matchAll(/"([A-Za-z][A-Za-z0-9]*)"/gu),
].map((match) => match[1]);
const idByteCountMatch = /FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT = (\d+);/u.exec(
  catalogueSource,
);
assert.ok(
  idByteCountMatch !== null,
  `${CATALOGUE} no longer declares FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT`,
);
const catalogueIdByteCount = Number(idByteCountMatch[1]);

const catalogue = rule.catalogueBinding;
assert.equal(
  catalogue.file,
  CATALOGUE,
  "output 1 cites a different catalogue module",
);
assert.equal(
  catalogue.orderConstant,
  "FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
  "output 1 cites a different order constant",
);
assert.equal(
  catalogue.measuredCategoryCount,
  catalogueOrder.length,
  "the published catalogue order length is not the one the order declares",
);
assert.equal(
  catalogue.categoryIdByteCount,
  catalogueIdByteCount,
  "the published identifier width is not the one the catalogue declares",
);
const catalogueRegistered = catalogueOrder.includes(catalogue.category);
assert.equal(
  catalogue.registered,
  catalogueRegistered,
  `the artifact records this family as ${catalogue.registered ? "registered" : "unregistered"} but the append-only order says otherwise`,
);
// A positive control on the same read: the order must carry settled siblings, so
// "this family is absent" cannot be satisfied by an order this gate failed to
// parse.
assert.ok(
  catalogueOrder.includes("zeroInput") && catalogueOrder.length > 5,
  `${CATALOGUE}: the parsed category order does not carry the settled siblings it must, so the family's measured absence is not trustworthy`,
);
const derivedCategoryIndex = catalogueRegistered
  ? catalogueOrder.indexOf(catalogue.category)
  : catalogueOrder.length;
assert.equal(
  catalogue.categoryIndex,
  derivedCategoryIndex,
  "the published category index is not the one the append-only order yields",
);
const derivedCategoryId = derivedCategoryIndex
  .toString(16)
  .padStart(catalogueIdByteCount * 2, "0");
assert.equal(
  catalogue.categoryId,
  derivedCategoryId,
  "the published category id is not the one the order's own index encodes",
);

const onchainCategory = rule.onchainCategoryConstant;
requireSourceMarker({
  source: await readRepositoryFile(onchainCategory.file),
  marker: `${onchainCategory.declaration} = #"${onchainCategory.value}"`,
  where: `output 1's on-chain category constant in ${onchainCategory.file}:`,
});
assert.equal(
  onchainCategory.value,
  derivedCategoryId,
  "the on-chain category constant is not the identifier the append-only order derives; a reservation that drifts from the order it reserves in would file this family's convictions under another family's rule",
);
const offchainCategory = rule.offchainCategoryConstant;
requireSourceMarker({
  source: await readRepositoryFile(offchainCategory.file),
  marker: `${offchainCategory.declaration} = ${JSON.stringify(offchainCategory.value)}`,
  where: `output 1's off-chain category constant in ${offchainCategory.file}:`,
});
assert.equal(
  offchainCategory.value,
  derivedCategoryId,
  "the SDK category constant is not the identifier the append-only order derives",
);

for (const site of rule.onchainDecisionSites) {
  requireSourceMarker({
    source: withoutAikenComments(await readRepositoryFile(site.file)),
    marker: site.declaration,
    where: `output 1's decision site in ${site.file}:`,
  });
  assert.ok(
    typeof site.role === "string" && site.role.length > 0,
    `${site.declaration} must state what it decides`,
  );
}
assert.equal(
  rule.measuredDecisionSiteCount,
  rule.onchainDecisionSites.length,
  "the published decision-site count is not the number of sites listed",
);
assert.ok(
  typeof rule.whyNoLocalRejectCode === "string" &&
    rule.whyNoLocalRejectCode.length > 0,
  "a family with no RejectCodes member must justify the absence rather than leave it unexplained",
);
declareVitest(FAMILY_SUITE, rule.measuredRuleEnforcementTitles);
assert.ok(
  typeof rule.ruleEnforcementNote === "string" &&
    rule.ruleEnforcementNote.length > 0,
  "the cited rule-enforcement titles must say what they are required to prove",
);

// ---------------------------------------------------------------------------
// Output 2 — canonical evidence schema and strict codec agreement
// ---------------------------------------------------------------------------

const codec = evidence.output2SchemaAndCodecAgreement;
let measuredFieldCount = 0;
for (const record of codec.records) {
  const where = `output 2 record ${record.label}`;
  const aikenFields = await aikenRecordFields({
    module: record.aikenModule,
    type: record.aikenType,
  });
  const typescriptFields = await typescriptSchemaFields({
    module: record.typescriptModule,
    schema: record.typescriptSchema,
  });
  assert.ok(
    aikenFields.length > 0,
    `${where}: ${record.aikenType} parsed to zero fields, so nothing was compared`,
  );
  assert.deepEqual(
    aikenFields,
    record.fields,
    `${where}: the published field list is not the one ${record.aikenModule} declares`,
  );
  assert.deepEqual(
    typescriptFields,
    record.fields,
    `${where}: the published field list is not the one ${record.typescriptModule} declares`,
  );
  assert.equal(
    record.fieldCount,
    record.fields.length,
    `${where}: the published field count is not the length of its own field list`,
  );
  measuredFieldCount += record.fields.length;
}
assert.equal(
  codec.measuredRecordCount,
  codec.records.length,
  "output 2's published record count is not the number of records it compares",
);
assert.equal(
  codec.measuredFieldCount,
  measuredFieldCount,
  "output 2's published field count is not the number of fields it compared",
);

assert.equal(
  codec.enumBindingIsPositional,
  true,
  "these enums cross the boundary as constructor indices, so the binding must be positional; a set comparison would accept a swap of two arms",
);
assert.ok(
  typeof codec.whyPositional === "string" && codec.whyPositional.length > 0,
  "output 2 must say why the enum binding is ordered rather than set-wise",
);
for (const enumeration of codec.enums) {
  const where = `output 2 enum ${enumeration.label}`;
  const aikenArms = await aikenEnumArms({
    module: enumeration.aikenModule,
    type: enumeration.aikenType,
  });
  const typescriptArms = await typescriptEnumArms({
    module: enumeration.typescriptModule,
    schema: enumeration.typescriptSchema,
  });
  assert.ok(
    aikenArms.length > 1,
    `${where}: ${enumeration.aikenType} parsed to ${String(aikenArms.length)} arm(s), so nothing was compared`,
  );
  assert.deepEqual(
    aikenArms,
    enumeration.arms,
    `${where}: the published arm order is not the one ${enumeration.aikenModule} declares`,
  );
  assert.deepEqual(
    typescriptArms,
    enumeration.arms,
    `${where}: the published arm order is not the one ${enumeration.typescriptModule} declares`,
  );
}
assert.equal(
  codec.measuredEnumCount,
  codec.enums.length,
  "output 2's published enum count is not the number of enums it compares",
);

for (const alias of codec.aliasBindings) {
  requireSourceMarker({
    source: await readRepositoryFile(alias.file),
    marker: alias.binding,
    where: `output 2's alias binding in ${alias.file}:`,
  });
  assert.ok(
    typeof alias.why === "string" && alias.why.length > 0,
    `the alias binding in ${alias.file} must say what it makes load-bearing`,
  );
}
assert.equal(
  codec.measuredAliasBindingCount,
  codec.aliasBindings.length,
  "output 2's published alias-binding count is not the number of bindings it measures",
);

assert.equal(
  codec.byteTwinSuite,
  SDK_TWIN_SUITE,
  "output 2 cites a different byte-twin suite",
);
assert.equal(
  codec.familySuite,
  FAMILY_SUITE,
  "output 2 cites a different family suite",
);
// The byte twin's inventory must be EXACTLY the titles the artifact cites: a twin
// added to that suite and left uncited would sit outside every count published
// here.
const twinSource = await readRepositoryFile(SDK_TWIN_SUITE);
assert.deepEqual(
  declaredTitles(twinSource),
  codec.byteTwinTitles,
  `${SDK_TWIN_SUITE} declares a different set of tests, in a different order, than output 2 cites`,
);
declareVitest(SDK_TWIN_SUITE, codec.byteTwinTitles);

// ---------------------------------------------------------------------------
// Output 3 — committed-field binding, measured as an ABSENCE plus its anchoring
// ---------------------------------------------------------------------------

const binding = evidence.output3CommittedFieldBinding;
assert.equal(
  binding.opensNoCommittedField,
  true,
  "output 3's shape for this family is an absence; a family that opens a committed field must publish the bound field instead",
);
assert.deepEqual(
  binding.openedCommittedFields,
  [],
  "output 3 records an absence, so the opened-field list must be empty",
);

const sweep = binding.absenceSweep;
assert.equal(
  sweep.measuredModuleCount,
  sweep.modules.length,
  "output 3's swept-module count is not the number of modules it sweeps",
);
let measuredAbsenceOccurrences = 0;
for (const module of sweep.modules) {
  const source = withoutAikenComments(await readRepositoryFile(module));
  assert.ok(
    source.length > 0,
    `${module} read empty, so its token sweep would pass vacuously`,
  );
  for (const token of sweep.absentTokens) {
    measuredAbsenceOccurrences += [...source.matchAll(new RegExp(token, "gu"))]
      .length;
  }
}
assert.equal(
  sweep.measuredOccurrences,
  measuredAbsenceOccurrences,
  "output 3's swept occurrence count is not the one this gate measured",
);
assert.equal(
  measuredAbsenceOccurrences,
  0,
  "output 3 records that this family opens no committed field, but a section-8.8 door or field-hash token is present in its modules",
);

const anchoring = binding.anchoring;
const countedRootCall = anchoring.countedRootCall;
const membershipSource = withoutAikenComments(
  await readRepositoryFile(countedRootCall.file),
);
for (const marker of [
  countedRootCall.declaration,
  countedRootCall.sharedCall,
  countedRootCall.domain,
  ...countedRootCall.boundHeaderFields.map((field) => `header.${field}`),
  ...countedRootCall.serialisedKeyAndValue,
]) {
  requireSourceMarker({
    source: membershipSource,
    marker,
    where: `output 3's counted-root anchoring in ${countedRootCall.file}:`,
  });
}
for (const commitment of anchoring.contentCommitments) {
  const source = withoutAikenComments(
    await readRepositoryFile(commitment.file),
  );
  for (const marker of commitment.sourceMarkers) {
    requireSourceMarker({
      source,
      marker,
      where: `output 3's content commitment in ${commitment.file}:`,
    });
  }
  requireSourceMarker({
    source: await readRepositoryFile(commitment.twinFile),
    marker: `export const ${commitment.twin} = `,
    where: `output 3's off-chain twin in ${commitment.twinFile}:`,
  });
}
assert.equal(
  anchoring.measuredContentCommitmentCount,
  anchoring.contentCommitments.length,
  "output 3's published commitment count is not the number of commitments it measures",
);
const preimage = anchoring.openedPreimageSite;
const preimageSource = withoutAikenComments(
  await readRepositoryFile(preimage.file),
);
for (const marker of [
  preimage.declaration,
  preimage.hashCheck,
  preimage.pairingRefusal,
]) {
  requireSourceMarker({
    source: preimageSource,
    marker,
    where: `output 3's opened preimage site in ${preimage.file}:`,
  });
}

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
// decoration.
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
assert.equal(
  onchain.handlerCall,
  HANDLER_CALL,
  "output 4's published handler call is not the one this gate resolves selector scope against",
);
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
// The compiler's own report carries the declaration form back as `on_failure`, so
// the role has two independent sources that must agree.
const ROLE_ON_FAILURE = {
  positive: "fail_immediately",
  validBlockNegative: "succeed_eventually",
  additionalNegative: "succeed_eventually",
};
const SELECTOR_SCOPES = ["step", "moduleHelper"];

const declaredAiken = [];
const selectorExpectations = new Map();
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
  for (const selector of module.selectors) {
    const name = selector.selector;
    assert.ok(
      /^[a-z0-9_]+$/u.test(name),
      `${where} selector ${name} is not a focused-check-safe name`,
    );
    assert.ok(
      typeof selector.claim === "string" && selector.claim.length > 0,
      `${where} selector ${name} must state what it proves`,
    );
    const mustFail = ROLE_MUST_FAIL[selector.role];
    assert.ok(
      mustFail !== undefined,
      `${where} selector ${name} carries unknown role ${String(selector.role)}`,
    );
    assert.ok(
      SELECTOR_SCOPES.includes(selector.scope),
      `${where} selector ${name} carries unknown scope ${String(selector.scope)}`,
    );
    assert.equal(
      declarationBySelector.get(name).failsByDeclaration,
      mustFail,
      `${where} selector ${name} is declared as a ${declarationBySelector.get(name).failsByDeclaration ? "failure" : "success"} test, which contradicts its recorded role ${selector.role}`,
    );
    assert.equal(
      selector.onFailure,
      ROLE_ON_FAILURE[selector.role],
      `${where} selector ${name} publishes an on_failure disposition that contradicts its recorded role ${selector.role}`,
    );
    const measuredReach = reach.tests.get(name) ?? [];
    assert.deepEqual(
      measuredReach,
      selector.handlerReach,
      `${where} selector ${name}: the published route to ${HANDLER_CALL} is not the one its body takes`,
    );
    assert.equal(
      selector.scope === "step",
      measuredReach.length > 0,
      `${where} selector ${name} is recorded ${selector.scope} but ${measuredReach.length > 0 ? "does" : "does not"} reach ${HANDLER_CALL}`,
    );
    if (selector.scope === "moduleHelper") {
      assert.equal(
        selector.role,
        "positive",
        `${where} selector ${name} is a module-helper unit case and may not be counted as a ${selector.role}; only a step-scoped selector drives the handler a negative has to be judged against`,
      );
      moduleHelperSelectors += 1;
    } else {
      stepScopedSelectors += 1;
    }
    if (selector.role === "positive") {
      positiveSelectors += 1;
      if (selector.scope === "step") {
        moduleStepScopedPositives += 1;
      }
    } else if (selector.role === "validBlockNegative") {
      moduleValidBlockNegatives += 1;
      validBlockNegativeSelectors += 1;
    } else {
      additionalNegativeSelectors += 1;
    }
    declaredAiken.push({ module: module.module, selector: name });
    selectorExpectations.set(name, {
      module: module.module,
      onFailure: selector.onFailure,
      mem: selector.mem,
      cpu: selector.cpu,
    });
  }
  assert.equal(
    module.stepScopedPositiveCount,
    moduleStepScopedPositives,
    `${where} publishes a step-scoped positive count that is not the number its own selector list carries`,
  );
  // GOAL_SPEC.md 9.1 output 4 asks for positive AND valid-block negative tests
  // for the family's proof STEPS, so the requirement is checked per step rather
  // than in aggregate — and on step-scoped positives, so a module-local unit case
  // added later cannot satisfy it with the real proof-step positives deleted.
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
assert.ok(
  typeof onchain.roleDerivation === "string" &&
    typeof onchain.scopeDerivation === "string" &&
    typeof onchain.perStepRequirement === "string",
  "output 4 must state how its roles, scopes and per-step requirement are derived",
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

// The eight family-owned modules must be byte-identical to what the pinned
// fork's own formatter produces. This gate parses those modules for roles,
// scopes, handler reach and token absence, so a reformat that moved a
// `test ... fail` declaration off the start of a line would silently change what
// the parses measure.
const formatGate = onchain.formatGate;
const formatFiles = [
  ...sweep.modules.map((module) =>
    module.replace(new RegExp(`^${AIKEN_PROJECT_DIRECTORY}/`, "u"), ""),
  ),
].sort();
assert.equal(
  formatGate.measuredFileCount,
  formatFiles.length,
  "output 4's format gate publishes a file count that is not the number of family-owned modules output 3 sweeps",
);
const formatCommand = `cd ${AIKEN_PROJECT_DIRECTORY} && node ${FORMAT_SCRIPT} ${formatFiles.join(" ")}`;
assert.equal(
  formatGate.command,
  formatCommand,
  "output 4's published format command is not the one this gate executes",
);

// ---------------------------------------------------------------------------
// Output 5 — the adversarial proof fit, MEASURED here at run time
// ---------------------------------------------------------------------------

const fit = evidence.output5AdversarialProofFit;
assert.equal(
  fit.measuredHere,
  true,
  "this family is not bound by the shared Q1x artifact, so its proof fit must be measured here rather than delegated",
);
assert.ok(
  typeof fit.whyNotDelegated === "string" && fit.whyNotDelegated.length > 0,
  "output 5 must say why it is measured family-locally",
);
assert.ok(
  typeof fit.rule === "string" && fit.rule.includes("l1ByteMargin >= 0"),
  "output 5 must publish the fit rule it is checked against, in the form this gate enforces",
);

const faultProofsRequire = createRequire(
  resolve(repositoryRoot, "demo/midgard-fault-proofs/package.json"),
);
const loadDependency = async ({ specifier, hint }) => {
  let resolved = null;
  try {
    resolved = faultProofsRequire.resolve(specifier);
  } catch (error) {
    assert.fail(
      `ERR_Q39_PROOF_FIT_DEPENDENCY_UNAVAILABLE: could not resolve ${specifier} from demo/midgard-fault-proofs (${error instanceof Error ? error.message : String(error)}); ${hint}`,
    );
  }
  return import(pathToFileURL(resolved).href);
};

const SDK = await loadDependency({
  specifier: "@al-ft/midgard-sdk",
  hint: "run `pnpm --dir demo/midgard-sdk build` — the proof-fit sweep measures byte sizes through the SDK's own codec, which is a built artifact",
});
const { Trie } = await loadDependency({
  specifier: "@aiken-lang/merkle-patricia-forestry",
  hint: "run `pnpm install` in demo/ — the sweep builds a real deposits MPF rather than modelling one",
});
const { Data, PROTOCOL_PARAMETERS_DEFAULT } = await loadDependency({
  specifier: "@lucid-evolution/lucid",
  hint: "run `pnpm install` in demo/ — the L1 byte limit is read out of the same lucid build the family's package resolves",
});
const { Effect } = await loadDependency({
  specifier: "effect",
  hint: "run `pnpm install` in demo/",
});
for (const name of [
  "committedDepositKeyBytesV1",
  "committedDepositValueBytesV1",
  "commitCountedRootProgram",
  "FabricatedDepositStep01SpendRedeemerSchema",
  "FabricatedDepositStep02SpendRedeemerSchema",
  "FabricatedDepositStep03SpendRedeemerSchema",
  "FabricatedDepositStep04SpendRedeemerSchema",
  "FabricatedDepositStep01DatumSchema",
  "FabricatedDepositStep02DatumSchema",
  "FabricatedDepositStep03DatumSchema",
  "FabricatedDepositStep04DatumSchema",
]) {
  assert.ok(
    SDK[name] !== undefined,
    `ERR_Q39_PROOF_FIT_CODEC_STALE: the loaded @al-ft/midgard-sdk build does not export ${name}; run \`pnpm --dir demo/midgard-sdk build\` before publishing sizes measured through it`,
  );
}

const measuredByteLimit = PROTOCOL_PARAMETERS_DEFAULT.maxTxSize;
assert.equal(
  fit.l1ByteLimit,
  measuredByteLimit,
  "output 5's L1 byte limit is not the maxTxSize the resolved lucid build declares",
);

// The cardinality bound is read out of the Aiken source, so a change to the
// normative bound re-runs the whole sweep against the new number rather than
// leaving a stale pin.
const ledgerStateSource = withoutAikenComments(
  await readRepositoryFile(LEDGER_STATE),
);
const cardinality = fit.cardinalityBound;
assert.equal(
  cardinality.file,
  LEDGER_STATE,
  "output 5 cites a different module for the cardinality bound",
);
const boundMatch = new RegExp(
  `pub const ${cardinality.constant}: Int = ([0-9_]+)`,
  "u",
).exec(ledgerStateSource);
assert.ok(
  boundMatch !== null,
  `${LEDGER_STATE} no longer declares ${cardinality.constant}`,
);
const measuredMaxDepositCount = Number(boundMatch[1].replaceAll("_", ""));
assert.equal(
  cardinality.value,
  measuredMaxDepositCount,
  "output 5's cardinality bound is not the one ledger-state declares",
);
requireSourceMarker({
  source: ledgerStateSource,
  marker: cardinality.enforcedBy,
  where: `output 5's cardinality bound in ${LEDGER_STATE}:`,
});

// The unbounded field is measured to be unbounded: `l2_datum` is declared
// `Option<Data>` and no length bound anywhere in ledger-state mentions it. That
// absence is what residual finding Q39-F5 owns, so it is measured rather than
// asserted.
const unbounded = fit.unboundedContentCarriage;
assert.equal(
  unbounded.declarationFile,
  LEDGER_STATE,
  "output 5 cites a different module for the unbounded field's declaration",
);
requireSourceMarker({
  source: ledgerStateSource,
  marker: unbounded.declaration,
  where: `output 5's unbounded field in ${LEDGER_STATE}:`,
});
const measuredL2DatumBound =
  new RegExp(
    `max[a-z_]*${unbounded.unboundedField.split(".").at(-1)}[a-z_]*`,
    "u",
  ).exec(ledgerStateSource) ??
  new RegExp(
    `${unbounded.unboundedField.split(".").at(-1)}[^\\n]*length`,
    "u",
  ).exec(ledgerStateSource);
assert.equal(
  unbounded.normativeSizeBound,
  measuredL2DatumBound === null ? null : measuredL2DatumBound[0],
  "output 5 records whether a normative size bound exists for the unbounded field, and the measurement disagrees",
);

// The codec is anchored before it is trusted: the two Aiken-measured leaf
// constants are read out of the tracked family suite, decoded, re-encoded through
// the family's own byte helpers, and required to reproduce the same bytes.
const familySuiteSource = await readRepositoryFile(FAMILY_SUITE);
const readAnchor = (name) => {
  const match = new RegExp(`const ${name} =\\s*"([0-9a-f]+)";`, "u").exec(
    familySuiteSource,
  );
  assert.ok(
    match !== null,
    `${FAMILY_SUITE} no longer declares the Aiken-measured constant ${name}, so the sweep has nothing to anchor its codec against`,
  );
  return match[1];
};
const codecAnchoring = fit.codecAnchoring;
assert.equal(
  codecAnchoring.anchorFile,
  FAMILY_SUITE,
  "output 5's codec anchors must be read out of the tracked family suite",
);
const keyAnchor = readAnchor(codecAnchoring.keyAnchorConstant);
const valueAnchor = readAnchor(codecAnchoring.valueAnchorConstant);
const anchorDepositId = Data.from(keyAnchor, SDK.OutputReference);
const anchorDepositInfo = Data.from(valueAnchor, SDK.DepositInfo);
assert.equal(
  SDK.committedDepositKeyBytesV1(anchorDepositId),
  keyAnchor,
  `ERR_Q39_PROOF_FIT_CODEC_STALE: the loaded SDK build does not reproduce the Aiken-measured leaf key; run \`pnpm --dir demo/midgard-sdk build\``,
);
assert.equal(
  SDK.committedDepositValueBytesV1(anchorDepositInfo),
  valueAnchor,
  `ERR_Q39_PROOF_FIT_CODEC_STALE: the loaded SDK build does not reproduce the Aiken-measured leaf value; run \`pnpm --dir demo/midgard-sdk build\``,
);

const proofFitStartedAt = Date.now();
const depositIdAt = (index) => ({
  transactionId: createHash("sha256")
    .update(`q39-deposit-${String(index)}`)
    .digest("hex"),
  outputIndex: BigInt(index % 8),
});
const infoWithL2Datum = (datumBytes) =>
  datumBytes === null
    ? anchorDepositInfo
    : { ...anchorDepositInfo, l2_datum: "ab".repeat(datumBytes) };
const leafEntries = (count, replacement) => {
  const baseValue = Buffer.from(
    SDK.committedDepositValueBytesV1(anchorDepositInfo),
    "hex",
  );
  const entries = [];
  for (let index = 0; index < count; index += 1) {
    const key = Buffer.from(
      SDK.committedDepositKeyBytesV1(depositIdAt(index)),
      "hex",
    );
    entries.push({
      key,
      value:
        replacement !== undefined && replacement.key.equals(key)
          ? replacement.value
          : baseValue,
    });
  }
  return entries.sort((left, right) => Buffer.compare(left.key, right.key));
};
// Every leaf is proved and the BYTE-maximising proof is kept, not the
// step-maximising one: a branch step and a terminal step are different sizes, so
// selecting on step count publishes an optimistic worst case.
const proveEveryLeaf = async (trie, entries) => {
  let largest = null;
  let maxSteps = 0;
  for (const entry of entries) {
    const cbor = Buffer.from((await trie.prove(entry.key)).toCBOR()).toString(
      "hex",
    );
    const steps = Data.from(cbor, SDK.Proof);
    maxSteps = Math.max(maxSteps, steps.length);
    if (largest === null || cbor.length > largest.cbor.length) {
      largest = { cbor, steps, key: entry.key };
    }
  }
  return { largest, maxSteps };
};

const measuredLadder = [];
let worstCaseProof = null;
let worstCasePhasRoot = null;
for (const rung of fit.mpfLadder) {
  const entries = leafEntries(rung.committedDepositCount);
  const trie = await Trie.fromList(entries);
  const { largest, maxSteps } = await proveEveryLeaf(trie, entries);
  measuredLadder.push({
    committedDepositCount: rung.committedDepositCount,
    maxProofSteps: maxSteps,
    largestProofSteps: largest.steps.length,
    largestProofCborBytes: hexBytes(largest.cbor),
    perStepCborBytes: largest.steps.map((step) =>
      hexBytes(Data.to(step, SDK.ProofStep)),
    ),
  });
  if (rung.committedDepositCount === measuredMaxDepositCount) {
    worstCaseProof = largest;
    worstCasePhasRoot = Buffer.from(trie.hash).toString("hex");
  }
}
assert.deepEqual(
  fit.mpfLadder,
  measuredLadder,
  "output 5's published MPF ladder is not the one this sweep measured",
);
assert.ok(
  worstCaseProof !== null,
  `output 5's ladder must include a rung at the normative bound of ${String(measuredMaxDepositCount)} committed deposits, or it never measured the worst case`,
);

// The closed form is what licenses reading the byte envelope past the cardinality
// this sweep can build, so it is checked against every rung rather than stated.
const closed = fit.closedForm;
const closedFormBytes = (levels) =>
  levels === 0
    ? closed.listFramingBytes
    : (levels - 1) * closed.branchStepCborBytes +
      closed.terminalStepCborBytes +
      closed.listFramingBytes;
const closedFormAgrees = measuredLadder.every(
  (rung) =>
    closedFormBytes(rung.largestProofSteps) === rung.largestProofCborBytes,
);
assert.equal(
  closed.agreesWithLadder,
  closedFormAgrees,
  "output 5 records whether its closed form reproduces the measured ladder, and the measurement disagrees",
);
assert.equal(
  closedFormAgrees,
  true,
  "output 5's closed form does not reproduce every measured rung, so it may not be used to extrapolate the byte envelope",
);

const worst = fit.worstCase;
assert.equal(
  worst.committedDepositCount,
  measuredMaxDepositCount,
  "output 5's worst case is not measured at the normative cardinality bound",
);
assert.equal(
  worst.phasRoot,
  worstCasePhasRoot,
  "output 5's published PHAS root is not the one the worst-case tree measured",
);
const measuredCountedRoot = await Effect.runPromise(
  SDK.commitCountedRootProgram({
    domain: worst.countedRootDomain,
    phasRoot: worstCasePhasRoot,
    count: BigInt(measuredMaxDepositCount),
  }),
);
assert.equal(
  worst.countedRoot,
  measuredCountedRoot,
  "output 5's published counted root is not the one the family's own commitment helper derives from the measured PHAS root and cardinality",
);
assert.equal(
  worst.membershipProofSteps,
  worstCaseProof.steps.length,
  "output 5's published worst-case proof depth is not the measured one",
);
assert.equal(
  worst.membershipProofCborBytes,
  hexBytes(worstCaseProof.cbor),
  "output 5's published worst-case proof size is not the measured one",
);

// Fixed-width placeholders: every remaining field of the thread states and of the
// non-membership redeemer arms is a hash or key hash of a declared width, so the
// measured sizes do not depend on the values chosen here.
const placeholder = (label, bytes) =>
  createHash("sha256")
    .update(label)
    .digest("hex")
    .slice(0, bytes * 2);
const fraudProver = placeholder("q39-fraud-prover", 28);
const headerHash = placeholder("q39-challenged-header", 28);
const eventWitness = placeholder("q39-event-witness", 28);
const committedInfoHash = placeholder("q39-committed-info", 32);
const authenticInfoHash = placeholder("q39-authentic-info", 32);
const eventDatumHash = placeholder("q39-event-datum", 32);
const startTime = 10n;
const endTime = 20n;
const inclusionTime = 15n;

const encodedBytes = (value, schema) => hexBytes(Data.to(value, schema));
const buildPayloads = ({ depositId, proofSteps, l2DatumBytes }) => {
  const state02 = {
    challenged_header_hash: headerHash,
    header_start_time: startTime,
    header_end_time: endTime,
    committed_deposit_id: depositId,
    committed_deposit_info_hash: committedInfoHash,
  };
  const state03 = {
    ...state02,
    verdict: {
      DepositEventObserved: {
        event_datum_hash: eventDatumHash,
        event_inclusion_time: inclusionTime,
      },
    },
  };
  const state04 = {
    challenged_header_hash: headerHash,
    header_start_time: startTime,
    header_end_time: endTime,
    committed_deposit_id: depositId,
    fault: {
      MismatchedDepositContent: {
        committed_deposit_info_hash: committedInfoHash,
        authentic_deposit_info_hash: authenticInfoHash,
        event_inclusion_time: inclusionTime,
      },
    },
  };
  const datum = {
    step_01: encodedBytes(
      { fraud_prover: fraudProver, data: null },
      SDK.FabricatedDepositStep01DatumSchema,
    ),
    step_02: encodedBytes(
      { fraud_prover: fraudProver, data: state02 },
      SDK.FabricatedDepositStep02DatumSchema,
    ),
    step_03: encodedBytes(
      { fraud_prover: fraudProver, data: state03 },
      SDK.FabricatedDepositStep03DatumSchema,
    ),
    step_04: encodedBytes(
      { fraud_prover: fraudProver, data: state04 },
      SDK.FabricatedDepositStep04DatumSchema,
    ),
  };
  const redeemer = {
    step_01: encodedBytes(
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: 0n,
            state_queue_node_ref_input_index: 1n,
            committed_deposit: {
              domain: worst.countedRootDomain,
              root: measuredCountedRoot,
              phas_root: worstCasePhasRoot,
              count: BigInt(measuredMaxDepositCount),
              key: depositId,
              value: infoWithL2Datum(l2DatumBytes),
              proof: proofSteps,
            },
          },
        ],
      },
      SDK.FabricatedDepositStep01SpendRedeemerSchema,
    ),
    step_02: encodedBytes(
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            evidence: {
              PresentDepositEvent: {
                hub_ref_input_index: 0n,
                event_ref_input_index: 1n,
              },
            },
          },
        ],
      },
      SDK.FabricatedDepositStep02SpendRedeemerSchema,
    ),
    step_03: encodedBytes(
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            authentic_content: {
              RetainedEventDatum: {
                event_datum: {
                  event: { id: depositId, info: infoWithL2Datum(l2DatumBytes) },
                  inclusion_time: inclusionTime,
                  witness: eventWitness,
                },
              },
            },
          },
        ],
      },
      SDK.FabricatedDepositStep03SpendRedeemerSchema,
    ),
    step_04: encodedBytes(
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            fraud_proof_mint_redeemer_index: 0n,
          },
        ],
      },
      SDK.FabricatedDepositStep04SpendRedeemerSchema,
    ),
  };
  // Each step's payload is the family's own bytes that ride that step's
  // transaction: the spend redeemer, the thread datum it consumes and the thread
  // datum it produces. The terminal step produces no successor state.
  const payload = {
    step_01: redeemer.step_01 + datum.step_01 + datum.step_02,
    step_02: redeemer.step_02 + datum.step_02 + datum.step_03,
    step_03: redeemer.step_03 + datum.step_03 + datum.step_04,
    step_04: redeemer.step_04 + datum.step_04,
  };
  return { datum, redeemer, payload };
};

const worstKeyDepositId = Data.from(
  worstCaseProof.key.toString("hex"),
  SDK.OutputReference,
);
const worstCaseMeasured = buildPayloads({
  depositId: worstKeyDepositId,
  proofSteps: worstCaseProof.steps,
  l2DatumBytes: null,
});
assert.deepEqual(
  worst.redeemerCborBytes,
  worstCaseMeasured.redeemer,
  "output 5's published worst-case redeemer sizes are not the measured ones",
);
assert.deepEqual(
  worst.datumCborBytes,
  worstCaseMeasured.datum,
  "output 5's published worst-case datum sizes are not the measured ones",
);
assert.deepEqual(
  worst.stepPayloadCborBytes,
  worstCaseMeasured.payload,
  "output 5's published worst-case step payloads are not the measured ones",
);
const measuredMargins = Object.fromEntries(
  Object.entries(worstCaseMeasured.payload).map(([step, bytes]) => [
    step,
    measuredByteLimit - bytes,
  ]),
);
assert.deepEqual(
  worst.l1ByteMargin,
  measuredMargins,
  "output 5's published L1 byte margins are not the measured ones",
);
// The rule output 5 exists to enforce.
for (const [step, margin] of Object.entries(measuredMargins)) {
  assert.ok(
    margin >= 0,
    `ERR_Q39_PROOF_DOES_NOT_FIT: ${step}'s payload is ${String(worstCaseMeasured.payload[step])} bytes against an L1 limit of ${String(measuredByteLimit)} at the worst supported cardinality`,
  );
}
const measuredBindingStep = Object.entries(measuredMargins).reduce(
  (tightest, candidate) => (candidate[1] < tightest[1] ? candidate : tightest),
)[0];
assert.equal(
  worst.bindingStep,
  measuredBindingStep,
  "output 5's published binding step is not the one with the tightest measured margin",
);

// The identity-width axis: a `DepositId`'s output index is a Plutus integer, so
// the identity is not a fixed-width field. The axis is bounded and measured
// rather than ignored.
const width = fit.identifierWidthHeadroom;
assert.equal(
  width.depositIdCborBytesAtWorstCaseKey,
  encodedBytes(worstKeyDepositId, SDK.OutputReference),
  "output 5's published worst-case identity width is not the measured one",
);
const probeDepositId = {
  transactionId: worstKeyDepositId.transactionId,
  outputIndex: BigInt(width.probeOutputIndex),
};
assert.equal(
  width.depositIdCborBytesAtProbeIndex,
  encodedBytes(probeDepositId, SDK.OutputReference),
  "output 5's published probe identity width is not the measured one",
);
const probePayloads = buildPayloads({
  depositId: probeDepositId,
  proofSteps: worstCaseProof.steps,
  l2DatumBytes: null,
});
assert.equal(
  width.bindingStepPayloadBytesAtProbeIndex,
  probePayloads.payload[measuredBindingStep],
  "output 5's published binding-step payload at the probe identity width is not the measured one",
);
assert.equal(
  width.bindingStepPayloadDeltaBytes,
  probePayloads.payload[measuredBindingStep] -
    worstCaseMeasured.payload[measuredBindingStep],
  "output 5's published identity-width headroom is not the measured delta",
);

// Value-size invariance: the two adversarial axes are measured independent rather
// than assumed independent.
const invariance = fit.valueSizeInvariance;
const swollenValue = Buffer.from(
  SDK.committedDepositValueBytesV1(
    infoWithL2Datum(invariance.swollenL2DatumBytes),
  ),
  "hex",
);
const swollenTrie = await Trie.fromList(
  leafEntries(measuredMaxDepositCount, {
    key: worstCaseProof.key,
    value: swollenValue,
  }),
);
const swollenProofBytes = hexBytes(
  Buffer.from((await swollenTrie.prove(worstCaseProof.key)).toCBOR()).toString(
    "hex",
  ),
);
assert.equal(
  invariance.membershipProofCborBytes,
  swollenProofBytes,
  "output 5's published proof size under a swollen leaf value is not the measured one",
);
assert.equal(
  invariance.proofBytesUnchanged,
  swollenProofBytes === hexBytes(worstCaseProof.cbor),
  "output 5 records whether the proof size is independent of the leaf value's size, and the measurement disagrees",
);
assert.equal(
  invariance.phasRootChanged,
  Buffer.from(swollenTrie.hash).toString("hex") !== worstCasePhasRoot,
  "output 5 records whether swelling the challenged leaf changes the PHAS root, and the measurement disagrees",
);

// The depth envelope: how many levels the byte envelope admits, against how many
// the normative cardinality realizes.
const depth = fit.depthEnvelope;
const measuredFixedNonProof =
  worstCaseMeasured.payload[measuredBindingStep] -
  hexBytes(worstCaseProof.cbor);
assert.equal(
  depth.fixedNonProofPayloadBytes,
  measuredFixedNonProof,
  "output 5's published fixed non-proof payload is not the measured one",
);
let measuredLevelCeiling = 0;
while (
  measuredFixedNonProof + closedFormBytes(measuredLevelCeiling + 1) <=
  measuredByteLimit
) {
  measuredLevelCeiling += 1;
}
assert.equal(
  depth.proofLevelCeiling,
  measuredLevelCeiling,
  "output 5's published proof-level ceiling is not the one the closed form and the byte envelope yield",
);
assert.equal(
  depth.payloadAtCeiling,
  measuredFixedNonProof + closedFormBytes(measuredLevelCeiling),
  "output 5's published payload at the ceiling is not the derived one",
);
assert.equal(
  depth.payloadAtCeilingPlusOne,
  measuredFixedNonProof + closedFormBytes(measuredLevelCeiling + 1),
  "output 5's published payload one level past the ceiling is not the derived one",
);
assert.ok(
  depth.payloadAtCeilingPlusOne > measuredByteLimit,
  "output 5's ceiling is not a ceiling: one more level still fits the byte envelope",
);
// An MPF key is a nibble radix, so forcing one more level of branching means
// grinding four more bits of collision in the hashed key space.
assert.equal(
  depth.log2WorkToForceCeiling,
  measuredLevelCeiling * 4,
  "output 5's published grinding cost is not four bits per forced level of the nibble radix",
);
assert.equal(
  depth.realizedLevelsAtCardinalityBound,
  worstCaseProof.steps.length,
  "output 5's published realized depth is not the measured worst-case depth",
);
assert.ok(
  depth.realizedLevelsAtCardinalityBound < measuredLevelCeiling,
  "output 5 claims the cardinality axis does not bind, but the realized depth reaches the byte envelope's ceiling",
);

// The axis that DOES bind: the unbounded content, carried once each by two
// disjoint steps and never by a handoff.
assert.equal(
  unbounded.handoffsCarryOnlyCommitments,
  true,
  "output 5 must record that the thread handoffs carry commitments rather than the unbounded content",
);
const largestFitting = (measure) => {
  let low = 0;
  let high = 1;
  while (measure(high) <= measuredByteLimit) {
    low = high;
    high *= 2;
  }
  while (high - low > 1) {
    const middle = (low + high) >> 1;
    if (measure(middle) <= measuredByteLimit) {
      low = middle;
    } else {
      high = middle;
    }
  }
  return low;
};
const payloadWithL2Datum = (step) => (datumBytes) =>
  buildPayloads({
    depositId: worstKeyDepositId,
    proofSteps: worstCaseProof.steps,
    l2DatumBytes: datumBytes,
  }).payload[step];
const measuredLargestCommitted = largestFitting(payloadWithL2Datum("step_01"));
const measuredLargestOpened = largestFitting(payloadWithL2Datum("step_03"));
assert.equal(
  unbounded.largestCommittedL2DatumBytes,
  measuredLargestCommitted,
  "output 5's published ceiling on the committed unbounded field is not the measured one",
);
assert.equal(
  unbounded.largestOpenedL2DatumBytes,
  measuredLargestOpened,
  "output 5's published ceiling on the opened unbounded field is not the measured one",
);
assert.equal(
  unbounded.bindingStep,
  measuredLargestCommitted <= measuredLargestOpened ? "step_01" : "step_03",
  "output 5's published binding step for the content axis is not the tighter of the two measured ceilings",
);
assert.deepEqual(
  [...unbounded.carryingSteps, ...unbounded.commitmentOnlySteps].sort(),
  ["step_01", "step_02", "step_03", "step_04"],
  "output 5 must account for all four steps as either carrying the unbounded content or carrying only its commitment",
);
// The two steps that carry it must be the two whose payload actually grows with
// it; the other two must not grow at all.
for (const step of ["step_01", "step_02", "step_03", "step_04"]) {
  const grows = payloadWithL2Datum(step)(1024) > payloadWithL2Datum(step)(1);
  assert.equal(
    unbounded.carryingSteps.includes(step),
    grows,
    `output 5 records ${step} as ${unbounded.carryingSteps.includes(step) ? "carrying" : "not carrying"} the unbounded content, but its payload ${grows ? "grows" : "does not grow"} with that field`,
  );
}

const execution = fit.executionEnvelope;
assert.equal(
  execution.measuredAtAdversarialDepth,
  false,
  "output 5 may not claim an adversarial execution measurement while the blueprint carries no compiled code for this family",
);
assert.ok(
  typeof execution.whyNotMeasured === "string" &&
    execution.whyNotMeasured.length > 0,
  "output 5 must say why its execution axis is unmeasured rather than leave the gap silent",
);
const proofFitWallSeconds = (Date.now() - proofFitStartedAt) / 1000;

// ---------------------------------------------------------------------------
// Output 6 — catalogue identifier and deployed identity, MEASURED
// ---------------------------------------------------------------------------

const identity = evidence.output6CatalogueAndDeployedIdentity;
assert.equal(
  identity.blueprint,
  BLUEPRINT,
  "output 6 cites a different blueprint",
);
// The blueprint is a build product. Its untracked status is measured from the
// ignore rule rather than asserted, because that is what makes its regeneration
// owner-gated rather than a working-tree accident.
const aikenIgnore = await readRepositoryFile("onchain/aiken/.gitignore");
assert.equal(
  identity.blueprintIsTracked,
  !aikenIgnore.split(/\r?\n/u).some((line) => line.trim() === "plutus.json"),
  "output 6 records whether the blueprint is tracked, and the ignore rule disagrees",
);
let blueprint = null;
try {
  blueprint = JSON.parse(await readRepositoryFile(BLUEPRINT));
} catch (error) {
  assert.fail(
    `ERR_Q39_BLUEPRINT_UNREADABLE: ${BLUEPRINT} could not be read (${error instanceof Error ? error.message : String(error)}); this gate measures the family's ABSENCE from it, which is meaningless without a real blueprint — build it with \`cd onchain/aiken && $MIDGARD_AIKEN_BIN build\``,
  );
}
const blueprintTitles = new Set(
  blueprint.validators.map((validator) => validator.title),
);
assert.equal(
  identity.measuredBlueprintValidatorCount,
  blueprint.validators.length,
  "output 6's published blueprint size is not the measured one",
);
// The control: a title the blueprint DOES carry, so "the family is missing" can
// never be satisfied by a failed or empty read.
assert.ok(
  blueprintTitles.has(identity.controlTitle),
  `ERR_Q39_BLUEPRINT_CONTROL_ABSENT: ${BLUEPRINT} does not carry the control title ${identity.controlTitle}, so this family's measured absence from it proves nothing`,
);
// The family's own titles are DERIVED from its emulator suite's declaration
// rather than restated here, so the two cannot drift.
assert.equal(
  identity.titleDeclarationFile,
  EMULATOR_SUITE,
  "output 6 must derive the family's blueprint titles from the suite that asserts their absence, so the two cannot drift apart",
);
const emulatorSource = await readRepositoryFile(EMULATOR_SUITE);
const titleListOpener = `${identity.titleDeclaration} = [`;
const titleListStart = emulatorSource.indexOf(titleListOpener);
assert.ok(
  titleListStart >= 0,
  `${EMULATOR_SUITE} no longer declares ${identity.titleDeclaration}`,
);
const titleListEnd = emulatorSource.indexOf("\n]", titleListStart);
const declaredBlueprintTitles = [
  ...emulatorSource
    .slice(titleListStart + titleListOpener.length, titleListEnd)
    .matchAll(/"([^"]+)"/gu),
].map((match) => match[1]);
assert.deepEqual(
  identity.titles,
  declaredBlueprintTitles,
  `output 6's published title list is not the one ${EMULATOR_SUITE} declares`,
);
assert.equal(
  identity.measuredTitleCount,
  declaredBlueprintTitles.length,
  "output 6's published title count is not the length of its own title list",
);
const presentFamilyTitles = declaredBlueprintTitles.filter((title) =>
  blueprintTitles.has(title),
);
assert.equal(
  identity.measuredPresentTitleCount,
  presentFamilyTitles.length,
  "output 6's published count of blueprint-present family titles is not the measured one",
);
assert.equal(
  identity.catalogueRegistered,
  catalogueRegistered,
  "output 6's registration claim disagrees with the append-only order measured above",
);
assert.equal(
  identity.firstStepTitle,
  declaredBlueprintTitles[0],
  "output 6's first-step title is not the first of its own title list",
);
requireSourceMarker({
  source: await readRepositoryFile(identity.contractsType.file),
  marker: identity.contractsType.declaration,
  where: `output 6's contracts type in ${identity.contractsType.file}:`,
});
// The status is DERIVED: a deployed identity exists only when every one of the
// family's titles is in the blueprint AND its category is registered.
const deployedIdentityAvailable =
  presentFamilyTitles.length === declaredBlueprintTitles.length &&
  catalogueRegistered;

// ---------------------------------------------------------------------------
// Output 7 — DA-first evidence builder
// ---------------------------------------------------------------------------

const builder = evidence.output7DaFirstEvidenceBuilder;
const builderSource = await readRepositoryFile(builder.builderFile);
for (const name of builder.builders) {
  requireSourceMarker({
    source: builderSource,
    marker: `export const ${name} = `,
    where: `output 7's builder in ${builder.builderFile}:`,
  });
}
assert.equal(
  builder.measuredBuilderCount,
  builder.builders.length,
  "output 7's published builder count is not the number of builders it names",
);
requireSourceMarker({
  source: builderSource,
  marker: `export type ${builder.rejectionCodeType} =`,
  where: `output 7's rejection code union in ${builder.builderFile}:`,
});
assert.ok(
  typeof builder.retainedMaterialRoute === "string" &&
    builder.retainedMaterialRoute.includes(builder.builders[0]),
  "output 7 must name the retained-DA route by the builder that performs it",
);
declareVitest(FAMILY_SUITE, [
  ...builder.measuredAdmissionTitles,
  ...builder.measuredPlanTitles,
]);

// ---------------------------------------------------------------------------
// Output 8 — one resumable prepare/submit chain
// ---------------------------------------------------------------------------

const chain = evidence.output8CommandChain;
const cliSource = await readRepositoryFile(chain.cliFile);
// The verb token has to be FAMILY-SCOPED, and that is checked rather than
// trusted. A bare `fabricated` would also be satisfied by a sibling
// `fabricated-*` family's wiring landing in the same file, which would close this
// cell for a family whose own verbs are still absent — the exact failure mode a
// derived status is meant to rule out.
assert.ok(
  new RegExp(chain.cliVerbTokenPattern, "iu").test(FAMILY),
  `output 8's verb token ${JSON.stringify(chain.cliVerbTokenPattern)} does not identify this family, so the count it produces is not this family's`,
);
assert.ok(
  !new RegExp(`^${chain.cliVerbTokenPattern}$`, "iu").test(
    FAMILY.split("-")[0],
  ),
  `output 8's verb token ${JSON.stringify(chain.cliVerbTokenPattern)} matches this family's class word on its own, so a sibling ${FAMILY.split("-")[0]}-* family's CLI wiring would close this cell`,
);
const measuredCliVerbCount = [
  ...cliSource.matchAll(new RegExp(chain.cliVerbTokenPattern, "giu")),
].length;
assert.equal(
  chain.measuredCliVerbCount,
  measuredCliVerbCount,
  "output 8's published CLI verb count is not the measured one",
);
// The control: a settled sibling's verb must be present in the same read, so this
// family's absence is a property of the file rather than of a failed read.
requireSourceMarker({
  source: cliSource,
  marker: chain.controlVerb,
  where: `output 8's CLI control verb in ${chain.cliFile}:`,
});
let measuredResumableSteps = 0;
for (const stage of chain.moduleChain) {
  const source = await readRepositoryFile(stage.file);
  requireSourceMarker({
    source,
    marker: `export const ${stage.entryPoint} = `,
    where: `output 8's chain stage ${stage.step} in ${stage.file}:`,
  });
  const resultType = /^export type Submit[A-Za-z0-9]*Result = \{[^}]*\}/mu.exec(
    source,
  );
  if (stage.handoff === undefined) {
    continue;
  }
  assert.ok(
    resultType !== null,
    `${stage.file} declares no submit result type, so its handoff cannot be measured`,
  );
  assert.ok(
    resultType[0].includes(`readonly ${stage.handoff}:`),
    `${stage.file}'s submit result does not carry the published handoff ${stage.handoff}`,
  );
  if (stage.handoff === "nextThreadOutRef") {
    measuredResumableSteps += 1;
  } else {
    // The terminal step must NOT carry a successor handoff: a chain whose last
    // step still hands a thread forward has not finalized anything.
    assert.ok(
      !resultType[0].includes("readonly nextThreadOutRef:"),
      `${stage.file} is the terminal step but still returns a nextThreadOutRef`,
    );
  }
}
assert.equal(
  chain.measuredChainLength,
  chain.moduleChain.length,
  "output 8's published chain length is not the number of stages it names",
);
assert.equal(
  chain.measuredResumableSteps,
  measuredResumableSteps,
  "output 8's published resumable-step count is not the measured one",
);
assert.equal(
  chain.terminalStepReturnsHandoff,
  false,
  "output 8 must record that the terminal step returns no successor handoff",
);
declareVitest(FAMILY_SUITE, chain.measuredSubmitSideTitles);

// The family suite's inventory must be EXACTLY the titles outputs 1, 7 and 8
// cite between them, so a test added there and left uncited sits outside no
// count.
const familyDeclaration = declaredVitestFiles.get(FAMILY_SUITE);
assert.deepEqual(
  declaredTitles(familySuiteSource).slice().sort(),
  familyDeclaration.titles.slice().sort(),
  `${FAMILY_SUITE} declares a different set of tests than outputs 1, 7 and 8 cite between them`,
);

// ---------------------------------------------------------------------------
// Output 9 — emulator lifecycle, measured through a purpose-built derivation
// ---------------------------------------------------------------------------

const lifecycle = evidence.output9EmulatorLifecycle;
assert.equal(
  lifecycle.suite,
  EMULATOR_SUITE,
  "output 9 cites a different emulator suite",
);
requireSourceMarker({
  source: emulatorSource,
  marker: `describe(${JSON.stringify(lifecycle.describeTitle)}`,
  where: `output 9's describe block in ${EMULATOR_SUITE}:`,
});
const emulatorTitles = declaredTitles(emulatorSource);
assert.deepEqual(
  emulatorTitles,
  [lifecycle.blockTitle],
  `${EMULATOR_SUITE} declares a different set of blocks than output 9 cites`,
);
assert.equal(
  lifecycle.measuredDeclaredBlocks,
  emulatorTitles.length,
  "output 9's published declared-block count is not the measured one",
);
// Markers are required inside the block's OWN body, so a marker that moved into a
// helper or a sibling block cannot satisfy this cell.
const blockStart = emulatorSource.indexOf(
  `it(${JSON.stringify(lifecycle.blockTitle)}`,
);
assert.ok(
  blockStart >= 0,
  `${EMULATOR_SUITE} no longer declares its lifecycle block`,
);
const blockBody = emulatorSource.slice(blockStart);
for (const marker of lifecycle.blockMarkers) {
  requireSourceMarker({
    source: blockBody,
    marker,
    where: `output 9's block marker in ${EMULATOR_SUITE}:`,
  });
}
assert.equal(
  lifecycle.measuredBlockMarkerCount,
  lifecycle.blockMarkers.length,
  "output 9's published marker count is not the number of markers it requires",
);
for (const reason of lifecycle.skipReasonContains) {
  requireSourceMarker({
    source: blockBody,
    marker: reason,
    where: `output 9's skip reason in ${EMULATOR_SUITE}:`,
  });
}
assert.ok(
  Array.isArray(lifecycle.measuredOffChainStages) &&
    lifecycle.measuredOffChainStages.length > 0,
  "output 9 must name what the block DOES measure, or its boundary reads as an absence of coverage",
);
assert.ok(
  typeof lifecycle.derivationNote === "string" &&
    lifecycle.derivationNote.includes("skipped"),
  "output 9 must state why it needs a derivation the shared helper cannot supply",
);

// The purpose-built derivation. The shared one refuses a skipped test outright,
// which is right for a suite claiming passage; this suite claims a boundary, so
// the skip is the measurement — but it must be EXACTLY one skip, with no failure
// and no silent pass.
const deriveBoundaryOutcome = ({ label, report, status, requiredTitle }) => {
  const files = Array.isArray(report.testResults) ? report.testResults : [];
  assert.ok(
    files.length > 0,
    `ERR_Q39_EMULATOR_NO_FILES: ${label}: the declared suite matched no test file`,
  );
  const assertions = files.flatMap((file) =>
    Array.isArray(file.assertionResults) ? file.assertionResults : [],
  );
  assert.equal(
    assertions.length,
    1,
    `ERR_Q39_EMULATOR_UNEXPECTED_COLLECTION: ${label}: expected exactly one collected block, measured ${String(assertions.length)}`,
  );
  const [only] = assertions;
  assert.equal(
    only.status,
    "skipped",
    `ERR_Q39_EMULATOR_NOT_A_BOUNDARY: ${label}: the block's measured status is ${String(only.status)}; a block that now passes has crossed the frozen-blueprint boundary and this cell must be re-derived as LOCAL_PASS`,
  );
  const named = only.fullName ?? only.title ?? "";
  assert.ok(
    named.includes(requiredTitle),
    `ERR_Q39_EMULATOR_TITLE_NOT_COLLECTED: ${label}: the runner collected ${JSON.stringify(named)} rather than the cited block`,
  );
  assert.equal(
    report.numFailedTests,
    0,
    `ERR_Q39_EMULATOR_FAILED: ${label}: the suite reported ${String(report.numFailedTests)} failures`,
  );
  assert.equal(
    report.numPassedTests,
    0,
    `ERR_Q39_EMULATOR_UNEXPECTED_PASS: ${label}: the suite reported a passing block where it must stop at the boundary`,
  );
  assert.equal(
    report.numPendingTests,
    1,
    `ERR_Q39_EMULATOR_PENDING_MISMATCH: ${label}: expected exactly one pending block, measured ${String(report.numPendingTests)}`,
  );
  assert.equal(
    status,
    0,
    `ERR_Q39_EMULATOR_NONZERO_EXIT: ${label}: a deliberate boundary must still exit 0, measured ${String(status)}`,
  );
  return {
    collected: assertions.length,
    passed: report.numPassedTests,
    failed: report.numFailedTests,
    skipped: report.numPendingTests,
    exitStatus: status,
  };
};
const emulatorDeclaration = {
  file: EMULATOR_SUITE,
  packageDirectory: EMULATOR_SUITE.split("/").slice(0, 2).join("/"),
  testFile: EMULATOR_SUITE.split("/").slice(2).join("/"),
};

// ---------------------------------------------------------------------------
// Output 10 — the parent-owned matrix rows, MEASURED
// ---------------------------------------------------------------------------

// Output 10 is not asserted, it is derived. Both files are parent-owned — the
// manifest's Q39 row does not list them — so this gate reads their cells through
// each table's own header and refuses to edit them.
const matrices = evidence.output10MatrixRows;
assert.equal(
  matrices.owner,
  "parent",
  "the coverage matrices are parent-owned",
);
const measureMatrixRow = async ({ file, line, rowKey, statusColumn }) => {
  const lines = (await readRepositoryFile(file)).split(/\r?\n/u);
  // Only table rows are searched, so a mention of the family in prose above the
  // table can never be mistaken for its row.
  const rowIndex = lines.findIndex(
    (candidate) => candidate.startsWith("|") && candidate.includes(rowKey),
  );
  assert.ok(
    rowIndex >= 0,
    `${file} no longer carries a table row for ${rowKey}`,
  );
  assert.equal(
    rowIndex + 1,
    line,
    `${file}: the row for ${rowKey} is at line ${String(rowIndex + 1)}, not the published ${String(line)}`,
  );
  let delimiterIndex = rowIndex - 1;
  while (delimiterIndex >= 0 && !TABLE_DELIMITER.test(lines[delimiterIndex])) {
    delimiterIndex -= 1;
  }
  assert.ok(
    delimiterIndex > 0,
    `${file}: no table delimiter row could be resolved above the row for ${rowKey}, so its columns cannot be addressed`,
  );
  const headerCells = tableCells(lines[delimiterIndex - 1]);
  assert.ok(
    headerCells.length === tableCells(lines[rowIndex]).length,
    `${file}: the resolved header for ${rowKey} has ${String(headerCells.length)} cells against the row's ${String(tableCells(lines[rowIndex]).length)}, so column N of the header does not address column N of the row`,
  );
  if (statusColumn === null) {
    assert.ok(
      !headerCells.includes("Status"),
      `${file}: the table above ${rowKey} does carry a Status column, so the published null is wrong`,
    );
    return { headerCells, statusCell: null };
  }
  return {
    headerCells,
    statusCell: tableCells(lines[rowIndex])[
      columnIndex({ file, headerCells, header: statusColumn })
    ],
  };
};
let matrixRowsMeasured = 0;
for (const row of matrices.rows) {
  const measured = await measureMatrixRow(row);
  assert.equal(
    row.measuredStatusCell,
    measured.statusCell,
    `${row.file}: the published Status cell for ${row.rowKey} is not the one the file carries`,
  );
  if (Array.isArray(row.measuredTableHeader)) {
    assert.deepEqual(
      measured.headerCells.filter((cell) => cell.length > 0),
      row.measuredTableHeader,
      `${row.file}: the published table header for ${row.rowKey} is not the measured one`,
    );
  }
  matrixRowsMeasured += 1;
}
assert.equal(
  matrices.measuredRowCount,
  matrixRowsMeasured,
  "output 10's published row count is not the number of rows it measures",
);
// The positive control: a settled sibling row is measured through the same
// machinery, so "this family's cells do not record its closure" is a measurement
// rather than a parsing failure — and the pinned form the pending edits must take
// is read out of the tree rather than invented.
const control = matrices.positiveControl;
const measuredControl = await measureMatrixRow(control);
assert.equal(
  control.measuredStatusCell,
  measuredControl.statusCell,
  `${control.file}: the settled sibling row's Status cell is not the published one, so the pinned form these edits must take cannot be read out of the tree`,
);
assert.ok(
  measuredControl.statusCell.includes("LOCAL_PASS"),
  `${control.file}: the control row does not record a LOCAL_PASS, so it cannot show what a recorded closure looks like`,
);
// Derived: this family's cells record its closure only when both carry its goal
// id and a local status.
const matricesRecordLocalPass = matrices.rows.every(
  (row) =>
    typeof row.measuredStatusCell === "string" &&
    row.measuredStatusCell.includes(GOAL_ID) &&
    row.measuredStatusCell.includes("LOCAL_PASS"),
);
assert.equal(
  matrices.recordsLocalPass,
  matricesRecordLocalPass,
  "output 10 records whether the matrices carry this family's closure, and the measurement disagrees",
);

// ---------------------------------------------------------------------------
// The owned executable inventory, DERIVED from the parent-owned manifest row
// ---------------------------------------------------------------------------

const inventory = evidence.ownedExecutableInventory;
assert.equal(
  inventory.manifest,
  MANIFEST,
  "the inventory cites a different manifest",
);
const manifest = JSON.parse(await readRepositoryFile(MANIFEST));
const manifestRow = (manifest.tasks ?? manifest.rows ?? []).find(
  (task) => (task.goalId ?? task.id) === inventory.manifestRow,
);
assert.ok(
  manifestRow !== undefined,
  `${MANIFEST} carries no ${inventory.manifestRow} row, so the owned inventory cannot be derived from it`,
);
const manifestWritable = manifestRow.writablePaths ?? [];
assert.deepEqual(
  inventory.surfaces.filter((surface) => manifestWritable.includes(surface)),
  inventory.surfaces,
  `${MANIFEST}: the ${inventory.manifestRow} row does not list both closure surfaces as writable`,
);
assert.deepEqual(
  manifestWritable
    .filter((path) => !inventory.surfaces.includes(path))
    .slice()
    .sort(),
  inventory.paths.slice().sort(),
  `the published owned inventory is not the ${inventory.manifestRow} row's writable set minus the two closure surfaces`,
);
assert.equal(
  inventory.measuredPathCount,
  inventory.paths.length,
  "the published inventory count is not the length of its own path list",
);
for (const path of inventory.paths) {
  const source = await readRepositoryFile(path);
  assert.ok(
    source.length > 0,
    `${path} is in the owned inventory but reads empty, so nothing about it was exercised`,
  );
}

// ---------------------------------------------------------------------------
// Execute. One batched `aiken check -e` under the pinned fork, the fork's own
// formatter over the eight family modules, then each cited suite once by its own
// package's Vitest CLI. Every count published above is read out of these reports.
// ---------------------------------------------------------------------------

const aikenStartedAt = Date.now();
const aikenRun = runAikenCheck({
  projectRoot: aikenProjectRoot,
  selectors: aikenSelectors,
  binary: aikenBinaryPath,
});
const aikenOutcome = deriveAikenOutcome({
  label: `${GOAL_ID} ${FAMILY} on-chain selectors`,
  declared: declaredAiken,
  ...aikenRun,
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

// The compiler reports the declaration form back as `on_failure` and the cost of
// each run as `execution_units`. Both are re-measured against the census: the
// disposition is the second, independent source for every selector's role, and
// the units are the family's measured on-chain cost under the pinned compiler.
let peakExecutionUnits = { module: null, selector: null, mem: 0, cpu: 0 };
let measuredSelectorReports = 0;
for (const module of aikenRun.report.modules ?? []) {
  for (const test of module.tests ?? []) {
    const expectation = selectorExpectations.get(test.title);
    if (expectation === undefined) {
      continue;
    }
    measuredSelectorReports += 1;
    assert.equal(
      test.on_failure,
      expectation.onFailure,
      `${test.title}: the compiler reports on_failure ${String(test.on_failure)}, which contradicts the artifact's published disposition ${expectation.onFailure}`,
    );
    assert.deepEqual(
      { mem: test.execution_units?.mem, cpu: test.execution_units?.cpu },
      { mem: expectation.mem, cpu: expectation.cpu },
      `${test.title}: the published execution units are not the ones ${measuredCompiler} measured`,
    );
    if (test.execution_units.mem > peakExecutionUnits.mem) {
      peakExecutionUnits = {
        module: expectation.module,
        selector: test.title,
        mem: test.execution_units.mem,
        cpu: test.execution_units.cpu,
      };
    }
  }
}
assert.equal(
  measuredSelectorReports,
  declaredAiken.length,
  "the compiler's report does not carry a per-selector entry for every declared selector",
);
assert.deepEqual(
  onchain.peakExecutionUnits,
  peakExecutionUnits,
  "the published peak execution units are not the measured peak",
);
assert.deepEqual(
  fit.executionEnvelope.measuredAtFixtureDepth,
  {
    selector: peakExecutionUnits.selector,
    mem: peakExecutionUnits.mem,
    cpu: peakExecutionUnits.cpu,
  },
  "output 5's fixture-depth execution figure is not the measured peak of the family's own handler runs",
);

const formatRun = spawnSync(process.execPath, [FORMAT_SCRIPT, ...formatFiles], {
  cwd: aikenProjectRoot,
  encoding: "utf8",
  maxBuffer: 16 * 1024 * 1024,
  env: { ...process.env, MIDGARD_AIKEN_BIN: aikenBinaryPath },
});
assert.equal(
  formatRun.status,
  0,
  `ERR_Q39_FORMAT_NOT_NORMALIZED: the family's eight Aiken modules are not at the pinned fork's normalized format — ${formatRun.stdout ?? ""}${formatRun.stderr ?? ""}`,
);
assert.equal(
  formatGate.measured,
  (formatRun.stdout ?? "").trim(),
  "output 4's published format-gate result is not the one the normalizer reported",
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
const emulatorStartedAt = Date.now();
const emulatorOutcome = deriveBoundaryOutcome({
  label: `${GOAL_ID} ${FAMILY} ${EMULATOR_SUITE}`,
  requiredTitle: lifecycle.blockTitle,
  ...runVitest({
    packageRoot: resolve(repositoryRoot, emulatorDeclaration.packageDirectory),
    testFile: emulatorDeclaration.testFile,
  }),
});
const emulatorWallSeconds = (Date.now() - emulatorStartedAt) / 1000;
if (emulatorWallSeconds > slowestVitestSuite.seconds) {
  slowestVitestSuite = { file: EMULATOR_SUITE, seconds: emulatorWallSeconds };
}
assert.deepEqual(
  lifecycle.measured,
  emulatorOutcome,
  "output 9's published emulator measurement is not the one the runner produced",
);

// The published commands are recomputed from what this gate just executed, so a
// runner list that drifted from the runs is a failure rather than a stale line.
assert.deepEqual(
  evidence.runners,
  [
    aikenCommand,
    formatCommand,
    ...[...declaredVitestFiles.values()].map((declaration) =>
      vitestPublishedCommand({
        packageDirectory: declaration.packageDirectory,
        testFile: declaration.testFile,
      }),
    ),
    vitestPublishedCommand({
      packageDirectory: emulatorDeclaration.packageDirectory,
      testFile: emulatorDeclaration.testFile,
    }),
  ],
  "published runner commands drifted from the commands this gate executes",
);

assertRecordedMeasurement({
  label: `output 2 ${SDK_TWIN_SUITE}`,
  recorded: codec.byteTwinMeasured,
  outcome: vitestOutcomes.get(SDK_TWIN_SUITE),
});
assertRecordedMeasurement({
  label: `output 2 ${FAMILY_SUITE}`,
  recorded: codec.familySuiteMeasured,
  outcome: vitestOutcomes.get(FAMILY_SUITE),
});

const measuredFailures =
  aikenOutcome.collected -
  aikenOutcome.passed +
  [...vitestOutcomes.values()].reduce(
    (total, outcome) => total + (outcome.collected - outcome.passed),
    0,
  ) +
  emulatorOutcome.failed;
assert.equal(
  inventory.measuredFailures,
  measuredFailures,
  "the published inventory failure count is not the sum the runners reported",
);

// ---------------------------------------------------------------------------
// Output status matrix — every status DERIVED from the measurement above
// ---------------------------------------------------------------------------

assert.deepEqual(
  evidence.outputStatus.map((row) => row.output),
  OUTPUTS,
  "the artifact must publish exactly the ten 9.1 outputs, in order",
);
assert.deepEqual(
  evidence.notApplicableOutputs,
  [],
  "no output of the 9.1 contract is N/A for this family",
);
assert.ok(
  typeof evidence.notApplicableNote === "string" &&
    evidence.notApplicableNote.length > 0,
  "the artifact must say why no output is N/A, rather than leaving an empty list unexplained",
);

// Outputs 1 to 5 and 7 are LOCAL_PASS because every claim behind them was
// re-derived above; a failure there aborted this gate before here. Outputs 6, 8,
// 9 and 10 are derived from the four measurements of the surfaces they name, so
// the #617 wave and the parent's matrix edits close them without an edit to the
// artifact.
const DERIVED_STATUS = {
  1: "LOCAL_PASS",
  2: "LOCAL_PASS",
  3: "LOCAL_PASS",
  4: "LOCAL_PASS",
  5: "LOCAL_PASS",
  6: deployedIdentityAvailable ? "LOCAL_PASS" : "OPEN",
  7: "LOCAL_PASS",
  8: measuredCliVerbCount > 0 ? "LOCAL_PASS" : "OPEN",
  9: emulatorOutcome.passed > 0 ? "LOCAL_PASS" : "OPEN",
  10: matricesRecordLocalPass ? "LOCAL_PASS" : "OPEN",
};
const OPEN_OWNERS = new Set(["#617", "parent"]);
let localPassOutputs = 0;
let openOutputs = 0;
for (const row of evidence.outputStatus) {
  const where = `output ${String(row.output)}`;
  assert.ok(
    typeof row.title === "string" && row.title.length > 0,
    `${where} must carry the 9.1 contract's own title for the output`,
  );
  assert.ok(
    typeof row.evidence === "string" && row.evidence.length > 0,
    `${where} must state the evidence behind its status`,
  );
  assert.equal(
    row.status,
    DERIVED_STATUS[row.output],
    `${where} records ${String(row.status)} but this gate measured ${DERIVED_STATUS[row.output]}; a cell that closed while the artifact still records OPEN is as wrong as one that claims a closure it does not have`,
  );
  if (row.status === "LOCAL_PASS") {
    localPassOutputs += 1;
    assert.equal(
      row.whyOpen,
      undefined,
      `${where} is LOCAL_PASS and may not also carry a whyOpen rider`,
    );
  } else {
    openOutputs += 1;
    assert.ok(
      typeof row.whyOpen === "string" && row.whyOpen.length > 0,
      `${where} is OPEN and must say why`,
    );
    assert.ok(
      OPEN_OWNERS.has(row.owner),
      `${where} is OPEN and must be owned by the parent or by the integrating issue, measured ${String(row.owner)}`,
    );
  }
}
assert.equal(
  localPassOutputs + openOutputs,
  OUTPUTS.length,
  "the status split does not account for every output exactly once",
);

// ---------------------------------------------------------------------------
// Residual findings: claimed ownership, checked both ways
// ---------------------------------------------------------------------------

const SEVERITIES = new Set(["soundness-adjacent", "gap", "observation"]);
const ownedClaims = new Map();
for (const finding of evidence.residualFindings) {
  assert.match(
    finding.id,
    new RegExp(`^${GOAL_ID}-F\\d+$`, "u"),
    `residual finding ${String(finding.id)} does not carry this family's identifier scheme`,
  );
  assert.ok(
    SEVERITIES.has(finding.severity),
    `residual finding ${finding.id} carries unknown severity ${String(finding.severity)}`,
  );
  assert.ok(
    typeof finding.finding === "string" && finding.finding.length > 0,
    `residual finding ${finding.id} must state the finding`,
  );
  assert.ok(
    typeof finding.owner === "string" && finding.owner.length > 0,
    `residual finding ${finding.id} must name an owner`,
  );
  assert.ok(
    Array.isArray(finding.owns) && finding.owns.length > 0,
    `residual finding ${finding.id} must claim what it owns, or nothing binds it to a measurement`,
  );
  for (const claim of finding.owns) {
    // Anti-vacuity: a finding may only own what it actually discusses.
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

// Every honest limit this family carries must be named rather than absorbed into
// a LOCAL_PASS.
if (!matricesRecordLocalPass) {
  for (const row of matrices.rows) {
    requireOwned(
      row.file,
      `output 10 is OPEN on ${row.file} and that pendency must be owned by a residual finding`,
    );
  }
}
if (!deployedIdentityAvailable) {
  requireOwned(
    BLUEPRINT,
    "outputs 6 and 9 are OPEN because the frozen blueprint carries no compiled code for this family, and that must be owned by a residual finding",
  );
}
if (!catalogueRegistered) {
  requireOwned(
    CATALOGUE,
    "this family's category id is a reserved append slot rather than a registered index, and that disposition must be owned by a residual finding rather than left as an unexplained absence",
  );
}
if (measuredCliVerbCount === 0) {
  requireOwned(
    chain.cliFile,
    `output 8 is OPEN because ${chain.cliFile} carries no verb for this family, and that must be owned by a residual finding`,
  );
}
// The measured exposure: the field that binds output 5's real axis has no
// normative bound, so the ceiling this gate measures is reachable by choosing one
// field's size. A gate that published the margin without owning this would be
// publishing a fit that a block producer can defeat.
if (unbounded.normativeSizeBound === null) {
  requireOwned(
    unbounded.unboundedField.split(".").at(-1),
    "the field that binds this family's proof fit is measured to carry no normative size bound, and that exposure must be owned by a residual finding rather than absorbed into output 5",
  );
  requireOwned(
    "largestCommittedL2DatumBytes",
    "the measured ceiling on the committed unbounded field is the number that exposure turns on, and it must be owned by a residual finding",
  );
}
if (!execution.measuredAtAdversarialDepth) {
  requireOwned(
    "executionEnvelope",
    "output 5's execution axis is unmeasured at adversarial depth and that limit must be owned by a residual finding, so a reader of the byte margin is not misled into reading it as a whole-envelope fit",
  );
}
// The sweep measures through a build product, which is a real dependency and must
// be stated as one.
requireOwned(
  "demo/midgard-sdk/dist",
  "output 5's sizes are measured through the SDK's built output, and that dependency must be owned by a residual finding",
);
const sdkIgnore = await readRepositoryFile("demo/midgard-sdk/.gitignore");
assert.ok(
  sdkIgnore.split(/\r?\n/u).some((line) => line.trim() === "dist"),
  "the residual finding for the SDK build asserts it is untracked; the ignore rule no longer says so",
);
// The relationship to the shared artifact runs one way only, and saying so is
// what keeps a reader from taking this family's proof fit as delegated.
requireOwned(
  SHARED_ARTIFACT,
  "this family delegates nothing to the shared artifact and that must be stated, so no cell here is read as resting on its numbers",
);
requireOwned(
  MANIFEST,
  "the manifest row for this family is parent-owned and does not anticipate its four OPEN cells; that divergence must be owned by a residual finding",
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
  notApplicable: evidence.notApplicableOutputs.length,
  delegatedOutputs: 0,
  aikenModules: onchain.modules.length,
  aikenSelectorsDeclared: declaredAiken.length,
  aikenSelectorsPassed: aikenOutcome.passed,
  aikenPositiveSelectors: positiveSelectors,
  aikenValidBlockNegativeSelectors: validBlockNegativeSelectors,
  aikenAdditionalNegativeSelectors: additionalNegativeSelectors,
  aikenStepScopedSelectors: stepScopedSelectors,
  aikenModuleHelperSelectors: moduleHelperSelectors,
  // Derived from the sweep across all eight family-owned modules, not asserted:
  // this family opens none of the nine committed fields.
  committedFieldsOpened: binding.openedCommittedFields.length,
  codecRecordsCompared: codec.records.length,
  codecFieldsCompared: measuredFieldCount,
  codecEnumsCompared: codec.enums.length,
  // The emulator suite is counted as executed but contributes no passing test:
  // it is a boundary, and folding its skip into the pass count is exactly the
  // defect the existence-versus-passage discipline exists to prevent.
  vitestSuitesExecuted: vitestOutcomes.size + 1,
  vitestTestsPassed,
  vitestTitlesRequired,
  emulatorBlocksDeclared: emulatorOutcome.collected,
  emulatorBlocksSkipped: emulatorOutcome.skipped,
  emulatorBlocksPassed: emulatorOutcome.passed,
  proofFitLadderRungs: measuredLadder.length,
  proofFitWorstCasePayloadBytes: worstCaseMeasured.payload[measuredBindingStep],
  proofFitWorstCaseMarginBytes: measuredMargins[measuredBindingStep],
  proofFitProofLevelCeiling: measuredLevelCeiling,
  ownedExecutablePaths: inventory.paths.length,
  ownedExecutableFailures: measuredFailures,
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
  "matrix, manifest, ledger and workflow integration is parent-owned",
);
assert.ok(
  evidence.parentIntegration.pendingEdits.length > 0,
  "the parent-owned edits this artifact supports must be listed explicitly",
);
for (const file of [
  ...matrices.rows.map((row) => row.file),
  "GOAL_PROGRESS.md",
]) {
  assert.ok(
    evidence.parentIntegration.pendingEdits.some((edit) => edit.includes(file)),
    `the parent-owned edit to ${file} must be listed, so the row this artifact supports is not left to be inferred`,
  );
}

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
  // The discipline output 9 has to step around deliberately: a skipped test is a
  // declaration, not a result. The shared derivation must keep refusing it, or
  // this gate's purpose-built boundary derivation would be the only thing between
  // a silently skipped suite and a published pass.
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
  // The compiler leg: an unavailable binary must fail closed rather than falling
  // through to whatever `aiken` happens to be first on PATH — which on this tree
  // is the retired unsound build.
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
  gate: GATE,
  artifact: ARTIFACT,
  compiler: measuredCompiler,
  outputs: evidence.summary.outputs,
  localPass: evidence.summary.localPass,
  open: evidence.summary.open,
  notApplicable: evidence.summary.notApplicable,
  delegatedOutputs: evidence.summary.delegatedOutputs,
  openOutputs: evidence.outputStatus
    .filter((row) => row.status === "OPEN")
    .map((row) => ({ output: row.output, owner: row.owner })),
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
  aikenPeakExecutionUnits: peakExecutionUnits,
  aikenWallSeconds,
  normalizedFormat: (formatRun.stdout ?? "").trim(),
  committedFieldsOpened: evidence.summary.committedFieldsOpened,
  committedFieldAbsenceTokensSwept: sweep.absentTokens.length,
  codecRecordsCompared: evidence.summary.codecRecordsCompared,
  codecFieldsCompared: evidence.summary.codecFieldsCompared,
  codecEnumsCompared: evidence.summary.codecEnumsCompared,
  vitestSuitesExecuted: evidence.summary.vitestSuitesExecuted,
  vitestTestsPassed: evidence.summary.vitestTestsPassed,
  vitestTitlesRequired: evidence.summary.vitestTitlesRequired,
  slowestVitestSuite,
  emulatorBlocksDeclared: evidence.summary.emulatorBlocksDeclared,
  emulatorBlocksSkipped: evidence.summary.emulatorBlocksSkipped,
  emulatorBlocksPassed: evidence.summary.emulatorBlocksPassed,
  proofFit: {
    l1ByteLimit: measuredByteLimit,
    maxDepositCount: measuredMaxDepositCount,
    ladderRungs: measuredLadder.length,
    worstCaseProofSteps: worstCaseProof.steps.length,
    worstCaseProofCborBytes: hexBytes(worstCaseProof.cbor),
    bindingStep: measuredBindingStep,
    bindingStepPayloadBytes: worstCaseMeasured.payload[measuredBindingStep],
    bindingStepMarginBytes: measuredMargins[measuredBindingStep],
    proofLevelCeiling: measuredLevelCeiling,
    log2WorkToForceCeiling: measuredLevelCeiling * 4,
    largestCommittedL2DatumBytes: measuredLargestCommitted,
    largestOpenedL2DatumBytes: measuredLargestOpened,
    identityWidthHeadroomBytes: width.bindingStepPayloadDeltaBytes,
    wallSeconds: proofFitWallSeconds,
  },
  blueprintValidators: blueprint.validators.length,
  blueprintFamilyTitlesPresent: presentFamilyTitles.length,
  blueprintFamilyTitlesDeclared: declaredBlueprintTitles.length,
  catalogueRegistered,
  catalogueOrderLength: catalogueOrder.length,
  derivedCategoryId,
  cliVerbsForFamily: measuredCliVerbCount,
  resumableSubmitSteps: measuredResumableSteps,
  matrixRowsMeasured,
  matricesRecordLocalPass,
  ownedExecutablePaths: evidence.summary.ownedExecutablePaths,
  ownedExecutableFailures: evidence.summary.ownedExecutableFailures,
  residualFindings: evidence.summary.residualFindings,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `${GOAL_ID} ${FAMILY} outputs 1-10: PASS (${String(evidence.summary.localPass)} LOCAL_PASS, ${String(
      evidence.summary.open,
    )} OPEN, ${String(evidence.summary.notApplicable)} N/A, nothing delegated; ${String(
      evidence.summary.aikenSelectorsPassed,
    )}/${String(evidence.summary.aikenSelectorsDeclared)} on-chain selectors across ${String(
      evidence.summary.aikenModules,
    )} step modules under ${measuredCompiler} (${String(
      evidence.summary.aikenPositiveSelectors,
    )} positive, ${String(
      evidence.summary.aikenValidBlockNegativeSelectors,
    )} valid-block negative, ${String(
      evidence.summary.aikenAdditionalNegativeSelectors,
    )} further negative, all ${String(
      evidence.summary.aikenStepScopedSelectors,
    )} step-scoped, peak ${String(peakExecutionUnits.mem)} mem / ${String(
      peakExecutionUnits.cpu,
    )} cpu on ${String(peakExecutionUnits.selector)}), ${(
      formatRun.stdout ?? ""
    ).trim()}, ${String(evidence.summary.codecFieldsCompared)} codec fields and ${String(
      evidence.summary.codecEnumsCompared,
    )} positionally-bound enums compared across the boundary, ${String(
      evidence.summary.vitestTestsPassed,
    )} tests over ${String(
      evidence.summary.vitestSuitesExecuted,
    )} suites with ${String(
      evidence.summary.vitestTitlesRequired,
    )} required titles, ${String(
      evidence.summary.emulatorBlocksSkipped,
    )} emulator block stopping at the frozen blueprint, ${String(
      evidence.summary.committedFieldsOpened,
    )} of 9 committed fields opened across ${String(
      sweep.measuredModuleCount,
    )} swept modules, adversarial proof fit at ${String(
      measuredMaxDepositCount,
    )} committed deposits: ${String(
      hexBytes(worstCaseProof.cbor),
    )}-byte membership proof over ${String(
      worstCaseProof.steps.length,
    )} levels, ${measuredBindingStep} payload ${String(
      worstCaseMeasured.payload[measuredBindingStep],
    )}/${String(measuredByteLimit)} bytes for a ${String(
      measuredMargins[measuredBindingStep],
    )}-byte margin, depth ceiling ${String(
      measuredLevelCeiling,
    )} levels (~2^${String(
      measuredLevelCeiling * 4,
    )} work) and the binding content ceiling ${String(
      measuredLargestCommitted,
    )} bytes; deployed identity OPEN with ${String(
      presentFamilyTitles.length,
    )}/${String(declaredBlueprintTitles.length)} titles in a ${String(
      blueprint.validators.length,
    )}-validator blueprint and category ${derivedCategoryId} reserved at index ${String(
      derivedCategoryIndex,
    )} of an unregistered order, CLI OPEN with ${String(
      measuredCliVerbCount,
    )} verbs beside ${String(
      measuredResumableSteps,
    )} resumable module steps, output 10 OPEN on ${String(
      matrixRowsMeasured,
    )} parent-owned matrix rows, ${String(
      evidence.summary.ownedExecutablePaths,
    )} owned paths with ${String(
      evidence.summary.ownedExecutableFailures,
    )} failures, ${String(evidence.summary.residualFindings)} residual findings)`,
  );
}
