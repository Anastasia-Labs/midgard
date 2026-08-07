#!/usr/bin/env node

// Verifies the Q62 (decision row D-DA4) non-retroactive DA committee rotation
// artifact against the final tree.
//
// The defect Q62 closes had two sites, and they fail in different ways, so this
// gate measures them differently.
//
//   1. `ApplyToStateQueue` read no DA params at all. It decided quorum from the
//      attestation datum's own frozen `da_threshold` and burnt the DAAT, so a
//      quorum gathered under a committee governance had since replaced stayed
//      applicable forever. That is behaviour, and behaviour is settled by
//      running the handler: one control transaction that must still apply, and
//      five one-field mutations that must not.
//
//   2. `get_da_params` took `committee_signers_hash` on the datum's word. That
//      is not behaviour anyone can exhibit with a single transaction — it is a
//      property of the source: the hash must be re-derived, exactly once, on
//      every path that reads the params. A test can show that *a* forged datum
//      is refused; only a structural measurement can show there is no second,
//      unguarded way to obtain params. This gate therefore recomputes both, and
//      publishes them as separately named numbers rather than one blended count.
//
// Every published count is recomputed from an executed runner report or
// recomputed from source here. Nothing is read from the artifact and echoed
// back: the artifact declares a plan, and this gate decides whether the plan
// was met.
//
// `REQUIRED_GROUPS` below is held by the verifier, not by the artifact. An
// acceptance clause may not be dropped, shrunk, or replaced by an invented one.
//
// Note on the seeded evidence fixtures at the bottom: they mutate `groups`,
// which is never empty. The sibling Q63 gate seeds its equivalents against an
// `openGroups` array instead, and those three fixtures become silent no-ops the
// moment that clause is completed and the array empties — a gate that cannot
// go green. Mutating the always-populated array avoids that failure mode.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { runFixtureMode } from "./lib/runner-fixtures.mjs";
import {
  aikenCompilerVersion,
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
// code. It must run before any artifact is read. The fixture package root only
// supplies a Vitest CLI for the seeded synthetic projects; it is deliberately
// not the package this gate verifies.
runFixtureMode({
  argv: process.argv.slice(2),
  packageRoot: resolve(repositoryRoot, "demo/midgard-validation"),
});

const emitJson = process.argv.slice(2).includes("--json");

const readRepositoryFile = async (relativePath) =>
  readFile(resolve(repositoryRoot, relativePath), "utf8");

const VALIDATOR_PATH = "onchain/aiken/validators/da-attestation.ak";
const TYPES_PATH = "onchain/aiken/lib/midgard/da-attestation-types.ak";

// ---------------------------------------------------------------------------
// Q62's acceptance shape, held here rather than read from the artifact.
// ---------------------------------------------------------------------------

const REQUIRED_GROUPS = [
  {
    id: "unchanged-committee-apply-control",
    expected: 1,
    languages: ["aiken"],
  },
  { id: "rotation-mutation-rejections", expected: 5, languages: ["aiken"] },
  // Q62 adds a *second* frozen value to the apply gate (`da_threshold`), and so
  // a second way for an attestation to become unappliable. Requiring this group
  // is what stops the rescue path from covering only the first: without it, a
  // threshold-only governance update would leave an attestation that can never
  // apply and never be rescued, with its ADA locked for good.
  { id: "strandedness-completeness", expected: 1, languages: ["aiken"] },
  { id: "burn-redeemer-cross-binding", expected: 3, languages: ["aiken"] },
  { id: "rescue-refund-value-binding", expected: 1, languages: ["aiken"] },
  { id: "offchain-rotation-abi", expected: 6, languages: ["vitest"] },
];

const requiredGroup = (id) =>
  REQUIRED_GROUPS.find((candidate) => candidate.id === id);

// ---------------------------------------------------------------------------
// Structural measurement — "the committee hash is re-derived exactly once per
// params read".
//
// Three separable facts, each with its own diagnostic and its own seeded
// mutation, because a single blended check could pass for the wrong reason:
//
//   (a) production code re-derives the hash exactly once;
//   (b) that one site is inside `get_da_params`;
//   (c) `get_da_params` is the only place a `DaParamsDatum` is decoded, so
//       there is no second route to params that would bypass (b).
//
// Only (a) and (b) together give "exactly once"; only (c) gives "per params
// read". The region measured stops at the fixture sentinel, since fixtures
// legitimately call `blake2b_256` to build params.
// ---------------------------------------------------------------------------

const FIXTURE_SENTINEL = "// midgard:test-fixtures-below";

const countOccurrences = (haystack, needle) =>
  haystack.split(needle).length - 1;

const measureRederivation = (source) => {
  const sentinelIndex = source.indexOf(FIXTURE_SENTINEL);
  if (sentinelIndex === -1) {
    throw new Error(
      `ERR_FIXTURE_SENTINEL_MISSING: ${VALIDATOR_PATH} no longer carries the "${FIXTURE_SENTINEL}" sentinel, so this gate cannot tell production code from fixture code and would measure the re-derivation over a region that legitimately hashes committees`,
    );
  }
  const production = source.slice(0, sentinelIndex);

  const declaration = production.indexOf("fn get_da_params(");
  if (declaration === -1) {
    throw new Error(
      `ERR_PARAMS_READER_MISSING: ${VALIDATOR_PATH} no longer declares get_da_params, which is the single authenticated route to the DA params`,
    );
  }
  const afterDeclaration = production.slice(declaration);
  const bodyEnd = afterDeclaration.indexOf("\n}\n");
  if (bodyEnd === -1) {
    throw new Error(
      `ERR_PARAMS_READER_MISSING: get_da_params has no closing brace in ${VALIDATOR_PATH}`,
    );
  }
  const readerBody = afterDeclaration.slice(0, bodyEnd);

  const productionRederivations = countOccurrences(production, "blake2b_256(");
  const readerRederivations = countOccurrences(readerBody, "blake2b_256(");

  if (productionRederivations === 0) {
    throw new Error(
      `ERR_REDERIVATION_ABSENT: no committee-hash re-derivation remains in ${VALIDATOR_PATH}; without it every comparison against committee_signers_hash trusts a field nothing recomputes, which is the W-C15 defect Q62 closes`,
    );
  }
  if (productionRederivations !== 1) {
    throw new Error(
      `ERR_REDERIVATION_NOT_UNIQUE: ${VALIDATOR_PATH} re-derives the committee hash at ${String(productionRederivations)} production sites; the claim Q62 publishes is that it is re-derived exactly once per params read, and a second site means some path pays for it twice or a different site guards a different route`,
    );
  }
  if (readerRederivations !== 1) {
    throw new Error(
      `ERR_REDERIVATION_OUTSIDE_PARAMS_READ: the single committee-hash re-derivation in ${VALIDATOR_PATH} is not inside get_da_params, so a caller that reads the params by any other route would not be covered by it`,
    );
  }

  // (c): the only decode of a params datum. `get_da_params` authenticates the
  // UTxO before decoding, so a second decode elsewhere would be a params read
  // that skipped both the NFT check and the re-derivation.
  const productionDecodes = countOccurrences(production, ": DaParamsDatum =");
  const readerDecodes = countOccurrences(readerBody, ": DaParamsDatum =");
  if (productionDecodes !== 1 || readerDecodes !== 1) {
    throw new Error(
      `ERR_PARAMS_DECODE_NOT_UNIQUE: ${VALIDATOR_PATH} decodes a DaParamsDatum at ${String(productionDecodes)} production site(s), ${String(readerDecodes)} of them inside get_da_params; a decode outside the authenticated reader is a params read that bypasses both the governance NFT check and the committee-hash re-derivation`,
    );
  }

  // Call sites, excluding the declaration itself. Published, not bounded: the
  // number grows whenever a redeemer starts reading params, and the point of
  // the claim is that each such site is covered, not that there are N of them.
  const paramsReadCallSites =
    countOccurrences(production, "get_da_params(") - 1;

  return {
    rederivationSites: productionRederivations,
    rederivationSitesInsideParamsReader: readerRederivations,
    paramsDecodeSites: productionDecodes,
    paramsReadCallSites,
  };
};

// The ABI fact D-DA4 turns on: apply must be able to reach the current params.
const measureApplyRedeemer = (typesSource) => {
  const applyBody = /ApplyToStateQueue \{([^}]*)\}/su.exec(typesSource)?.[1];
  if (applyBody === undefined) {
    throw new Error(
      `ERR_APPLY_REDEEMER_MISSING: ${TYPES_PATH} no longer declares an ApplyToStateQueue constructor`,
    );
  }
  if (!applyBody.includes("da_params_ref_input_index: Int")) {
    throw new Error(
      `ERR_APPLY_REDEEMER_UNGOVERNED: ApplyToStateQueue carries no da_params_ref_input_index, so the apply handler has no way to reach the current DA params — the exact shape in which committee rotation was not retroactive`,
    );
  }
  return true;
};

// ---------------------------------------------------------------------------
// Seeded source mutations. These prove the structural measurement above can
// fail; without them it is an assurance, not a check.
// ---------------------------------------------------------------------------

const sourceFixtures = {
  intact: (source) => source,
  "missing-sentinel": (source) =>
    source.replace(FIXTURE_SENTINEL, "// removed"),
  "absent-rederivation": (source) =>
    source.replace(
      "  expect blake2b_256(datum.committee) == datum.committee_signers_hash\n",
      "",
    ),
  "duplicated-rederivation": (source) =>
    source.replace(
      "  expect blake2b_256(datum.committee) == datum.committee_signers_hash\n",
      '  expect blake2b_256(datum.committee) == datum.committee_signers_hash\n  expect blake2b_256(datum.committee) != #"00"\n',
    ),
  "rederivation-outside-params-read": (source) =>
    source
      .replace(
        "  expect blake2b_256(datum.committee) == datum.committee_signers_hash\n",
        "",
      )
      .replace(
        "fn attestation_message(header_hash: HeaderHash) -> ByteArray {\n",
        'fn attestation_message(header_hash: HeaderHash) -> ByteArray {\n  expect blake2b_256(header_hash) != #"00"\n',
      ),
  "second-params-decode": (source) =>
    source.replace(
      "fn attestation_message(header_hash: HeaderHash) -> ByteArray {\n",
      "fn attestation_message(header_hash: HeaderHash) -> ByteArray {\n  expect _shadow: DaParamsDatum = builtin.i_data(0)\n",
    ),
};

const sourceFixtureArgument = process.argv
  .slice(2)
  .find((argument) => argument.startsWith("--source-fixture="));
if (sourceFixtureArgument !== undefined) {
  const fixtureName = sourceFixtureArgument.slice("--source-fixture=".length);
  const mutate = sourceFixtures[fixtureName];
  try {
    if (mutate === undefined) {
      throw new Error(`unknown source fixture ${fixtureName}`);
    }
    const measurement = measureRederivation(
      mutate(await readRepositoryFile(VALIDATOR_PATH)),
    );
    process.stdout.write(
      `source fixture ${fixtureName}: ${JSON.stringify(measurement)}\n`,
    );
    process.exit(0);
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exit(1);
  }
}

// ---------------------------------------------------------------------------
// Artifact.
// ---------------------------------------------------------------------------

const evidence = JSON.parse(
  await readRepositoryFile(
    "docs/exec-plans/evidence/canonical-v1-q62-da-rotation-v1.json",
  ),
);

assert.equal(
  evidence.schema,
  "midgard.canonical-v1-q62-da-rotation.v1",
  "unexpected Q62 evidence schema",
);
assert.equal(evidence.version, 1);
assert.deepEqual(evidence.goalIds, ["Q62"]);
assert.equal(evidence.decisionRow, "D-DA4");

const assertCitedTitlesUnique = (candidate) => {
  const titles = [];
  const seen = new Set();
  for (const group of candidate.groups ?? []) {
    for (const item of group.evidence ?? []) {
      if (item.kind !== "vitest") {
        continue;
      }
      for (const title of item.titles ?? []) {
        if (seen.has(title)) {
          throw new Error(
            `ERR_DUPLICATE_VITEST_CITATION: the artifact cites the Vitest title "${title}" more than once, so one executed test would be counted for two claims`,
          );
        }
        seen.add(title);
        titles.push(title);
      }
    }
  }
  for (const title of titles) {
    for (const other of titles) {
      if (title !== other && other.includes(title)) {
        throw new Error(
          `ERR_AMBIGUOUS_VITEST_CITATION: the cited title "${title}" is a substring of the cited title "${other}", so under the substring-tolerant runner match one executed test could be counted for both citations`,
        );
      }
    }
  }
};

const assertRequiredGroups = (candidate) => {
  const seenIds = new Set();
  for (const group of candidate.groups ?? []) {
    if (seenIds.has(group.id)) {
      throw new Error(
        `ERR_DUPLICATE_GROUP_ID: ${group.id} is declared more than once in groups; indexing by id would silently keep only the last, leaving the earlier declaration's cardinality unchecked`,
      );
    }
    seenIds.add(group.id);
  }
  const measured = new Map(
    (candidate.groups ?? []).map((group) => [group.id, group]),
  );
  for (const required of REQUIRED_GROUPS) {
    const group = measured.get(required.id);
    if (group === undefined) {
      throw new Error(
        `ERR_REQUIRED_GROUP_MISSING: Q62 acceptance requires the group ${required.id}, which the artifact does not publish; dropping a clause is how an unmet one would read as a green gate`,
      );
    }
    if (group.expected !== required.expected) {
      throw new Error(
        `ERR_REQUIRED_GROUP_CARDINALITY: ${required.id} claims ${String(group.expected)} check(s) but Q62 acceptance requires ${String(required.expected)}`,
      );
    }
  }
  const requiredIds = new Set(REQUIRED_GROUPS.map(({ id }) => id));
  for (const group of candidate.groups ?? []) {
    if (!requiredIds.has(group.id)) {
      throw new Error(
        `ERR_UNDECLARED_GROUP: ${group.id} is published but is not one of Q62's required acceptance groups, so nothing fixes what it owes`,
      );
    }
  }
  return measured;
};

const assertArtifactInvariants = (candidate) => {
  const measured = assertRequiredGroups(candidate);
  assertCitedTitlesUnique(candidate);
  return measured;
};

const evidenceFixtures = {
  intact: (artifact) => artifact,
  "dropped-group": (artifact) => ({
    ...artifact,
    groups: artifact.groups.filter(
      (group) => group.id !== "rotation-mutation-rejections",
    ),
  }),
  "shrunken-group": (artifact) => ({
    ...artifact,
    groups: artifact.groups.map((group) =>
      group.id === "rotation-mutation-rejections"
        ? { ...group, expected: 1 }
        : group,
    ),
  }),
  "invented-group": (artifact) => ({
    ...artifact,
    groups: [
      ...artifact.groups,
      { id: "invented-extra-group", expected: 1, disposition: "PASS" },
    ],
  }),
  "duplicate-group-id": (artifact) => ({
    ...artifact,
    groups: [...artifact.groups, structuredClone(artifact.groups[0])],
  }),
  "duplicate-vitest-citation": (artifact) => {
    const groups = structuredClone(artifact.groups);
    const item = groups
      .flatMap((group) => group.evidence)
      .find((evidenceItem) => evidenceItem.kind === "vitest");
    item.titles = [...item.titles, item.titles[0]];
    return { ...artifact, groups };
  },
  "substring-vitest-citation": (artifact) => {
    const groups = structuredClone(artifact.groups);
    const item = groups
      .flatMap((group) => group.evidence)
      .find((evidenceItem) => evidenceItem.kind === "vitest");
    item.titles = [...item.titles, item.titles[0].slice(0, 12)];
    return { ...artifact, groups };
  },
};

const evidenceFixtureArgument = process.argv
  .slice(2)
  .find((argument) => argument.startsWith("--evidence-fixture="));
if (evidenceFixtureArgument !== undefined) {
  const fixtureName = evidenceFixtureArgument.slice(
    "--evidence-fixture=".length,
  );
  const mutate = evidenceFixtures[fixtureName];
  try {
    if (mutate === undefined) {
      throw new Error(`unknown evidence fixture ${fixtureName}`);
    }
    assertArtifactInvariants(mutate(structuredClone(evidence)));
    process.stdout.write(
      `evidence fixture ${fixtureName}: artifact invariants accepted\n`,
    );
    process.exit(0);
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exit(1);
  }
}

const measuredGroups = assertArtifactInvariants(evidence);

// ---------------------------------------------------------------------------
// Structural half, recomputed from the real source.
// ---------------------------------------------------------------------------

const validatorSource = await readRepositoryFile(VALIDATOR_PATH);
const rederivation = measureRederivation(validatorSource);
measureApplyRedeemer(await readRepositoryFile(TYPES_PATH));

assert.deepEqual(
  evidence.committeeHashRederivation,
  rederivation,
  "the recorded committee-hash re-derivation measurement disagrees with the source",
);

// ---------------------------------------------------------------------------
// Phase 1 — collect what the artifact declares.
// ---------------------------------------------------------------------------

const declaredAikenChecks = [];
const declaredVitestFiles = new Map();
const groupPlans = new Map();

for (const group of evidence.groups) {
  const required = requiredGroup(group.id);
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
  assert.deepEqual(
    group.languages,
    required.languages,
    `group ${group.id} must declare the language(s) Q62 requires of it`,
  );
  assert.ok(
    typeof group.languageRationale === "string" &&
      group.languageRationale.length > 0,
    `group ${group.id} must say why it carries only ${required.languages.join(", ")}`,
  );

  const plan = { id: group.id, aikenSelectors: [], vitestTitles: [] };
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
        plan.aikenSelectors.push(selector);
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
      plan.vitestTitles.push(...item.titles);
    } else {
      throw new Error(
        `unknown evidence kind ${String(item.kind)} in group ${group.id}`,
      );
    }
  }

  for (const [language, cited] of [
    ["aiken", plan.aikenSelectors.length],
    ["vitest", plan.vitestTitles.length],
  ]) {
    const expected = required.languages.includes(language)
      ? required.expected
      : 0;
    assert.equal(
      cited,
      expected,
      `group ${group.id} cites ${String(cited)} ${language} check(s) but Q62 acceptance requires ${String(expected)}`,
    );
  }
  groupPlans.set(group.id, plan);
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

// Aiken test execution for this goal is fork-only: the stock v1.1.22 build that
// is the authority for compilation and applied validator hashes ships a codegen
// defect, so it is used only for `aiken check --skip-tests`. Resolving the
// compiler by name rather than by PATH default is what lets this gate publish
// *which* build produced its on-chain result.
const aikenBinaryVariable = [
  "MIDGARD_AIKEN_BIN",
  "MIDGARD_FORK_AIKEN_BIN",
].find(
  (name) =>
    typeof process.env[name] === "string" && process.env[name].length > 0,
);
assert.ok(
  aikenBinaryVariable !== undefined,
  "ERR_AIKEN_BINARY_UNPINNED: neither MIDGARD_AIKEN_BIN nor MIDGARD_FORK_AIKEN_BIN names the patched Aiken fork — Q62's on-chain measurement is fork-only, and leaving both unset would run whatever `aiken` is first on PATH while still publishing the result as Q62's",
);
const aikenBinaryPath = process.env[aikenBinaryVariable];
assert.deepEqual(
  evidence.compiler.environmentVariables,
  ["MIDGARD_AIKEN_BIN", "MIDGARD_FORK_AIKEN_BIN"],
  "the artifact must record the compiler variables this gate accepts",
);

const FORK_VERSION_PREFIX = "aiken v1.1.23";
const aikenCompiler = aikenCompilerVersion(aikenBinaryPath);
assert.ok(
  aikenCompiler.startsWith(FORK_VERSION_PREFIX),
  `ERR_AIKEN_COMPILER_MISMATCH: ${aikenBinaryVariable}=${aikenBinaryPath} reports "${aikenCompiler}", which is not the patched fork Q62's on-chain measurement requires (expected a ${FORK_VERSION_PREFIX} build)`,
);

const aikenOutcome = deriveAikenOutcome({
  label: "Q62 DA rotation on-chain selectors",
  declared: declaredAikenChecks,
  ...runAikenCheck({
    projectRoot: aikenProjectRoot,
    selectors: aikenSelectors,
    binary: aikenBinaryPath,
  }),
});
assert.equal(
  aikenOutcome.passed,
  declaredAikenChecks.length,
  "every cited on-chain selector must be measured as passing",
);
const measuredAikenSelectors = new Set(
  aikenOutcome.measured.map(({ selector }) => selector),
);

const measuredVitestTitles = new Set();
for (const declaration of declaredVitestFiles.values()) {
  const run = runVitest({
    packageRoot: resolve(repositoryRoot, declaration.packageDirectory),
    testFile: declaration.testFile,
  });
  deriveVitestOutcome({
    label: `Q62 DA rotation ${declaration.file}`,
    requiredTitles: declaration.titles,
    ...run,
  });

  const passedTitles = new Set();
  for (const file of run.report.testResults ?? []) {
    for (const assertion of file.assertionResults ?? []) {
      if (assertion.status !== "passed") {
        continue;
      }
      for (const name of [assertion.title, assertion.fullName]) {
        if (typeof name === "string") {
          passedTitles.add(name);
        }
      }
    }
  }
  for (const title of declaration.titles) {
    const matched =
      passedTitles.has(title) ||
      [...passedTitles].some((name) => name.includes(title));
    assert.ok(
      matched,
      `Q62 DA rotation ${declaration.file}: the runner did not report the cited title as passing — ${title}`,
    );
    measuredVitestTitles.add(title);
  }
}

// ---------------------------------------------------------------------------
// Recompute every published number.
// ---------------------------------------------------------------------------

const groupTotals = {};
for (const required of REQUIRED_GROUPS) {
  const plan = groupPlans.get(required.id);
  const totals = {};
  for (const language of required.languages) {
    const count =
      language === "aiken"
        ? plan.aikenSelectors.filter((selector) =>
            measuredAikenSelectors.has(selector),
          ).length
        : plan.vitestTitles.filter((title) => measuredVitestTitles.has(title))
            .length;
    assert.equal(
      count,
      required.expected,
      `group ${required.id}: the runner measured ${String(count)} passing ${language} check(s), but Q62 acceptance requires ${String(required.expected)}`,
    );
    totals[language] = count;
  }
  groupTotals[required.id] = totals;
}

const vitestChecksExecuted = measuredVitestTitles.size;
const executedChecks = aikenOutcome.passed + vitestChecksExecuted;

const recomputedSummary = {
  measuredGroups: REQUIRED_GROUPS.filter(({ id }) => measuredGroups.has(id))
    .length,
  groupTotals,
  aikenCompiler,
  aikenChecksExecuted: aikenOutcome.passed,
  vitestChecksExecuted,
  executedChecks,
  runners: [
    aikenPublishedCommand({
      projectDirectory: aikenProjectDirectory,
      selectors: aikenSelectors,
      command: "$MIDGARD_AIKEN_BIN",
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

assert.equal(
  evidence.acceptanceComplete,
  REQUIRED_GROUPS.every(({ id }) => measuredGroups.has(id)),
  "acceptanceComplete must agree with whether every required acceptance group has been measured",
);

// ---------------------------------------------------------------------------
// Declared, not measured.
// ---------------------------------------------------------------------------

const declared = evidence.declared;
assert.deepEqual(
  declared.structural.standaloneCatalogueIds,
  [],
  "Q62 must not introduce a standalone catalogue ID",
);
assert.equal(declared.parentIntegration.owner, "parent");
assert.ok(
  declared.parentIntegration.pendingEdits.length > 0,
  "the parent handoff must list the edits it owns",
);

// ---------------------------------------------------------------------------
// Negative self-tests.
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
  // The acceptance-shape seeds. Each mutates `groups`, which is never empty.
  [
    "--evidence-fixture=dropped-group",
    /ERR_REQUIRED_GROUP_MISSING: .*rotation-mutation-rejections/su,
  ],
  [
    "--evidence-fixture=shrunken-group",
    /ERR_REQUIRED_GROUP_CARDINALITY: .*rotation-mutation-rejections/su,
  ],
  [
    "--evidence-fixture=invented-group",
    /ERR_UNDECLARED_GROUP: .*invented-extra-group/su,
  ],
  [
    "--evidence-fixture=duplicate-group-id",
    /ERR_DUPLICATE_GROUP_ID: .*declared more than once in groups/su,
  ],
  [
    "--evidence-fixture=duplicate-vitest-citation",
    /ERR_DUPLICATE_VITEST_CITATION: .*counted for two claims/su,
  ],
  [
    "--evidence-fixture=substring-vitest-citation",
    /ERR_AMBIGUOUS_VITEST_CITATION: .*is a substring of the cited title/su,
  ],
  // The structural seeds. Without these the re-derivation measurement would be
  // an assurance about source rather than a check that can fail.
  [
    "--source-fixture=missing-sentinel",
    /ERR_FIXTURE_SENTINEL_MISSING: .*production code from fixture code/su,
  ],
  [
    "--source-fixture=absent-rederivation",
    /ERR_REDERIVATION_ABSENT: .*W-C15 defect/su,
  ],
  [
    "--source-fixture=duplicated-rederivation",
    /ERR_REDERIVATION_NOT_UNIQUE: .*exactly once per params read/su,
  ],
  [
    "--source-fixture=rederivation-outside-params-read",
    /ERR_REDERIVATION_OUTSIDE_PARAMS_READ: .*any other route/su,
  ],
  [
    "--source-fixture=second-params-decode",
    /ERR_PARAMS_DECODE_NOT_UNIQUE: .*bypasses both the governance NFT check/su,
  ],
];
for (const [flag, expectedDiagnostic] of selfTests) {
  const selfTest = runSelfTest(flag);
  assert.notEqual(
    selfTest.status,
    0,
    `Q62 DA rotation gate accepted the seeded defect ${flag}`,
  );
  assert.match(
    selfTest.stderr,
    expectedDiagnostic,
    `Q62 DA rotation gate rejected ${flag} without its specific diagnostic`,
  );
}
for (const [flag, expectedStdout] of [
  ["--vitest-fixture=passing", /vitest fixture passing: 1\/1 passed/u],
  ["--aiken-fixture=passing", /aiken fixture passing: 1\/1 passed/u],
  [
    "--evidence-fixture=intact",
    /evidence fixture intact: artifact invariants accepted/u,
  ],
  [
    "--source-fixture=intact",
    /source fixture intact: .*"rederivationSites":1/u,
  ],
]) {
  const control = runSelfTest(flag);
  assert.equal(
    control.status,
    0,
    `Q62 DA rotation gate rejected a passing fixture (${flag}): ${control.stderr}`,
  );
  assert.match(control.stdout, expectedStdout);
}

// Both compiler claims are seeded rather than asserted, since each is exactly
// the kind of fail-closed assurance this gate exists to stop anyone taking on
// trust. Both reach the pin before any runner, so they cost a process start.
const spawnGateWith = (environment) =>
  spawnSync(process.execPath, [fileURLToPath(import.meta.url)], {
    cwd: repositoryRoot,
    encoding: "utf8",
    maxBuffer: 128 * 1024 * 1024,
    env: environment,
  });

const unpinnedEnvironment = { ...process.env };
delete unpinnedEnvironment.MIDGARD_AIKEN_BIN;
delete unpinnedEnvironment.MIDGARD_FORK_AIKEN_BIN;
const unpinnedRun = spawnGateWith(unpinnedEnvironment);
assert.notEqual(
  unpinnedRun.status,
  0,
  "Q62 DA rotation gate ran its on-chain measurement with neither compiler variable set",
);
assert.match(
  unpinnedRun.stderr,
  /ERR_AIKEN_BINARY_UNPINNED/su,
  "the gate rejected an unpinned compiler without its specific diagnostic",
);

const compilerStubRoot = mkdtempSync(join(tmpdir(), "midgard-q62-compiler-"));
try {
  const stubBinary = resolve(compilerStubRoot, "stock-aiken");
  writeFileSync(
    stubBinary,
    "#!/bin/sh\nprintf '%s\\n' 'aiken v1.1.22+unknown'\n",
  );
  chmodSync(stubBinary, 0o755);
  const stockEnvironment = { ...process.env, MIDGARD_AIKEN_BIN: stubBinary };
  delete stockEnvironment.MIDGARD_FORK_AIKEN_BIN;
  const stockRun = spawnGateWith(stockEnvironment);
  assert.notEqual(
    stockRun.status,
    0,
    "Q62 DA rotation gate accepted a stock-versioned compiler for test execution",
  );
  assert.match(
    stockRun.stderr,
    /ERR_AIKEN_COMPILER_MISMATCH: .*v1\.1\.22/su,
    "the gate rejected a stock compiler without its specific diagnostic",
  );
} finally {
  rmSync(compilerStubRoot, { recursive: true, force: true });
}

const report = {
  status: "PASS",
  goalIds: evidence.goalIds,
  decisionRow: evidence.decisionRow,
  aikenCompiler: {
    variable: aikenBinaryVariable,
    binary: aikenBinaryPath,
    version: aikenCompiler,
  },
  committeeHashRederivation: rederivation,
  groupTotals,
  aikenChecksExecuted: aikenOutcome.passed,
  aikenSelectorsCollected: aikenOutcome.collected,
  vitestChecksExecuted,
  executedChecks,
  declaredStandaloneCatalogueIds:
    declared.structural.standaloneCatalogueIds.length,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q62 DA rotation: PASS (${Object.entries(groupTotals)
      .map(
        ([id, totals]) =>
          `${id} ${Object.entries(totals)
            .map(([language, total]) => `${language}=${String(total)}`)
            .join("/")}`,
      )
      .join(
        ", ",
      )}; committee hash re-derived at ${String(rederivation.rederivationSites)} production site inside the single params reader, covering ${String(rederivation.paramsReadCallSites)} params reads; ${String(executedChecks)} runner-executed checks under ${aikenCompiler} via ${aikenBinaryVariable}, ${String(declared.structural.standaloneCatalogueIds.length)} declared standalone catalogue IDs)`,
  );
}
