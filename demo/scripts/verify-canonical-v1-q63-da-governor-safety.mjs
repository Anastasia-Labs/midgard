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
// owner never accepted. Quoting is necessary but not sufficient, so the gate
// also recomputes `max(2, ceil(2n/3))` from an independently written integer
// ceiling and pins the whole floor table by digest — a transcription error in
// an implementation is a different defect from a drifted quotation, and each
// now has its own detector.
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
// Crucially, that incompleteness signal is NOT self-declared. `REQUIRED_GROUPS`
// below is the authority on which acceptance groups exist and how many checks
// each owes; the artifact may record a group OPEN, but it cannot make one
// disappear. Deleting the two rescue entries used to turn this gate green with
// zero rescue paths measured — it now fails ERR_REQUIRED_GROUP_MISSING, and the
// `--evidence-fixture=` self-tests at the bottom prove it against this very
// gate rather than describing it.
//
// usage: node demo/scripts/verify-canonical-v1-q63-da-governor-safety.mjs [--json]

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
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
// Q63's acceptance shape, held here rather than read from the artifact.
//
// This is the fix for the review's finding E1. Previously the only thing
// standing between "two rescue clauses are unmet" and a green gate was the
// artifact's own `openGroups` array: delete both entries and the gate reported
// PASS with 0 rescue paths measured. The table below makes the group set and
// each group's cardinality a property of the *verifier*, so an acceptance
// clause may be recorded OPEN (with a blocker) but may not be dropped, shrunk,
// or replaced by an invented one.
//
// `languages` is part of the requirement, not the artifact's choice. Most
// groups must be carried independently in both languages — the floors are a
// cross-language pair and one side alone is not the claim. Two groups are
// single-language by construction and must say so here to be allowed it: a
// Plutus spending handler has no off-chain twin, and a Node verifier can import
// no Aiken.
// ---------------------------------------------------------------------------

const REQUIRED_GROUPS = [
  {
    id: "governed-floor-drain-invariants",
    expected: 3,
    languages: ["aiken", "vitest"],
  },
  { id: "valid-boundary-controls", expected: 2, languages: ["aiken", "vitest"] },
  {
    id: "below-floor-drain-rejections",
    expected: 3,
    languages: ["aiken", "vitest"],
  },
  {
    id: "single-member-committee-consequence",
    expected: 2,
    languages: ["aiken", "vitest"],
  },
  { id: "spend-seam-datum-enforcement", expected: 2, languages: ["aiken"] },
  { id: "floor-arithmetic-provenance", expected: 1, languages: ["vitest"] },
  { id: "partial-attestation-rescue-path", expected: 1, languages: ["aiken"] },
  {
    id: "rescue-theft-duplicate-replay-rejections",
    expected: 3,
    languages: ["aiken"],
  },
];

const requiredGroup = (id) =>
  REQUIRED_GROUPS.find((candidate) => candidate.id === id);

// `max_indexed_signer_count` from `onchain/aiken/lib/midgard/da-attestation-types.ak`
// — the largest committee the attested-signer bitmap can index, and therefore
// the largest set size the floor must be defined over. Held as a constant here
// because this gate is a Node process and cannot import Aiken; the
// `floor-arithmetic-provenance` Vitest check reads the real Aiken declaration
// and fails if it ever stops being 256.
const MAX_INDEXED_SIGNER_COUNT = 256;

// `max(2, ceil(2n/3))`, written as an exact integer ceiling rather than as the
// `(2n + 2) / 3` integer-division form BOTH implementations use. Sharing the
// clever form would make this a restatement; deriving the ceiling from a
// quotient and a remainder makes a floor/ceil transcription error in that form
// visible here.
const independentGovernedThresholdFloor = (setLength) => {
  const numerator = 2 * setLength;
  const twoThirdsCeiling =
    Math.trunc(numerator / 3) + (numerator % 3 === 0 ? 0 : 1);
  return Math.max(2, twoThirdsCeiling);
};

const independentFloorTable = [];
for (
  let setLength = 0;
  setLength <= MAX_INDEXED_SIGNER_COUNT;
  setLength += 1
) {
  independentFloorTable.push(independentGovernedThresholdFloor(setLength));
}
const independentFloorDigest = createHash("sha256")
  .update(JSON.stringify(independentFloorTable))
  .digest("hex");

// Pure: re-derives the arithmetic F04 authorises and pins the artifact's whole
// floor table to it. Quoting the decision record proves it still says what Q63
// cited; only this proves something computes it.
const assertFloorTableProvenance = (candidate) => {
  const floorTable = candidate.floorTable ?? {};
  if (
    !Array.isArray(floorTable.range) ||
    floorTable.range.length !== 2 ||
    floorTable.range[0] !== 0 ||
    floorTable.range[1] !== MAX_INDEXED_SIGNER_COUNT
  ) {
    throw new Error(
      `ERR_FLOOR_TABLE_RANGE: the published floor table must span [0, ${String(MAX_INDEXED_SIGNER_COUNT)}] — every representable set size — but declares ${JSON.stringify(floorTable.range)}`,
    );
  }
  if (floorTable.sha256 !== independentFloorDigest) {
    throw new Error(
      `ERR_FLOOR_TABLE_DIGEST: the published floor table digest ${String(floorTable.sha256)} disagrees with max(2, ceil(2n/3)) recomputed here as ${independentFloorDigest}, so an implementation or the table has a floor/ceil transcription error`,
    );
  }
  for (const [name, setName] of [
    ["da_threshold_floor", "committee_len"],
    ["update_threshold_floor", "owner_len"],
  ]) {
    const bound = (candidate.decisionSource?.boundValues ?? []).find(
      (entry) => entry.name === name,
    );
    if (bound === undefined) {
      throw new Error(`ERR_FLOOR_TABLE_UNBOUND: the artifact does not bind ${name}`);
    }
    if (bound.expression !== floorTable.expression.replace("set_len", setName)) {
      throw new Error(
        `ERR_FLOOR_TABLE_EXPRESSION: the F04 ${name} expression ${bound.expression} is not the digest-pinned floor expression with the set named`,
      );
    }
  }
};

// Pure: a title cited twice would let one executed test be counted for two
// separate claims.
const assertCitedTitlesUnique = (candidate) => {
  const seen = new Set();
  for (const group of candidate.groups ?? []) {
    for (const item of group.evidence ?? []) {
      if (item.kind !== "vitest") {
        continue;
      }
      for (const title of item.titles ?? []) {
        if (seen.has(title)) {
          throw new Error(
            `ERR_DUPLICATE_VITEST_CITATION: the artifact cites the Vitest title "${title}" twice, so one executed test would be counted for two claims`,
          );
        }
        seen.add(title);
      }
    }
  }
};

// Pure: takes a parsed artifact and returns the measured/open partition of the
// required groups, or throws naming exactly which requirement it failed. Kept
// side-effect free so the seeded mutations below can drive it directly.
const partitionRequiredGroups = (candidate) => {
  const measured = new Map(
    (candidate.groups ?? []).map((group) => [group.id, group]),
  );
  const open = new Map(
    (candidate.openGroups ?? []).map((group) => [group.id, group]),
  );

  for (const required of REQUIRED_GROUPS) {
    const group = measured.get(required.id) ?? open.get(required.id);
    if (group === undefined) {
      throw new Error(
        `ERR_REQUIRED_GROUP_MISSING: Q63 acceptance requires the group ${required.id}, which appears in neither groups nor openGroups. A clause may be recorded OPEN with its blocker, but it may not be dropped — dropping it is how an unmet clause would read as a green gate`,
      );
    }
    if (measured.has(required.id) && open.has(required.id)) {
      throw new Error(
        `ERR_REQUIRED_GROUP_DUPLICATED: ${required.id} is published as both a measured group and an open group, so no single disposition backs it`,
      );
    }
    if (group.expected !== required.expected) {
      throw new Error(
        `ERR_REQUIRED_GROUP_CARDINALITY: ${required.id} claims ${String(
          group.expected,
        )} check(s) but Q63 acceptance requires ${String(required.expected)}`,
      );
    }
  }

  const requiredIds = new Set(REQUIRED_GROUPS.map(({ id }) => id));
  for (const group of [
    ...(candidate.groups ?? []),
    ...(candidate.openGroups ?? []),
  ]) {
    if (!requiredIds.has(group.id)) {
      throw new Error(
        `ERR_UNDECLARED_GROUP: ${group.id} is published but is not one of Q63's required acceptance groups, so nothing fixes what it owes`,
      );
    }
  }

  return { measured, open };
};

// ---------------------------------------------------------------------------
// Seeded artifact mutations. These run before anything expensive and exit, so
// the self-tests at the bottom can spawn this gate and observe its real exit
// code on a dropped, shrunken, or invented acceptance group.
// ---------------------------------------------------------------------------

const evidenceFixtures = {
  intact: (artifact) => artifact,
  "dropped-open-group": (artifact) => ({
    ...artifact,
    openGroups: artifact.openGroups.filter(
      (group) => group.id !== "partial-attestation-rescue-path",
    ),
  }),
  // The exact E1 shape: delete every OPEN entry and the incompleteness signal
  // vanishes with them.
  "dropped-all-open-groups": (artifact) => ({
    ...artifact,
    openGroups: [],
    acceptanceComplete: true,
  }),
  "dropped-measured-group": (artifact) => ({
    ...artifact,
    groups: artifact.groups.filter(
      (group) => group.id !== "spend-seam-datum-enforcement",
    ),
  }),
  "shrunken-open-group": (artifact) => ({
    ...artifact,
    openGroups: artifact.openGroups.map((group) =>
      group.id === "rescue-theft-duplicate-replay-rejections"
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
  "duplicated-group": (artifact) => ({
    ...artifact,
    openGroups: [
      ...artifact.openGroups,
      {
        ...artifact.groups.find(
          (group) => group.id === "valid-boundary-controls",
        ),
        disposition: "OPEN",
      },
    ],
  }),
  // Finding E3: a floor/ceil transcription error, which the quoted F04 text
  // cannot see.
  "floor-digest-drift": (artifact) => ({
    ...artifact,
    floorTable: {
      ...artifact.floorTable,
      sha256: `${artifact.floorTable.sha256.slice(0, -1)}${
        artifact.floorTable.sha256.endsWith("0") ? "1" : "0"
      }`,
    },
  }),
  "floor-range-truncated": (artifact) => ({
    ...artifact,
    floorTable: { ...artifact.floorTable, range: [0, 128] },
  }),
  // Finding E2: one executed test counted for two claims.
  "duplicate-vitest-citation": (artifact) => {
    const groups = structuredClone(artifact.groups);
    const item = groups
      .flatMap((group) => group.evidence)
      .find((evidenceItem) => evidenceItem.kind === "vitest");
    item.titles = [...item.titles, item.titles[0]];
    return { ...artifact, groups };
  },
};

// Every pure artifact-shape guard, run together so one seeded mutation
// exercises exactly the check it targets.
const assertArtifactInvariants = (candidate) => {
  const partition = partitionRequiredGroups(candidate);
  assertFloorTableProvenance(candidate);
  assertCitedTitlesUnique(candidate);
  return partition;
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

const { measured: measuredGroups, open: openGroups } =
  assertArtifactInvariants(evidence);

// ---------------------------------------------------------------------------
// F04 is the only source of Q63's numbers. Re-read it at the cited lines, then
// re-derive the arithmetic those lines authorise.
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

// The floor-table provenance (`assertFloorTableProvenance`) already ran above
// as part of `assertArtifactInvariants`, so the arithmetic F04 authorises is
// re-derived before any runner is spawned.

// ---------------------------------------------------------------------------
// Phase 1 — collect what the artifact declares. Nothing here decides whether a
// check passed; the declarations are only the plan the runners execute.
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
  // A single-language group has to declare and justify it. The requirement is
  // still the table's; this only stops a reader from having to infer the
  // narrowing from an absent citation.
  if (required.languages.length === 1) {
    assert.deepEqual(
      group.languages,
      required.languages,
      `group ${group.id} is single-language by construction and must declare it`,
    );
    assert.ok(
      typeof group.languageRationale === "string" &&
        group.languageRationale.length > 0,
      `group ${group.id} must say why it carries only ${required.languages.join(", ")}`,
    );
  } else {
    assert.equal(
      group.languages,
      undefined,
      `group ${group.id} must carry both languages and may not declare a narrower set`,
    );
  }

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

  // Each required language must independently carry the group's full
  // cardinality, and a language the group does not carry must cite nothing.
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
      `group ${group.id} cites ${String(cited)} ${language} check(s) but Q63 acceptance requires ${String(expected)}`,
    );
  }
  groupPlans.set(group.id, plan);
}

// Open acceptance groups must name a real blocker rather than simply vanish.
// (That they exist at all is enforced by `partitionRequiredGroups`.)
for (const group of openGroups.values()) {
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
  assert.ok(
    !Array.isArray(group.evidence) || group.evidence.length === 0,
    `open group ${group.id} may not cite evidence it has not measured`,
  );
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

// Cited Vitest titles were proven unique by `assertCitedTitlesUnique` above,
// under the `--evidence-fixture=duplicate-vitest-citation` seed.

// Aiken test execution for this goal is fork-only. Resolving the compiler by
// name rather than by PATH default is what lets the gate publish *which* build
// produced its on-chain result; falling through to a stock `aiken` would
// silently supply the answer under another compiler's identity.
const aikenBinaryPath = process.env.MIDGARD_AIKEN_BIN;
assert.ok(
  typeof aikenBinaryPath === "string" && aikenBinaryPath.length > 0,
  "ERR_AIKEN_BINARY_UNPINNED: MIDGARD_AIKEN_BIN must name the patched Aiken fork — Q63's on-chain measurement is fork-only, and an unset variable would run whatever `aiken` is first on PATH while still publishing the result as Q63's",
);
assert.equal(
  evidence.compiler.environmentVariable,
  "MIDGARD_AIKEN_BIN",
  "the artifact must record the compiler variable this gate pins",
);
const aikenCompiler = aikenCompilerVersion(aikenBinaryPath);
assert.ok(
  aikenCompiler.length > 0,
  `${aikenBinaryPath} reported an empty version string`,
);

const aikenOutcome = deriveAikenOutcome({
  label: "Q63 DA-governor safety on-chain selectors",
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

// Titles the runner actually collected AND reported as passed. `vitestChecksExecuted`
// is derived from this set rather than from the declaration's own length — the
// review's finding E2: summing the titles the artifact declares republishes a
// declaration count under the name of a measurement.
const measuredVitestTitles = new Set();
for (const declaration of declaredVitestFiles.values()) {
  const run = runVitest({
    packageRoot: resolve(repositoryRoot, declaration.packageDirectory),
    testFile: declaration.testFile,
  });
  deriveVitestOutcome({
    label: `Q63 DA-governor safety ${declaration.file}`,
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
      `Q63 DA-governor safety ${declaration.file}: the runner did not report the cited title as passing — ${title}`,
    );
    measuredVitestTitles.add(title);
  }
}

// ---------------------------------------------------------------------------
// Recompute every published number from what the runners measured.
// ---------------------------------------------------------------------------

const groupTotals = {};
for (const required of REQUIRED_GROUPS) {
  const plan = groupPlans.get(required.id);
  if (plan === undefined) {
    // An OPEN clause has measured nothing. That is the honest number, and the
    // required-group table is what stops the clause from vanishing instead.
    groupTotals[required.id] = 0;
    continue;
  }
  const perLanguage = required.languages.map((language) =>
    language === "aiken"
      ? plan.aikenSelectors.filter((selector) =>
          measuredAikenSelectors.has(selector),
        ).length
      : plan.vitestTitles.filter((title) => measuredVitestTitles.has(title))
          .length,
  );
  for (const [index, count] of perLanguage.entries()) {
    assert.equal(
      count,
      required.expected,
      `group ${required.id}: the runner measured ${String(count)} passing ${required.languages[index]} check(s), but Q63 acceptance requires ${String(required.expected)}`,
    );
  }
  groupTotals[required.id] = perLanguage[0];
}

const vitestChecksExecuted = measuredVitestTitles.size;
const executedChecks = aikenOutcome.passed + vitestChecksExecuted;

const recomputedSummary = {
  measuredGroups: REQUIRED_GROUPS.filter(({ id }) => measuredGroups.has(id))
    .length,
  openGroups: REQUIRED_GROUPS.filter(({ id }) => openGroups.has(id)).length,
  groupTotals,
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

// Acceptance is complete exactly when every required group has been measured —
// derived from the required-group table, not from the length of the artifact's
// own `openGroups` array.
assert.equal(
  evidence.acceptanceComplete,
  REQUIRED_GROUPS.every(({ id }) => measuredGroups.has(id)),
  "acceptanceComplete must agree with whether every required acceptance group has been measured",
);

// ---------------------------------------------------------------------------
// Declared, not measured. These are author claims about scope that no runner
// can settle, so they are checked against constants held here and published
// under a name that says they are declarations. Asserting them against
// themselves would have looked like verification and proved nothing.
// ---------------------------------------------------------------------------

const declared = evidence.declared;
assert.deepEqual(
  declared.structural.standaloneCatalogueIds,
  [],
  "Q63 must not introduce a standalone catalogue ID",
);
assert.equal(
  declared.structural.implementationOrLiveClaimsFromF04,
  0,
  "no implementation or live-readiness claim may be derived solely from F04",
);
assert.equal(declared.parentIntegration.owner, "parent");
assert.ok(
  declared.parentIntegration.pendingEdits.length > 0,
  "the parent handoff must list the edits it owns",
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
  // Finding E1. Each of these turned the gate green before the required-group
  // table existed.
  [
    "--evidence-fixture=dropped-open-group",
    /ERR_REQUIRED_GROUP_MISSING: .*partial-attestation-rescue-path/su,
  ],
  [
    "--evidence-fixture=dropped-all-open-groups",
    /ERR_REQUIRED_GROUP_MISSING: .*partial-attestation-rescue-path/su,
  ],
  [
    "--evidence-fixture=dropped-measured-group",
    /ERR_REQUIRED_GROUP_MISSING: .*spend-seam-datum-enforcement/su,
  ],
  [
    "--evidence-fixture=shrunken-open-group",
    /ERR_REQUIRED_GROUP_CARDINALITY: .*rescue-theft-duplicate-replay-rejections/su,
  ],
  [
    "--evidence-fixture=invented-group",
    /ERR_UNDECLARED_GROUP: .*invented-extra-group/su,
  ],
  [
    "--evidence-fixture=duplicated-group",
    /ERR_REQUIRED_GROUP_DUPLICATED: .*valid-boundary-controls/su,
  ],
  // Finding E3.
  [
    "--evidence-fixture=floor-digest-drift",
    /ERR_FLOOR_TABLE_DIGEST: .*transcription error/su,
  ],
  [
    "--evidence-fixture=floor-range-truncated",
    /ERR_FLOOR_TABLE_RANGE: .*every representable set size/su,
  ],
  // Finding E2.
  [
    "--evidence-fixture=duplicate-vitest-citation",
    /ERR_DUPLICATE_VITEST_CITATION: .*counted for two claims/su,
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
  [
    "--evidence-fixture=intact",
    /evidence fixture intact: artifact invariants accepted/u,
  ],
]) {
  const control = runSelfTest(flag);
  assert.equal(
    control.status,
    0,
    `Q63 DA-governor safety gate rejected a passing fixture (${flag}): ${control.stderr}`,
  );
  assert.match(control.stdout, expectedStdout);
}

// Finding E7, seeded rather than asserted: spawn this gate's real main path
// with the compiler pin removed and require it to fail closed. It reaches the
// pin before any runner, so this costs a process start and nothing more.
const unpinnedEnvironment = { ...process.env };
delete unpinnedEnvironment.MIDGARD_AIKEN_BIN;
const unpinnedRun = spawnSync(
  process.execPath,
  [fileURLToPath(import.meta.url)],
  {
    cwd: repositoryRoot,
    encoding: "utf8",
    maxBuffer: 128 * 1024 * 1024,
    env: unpinnedEnvironment,
  },
);
assert.notEqual(
  unpinnedRun.status,
  0,
  "Q63 DA-governor safety gate ran its on-chain measurement without a pinned Aiken compiler",
);
assert.match(
  unpinnedRun.stderr,
  /ERR_AIKEN_BINARY_UNPINNED/su,
  "the gate rejected an unpinned compiler without its specific diagnostic",
);

const openGroupIds = REQUIRED_GROUPS.filter(({ id }) => openGroups.has(id)).map(
  ({ id }) => id,
);
const status = openGroupIds.length === 0 ? "PASS" : "INCOMPLETE";

const report = {
  status,
  goalIds: evidence.goalIds,
  decisionRow: evidence.decisionRow,
  aikenCompiler: { binary: aikenBinaryPath, version: aikenCompiler },
  floorTableDigest: independentFloorDigest,
  floorTableRange: [0, MAX_INDEXED_SIGNER_COUNT],
  groupTotals,
  aikenChecksExecuted: aikenOutcome.passed,
  aikenSelectorsCollected: aikenOutcome.collected,
  vitestChecksExecuted,
  executedChecks,
  declaredStandaloneCatalogueIds:
    declared.structural.standaloneCatalogueIds.length,
  declaredImplementationOrLiveClaimsFromF04:
    declared.structural.implementationOrLiveClaimsFromF04,
  openGroups: openGroupIds,
};

if (emitJson) {
  console.log(JSON.stringify(report, null, 2));
} else {
  console.log(
    `Q63 DA-governor safety: ${status} (${Object.entries(groupTotals)
      .map(([id, total]) => `${id} ${String(total)}`)
      .join(", ")}; ${String(executedChecks)} runner-executed checks under ${aikenCompiler}, ${String(declared.structural.standaloneCatalogueIds.length)} declared standalone catalogue IDs, ${String(declared.structural.implementationOrLiveClaimsFromF04)} declared F04-derived implementation/live claims)`,
  );
}

if (openGroupIds.length > 0) {
  const blocker = openGroups.get(openGroupIds[0]).blockedOn;
  console.error(
    `Q63_INCOMPLETE: acceptance clause (c) is unmet — ${openGroupIds.join(", ")} remain OPEN, blocked on the ${blocker.leasedPath} lease held by ${blocker.leaseHolder}`,
  );
  process.exit(1);
}
