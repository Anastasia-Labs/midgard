#!/usr/bin/env node
/**
 * CG3 — P3 gate (GOAL_SPEC §8.3 line 903): "No enabled accepted transition or
 * rejection reason lacks an L1 one-step verifier; TypeScript/Aiken vectors
 * cover every instruction."
 *
 * This is a measurement tool, not a claim of completion. It re-derives, from
 * source, the exhaustive surface CG3 must cover and REFUSES PASS with exact
 * counted findings whenever a hole exists, rather than failing opaquely or
 * asserting a fixed expectation that could drift silently out of sync with
 * the tree. Nothing here is hardcoded except the file paths it reads and the
 * two structural facts that "terminal" has no resolver and phase indices are
 * dense from 0: every phase name, resolver title, semantic-resolver count,
 * rejection code and one-step verifier selector is read live.
 *
 * What is checked:
 *
 *   1. The 14 nonterminal `ValidationPhase` values (from
 *      `resolverPhaseIndex` in validation-machine-data.ts) each have a
 *      concrete `verify_<phase>_one_step_v1` Aiken selector
 *      (validation-machine-v1.ak) and a resolver title
 *      (`prepares` in contracts.ts; no direct resolvers remain).
 *   2. The two onchain semantic-resolver cardinality guards
 *      (validation-resolver-v1.ak) reconcile against the TypeScript
 *      registry's own per-phase semantic-resolver grouping
 *      (`semanticResolverGroups` in contracts.ts).
 *   3. Every `reject_*` rejection code declared in validation-machine-v1.ak
 *      is reachable, via a source-derived call graph, from at least one
 *      phase's one-step verifier. A code reachable from no phase is an
 *      unprovable gap.
 *   4. The two formerly-direct phases (`cek`, `valueAndMint`) keep their
 *      semantic-resolver decomposition: `validationSemanticResolverIndexV1`
 *      must not return null for them and `semanticResolverOffsetsV1` must
 *      not pin -1 at their resolver indices. Both facts are read live; the
 *      #617 wave split the aggregates (decision 0005 R5), and either fact
 *      regressing is reported as a reopened totality hole.
 *   5. The cross-language vector: the Aiken test count in
 *      validation-one-step-cross-language.test.ak, and whether the
 *      TypeScript fixture generator that feeds it exists.
 *
 * Every one of the above is a counted finding, not a narrative one. The
 * final line names the exact count. It cannot be zero merely because the
 * gate ran; it is zero only when the source says so.
 */

import { readFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");

const CONTRACTS_PATH = "demo/midgard-sdk/src/fraud-proof/contracts.ts";
const RESOLVER_AK_PATH = "onchain/aiken/lib/midgard/validation-resolver-v1.ak";
const MACHINE_AK_PATH = "onchain/aiken/lib/midgard/validation-machine-v1.ak";
const MACHINE_DATA_TS_PATH =
  "demo/midgard-validation/src/validation-machine-data.ts";
const MACHINE_TEST_TS_PATH =
  "demo/midgard-validation/tests/validation-machine.test.ts";
const CROSS_LANG_TEST_AK_PATH =
  "onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak";
const FIXTURE_GENERATOR_PATH =
  "demo/midgard-validation/scripts/generate-validation-one-step-aiken-fixture.mjs";

const read = (relativePath) =>
  readFileSync(resolve(repositoryRoot, relativePath), "utf8");

/** Internal-error guard: a source shape this script does not understand must
 * abort loudly rather than silently under-count. This is distinct from a
 * measured gap, which is a finding, not an error. */
const internalError = (message) => {
  process.stderr.write(`p3-totality: internal error: ${message}\n`);
  process.exit(1);
};

const snakeCase = (camel) =>
  camel.replace(/[A-Z]/g, (letter) => `_${letter.toLowerCase()}`);

/* ------------------------------------------------------------------ */
/* 1. The 14 nonterminal phases, in `resolverPhaseIndex` order.        */
/* ------------------------------------------------------------------ */

const machineDataText = read(MACHINE_DATA_TS_PATH);
const resolverPhaseIndexBlock = machineDataText.match(
  /const resolverPhaseIndex = \(phase: MidgardValidationPhaseName\): number => \{\s*const index = \{([\s\S]*?)\}\[phase\];/,
);
if (resolverPhaseIndexBlock === null) {
  internalError(
    `${MACHINE_DATA_TS_PATH} no longer declares resolverPhaseIndex in the expected shape`,
  );
}
const phaseIndexEntries = [
  ...resolverPhaseIndexBlock[1].matchAll(/^\s*([a-zA-Z]+):\s*(-?\d+),?$/gm),
].map(([, name, value]) => ({ name, value: Number(value) }));
const terminalEntry = phaseIndexEntries.find(({ name }) => name === "terminal");
if (terminalEntry === undefined || terminalEntry.value !== -1) {
  internalError(
    "resolverPhaseIndex no longer marks exactly one terminal phase at -1",
  );
}
const nonterminalPhases = phaseIndexEntries
  .filter(({ name }) => name !== "terminal")
  .sort((a, b) => a.value - b.value);
const expectedIndices = nonterminalPhases.map((_, index) => index);
if (
  nonterminalPhases.length === 0 ||
  !nonterminalPhases.every(
    ({ value }, index) => value === expectedIndices[index],
  )
) {
  internalError(
    "resolverPhaseIndex's nonterminal values are not a dense 0..N-1 sequence",
  );
}
const PHASE_COUNT = nonterminalPhases.length;
const phaseNames = nonterminalPhases.map(({ name }) => name);

/* ------------------------------------------------------------------ */
/* 2. Resolver titles and semantic-resolver grouping (contracts.ts).  */
/* ------------------------------------------------------------------ */

const contractsText = read(CONTRACTS_PATH);

const extractOrderedStringPairs = (blockText) =>
  [...blockText.matchAll(/([a-zA-Z][a-zA-Z0-9]*):\s*\n?\s*"([^"]+)"/g)].map(
    ([, key, value]) => ({ key, value }),
  );

const sliceNamedBlock = (label, text) => {
  const match = text.match(
    new RegExp(`\\b${label}:\\s*\\{([\\s\\S]*?)\\n  \\},`),
  );
  if (match === null) {
    internalError(`${CONTRACTS_PATH} no longer declares a ${label} block`);
  }
  return match[1];
};

const preparesBlock = sliceNamedBlock("prepares", contractsText);
const preparesEntries = extractOrderedStringPairs(preparesBlock);
// Every phase is a prepare + semantic decomposition since the #617 wave split
// the cek and ValueAndMint direct resolvers (decision 0005 R5); a
// `directResolvers` block reappearing in contracts.ts is a regression of that
// split, not a shape this verifier tolerates.
if (preparesEntries.length !== PHASE_COUNT) {
  internalError(
    `prepares declares ${String(preparesEntries.length)} entries; expected ${String(PHASE_COUNT)} (one prepare resolver per phase)`,
  );
}
if (/directResolvers\s*:/.test(contractsText)) {
  internalError(
    "contracts.ts declares a directResolvers block; every phase must be a prepare + semantic decomposition",
  );
}

const semanticsBlock = sliceNamedBlock("semantics", contractsText);
const semanticsEntries = extractOrderedStringPairs(semanticsBlock);

const resolverCountMatch = contractsText.match(
  /export const VALIDATION_TRACE_RESOLVER_COUNT_V1 = (\d+);/,
);
if (resolverCountMatch === null) {
  internalError(
    `${CONTRACTS_PATH} no longer exports VALIDATION_TRACE_RESOLVER_COUNT_V1`,
  );
}
const VALIDATION_TRACE_RESOLVER_COUNT_V1 = Number(resolverCountMatch[1]);

const canonicalDecodeItemStagesBlock = sliceNamedBlock(
  "canonicalDecodeItemStages",
  contractsText,
);
const canonicalDecodeItemStagesCount = extractOrderedStringPairs(
  canonicalDecodeItemStagesBlock,
).length;

const scriptSourcesStageOneRedeemerStagesBlock = sliceNamedBlock(
  "scriptSourcesStageOneRedeemerStages",
  contractsText,
);
const scriptSourcesStageOneRedeemerStagesEntries = extractOrderedStringPairs(
  scriptSourcesStageOneRedeemerStagesBlock,
);

const envelopeEntry = scriptSourcesStageOneRedeemerStagesEntries.find(
  ({ key }) => key === "envelope",
);
if (envelopeEntry === undefined) {
  internalError(
    "scriptSourcesStageOneRedeemerStages no longer declares an envelope stage",
  );
}

/**
 * `semanticResolverGroups`: an ordered array of arrays, each holding the
 * `semanticResolvers[N]` indices deployed for one prepare-routed phase, in
 * exactly the `prepares` key order (both are built from the same
 * `prepareTitles.entries()` loop in contracts.ts). The scriptSources group
 * additionally borrows `semanticResolvers[75]` (the stage-one-redeemer
 * envelope), which is why it is read structurally here rather than assumed.
 */
const semanticResolverGroupsMatch = contractsText.match(
  /const semanticResolverGroups = \[([\s\S]*?)\] as const;/,
);
if (semanticResolverGroupsMatch === null) {
  internalError(`${CONTRACTS_PATH} no longer declares semanticResolverGroups`);
}
const semanticResolverGroupBodies = [
  ...semanticResolverGroupsMatch[1].matchAll(
    /\[\s*(?:semanticResolvers\[\d+\]!?\s*,?\s*)+\]/g,
  ),
].map(([body]) => body);
if (semanticResolverGroupBodies.length !== preparesEntries.length) {
  internalError(
    `semanticResolverGroups declares ${String(semanticResolverGroupBodies.length)} groups; expected ${String(preparesEntries.length)} (one per prepares entry)`,
  );
}
const semanticResolverGroupSizes = semanticResolverGroupBodies.map(
  (body) => [...body.matchAll(/semanticResolvers\[\d+\]/g)].length,
);
const totalSemanticSlots = semanticResolverGroupSizes.reduce(
  (sum, size) => sum + size,
  0,
);
if (totalSemanticSlots !== semanticsEntries.length + 1) {
  internalError(
    `semanticResolverGroups covers ${String(totalSemanticSlots)} slots; expected ${String(semanticsEntries.length + 1)} (the ${String(semanticsEntries.length)}-entry semantics registry plus the borrowed envelope)`,
  );
}

/* The two onchain cardinality guards (read live, not pinned). */
const resolverAkText = read(RESOLVER_AK_PATH);
const onchainGuard = (constName) => {
  const match = resolverAkText.match(
    new RegExp(`pub const ${constName}: Int = (\\d+)`),
  );
  if (match === null) {
    internalError(`${RESOLVER_AK_PATH} no longer declares ${constName}`);
  }
  return Number(match[1]);
};
const onchainPhaseAScriptPreconditionsCount = onchainGuard(
  "phase_a_script_preconditions_semantic_resolver_count",
);
const onchainScriptSourcesCount = onchainGuard(
  "script_sources_semantic_resolver_count",
);

/* ------------------------------------------------------------------ */
/* 3. Aiken source structure: top-level declarations and a call graph. */
/* ------------------------------------------------------------------ */

const machineAkText = read(MACHINE_AK_PATH);
const machineAkLines = machineAkText.split("\n");

const topLevelDeclarations = [];
for (const [index, line] of machineAkLines.entries()) {
  const fnMatch = line.match(/^(?:pub\s+)?fn\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(/);
  if (fnMatch !== null) {
    topLevelDeclarations.push({
      kind: "fn",
      name: fnMatch[1],
      line: index + 1,
    });
    continue;
  }
  const otherMatch = line.match(
    /^(?:pub\s+)?(?:const|type)\s+([a-zA-Z_][a-zA-Z0-9_]*)/,
  );
  if (otherMatch !== null) {
    topLevelDeclarations.push({
      kind: "other",
      name: otherMatch[1],
      line: index + 1,
    });
  }
}
if (topLevelDeclarations.length === 0) {
  internalError(`${MACHINE_AK_PATH} yielded no top-level declarations`);
}

/** Each declaration's body ends the line before the NEXT declaration of any
 * kind (fn/const/type) — so a trailing top-level const between two functions
 * is never misread as part of the preceding function's body. */
const functionBodies = new Map();
for (const [index, declaration] of topLevelDeclarations.entries()) {
  if (declaration.kind !== "fn") continue;
  const nextLine =
    index + 1 < topLevelDeclarations.length
      ? topLevelDeclarations[index + 1].line
      : machineAkLines.length + 1;
  const body = machineAkLines
    .slice(declaration.line - 1, nextLine - 1)
    .join("\n");
  functionBodies.set(declaration.name, body);
}
const functionNames = new Set(functionBodies.keys());

/** caller -> set of callee names, derived from which known function names are
 * followed by "(" anywhere in the caller's body text. */
const callGraph = new Map();
for (const [name, body] of functionBodies) {
  const callees = new Set();
  for (const match of body.matchAll(/\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\(/g)) {
    const callee = match[1];
    if (callee !== name && functionNames.has(callee)) callees.add(callee);
  }
  callGraph.set(name, callees);
}

const reachableFrom = (seeds) => {
  const seen = new Set(seeds);
  const queue = [...seeds];
  while (queue.length > 0) {
    const current = queue.shift();
    for (const callee of callGraph.get(current) ?? []) {
      if (!seen.has(callee)) {
        seen.add(callee);
        queue.push(callee);
      }
    }
  }
  return seen;
};

/* ------------------------------------------------------------------ */
/* 4. Per-phase one-step selector, gate function and reachable set.   */
/* ------------------------------------------------------------------ */

const findings = [];
const phaseRows = [];
for (const phaseName of phaseNames) {
  const oneStepSelector = `verify_${snakeCase(phaseName)}_one_step_v1`;
  const selectorPresent = functionNames.has(oneStepSelector);
  if (!selectorPresent) {
    findings.push(
      `phase "${phaseName}" has no ${oneStepSelector} in ${MACHINE_AK_PATH}`,
    );
  }
  let gateName = null;
  let reachable = new Set();
  if (selectorPresent) {
    const body = functionBodies.get(oneStepSelector);
    const gateCandidates = new Set(
      [...body.matchAll(/\bverify_[a-z0-9_]+\s*\(/g)]
        .map((match) => match[0].replace(/\s*\($/, ""))
        .filter((candidate) => candidate !== oneStepSelector),
    );
    if (gateCandidates.size !== 1) {
      internalError(
        `${oneStepSelector} calls ${String(gateCandidates.size)} verify_* functions; expected exactly 1 gate call`,
      );
    }
    gateName = [...gateCandidates][0];
    reachable = reachableFrom([oneStepSelector, gateName]);
  }

  const resolverTitle =
    preparesEntries.find(({ key }) => key === phaseName)?.value ?? null;
  if (resolverTitle === null) {
    findings.push(
      `phase "${phaseName}" has no resolver title in contracts.ts (prepares)`,
    );
  }
  const semanticResolverCount =
    semanticResolverGroupSizes[
      preparesEntries.findIndex(({ key }) => key === phaseName)
    ] ?? null;

  phaseRows.push({
    phaseName,
    resolverTitle,
    semanticResolverCount,
    oneStepSelector,
    selectorPresent,
    gateName,
    reachableCount: reachable.size,
    reachable,
  });
}

/* ------------------------------------------------------------------ */
/* 5. Cardinality reconciliation.                                     */
/* ------------------------------------------------------------------ */

const phaseAScriptPreconditionsRow = phaseRows.find(
  (row) => row.phaseName === "phaseAScriptPreconditions",
);
const scriptSourcesRow = phaseRows.find(
  (row) => row.phaseName === "scriptSources",
);
if (
  phaseAScriptPreconditionsRow === undefined ||
  scriptSourcesRow === undefined
) {
  internalError(
    "phaseAScriptPreconditions/scriptSources phase rows are missing from the derived phase set",
  );
}
const cardinalityChecks = [
  {
    label: "phase_a_script_preconditions_semantic_resolver_count",
    onchain: onchainPhaseAScriptPreconditionsCount,
    ts: phaseAScriptPreconditionsRow.semanticResolverCount,
  },
  {
    label: "script_sources_semantic_resolver_count",
    onchain: onchainScriptSourcesCount,
    ts: scriptSourcesRow.semanticResolverCount,
  },
];
for (const check of cardinalityChecks) {
  if (check.onchain !== check.ts) {
    findings.push(
      `${check.label}: onchain=${String(check.onchain)} disagrees with contracts.ts-derived=${String(check.ts)}`,
    );
  }
}

/* ------------------------------------------------------------------ */
/* 6. Every reject_* rejection code must be reachable from a phase.   */
/* ------------------------------------------------------------------ */

const rejectDefinitionOrder = [
  ...machineAkText.matchAll(/^const (reject_[a-zA-Z0-9_]+)\s*=/gm),
].map(([, name]) => name);
if (rejectDefinitionOrder.length === 0) {
  internalError(`${MACHINE_AK_PATH} declares no reject_* constants`);
}

const decodeHexAscii = (hex) => {
  try {
    return Buffer.from(hex, "hex").toString("ascii");
  } catch {
    return null;
  }
};

const rejectDefinitionValue = new Map(
  [
    ...machineAkText.matchAll(
      /^const (reject_[a-zA-Z0-9_]+)\s*=\s*\n?\s*#"([0-9a-fA-F]*)"/gm,
    ),
  ].map(([, name, hex]) => [name, hex]),
);

const rejectRows = rejectDefinitionOrder.map((codeName) => {
  const raisers = [...functionBodies.entries()]
    .filter(([, body]) => new RegExp(`\\b${codeName}\\b`).test(body))
    .map(([name]) => name)
    .sort();
  const provingPhases = phaseRows
    .filter((row) => raisers.some((raiser) => row.reachable.has(raiser)))
    .map((row) => row.phaseName);
  return {
    codeName,
    label: decodeHexAscii(rejectDefinitionValue.get(codeName) ?? ""),
    raisers,
    provingPhases,
  };
});
const unmappedRejectCodes = rejectRows.filter(
  (row) => row.provingPhases.length === 0,
);
for (const row of unmappedRejectCodes) {
  findings.push(
    `rejection code ${row.codeName} is reachable from no phase's one-step verifier (raisers: ${row.raisers.join(", ") || "none"})`,
  );
}

/* ------------------------------------------------------------------ */
/* 7. Known totality holes: cek/valueAndMint carry no semantic index. */
/* ------------------------------------------------------------------ */

const semanticResolverIndexFnMatch = machineDataText.match(
  /export const validationSemanticResolverIndexV1 = \(([\s\S]*?)\n\};/,
);
if (semanticResolverIndexFnMatch === null) {
  internalError(
    `${MACHINE_DATA_TS_PATH} no longer exports validationSemanticResolverIndexV1`,
  );
}
const semanticResolverIndexFnBody = semanticResolverIndexFnMatch[1];
const directResolverPhaseKeys = ["cek", "valueAndMint"];

const machineTestText = read(MACHINE_TEST_TS_PATH);
const offsetsMatch = machineTestText.match(
  /const semanticResolverOffsetsV1 = \[([\s\S]*?)\] as const;/,
);
if (offsetsMatch === null) {
  internalError(
    `${MACHINE_TEST_TS_PATH} no longer declares semanticResolverOffsetsV1`,
  );
}
const semanticResolverOffsetsV1 = offsetsMatch[1]
  .split(",")
  .map((entry) => entry.trim())
  .filter((entry) => entry.length > 0)
  .map(Number);
if (semanticResolverOffsetsV1.length !== PHASE_COUNT) {
  internalError(
    `semanticResolverOffsetsV1 has ${String(semanticResolverOffsetsV1.length)} entries; expected ${String(PHASE_COUNT)} (one per nonterminal phase)`,
  );
}

/**
 * Each direct-resolver phase is ONE totality hole, evidenced by two
 * independent source facts that must currently agree (both the runtime
 * index function and the pinned test offset say "no semantic resolver"). If
 * either fact changes alone, that is a fresh inconsistency between them
 * rather than the known hole, and is reported as such rather than silently
 * treated as "closed".
 */
const directResolverHoles = [];
for (const phaseName of directResolverPhaseKeys) {
  const caseMatch = semanticResolverIndexFnBody.match(
    new RegExp(
      `case\\s+"${phaseName}":[\\s\\S]{0,80}?return\\s+(null|-?\\d+);`,
    ),
  );
  if (caseMatch === null) {
    findings.push(
      `${MACHINE_DATA_TS_PATH}: validationSemanticResolverIndexV1 has no case for phase "${phaseName}"`,
    );
    continue;
  }
  const returnsNull = caseMatch[1] === "null";

  const phaseEntry = nonterminalPhases.find(({ name }) => name === phaseName);
  if (phaseEntry === undefined) {
    internalError(`nonterminalPhases has no entry for phase "${phaseName}"`);
  }
  const offset = semanticResolverOffsetsV1[phaseEntry.value];
  const offsetPinned = offset === -1;

  if (returnsNull && offsetPinned) {
    directResolverHoles.push(phaseName);
    findings.push(
      `direct-resolver phase "${phaseName}" has no semantic-resolver decomposition: validationSemanticResolverIndexV1 returns null and semanticResolverOffsetsV1[${String(phaseEntry.value)}] is pinned -1`,
    );
  } else if (returnsNull !== offsetPinned) {
    findings.push(
      `direct-resolver phase "${phaseName}" is inconsistent: validationSemanticResolverIndexV1 null-return=${String(returnsNull)} but semanticResolverOffsetsV1[${String(phaseEntry.value)}]=${String(offset)} (pinned=${String(offsetPinned)})`,
    );
  }
}

/* ------------------------------------------------------------------ */
/* 8. Cross-language vector coverage.                                 */
/* ------------------------------------------------------------------ */

const crossLangText = read(CROSS_LANG_TEST_AK_PATH);
const crossLangTestNames = [
  ...crossLangText.matchAll(/^test\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(/gm),
].map(([, name]) => name);
const fixtureGeneratorPresent = existsSync(
  resolve(repositoryRoot, FIXTURE_GENERATOR_PATH),
);
if (!fixtureGeneratorPresent) {
  findings.push(
    `${FIXTURE_GENERATOR_PATH} does not exist: the cross-language vectors have no TypeScript fixture source`,
  );
}
if (crossLangTestNames.length === 0) {
  findings.push(
    `${CROSS_LANG_TEST_AK_PATH} declares no Aiken tests: the cross-language vector has no Aiken-side check`,
  );
}

/* ------------------------------------------------------------------ */
/* Output.                                                             */
/* ------------------------------------------------------------------ */

const lines = [];
lines.push("Canonical V1 CG3 P3 gate: prescribed-missing totality verifier.");
lines.push("");
lines.push(
  `phases: ${String(PHASE_COUNT)} nonterminal ValidationPhase values (resolverPhaseIndex, ${MACHINE_DATA_TS_PATH})`,
);
lines.push(
  "phase-table: phase | resolver | semanticResolvers | oneStepSelector | gate | reachableFns",
);
for (const row of phaseRows) {
  lines.push(
    `phase-row: ${row.phaseName} | prepare:${String(row.resolverTitle)} | ${String(row.semanticResolverCount)} | ${row.oneStepSelector} (present=${String(row.selectorPresent)}) | ${String(row.gateName)} | ${String(row.reachableCount)}`,
  );
}
lines.push("");
lines.push(
  `contracts-registry: prepares=${String(preparesEntries.length)}, semantics=${String(semanticsEntries.length)}, canonicalDecodeItemStages=${String(canonicalDecodeItemStagesCount)}, VALIDATION_TRACE_RESOLVER_COUNT_V1=${String(VALIDATION_TRACE_RESOLVER_COUNT_V1)}`,
);
lines.push("");
lines.push("cardinality-reconciliation:");
for (const check of cardinalityChecks) {
  lines.push(
    `  ${check.label}: onchain=${String(check.onchain)}, contracts.ts-derived=${String(check.ts)}, agree=${String(check.onchain === check.ts)}`,
  );
}
lines.push("");
lines.push(
  `rejection-codes: ${String(rejectRows.length)} reject_* constants in ${MACHINE_AK_PATH}`,
);
for (const row of rejectRows) {
  lines.push(
    `  reject-row: ${row.codeName} (${String(row.label)}) | raisers=${row.raisers.length} | phases=${row.provingPhases.length > 0 ? row.provingPhases.join(",") : "NONE"}`,
  );
}
lines.push("");
lines.push(
  `known-direct-resolver-holes: ${String(directResolverHoles.length)} of ${String(directResolverPhaseKeys.length)} direct-resolver phases [${directResolverHoles.join(", ")}]`,
);
lines.push("");
lines.push(
  `cross-language-vectors: ${CROSS_LANG_TEST_AK_PATH} tests=${String(crossLangTestNames.length)} [${crossLangTestNames.join(", ")}]; fixture-generator-present=${String(fixtureGeneratorPresent)}`,
);
lines.push("");

const sortedFindings = [...findings].sort();
if (sortedFindings.length > 0) {
  lines.push(`findings (${String(sortedFindings.length)}):`);
  for (const finding of sortedFindings) lines.push(`  - ${finding}`);
  lines.push("");
}

if (sortedFindings.length > 0) {
  lines.push(
    `p3-totality: REFUSE-PASS (${String(sortedFindings.length)} unprovable gaps: ${sortedFindings.join(" | ")})`,
  );
} else {
  lines.push(
    `p3-totality: PASS (${String(PHASE_COUNT)} phases, ${String(rejectRows.length)} rejection codes, 0 unprovable gaps)`,
  );
}
lines.push("");

process.stdout.write(lines.join("\n"));
process.exit(0);
