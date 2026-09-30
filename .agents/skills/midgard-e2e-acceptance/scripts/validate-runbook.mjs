#!/usr/bin/env node
// Checks that the e2e acceptance runbook still matches the code it drives: the
// `e2e-stack` command, its flags and package script, the steps the stack runs
// and the node commands they call, the stack's stop messages, and the
// release-readiness gates the runbook says the stack does not produce.
//
// Usage: node validate-runbook.mjs [--skill-dir <dir>]
// `--skill-dir` validates a copy of the skill against this repository's
// sources; the tests use it to prove each check fails on a stale document.
import { spawnSync } from "node:child_process";
import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { readSourceFacets } from "../../../../scripts/lib/source-facets.mjs";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(scriptDir, "../../../..");
const skillDirFlag = process.argv.indexOf("--skill-dir");
const skillDir =
  skillDirFlag >= 0
    ? resolve(process.argv[skillDirFlag + 1] ?? "")
    : resolve(scriptDir, "..");

const tools = join(repoRoot, "demo/midgard-node-tools");
const fullStack = join(tools, "src/full-stack");
const paths = {
  skill: join(skillDir, "SKILL.md"),
  live: join(skillDir, "references/live-acceptance.md"),
  recovery: join(skillDir, "references/recovery.md"),
  benchmark: join(skillDir, "references/benchmark.md"),
  releaseReadiness: join(skillDir, "references/release-readiness.md"),
  preprodStack: join(tools, "docs/PREPROD_STACK.md"),
  cli: join(repoRoot, "demo/midgard-node/src/index.ts"),
  // The stack, finalizer and stress commands are registered in the tooling
  // binary, not the operator binary.
  toolsCli: join(tools, "src/index.ts"),
  toolsPackage: join(tools, "package.json"),
  nonceResume: join(
    repoRoot,
    "demo/midgard-node/src/commands/prepare-hub-oracle-nonce.resume-signed.ts",
  ),
  finalizer: join(tools, "src/commands/e2e-finalize-summary.ts"),
  stateCorrection: join(
    tools,
    "src/commands/e2e-state-correction-acceptance.ts",
  ),
  stateCorrectionTest: join(
    tools,
    "tests/e2e-state-correction-acceptance.test.ts",
  ),
  stateCorrectionAuthority: join(
    tools,
    "src/commands/e2e-state-correction-local-authority.ts",
  ),
  stateCorrectionAuthorityTest: join(
    tools,
    "tests/e2e-state-correction-local-authority.test.ts",
  ),
};

const failures = [];
const fail = (message) => failures.push(message);
const read = (path) => {
  try {
    return readSourceFacets(path);
  } catch (error) {
    fail(`cannot read ${path}: ${error.message}`);
    return "";
  }
};

const documents = Object.fromEntries(
  ["skill", "live", "recovery", "benchmark", "releaseReadiness"].map((name) => [
    name,
    read(paths[name]),
  ]),
);
const preprodStackDoc = read(paths.preprodStack);
const cliSource = read(paths.cli);
const toolsCliSource = read(paths.toolsCli);
const nonceResumeSource = read(paths.nonceResume);
const finalizerSource = read(paths.finalizer);
const stateCorrectionSource = read(paths.stateCorrection);
const stateCorrectionTestSource = read(paths.stateCorrectionTest);
const stateCorrectionAuthoritySource = read(paths.stateCorrectionAuthority);
const stateCorrectionAuthorityTestSource = read(
  paths.stateCorrectionAuthorityTest,
);
const stackSources = Object.fromEntries(
  (() => {
    try {
      return readdirSync(fullStack)
        .filter((name) => name.endsWith(".ts"))
        .sort()
        .map((name) => [name, readFileSync(join(fullStack, name), "utf8")]);
    } catch (error) {
      fail(`cannot read ${fullStack}: ${error.message}`);
      return [];
    }
  })(),
);
const stackSource = Object.values(stackSources).join("\n");
let toolsScripts = {};
try {
  toolsScripts = JSON.parse(readFileSync(paths.toolsPackage, "utf8")).scripts;
} catch (error) {
  fail(`cannot read ${paths.toolsPackage}: ${error.message}`);
}
const allDocs = Object.values(documents).join("\n");

const requireText = (text, needle, label) => {
  if (!text.includes(needle)) fail(`missing ${label}: ${needle}`);
};
const escapeRegExp = (text) => text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
const backticked = (text) =>
  [...text.matchAll(/`([^`\n]+)`/g)].map((match) => match[1]);

// Shape: size, fences, line endings, contents tables, routes.
const skillLines = documents.skill.split("\n").length;
if (skillLines > 500) {
  fail(`SKILL.md has ${skillLines} lines; keep the entrypoint at or below 500`);
}
for (const [name, text] of Object.entries(documents)) {
  const fences = text.match(/^```/gm)?.length ?? 0;
  if (fences % 2 !== 0) fail(`${name} has an unmatched fenced code block`);
  if (/\\\n```/m.test(text)) {
    fail(`${name} has a dangling shell continuation before a closing fence`);
  }
  if (text.includes("\r")) fail(`${name} contains CRLF line endings`);
  const lines = text.split("\n").length;
  if (name !== "skill" && lines > 100) {
    requireText(text, "## Contents", `${name} table of contents`);
  }
  if (name !== "skill" && lines > 400) {
    fail(`${name} has ${lines} lines; keep each reference at or below 400`);
  }
}
for (const route of [
  "references/live-acceptance.md",
  "references/recovery.md",
  "references/benchmark.md",
  "references/release-readiness.md",
]) {
  requireText(documents.skill, route, "reference route");
}

// Retired instructions. The hand-driven flow was replaced by `e2e-stack`;
// naming its commands again would recreate a second way to run acceptance.
for (const forbidden of [
  "e2e-run-step",
  "e2e-start-service",
  "STEP_SUMMARY_ARGS",
  "append_tx_arg",
  "attest-state-queue-once",
  "logs/phase-1-full-corpus-",
  "logs/phase-1-live-acceptance/",
  "preprod-da-2of3/secrets/l1-submitter.seed",
  "--mode fresh-redeploy",
]) {
  if (allDocs.includes(forbidden))
    fail(`forbidden stale instruction: ${forbidden}`);
}

// Commands named in the documents are declared by the binary that runs them.
const commandsDeclaredIn = (source) =>
  new Set(
    [...source.matchAll(/\.command\("([a-zA-Z0-9:_-]+)"\)/g)].map(
      (match) => match[1],
    ),
  );
const declaredOperatorCommands = commandsDeclaredIn(cliSource);
const declaredToolsCommands = commandsDeclaredIn(toolsCliSource);
const referencedOperatorCommands = new Set(
  [...allDocs.matchAll(/node dist\/index\.js\s+([a-zA-Z0-9:_-]+)/g)].map(
    (match) => match[1],
  ),
);
const referencedToolsCommands = new Set(
  [
    ...allDocs.matchAll(
      /node (?:"\$TOOLS_CLI"|demo\/midgard-node-tools\/dist\/index\.js)\s+([a-zA-Z0-9:_-]+)/g,
    ),
  ].map((match) => match[1]),
);
for (const command of referencedOperatorCommands) {
  if (!declaredOperatorCommands.has(command)) {
    fail(
      declaredToolsCommands.has(command)
        ? `documented as an operator command but declared by midgard-node-tools: ${command}`
        : `documented Midgard CLI command is not declared: ${command}`,
    );
  }
}
for (const command of referencedToolsCommands) {
  if (!declaredToolsCommands.has(command)) {
    fail(`documented midgard-node-tools command is not declared: ${command}`);
  }
}
const referencedToolsScripts = new Set(
  [
    ...[...allDocs, preprodStackDoc]
      .join("")
      .matchAll(
        /pnpm --dir (?:"\$TOOLS_DIR"|demo\/midgard-node-tools) run ([a-zA-Z0-9:_-]+)/g,
      ),
  ].map((match) => match[1]),
);
if (!referencedToolsScripts.has("e2e-stack")) {
  fail("the runbook no longer shows the e2e-stack package script invocation");
}
for (const script of referencedToolsScripts) {
  if (typeof toolsScripts?.[script] !== "string") {
    fail(`documented midgard-node-tools package script is missing: ${script}`);
  }
}

// `e2e-stack` flags: an invocation may pass only options the command declares.
const stackCommandBlock = (() => {
  const start = toolsCliSource.indexOf('.command("e2e-stack")');
  if (start < 0) {
    fail('cannot find .command("e2e-stack") in the tooling CLI');
    return "";
  }
  const end = toolsCliSource.indexOf(".action(", start);
  return toolsCliSource.slice(start, end < 0 ? undefined : end);
})();
const stackFlags = new Set(
  [...stackCommandBlock.matchAll(/"(--[a-z][a-z-]*)/g)].map(
    (match) => match[1],
  ),
);
const declaredFlags = new Set(
  [...`${cliSource}\n${toolsCliSource}`.matchAll(/"(--[a-z][a-z0-9-]*)/g)].map(
    (match) => match[1],
  ),
);
const documentedStackFlags = new Set();
for (const [name, text] of [
  ...Object.entries(documents),
  ["PREPROD_STACK.md", preprodStackDoc],
]) {
  const joined = text.replace(/\\\n\s*/g, " ");
  for (const line of joined.split("\n")) {
    const at = line.search(/\be2e-stack\b(?![-\w])/);
    if (at < 0) continue;
    // Stop at the end of a backticked span so prose after it is not read as
    // flags of the invocation.
    const rest = line.slice(at);
    const invocation = rest.split("`")[0];
    for (const [flag] of invocation.matchAll(/--[a-z][a-z-]*/g)) {
      documentedStackFlags.add(flag);
      if (!stackFlags.has(flag))
        fail(`${name} passes an undeclared e2e-stack flag: ${flag}`);
    }
  }
  // A flag named on its own must belong to some command of either binary.
  for (const span of backticked(text)) {
    if (/^--[a-z][a-z0-9-]*$/.test(span) && !declaredFlags.has(span))
      fail(`${name} names a flag no CLI declares: ${span}`);
  }
}
for (const flag of stackFlags) {
  if (!documentedStackFlags.has(flag) && !allDocs.includes(`\`${flag}\``))
    fail(`e2e-stack flag is undocumented in the runbook: ${flag}`);
}

// The step table names every step the stack runs, and only those.
const sourceStepIds = new Set([
  ...[...stackSource.matchAll(/^\s*id: "([a-z0-9-]+)",$/gm)].map((m) => m[1]),
  ...[...stackSource.matchAll(/^\s*id: `\$\{prefix\}-([a-z0-9-]+)`,$/gm)].map(
    (m) => `cycle-N-${m[1]}`,
  ),
]);
if (sourceStepIds.size < 10) {
  fail(`found only ${sourceStepIds.size} stack step ids in ${fullStack}`);
}
const tableRows = (text, heading) => {
  const start = text.indexOf(heading);
  if (start < 0) {
    fail(`cannot find section ${heading}`);
    return [];
  }
  const next = text.indexOf("\n## ", start + heading.length);
  return text
    .slice(start, next < 0 ? undefined : next)
    .split("\n")
    .filter((line) => line.startsWith("| ") && !/^\| -/.test(line))
    .slice(1)
    .map((line) => line.split(" | ").map((cell) => cell.replace(/^\| ?/, "")));
};
const stepRows = tableRows(documents.live, "## What each step does");
const documentedStepIds = new Set(
  stepRows.map(([first]) => backticked(first)[0]).filter(Boolean),
);
for (const id of sourceStepIds) {
  if (!documentedStepIds.has(id)) fail(`stack step is undocumented: ${id}`);
}
for (const id of documentedStepIds) {
  if (!sourceStepIds.has(id))
    fail(`documented stack step does not exist: ${id}`);
}
// Node commands the step table says the stack runs are ones it does run.
const stackNodeCommands = new Set(
  [
    ...stackSource.matchAll(
      /\.node\(\s*(?:"[^"]*"|`[^`]*`)\s*,\s*\[\s*"([a-z0-9:-]+)"/g,
    ),
  ].map((match) => match[1]),
);
for (const [, ...cells] of stepRows) {
  for (const span of backticked(cells.join(" "))) {
    const command = span.split(" ")[0];
    if (!/^[a-z][a-z0-9:-]*$/.test(command) || sourceStepIds.has(command))
      continue;
    if (!declaredOperatorCommands.has(command)) continue;
    if (!stackNodeCommands.has(command))
      fail(
        `step table names a node command the stack does not run: ${command}`,
      );
  }
}

// DA order: the committee is ready before the producer preflight, and the node
// starts only after it. The runbook states that order; the source must keep it.
const runtimeSource = stackSources["runtime.ts"] ?? "";
const order = [
  '"committee-start"',
  '"committee readiness"',
  '"producer-da-preflight"',
  '"runtime-start"',
].map((marker) => [marker, runtimeSource.indexOf(marker)]);
if (
  order.some(([, at]) => at < 0) ||
  !order.every(([, at], index) => index === 0 || order[index - 1][1] < at)
) {
  fail(
    `stack DA order changed in runtime.ts (${order.map(([marker, at]) => `${marker}@${at}`).join(", ")}); update the runbook`,
  );
}
requireText(
  documents.live,
  "producer's DA preflight, and the node starts only after that preflight",
  "DA order statement",
);

// Every stop message the recovery table routes is one the stack can print.
const messageSource = `${stackSource}\n${nonceResumeSource}`;
for (const [first] of tableRows(
  documents.recovery,
  "## Route the stop message",
)) {
  for (const span of backticked(first)) {
    for (const fragment of span.replace(/\.\.\./g, "<>").split(/<[^>]*>/)) {
      if (fragment.trim().length >= 8 && !messageSource.includes(fragment))
        fail(
          `recovery routes a stop message the stack does not print: ${span}`,
        );
    }
  }
}
for (const marker of ["SignedNonceConflictError", "SignedNonceRejectedError"]) {
  requireText(nonceResumeSource, marker, "node nonce error");
  requireText(documents.recovery, marker, "nonce error route");
}

// Release readiness: the gates exist, and the reasons the runbook gives for
// the stack not satisfying them are still true.
const parseConstStringArray = (source, name, sourceLabel) => {
  const match = source.match(
    new RegExp(`export const ${name} = \\[([\\s\\S]*?)\\] as const`),
  );
  if (!match) {
    fail(`cannot parse ${name} from ${sourceLabel}`);
    return [];
  }
  return [...match[1].matchAll(/"([^"]+)"/g)].map((entry) => entry[1]);
};
const requiredStepIds = parseConstStringArray(
  finalizerSource,
  "REQUIRED_FRESH_E2E_STEP_IDS",
  "e2e-finalize-summary.ts",
);
const requiredTransactionLabels = parseConstStringArray(
  finalizerSource,
  "REQUIRED_FRESH_TRANSACTION_LABELS",
  "e2e-finalize-summary.ts",
);
const stateCorrectionGateLabels = parseConstStringArray(
  stateCorrectionSource,
  "REQUIRED_STATE_CORRECTION_GATE_LABELS",
  "e2e-state-correction-acceptance.ts",
);
const stateCorrectionRecoveryDrills = parseConstStringArray(
  stateCorrectionSource,
  "REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS",
  "e2e-state-correction-acceptance.ts",
);
const readiness = documents.releaseReadiness;
for (const name of [
  "REQUIRED_FRESH_E2E_STEP_IDS",
  "REQUIRED_FRESH_TRANSACTION_LABELS",
  "REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS",
]) {
  requireText(readiness, name, "release-readiness source reference");
}
for (const id of [
  "hub-oracle-nonce",
  "init-protocol",
  "await-automatic-merge",
]) {
  if (!requiredStepIds.includes(id))
    fail(
      `release-readiness cites a fresh step id the finalizer dropped: ${id}`,
    );
  if (sourceStepIds.has(id))
    fail(`the stack now runs step ${id}; revisit release-readiness.md`);
}
requireText(
  finalizerSource,
  "consumedDeposits === 1n",
  "finalizer single-deposit baseline cited by release-readiness.md",
);
for (const gate of stateCorrectionGateLabels) {
  requireText(readiness, gate, `state-correction gate ${gate}`);
}
for (const test of [
  "tests/e2e-state-correction-acceptance.test.ts",
  "tests/e2e-state-correction-reconciliation.test.ts",
  "tests/e2e-state-correction-local-authority.test.ts",
]) {
  requireText(readiness, test, "non-state-changing rehearsal");
}
if (stateCorrectionRecoveryDrills.length !== 22) {
  fail(
    `state-correction recovery matrix must have 22 cases; found ${stateCorrectionRecoveryDrills.length}`,
  );
}
requireText(readiness, "22 cases", "recovery matrix size");
for (const flag of [
  "--state-correction-evidence <path>",
  "--state-correction-deployment-manifest <path>",
  "--state-correction-blueprint <path>",
  "--state-correction-catalogue <path>",
  "--state-correction-parameters <path>",
  "--state-correction-workflow-journal <directory>",
  "--state-correction-l1-observation <path>",
  "--state-correction-recovery-observation <path>",
  "--state-correction-final-snapshot <path>",
]) {
  requireText(toolsCliSource, flag, "finalizer state-correction option");
  requireText(readiness, `\`${flag.split(" ")[0]}\``, "finalizer input");
}
for (const [text, needle, label] of [
  [finalizerSource, "stateCorrectionAcceptanceEvidence", "finalizer gate"],
  [
    stateCorrectionSource,
    "FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
    "canonical launch-scope source",
  ],
  [
    stateCorrectionAuthoritySource,
    "finalityPolicy: parseReleaseL1FinalityPolicy(manifest.l1Finality)",
    "manifest-bound release finality authority",
  ],
  [
    stateCorrectionAuthoritySource,
    "economicsPolicy: releaseEconomicsPolicyFromDeploymentManifest(manifest)",
    "manifest-bound release economics authority",
  ],
  [
    stateCorrectionAuthoritySource,
    "stateCorrectionValueDigest",
    "canonical live Q57 value digest",
  ],
]) {
  requireText(text, needle, label);
}
for (const marker of [
  "omitted family",
  "inexact slash",
  "incomplete recovery",
  "without public autonomous correction",
  "payout destination",
]) {
  requireText(
    stateCorrectionTestSource,
    marker,
    `negative rehearsal ${marker}`,
  );
}
for (const marker of [
  "live Kupo/Ogmios output disagreement",
  "fee does not equal the exact removal fee",
  "reserve value does not match",
]) {
  requireText(
    stateCorrectionAuthorityTestSource,
    marker,
    `local Q57 authority negative rehearsal ${marker}`,
  );
}

// Every shell block parses.
const bashBlocks = [];
for (const [name, text] of Object.entries(documents)) {
  for (const match of text.matchAll(/```bash\n([\s\S]*?)\n```/g)) {
    bashBlocks.push({ name, body: match[1] });
  }
}
for (const [index, block] of bashBlocks.entries()) {
  const sanitized = block.body.replace(/<[^>\n]+>/g, "placeholder");
  const result = spawnSync("bash", ["-n", "-c", sanitized], {
    encoding: "utf8",
    timeout: 2000,
  });
  if (result.error?.code === "ETIMEDOUT") {
    fail(`${block.name} bash block ${index + 1} timed out in bash -n`);
  } else if (result.status !== 0) {
    fail(
      `${block.name} bash block ${index + 1} fails bash -n: ${result.stderr.trim()}`,
    );
  }
}

if (failures.length > 0) {
  process.stderr.write(
    `Midgard E2E skill validation failed (${failures.length}):\n${failures
      .map((entry) => `- ${entry}`)
      .join("\n")}\n`,
  );
  process.exit(1);
}

process.stdout.write(
  JSON.stringify(
    {
      status: "ok",
      skillLines,
      referencedCommandCount:
        referencedOperatorCommands.size + referencedToolsCommands.size,
      stackFlags: [...stackFlags],
      stackStepIds: [...sourceStepIds],
      stackNodeCommands: [...stackNodeCommands],
      finalizerRequiredStepIds: requiredStepIds,
      finalizerRequiredTransactionLabels: requiredTransactionLabels,
      stateCorrectionGateLabels,
      stateCorrectionRecoveryDrillCount: stateCorrectionRecoveryDrills.length,
      bashBlockCount: bashBlocks.length,
    },
    null,
    2,
  ) + "\n",
);
