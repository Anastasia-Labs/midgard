#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const skillDir = resolve(scriptDir, "..");
const repoRoot = resolve(skillDir, "../../..");

const paths = {
  skill: join(skillDir, "SKILL.md"),
  live: join(skillDir, "references/live-acceptance.md"),
  recovery: join(skillDir, "references/recovery.md"),
  benchmark: join(skillDir, "references/benchmark.md"),
  cli: join(repoRoot, "demo/midgard-node/src/index.ts"),
  // The e2e step runner, service supervisor, finalizer, and stress commands
  // are registered in the tooling binary, not the operator binary.
  toolsCli: join(repoRoot, "demo/midgard-node-tools/src/index.ts"),
  finalizer: join(
    repoRoot,
    "demo/midgard-node-tools/src/commands/e2e-finalize-summary.ts",
  ),
  stateCorrection: join(
    repoRoot,
    "demo/midgard-node-tools/src/commands/e2e-state-correction-acceptance.ts",
  ),
  stateCorrectionTest: join(
    repoRoot,
    "demo/midgard-node-tools/tests/e2e-state-correction-acceptance.test.ts",
  ),
  stateCorrectionAuthority: join(
    repoRoot,
    "demo/midgard-node-tools/src/commands/e2e-state-correction-local-authority.ts",
  ),
  stateCorrectionAuthorityTest: join(
    repoRoot,
    "demo/midgard-node-tools/tests/e2e-state-correction-local-authority.test.ts",
  ),
};

const failures = [];
const fail = (message) => failures.push(message);
const read = (path) => {
  try {
    return readFileSync(path, "utf8");
  } catch (error) {
    fail(`cannot read ${path}: ${error.message}`);
    return "";
  }
};

const documents = Object.fromEntries(
  ["skill", "live", "recovery", "benchmark"].map((name) => [
    name,
    read(paths[name]),
  ]),
);
const cliSource = read(paths.cli);
const toolsCliSource = read(paths.toolsCli);
const finalizerSource = read(paths.finalizer);
const stateCorrectionSource = read(paths.stateCorrection);
const stateCorrectionTestSource = read(paths.stateCorrectionTest);
const stateCorrectionAuthoritySource = read(paths.stateCorrectionAuthority);
const stateCorrectionAuthorityTestSource = read(
  paths.stateCorrectionAuthorityTest,
);
const allDocs = Object.values(documents).join("\n");

const requireText = (text, needle, label) => {
  if (!text.includes(needle)) fail(`missing ${label}: ${needle}`);
};

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
  if (name !== "skill" && text.split("\n").length > 100) {
    requireText(text, "## Contents", `${name} table of contents`);
  }
}

requireText(
  documents.skill,
  "references/live-acceptance.md",
  "live runbook route",
);
requireText(documents.skill, "references/recovery.md", "recovery route");
requireText(documents.skill, "references/benchmark.md", "benchmark route");

for (const forbidden of [
  "logs/phase-1-full-corpus-",
  "logs/phase-1-live-acceptance/",
  "preprod-da-2of3/secrets/l1-submitter.seed",
  "--mode fresh-redeploy",
]) {
  if (allDocs.includes(forbidden))
    fail(`forbidden stale instruction: ${forbidden}`);
}

const commandsDeclaredIn = (source) =>
  new Set(
    [...source.matchAll(/\.command\("([a-zA-Z0-9:_-]+)"\)/g)].map(
      (match) => match[1],
    ),
  );
const declaredOperatorCommands = commandsDeclaredIn(cliSource);
const declaredToolsCommands = commandsDeclaredIn(toolsCliSource);
// Operator commands are invoked as `node dist/index.js <command>` from the
// node directory; tooling commands as `node "$TOOLS_CLI" <command>`.
const referencedOperatorCommands = new Set(
  [...allDocs.matchAll(/node dist\/index\.js\s+([a-zA-Z0-9:_-]+)/g)].map(
    (match) => match[1],
  ),
);
const referencedToolsCommands = new Set(
  [...allDocs.matchAll(/node "\$TOOLS_CLI"\s+([a-zA-Z0-9:_-]+)/g)].map(
    (match) => match[1],
  ),
);
for (const command of referencedOperatorCommands) {
  const dynamicReferenceScriptCommand =
    command.startsWith("deploy-reference-script-") &&
    cliSource.includes("deploy-reference-script-${commandName}");
  if (
    !declaredOperatorCommands.has(command) &&
    !dynamicReferenceScriptCommand
  ) {
    fail(
      declaredToolsCommands.has(command)
        ? `documented as an operator command but declared by midgard-node-tools; invoke it through "$TOOLS_CLI": ${command}`
        : `documented Midgard CLI command is not declared: ${command}`,
    );
  }
}
for (const command of referencedToolsCommands) {
  if (!declaredToolsCommands.has(command)) {
    fail(`documented midgard-node-tools command is not declared: ${command}`);
  }
}

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

for (const [text, needle, label] of [
  [
    toolsCliSource,
    "--state-correction-evidence <path>",
    "state-correction CLI option",
  ],
  ...[
    "--state-correction-deployment-manifest <path>",
    "--state-correction-blueprint <path>",
    "--state-correction-catalogue <path>",
    "--state-correction-parameters <path>",
    "--state-correction-release-evidence <path>",
    "--state-correction-workflow-journal <directory>",
    "--state-correction-l1-observation <path>",
    "--state-correction-recovery-observation <path>",
    "--state-correction-final-snapshot <path>",
  ].map((flag) => [
    toolsCliSource,
    flag,
    `independent source CLI option ${flag}`,
  ]),
  [
    finalizerSource,
    "stateCorrectionAcceptanceEvidence",
    "state-correction finalizer gate",
  ],
  [
    stateCorrectionSource,
    "FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
    "canonical launch-scope source",
  ],
  [
    documents.live,
    '--state-correction-evidence "$STATE_CORRECTION_EVIDENCE"',
    "state-correction dashboard argument",
  ],
  [
    documents.live,
    "tests/e2e-state-correction-acceptance.test.ts",
    "non-state-changing state-correction rehearsal",
  ],
  [
    documents.live,
    "tests/e2e-state-correction-reconciliation.test.ts",
    "non-state-changing independent reconciliation rehearsal",
  ],
  [
    documents.live,
    "tests/e2e-state-correction-local-authority.test.ts",
    "non-state-changing local Kupmios authority rehearsal",
  ],
  [
    stateCorrectionAuthoritySource,
    "stateCorrectionValueDigest",
    "canonical live Q57 value digest",
  ],
  [
    stateCorrectionAuthoritySource,
    "live Kupo/Ogmios output disagreement",
    "live Q57 cross-source economic comparison",
  ],
  [
    documents.live,
    "REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS",
    "recovery matrix source",
  ],
]) {
  requireText(text, needle, label);
}

for (const gate of stateCorrectionGateLabels) {
  requireText(
    documents.skill,
    gate,
    `state-correction acceptance gate ${gate}`,
  );
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
    `state-correction negative rehearsal ${marker}`,
  );
}
if (stateCorrectionRecoveryDrills.length !== 22) {
  fail(
    `state-correction recovery matrix must have 22 cases; found ${stateCorrectionRecoveryDrills.length}`,
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

const summaryStart = documents.live.indexOf("STEP_SUMMARY_ARGS=()");
const summaryEnd = documents.live.indexOf("TX_ARGS=()", summaryStart);
if (summaryStart < 0 || summaryEnd < 0) {
  fail("cannot find final STEP_SUMMARY_ARGS block");
} else {
  const summaryBlock = documents.live.slice(summaryStart, summaryEnd);
  for (const stepId of requiredStepIds) {
    const escaped = stepId.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    const stepMatch = documents.live.match(
      new RegExp(
        `--id\\s+${escaped}[\\s\\S]{0,500}?--summary-out\\s+"\\$([A-Z0-9_]+)"`,
      ),
    );
    if (!stepMatch) {
      fail(
        `required fresh step lacks a documented structured runner: ${stepId}`,
      );
      continue;
    }
    const variable = stepMatch[1];
    if (!summaryBlock.includes(`"$${variable}"`)) {
      fail(`required fresh step summary is omitted from dashboard: ${stepId}`);
    }
  }
}

for (const label of requiredTransactionLabels) {
  if (!documents.live.includes(`append_tx_arg ${label} `)) {
    fail(`required transaction label is omitted from dashboard: ${label}`);
  }
}

for (const marker of [
  "--target producer",
  "--profile producer-container-watcher-host",
  "--profile host",
  "--target watcher",
  "$PRODUCER_PREFLIGHT_MANIFEST",
  "DA_L1_SUBMITTER_KEY_SOURCE",
]) {
  requireText(documents.live, marker, "DA workflow marker");
}

const daSection = documents.live.slice(
  documents.live.indexOf("## DA manifests and watcher"),
);
const watcherStart = daSection.indexOf("e2e-start-service");
const bindPreflight = daSection.indexOf("--id da-libp2p-bind-listen-preflight");
const nodeStart = daSection.indexOf("$COMPOSE up -d midgard-node");
if (
  watcherStart < 0 ||
  bindPreflight < 0 ||
  nodeStart < 0 ||
  !(watcherStart < bindPreflight && bindPreflight < nodeStart)
) {
  fail(
    "DA order must be watcher start, bind/listen preflight, then node start",
  );
}

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
      requiredStepIds,
      requiredTransactionLabels,
      stateCorrectionGateLabels,
      stateCorrectionRecoveryDrillCount: stateCorrectionRecoveryDrills.length,
      bashBlockCount: bashBlocks.length,
    },
    null,
    2,
  ) + "\n",
);
