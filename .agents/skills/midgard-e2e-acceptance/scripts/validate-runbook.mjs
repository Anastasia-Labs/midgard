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
  finalizer: join(
    repoRoot,
    "demo/midgard-node/src/commands/e2e-finalize-summary.ts",
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
const finalizerSource = read(paths.finalizer);
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

const declaredCommands = new Set(
  [...cliSource.matchAll(/\.command\("([a-zA-Z0-9:_-]+)"\)/g)].map(
    (match) => match[1],
  ),
);
const referencedCommands = new Set(
  [...allDocs.matchAll(/node dist\/index\.js\s+([a-zA-Z0-9:_-]+)/g)].map(
    (match) => match[1],
  ),
);
for (const command of referencedCommands) {
  const dynamicReferenceScriptCommand =
    command.startsWith("deploy-reference-script-") &&
    cliSource.includes("deploy-reference-script-${commandName}");
  if (!declaredCommands.has(command) && !dynamicReferenceScriptCommand) {
    fail(`documented Midgard CLI command is not declared: ${command}`);
  }
}

const parseConstStringArray = (source, name) => {
  const match = source.match(
    new RegExp(`export const ${name} = \\[([\\s\\S]*?)\\] as const`),
  );
  if (!match) {
    fail(`cannot parse ${name} from e2e-finalize-summary.ts`);
    return [];
  }
  return [...match[1].matchAll(/"([^"]+)"/g)].map((entry) => entry[1]);
};

const requiredStepIds = parseConstStringArray(
  finalizerSource,
  "REQUIRED_FRESH_E2E_STEP_IDS",
);
const requiredTransactionLabels = parseConstStringArray(
  finalizerSource,
  "REQUIRED_FRESH_TRANSACTION_LABELS",
);

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
      referencedCommandCount: referencedCommands.size,
      requiredStepIds,
      requiredTransactionLabels,
      bashBlockCount: bashBlocks.length,
    },
    null,
    2,
  ) + "\n",
);
