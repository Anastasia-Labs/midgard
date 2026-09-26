#!/usr/bin/env node

// Shrinks baseline.json to the violations that still occur.
//
// Lints the demo workspace with the Midgard rules only and without the
// baseline, then drops every baselined site that no longer occurs. It never
// adds an entry or a site: a new violation is fixed, or added to the baseline
// by hand with a reason.
//
// Usage (from demo/): node scripts/lib/eslint-plugin-midgard/prune-baseline.mjs [--check]
//   --check  write nothing; exit 1 if the baseline would shrink.
// Exit codes: 0 done (or nothing to prune), 1 --check found stale sites,
// 2 the lint could not run.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join, relative, sep } from "node:path";
import { fileURLToPath } from "node:url";

import {
  BASELINE_ENV,
  BASELINE_PATH,
  fingerprint,
  pruneBaseline,
} from "./baseline.mjs";
import { PLUGIN_NAME } from "./index.mjs";

const demoRoot = join(dirname(fileURLToPath(import.meta.url)), "../../..");

/**
 * Every violation of the Midgard rules in the workspace, as
 * `{ [ruleId]: { [file]: [site, ...] } }`. The type-aware parser setup is
 * switched off: none of these rules needs type information.
 */
export const collectViolations = async () => {
  process.env[BASELINE_ENV] = "off";
  const { ESLint } = await import("eslint");
  const eslint = new ESLint({
    cwd: demoRoot,
    ruleFilter: ({ ruleId }) => ruleId.startsWith(`${PLUGIN_NAME}/`),
    overrideConfig: [
      {
        files: ["**/*.{ts,tsx}"],
        languageOptions: {
          parserOptions: { projectService: false, project: null },
        },
      },
    ],
  });
  const results = await eslint.lintFiles(["."]);
  const violations = {};
  for (const result of results) {
    const fatal = result.messages.find((message) => message.fatal);
    if (fatal !== undefined) {
      throw new Error(`${result.filePath}: ${fatal.message}`);
    }
    const file = relative(demoRoot, result.filePath).split(sep).join("/");
    const lines = (
      result.source ?? readFileSync(result.filePath, "utf8")
    ).split(/\r?\n/u);
    for (const message of result.messages) {
      if (!message.ruleId?.startsWith(`${PLUGIN_NAME}/`)) continue;
      violations[message.ruleId] ??= {};
      violations[message.ruleId][file] ??= [];
      violations[message.ruleId][file].push(
        fingerprint(lines[message.line - 1] ?? ""),
      );
    }
  }
  return violations;
};

/** The baseline as the file stores it: JSON, formatted as Prettier would. */
export const formatBaseline = async (baseline) => {
  const prettier = await import("prettier");
  const options = (await prettier.resolveConfig(BASELINE_PATH)) ?? {};
  return prettier.format(JSON.stringify(baseline), {
    ...options,
    filepath: BASELINE_PATH,
  });
};

const main = async (argv) => {
  const check = argv.includes("--check");
  const baseline = JSON.parse(readFileSync(BASELINE_PATH, "utf8"));
  let violations;
  try {
    violations = await collectViolations();
  } catch (error) {
    console.error(`prune-baseline: could not lint: ${error.message}`);
    return 2;
  }
  const { pruned, removed } = pruneBaseline(baseline, violations);
  if (removed === 0) {
    console.log("prune-baseline: every baselined site still occurs");
    return 0;
  }
  if (check) {
    console.error(
      `prune-baseline: ${removed} baselined site(s) no longer occur; run without --check to drop them`,
    );
    return 1;
  }
  writeFileSync(BASELINE_PATH, await formatBaseline(pruned));
  console.log(
    `prune-baseline: removed ${removed} site(s) that no longer occur`,
  );
  return 0;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  process.exitCode = await main(process.argv.slice(2));
}
