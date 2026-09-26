#!/usr/bin/env node

// Runs the local checks a change can break, selected from the diff against
// the base branch. The doctrine, the check list and every flag are in
// docs/agents/required-checks.md (generated from scripts/preflight/registry.mjs).
//
// usage: node scripts/preflight.mjs [--strict | --pre-push] [--base <ref>]
//                                   [--full] [--list] [--json]
//        node scripts/preflight.mjs --write-docs | --check-docs
// Exit 0 all passed, 1 a check failed, 2 usage error, 3 a check was skipped
// for a missing capability and nothing failed.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  REQUIRED_CHECKS_DOC,
  renderRequiredChecks,
} from "./preflight/docs.mjs";
import { createProbeSet } from "./preflight/probes.mjs";
import {
  buildRegistry,
  formatCommand as formatStep,
  FULL_RUN_ENV,
} from "./preflight/registry.mjs";
import {
  collectChanges,
  EXIT,
  planPreflight,
  resolveBase,
  runPreflight,
  UsageError,
} from "./preflight/run.mjs";

export const JSON_SCHEMA = "midgard-preflight/v1";

const USAGE = `usage: node scripts/preflight.mjs [--strict | --pre-push] [--base <ref>] [--full] [--list] [--json]
       node scripts/preflight.mjs --write-docs | --check-docs`;

export const parseArguments = (argv) => {
  const options = {
    strict: false,
    prePush: false,
    base: undefined,
    full: false,
    list: false,
    json: false,
    docs: undefined,
  };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    switch (arg) {
      case "--strict":
        options.strict = true;
        break;
      case "--pre-push":
        options.prePush = true;
        options.strict = true;
        break;
      case "--full":
        options.full = true;
        break;
      case "--list":
        options.list = true;
        break;
      case "--json":
        options.json = true;
        break;
      case "--write-docs":
      case "--check-docs":
        if (options.docs !== undefined) {
          throw new UsageError("--write-docs and --check-docs are exclusive");
        }
        options.docs = arg.slice(2, -"-docs".length);
        break;
      case "--base": {
        const value = argv[index + 1];
        if (value === undefined || value.startsWith("--")) {
          throw new UsageError("--base needs a ref");
        }
        options.base = value;
        index += 1;
        break;
      }
      case "--help":
      case "-h":
        options.help = true;
        break;
      default:
        throw new UsageError(`unknown argument '${arg}'`);
    }
  }
  if (options.docs !== undefined && argv.length !== 1) {
    throw new UsageError(`--${options.docs}-docs takes no other argument`);
  }
  return options;
};

// Doc mode: the committed file must equal what the registry renders.
export const docsCommand = (
  root,
  mode,
  { stdout, stderr, path = resolve(root, REQUIRED_CHECKS_DOC) },
) => {
  const rendered = renderRequiredChecks(buildRegistry(root));
  if (mode === "write") {
    writeFileSync(path, rendered);
    stdout(`wrote ${REQUIRED_CHECKS_DOC}\n`);
    return EXIT.passed;
  }
  let committed;
  try {
    committed = readFileSync(path, "utf8");
  } catch {
    committed = undefined;
  }
  if (committed === rendered) {
    stdout(`${REQUIRED_CHECKS_DOC} is current\n`);
    return EXIT.passed;
  }
  stderr(
    [
      `${REQUIRED_CHECKS_DOC} ${committed === undefined ? "is missing" : "does not match the preflight registry"}.`,
      "Fix:",
      "  1. Regenerate it: node scripts/preflight.mjs --write-docs",
      `  2. Stage it by explicit path: git add ${REQUIRED_CHECKS_DOC}`,
      "",
    ].join("\n"),
  );
  return EXIT.failed;
};

const numbered = (steps) =>
  steps.map((text, index) => `  ${String(index + 1)}. ${text}`).join("\n");

const summaryLine = (result) =>
  `${result.status.toUpperCase().padEnd(7)} ${result.id}${result.reason ? ` — ${result.reason}` : ""}${result.status === "failed" && result.fix ? `\n          fix: ${result.fix}` : ""}`;

export const main = async (
  argv,
  {
    root = resolve(dirname(fileURLToPath(import.meta.url)), ".."),
    env = process.env,
    stdout = (text) => process.stdout.write(text),
    stderr = (text) => process.stderr.write(text),
    probes,
    runStep,
  } = {},
) => {
  let options;
  try {
    options = parseArguments(argv);
  } catch (error) {
    stderr(`${error.message}\n${USAGE}\n`);
    return EXIT.usage;
  }
  if (options.help) {
    stdout(`${USAGE}\nSee ${REQUIRED_CHECKS_DOC}.\n`);
    return EXIT.passed;
  }
  if (options.docs !== undefined) {
    return docsCommand(root, options.docs, { stdout, stderr });
  }

  // With --json, stdout carries exactly one JSON document; everything a human
  // reads goes to stderr.
  const say = options.json ? stderr : stdout;
  let base;
  let changes;
  let registry;
  try {
    base = resolveBase(root, options.base);
    changes = collectChanges(root, { base, strict: options.strict });
    registry = buildRegistry(root);
  } catch (error) {
    stderr(`preflight: ${error.message}\n`);
    return EXIT.usage;
  }
  const killSwitch = env[FULL_RUN_ENV] === "1";
  const plan = planPreflight(registry, changes.changed, {
    full: options.full || killSwitch,
    fullReasons: [
      ...(options.full ? ["--full"] : []),
      ...(killSwitch ? [`${FULL_RUN_ENV}=1`] : []),
      ...changes.fullReasons,
    ],
    prePush: options.prePush,
  });

  say(
    `preflight: ${String(changes.changed.length)} changed file(s) against ${base} (merge-base ${changes.mergeBase.slice(0, 12)}), ${options.strict ? "committed changes only" : "working tree"}\n`,
  );
  if (plan.full) {
    say(`full run: ${plan.fullReasons.join("; ")}\n`);
  }
  if (changes.dirty) {
    say(
      "note: the working tree has uncommitted changes to tracked files; --strict judged HEAD only\n",
    );
  }

  let results = [];
  let exitCode = EXIT.passed;
  if (options.list) {
    for (const { check, matched, steps } of plan.planned) {
      say(
        `would run ${check.id}${check.warnOnly ? " (warn only)" : ""}${check.capabilities.length > 0 ? ` [needs ${check.capabilities.join(", ")}]` : ""}\n`,
      );
      if (matched.length > 0 && !plan.full) {
        say(
          `    selected by ${matched.slice(0, 5).join(", ")}${matched.length > 5 ? `, and ${String(matched.length - 5)} more` : ""}\n`,
        );
      }
      for (const step of steps) {
        say(`    $ ${formatStep(step)}\n`);
      }
    }
  } else {
    const run = await runPreflight({
      root,
      plan,
      probes: probes ?? createProbeSet({ root, env }),
      base,
      env,
      log: stderr,
      ...(runStep === undefined ? {} : { runStep }),
    });
    results = run.results;
    exitCode = run.exitCode;
    say("\npreflight summary\n");
    for (const result of results) {
      say(`  ${summaryLine(result)}\n`);
    }
  }

  for (const advisory of plan.advisories) {
    say(
      `\nadvice (${advisory.id}): ${advisory.message}\n${numbered(advisory.steps)}\n`,
    );
  }
  if (plan.uncovered.length > 0) {
    say(
      `\nno check covers ${String(plan.uncovered.length)} changed file(s): ${plan.uncovered.join(", ")}\n`,
    );
  }
  say(
    "\nSelection is not the final gate: CI runs every check before merge. See docs/agents/required-checks.md.\n",
  );
  if (!options.list) {
    say(
      exitCode === EXIT.passed
        ? "preflight passed\n"
        : exitCode === EXIT.failed
          ? "preflight FAILED\n"
          : "preflight incomplete: nothing failed, but some checks could not run (exit 3)\n",
    );
  }

  if (options.json) {
    stdout(
      `${JSON.stringify(
        {
          schema: JSON_SCHEMA,
          base,
          mergeBase: changes.mergeBase,
          strict: options.strict,
          prePush: options.prePush,
          full: plan.full,
          fullReasons: plan.fullReasons,
          changedFiles: changes.changed,
          // A dry run executes nothing, so checks[] stays empty and the plan
          // is reported separately.
          checks: results,
          planned: plan.planned.map(({ check, matched, steps }) => ({
            id: check.id,
            command: steps.map(formatStep).join(" && ") || check.display,
            capabilities: check.capabilities,
            warnOnly: check.warnOnly,
            matched,
          })),
          advisories: plan.advisories,
          uncovered: plan.uncovered,
          exitCode,
        },
        null,
        2,
      )}\n`,
    );
  }
  return exitCode;
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  process.exitCode = await main(process.argv.slice(2));
}
