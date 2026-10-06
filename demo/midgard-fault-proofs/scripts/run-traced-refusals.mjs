#!/usr/bin/env node
/**
 * Run every pinned emulator negative against validators that carry their
 * verbose traces, so each pin checks the exact on-chain check that refuses it.
 *
 * A plain build carries no traces, so the default run can only see that some
 * validator refused. A negative pins its refusal with
 * `expectOnchainRefusal(build, { refusedBy: "<module>", check: /<trace>/ })`;
 * this script reads every `refusedBy` literal under tests/, in test files and
 * support modules alike, and the `it` cases that reach each one
 * (scripts/traced-refusal-plan.mjs), and builds the blueprint again with
 * verbose traces. For each named module it swaps that module alone into the
 * plain blueprint and runs the cases that reach its pins against the result.
 * Every other validator keeps its plain code and hash.
 *
 * Only the cases that reach a pin run, one run per pinned module, and in each
 * run that module is the only traced one. A plain validator's failure carries
 * no trace, so a traced refusal names the module that failed: a pin whose
 * transaction another validator refuses arrives untraced and fails. Each
 * checked pin is recorded with the file that called `expectOnchainRefusal`,
 * and the run fails unless every declared pin was checked against a trace
 * from the file that declares it: a pin whose case is skipped, never planned
 * or whose refusal arrives untraced cannot pass silently.
 *
 *   node scripts/run-traced-refusals.mjs [file that declares a pin ...]
 *
 * With no arguments it checks every pin. Each case runs in its test file's
 * Vitest project against that project's blueprint: the testing-profile files
 * need onchain/aiken/plutus.json fresh (`pnpm --dir demo deployment:build
 * <profile>`), and the interactive-emulator files use the blueprint that
 * project stamps for itself. Each traced build uses its plain blueprint's
 * profile and is cached by scripts/traced-blueprint.mjs under
 * onchain/aiken/build/traced-refusals until the sources or compiler change.
 */
import { spawn } from "node:child_process";
import { globSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  blueprintHash,
  buildRecordPath,
  checkBlueprintStamp,
} from "../../scripts/lib/blueprint-stamp.mjs";
import prepareInteractiveBlueprint, {
  interactiveEmulatorBlueprint,
} from "../../midgard-test-support/interactive-emulator.js";
import { interactiveTests } from "../vitest.interactive-tests.mjs";
import {
  tracedBlueprint as buildTracedBlueprint,
  tracedBuildDirectory as buildDirectory,
} from "./traced-blueprint.mjs";
import { caseNamePattern, tracedRefusalPlan } from "./traced-refusal-plan.mjs";

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const plainBlueprint = resolve(packageRoot, "../../onchain/aiken/plutus.json");
const checkedPinsLog = resolve(buildDirectory, "checked-pins.jsonl");

const fail = (message) => {
  console.error(`[traced-refusals] ${message}`);
  process.exit(1);
};

/** A project's plain blueprint's build record, once the blueprint is fresh. */
const plainRecord = (blueprintPath) => {
  const verdict = checkBlueprintStamp({ blueprintPath });
  if (verdict.status !== "fresh") {
    fail(
      `${verdict.detail}${verdict.fix === null ? "" : `\nRebuild it: ${verdict.fix}`}`,
    );
  }
  return JSON.parse(readFileSync(buildRecordPath(blueprintPath), "utf8"));
};

/**
 * A project's plain blueprint with every handler of one module traced,
 * written next to its traced build. Its build record is the plain one's,
 * rebound to the overlay's bytes: both come from the same sources, compiler
 * and profile.
 */
const writeOverlay = (plainBlueprint, module) => {
  const record = plainRecord(plainBlueprint);
  let tracedBlueprint;
  try {
    tracedBlueprint = buildTracedBlueprint(record.profile.name);
  } catch (error) {
    fail(error instanceof Error ? error.message : String(error));
  }
  const overlayBlueprint = resolve(
    dirname(tracedBlueprint),
    `overlay.${module.replaceAll("/", ".")}.json`,
  );
  const plain = JSON.parse(readFileSync(plainBlueprint, "utf8"));
  const traced = new Map(
    JSON.parse(readFileSync(tracedBlueprint, "utf8")).validators.map(
      (validator) => [validator.title, validator],
    ),
  );
  const moduleOf = (title) => title.slice(0, title.indexOf("."));
  const swapped = plain.validators.filter(
    (validator) => moduleOf(validator.title) === module,
  );
  if (swapped.length === 0) fail(`refusedBy names no validator: ${module}`);
  for (const validator of swapped) {
    const replacement = traced.get(validator.title);
    if (replacement === undefined) {
      fail(`traced blueprint lacks ${validator.title}`);
    }
    validator.compiledCode = replacement.compiledCode;
    validator.hash = replacement.hash;
  }
  writeFileSync(overlayBlueprint, JSON.stringify(plain, null, 2) + "\n");
  writeFileSync(
    buildRecordPath(overlayBlueprint),
    JSON.stringify(
      { ...record, blueprintHash: blueprintHash(overlayBlueprint) },
      null,
      2,
    ) + "\n",
  );
  return overlayBlueprint;
};

/** A vitest name filter matching exactly the planned cases. */
const casePattern = (sites) =>
  `(?:${[...new Set(sites.map(caseNamePattern))].join("|")})$`;

let allPins;
try {
  allPins = tracedRefusalPlan(packageRoot);
} catch (error) {
  fail(error instanceof Error ? error.message : String(error));
}
const requested = process.argv
  .slice(2)
  .map((file) => relative(packageRoot, resolve(file)));
for (const file of requested) {
  if (!allPins.some((pin) => pin.file === file)) {
    fail(`${file} declares no refusedBy pin`);
  }
}
const pins =
  requested.length > 0
    ? allPins.filter((pin) => requested.includes(pin.file))
    : allPins;
if (pins.length === 0) fail("no file declares a refusedBy pin");

// Each Vitest project runs its files against its own blueprint, with the
// modules its pins name swapped for their traced builds.
const interactiveFiles = new Set(
  globSync(
    interactiveTests.map((pattern) => pattern.replace(/^\.\//u, "")),
    { cwd: packageRoot },
  ),
);
const sitesIn = (interactive) =>
  pins.flatMap(({ module, sites }) =>
    sites
      .filter((site) => interactiveFiles.has(site.file) === interactive)
      .map((site) => ({ ...site, module })),
  );
const projects = [
  {
    name: "testing-profile",
    // workspaceBundleProjects routes the files that need module-by-module
    // loading to `testing-profile:source`; a `--project` filter must name
    // both, or such a file is reported as "No test files found".
    projects: ["testing-profile", "testing-profile:source"],
    sites: sitesIn(false),
    blueprint: () => plainBlueprint,
    overlayVariable: "MIDGARD_REAL_BLUEPRINT_PATH",
  },
  {
    name: "interactive-emulator",
    projects: ["interactive-emulator"],
    sites: sitesIn(true),
    blueprint: async () => {
      await prepareInteractiveBlueprint();
      return interactiveEmulatorBlueprint;
    },
    overlayVariable: "MIDGARD_TRACED_INTERACTIVE_BLUEPRINT",
  },
].filter((project) => project.sites.length > 0);

/** At most this many Vitest runs at once; MIDGARD_TRACED_REFUSALS_JOBS overrides. */
const jobs = Math.max(
  1,
  Number.parseInt(process.env.MIDGARD_TRACED_REFUSALS_JOBS ?? "4", 10) || 1,
);

/** One Vitest run, its output held and printed whole when it exits. */
const runVitest = ({ label, args, env }) =>
  new Promise((resolveRun, rejectRun) => {
    const child = spawn(
      resolve(packageRoot, "node_modules/.bin/vitest"),
      args,
      { cwd: packageRoot, env, stdio: ["ignore", "pipe", "pipe"] },
    );
    const output = [];
    child.stdout.on("data", (chunk) => output.push(chunk));
    child.stderr.on("data", (chunk) => output.push(chunk));
    child.on("error", rejectRun);
    child.on("close", (status) => {
      process.stdout.write(
        `\n[traced-refusals] ${label}\n${Buffer.concat(output).toString()}`,
      );
      resolveRun(status);
    });
  });

mkdirSync(buildDirectory, { recursive: true });
writeFileSync(checkedPinsLog, "");
const runs = [];
for (const project of projects) {
  const blueprint = await project.blueprint();
  const modules = new Map();
  for (const site of project.sites) {
    const run = modules.get(site.module) ?? { files: new Set(), sites: [] };
    run.files.add(site.file);
    run.sites.push(site);
    modules.set(site.module, run);
  }
  for (const [module, { files: moduleFiles, sites }] of modules) {
    // Only this module is traced, so a traced refusal is its own.
    const overlay = writeOverlay(blueprint, module);
    runs.push({
      label: `${project.name}: ${module}`,
      args: [
        "run",
        ...project.projects.flatMap((name) => ["--project", name]),
        "-t",
        casePattern(sites),
        ...[...moduleFiles].sort(),
      ],
      env: {
        ...process.env,
        [project.overlayVariable]: overlay,
        MIDGARD_EMULATOR_TRACED_REFUSALS: "1",
        MIDGARD_TRACED_REFUSAL_MODULE: module,
        MIDGARD_TRACED_REFUSALS_LOG: checkedPinsLog,
      },
    });
  }
}
const failed = [];
let next = 0;
await Promise.all(
  Array.from({ length: Math.min(jobs, runs.length) }, async () => {
    while (next < runs.length) {
      const run = runs[next];
      next += 1;
      const status = await runVitest(run);
      if (status !== 0) failed.push(`${run.label}: vitest exited ${status}`);
    }
  }),
);
if (failed.length > 0) fail(failed.join("\n"));

// Every declared pin must have been checked against a trace from the file
// that declares it, and every checked pin must be one this script read.
const pinKey = ({ file, module }) => `${file} ${module}`;
const checked = readFileSync(checkedPinsLog, "utf8")
  .split("\n")
  .filter((line) => line.length > 0)
  .map((line) => JSON.parse(line))
  .map(({ site, refusedBy }) => pinKey({ file: site, module: refusedBy }));
const declared = [...new Set(pins.map(pinKey))];
const known = new Set(allPins.map(pinKey));
const unchecked = declared.filter((pin) => !checked.includes(pin));
const undeclared = [...new Set(checked)].filter((pin) => !known.has(pin));
if (unchecked.length > 0) {
  fail(`pins never checked against a trace:\n  ${unchecked.join("\n  ")}`);
}
if (undeclared.length > 0) {
  fail(
    `pins checked but not declared as a refusedBy string literal in the file that calls expectOnchainRefusal:\n  ${undeclared.join("\n  ")}`,
  );
}
console.log(
  `[traced-refusals] ${declared.length.toString()} pins in ${new Set(pins.map(({ file }) => file)).size.toString()} files checked against traces`,
);
