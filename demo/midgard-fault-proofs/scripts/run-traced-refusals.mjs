#!/usr/bin/env node
/**
 * Run every pinned emulator negative against validators that carry their
 * verbose traces, so each pin checks the exact on-chain check that refuses it.
 *
 * A plain build carries no traces, so the default run can only see that some
 * validator refused. A negative pins its refusal with
 * `expectOnchainRefusal(build, { refusedBy: "<module>", check: /<trace>/ })`;
 * this script reads every `refusedBy` literal from the test files, builds the
 * blueprint again with verbose traces, swaps exactly those modules into the
 * plain blueprint, and runs the pinned negatives against the result. Every
 * other validator keeps its plain code and hash.
 *
 * Only the cases holding a pin run. The run fails unless every declared pin
 * was checked against a trace: a pin whose case is skipped, or whose refusal
 * arrives untraced, cannot pass silently.
 *
 *   node scripts/run-traced-refusals.mjs [test file ...]
 *
 * With no arguments it runs every file that declares a pin. Each file runs in
 * its own Vitest project against that project's blueprint: the testing-profile
 * files need onchain/aiken/plutus.json fresh (`pnpm --dir demo
 * deployment:build <profile>`), and the interactive-emulator files use the
 * blueprint that project stamps for itself. Each traced build uses its plain
 * blueprint's profile and is cached under onchain/aiken/build/traced-refusals
 * until the sources or compiler change.
 */
import { spawnSync } from "node:child_process";
import {
  existsSync,
  globSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  writeFileSync,
} from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  assertPinnedAiken,
  defaultAikenBinary,
} from "../../../onchain/aiken/scripts/pinned-compiler.mjs";
import {
  blueprintHash,
  blueprintSourceHash,
  buildRecordPath,
  checkBlueprintStamp,
} from "../../scripts/lib/blueprint-stamp.mjs";
import prepareInteractiveBlueprint, {
  interactiveEmulatorBlueprint,
} from "../../midgard-test-support/interactive-emulator.js";
import { interactiveTests } from "../vitest.interactive-tests.mjs";

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const aikenRoot = resolve(packageRoot, "../../onchain/aiken");
const plainBlueprint = resolve(aikenRoot, "plutus.json");
const buildDirectory = resolve(aikenRoot, "build/traced-refusals");
const checkedPinsLog = resolve(buildDirectory, "checked-pins.jsonl");

const fail = (message) => {
  console.error(`[traced-refusals] ${message}`);
  process.exit(1);
};

/**
 * Each test file's declared pins: the `refusedBy` module and the name of the
 * `it` case the pin sits in, which is how the run selects the negatives alone.
 */
const declaredPins = () => {
  const testsRoot = resolve(packageRoot, "tests");
  const pins = new Map();
  for (const entry of readdirSync(testsRoot, { recursive: true })) {
    const file = String(entry);
    if (!file.endsWith(".test.ts")) continue;
    const source = readFileSync(resolve(testsRoot, file), "utf8");
    const declared = [...source.matchAll(/\brefusedBy:\s*"([^"]+)"/gu)].map(
      (match) => {
        const cases = [
          ...source
            .slice(0, match.index)
            .matchAll(/\bit\(\s*"((?:[^"\\]|\\.)*)"/gu),
        ];
        if (cases.length === 0) {
          fail(`tests/${file}: the ${match[1]} pin is not inside an it case`);
        }
        return { module: match[1], name: JSON.parse(`"${cases.at(-1)[1]}"`) };
      },
    );
    if (declared.length > 0) pins.set(`tests/${file}`, declared);
  }
  return pins;
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

/** Build the traced blueprint from the plain one's sources and profile. */
const buildTraced = (profile) => {
  const directory = resolve(buildDirectory, profile);
  const tracedBlueprint = resolve(directory, "plutus.json");
  const tracedRecord = resolve(directory, "traced-build.json");
  const compilerPath = defaultAikenBinary();
  const record = {
    profile,
    compiler: assertPinnedAiken(compilerPath),
    sourceHash: blueprintSourceHash(),
  };
  if (
    existsSync(tracedBlueprint) &&
    existsSync(tracedRecord) &&
    readFileSync(tracedRecord, "utf8") === JSON.stringify(record)
  ) {
    return tracedBlueprint;
  }
  mkdirSync(directory, { recursive: true });
  const result = spawnSync(
    compilerPath,
    [
      "build",
      "--env",
      profile.replaceAll("-", "_"),
      "--trace-level",
      "verbose",
      "--trace-filter",
      "all",
      "--out",
      tracedBlueprint,
    ],
    { cwd: aikenRoot, stdio: "inherit" },
  );
  if (result.error) throw result.error;
  if (result.status !== 0) fail(`traced build exited ${result.status}`);
  writeFileSync(tracedRecord, JSON.stringify(record));
  return tracedBlueprint;
};

/**
 * A project's plain blueprint with every handler of each named module traced,
 * written next to its traced build. Its build record is the plain one's,
 * rebound to the overlay's bytes: both come from the same sources, compiler
 * and profile.
 */
const writeOverlay = (plainBlueprint, modules) => {
  const record = plainRecord(plainBlueprint);
  const tracedBlueprint = buildTraced(record.profile.name);
  const overlayBlueprint = resolve(dirname(tracedBlueprint), "overlay.json");
  const plain = JSON.parse(readFileSync(plainBlueprint, "utf8"));
  const traced = new Map(
    JSON.parse(readFileSync(tracedBlueprint, "utf8")).validators.map(
      (validator) => [validator.title, validator],
    ),
  );
  const moduleOf = (title) => title.slice(0, title.indexOf("."));
  for (const module of modules) {
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

/** A vitest name filter matching exactly the named cases. */
const casePattern = (names) =>
  `(?:${[...new Set(names)]
    .map((name) => name.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&"))
    .join("|")})$`;

const pins = declaredPins();
const requested = process.argv
  .slice(2)
  .map((file) => relative(packageRoot, resolve(file)));
for (const file of requested) {
  if (!pins.has(file)) fail(`${file} declares no refusedBy pin`);
}
const files = requested.length > 0 ? requested : [...pins.keys()].sort();
if (files.length === 0) fail("no test file declares a refusedBy pin");

// Each Vitest project runs its files against its own blueprint, with the
// modules its pins name swapped for their traced builds.
const interactiveFiles = new Set(
  globSync(
    interactiveTests.map((pattern) => pattern.replace(/^\.\//u, "")),
    { cwd: packageRoot },
  ),
);
const projects = [
  {
    name: "testing-profile",
    files: files.filter((file) => !interactiveFiles.has(file)),
    blueprint: () => plainBlueprint,
    overlayVariable: "MIDGARD_REAL_BLUEPRINT_PATH",
  },
  {
    name: "interactive-emulator",
    files: files.filter((file) => interactiveFiles.has(file)),
    blueprint: async () => {
      await prepareInteractiveBlueprint();
      return interactiveEmulatorBlueprint;
    },
    overlayVariable: "MIDGARD_TRACED_INTERACTIVE_BLUEPRINT",
  },
].filter((project) => project.files.length > 0);

mkdirSync(buildDirectory, { recursive: true });
writeFileSync(checkedPinsLog, "");
for (const project of projects) {
  const selected = project.files.flatMap((file) => pins.get(file));
  const overlay = writeOverlay(
    await project.blueprint(),
    new Set(selected.map(({ module }) => module)),
  );
  const run = spawnSync(
    resolve(packageRoot, "node_modules/.bin/vitest"),
    [
      "run",
      "--project",
      project.name,
      "-t",
      casePattern(selected.map(({ name }) => name)),
      ...project.files,
    ],
    {
      cwd: packageRoot,
      stdio: "inherit",
      env: {
        ...process.env,
        [project.overlayVariable]: overlay,
        MIDGARD_EMULATOR_TRACED_REFUSALS: "1",
        MIDGARD_TRACED_REFUSALS_LOG: checkedPinsLog,
      },
    },
  );
  if (run.error) throw run.error;
  if (run.status !== 0) fail(`${project.name}: vitest exited ${run.status}`);
}

// Every declared pin must have been checked against a trace, and every
// checked pin must be one this script read and traced.
const checked = readFileSync(checkedPinsLog, "utf8")
  .split("\n")
  .filter((line) => line.length > 0)
  .map((line) => JSON.parse(line))
  .map(({ file, refusedBy }) => `${file} ${refusedBy}`);
const declared = files.flatMap((file) =>
  pins.get(file).map(({ module }) => `${file} ${module}`),
);
const unchecked = declared.filter((pin) => !checked.includes(pin));
const undeclared = checked.filter((pin) => !declared.includes(pin));
if (unchecked.length > 0) {
  fail(`pins never checked against a trace:\n  ${unchecked.join("\n  ")}`);
}
if (undeclared.length > 0) {
  fail(
    `pins checked but not declared as a refusedBy string literal:\n  ${undeclared.join("\n  ")}`,
  );
}
console.log(
  `[traced-refusals] ${declared.length.toString()} pins in ${files.length.toString()} files checked against traces`,
);
