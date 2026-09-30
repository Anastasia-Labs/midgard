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
 * With no arguments it runs every file that declares a pin. It needs the plain
 * blueprint fresh (`pnpm --dir demo deployment:build <profile>`); the traced
 * build uses the same profile and is cached under
 * onchain/aiken/build/traced-refusals until the sources or compiler change.
 */
import { spawnSync } from "node:child_process";
import {
  existsSync,
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

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const aikenRoot = resolve(packageRoot, "../../onchain/aiken");
const plainBlueprint = resolve(aikenRoot, "plutus.json");
const buildDirectory = resolve(aikenRoot, "build/traced-refusals");
const tracedBlueprint = resolve(buildDirectory, "plutus.json");
const tracedRecord = resolve(buildDirectory, "traced-build.json");
const overlayBlueprint = resolve(buildDirectory, "overlay.json");
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

const plainRecord = () => {
  const verdict = checkBlueprintStamp({ blueprintPath: plainBlueprint });
  if (verdict.status !== "fresh") {
    fail(
      `${verdict.detail}${verdict.fix === null ? "" : `\nRebuild it: ${verdict.fix}`}`,
    );
  }
  return JSON.parse(readFileSync(buildRecordPath(plainBlueprint), "utf8"));
};

/** Build the traced blueprint from the plain one's sources and profile. */
const buildTraced = (profile) => {
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
    return;
  }
  mkdirSync(buildDirectory, { recursive: true });
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
};

/**
 * The plain blueprint with every handler of each named module traced. Its
 * build record is the plain one's, rebound to the overlay's bytes: both come
 * from the same sources, compiler and profile.
 */
const writeOverlay = (record, modules) => {
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

const record = plainRecord();
buildTraced(record.profile.name);
const selected = files.flatMap((file) => pins.get(file));
writeOverlay(record, new Set(selected.map(({ module }) => module)));
writeFileSync(checkedPinsLog, "");

const run = spawnSync(
  resolve(packageRoot, "node_modules/.bin/vitest"),
  [
    "run",
    "--project",
    "testing-profile",
    "-t",
    casePattern(selected.map(({ name }) => name)),
    ...files,
  ],
  {
    cwd: packageRoot,
    stdio: "inherit",
    env: {
      ...process.env,
      MIDGARD_REAL_BLUEPRINT_PATH: overlayBlueprint,
      MIDGARD_EMULATOR_TRACED_REFUSALS: "1",
      MIDGARD_TRACED_REFUSALS_LOG: checkedPinsLog,
    },
  },
);
if (run.error) throw run.error;
if (run.status !== 0) fail(`vitest exited ${run.status}`);

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
