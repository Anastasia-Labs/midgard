#!/usr/bin/env node
/**
 * The verbose-traced blueprint of one deployment profile, which
 * run-traced-refusals.mjs swaps single modules out of. It is cached under
 * onchain/aiken/build/traced-refusals/<profile> with a record of the profile,
 * the compiler, the source hash and the blueprint's own hash, and is reused
 * only while all four still match.
 *
 * With MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS=1 (CI, where the `test-blueprints`
 * job built it) a missing or unmatched cache fails instead of being rebuilt.
 *
 *   node scripts/traced-blueprint.mjs <profile>
 *
 * builds it, or leaves a matching cache in place.
 */
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  assertPinnedAiken,
  defaultAikenBinary,
} from "../../../onchain/aiken/scripts/pinned-compiler.mjs";
import {
  blueprintHash,
  blueprintSourceHash,
} from "../../scripts/lib/blueprint-stamp.mjs";

const aikenRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../../onchain/aiken",
);
export const tracedBuildDirectory = resolve(aikenRoot, "build/traced-refusals");

const paths = (profile) => {
  const directory = resolve(tracedBuildDirectory, profile);
  return {
    directory,
    blueprint: resolve(directory, "plutus.json"),
    record: resolve(directory, "traced-build.json"),
  };
};

/** Why the cached traced blueprint cannot be used, or undefined when it can. */
const staleness = (profile, expected) => {
  const { blueprint, record } = paths(profile);
  if (!existsSync(blueprint)) return `no traced blueprint at ${blueprint}`;
  if (!existsSync(record)) return `${blueprint} has no build record ${record}`;
  let recorded;
  try {
    recorded = JSON.parse(readFileSync(record, "utf8"));
  } catch (error) {
    return `${record} is unreadable: ${error instanceof Error ? error.message : String(error)}`;
  }
  const mismatched = Object.entries({
    ...expected,
    blueprintHash: blueprintHash(blueprint),
  })
    .filter(([key, value]) => recorded[key] !== value)
    .map(([key]) => key);
  return mismatched.length === 0
    ? undefined
    : `${blueprint} does not match its ${mismatched.join(", ")}`;
};

/** The traced blueprint of `profile`, built unless a matching one is cached. */
export const tracedBlueprint = (profile) => {
  const { directory, blueprint, record } = paths(profile);
  const compilerPath = defaultAikenBinary();
  const expected = {
    profile,
    compiler: assertPinnedAiken(compilerPath),
    sourceHash: blueprintSourceHash(),
  };
  const reason = staleness(profile, expected);
  if (reason === undefined) return blueprint;
  if (process.env.MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS === "1")
    throw new Error(
      `MIDGARD_REQUIRE_PREBUILT_BLUEPRINTS=1 forbids rebuilding the traced ${profile} blueprint, and the prebuilt one is unusable: ${reason}`,
    );
  mkdirSync(directory, { recursive: true });
  if (existsSync(record)) rmSync(record);
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
      blueprint,
    ],
    { cwd: aikenRoot, stdio: "inherit" },
  );
  if (result.error) throw result.error;
  if (result.status !== 0)
    throw new Error(`traced ${profile} build exited ${result.status}`);
  writeFileSync(
    record,
    JSON.stringify({ ...expected, blueprintHash: blueprintHash(blueprint) }),
  );
  return blueprint;
};

if (
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const [profile, ...rest] = process.argv.slice(2);
  if (profile === undefined || rest.length > 0) {
    console.error("usage: node scripts/traced-blueprint.mjs <profile>");
    process.exit(2);
  }
  console.log(tracedBlueprint(profile));
}
