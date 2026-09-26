#!/usr/bin/env node
// Compares the validators of two Aiken blueprints (plutus.json) built from the
// tree before and after a module split, and fails on any difference.
//
// For every validator it compares the title, the script hash and a SHA-256 of
// the compiled code. A pure move of Aiken code leaves all three unchanged; a
// changed title means a validator file moved or was renamed, which changes
// what the off-chain packages look up.
//
// The blueprint's `definitions` keys follow the module a type is defined in,
// so moving a type legitimately renames them. They are reported, not failed
// on; anything that pins a definitions key needs updating in the same change.
//
// It does not check how the blueprints were built. Build both sides with the
// same compiler, the same `--env`, and the default (silent) trace level.
//
// Usage: node compare-blueprint-hashes.mjs <before.json> <after.json>
//
// Exit codes: 0 identical; 1 validators differ; 2 usage error; 3 could not
// compare (unreadable or malformed blueprint, duplicate titles, or the two
// blueprints name different compilers or Plutus versions).

import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const EXIT_IDENTICAL = 0;
const EXIT_DIFFERENT = 1;
const EXIT_USAGE = 2;
const EXIT_COULD_NOT_COMPARE = 3;

class CouldNotCompare extends Error {}

const USAGE = "Usage: compare-blueprint-hashes.mjs <before.json> <after.json>";

export const loadBlueprint = (path) => {
  let blueprint;
  try {
    blueprint = JSON.parse(readFileSync(path, "utf8"));
  } catch (error) {
    throw new CouldNotCompare(`could not read ${path}: ${error.message}`);
  }
  if (
    blueprint === null ||
    typeof blueprint !== "object" ||
    !Array.isArray(blueprint.validators)
  ) {
    throw new CouldNotCompare(`${path} has no validators array`);
  }
  const validators = new Map();
  for (const validator of blueprint.validators) {
    const { title, hash, compiledCode } = validator ?? {};
    if (
      typeof title !== "string" ||
      typeof hash !== "string" ||
      typeof compiledCode !== "string"
    ) {
      throw new CouldNotCompare(
        `${path} has a validator without a string title, hash and compiledCode`,
      );
    }
    if (validators.has(title)) {
      throw new CouldNotCompare(`${path} names validator ${title} twice`);
    }
    validators.set(title, {
      hash,
      code: createHash("sha256").update(compiledCode).digest("hex"),
    });
  }
  return {
    path,
    compiler: blueprint.preamble?.compiler?.version,
    plutusVersion: blueprint.preamble?.plutusVersion,
    validators,
    definitions: new Set(Object.keys(blueprint.definitions ?? {})),
  };
};

export const compareBlueprints = (before, after) => {
  if (before.compiler !== after.compiler) {
    throw new CouldNotCompare(
      `the blueprints name different compilers (${before.compiler} vs ${after.compiler}); rebuild both with the pinned fork`,
    );
  }
  if (before.plutusVersion !== after.plutusVersion) {
    throw new CouldNotCompare(
      `the blueprints name different Plutus versions (${before.plutusVersion} vs ${after.plutusVersion})`,
    );
  }
  const findings = [];
  for (const [title, was] of before.validators) {
    const now = after.validators.get(title);
    if (now === undefined) findings.push(`missing     ${title}`);
    else if (now.hash !== was.hash || now.code !== was.code) {
      findings.push(`changed     ${title}: hash ${was.hash} -> ${now.hash}`);
    }
  }
  for (const title of after.validators.keys()) {
    if (!before.validators.has(title)) findings.push(`unexpected  ${title}`);
  }
  const definitionsOnlyBefore = [...before.definitions].filter(
    (key) => !after.definitions.has(key),
  );
  const definitionsOnlyAfter = [...after.definitions].filter(
    (key) => !before.definitions.has(key),
  );
  return { findings, definitionsOnlyBefore, definitionsOnlyAfter };
};

export const main = (
  argv,
  { stdout = process.stdout, stderr = process.stderr } = {},
) => {
  if (argv.length !== 2 || argv.some((arg) => arg.startsWith("-"))) {
    stderr.write(`${USAGE}\n`);
    return EXIT_USAGE;
  }
  try {
    const before = loadBlueprint(argv[0]);
    const after = loadBlueprint(argv[1]);
    const { findings, definitionsOnlyBefore, definitionsOnlyAfter } =
      compareBlueprints(before, after);
    const scope = `${before.validators.size} validators before, ${after.validators.size} after, compiler ${before.compiler}`;
    if (findings.length === 0) {
      stdout.write(`Blueprint validators identical: ${scope}\n`);
    } else {
      stdout.write(
        `Blueprint validators DIFFER: ${findings.length} finding(s) (${scope})\n`,
      );
      for (const finding of findings) stdout.write(`  ${finding}\n`);
    }
    if (definitionsOnlyBefore.length + definitionsOnlyAfter.length > 0) {
      stdout.write(
        `Note: definitions keys moved (${definitionsOnlyBefore.length} only before, ${definitionsOnlyAfter.length} only after); update anything that pins them.\n`,
      );
      for (const key of definitionsOnlyBefore)
        stdout.write(`  only before  ${key}\n`);
      for (const key of definitionsOnlyAfter)
        stdout.write(`  only after   ${key}\n`);
    }
    return findings.length === 0 ? EXIT_IDENTICAL : EXIT_DIFFERENT;
  } catch (error) {
    if (!(error instanceof CouldNotCompare)) throw error;
    stderr.write(
      `compare-blueprint-hashes: could not compare: ${error.message}\n`,
    );
    return EXIT_COULD_NOT_COMPARE;
  }
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  process.exitCode = main(process.argv.slice(2));
}
