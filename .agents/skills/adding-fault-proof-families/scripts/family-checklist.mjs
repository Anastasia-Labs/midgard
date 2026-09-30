#!/usr/bin/env node
// Reports, for one fault-proof catalogue category, which of the artifacts a
// complete family needs are present in the tree.
//
// It reads source text, not built output, so it runs without an install or a
// build. It parses the registries by locating each exported table and reading
// its keys and string values; it does not evaluate TypeScript.
//
// Exit codes:
//   0  every gated artifact is present
//   1  looked, and at least one gated artifact is missing (a GAP line says which)
//   2  could not look: a source file is missing or a table no longer has the
//      shape this script parses. Nothing is known about the family; fix the
//      script or the path before trusting any result.
//   64 usage error
//
// Gated checks (a missing one is a GAP): catalogue order and a unique 8-hex ID;
// the core deployment-manifest mirror; a reference-script role and token name
// for every contract named after the family's first-step contract; the SDK
// build<Name>Chain with a validator file per blueprint title; a family
// application record; the classification and adapter-registration rows at
// the catalogue position; a catalogue-status.md row; a devnet journey owner;
// and at least one fault-proofs test file named after the family.
// Informational: family definition kind, typed reasons routed here, and which
// matched tests are lifecycles, assert validator refusal, or record coverage.
//
// What it cannot see: whether the validator is correct, whether the emulator
// tests exercise both polarities at the exact check, or whether the docs text
// is true. Test evidence is matched by file name and is labelled heuristic.
//
// Usage: node family-checklist.mjs <category> [--root <repository-root>]

import "node:fs";
import "node:path";
import "node:url";
import "./family-checklist.top-level-elements.mjs";
import "./family-checklist.check-family.mjs";
import "./family-checklist.exit-code-for.mjs";

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { checkFamily } from "./family-checklist.check-family.mjs";
import { exitCodeFor, USAGE } from "./family-checklist.exit-code-for.mjs";
import {
  CannotLook,
  EXIT_CANNOT_READ,
  EXIT_USAGE,
} from "./family-checklist.top-level-elements.mjs";

export const main = (argv, log = console.log, error = console.error) => {
  const args = [...argv];
  let root = resolve(
    join(dirname(fileURLToPath(import.meta.url)), "../../../.."),
  );
  const rootFlag = args.indexOf("--root");
  if (rootFlag >= 0) {
    if (args[rootFlag + 1] === undefined) {
      error(USAGE);
      return EXIT_USAGE;
    }
    root = resolve(args[rootFlag + 1]);
    args.splice(rootFlag, 2);
  }
  if (args.length !== 1 || !/^[a-z][A-Za-z0-9]*$/u.test(args[0])) {
    error(USAGE);
    error("  <category> is the catalogue key, e.g. mintItemNonCanonical");
    return EXIT_USAGE;
  }
  const category = args[0];
  let results;
  try {
    results = checkFamily(root, category);
  } catch (caught) {
    // Any failure to read or parse is "could not look", never a gap: exit 1
    // must only ever mean the script looked and found something missing.
    const reason =
      caught instanceof CannotLook
        ? caught.message
        : `unexpected error: ${caught?.stack ?? caught}`;
    error(`family-checklist: could not look: ${reason}`);
    error("  no result for this family; fix the path or this script's parser");
    return EXIT_CANNOT_READ;
  }
  for (const { id, status, detail } of results) {
    log(`${status.toUpperCase().padEnd(4)}  ${id.padEnd(24)}  ${detail}`);
  }
  const gaps = results.filter((result) => result.status === "gap").length;
  log(
    gaps === 0
      ? `${category}: every gated artifact present (tests are matched by name; review still owns correctness)`
      : `${category}: ${gaps} gap(s)`,
  );
  return exitCodeFor(results);
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  process.exitCode = main(process.argv.slice(2));
}
export { checkFamily } from "./family-checklist.check-family.mjs";
export { exitCodeFor } from "./family-checklist.exit-code-for.mjs";
export {
  arrayStrings,
  CannotLook,
  EXIT_CANNOT_READ,
  EXIT_COMPLETE,
  EXIT_GAPS,
  EXIT_USAGE,
  extractLiteral,
  kebab,
  objectKeys,
  objectStringPairs,
  SOURCES,
  topLevelElements,
  validatorFileCandidates,
} from "./family-checklist.top-level-elements.mjs";
