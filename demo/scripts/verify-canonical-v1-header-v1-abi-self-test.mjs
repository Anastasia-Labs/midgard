#!/usr/bin/env node

// Behavioral self-test for the Header ABI gate (V-8, issue #531). The gate is
// the only path that compares the registry-owned contract against BOTH real
// projections — the blueprint the Aiken compiler generated and the Data schema
// the built SDK exports — and until now nothing invoked it, so nothing proved
// it could fail. The gate is tested by behavior, not by reading its source:
// each hostile mutation is written to a throwaway copy of ONE input, the real
// gate is invoked against that copy through the matching `--*-under-test=`
// hook, and the run must exit non-zero carrying the expected diagnostic. The
// other two inputs stay the real repository artifacts on every run, so a
// seeded mismatch is judged against reality rather than against a second copy
// of itself. Clean control runs bracket the suite, so a gate that rejected
// everything (or nothing) could not pass it.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(scriptDir, "../..");
const gatePath = resolve(scriptDir, "verify-canonical-v1-header-v1-abi.mjs");
const registryPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-format-registry-v1.json",
);
const blueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");
const sdkModulePath = resolve(repoRoot, "demo/midgard-sdk/dist/index.js");

for (const [label, path, remedy] of [
  [
    "generated Aiken blueprint",
    blueprintPath,
    "aiken build (in onchain/aiken)",
  ],
  [
    "built SDK runtime module",
    sdkModulePath,
    "pnpm --dir demo/midgard-sdk run build",
  ],
]) {
  assert.ok(
    existsSync(path),
    `${label} is required and absent: ${path}; run \`${remedy}\` first`,
  );
}

const registry = JSON.parse(readFileSync(registryPath, "utf8"));
const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8"));
const HEADER_DEFINITION = "midgard/ledger_state/Header";

const workspace = mkdtempSync(resolve(tmpdir(), "header-v1-abi-self-test-"));
const candidateRegistryPath = resolve(workspace, "registry.json");
const candidateBlueprintPath = resolve(workspace, "plutus.json");

const runGate = (extraArguments) => {
  const result = spawnSync(process.execPath, [gatePath, ...extraArguments], {
    cwd: repoRoot,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  assert.ok(
    result.status !== null,
    `gate did not run: ${result.error?.message ?? "unknown failure"}`,
  );
  return result;
};

// Positive control: the published registry, the generated blueprint and the
// built SDK schema must agree with no redirection at all.
const control = runGate([]);
assert.equal(
  control.status,
  0,
  `real registry/blueprint/SDK must agree: ${control.stderr}${control.stdout}`,
);
assert.match(
  control.stdout,
  /^header-v1-abi: PASS constructor 0 arity 25$/mu,
  `control run did not report the compared shape:\n${control.stdout}`,
);

let rejectedMutations = 0;
const mustReject = (label, expected, seed) => {
  const extraArguments = seed();
  const { status, stderr } = runGate(extraArguments);
  assert.notEqual(status, 0, `${label}: gate exited 0 on a seeded mismatch`);
  assert.match(stderr, expected, `${label}: missing diagnostic in:\n${stderr}`);
  process.stdout.write(
    `rejected: ${label} -> exit ${status}, ${stderr.match(expected)[0]}\n`,
  );
  rejectedMutations += 1;
};

// --- Seeded blueprint mismatches (real registry, real SDK schema) -----------

// Only the Header definition is cloned; every other definition in the 11 MB
// generated blueprint is carried over by reference, so each candidate is the
// real compiler output with exactly one seeded edit.
const seedBlueprint = (mutate) => {
  const definitions = { ...blueprint.definitions };
  definitions[HEADER_DEFINITION] = structuredClone(
    blueprint.definitions[HEADER_DEFINITION],
  );
  const candidate = { ...blueprint, definitions };
  mutate(definitions[HEADER_DEFINITION], definitions);
  writeFileSync(candidateBlueprintPath, JSON.stringify(candidate));
  return [`--blueprint-under-test=${candidateBlueprintPath}`];
};
const headerFields = (definition) => definition.anyOf[0].fields;

mustReject(
  "the blueprint renames Header field 0",
  /Aiken field 0 name mismatch/u,
  () =>
    seedBlueprint((definition) => {
      headerFields(definition)[0].title = "hostile_renamed_field";
    }),
);

mustReject(
  "the blueprint transposes Header fields 0 and 1",
  /Aiken field 0 name mismatch/u,
  () =>
    seedBlueprint((definition) => {
      const fields = headerFields(definition);
      [fields[0], fields[1]] = [fields[1], fields[0]];
    }),
);

mustReject(
  "the blueprint drops a Header field",
  /Aiken constructor arity mismatch: expected 25, got 24/u,
  () =>
    seedBlueprint((definition) => {
      headerFields(definition).pop();
    }),
);

mustReject(
  "the blueprint appends a Header field",
  /Aiken constructor arity mismatch: expected 25, got 26/u,
  () =>
    seedBlueprint((definition) => {
      const fields = headerFields(definition);
      fields.push(structuredClone(fields[fields.length - 1]));
    }),
);

mustReject(
  "the blueprint moves Header off constructor tag 0",
  /blueprint Header constructor 0 is missing/u,
  () =>
    seedBlueprint((definition) => {
      definition.anyOf[0].index = 1;
    }),
);

mustReject(
  "the blueprint retypes a Header field's referenced definition",
  /Aiken field \d+ definition mismatch/u,
  () =>
    seedBlueprint((definition) => {
      const fields = headerFields(definition);
      const replacement = fields.find((field) => field.$ref !== fields[0].$ref);
      assert.ok(
        replacement !== undefined,
        "Header must reference more than one definition",
      );
      fields[0].$ref = replacement.$ref;
    }),
);

mustReject(
  "the blueprint no longer defines Header at all",
  /blueprint Header definition must be an object/u,
  () =>
    seedBlueprint((_definition, definitions) => {
      delete definitions[HEADER_DEFINITION];
    }),
);

// --- Seeded registry mismatches (real blueprint, real SDK schema) -----------

const seedRegistry = (mutate) => {
  const candidate = structuredClone(registry);
  const row = candidate.formats.find((format) => format.id === "L01");
  assert.ok(row !== undefined, "L01 must exist in the registry");
  mutate(row.canonicalForms[0], row);
  writeFileSync(candidateRegistryPath, JSON.stringify(candidate));
  return [`--registry-under-test=${candidateRegistryPath}`];
};

mustReject(
  "the registry renames an Aiken-facing Header field",
  /Aiken field 7 name mismatch/u,
  () =>
    seedRegistry((contract) => {
      contract.fields[7].name = "hostile_registry_field";
      contract.exactFields[7] = "hostile_registry_field";
    }),
);

mustReject(
  "the registry renames an SDK-facing Header field",
  /SDK field 2 name mismatch/u,
  () =>
    seedRegistry((contract) => {
      contract.fields[2].sdkName = "hostileRegistryField";
    }),
);

mustReject(
  "the registry retypes an SDK-facing Header field",
  /SDK field \d+ type mismatch/u,
  () =>
    seedRegistry((contract) => {
      const index = contract.fields.findIndex((field) =>
        field.type.startsWith("bytes"),
      );
      assert.ok(index >= 0, "Header must declare a bytes field");
      // The Aiken comparison erases the width suffix, so only the SDK's
      // runtime minLength/maxLength can catch this: the gate must not be
      // satisfied by the blueprint half agreeing.
      contract.fields[index].type =
        contract.fields[index].type === "bytes28" ? "bytes32" : "bytes28";
    }),
);

mustReject(
  "the registry understates the Header constructor arity",
  /contract\.constructor\.arity must be 25/u,
  () =>
    seedRegistry((contract) => {
      contract.constructor.arity = 24;
    }),
);

mustReject(
  "the registry deletes the L01 contract the gate compares",
  /format registry L01 must provide canonicalForms\[0\]/u,
  () =>
    seedRegistry((_contract, row) => {
      row.canonicalForms = [];
    }),
);

// --- A missing build may not be mistaken for agreement ---------------------

mustReject(
  "the SDK runtime module is absent",
  /SDK runtime module does not exist/u,
  () => [`--sdk-module-under-test=${resolve(workspace, "no-such-index.js")}`],
);

mustReject(
  "the generated blueprint is absent",
  /Aiken blueprint does not exist/u,
  () => [`--blueprint-under-test=${resolve(workspace, "no-such-plutus.json")}`],
);

// Closing control: the same harness still accepts the real three-way
// comparison, so the rejections above cannot be a gate that rejects
// everything.
const closingControl = runGate([]);
assert.equal(
  closingControl.status,
  0,
  `real registry/blueprint/SDK must still agree: ${closingControl.stderr}`,
);

rmSync(workspace, { recursive: true, force: true });
process.stdout.write(
  `header-v1-abi:self-test: PASS\ncontrol runs accepted: 2; hostile mutations rejected: ${rejectedMutations}\n`,
);
