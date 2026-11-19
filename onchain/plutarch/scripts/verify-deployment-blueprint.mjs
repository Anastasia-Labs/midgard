#!/usr/bin/env node

import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdtemp, readFile, readdir, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";

import {
  buildOffchainBlueprint,
  offchainTitleForGeneratedFile,
} from "./build-offchain-blueprint.mjs";

const [targetPath, generatedDir, outputBlueprintPath, outputManifestPath] =
  process.argv.slice(2);
if (
  !targetPath ||
  !generatedDir ||
  !outputBlueprintPath ||
  !outputManifestPath
) {
  throw new Error(
    "Usage: verify-deployment-blueprint.mjs TARGET_AIKEN_JSON TESTNET_GENERATED_DIR OUTPUT_BLUEPRINT_JSON OUTPUT_MANIFEST_JSON",
  );
}

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
const parameterTitles = (validator) =>
  (validator.parameters ?? []).map(({ title }) => title);

const targetBytes = await readFile(targetPath);
const target = JSON.parse(targetBytes);
assert(Array.isArray(target.validators), "Target blueprint has no validators");
const targetByTitle = new Map();
for (const validator of target.validators) {
  assert(
    !targetByTitle.has(validator.title),
    `Duplicate target validator title: ${validator.title}`,
  );
  targetByTitle.set(validator.title, validator);
}

const temporaryDir = await mkdtemp(
  path.join(os.tmpdir(), "midgard-plutarch-deployment-blueprint-"),
);
const temporaryBlueprintPath = path.join(temporaryDir, "plutus.json");
try {
  await buildOffchainBlueprint({
    generatedDir,
    outputPath: temporaryBlueprintPath,
  });
  const blueprintBytes = await readFile(temporaryBlueprintPath);
  const blueprint = JSON.parse(blueprintBytes);
  const portedByTitle = new Map(
    blueprint.validators.map((validator) => [validator.title, validator]),
  );
  assert.equal(
    portedByTitle.size,
    blueprint.validators.length,
    "Duplicate Plutarch validator title",
  );

  const files = (await readdir(generatedDir))
    .filter((fileName) => fileName.endsWith(".plutus.json"))
    .sort();
  assert.equal(files.length, blueprint.validators.length);

  const validators = [];
  for (const file of files) {
    const title = offchainTitleForGeneratedFile(file);
    const ported = portedByTitle.get(title);
    const original = targetByTitle.get(title);
    assert(ported, `Missing Plutarch blueprint validator: ${title}`);
    assert(
      original,
      `Plutarch export is absent from target blueprint: ${title}`,
    );
    assert.deepEqual(
      parameterTitles(ported),
      parameterTitles(original),
      `Parameter-list mismatch: ${title}`,
    );
    const artifactPath = path.join(generatedDir, file);
    const artifactBytes = await readFile(artifactPath);
    validators.push({
      file,
      title,
      parameters: parameterTitles(ported),
      compiledCodeBytes: ported.compiledCode.length / 2,
      compiledCodeSha256: sha256(Buffer.from(ported.compiledCode, "hex")),
      artifactSha256: sha256(artifactBytes),
      targetPlutusV3ScriptHash: original.hash,
    });
  }

  await writeFile(outputBlueprintPath, blueprintBytes);
  const manifest = {
    target: path.resolve(targetPath),
    targetSha256: sha256(targetBytes),
    targetValidatorCount: target.validators.length,
    generatedDir: path.resolve(generatedDir),
    blueprint: path.resolve(outputBlueprintPath),
    blueprintSha256: sha256(blueprintBytes),
    validatorCount: validators.length,
    exactTitleMatches: validators.length,
    exactParameterLists: validators.length,
    validators,
  };
  await writeFile(outputManifestPath, `${JSON.stringify(manifest, null, 2)}\n`);
  process.stdout.write(
    `Verified ${validators.length.toString()} Plutarch deployment exports: ${validators.length.toString()} exact target titles and parameter lists. Blueprint SHA-256 ${manifest.blueprintSha256}.\n`,
  );
} finally {
  await rm(temporaryDir, { recursive: true, force: true });
}
