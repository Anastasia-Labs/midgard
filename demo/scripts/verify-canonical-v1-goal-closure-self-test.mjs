#!/usr/bin/env node

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  canonicalClosureDigest,
  decodeCanonicalV1GoalClosure,
  isReleaseReady,
} from "./canonical-v1-goal-closure-v1.mjs";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const manifest = JSON.parse(
  readFileSync(
    resolve(
      repoRoot,
      "docs/exec-plans/evidence/canonical-v1-goal-closure-v1.json",
    ),
    "utf8",
  ),
);
const clone = (value) => structuredClone(value);
const mustReject = (mutate, pattern) => {
  const candidate = clone(manifest);
  mutate(candidate);
  assert.throws(() => decodeCanonicalV1GoalClosure(candidate), pattern);
};

decodeCanonicalV1GoalClosure(manifest);
assert.equal(isReleaseReady(manifest), false);
assert.match(canonicalClosureDigest(manifest), /^[0-9a-f]{64}$/);
assert.equal(
  canonicalClosureDigest(manifest),
  canonicalClosureDigest(manifest),
);

mustReject((candidate) => {
  candidate.unknown = true;
}, /unexpected keys|expected keys/u);
mustReject((candidate) => {
  candidate.acceptanceCriteria.pop();
}, /exact ordered criteria/u);
mustReject((candidate) => {
  candidate.acceptanceCriteria[1].id = candidate.acceptanceCriteria[0].id;
}, /exact ordered criteria/u);
mustReject((candidate) => {
  candidate.acceptanceCriteria[0].status = "PASS";
}, /PASS requires at least one bound evidence file/u);
mustReject((candidate) => {
  candidate.blueprint.sha256 = "0".repeat(64);
}, /expected keys \[path, status\]/u);
mustReject((candidate) => {
  candidate.deployment.network = "mainnet";
}, /must be preprod/u);
mustReject((candidate) => {
  candidate.release.status = "BOUND";
}, /BOUND requires digest/u);

process.stdout.write(
  `${JSON.stringify({
    status: "PASS",
    hostileMutations: 7,
    criteria: manifest.acceptanceCriteria.length,
  })}\n`,
);
