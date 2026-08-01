#!/usr/bin/env node

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  allBound,
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
const boundEvidence = {
  path: "docs/exec-plans/evidence/canonical-v1-goal-closure-v1.json",
  status: "BOUND",
};
const openEvidence = {
  path: "docs/exec-plans/evidence/canonical-v1-goal-closure-v1.json",
  status: "OPEN",
};
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
mustReject((candidate) => {
  candidate.validatorSet.status = "BOUND";
  candidate.validatorSet.identity = "validator-set";
  candidate.validatorSet.evidence = [openEvidence];
}, /BOUND requires all evidence files to be bound/u);
mustReject((candidate) => {
  candidate.deployment.status = "BOUND";
  candidate.deployment.identity = "deployment";
  candidate.deployment.evidence = [openEvidence];
}, /BOUND requires all evidence files to be bound/u);
mustReject((candidate) => {
  candidate.acceptanceCriteria[0].status = "PASS";
  candidate.acceptanceCriteria[0].evidence = [openEvidence];
}, /PASS requires all evidence files to be bound/u);
mustReject((candidate) => {
  candidate.secrets.scanStatus = "PASS";
  candidate.secrets.evidence = [openEvidence];
}, /PASS requires all evidence files to be bound/u);

const releaseReadyCandidate = clone(manifest);
releaseReadyCandidate.revision.worktree = "BASELINE_RELATIVE_CLEAN";
releaseReadyCandidate.revision.releaseCommit =
  releaseReadyCandidate.revision.headCommit;
releaseReadyCandidate.parameterSnapshot.status = "BOUND";
releaseReadyCandidate.blueprint.status = "BOUND";
releaseReadyCandidate.validatorSet = {
  identity: "validator-set",
  status: "BOUND",
  evidence: [boundEvidence],
};
releaseReadyCandidate.deployment = {
  network: "preprod",
  identity: "deployment",
  status: "BOUND",
  evidence: [boundEvidence],
};
releaseReadyCandidate.commandResults = [
  {
    artifactIdentity: null,
    command: ["node", "self-test"],
    durationMs: 1,
    exitCode: 0,
    finishedAt: releaseReadyCandidate.generatedAt,
    id: "self-test",
    revision: releaseReadyCandidate.revision.headCommit,
    testCount: 1,
  },
];
releaseReadyCandidate.acceptanceCriteria.forEach((criterion) => {
  criterion.status = "PASS";
  criterion.evidence = [boundEvidence];
});
releaseReadyCandidate.secrets = {
  scanStatus: "PASS",
  evidence: [boundEvidence],
};
releaseReadyCandidate.release.status = "BOUND";
releaseReadyCandidate.release.digest = "0".repeat(64);

assert.equal(allBound([boundEvidence]), true);
decodeCanonicalV1GoalClosure(releaseReadyCandidate);
assert.equal(isReleaseReady(releaseReadyCandidate), true);
releaseReadyCandidate.acceptanceCriteria[0].evidence = [openEvidence];
assert.equal(isReleaseReady(releaseReadyCandidate), false);

process.stdout.write(
  `${JSON.stringify({
    status: "PASS",
    hostileMutations: 11,
    criteria: manifest.acceptanceCriteria.length,
  })}\n`,
);
