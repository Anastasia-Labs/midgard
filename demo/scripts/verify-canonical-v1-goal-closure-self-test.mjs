#!/usr/bin/env node

import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import {
  copyFileSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { readFileSync } from "node:fs";
import { tmpdir } from "node:os";
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
// Counted rather than hard-coded: the printed total is the number of hostile
// mutations this run actually rejected, so it can never drift from the suite.
let hostileMutations = 0;
const mustReject = (mutate, pattern) => {
  const candidate = clone(manifest);
  mutate(candidate);
  assert.throws(() => decodeCanonicalV1GoalClosure(candidate), pattern);
  hostileMutations += 1;
};
let releaseGateRejections = 0;
const mustNotBeReleaseReady = (candidate) => {
  assert.equal(isReleaseReady(candidate), false);
  releaseGateRejections += 1;
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

// GOAL_SPEC §13.3/C70 dual parameter snapshots: neither may be dropped, and
// one file cannot stand in for both the mainnet capability floor and the
// target-testnet deployment parameters.
mustReject((candidate) => {
  delete candidate.targetTestnetParameterSnapshot;
}, /expected keys/u);
mustReject((candidate) => {
  candidate.targetTestnetParameterSnapshot.path =
    candidate.parameterSnapshot.path;
}, /must be distinct files/u);
mustReject((candidate) => {
  candidate.targetTestnetParameterSnapshot.status = "PASS";
}, /expected one of OPEN, BOUND/u);

// GOAL_SPEC §9.5 residual launch blockers: never silence, never unevidenced
// owner acceptance, never a duplicate id.
mustReject((candidate) => {
  delete candidate.residualBlockers;
}, /expected keys/u);
mustReject((candidate) => {
  candidate.residualBlockers = [
    {
      id: "RB-1",
      description: "unaccepted",
      ownerAccepted: "yes",
      evidence: [],
    },
  ];
}, /expected a boolean/u);
mustReject((candidate) => {
  candidate.residualBlockers = [
    {
      id: "RB-1",
      description: "accepted without evidence",
      ownerAccepted: true,
      evidence: [],
    },
  ];
}, /ownerAccepted requires at least one bound evidence file/u);
mustReject((candidate) => {
  candidate.residualBlockers = [
    {
      id: "RB-1",
      description: "accepted with unbound evidence",
      ownerAccepted: true,
      evidence: [openEvidence],
    },
  ];
}, /ownerAccepted requires all evidence files to be bound/u);
mustReject((candidate) => {
  candidate.residualBlockers = [
    { id: "RB-1", description: "first", ownerAccepted: false, evidence: [] },
    { id: "RB-1", description: "second", ownerAccepted: false, evidence: [] },
  ];
}, /duplicate id/u);
mustReject((candidate) => {
  candidate.residualBlockers = [
    {
      id: "RB-1",
      description: "extra key",
      ownerAccepted: false,
      evidence: [],
      owner: "someone",
    },
  ];
}, /expected keys \[description, evidence, id, ownerAccepted\]/u);

// F41 live-acceptance transaction identities: exact 32-byte hashes only, no
// duplicates, and no empty list masquerading as recorded evidence.
const syntheticCommandResult = {
  artifactIdentity: null,
  command: ["node", "self-test"],
  durationMs: 1,
  exitCode: 0,
  finishedAt: manifest.generatedAt,
  id: "self-test",
  revision: manifest.revision.headCommit,
  testCount: 1,
};
const commandResultWith = (candidate, patch) => {
  candidate.commandResults = [
    { ...(candidate.commandResults[0] ?? syntheticCommandResult), ...patch },
  ];
};
mustReject((candidate) => {
  commandResultWith(candidate, { transactionHashes: [] });
}, /omit the key instead of recording an empty transaction list/u);
mustReject((candidate) => {
  commandResultWith(candidate, { transactionHashes: ["deadbeef"] });
}, /expected a lowercase 32-byte transaction hash/u);
mustReject((candidate) => {
  commandResultWith(candidate, {
    transactionHashes: ["a".repeat(64), "a".repeat(64)],
  });
}, /duplicate transaction hash/u);
mustReject((candidate) => {
  commandResultWith(candidate, { txHashes: ["a".repeat(64)] });
}, /optional \[transactionHashes\]/u);

// A recorded live-acceptance transaction list is accepted when it is exact.
const transactionHashCandidate = clone(manifest);
commandResultWith(transactionHashCandidate, {
  transactionHashes: ["a".repeat(64), "b".repeat(64)],
});
decodeCanonicalV1GoalClosure(transactionHashCandidate);

const releaseReadyCandidate = clone(manifest);
releaseReadyCandidate.revision.worktree = "BASELINE_RELATIVE_CLEAN";
releaseReadyCandidate.revision.releaseCommit =
  releaseReadyCandidate.revision.headCommit;
releaseReadyCandidate.parameterSnapshot.status = "BOUND";
releaseReadyCandidate.targetTestnetParameterSnapshot.status = "BOUND";
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

// An unbound second C70 snapshot keeps release closed: §13.3 requires both.
const singleSnapshotCandidate = clone(releaseReadyCandidate);
singleSnapshotCandidate.targetTestnetParameterSnapshot.status = "OPEN";
mustNotBeReleaseReady(singleSnapshotCandidate);

// A named §9.5 residual launch blocker without recorded owner acceptance keeps
// release closed; the same blocker with bound acceptance evidence does not.
const residualBlockerCandidate = clone(releaseReadyCandidate);
residualBlockerCandidate.residualBlockers = [
  {
    id: "RB-1",
    description: "named residual launch blocker",
    ownerAccepted: false,
    evidence: [],
  },
];
decodeCanonicalV1GoalClosure(residualBlockerCandidate);
mustNotBeReleaseReady(residualBlockerCandidate);
residualBlockerCandidate.residualBlockers[0].ownerAccepted = true;
residualBlockerCandidate.residualBlockers[0].evidence = [boundEvidence];
decodeCanonicalV1GoalClosure(residualBlockerCandidate);
assert.equal(isReleaseReady(residualBlockerCandidate), true);

releaseReadyCandidate.acceptanceCriteria[0].evidence = [openEvidence];
mustNotBeReleaseReady(releaseReadyCandidate);

// The original suite carried 11 hostile mutations; new manifest fields may only
// add to that set, never replace it.
assert.ok(hostileMutations >= 11);

// The dirty-baseline existence rule lives in the CLI verifier rather than the
// decoder, so it is exercised end to end against a synthetic repository whose
// tracked/untracked state is built on purpose. A PROTECTED_EXTERNAL_UNTRACKED
// path was never in the index, so a fresh clone legitimately lacks it; every
// other missing recorded path — including a tracked path mislabelled as
// untracked — must still fail the gate.
const scriptsDirectory = dirname(fileURLToPath(import.meta.url));
const evidenceDirectory = "docs/exec-plans/evidence";
const fixtureManifestPath = `${evidenceDirectory}/canonical-v1-goal-closure-v1.json`;
const fixturePrimaryEvidence = `${evidenceDirectory}/self-test-evidence-a.json`;
const fixtureSecondaryEvidence = `${evidenceDirectory}/self-test-evidence-b.json`;

const rebindEvidencePaths = (value) => {
  if (Array.isArray(value)) {
    value.forEach(rebindEvidencePaths);
    return;
  }
  if (value === null || typeof value !== "object") {
    return;
  }
  const keys = Object.keys(value).sort();
  if (keys.length === 2 && keys[0] === "path" && keys[1] === "status") {
    value.path = fixturePrimaryEvidence;
    return;
  }
  Object.values(value).forEach(rebindEvidencePaths);
};

let dirtyBaselineCases = 0;
const runFixtureVerifier = ({ protectedPaths, tracked, deleted }) => {
  const fixtureRoot = mkdtempSync(resolve(tmpdir(), "midgard-goal-closure-"));
  try {
    mkdirSync(resolve(fixtureRoot, "demo/scripts"), { recursive: true });
    mkdirSync(resolve(fixtureRoot, evidenceDirectory), { recursive: true });
    for (const script of [
      "canonical-v1-goal-closure-v1.mjs",
      "verify-canonical-v1-goal-closure.mjs",
    ]) {
      copyFileSync(
        resolve(scriptsDirectory, script),
        resolve(fixtureRoot, "demo/scripts", script),
      );
    }
    writeFileSync(resolve(fixtureRoot, fixturePrimaryEvidence), "{}\n");
    writeFileSync(resolve(fixtureRoot, fixtureSecondaryEvidence), "{}\n");
    for (const path of tracked) {
      writeFileSync(resolve(fixtureRoot, path), "baseline\n");
    }
    const fixtureManifest = clone(manifest);
    rebindEvidencePaths(fixtureManifest);
    fixtureManifest.targetTestnetParameterSnapshot.path =
      fixtureSecondaryEvidence;
    fixtureManifest.revision.releaseCommit = null;
    fixtureManifest.dirtyBaseline.protectedPaths = protectedPaths;
    const writeFixtureManifest = () =>
      writeFileSync(
        resolve(fixtureRoot, fixtureManifestPath),
        `${JSON.stringify(fixtureManifest, null, 2)}\n`,
      );
    const git = (...gitArgs) =>
      execFileSync("git", gitArgs, {
        cwd: fixtureRoot,
        encoding: "utf8",
        stdio: ["ignore", "pipe", "pipe"],
      }).trim();
    writeFixtureManifest();
    git("init", "-b", fixtureManifest.revision.branch);
    git("config", "user.email", "closure-self-test@example.invalid");
    git("config", "user.name", "closure self-test");
    git("config", "commit.gpgsign", "false");
    git("add", "-A");
    git("commit", "-m", "closure self-test fixture");
    fixtureManifest.revision.headCommit = git("rev-parse", "HEAD");
    fixtureManifest.dirtyBaseline.startingRevision =
      fixtureManifest.revision.headCommit;
    writeFixtureManifest();
    for (const path of deleted) {
      rmSync(resolve(fixtureRoot, path), { force: true });
    }
    const result = spawnSync(
      process.execPath,
      ["demo/scripts/verify-canonical-v1-goal-closure.mjs", "--schema-only"],
      { cwd: fixtureRoot, encoding: "utf8" },
    );
    dirtyBaselineCases += 1;
    return result;
  } finally {
    rmSync(fixtureRoot, { recursive: true, force: true });
  }
};

// A recorded-untracked path that a clean checkout never had passes the gate.
const absentUntracked = runFixtureVerifier({
  protectedPaths: [
    { path: "tracked-present.txt", disposition: "PROTECTED_EXACT_BASELINE" },
    { path: "scratch-note.md", disposition: "PROTECTED_EXTERNAL_UNTRACKED" },
  ],
  tracked: ["tracked-present.txt"],
  deleted: [],
});
assert.equal(absentUntracked.status, 0, absentUntracked.stderr);
assert.match(absentUntracked.stdout, /"status":"schema-valid"/u);

// A recorded TRACKED path that is missing is deleted evidence: still fatal.
const absentTracked = runFixtureVerifier({
  protectedPaths: [
    { path: "tracked-absent.txt", disposition: "PROTECTED_EXACT_BASELINE" },
  ],
  tracked: ["tracked-absent.txt"],
  deleted: ["tracked-absent.txt"],
});
assert.equal(absentTracked.status, 1);
assert.match(
  absentTracked.stderr,
  /recorded dirty-baseline path is missing: tracked-absent\.txt/u,
);

// The untracked disposition is checked against Git, not believed: a tracked
// path mislabelled PROTECTED_EXTERNAL_UNTRACKED cannot buy its way out.
const mislabelledTracked = runFixtureVerifier({
  protectedPaths: [
    {
      path: "tracked-absent.txt",
      disposition: "PROTECTED_EXTERNAL_UNTRACKED",
    },
  ],
  tracked: ["tracked-absent.txt"],
  deleted: ["tracked-absent.txt"],
});
assert.equal(mislabelledTracked.status, 1);
assert.match(
  mislabelledTracked.stderr,
  /recorded dirty-baseline path is missing: tracked-absent\.txt/u,
);

process.stdout.write(
  `${JSON.stringify({
    status: "PASS",
    hostileMutations,
    releaseGateRejections,
    dirtyBaselineCases,
    criteria: manifest.acceptanceCriteria.length,
  })}\n`,
);
