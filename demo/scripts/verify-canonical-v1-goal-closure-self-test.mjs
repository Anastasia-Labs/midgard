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
  DECLARED_EVIDENCE_PATHS,
  allBound,
  canonicalClosureDigest,
  decodeCanonicalV1GoalClosure,
  isReleaseReady,
  parameterSnapshotDigest,
  releaseBlockers,
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
// owner acceptance, never a duplicate id, and never an acceptance stamp that
// does not name who accepted it and when.
const openBlocker = (patch = {}) => ({
  id: "RB-1",
  description: "named residual launch blocker",
  ownerAccepted: false,
  acceptedBy: null,
  acceptedAt: null,
  evidence: [],
  ...patch,
});
const acceptedBlocker = (patch = {}) =>
  openBlocker({
    ownerAccepted: true,
    acceptedBy: "goal owner",
    acceptedAt: manifest.generatedAt,
    evidence: [boundEvidence],
    ...patch,
  });

mustReject((candidate) => {
  delete candidate.residualBlockers;
}, /expected keys/u);
mustReject((candidate) => {
  candidate.residualBlockers = [openBlocker({ ownerAccepted: "yes" })];
}, /expected a boolean/u);
mustReject((candidate) => {
  candidate.residualBlockers = [acceptedBlocker({ evidence: [] })];
}, /ownerAccepted requires at least one bound evidence file/u);
mustReject((candidate) => {
  candidate.residualBlockers = [acceptedBlocker({ evidence: [openEvidence] })];
}, /ownerAccepted requires all evidence files to be bound/u);
mustReject((candidate) => {
  candidate.residualBlockers = [
    openBlocker({ description: "first" }),
    openBlocker({ description: "second" }),
  ];
}, /duplicate id/u);
mustReject((candidate) => {
  candidate.residualBlockers = [openBlocker({ owner: "someone" })];
}, /expected keys \[acceptedAt, acceptedBy, description, evidence, id, ownerAccepted\]/u);
// Acceptance must name an owner and a time — and an unaccepted blocker must not
// carry a stamp it never earned.
mustReject((candidate) => {
  candidate.residualBlockers = [acceptedBlocker({ acceptedBy: null })];
}, /ownerAccepted requires acceptedBy and only then/u);
mustReject((candidate) => {
  candidate.residualBlockers = [acceptedBlocker({ acceptedAt: null })];
}, /ownerAccepted requires acceptedAt and only then/u);
mustReject((candidate) => {
  candidate.residualBlockers = [openBlocker({ acceptedBy: "goal owner" })];
}, /ownerAccepted requires acceptedBy and only then/u);
mustReject((candidate) => {
  candidate.residualBlockers = [
    openBlocker({ acceptedAt: manifest.generatedAt }),
  ];
}, /ownerAccepted requires acceptedAt and only then/u);

// GOAL_SPEC §0.2 declared evidence paths: the manifest may not widen, narrow,
// reorder, or relabel the three classes the spec fixes.
mustReject((candidate) => {
  delete candidate.evidencePaths;
}, /expected keys/u);
mustReject((candidate) => {
  candidate.evidencePaths.pop();
}, /must declare exactly the GOAL_SPEC §0\.2 evidence paths/u);
mustReject((candidate) => {
  candidate.evidencePaths.push({ path: "onchain/", kind: "TREE" });
}, /must declare exactly the GOAL_SPEC §0\.2 evidence paths/u);
mustReject((candidate) => {
  candidate.evidencePaths[0].path = "onchain/aiken/";
}, /must declare exactly the GOAL_SPEC §0\.2 evidence paths/u);
mustReject((candidate) => {
  candidate.evidencePaths[2].kind = "FILE";
}, /must declare exactly the GOAL_SPEC §0\.2 evidence paths/u);
mustReject((candidate) => {
  candidate.evidencePaths.reverse();
}, /must declare exactly the GOAL_SPEC §0\.2 evidence paths/u);
mustReject((candidate) => {
  candidate.evidencePaths[1].kind = "DIRECTORY";
}, /expected one of FILE, TREE/u);

// GOAL_SPEC §13.3/C70 snapshot identity: a BOUND snapshot must carry a digest,
// an OPEN one must not, and the two snapshots must be different parameter sets.
mustReject((candidate) => {
  candidate.parameterSnapshot.status = "BOUND";
}, /BOUND requires snapshotDigest and OPEN requires snapshotDigest=null/u);
mustReject((candidate) => {
  candidate.parameterSnapshot.snapshotDigest = "a".repeat(64);
}, /BOUND requires snapshotDigest and OPEN requires snapshotDigest=null/u);
mustReject((candidate) => {
  candidate.parameterSnapshot.status = "BOUND";
  candidate.parameterSnapshot.snapshotDigest = "not-a-digest";
}, /expected a lowercase SHA-256 digest/u);
mustReject((candidate) => {
  candidate.parameterSnapshot.status = "BOUND";
  candidate.parameterSnapshot.snapshotDigest = "a".repeat(64);
  candidate.targetTestnetParameterSnapshot.status = "BOUND";
  candidate.targetTestnetParameterSnapshot.snapshotDigest = "a".repeat(64);
}, /must be distinct parameter sets/u);
mustReject((candidate) => {
  delete candidate.parameterSnapshot.snapshotDigest;
}, /expected keys \[path, snapshotDigest, status\]/u);

// GOAL_SPEC §2.4/§13.4 regeneration records.
const regenerationRecord = (patch = {}) => ({
  id: "REGEN-1",
  description: "emulator sweep log",
  command: ["pnpm", "--dir", "demo", "test"],
  outputPath: "demo/.artifacts/emulator-sweep.log",
  reason: "TOO_LARGE",
  ...patch,
});
mustReject((candidate) => {
  delete candidate.regenerationRecords;
}, /expected keys/u);
mustReject((candidate) => {
  candidate.regenerationRecords = [regenerationRecord({ command: [] })];
}, /expected at least one command token/u);
mustReject((candidate) => {
  candidate.regenerationRecords = [regenerationRecord({ reason: "BECAUSE" })];
}, /expected one of TOO_LARGE, TOO_TRANSIENT, UNSAFE_TO_COMMIT/u);
mustReject((candidate) => {
  candidate.regenerationRecords = [
    regenerationRecord(),
    regenerationRecord({ outputPath: "demo/.artifacts/other.log" }),
  ];
}, /duplicate id/u);
mustReject((candidate) => {
  candidate.regenerationRecords = [
    regenerationRecord(),
    regenerationRecord({ id: "REGEN-2" }),
  ];
}, /duplicate outputPath/u);
mustReject((candidate) => {
  candidate.regenerationRecords = [regenerationRecord({ note: "extra" })];
}, /expected keys \[command, description, id, outputPath, reason\]/u);

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
releaseReadyCandidate.parameterSnapshot.snapshotDigest = "a".repeat(64);
releaseReadyCandidate.targetTestnetParameterSnapshot.status = "BOUND";
releaseReadyCandidate.targetTestnetParameterSnapshot.snapshotDigest =
  "b".repeat(64);
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
residualBlockerCandidate.residualBlockers = [openBlocker()];
decodeCanonicalV1GoalClosure(residualBlockerCandidate);
mustNotBeReleaseReady(residualBlockerCandidate);
residualBlockerCandidate.residualBlockers = [acceptedBlocker()];
decodeCanonicalV1GoalClosure(residualBlockerCandidate);
assert.equal(isReleaseReady(residualBlockerCandidate), true);

// The release gate names what is missing rather than answering "incomplete":
// an operator facing 35 criteria needs the list, and a blocker list that never
// names anything is the failure mode that lets an open criterion through.
const unboundReleaseCommit = clone(releaseReadyCandidate);
unboundReleaseCommit.revision.releaseCommit = null;
assert.deepEqual(releaseBlockers(unboundReleaseCommit), [
  "revision.releaseCommit is unbound",
]);
assert.deepEqual(releaseBlockers(releaseReadyCandidate), []);
const openCriterionCandidate = clone(releaseReadyCandidate);
openCriterionCandidate.acceptanceCriteria[3].status = "OPEN";
assert.match(
  releaseBlockers(openCriterionCandidate).join("; "),
  /1 acceptance criteria are open or unevidenced: AC-03/u,
);
const failedCommandCandidate = clone(releaseReadyCandidate);
failedCommandCandidate.commandResults[0].exitCode = 1;
assert.match(
  releaseBlockers(failedCommandCandidate).join("; "),
  /commandResults contain nonzero exits: self-test/u,
);

releaseReadyCandidate.acceptanceCriteria[0].evidence = [openEvidence];
mustNotBeReleaseReady(releaseReadyCandidate);

// Pinned to the exact measured count rather than a floor, so a guard cannot be
// deleted and replaced by a new one without the number moving. The suite grew
// 11 -> 24 -> 46 as the §0.2 release binding landed; each step added coverage
// and removed none. Raising this requires adding mutations, never relaxing one.
assert.equal(hostileMutations, 46);

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

// ---------------------------------------------------------------------------
// GOAL_SPEC §0.2 release binding, end to end.
//
// A release gate that has never been observed to pass is indistinguishable from
// one that cannot pass, and "make the real manifest release-ready" is not an
// option — it would mean asserting a closure the Goal has not earned. So the
// full §0.2 shape is built in a synthetic repository: a releaseCommit holding
// every source-bearing file, then an evidence-only descendant carrying the
// manifest that names it. The gate must exit 0 there, and must still reject
// each individual violation of the binding.
// ---------------------------------------------------------------------------
let releaseGatePasses = 0;
let releaseBindingRejections = 0;

const buildReleaseFixture = ({
  mutateManifest = () => {},
  tamperAfterDigest = () => {},
  extraReleaseFiles = {},
  evidenceCommitFiles = {},
  dirtyAfterCommit = {},
  readiness = "# public testnet readiness\n",
} = {}) => {
  const fixtureRoot = mkdtempSync(resolve(tmpdir(), "midgard-goal-release-"));
  const git = (...gitArgs) =>
    execFileSync("git", gitArgs, {
      cwd: fixtureRoot,
      encoding: "utf8",
      stdio: ["ignore", "pipe", "pipe"],
    }).trim();

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

  // Two genuinely different C70 parameter sets, so the digests differ for a
  // real reason rather than because the fixture hand-picked two constants.
  const mainnetSnapshot = { network: "mainnet", maxTxSize: 16384 };
  const testnetSnapshot = {
    network: "preprod",
    maxTxSize: 16384,
    pending: true,
  };
  const mainnetPath = `${evidenceDirectory}/param-mainnet.json`;
  const testnetPath = `${evidenceDirectory}/param-testnet.json`;
  const releaseFiles = {
    [mainnetPath]: `${JSON.stringify(mainnetSnapshot, null, 2)}\n`,
    [testnetPath]: `${JSON.stringify(testnetSnapshot, null, 2)}\n`,
    [fixturePrimaryEvidence]: "{}\n",
    [fixtureSecondaryEvidence]: "{}\n",
    "public_testnet_readiness.md": readiness,
    "baseline-drift.txt": "baseline\n",
    ...extraReleaseFiles,
  };
  for (const [path, content] of Object.entries(releaseFiles)) {
    mkdirSync(dirname(resolve(fixtureRoot, path)), { recursive: true });
    writeFileSync(resolve(fixtureRoot, path), content);
  }

  const fixtureManifest = clone(manifest);
  rebindEvidencePaths(fixtureManifest);
  fixtureManifest.revision.worktree = "BASELINE_RELATIVE_CLEAN";
  fixtureManifest.parameterSnapshot = {
    path: mainnetPath,
    status: "BOUND",
    snapshotDigest: parameterSnapshotDigest(releaseFiles[mainnetPath]),
  };
  fixtureManifest.targetTestnetParameterSnapshot = {
    path: testnetPath,
    status: "BOUND",
    snapshotDigest: parameterSnapshotDigest(releaseFiles[testnetPath]),
  };
  fixtureManifest.blueprint = { path: fixturePrimaryEvidence, status: "BOUND" };
  fixtureManifest.closureArtifacts = [
    fixturePrimaryEvidence,
    fixtureSecondaryEvidence,
    mainnetPath,
    testnetPath,
    "public_testnet_readiness.md",
  ].map((path) => ({ path, status: "BOUND" }));
  fixtureManifest.validatorSet = {
    identity: "validator-set",
    status: "BOUND",
    evidence: [{ path: fixturePrimaryEvidence, status: "BOUND" }],
  };
  fixtureManifest.deployment = {
    network: "preprod",
    identity: "deployment",
    status: "BOUND",
    evidence: [{ path: fixturePrimaryEvidence, status: "BOUND" }],
  };
  fixtureManifest.fixtureSets = [
    { path: fixtureSecondaryEvidence, status: "BOUND" },
  ];
  fixtureManifest.secrets = {
    scanStatus: "PASS",
    evidence: [{ path: fixturePrimaryEvidence, status: "BOUND" }],
  };
  fixtureManifest.acceptanceCriteria.forEach((criterion) => {
    criterion.status = "PASS";
    criterion.evidence = [{ path: fixturePrimaryEvidence, status: "BOUND" }];
  });
  fixtureManifest.dirtyBaseline.protectedPaths = [
    {
      path: "baseline-drift.txt",
      disposition: "PROTECTED_ACCEPTED_EXTERNAL_DRIFT",
    },
  ];

  git("init", "-b", fixtureManifest.revision.branch);
  git("config", "user.email", "closure-self-test@example.invalid");
  git("config", "user.name", "closure self-test");
  git("config", "commit.gpgsign", "false");
  git("add", "-A");
  git("commit", "-m", "release commit");
  const releaseCommit = git("rev-parse", "HEAD");

  fixtureManifest.revision.releaseCommit = releaseCommit;
  fixtureManifest.revision.headCommit = releaseCommit;
  fixtureManifest.dirtyBaseline.startingRevision = releaseCommit;
  fixtureManifest.commandResults = [
    {
      artifactIdentity: null,
      command: ["node", "self-test"],
      durationMs: 1,
      exitCode: 0,
      finishedAt: fixtureManifest.generatedAt,
      id: "self-test",
      revision: releaseCommit,
      testCount: 1,
    },
  ];
  mutateManifest(fixtureManifest, { fixtureRoot, releaseCommit });
  fixtureManifest.release = {
    status: "BOUND",
    digestAlgorithm: "sha256",
    digest: canonicalClosureDigest(fixtureManifest),
  };
  // Runs after the digest is sealed, so a case can alter a covered field
  // without the digest silently re-deriving to match the tampered value.
  tamperAfterDigest(fixtureManifest);

  // The evidence commit: the manifest that names releaseCommit, plus whatever
  // the individual case wants to smuggle alongside it.
  for (const [path, content] of Object.entries(evidenceCommitFiles)) {
    mkdirSync(dirname(resolve(fixtureRoot, path)), { recursive: true });
    writeFileSync(resolve(fixtureRoot, path), content);
  }
  writeFileSync(
    resolve(fixtureRoot, fixtureManifestPath),
    `${JSON.stringify(fixtureManifest, null, 2)}\n`,
  );
  git("add", "-A");
  git("commit", "-m", "evidence commit");

  // Recreate the recorded baseline drift so the worktree is baseline-relative
  // clean rather than pristine — the state a real closure is verified in.
  writeFileSync(resolve(fixtureRoot, "baseline-drift.txt"), "drifted\n");
  for (const [path, content] of Object.entries(dirtyAfterCommit)) {
    mkdirSync(dirname(resolve(fixtureRoot, path)), { recursive: true });
    writeFileSync(resolve(fixtureRoot, path), content);
  }

  const result = spawnSync(
    process.execPath,
    ["demo/scripts/verify-canonical-v1-goal-closure.mjs", "--release"],
    { cwd: fixtureRoot, encoding: "utf8" },
  );
  rmSync(fixtureRoot, { recursive: true, force: true });
  return result;
};

// The gate passes on a correctly bound §0.2 release.
const releasePass = buildReleaseFixture();
assert.equal(releasePass.status, 0, releasePass.stderr);
assert.match(releasePass.stdout, /"status":"release-ready"/u);
assert.match(releasePass.stdout, /"releaseCommitsBound":1/u);
assert.match(releasePass.stdout, /"escapingEvidencePaths":0/u);
assert.match(releasePass.stdout, /"passingCriteria":35/u);
assert.match(releasePass.stdout, /"parameterSnapshots":2/u);
assert.match(releasePass.stdout, /"evidencePathClasses":3/u);
releaseGatePasses += 1;

const mustRejectRelease = (options, pattern) => {
  const result = buildReleaseFixture(options);
  assert.equal(result.status, 1, `expected rejection, got:\n${result.stdout}`);
  assert.match(result.stderr, pattern);
  releaseBindingRejections += 1;
};

// A commit after releaseCommit that touches a source-bearing file is not an
// evidence commit: it moves releaseCommit and invalidates the binding.
mustRejectRelease(
  { evidenceCommitFiles: { "onchain/aiken/validators/thing.ak": "changed\n" } },
  /diff escapes declared evidence paths: onchain\/aiken\/validators\/thing\.ak/u,
);

// The evidence-path allowance is a prefix rule on the declared TREE class, not
// a substring one: a sibling directory that merely starts similarly is outside.
mustRejectRelease(
  { evidenceCommitFiles: { "docs/exec-plans/evidence-extra/x.json": "{}\n" } },
  /diff escapes declared evidence paths: docs\/exec-plans\/evidence-extra\/x\.json/u,
);

// §0.2: the manifest cannot name the commit that contains it.
mustRejectRelease(
  {
    mutateManifest: (fixtureManifest) => {
      fixtureManifest.revision.releaseCommit = "0".repeat(40);
    },
  },
  /is not HEAD or an ancestor of it/u,
);

// A BOUND snapshot whose recorded identity is not the identity of the snapshot
// actually committed fails: the digest is recomputed, never believed.
mustRejectRelease(
  {
    mutateManifest: (fixtureManifest) => {
      fixtureManifest.targetTestnetParameterSnapshot.snapshotDigest =
        "c".repeat(64);
    },
  },
  /targetTestnetParameterSnapshot snapshotDigest mismatch/u,
);

// §9.5: a residual launch blocker recorded here but absent from the public
// readiness document is exactly the silence the rule forbids.
mustRejectRelease(
  {
    mutateManifest: (fixtureManifest) => {
      fixtureManifest.residualBlockers = [
        {
          id: "RB-SILENT",
          description: "not named publicly",
          ownerAccepted: true,
          acceptedBy: "goal owner",
          acceptedAt: fixtureManifest.generatedAt,
          evidence: [{ path: fixturePrimaryEvidence, status: "BOUND" }],
        },
      ];
    },
  },
  /residual launch blocker RB-SILENT is not named in root public_testnet_readiness\.md/u,
);

// The same blocker passes once the public readiness document names it, so the
// control above is satisfiable rather than a blanket ban on blockers.
const namedBlockerRelease = buildReleaseFixture({
  readiness: "# public testnet readiness\n\n- RB-SILENT: accepted residual\n",
  mutateManifest: (fixtureManifest) => {
    fixtureManifest.residualBlockers = [
      {
        id: "RB-SILENT",
        description: "named publicly",
        ownerAccepted: true,
        acceptedBy: "goal owner",
        acceptedAt: fixtureManifest.generatedAt,
        evidence: [{ path: fixturePrimaryEvidence, status: "BOUND" }],
      },
    ];
  },
});
assert.equal(namedBlockerRelease.status, 0, namedBlockerRelease.stderr);
assert.match(namedBlockerRelease.stdout, /"residualBlockers":1/u);
releaseGatePasses += 1;

// §2.4: a regeneration record is for what cannot be committed. Pointing one at
// a tracked file means the evidence should simply have been bound by path.
mustRejectRelease(
  {
    mutateManifest: (fixtureManifest) => {
      fixtureManifest.regenerationRecords = [
        {
          id: "REGEN-1",
          description: "tracked output masquerading as transient",
          command: ["pnpm", "--dir", "demo", "test"],
          outputPath: fixturePrimaryEvidence,
          reason: "TOO_LARGE",
        },
      ];
    },
  },
  /regeneration record REGEN-1 declares tracked outputPath/u,
);

// An untracked regeneration output is accepted: the control is satisfiable.
const regenerationRelease = buildReleaseFixture({
  mutateManifest: (fixtureManifest) => {
    fixtureManifest.regenerationRecords = [
      {
        id: "REGEN-1",
        description: "emulator sweep log, too large to commit",
        command: ["pnpm", "--dir", "demo", "test"],
        outputPath: "demo/.artifacts/emulator-sweep.log",
        reason: "TOO_LARGE",
      },
    ];
  },
});
assert.equal(regenerationRelease.status, 0, regenerationRelease.stderr);
assert.match(regenerationRelease.stdout, /"regenerationRecords":1/u);
releaseGatePasses += 1;

// A field edited after the digest was sealed fails: the release digest is
// recomputed from the manifest's own content, not taken on trust.
mustRejectRelease(
  {
    tamperAfterDigest: (fixtureManifest) => {
      fixtureManifest.generatedAt = "2026-01-01T00:00:00.000Z";
    },
  },
  /release digest mismatch/u,
);

// An open acceptance criterion keeps release closed, and the gate names it
// rather than reporting a bare "incomplete".
mustRejectRelease(
  {
    mutateManifest: (fixtureManifest) => {
      fixtureManifest.acceptanceCriteria[7].status = "OPEN";
    },
  },
  /1 acceptance criteria are open or unevidenced: AC-C30/u,
);

// A nonzero recorded command exit keeps release closed and is named.
mustRejectRelease(
  {
    mutateManifest: (fixtureManifest) => {
      fixtureManifest.commandResults[0].exitCode = 2;
    },
  },
  /commandResults contain nonzero exits: self-test/u,
);

// An unrecorded dirty path keeps release closed: the worktree must be clean
// relative to exactly the recorded baseline, no more and no less.
mustRejectRelease(
  { dirtyAfterCommit: { "stray.txt": "unrecorded\n" } },
  /worktree is not baseline-relative clean/u,
);

process.stdout.write(
  `${JSON.stringify({
    status: "PASS",
    hostileMutations,
    releaseGateRejections,
    dirtyBaselineCases,
    releaseGatePasses,
    releaseBindingRejections,
    criteria: manifest.acceptanceCriteria.length,
    declaredEvidencePaths: DECLARED_EVIDENCE_PATHS.length,
  })}\n`,
);
