#!/usr/bin/env node

// Behavioural self-test for the watcher dependency map's git-authority
// bindings (issue #537). Before this lane the map's §0-integrity revisions and
// the six W13-W17/W23 `independentAudit` strings were compared only to
// hardcoded copies of themselves inside
// demo/scripts/verify-canonical-v1-watcher-dependency-map.mjs: the revisions
// did not have to exist, and editing both literals passed CI. The bindings are
// asked of Git now, and this suite proves it by behaviour rather than by
// reading the gate's source: each hostile provenance claim is written into a
// throwaway copy of the map, the real gate is invoked against that copy through
// its `--map-under-test=` hook, and the run must exit non-zero carrying the
// expected diagnostic. Only the map is redirected — Git history, the staged
// watcher sources and the CI workflows the gate checks stay the real ones, so a
// seeded claim is judged against reality. A clean copy is run first and last,
// so a gate that rejected everything (or nothing) could not pass this suite.

import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(scriptDir, "../..");
const gatePath = resolve(
  scriptDir,
  "verify-canonical-v1-watcher-dependency-map.mjs",
);
const mapPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
);
const publishedMap = JSON.parse(readFileSync(mapPath, "utf8"));
const clone = (value) => structuredClone(value);

const git = (...args) =>
  execFileSync("git", args, {
    cwd: repoRoot,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  }).trim();

const headRevision = git("rev-parse", "HEAD");
// A commit object that genuinely exists but is not reachable from HEAD: it is
// built ON TOP of HEAD and never referenced, so `merge-base --is-ancestor` must
// reject it. Using a real object rather than random hex proves the gate asks
// about reachability and not merely about existence or hex shape.
const nonAncestorRevision = execFileSync(
  "git",
  [
    "commit-tree",
    `${headRevision}^{tree}`,
    "-p",
    headRevision,
    "-m",
    "issue #537 self-test probe: a real commit that is not an ancestor of HEAD",
  ],
  {
    cwd: repoRoot,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
    env: {
      ...process.env,
      GIT_AUTHOR_NAME: "midgard-self-test",
      GIT_AUTHOR_EMAIL: "self-test@example.invalid",
      GIT_AUTHOR_DATE: "2026-08-06T00:00:00+00:00",
      GIT_COMMITTER_NAME: "midgard-self-test",
      GIT_COMMITTER_EMAIL: "self-test@example.invalid",
      GIT_COMMITTER_DATE: "2026-08-06T00:00:00+00:00",
    },
  },
).trim();
assert.match(
  nonAncestorRevision,
  /^[0-9a-f]{40}$/u,
  "the non-ancestor probe commit must be a full commit id",
);
const absentRevision = "0".repeat(39) + "1";

const workspace = mkdtempSync(resolve(tmpdir(), "watcher-dependency-map-"));
const candidatePath = resolve(workspace, "map.json");
const runGate = (candidate) => {
  writeFileSync(candidatePath, `${JSON.stringify(candidate, null, 2)}\n`);
  const result = spawnSync(
    process.execPath,
    [gatePath, `--map-under-test=${candidatePath}`],
    { cwd: repoRoot, encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
  );
  assert.ok(
    result.status !== null,
    `gate did not run: ${result.error?.message ?? "unknown failure"}`,
  );
  return result;
};

// Opening positive control: the map as published must be accepted.
const openingControl = runGate(clone(publishedMap));
assert.equal(
  openingControl.status,
  0,
  `published map must pass: ${openingControl.stderr}`,
);

let acceptedControls = 1;
let rejectedMutations = 0;
const mustReject = (label, expected, mutate) => {
  const candidate = clone(publishedMap);
  mutate(candidate);
  const { status, stderr } = runGate(candidate);
  assert.notEqual(status, 0, `${label}: gate exited 0 on a seeded defect`);
  assert.match(stderr, expected, `${label}: missing diagnostic in:\n${stderr}`);
  process.stdout.write(
    `rejected: ${label} -> exit ${status}, ${stderr.match(expected)[0]}\n`,
  );
  rejectedMutations += 1;
};
const mustAccept = (label, adjust) => {
  const candidate = clone(publishedMap);
  adjust(candidate);
  const { status, stderr } = runGate(candidate);
  assert.equal(
    status,
    0,
    `${label}: gate rejected a legitimate map: ${stderr}`,
  );
  process.stdout.write(`accepted: ${label}\n`);
  acceptedControls += 1;
};

const rollbackReview = () =>
  publishedMap.requiredWatcherPackage.rollbackEngine.reviewRecord;

// §0 binding 1: the published merge and its two reviewed parents.
mustReject(
  "a published parent revision that does not exist",
  /publishedParentRevisions\[1\] .* is not an ancestor of HEAD/u,
  (candidate) => {
    candidate.authority.publishedParentRevisions[1] = absentRevision;
    candidate.authority.publishedMergeRevision = absentRevision;
  },
);
mustReject(
  "a published merge revision that is not an ancestor of HEAD",
  /publishedMergeRevision .* is not an ancestor of HEAD/u,
  (candidate) => {
    candidate.authority.publishedMergeRevision = nonAncestorRevision;
  },
);
mustReject(
  "the declared parents are not the merge's parents, in order",
  /does not merge exactly .* in that order/u,
  (candidate) => {
    candidate.authority.publishedParentRevisions.reverse();
    candidate.authority.publishedParentArtifactSha256.reverse();
  },
);
mustReject(
  "a published parent's historical bytes do not match the declared digest",
  /publishedParentArtifactSha256\[0\] declares .* hashes to/u,
  (candidate) => {
    candidate.authority.publishedParentArtifactSha256[0] =
      candidate.authority.sourceArtifactSha256;
  },
);
mustReject(
  "a published parent digest that is not a SHA-256 digest",
  /publishedParentArtifactSha256 must be one 64-character SHA-256 hex digest/u,
  (candidate) => {
    candidate.authority.publishedParentArtifactSha256[1] = "not-a-digest";
  },
);

// §0 binding 2: the revision this artifact was first published at.
mustReject(
  "a source revision that is not an ancestor of HEAD",
  /sourceRevision .* is not an ancestor of HEAD/u,
  (candidate) => {
    candidate.authority.sourceRevision = nonAncestorRevision;
  },
);
mustReject(
  "a source revision that postdates the reviewed parents",
  /sourceRevision .* is not an ancestor of publishedParentRevisions\[0\]/u,
  (candidate) => {
    candidate.authority.sourceRevision = headRevision;
  },
);
mustReject(
  "a source revision that does not contain the published artifact",
  /sourceRevision .* does not contain docs\/exec-plans\/evidence/u,
  (candidate) => {
    candidate.authority.sourceRevision = candidate.authority.baseRevision;
  },
);
mustReject(
  "the source revision's historical bytes do not match the declared digest",
  /sourceArtifactSha256 declares .* hashes to/u,
  (candidate) => {
    candidate.authority.sourceArtifactSha256 =
      candidate.authority.publishedParentArtifactSha256[0];
  },
);

// §0 binding 3: the pre-publication upstream base.
mustReject(
  "a base revision whose tree is not the declared tree object",
  /baseTree declares .* but git resolves/u,
  (candidate) => {
    candidate.authority.baseTree = candidate.authority.publishedMergeRevision;
  },
);
mustReject(
  "a base revision that already contains the published artifact",
  /already contains .* so it is not the pre-publication base/u,
  (candidate) => {
    candidate.authority.baseRevision = candidate.authority.sourceRevision;
    candidate.authority.baseTree = git(
      "rev-parse",
      `${candidate.authority.sourceRevision}^{tree}`,
    );
  },
);
mustReject(
  "the bound artifact path is not this artifact",
  /artifactPath must be docs\/exec-plans\/evidence/u,
  (candidate) => {
    candidate.authority.artifactPath = "GOAL_SPEC.md";
  },
);

// The six migrated review records.
mustReject(
  "a review record whose revision is not a full commit id",
  /W13 rollback-engine reviewRecord\.reviewedAtRev must be a full 40-character Git commit/u,
  (candidate) => {
    candidate.requiredWatcherPackage.rollbackEngine.reviewRecord.reviewedAtRev =
      rollbackReview().reviewedAtRev.slice(0, 12);
  },
);
mustReject(
  "a review record at a revision that is not an ancestor of HEAD",
  /W14 state-queue-indexer reviewRecord\.reviewedAtRev .* is not an ancestor of HEAD/u,
  (candidate) => {
    candidate.requiredWatcherPackage.stateQueueIndexer.reviewRecord.reviewedAtRev =
      nonAncestorRevision;
  },
);
mustReject(
  "a review record at a revision that never changed the reviewed paths",
  /W15 user-event-indexer reviewRecord\.reviewedAtRev .* did not change the reviewed paths/u,
  (candidate) => {
    candidate.requiredWatcherPackage.userEventIndexer.reviewRecord.reviewedAtRev =
      headRevision;
  },
);
mustReject(
  "a review record that omits the module it claims to review",
  /W16 settlement-indexer reviewRecord\.reviewedPaths does not cover/u,
  (candidate) => {
    candidate.requiredWatcherPackage.settlementIndexer.reviewRecord.reviewedPaths =
      ["demo/midgard-watcher/tests/settlement-indexer.test.ts"];
  },
);
mustReject(
  "a review record naming a path that does not exist at its revision",
  /W17 proof-thread-indexer reviewRecord\.reviewedAtRev .* does not contain demo\/midgard-watcher\/src\/block-replay\.ts/u,
  (candidate) => {
    candidate.requiredWatcherPackage.proofThreadIndexer.reviewRecord.reviewedPaths.push(
      "demo/midgard-watcher/src/block-replay.ts",
    );
  },
);
mustReject(
  "a review record with an escaping reviewed path",
  /W23 rule-bundle reviewRecord\.reviewedPaths must be a non-empty array of unique repository-relative paths/u,
  (candidate) => {
    candidate.requiredWatcherPackage.ruleBundle.reviewRecord.reviewedPaths.push(
      "../etc/passwd",
    );
  },
);
mustReject(
  "a review record with an empty summary",
  /W13 rollback-engine reviewRecord\.summary must be non-empty prose/u,
  (candidate) => {
    candidate.requiredWatcherPackage.rollbackEngine.reviewRecord.summary =
      "   ";
  },
);
mustReject(
  "a review record carrying an unrecognised field",
  /W13 rollback-engine reviewRecord carries the unknown field verdict/u,
  (candidate) => {
    candidate.requiredWatcherPackage.rollbackEngine.reviewRecord.verdict =
      "PASS";
  },
);
mustReject(
  "a missing review record",
  /W14 state-queue-indexer reviewRecord must be an object/u,
  (candidate) => {
    delete candidate.requiredWatcherPackage.stateQueueIndexer.reviewRecord;
  },
);
// The summary is prose, never evidence: rewriting it must not change any
// verdict. This is the exact freedom the retired `independentAudit` literals
// did not have, and the reason they proved nothing.
mustAccept("a rewritten review summary is prose, not evidence", (candidate) => {
  candidate.requiredWatcherPackage.rollbackEngine.reviewRecord.summary =
    "Rewritten summary text with no bearing on any assertion in the gate.";
});

// The optional second-party audit slot.
mustAccept("a well-formed second-party audit record", (candidate) => {
  candidate.requiredWatcherPackage.ruleBundle.reviewRecord.secondPartyAudit = {
    auditor: "example-external-reviewer",
    rev: headRevision,
    reportDigest: "a".repeat(64),
  };
});
mustReject(
  "a second-party audit whose report digest is not a SHA-256 digest",
  /W23 rule-bundle secondPartyAudit\.reportDigest must be a 64-character SHA-256 hex digest/u,
  (candidate) => {
    candidate.requiredWatcherPackage.ruleBundle.reviewRecord.secondPartyAudit =
      {
        auditor: "example-external-reviewer",
        rev: headRevision,
        reportDigest: "deadbeef",
      };
  },
);
mustReject(
  "a second-party audit at a revision that is not an ancestor of HEAD",
  /W23 rule-bundle secondPartyAudit\.rev .* is not an ancestor of HEAD/u,
  (candidate) => {
    candidate.requiredWatcherPackage.ruleBundle.reviewRecord.secondPartyAudit =
      {
        auditor: "example-external-reviewer",
        rev: nonAncestorRevision,
        reportDigest: "a".repeat(64),
      };
  },
);
mustReject(
  "a second-party audit with no named auditor",
  /W23 rule-bundle secondPartyAudit\.auditor must be a non-empty string/u,
  (candidate) => {
    candidate.requiredWatcherPackage.ruleBundle.reviewRecord.secondPartyAudit =
      {
        auditor: "",
        rev: headRevision,
        reportDigest: "a".repeat(64),
      };
  },
);

// The retired unfalsifiable field cannot come back, on any row.
mustReject(
  "the retired independentAudit field reappears on a row that carries a review record",
  /retired independentAudit field reappeared at \$\.requiredWatcherPackage\.rollbackEngine\.independentAudit/u,
  (candidate) => {
    candidate.requiredWatcherPackage.rollbackEngine.independentAudit =
      "PASS_all_original_and_residual_hostile_probes";
  },
);
mustReject(
  "the retired independentAudit field appears on W25, which never carried it",
  /retired independentAudit field reappeared at \$\.requiredWatcherPackage\.blockReplay\.independentAudit/u,
  (candidate) => {
    candidate.requiredWatcherPackage.blockReplay.independentAudit =
      "PASS_block_replay";
  },
);

// Closing positive control: the same harness still accepts the published map,
// so the rejections above cannot be a gate that rejects everything.
const closingControl = runGate(clone(publishedMap));
assert.equal(
  closingControl.status,
  0,
  `published map must still pass: ${closingControl.stderr}`,
);
acceptedControls += 1;

rmSync(workspace, { recursive: true, force: true });
process.stdout.write(
  `watcher-dependency-map:self-test: PASS\ncontrol runs accepted: ${acceptedControls}; hostile mutations rejected: ${rejectedMutations}\n`,
);
