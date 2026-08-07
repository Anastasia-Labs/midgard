#!/usr/bin/env node

import { existsSync, readFileSync } from "node:fs";
import { dirname, relative, resolve, sep } from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";

import {
  DECLARED_EVIDENCE_PATHS,
  canonicalClosureDigest,
  decodeCanonicalV1GoalClosure,
  isBoundFile,
  parameterSnapshotDigest,
  releaseBlockers,
} from "./canonical-v1-goal-closure-v1.mjs";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const manifestPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-goal-closure-v1.json",
);
const args = new Set(process.argv.slice(2));
const releaseMode = args.has("--release");
const schemaOnly = args.has("--schema-only");
const groupArgument = [...args].find((argument) =>
  argument.startsWith("--group="),
);
const group = groupArgument?.slice("--group=".length) ?? null;
const validGroups = new Set([
  "repository",
  "capability",
  "fault-proofs",
  "watcher",
  "cross-system",
]);

if (releaseMode && (schemaOnly || group !== null)) {
  throw new Error("--release cannot be combined with --schema-only or --group");
}
if (group !== null && !validGroups.has(group)) {
  throw new Error(`unsupported criterion group: ${group}`);
}
if (
  [...args].some(
    (argument) =>
      argument !== "--release" &&
      argument !== "--schema-only" &&
      argument !== groupArgument,
  )
) {
  throw new Error("unsupported argument");
}

const manifest = decodeCanonicalV1GoalClosure(
  JSON.parse(readFileSync(manifestPath, "utf8")),
);

const runGit = (...gitArgs) =>
  execFileSync("git", gitArgs, {
    cwd: repoRoot,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  }).trim();

const currentHead = runGit("rev-parse", "HEAD");
const currentBranch = runGit("branch", "--show-current");
const isAncestorOrEqual = (ancestor, descendant) => {
  if (ancestor === descendant) {
    return true;
  }
  try {
    execFileSync("git", ["merge-base", "--is-ancestor", ancestor, descendant], {
      cwd: repoRoot,
      stdio: ["ignore", "ignore", "pipe"],
    });
    return true;
  } catch {
    return false;
  }
};
if (!isAncestorOrEqual(manifest.revision.headCommit, currentHead)) {
  throw new Error(
    `closure revision ${manifest.revision.headCommit} is not HEAD ${currentHead} or an ancestor of it`,
  );
}
if (manifest.revision.branch !== currentBranch) {
  throw new Error(
    `closure branch ${manifest.revision.branch} does not match ${currentBranch}`,
  );
}

// GOAL_SPEC §0.2. The membership test is built from the pinned
// DECLARED_EVIDENCE_PATHS, never from `manifest.evidencePaths`: the decoder has
// already proved the manifest declares exactly this set, and the gate that
// decides which paths an evidence commit may touch must not read its answer
// out of the artifact it is judging. A FILE class matches that exact path; a
// TREE class matches anything beneath the directory.
const isDeclaredEvidencePath = (path) =>
  DECLARED_EVIDENCE_PATHS.some((entry) =>
    entry.kind === "TREE" ? path.startsWith(entry.path) : path === entry.path,
  );

// GOAL_SPEC §0.2: "No artifact is ever required to record the hash of the
// commit that contains it." The commit that contains this manifest is the last
// commit that touched its path, resolved from Git rather than declared. A
// manifest naming that commit as its own releaseCommit would be claiming a
// hash it could not have known when it was written — the tell of a fabricated
// or back-dated binding. §0.2's structure already implies the manifest lands in
// an evidence commit strictly after releaseCommit, so this always has a
// satisfiable answer.
// The rule binds `releaseCommit`, not the generation-time `headCommit`: a
// manifest regenerated in the working tree legitimately records the HEAD it was
// generated against, and that HEAD may well be the commit that last touched the
// manifest. So the check applies only to the release binding, and only while
// the manifest is clean — a dirty manifest has no containing commit for its
// current content, which makes the question vacuous rather than violated.
const manifestRepoPath =
  "docs/exec-plans/evidence/canonical-v1-goal-closure-v1.json";
const manifestIsClean =
  runGit("status", "--porcelain=v1", "--", manifestRepoPath) === "";
const containingCommit =
  runGit("log", "-1", "--format=%H", "--", manifestRepoPath) || null;
if (
  manifest.revision.releaseCommit !== null &&
  manifestIsClean &&
  containingCommit !== null &&
  manifest.revision.releaseCommit === containingCommit
) {
  throw new Error(
    `revision.releaseCommit ${containingCommit} is the commit containing this manifest; GOAL_SPEC §0.2 forbids an artifact recording the hash of its own containing commit, so the closure manifest must land in an evidence commit after releaseCommit`,
  );
}

let escapingEvidencePaths = 0;
if (manifest.revision.releaseCommit !== null) {
  if (!isAncestorOrEqual(manifest.revision.releaseCommit, currentHead)) {
    throw new Error(
      `releaseCommit ${manifest.revision.releaseCommit} is not HEAD or an ancestor of it`,
    );
  }
  const escaped = execFileSync(
    "git",
    ["diff", "--name-only", `${manifest.revision.releaseCommit}..HEAD`],
    { cwd: repoRoot, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] },
  )
    .split("\n")
    .filter(Boolean)
    .filter((path) => !isDeclaredEvidencePath(path));
  escapingEvidencePaths = escaped.length;
  if (escaped.length > 0) {
    throw new Error(
      `releaseCommit..HEAD diff escapes declared evidence paths: ${escaped.join(", ")}`,
    );
  }
}

const safeAbsolutePath = (path) => {
  const absolute = resolve(repoRoot, path);
  const fromRoot = relative(repoRoot, absolute);
  if (
    fromRoot === "" ||
    fromRoot === ".." ||
    fromRoot.startsWith(`..${sep}`) ||
    fromRoot.startsWith(sep)
  ) {
    throw new Error(`evidence path escapes repository: ${path}`);
  }
  return absolute;
};

// GOAL_SPEC §13.4 (owner amendment 2026-08-01): repository-file evidence is
// bound by repo-relative path only. Git already content-addresses tracked
// files, so byte-hashing them here detected nothing that `git log -p` does
// not, while forcing a rebind on every legitimate edit. What still matters is
// that each bound path EXISTS and sits on a declared evidence path — verify
// that instead. External (off-repo) bindings keep their digests; those are
// added with the external-binding shape and are not repository files.
const verifyFileBinding = (binding, owner) => {
  if (!isBoundFile(binding)) {
    return;
  }
  if (!existsSync(safeAbsolutePath(binding.path))) {
    throw new Error(
      `${owner} binding references a missing file: ${binding.path}`,
    );
  }
};

// A path Git tracks is evidence: if the manifest records it as part of the
// dirty baseline and it is gone, something deleted recorded evidence and the
// gate must fail. A path recorded as PROTECTED_EXTERNAL_UNTRACKED was never in
// the index — it is local scratch that only ever existed in the owner's
// worktree — so a fresh clone (CI) legitimately does not have it. Absence of a
// never-tracked file is not evidence loss. The exemption is confirmed against
// Git, not taken on the manifest's word: if the recorded path IS tracked, the
// untracked disposition is a mislabel and the missing file still fails.
const isTrackedPath = (path) => {
  try {
    execFileSync("git", ["ls-files", "--error-unmatch", "--", path], {
      cwd: repoRoot,
      stdio: ["ignore", "ignore", "pipe"],
    });
    return true;
  } catch {
    return false;
  }
};
const isExemptAbsentUntrackedPath = (entry) =>
  entry.disposition === "PROTECTED_EXTERNAL_UNTRACKED" &&
  !existsSync(safeAbsolutePath(entry.path)) &&
  !isTrackedPath(entry.path);

for (const entry of manifest.dirtyBaseline.protectedPaths) {
  if (existsSync(safeAbsolutePath(entry.path))) {
    continue;
  }
  if (isExemptAbsentUntrackedPath(entry)) {
    continue;
  }
  throw new Error(`recorded dirty-baseline path is missing: ${entry.path}`);
}

verifyFileBinding(manifest.parameterSnapshot, "parameterSnapshot");
verifyFileBinding(
  manifest.targetTestnetParameterSnapshot,
  "targetTestnetParameterSnapshot",
);
verifyFileBinding(manifest.blueprint, "blueprint");
for (const entry of manifest.closureArtifacts) {
  verifyFileBinding(entry, "closureArtifacts");
}
for (const entry of manifest.validatorSet.evidence) {
  verifyFileBinding(entry, "validatorSet");
}
for (const entry of manifest.deployment.evidence) {
  verifyFileBinding(entry, "deployment");
}
for (const entry of manifest.fixtureSets) {
  verifyFileBinding(entry, "fixtureSets");
}
for (const criterion of manifest.acceptanceCriteria) {
  for (const entry of criterion.evidence) {
    verifyFileBinding(entry, criterion.id);
  }
}
for (const entry of manifest.secrets.evidence) {
  verifyFileBinding(entry, "secrets");
}
// GOAL_SPEC §9.5: a residual launch blocker is only a legitimate outcome when
// it is named here AND in root `public_testnet_readiness.md` with the owner's
// explicit acceptance. Verify the acceptance evidence actually exists.
const READINESS_DOCUMENT = "public_testnet_readiness.md";
const readinessText = manifest.residualBlockers.length
  ? (() => {
      const absolute = safeAbsolutePath(READINESS_DOCUMENT);
      if (!existsSync(absolute)) {
        throw new Error(
          `${manifest.residualBlockers.length} residual launch blocker(s) are recorded but root ${READINESS_DOCUMENT} is missing`,
        );
      }
      return readFileSync(absolute, "utf8");
    })()
  : null;
for (const blocker of manifest.residualBlockers) {
  for (const entry of blocker.evidence) {
    verifyFileBinding(entry, `residualBlockers[${blocker.id}]`);
  }
  // §9.5 admits a residual launch blocker only when it is named in BOTH the
  // closure manifest and root public_testnet_readiness.md — "never silence".
  // Recording it here while the public readiness document stays quiet is
  // precisely the silence the rule forbids, so read the document and look.
  if (!readinessText.includes(blocker.id)) {
    throw new Error(
      `residual launch blocker ${blocker.id} is not named in root ${READINESS_DOCUMENT}`,
    );
  }
}

// GOAL_SPEC §2.4/§13.4: a regeneration record exists because the artifact is
// too large or transient to commit. If its output IS tracked, the record is
// being used to describe evidence that should simply have been committed and
// referenced by path — so confirm non-tracking against Git rather than
// accepting the manifest's reason for it.
for (const record of manifest.regenerationRecords) {
  safeAbsolutePath(record.outputPath);
  if (isTrackedPath(record.outputPath)) {
    throw new Error(
      `regeneration record ${record.id} declares tracked outputPath ${record.outputPath}; commit and bind it by path instead`,
    );
  }
}

const commandIds = manifest.commandResults.map(({ id }) => id);
if (new Set(commandIds).size !== commandIds.length) {
  throw new Error("commandResults contains duplicate ids");
}

const criterionMatchesGroup = (id, requestedGroup) => {
  if (requestedGroup === "repository") {
    return /^AC-0[0-3]$/.test(id);
  }
  if (requestedGroup === "capability") {
    return id.startsWith("AC-C");
  }
  if (requestedGroup === "fault-proofs") {
    return id.startsWith("AC-Q");
  }
  if (requestedGroup === "watcher") {
    return id.startsWith("AC-W");
  }
  if (requestedGroup === "cross-system") {
    return id.startsWith("AC-X");
  }
  return false;
};

const assertPassingCriteria = (criteria, label) => {
  const open = criteria.filter(({ status }) => status !== "PASS");
  if (open.length > 0) {
    throw new Error(
      `${label} criteria remain open: ${open.map(({ id }) => id).join(", ")}`,
    );
  }
  for (const criterion of criteria) {
    if (!criterion.evidence.every(isBoundFile)) {
      throw new Error(`${criterion.id} contains unbound evidence`);
    }
  }
};

if (group !== null) {
  const criteria = manifest.acceptanceCriteria.filter(({ id }) =>
    criterionMatchesGroup(id, group),
  );
  if (criteria.length === 0) {
    throw new Error(`criterion group ${group} is empty`);
  }
  assertPassingCriteria(criteria, group);
}

const parseDirtyPaths = () => {
  const raw = execFileSync("git", ["status", "--porcelain=v1", "-z"], {
    cwd: repoRoot,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  });
  const chunks = raw.split("\0").filter(Boolean);
  const paths = [];
  for (let index = 0; index < chunks.length; index += 1) {
    const entry = chunks[index];
    const status = entry.slice(0, 2);
    paths.push(entry.slice(3));
    if (status.includes("R") || status.includes("C")) {
      index += 1;
      if (index >= chunks.length) {
        throw new Error("malformed rename/copy entry in git status");
      }
      paths.push(chunks[index]);
    }
  }
  return paths.sort();
};

const secretPatterns = [
  /-----BEGIN [A-Z ]*PRIVATE KEY-----/,
  /\b(?:api[_-]?key|mnemonic|seed(?:[_ -]?phrase)?|secret|token)\b\s*[:=]\s*["'][^"'\r\n]{8,}["']/i,
];

const verifyNoSecrets = () => {
  const paths = new Set();
  const addBinding = (binding) => {
    if (isBoundFile(binding)) {
      paths.add(binding.path);
    }
  };
  addBinding(manifest.parameterSnapshot);
  addBinding(manifest.targetTestnetParameterSnapshot);
  addBinding(manifest.blueprint);
  manifest.residualBlockers.forEach(({ evidence }) =>
    evidence.forEach(addBinding),
  );
  manifest.closureArtifacts.forEach(addBinding);
  manifest.validatorSet.evidence.forEach(addBinding);
  manifest.deployment.evidence.forEach(addBinding);
  manifest.fixtureSets.forEach(addBinding);
  manifest.acceptanceCriteria.forEach(({ evidence }) =>
    evidence.forEach(addBinding),
  );
  manifest.secrets.evidence.forEach(addBinding);

  for (const path of paths) {
    const content = readFileSync(safeAbsolutePath(path), "utf8");
    if (secretPatterns.some((pattern) => pattern.test(content))) {
      throw new Error(`possible secret material in committed evidence ${path}`);
    }
  }
};

// GOAL_SPEC §13.3/C70: a BOUND snapshot's recorded identity must be the
// identity of the snapshot actually committed. Recomputed here from the file's
// normalized content, so the verdict comes from reading the snapshot rather
// than from the digest the manifest supplies about itself. Confined to release
// mode: while C70's snapshots are OPEN the digests are null and the gate is
// already closed by releaseBlockers().
const verifySnapshotDigest = (snapshot, owner) => {
  if (snapshot.status !== "BOUND") {
    return;
  }
  const absolute = safeAbsolutePath(snapshot.path);
  if (!existsSync(absolute)) {
    throw new Error(`${owner} is BOUND but ${snapshot.path} is missing`);
  }
  let actual;
  try {
    actual = parameterSnapshotDigest(readFileSync(absolute, "utf8"));
  } catch (error) {
    throw new Error(
      `${owner} snapshot ${snapshot.path} is not decodable JSON: ${error.message}`,
    );
  }
  if (actual !== snapshot.snapshotDigest) {
    throw new Error(
      `${owner} snapshotDigest mismatch: recomputed ${actual}, recorded ${snapshot.snapshotDigest}`,
    );
  }
};

if (releaseMode) {
  const blockers = releaseBlockers(manifest);
  if (blockers.length > 0) {
    throw new Error(
      `closure manifest is not release-ready (${blockers.length} unmet condition(s)): ${blockers.join("; ")}`,
    );
  }
  assertPassingCriteria(manifest.acceptanceCriteria, "release");
  verifySnapshotDigest(manifest.parameterSnapshot, "parameterSnapshot");
  verifySnapshotDigest(
    manifest.targetTestnetParameterSnapshot,
    "targetTestnetParameterSnapshot",
  );
  const expectedDigest = canonicalClosureDigest(manifest);
  if (manifest.release.digest !== expectedDigest) {
    throw new Error(
      `release digest mismatch: expected ${expectedDigest}, received ${manifest.release.digest}`,
    );
  }
  // Same exemption as the existence check above: a never-tracked scratch path
  // that is absent from this checkout cannot appear in `git status`, so it is
  // not expected in the dirty set either. Every other recorded path must still
  // be dirty, and any unrecorded dirty path still fails.
  const allowedDirty = manifest.dirtyBaseline.protectedPaths
    .filter((entry) => !isExemptAbsentUntrackedPath(entry))
    .map(({ path }) => path)
    .sort();
  const actualDirty = parseDirtyPaths();
  if (
    actualDirty.length !== allowedDirty.length ||
    actualDirty.some((path, index) => path !== allowedDirty[index])
  ) {
    throw new Error(
      `worktree is not baseline-relative clean; expected [${allowedDirty.join(", ")}], received [${actualDirty.join(", ")}]`,
    );
  }
  verifyNoSecrets();
}

const digestPreview = canonicalClosureDigest(manifest);
const status = releaseMode
  ? "release-ready"
  : group === null
    ? schemaOnly
      ? "schema-valid"
      : "current-tree-valid"
    : `${group}-criteria-pass`;
process.stdout.write(
  `${JSON.stringify({
    schema: manifest.schema,
    status,
    headCommit: currentHead,
    criteria: manifest.acceptanceCriteria.length,
    passingCriteria: manifest.acceptanceCriteria.filter(
      ({ status }) => status === "PASS",
    ).length,
    protectedPaths: manifest.dirtyBaseline.protectedPaths.length,
    commandResults: manifest.commandResults.length,
    residualBlockers: manifest.residualBlockers.length,
    evidencePathClasses: manifest.evidencePaths.length,
    parameterSnapshots: new Set([
      manifest.parameterSnapshot.path,
      manifest.targetTestnetParameterSnapshot.path,
    ]).size,
    regenerationRecords: manifest.regenerationRecords.length,
    releaseCommitsBound: manifest.revision.releaseCommit === null ? 0 : 1,
    escapingEvidencePaths,
    digestPreview,
  })}\n`,
);
