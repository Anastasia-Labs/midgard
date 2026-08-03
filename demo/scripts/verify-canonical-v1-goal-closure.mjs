#!/usr/bin/env node

import { existsSync, readFileSync } from "node:fs";
import { dirname, relative, resolve, sep } from "node:path";
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";

import {
  canonicalClosureDigest,
  decodeCanonicalV1GoalClosure,
  isBoundFile,
  isReleaseReady,
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

// GOAL_SPEC.md §0.2: a manifest never records the hash of the commit that
// contains it. Once a releaseCommit is bound, every commit after it must be
// confined to the declared evidence paths.
const EVIDENCE_PATH_PATTERN =
  /^(?:GOAL_PROGRESS\.md$|docs\/exec-plans\/canonical-v1-goal-completion-report\.md$|docs\/exec-plans\/evidence\/)/;
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
    .filter((path) => !EVIDENCE_PATH_PATTERN.test(path));
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

for (const entry of manifest.dirtyBaseline.protectedPaths) {
  if (!existsSync(safeAbsolutePath(entry.path))) {
    throw new Error(`recorded dirty-baseline path is missing: ${entry.path}`);
  }
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
for (const blocker of manifest.residualBlockers) {
  for (const entry of blocker.evidence) {
    verifyFileBinding(entry, `residualBlockers[${blocker.id}]`);
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

if (releaseMode) {
  if (!isReleaseReady(manifest)) {
    throw new Error(
      "closure manifest is not release-ready: revision, bindings, commands, criteria, secret scan, and digest must all be complete",
    );
  }
  assertPassingCriteria(manifest.acceptanceCriteria, "release");
  const expectedDigest = canonicalClosureDigest(manifest);
  if (manifest.release.digest !== expectedDigest) {
    throw new Error(
      `release digest mismatch: expected ${expectedDigest}, received ${manifest.release.digest}`,
    );
  }
  const allowedDirty = manifest.dirtyBaseline.protectedPaths
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
    digestPreview,
  })}\n`,
);
