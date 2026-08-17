// Measured-diff evidence checker (owner ruling 2026-08-17: a real measured
// check with an `--update` mode; a sweep-only scan was rejected).
//
// GOAL_PROGRESS.md is an append-only ledger. Its verdict rows pin file
// identities ("PASS ... at formatted registry SHA-256 `17561251...a8e3`"),
// and nothing re-reads those pins after they land, so a row can keep
// asserting PASS against bytes that no longer exist anywhere in the tree.
// Stale pins propagating between sessions is the recurring failure this
// gate closes.
//
// The artifact registers each tracked pin. This verifier re-measures every
// subject from the working tree and from git history, and fails closed on
// any UNACKNOWLEDGED difference:
//   - the registered quote must appear verbatim in the ledger row it names,
//   - the registered pin must be the pin the quote itself carries, so the
//     artifact cannot quietly re-pin while the ledger still shows the old
//     value,
//   - the subject must be byte-identical to HEAD before it is measured; a
//     measurement of dirty bytes is not reproducible,
//   - the commit era where the pin held is re-derived from git every run,
//     never trusted from the artifact,
//   - a pin that no longer matches must be acknowledged: measured.status
//     SUPERSEDED, the actual measured value, and exactly the commits that
//     moved the subject since the pin last held,
//   - a pin that never matched ANY committed state of the subject (F02-R is
//     one: its hash was measured in a session worktree whose formatted bytes
//     were never committed) must carry measured.status NEVER_HELD plus a
//     human-authored acknowledgement on the claim. --update derives the
//     status but refuses to invent the acknowledgement, so a mechanical
//     re-measurement alone can never bless an unreproducible pin,
//   - a pin that still matches must say so: measured.status CURRENT with an
//     empty mover list.
//
// `--update` rewrites only the measured blocks from a fresh measurement and
// re-derives the mover lists, then formats the artifact under the workspace
// prettier. It never touches pinned/quote: those mirror the ledger row, so
// accepting a NEW pin requires the ledger row itself to change. This gate
// cannot be satisfied by editing the artifact alone.

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const fail = (message) => {
  throw new Error(`Measured-diff evidence verification failed: ${message}`);
};

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const demoDirectory = resolve(scriptDirectory, "..");
const repositoryRoot = execFileSync("git", ["rev-parse", "--show-toplevel"], {
  cwd: demoDirectory,
  encoding: "utf8",
}).trim();
// git pathspecs (`log -- <path>`) resolve against the cwd, and every path in
// the artifact is repository-root-relative, so git always runs from the root.
const execGit = (args, encoding = "buffer") => {
  const output = execFileSync("git", args, {
    cwd: repositoryRoot,
    stdio: ["ignore", "pipe", "pipe"],
    maxBuffer: 256 * 1024 * 1024,
  });
  return encoding === "buffer"
    ? output
    : Buffer.from(output).toString(encoding);
};
const sha256Hex = (bytes) => createHash("sha256").update(bytes).digest("hex");
const isFullCommitSha = (value) =>
  typeof value === "string" && /^[0-9a-f]{40}$/u.test(value);
const hasText = (value) => typeof value === "string" && value.length > 0;
const isRepositoryRelativePath = (value) =>
  hasText(value) && !value.startsWith("/") && !value.split("/").includes("..");

const updateMode = process.argv.slice(2).includes("--update");
const argumentValue = (name) => {
  const found = process.argv
    .slice(2)
    .find((argument) => argument.startsWith(`${name}=`));
  return found === undefined ? undefined : found.slice(name.length + 1);
};
// `--artifact-under-test=` and `--document-root=` exist for the behavioural
// self-test only: they redirect which artifact is judged and where ledger
// documents are read from, while the SUBJECT files and git history stay the
// real ones, so a seeded mutation is judged against reality rather than
// against a fixture of its own choosing.
const artifactUnderTestArgument = argumentValue("--artifact-under-test");
const artifactPath =
  artifactUnderTestArgument === undefined
    ? resolve(
        repositoryRoot,
        "docs/exec-plans/evidence/canonical-v1-evidence-measured-diff-v1.json",
      )
    : resolve(artifactUnderTestArgument);
const documentRoot = argumentValue("--document-root") ?? repositoryRoot;
const artifact = JSON.parse(readFileSync(artifactPath, "utf8"));

if (artifact.schema !== "midgard.canonical-v1-evidence-measured-diff.v1") {
  fail("artifact schema identifier is not the expected one");
}
if (artifact.version !== 1) fail("artifact version must be 1");
if (!Array.isArray(artifact.claims) || artifact.claims.length === 0) {
  fail("artifact must register at least one claim");
}
const claimIds = artifact.claims.map((claim) => claim?.id);
if (new Set(claimIds).size !== claimIds.length) {
  fail("claim ids must be unique");
}
// Parent-owned coverage floor: removing a registered claim from the artifact
// must fail here, not silently narrow the gate. Registering a NEW claim is
// an artifact edit plus an entry in this list.
const requiredClaimIds = ["F02-R-registry-sha256"];
for (const requiredId of requiredClaimIds) {
  if (!claimIds.includes(requiredId)) {
    fail(`required claim ${requiredId} is not registered`);
  }
}

const headCommit = execGit(["rev-parse", "HEAD"], "utf8").trim();
const blobHashCache = new Map();
const subjectHashAtCommit = (commit, subjectPath) => {
  const blobId = execGit(
    ["rev-parse", `${commit}:${subjectPath}`],
    "utf8",
  ).trim();
  const cached = blobHashCache.get(blobId);
  if (cached !== undefined) return cached;
  const hash = sha256Hex(execGit(["show", `${commit}:${subjectPath}`]));
  blobHashCache.set(blobId, hash);
  return hash;
};

let currentClaims = 0;
let supersededClaims = 0;
let neverHeldClaims = 0;
for (const claim of artifact.claims) {
  const where = `claim ${String(claim?.id)}`;
  if (!hasText(claim?.id)) fail("every claim must carry a non-empty id");
  const recordedIn = claim.recordedIn;
  if (
    !isRepositoryRelativePath(recordedIn?.path) ||
    !hasText(recordedIn?.rowId) ||
    !hasText(recordedIn?.quote)
  ) {
    fail(`${where} recordedIn must carry path, rowId, and quote`);
  }
  const subject = claim.subject;
  if (subject?.kind !== "file-sha256") {
    fail(`${where} subject kind must be file-sha256`);
  }
  if (!isRepositoryRelativePath(subject.path)) {
    fail(`${where} subject path must be repository-relative`);
  }
  const pinned = claim.pinned;
  if (
    !/^[0-9a-f]{6,64}$/u.test(String(pinned?.prefix)) ||
    !/^[0-9a-f]{4,64}$/u.test(String(pinned?.suffix))
  ) {
    fail(`${where} pinned prefix/suffix must be lowercase hex`);
  }

  // The ledger row really carries this quote, and the quote really carries
  // this pin. Both bindings are re-read from the document every run.
  const documentLines = readFileSync(
    resolve(documentRoot, recordedIn.path),
    "utf8",
  ).split("\n");
  const rowPattern = new RegExp(
    `^\\|\\s*${recordedIn.rowId.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&")}\\s*\\|`,
    "u",
  );
  const rows = documentLines.filter((line) => rowPattern.test(line));
  if (rows.length !== 1) {
    fail(
      `${where} rowId ${recordedIn.rowId} matches ${String(rows.length)} rows of ${recordedIn.path}; exactly one is required`,
    );
  }
  if (!rows[0].includes(recordedIn.quote)) {
    fail(`${where} quote does not appear in ledger row ${recordedIn.rowId}`);
  }
  const quotedPin = recordedIn.quote.match(
    /`([0-9a-f]{6,64})\.\.\.([0-9a-f]{4,64})`/u,
  );
  if (quotedPin === null) {
    fail(`${where} quote carries no truncated hash pin to verify`);
  }
  if (quotedPin[1] !== pinned.prefix || quotedPin[2] !== pinned.suffix) {
    fail(
      `${where} registered pin ${String(pinned.prefix)}...${String(pinned.suffix)} is not the pin the quote carries (${quotedPin[1]}...${quotedPin[2]})`,
    );
  }
  const pinMatches = (hash) =>
    hash.startsWith(pinned.prefix) && hash.endsWith(pinned.suffix);

  // Measure the subject. The working tree must equal HEAD first: verifying
  // or updating against uncommitted bytes would record a measurement no
  // other checkout can reproduce.
  const workingTreeHash = sha256Hex(
    readFileSync(resolve(repositoryRoot, subject.path)),
  );
  const headHash = subjectHashAtCommit("HEAD", subject.path);
  if (workingTreeHash !== headHash) {
    fail(
      `${where} subject ${subject.path} differs from HEAD; commit or restore it before measuring`,
    );
  }

  // Re-derive from history where the pin last held. The artifact's own
  // mover list is never consulted for this - it is compared against it.
  const touchingCommits = execGit(
    ["log", "--format=%H", "HEAD", "--", subject.path],
    "utf8",
  )
    .split("\n")
    .filter((line) => line !== "");
  const lastHeldIndex = touchingCommits.findIndex((commit) =>
    pinMatches(subjectHashAtCommit(commit, subject.path)),
  );
  const derivedMovers =
    lastHeldIndex === -1 ? [] : touchingCommits.slice(0, lastHeldIndex);
  const derivedStatus = pinMatches(workingTreeHash)
    ? "CURRENT"
    : lastHeldIndex === -1
      ? "NEVER_HELD"
      : "SUPERSEDED";
  if (derivedStatus === "NEVER_HELD" && !hasText(claim.acknowledgement)) {
    fail(
      `${where} pin ${String(pinned.prefix)}...${String(pinned.suffix)} never matched any committed state of ${subject.path}; recording that requires a human-authored acknowledgement field on the claim, which --update will not invent`,
    );
  }
  if (derivedStatus === "CURRENT" && derivedMovers.length !== 0) {
    fail(
      `${where} matches the pin at the working tree but ${String(derivedMovers.length)} newer commits also touch the subject; the pin match must be the newest state`,
    );
  }

  if (updateMode) {
    claim.measured = {
      value: workingTreeHash,
      atCommit: headCommit,
      status: derivedStatus,
      movedBy: derivedMovers,
    };
  } else {
    const measured = claim.measured;
    if (measured?.value !== workingTreeHash) {
      fail(
        `${where} records measured value ${String(measured?.value)} but the subject measures ${workingTreeHash}; re-run with --update`,
      );
    }
    if (!isFullCommitSha(measured.atCommit)) {
      fail(`${where} measured.atCommit must be a full commit sha`);
    }
    try {
      execGit(["merge-base", "--is-ancestor", measured.atCommit, "HEAD"]);
    } catch {
      fail(`${where} measured.atCommit is not an ancestor of HEAD`);
    }
    if (measured.status !== derivedStatus) {
      fail(
        `${where} records status ${String(measured.status)} but measurement derives ${derivedStatus}; an unacknowledged difference between a ledger pin and the tree is exactly what this gate exists to catch`,
      );
    }
    if (
      !Array.isArray(measured.movedBy) ||
      JSON.stringify(measured.movedBy) !== JSON.stringify(derivedMovers)
    ) {
      fail(
        `${where} records movers [${(measured.movedBy ?? []).join(", ")}] but git derives [${derivedMovers.join(", ")}]`,
      );
    }
  }
  if (derivedStatus === "CURRENT") currentClaims += 1;
  else if (derivedStatus === "SUPERSEDED") supersededClaims += 1;
  else neverHeldClaims += 1;
}

if (updateMode) {
  writeFileSync(artifactPath, `${JSON.stringify(artifact, null, 2)}\n`);
  execFileSync("node_modules/.bin/prettier", ["--write", artifactPath], {
    cwd: demoDirectory,
    stdio: ["ignore", "ignore", "inherit"],
  });
}

process.stdout.write(
  `Canonical V1 measured-diff evidence ${updateMode ? "updated" : "verified"}: ${String(artifact.claims.length)} claims (${String(currentClaims)} CURRENT, ${String(supersededClaims)} SUPERSEDED-acknowledged, ${String(neverHeldClaims)} NEVER_HELD-acknowledged).\n`,
);
