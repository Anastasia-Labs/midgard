// Behavioural self-test for the measured-diff evidence checker. Every check
// the gate claims to make is demonstrated to actually reject a seeded
// mutation; a gate that cannot fail is the hazard this repo keeps re-finding.
//
// The fixture claims are judged against the REAL repository: real subject
// files, real git history. Only the artifact under test and the ledger
// document root are redirected. The SUPERSEDED path is exercised end to end
// by pinning a fixture row to the oldest committed state of a real evidence
// file and letting the checker derive, from git alone, the exact commits
// that moved it since.

import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const demoDirectory = resolve(scriptDirectory, "..");
const checkerPath = join(
  scriptDirectory,
  "verify-canonical-v1-evidence-measured-diff.mjs",
);
const repositoryRoot = execFileSync("git", ["rev-parse", "--show-toplevel"], {
  cwd: demoDirectory,
  encoding: "utf8",
}).trim();
// git pathspecs resolve against the cwd; every fixture path is
// repository-root-relative, so git always runs from the root.
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
const clone = (value) => JSON.parse(JSON.stringify(value));

const realArtifactPath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-evidence-measured-diff-v1.json",
);
const realArtifact = JSON.parse(readFileSync(realArtifactPath, "utf8"));

const runChecker = (args) =>
  spawnSync(process.execPath, [checkerPath, ...args], {
    cwd: demoDirectory,
    encoding: "utf8",
  });

// Positive control 1: the published artifact must verify as committed.
const publishedControl = runChecker([]);
assert.equal(
  publishedControl.status,
  0,
  `published artifact must verify: ${publishedControl.stderr}`,
);

// Build the SUPERSEDED fixture: pin a real subject at its OLDEST committed
// state. The movers the checker must derive are then every newer commit
// that touched the subject - computed here independently, from git, so the
// test does not trust the checker for its own expectation.
const fixtureSubjectPath =
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json";
const touchingCommits = execGit(
  ["log", "--format=%H", "HEAD", "--", fixtureSubjectPath],
  "utf8",
)
  .split("\n")
  .filter((line) => line !== "");
assert.ok(
  touchingCommits.length >= 2,
  "fixture subject needs at least two touching commits",
);
const oldestCommit = touchingCommits[touchingCommits.length - 1];
const oldestHash = sha256Hex(
  execGit(["show", `${oldestCommit}:${fixtureSubjectPath}`]),
);
const expectedMovers = touchingCommits.slice(0, -1);
const fixturePinPrefix = oldestHash.slice(0, 8);
const fixturePinSuffix = oldestHash.slice(-4);
const fixtureQuote = `fixture map SHA-256 \`${fixturePinPrefix}...${fixturePinSuffix}\``;

const temporaryDirectory = mkdtempSync(
  join(tmpdir(), "midgard-evidence-measured-diff-self-test-"),
);
try {
  writeFileSync(
    join(temporaryDirectory, "fixture-ledger.md"),
    `# fixture ledger\n\n| Row | Evidence |\n| --- | --- |\n| DM-1 | PASS recorded at ${fixtureQuote}; fixture row for the self-test |\n`,
  );
  // The riding F02-R claim reads its ledger row through the redirected
  // document root too, so the real ledger is copied in unmodified.
  writeFileSync(
    join(temporaryDirectory, "GOAL_PROGRESS.md"),
    readFileSync(resolve(repositoryRoot, "GOAL_PROGRESS.md")),
  );
  const fixtureClaim = {
    id: "SELF-TEST-superseded-map-sha256",
    recordedIn: {
      path: "fixture-ledger.md",
      rowId: "DM-1",
      quote: fixtureQuote,
    },
    subject: { kind: "file-sha256", path: fixtureSubjectPath },
    pinned: { prefix: fixturePinPrefix, suffix: fixturePinSuffix },
    measured: {
      value: "UNMEASURED",
      atCommit: "UNMEASURED",
      status: "UNMEASURED",
      movedBy: [],
    },
  };
  // The real F02-R claim rides along so requiredClaimIds stays satisfied; it
  // is judged against the real ledger and real history exactly as published.
  const fixtureArtifact = {
    ...clone(realArtifact),
    claims: [clone(realArtifact.claims[0]), fixtureClaim],
  };
  const fixtureArtifactPath = join(temporaryDirectory, "artifact.json");
  const writeFixture = (artifact) =>
    writeFileSync(
      fixtureArtifactPath,
      `${JSON.stringify(artifact, null, 2)}\n`,
    );
  writeFixture(fixtureArtifact);
  const fixtureArguments = [
    `--artifact-under-test=${fixtureArtifactPath}`,
    `--document-root=${temporaryDirectory}`,
  ];

  // Positive control 2: --update must derive SUPERSEDED and the exact mover
  // list from git, then verify must accept its own update.
  const updateRun = runChecker([...fixtureArguments, "--update"]);
  assert.equal(
    updateRun.status,
    0,
    `--update must succeed: ${updateRun.stderr}`,
  );
  const updatedArtifact = JSON.parse(readFileSync(fixtureArtifactPath, "utf8"));
  const updatedClaim = updatedArtifact.claims.find(
    (claim) => claim.id === "SELF-TEST-superseded-map-sha256",
  );
  assert.equal(updatedClaim.measured.status, "SUPERSEDED");
  assert.deepEqual(updatedClaim.measured.movedBy, expectedMovers);
  const verifyRun = runChecker(fixtureArguments);
  assert.equal(
    verifyRun.status,
    0,
    `verify must accept the updated fixture: ${verifyRun.stderr}`,
  );
  assert.ok(verifyRun.stdout.includes("1 SUPERSEDED-acknowledged"));

  let rejectedMutations = 0;
  const mustReject = (label, expectedDiagnostic, mutate) => {
    const candidate = clone(updatedArtifact);
    mutate(candidate);
    writeFixture(candidate);
    const { status, stderr } = runChecker(fixtureArguments);
    assert.notEqual(status, 0, `${label}: checker exited 0 on a seeded defect`);
    assert.ok(
      stderr.includes(expectedDiagnostic),
      `${label}: expected diagnostic "${expectedDiagnostic}", got: ${stderr}`,
    );
    rejectedMutations += 1;
    process.stdout.write(`rejected: ${label}\n`);
  };
  const fixtureOf = (artifact) =>
    artifact.claims.find(
      (claim) => claim.id === "SELF-TEST-superseded-map-sha256",
    );
  const f02Of = (artifact) =>
    artifact.claims.find((claim) => claim.id === "F02-R-registry-sha256");

  mustReject(
    "superseded status flipped to CURRENT",
    "records status CURRENT but measurement derives SUPERSEDED",
    (artifact) => {
      fixtureOf(artifact).measured.status = "CURRENT";
    },
  );
  mustReject(
    "measured value fabricated",
    "but the subject measures",
    (artifact) => {
      fixtureOf(artifact).measured.value = "ab".repeat(32);
    },
  );
  mustReject("newest mover dropped", "but git derives", (artifact) => {
    fixtureOf(artifact).measured.movedBy.shift();
  });
  mustReject(
    "mover list padded with the held-at commit",
    "but git derives",
    (artifact) => {
      fixtureOf(artifact).measured.movedBy.push(oldestCommit);
    },
  );
  mustReject(
    "pin re-pinned onto the measured value while the ledger row keeps the old pin",
    "is not the pin the quote carries",
    (artifact) => {
      const claim = fixtureOf(artifact);
      claim.pinned.prefix = claim.measured.value.slice(0, 8);
      claim.pinned.suffix = claim.measured.value.slice(-4);
    },
  );
  mustReject(
    "quote asserts text the ledger row does not carry",
    "does not appear in ledger row",
    (artifact) => {
      const claim = fixtureOf(artifact);
      claim.recordedIn.quote = `${fixtureQuote} re-verified GREEN`;
      claim.pinned = { prefix: fixturePinPrefix, suffix: fixturePinSuffix };
    },
  );
  mustReject(
    "rowId retargeted at a row that does not exist",
    "matches 0 rows",
    (artifact) => {
      fixtureOf(artifact).recordedIn.rowId = "DM-9";
    },
  );
  mustReject(
    "subject retargeted at a file the pin never described",
    "requires a human-authored acknowledgement",
    (artifact) => {
      fixtureOf(artifact).subject.path =
        "docs/exec-plans/evidence/canonical-v1-abi-freeze-v1.json";
    },
  );
  mustReject(
    "measured.atCommit outside the current history",
    "is not an ancestor of HEAD",
    (artifact) => {
      fixtureOf(artifact).measured.atCommit = "f".repeat(40);
    },
  );
  mustReject(
    "NEVER_HELD acknowledgement stripped from the F02-R claim",
    "requires a human-authored acknowledgement",
    (artifact) => {
      delete f02Of(artifact).acknowledgement;
    },
  );
  mustReject(
    "required F02-R claim removed",
    "required claim F02-R-registry-sha256 is not registered",
    (artifact) => {
      artifact.claims = artifact.claims.filter(
        (claim) => claim.id !== "F02-R-registry-sha256",
      );
    },
  );
  mustReject("duplicate claim ids", "claim ids must be unique", (artifact) => {
    artifact.claims.push(clone(fixtureOf(artifact)));
  });

  process.stdout.write(
    `evidence-measured-diff:self-test: PASS\ncontrol runs accepted: 3; hostile mutations rejected: ${String(rejectedMutations)}\n`,
  );
} finally {
  rmSync(temporaryDirectory, { recursive: true, force: true });
}
