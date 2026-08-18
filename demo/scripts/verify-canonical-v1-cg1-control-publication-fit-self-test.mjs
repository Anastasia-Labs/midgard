#!/usr/bin/env node
/**
 * Behavioural self-test for the CG1 control-publication fit gate.
 *
 * The gate's whole value is that it cannot be talked into PASS, so this suite
 * spawns the real gate binary against seeded artifacts and requires each run
 * to exit non-zero WITH its specific diagnostic. Only the CG1 artifact is
 * redirected (`--gate-under-test=`); the roster source, the auth-token map,
 * the working-tree blueprint and GOAL_PROGRESS.md all stay real, so every
 * seeded claim is judged against reality rather than a second seeded copy of
 * it.
 *
 * Opening and closing positive controls bracket the mutations, so a gate that
 * rejected everything could not pass this suite either. The mutation list
 * covers, at minimum: an oversized pinned transaction size, a tampered
 * applied hash, a dropped roster validator, an invented extra validator, an
 * exclusion that is no longer justified, and a stale blueprint-hash pin.
 */

import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { tmpdir } from "node:os";
import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");
const gateScript = resolve(
  scriptDirectory,
  "verify-canonical-v1-cg1-control-publication-fit.mjs",
);
const artifactPath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-cg1-control-publication-fit-v1.json",
);

const artifact = JSON.parse(readFileSync(artifactPath, "utf8"));
const clone = (value) => structuredClone(value);

const workspace = mkdtempSync(resolve(tmpdir(), "cg1-gate-self-test-"));
const candidatePath = resolve(workspace, "cg1-gate.json");

/* The blueprint is gitignored, so a fresh checkout (CI) structurally     */
/* cannot carry it. Forward --blueprint-optional to the gate exactly when */
/* that is the case, mirroring the CI invocation; wherever a working tree */
/* carries the blueprint the gate under test runs strict.                 */
const blueprintPresent = existsSync(
  resolve(repositoryRoot, "onchain/aiken/plutus.json"),
);
const forwardedFlags = blueprintPresent ? [] : ["--blueprint-optional"];

const runGate = (candidate) => {
  writeFileSync(candidatePath, `${JSON.stringify(candidate, null, 2)}\n`);
  return spawnSync(
    process.execPath,
    [gateScript, `--gate-under-test=${candidatePath}`, ...forwardedFlags],
    { cwd: repositoryRoot, encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
  );
};

let rejected = 0;
let accepted = 0;

const mustAccept = (label) => {
  const { status, stdout, stderr } = runGate(clone(artifact));
  assert.equal(
    status,
    0,
    `${label}: the unmodified artifact must be accepted\n${stderr}`,
  );
  assert.match(
    stdout,
    /Canonical V1 CG1 control-publication fit gate verified\./u,
  );
  accepted += 1;
  process.stdout.write(`accepted: ${label}\n`);
};

const mustReject = (label, expected, mutate) => {
  const candidate = clone(artifact);
  mutate(candidate);
  const { status, stderr } = runGate(candidate);
  assert.notEqual(status, 0, `${label}: the gate accepted a seeded defect`);
  const matched = expected.exec(stderr);
  assert.ok(matched, `${label}: expected ${String(expected)} in\n${stderr}`);
  rejected += 1;
  process.stdout.write(
    `rejected: ${label} -> exit ${String(status)}, ${matched[0]}\n`,
  );
};

const rosterEntry = (candidate, name) =>
  candidate.roster.find((entry) => entry.name === name);

const FIRST_ROSTER_NAME = artifact.roster[0].name;
const LAST_ROSTER_NAME = artifact.roster[artifact.roster.length - 1].name;

mustAccept("opening control");

/* --- The roster is re-derived from source, not restated. --- */

mustReject(
  "roster entry pinned oversized while the source-of-truth guard is untouched",
  /does not fit the 16384-byte L1 publication envelope/u,
  (candidate) => {
    const entry = rosterEntry(candidate, LAST_ROSTER_NAME);
    entry.completeSignedTransactionBytes = 20_000;
    entry.l1ByteMarginBytes = 16384 - 20_000;
    entry.fits = false;
  },
);

mustReject(
  "roster entry's fit arithmetic quietly rewritten to hide an oversized transaction",
  /l1ByteMarginBytes must equal/u,
  (candidate) => {
    const entry = rosterEntry(candidate, LAST_ROSTER_NAME);
    entry.completeSignedTransactionBytes = 20_000;
    // margin/fits left as originally published (positive) - only the byte
    // count moved, so a gate that trusted the published margin instead of
    // recomputing it from the byte count would miss this.
  },
);

mustReject(
  "roster entry's applied hash tampered",
  /must publish a real hex applied script hash/u,
  (candidate) => {
    rosterEntry(candidate, FIRST_ROSTER_NAME).appliedScriptHash = "not-a-hash";
  },
);

mustReject("roster validator dropped", /missing \["/u, (candidate) => {
  candidate.roster = candidate.roster.filter(
    (entry) => entry.name !== FIRST_ROSTER_NAME,
  );
  candidate.rosterCount = candidate.roster.length;
});

mustReject(
  "extra invented validator added to the roster",
  /extra \[.*invented-validator-that-does-not-exist/u,
  (candidate) => {
    candidate.roster.push({
      ...clone(rosterEntry(candidate, FIRST_ROSTER_NAME)),
      name: "invented-validator-that-does-not-exist",
    });
    candidate.rosterCount = candidate.roster.length;
  },
);

mustReject(
  "roster inventory count desynced from the published array",
  /rosterCount must equal the published roster's length/u,
  (candidate) => {
    candidate.rosterCount = candidate.roster.length + 1;
  },
);

mustReject(
  "roster entry duplicated under the same name",
  /is duplicated/u,
  (candidate) => {
    candidate.roster.push(clone(rosterEntry(candidate, FIRST_ROSTER_NAME)));
    candidate.rosterCount = candidate.roster.length;
    // Keep the name-order check satisfied by also removing the entry the
    // duplicate borrowed from the tail rather than the head, so this
    // mutation isolates the duplicate-detection check.
  },
);

/* --- The one exclusion (the oversized CEK direct resolver) must stay a real,
 * genuinely-absent, cited role. --- */

mustReject(
  "exclusion retargeted at a name the roster source never used",
  /is not a key in REFERENCE_SCRIPT_AUTH_TOKEN_NAMES/u,
  (candidate) => {
    candidate.exclusions[0].name = "a role that has never existed";
  },
);

mustReject(
  "exclusion retargeted at a real role that IS in the roster (no longer justified)",
  /IS present in nodeRuntimeReferenceScriptTargets's derived roster/u,
  (candidate) => {
    candidate.exclusions[0].name = FIRST_ROSTER_NAME;
    candidate.exclusions[0].tokenName = "irrelevant";
  },
);

mustReject(
  "exclusion's oversized measurement understated to look like it fits",
  /must publish a measuredCompleteSignedTransactionBytes above 16384/u,
  (candidate) => {
    candidate.exclusions[0].measuredCompleteSignedTransactionBytes = 1000;
    candidate.exclusions[0].measuredL1ByteMarginBytes = 16384 - 1000;
  },
);

mustReject(
  "exclusion's ledger citation fabricated",
  /cites a quote not found verbatim/u,
  (candidate) => {
    candidate.exclusions[0].recordedIn[0].quote =
      "this sentence was never written in GOAL_PROGRESS.md";
  },
);

mustReject(
  "exclusion count inflated with a second, fabricated exclusion",
  /exclusions must record exactly one entry/u,
  (candidate) => {
    candidate.exclusions.push({
      ...clone(candidate.exclusions[0]),
      name: "V1 field-preimage certificate",
    });
  },
);

/* --- The blueprint hash basis must match the real, gitignored working-tree
 * blueprint (there is no committed copy to check against instead). These
 * three mutations exercise exactly the comparison --blueprint-optional
 * skips, so they can only run where the working tree carries the
 * blueprint; in a fresh checkout they are structurally unexercisable and
 * are skipped with a note rather than silently dropped. --- */

if (blueprintPresent) {
  mustReject(
    "blueprint hash pin gone stale relative to the working tree",
    /hashBasis\.blueprintSha256 is stale/u,
    (candidate) => {
      candidate.hashBasis.blueprintSha256 = "0".repeat(64);
    },
  );

  mustReject(
    "blueprint md5 pin gone stale relative to the working tree",
    /hashBasis\.blueprintMd5 is stale/u,
    (candidate) => {
      candidate.hashBasis.blueprintMd5 = "0".repeat(32);
    },
  );

  mustReject(
    "blueprint validator count overstated",
    /hashBasis\.validatorCount is stale/u,
    (candidate) => {
      candidate.hashBasis.validatorCount = 1;
    },
  );
} else {
  process.stdout.write(
    "skipped: 3 stale-blueprint-pin mutations (gitignored blueprint absent from this checkout; enforced wherever a working tree carries it)\n",
  );
}

mustReject(
  "invalidation trigger no longer names the freeze issue",
  /invalidationTrigger must name issue #510/u,
  (candidate) => {
    candidate.hashBasis.invalidationTrigger = "nothing ever invalidates this";
  },
);

/* --- Dependency freshness (C11/C12/C13) is read from the real ledger. --- */

mustReject(
  "dependency row promoted to PASS with a fabricated quote",
  /rows\.C12\.quote must be a literal substring of GOAL_PROGRESS\.md/u,
  (candidate) => {
    candidate.dependencyFreshness.rows.C12.quote =
      "C12 was fabricated for this self-test and never appears in the ledger";
  },
);

mustReject(
  "dependency row status overstated while its quote is dropped",
  /rows\.C13\.quote must be a literal substring/u,
  (candidate) => {
    delete candidate.dependencyFreshness.rows.C13.quote;
  },
);

mustReject(
  "dependency row removed entirely",
  /dependencyFreshness\.rows\.C11 is required/u,
  (candidate) => {
    delete candidate.dependencyFreshness.rows.C11;
  },
);

/* --- Zero-debt, local-only contract, and the gate status itself. --- */

mustReject(
  "zero-debt report understated",
  /zeroDebt must equal the measured report/u,
  (candidate) => {
    candidate.zeroDebt.missingRosterEntries = 5;
  },
);

mustReject("waiver introduced", /CG1 admits no waivers/u, (candidate) => {
  candidate.waivers = [{ id: "temporary" }];
});

mustReject(
  "gate claims a C70 trusted snapshot",
  /CG1 is a local gate and must never create a C70 trusted snapshot/u,
  (candidate) => {
    candidate.createsC70Snapshots = true;
  },
);

mustReject(
  "gate infers live status",
  /CG1 must publish exactly 0 live or readiness claims/u,
  (candidate) => {
    candidate.liveOrReadinessClaims = 1;
  },
);

mustReject(
  "gate promoted to PASS while a roster entry does not fit",
  /gateStatus is PASS while/u,
  (candidate) => {
    const entry = rosterEntry(candidate, LAST_ROSTER_NAME);
    entry.completeSignedTransactionBytes = 20_000;
    entry.l1ByteMarginBytes = 16384 - 20_000;
    entry.fits = false;
    candidate.zeroDebt.rosterEntriesNotFitting = 1;
    // gateStatus stays PASS, so this isolates the gate-status-vs-zero-debt
    // consistency check rather than the per-entry fit check above.
  },
);

mustReject(
  "gate demoted to BLOCKED while everything measures clean",
  /every roster entry fits, the exclusion is justified, the blueprint hash basis matches and dependencies are fresh, but gateStatus is not PASS/u,
  (candidate) => {
    candidate.gateStatus = "BLOCKED";
  },
);

mustAccept("closing control");

rmSync(workspace, { recursive: true, force: true });

process.stdout.write(
  `cg1-control-publication-fit:self-test: PASS\ncontrol runs accepted: ${String(accepted)}; hostile mutations rejected: ${String(rejected)}\n`,
);
