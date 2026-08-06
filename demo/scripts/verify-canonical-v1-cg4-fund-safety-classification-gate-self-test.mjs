#!/usr/bin/env node
/**
 * Behavioural self-test for the CG4 fund-safety/classification gate.
 *
 * The gate's whole value is that it cannot be talked into PASS, so this suite
 * spawns the real gate binary against seeded artifacts and requires each run to
 * exit non-zero WITH its specific diagnostic. Only the CG4 artifact is
 * redirected: the F05 task manifest, the F20 reconciliation, the ABI freeze and
 * the git index all stay real, so a seeded claim is judged against reality.
 *
 * Opening and closing positive controls bracket the mutations, so a gate that
 * rejected everything could not pass this suite either.
 */

import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { tmpdir } from "node:os";
import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");
const gateScript = resolve(
  scriptDirectory,
  "verify-canonical-v1-cg4-fund-safety-classification-gate.mjs",
);
const artifactPath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-cg4-fund-safety-classification-gate-v1.json",
);

const artifact = JSON.parse(readFileSync(artifactPath, "utf8"));
const clone = (value) => structuredClone(value);

const workspace = mkdtempSync(resolve(tmpdir(), "cg4-gate-self-test-"));
const candidatePath = resolve(workspace, "cg4-gate.json");

const runGate = (candidate) => {
  writeFileSync(candidatePath, `${JSON.stringify(candidate, null, 2)}\n`);
  return spawnSync(
    process.execPath,
    [gateScript, `--gate-under-test=${candidatePath}`],
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
    /Canonical V1 CG4 fund-safety and classification gate verified\./u,
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

const classificationRow = (candidate, id) =>
  candidate.classificationRows.find((row) => row.id === id);

mustAccept("opening control");

/* --- The nine rows are reconciled against the manifest, not restated. --- */

mustReject(
  "row restates its own acceptance",
  /- C60 acceptance does not match the manifest row/u,
  (candidate) => {
    classificationRow(candidate, "C60").acceptance = "Anything goes.";
  },
);

mustReject(
  "row understates its manifest dependencies",
  /- C64 publishes blockedOn \[\] but the manifest declares \["C60","C61","C62","C63"\]/u,
  (candidate) => {
    classificationRow(candidate, "C64").blockedOn = [];
  },
);

mustReject(
  "row hides a prescribed surface it does not have",
  /- C66 publishes 0 missing surfaces but the index measures \d+/u,
  (candidate) => {
    classificationRow(candidate, "C66").missingSurfaces = [];
  },
);

mustReject(
  "row invents a prescribed surface",
  /- C60 prescribedSurfaces must equal the manifest row's writablePaths/u,
  (candidate) => {
    classificationRow(candidate, "C60").prescribedSurfaces = [
      "demo/does-not-matter.ts",
    ];
  },
);

mustReject(
  "row promoted while its prescribed surfaces are absent",
  /- C68 is PASS while \d+ prescribed surfaces are absent from the index/u,
  (candidate) => {
    classificationRow(candidate, "C68").status = "PASS";
  },
);

mustReject(
  "row promoted while the manifest still blocks it",
  /- C67 is PASS while the manifest still blocks it on C60, C61, C62, C63, C64, C65, C66: fund safety may never be inferred/u,
  (candidate) => {
    const row = classificationRow(candidate, "C67");
    row.status = "PASS";
    row.missingSurfaces = [];
  },
);

mustReject(
  "row inventory truncated",
  /- classificationRows must enumerate exactly the 9 C60-C68 rows/u,
  (candidate) => {
    candidate.classificationRows.pop();
  },
);

/* --- QG1 may not be promoted while it is unmeasured. --- */

mustReject(
  "QG1 promoted while its verifier does not exist",
  /- qg1Route is PASS while its verifier is absent and the reconciliation carries 49 open rows/u,
  (candidate) => {
    candidate.qg1Route.status = "PASS";
  },
);

mustReject(
  "QG1 verifier presence overstated",
  /- qg1Route\.verifierPresent is true but the index measures false/u,
  (candidate) => {
    candidate.qg1Route.verifierPresent = true;
  },
);

mustReject(
  "QG1 reconciliation numbers rewritten",
  /- qg1Route\.reconciliation must equal the F20 summary/u,
  (candidate) => {
    candidate.qg1Route.reconciliation.open = 0;
  },
);

mustReject(
  "QG1 route omitted entirely",
  /- qg1Route is required: CG4 depends on QG1 and may not omit it/u,
  (candidate) => {
    delete candidate.qg1Route;
  },
);

/* --- The consolidated ABI half is bound by digest. --- */

mustReject(
  "ABI freeze binding tampered",
  /- abiFreezeBinding is stale: the index measures/u,
  (candidate) => {
    candidate.abiFreezeBinding.artifactSha256 = "0".repeat(64);
  },
);

mustReject(
  "ABI freeze status overstated",
  /- abiFreezeBinding is stale: the index measures/u,
  (candidate) => {
    candidate.abiFreezeBinding.freezeStatus = "PASS";
  },
);

/* --- Zero-debt, local-only contract and the gate status itself. --- */

mustReject(
  "zero-debt report understated",
  /- zeroDebt must equal the measured report/u,
  (candidate) => {
    candidate.zeroDebt.missingFundSafetyRows = 0;
  },
);

mustReject("waiver introduced", /- CG4 admits no waivers/u, (candidate) => {
  candidate.waivers = [{ id: "temporary" }];
});

mustReject(
  "gate claims a C70 trusted snapshot",
  /- CG4 is a local gate and must never create a C70 trusted snapshot/u,
  (candidate) => {
    candidate.createsC70Snapshots = true;
  },
);

mustReject(
  "gate infers live status",
  /- CG4 must not cite LIVE_PASS: it infers deployment or live status/u,
  (candidate) => {
    classificationRow(candidate, "C60").evidence =
      "LIVE_PASS on the target net";
  },
);

mustReject(
  "gate promoted while blocked",
  /- gateStatus is PASS while a C60-C68 row, QG1 or the ABI freeze is not measured PASS/u,
  (candidate) => {
    candidate.gateStatus = "PASS";
  },
);

mustAccept("closing control");

rmSync(workspace, { recursive: true, force: true });

process.stdout.write(
  `cg4-fund-safety-classification-gate:self-test: PASS\ncontrol runs accepted: ${String(accepted)}; hostile mutations rejected: ${String(rejected)}\n`,
);
