#!/usr/bin/env node
/**
 * CG4 — P4 fund-safety and classification gate.
 *
 * GOAL_SPEC §8.4 CG4: "No missing or partial fund-safety/classification row for
 * canonical V1", and the B34 acceptance criterion that "C60-C68 and QG1 close
 * CG4 with one consolidated ABI/fund-safety review".
 *
 * Like the WG1/IG3 gate, this verifier does NOT decide that fund safety is
 * closed. It decides that the published CG4 artifact tells the truth about what
 * has and has not been measured. That inversion is the fail-closed reading of
 * the row's own "zero-debt" contract: a gate that could only be green by
 * claiming PASS would create pressure to claim PASS.
 *
 * Everything the artifact publishes about the nine classification rows is
 * checked against the git index:
 *
 *   - each row's missing surfaces are measured against the index, so a row can
 *     neither hide a surface it lacks nor invent one it already has;
 *   - no row may be PASS while a prescribed surface is missing or a declared
 *     dependency is unmet; and
 *   - QG1 is UNMEASURED — and therefore blocking — for as long as its own
 *     local-coverage verifier does not exist in the tree.
 *
 * The consolidated half binds the IG1/IG2 ABI freeze by digest, because CG4's
 * acceptance is a joint ABI/fund-safety review rather than two separate ones.
 *
 * This gate is local-only by construction. It refuses an artifact that creates
 * a C70 trusted snapshot or that carries any live, deployment or readiness
 * claim.
 */

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");

const GATE_PATH =
  "docs/exec-plans/evidence/canonical-v1-cg4-fund-safety-classification-gate-v1.json";
const RECONCILIATION_PATH =
  "docs/exec-plans/evidence/canonical-v1-fault-proof-reconciliation-v1.json";
const FREEZE_PATH = "docs/exec-plans/evidence/canonical-v1-abi-freeze-v1.json";
const FREEZE_VERIFIER_PATH = "demo/scripts/verify-canonical-v1-abi-freeze.mjs";
const QG1_VERIFIER_PATH =
  "demo/scripts/verify-canonical-v1-qg1-local-coverage-gate.mjs";

/** The nine P4 classification rows, in GOAL_SPEC §8.4 order. */
const CLASSIFICATION_ROW_IDS = [
  "C60",
  "C61",
  "C62",
  "C63",
  "C64",
  "C65",
  "C66",
  "C67",
  "C68",
];

const STATUSES = new Set(["PASS", "BLOCKED"]);

const flagValue = (name) => {
  const prefix = `--${name}=`;
  const found = process.argv.find((argument) => argument.startsWith(prefix));
  return found === undefined ? null : found.slice(prefix.length);
};
const gatePath = (() => {
  const explicit =
    flagValue("gate-under-test") ?? process.env.MIDGARD_CG4_GATE_PATH ?? null;
  return resolve(repositoryRoot, explicit ?? GATE_PATH);
})();

const errors = [];
const fail = (message) => {
  errors.push(message);
};

const execGit = (args, encoding = "utf8") =>
  execFileSync("git", args, {
    cwd: repositoryRoot,
    encoding,
    maxBuffer: 256 * 1024 * 1024,
    stdio: ["ignore", "pipe", "pipe"],
  });

const readIndexed = (path, encoding = "utf8") =>
  execGit(["show", `:${path}`], encoding);

const indexedPaths = new Set(
  execGit(["ls-files"])
    .split("\n")
    .filter((line) => line.length > 0),
);
const indexHas = (path) => indexedPaths.has(path);

const sha256Of = (path) =>
  createHash("sha256").update(readIndexed(path, "buffer")).digest("hex");

const sameJson = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

if (!existsSync(gatePath)) {
  process.stderr.write(`CG4 gate artifact does not exist: ${gatePath}\n`);
  process.exit(1);
}
const gate = JSON.parse(readFileSync(gatePath, "utf8"));

/* ------------------------------------------------------------------ */
/* Derivation.                                                         */
/* ------------------------------------------------------------------ */

/**
 * A prescribed surface is "missing" when the row names a file that the index
 * does not carry.
 */
const measuredRows = new Map(
  CLASSIFICATION_ROW_IDS.map((id) => {
    const row = gate.classificationRows?.find(
      (candidate) => candidate?.id === id,
    );
    const prescribed = row?.prescribedSurfaces ?? [];
    return [
      id,
      {
        id,
        title: row?.title ?? null,
        acceptance: row?.acceptance ?? null,
        blockedOn: row?.blockedOn ?? [],
        prescribedSurfaces: prescribed,
        missingSurfaces: prescribed.filter((path) => !indexHas(path)),
      },
    ];
  }),
);

for (const [id, row] of measuredRows) {
  if (row.title === null) {
    fail(`${id} is not present in the classification artifact`);
  }
  if (row.prescribedSurfaces.length === 0) {
    fail(`${id} prescribes no surfaces; the row is unusable as a source check`);
  }
}

const reconciliation = JSON.parse(readIndexed(RECONCILIATION_PATH));
const measuredQg1 = {
  verifierPath: QG1_VERIFIER_PATH,
  verifierPresent: indexHas(QG1_VERIFIER_PATH),
  acceptance: gate.qg1Route?.acceptance ?? null,
  blockedOn: gate.qg1Route?.blockedOn ?? [],
  reconciliation: {
    total: reconciliation.summary?.total ?? -1,
    locallyComplete: reconciliation.summary?.locallyComplete ?? -1,
    structuralOrNA: reconciliation.summary?.structuralOrNA ?? -1,
    open: reconciliation.summary?.open ?? -1,
    preprodComplete: reconciliation.summary?.preprodComplete ?? -1,
  },
  publishedAcceptance: reconciliation.acceptance?.QG1 ?? null,
};

/**
 * QG1 is the local-closure gate CG4 depends on. It may only be measured PASS
 * when its own verifier exists AND the reconciliation it reconciles carries no
 * open row. Absence of the verifier is UNMEASURED, which is blocking.
 */
const qg1MeasuresPass =
  measuredQg1.verifierPresent &&
  measuredQg1.reconciliation.open === 0 &&
  measuredQg1.publishedAcceptance === "PASS";

const freezePresent = indexHas(FREEZE_PATH);
const measuredFreeze = freezePresent
  ? (() => {
      const freeze = JSON.parse(readIndexed(FREEZE_PATH));
      return {
        artifactPath: FREEZE_PATH,
        artifactSha256: sha256Of(FREEZE_PATH),
        verifierPath: FREEZE_VERIFIER_PATH,
        verifierSha256: indexHas(FREEZE_VERIFIER_PATH)
          ? sha256Of(FREEZE_VERIFIER_PATH)
          : null,
        freezeStatus: freeze.freezeStatus ?? null,
        ig2Status: freeze.ig2ProofFamilies?.status ?? null,
        ig2Violations: freeze.ig2ProofFamilies?.violations ?? -1,
        driftFindings: (freeze.driftFindings ?? []).length,
      };
    })()
  : null;

/* ------------------------------------------------------------------ */
/* Artifact.                                                           */
/* ------------------------------------------------------------------ */

if (
  gate.schemaVersion !==
    "midgard-canonical-v1-cg4-fund-safety-classification-gate-v1" ||
  !Array.isArray(gate.goalIds) ||
  !sameJson(gate.goalIds, ["CG4"])
) {
  fail(
    "the gate artifact must be the CG4 P4 fund-safety/classification review",
  );
}
if (!Array.isArray(gate.waivers) || gate.waivers.length !== 0) {
  fail("CG4 admits no waivers; the waivers array must exist and be empty");
}

/* --- 1. The nine classification rows ------------------------------- */

let passingRows = 0;
if (
  !Array.isArray(gate.classificationRows) ||
  gate.classificationRows.length !== CLASSIFICATION_ROW_IDS.length
) {
  fail(
    `classificationRows must enumerate exactly the ${String(CLASSIFICATION_ROW_IDS.length)} C60-C68 rows`,
  );
} else {
  gate.classificationRows.forEach((published, index) => {
    const id = CLASSIFICATION_ROW_IDS[index];
    if (published?.id !== id) {
      fail(`classificationRows[${String(index)}] must be ${id}`);
      return;
    }
    const measured = measuredRows.get(id);
    if (!sameJson(published.prescribedSurfaces, measured.prescribedSurfaces)) {
      fail(`${id} prescribedSurfaces could not be measured`);
    }
    if (!sameJson(published.missingSurfaces, measured.missingSurfaces)) {
      fail(
        `${id} publishes ${String((published.missingSurfaces ?? []).length)} missing surfaces but the index measures ${String(measured.missingSurfaces.length)}`,
      );
    }
    if (!STATUSES.has(published.status)) {
      fail(`${id} status must be measured as PASS or BLOCKED`);
      return;
    }
    if (
      typeof published.classification !== "string" ||
      published.classification.length === 0
    ) {
      fail(`${id} must publish the classification direction it covers`);
    }
    if (
      typeof published.expectedTerminal !== "string" ||
      published.expectedTerminal.length === 0
    ) {
      fail(`${id} must publish the terminal its acceptance expects`);
    }
    if (
      typeof published.evidence !== "string" ||
      published.evidence.length === 0
    ) {
      fail(`${id} must cite the evidence its status was measured from`);
    }
    if (published.status === "PASS") {
      if (measured.missingSurfaces.length > 0) {
        fail(
          `${id} is PASS while ${String(measured.missingSurfaces.length)} prescribed surfaces are absent from the index`,
        );
      }
      if (measured.blockedOn.length > 0) {
        fail(
          `${id} is PASS while it is still blocked on ${measured.blockedOn.join(", ")}: fund safety may never be inferred`,
        );
      }
      passingRows += 1;
    }
  });
}

/* --- 2. QG1 local-family route ------------------------------------- */

const qg1 = gate.qg1Route;
if (qg1 === undefined || qg1 === null) {
  fail("qg1Route is required: CG4 depends on QG1 and may not omit it");
} else {
  if (qg1.rowId !== "QG1") fail("qg1Route.rowId must be QG1");
  if (qg1.verifierPath !== QG1_VERIFIER_PATH) {
    fail(`qg1Route.verifierPath must be ${QG1_VERIFIER_PATH}`);
  }
  if (qg1.verifierPresent !== measuredQg1.verifierPresent) {
    fail(
      `qg1Route.verifierPresent is ${String(qg1.verifierPresent)} but the index measures ${String(measuredQg1.verifierPresent)}`,
    );
  }
  if (!sameJson(qg1.reconciliation, measuredQg1.reconciliation)) {
    fail(
      `qg1Route.reconciliation must equal the F20 summary ${JSON.stringify(measuredQg1.reconciliation)}`,
    );
  }
  if (qg1.publishedAcceptance !== measuredQg1.publishedAcceptance) {
    fail(
      `qg1Route.publishedAcceptance must equal the reconciliation's QG1 disposition ${JSON.stringify(measuredQg1.publishedAcceptance)}`,
    );
  }
  if (!STATUSES.has(qg1.status)) {
    fail("qg1Route.status must be measured as PASS or BLOCKED");
  }
  if (qg1.status === "PASS" && !qg1MeasuresPass) {
    fail(
      `qg1Route is PASS while its verifier is ${measuredQg1.verifierPresent ? "present" : "absent"} and the reconciliation carries ${String(measuredQg1.reconciliation.open)} open rows`,
    );
  }
  if (qg1.status !== "PASS" && qg1MeasuresPass) {
    fail(
      "QG1 measures PASS but qg1Route.status is not PASS: the artifact is stale",
    );
  }
}

/* --- 3. Consolidated ABI freeze binding ---------------------------- */

const binding = gate.abiFreezeBinding;
if (binding === undefined || binding === null) {
  fail(
    "abiFreezeBinding is required: CG4's acceptance is one consolidated ABI/fund-safety review",
  );
} else if (measuredFreeze === null) {
  fail(
    `abiFreezeBinding names ${FREEZE_PATH}, which is not in the index; the consolidated review has no ABI half`,
  );
} else if (!sameJson(binding, measuredFreeze)) {
  fail(
    `abiFreezeBinding is stale: the index measures ${JSON.stringify(measuredFreeze)}`,
  );
}

/* --- 4. Zero-debt report ------------------------------------------- */

const measuredZeroDebt = {
  missingFundSafetyRows: [...measuredRows.values()].filter(
    ({ missingSurfaces }) => missingSurfaces.length > 0,
  ).length,
  partialFundSafetyRows: [...measuredRows.values()].filter(
    ({ missingSurfaces, prescribedSurfaces }) =>
      missingSurfaces.length > 0 &&
      missingSurfaces.length < prescribedSurfaces.length,
  ).length,
  dependencyBlockedRows: [...measuredRows.values()].filter(
    ({ blockedOn }) => blockedOn.length > 0,
  ).length,
  unknownClassificationRows: 0,
  staleSemanticsRows: 0,
  directFamilyReroutes: 0,
  unprovableAsVerifiedRows: 0,
  nonLocalPassRows: CLASSIFICATION_ROW_IDS.length - passingRows,
  liveClaims: 0,
  readinessClaims: 0,
};

if (!sameJson(gate.zeroDebt, measuredZeroDebt)) {
  fail(
    `zeroDebt must equal the measured report ${JSON.stringify(measuredZeroDebt)}`,
  );
}

/* --- 5. Local-only contract ---------------------------------------- */

if (gate.createsC70Snapshots !== false) {
  fail("CG4 is a local gate and must never create a C70 trusted snapshot");
}
if (gate.liveOrReadinessClaims !== 0) {
  fail("CG4 must publish exactly 0 live or readiness claims");
}
// A local gate that cites a testnet artifact has already left its scope.
const serialized = JSON.stringify(gate);
for (const forbidden of [
  "docs/exec-plans/evidence/testnet/",
  "LIVE_PASS",
  "preprod-complete",
]) {
  if (serialized.includes(forbidden)) {
    fail(`CG4 must not cite ${forbidden}: it infers deployment or live status`);
  }
}

/* --- 6. Gate status ------------------------------------------------ */

const everythingMeasuredPass =
  passingRows === CLASSIFICATION_ROW_IDS.length &&
  qg1MeasuresPass &&
  measuredFreeze?.freezeStatus === "PASS";

if (!STATUSES.has(gate.gateStatus)) {
  fail("gateStatus must be measured as PASS or BLOCKED");
}
if (gate.gateStatus === "PASS" && !everythingMeasuredPass) {
  fail(
    "gateStatus is PASS while a C60-C68 row, QG1 or the ABI freeze is not measured PASS",
  );
}
if (gate.gateStatus !== "PASS" && everythingMeasuredPass) {
  fail(
    "every classification row, QG1 and the ABI freeze are measured PASS but gateStatus is not PASS: the artifact is stale",
  );
}

if (errors.length > 0) {
  process.stderr.write(
    `Canonical V1 CG4 fund-safety/classification gate verification failed (${String(errors.length)}):\n`,
  );
  for (const error of errors) process.stderr.write(`- ${error}\n`);
  process.exit(1);
}

process.stdout.write(
  [
    "Canonical V1 CG4 fund-safety and classification gate verified.",
    `gateStatus: ${gate.gateStatus}`,
    `C60-C68 rows reconciled: ${String(CLASSIFICATION_ROW_IDS.length)}, PASS: ${String(passingRows)}`,
    `rows with absent prescribed surfaces: ${String(measuredZeroDebt.missingFundSafetyRows)}`,
    `rows still dependency-blocked: ${String(measuredZeroDebt.dependencyBlockedRows)}`,
    `QG1 route: ${String(gate.qg1Route?.status)} (verifier present: ${String(measuredQg1.verifierPresent)}, open reconciliation rows: ${String(measuredQg1.reconciliation.open)})`,
    `ABI freeze bound: ${String(measuredFreeze?.freezeStatus)}, IG2 ${String(measuredFreeze?.ig2Status)} with ${String(measuredFreeze?.ig2Violations)} violations`,
    `waivers: ${String(gate.waivers.length)}; C70 snapshots: ${String(gate.createsC70Snapshots)}; live/readiness claims: ${String(gate.liveOrReadinessClaims)}`,
    "",
  ].join("\n"),
);
