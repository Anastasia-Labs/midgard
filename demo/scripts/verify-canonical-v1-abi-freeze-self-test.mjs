#!/usr/bin/env node
/**
 * Behavioural self-test for the IG1/IG2 ABI freeze gate.
 *
 * An uninvoked gate and a gate that cannot fail are the same defect. This
 * suite never reads the gate's source: it spawns the real gate binary against
 * seeded artifacts and against seeded identity sources, and requires each run
 * to exit non-zero WITH its specific diagnostic. A generic failure does not
 * count.
 *
 * Two properties are deliberate:
 *
 *   - Only one input is seeded at a time. The blueprint, the Aiken corpus, the
 *     format registry, the coverage matrix and every other package's source
 *     stay real, so a seeded claim is judged against reality rather than
 *     against a second seeded copy of itself.
 *   - There is an opening AND a closing positive control. A gate that rejected
 *     everything, or that had stopped reading its inputs entirely, could not
 *     pass this suite.
 */

import { execFileSync, spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { tmpdir } from "node:os";
import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");
const gatePath = resolve(scriptDirectory, "verify-canonical-v1-abi-freeze.mjs");
const freezePath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-abi-freeze-v1.json",
);

const readIndexed = (path) =>
  execFileSync("git", ["show", `:${path}`], {
    cwd: repositoryRoot,
    encoding: "utf8",
    maxBuffer: 128 * 1024 * 1024,
  });

const freeze = JSON.parse(readFileSync(freezePath, "utf8"));
const clone = (value) => structuredClone(value);

const workspace = mkdtempSync(resolve(tmpdir(), "abi-freeze-self-test-"));
const candidatePath = resolve(workspace, "canonical-v1-abi-freeze-v1.json");

const runGate = (candidate, extra = []) => {
  writeFileSync(candidatePath, `${JSON.stringify(candidate, null, 2)}\n`);
  return spawnSync(
    process.execPath,
    [gatePath, `--freeze-under-test=${candidatePath}`, ...extra],
    { cwd: repositoryRoot, encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
  );
};

let rejected = 0;
let accepted = 0;

const mustAccept = (label) => {
  const { status, stdout, stderr } = runGate(clone(freeze));
  assert.equal(
    status,
    0,
    `${label}: the unmodified artifact must be accepted\n${stderr}`,
  );
  assert.match(stdout, /Canonical V1 ABI freeze verified\./u);
  accepted += 1;
  process.stdout.write(`accepted: ${label}\n`);
};

const mustReject = (label, expected, mutate, extra = []) => {
  const candidate = clone(freeze);
  mutate(candidate);
  const { status, stderr } = runGate(candidate, extra);
  assert.notEqual(status, 0, `${label}: the gate accepted a seeded defect`);
  const matched = expected.exec(stderr);
  assert.ok(matched, `${label}: expected ${String(expected)} in\n${stderr}`);
  rejected += 1;
  process.stdout.write(
    `rejected: ${label} -> exit ${String(status)}, ${matched[0]}\n`,
  );
};

const identity = (candidate, id) =>
  candidate.crossPackageIdentities.find((row) => row.id === id);
const family = (candidate, directory) =>
  candidate.ig2ProofFamilies.families.find(
    (row) => row.directory === directory,
  );

/* ------------------------------------------------------------------ */
/* Opening positive control.                                           */
/* ------------------------------------------------------------------ */

mustAccept("opening control");

/* ------------------------------------------------------------------ */
/* 1-6: cross-package identities are re-derived, not read.             */
/* ------------------------------------------------------------------ */

mustReject(
  "understated ABI-01 cardinality",
  /- ABI-01 cardinality is 54 but source declares 55/u,
  (candidate) => {
    identity(candidate, "ABI-01").cardinality = 54;
  },
);

mustReject(
  "tampered ABI-02 digest",
  /- ABI-02 digest is stale: source derives [0-9a-f]{64}/u,
  (candidate) => {
    identity(candidate, "ABI-02").digest = "0".repeat(64);
  },
);

mustReject(
  "ABI-04 declaration-order divergence hidden",
  /- ABI-04 publishes declarationOrder IDENTICAL but the sources measure DIVERGENT/u,
  (candidate) => {
    identity(candidate, "ABI-04").declarationOrder = "IDENTICAL";
  },
);

mustReject(
  "dropped identity",
  /- crossPackageIdentities must enumerate exactly 7 frozen identities/u,
  (candidate) => {
    candidate.crossPackageIdentities.pop();
  },
);

mustReject(
  "downgraded agreement",
  /- ABI-03 must publish agreement EXACT/u,
  (candidate) => {
    identity(candidate, "ABI-03").agreement = "APPROXIMATE";
  },
);

mustReject(
  "identity attributed to a source it was not read from",
  /- ABI-05 must name the exact declaring sources it was derived from/u,
  (candidate) => {
    identity(candidate, "ABI-05").declaredBy = [
      "demo/midgard-sdk/src/index.ts:X",
    ];
  },
);

/* ------------------------------------------------------------------ */
/* 7-8: a package that drifts alone is caught. Exactly one source is   */
/* seeded; the other packages stay real.                               */
/* ------------------------------------------------------------------ */

const seededSource = (path, transform) => {
  const seeded = resolve(workspace, `seeded-${path.split("/").pop()}`);
  writeFileSync(seeded, transform(readIndexed(path)), "utf8");
  return seeded;
};

const seededNodeManifest = seededSource(
  "demo/midgard-node/src/deployment-manifest-v1.ts",
  (text) => text.replace('"hubOracleMint",', '"hubOracleMintV2",'),
);
mustReject(
  "midgard-node contract vector drifts alone",
  /- ABI-01 is not frozen: node\.DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES disagrees with demo\/midgard-core/u,
  () => {},
  [`--source-node=${seededNodeManifest}`],
);

const seededSdkCatalogue = seededSource(
  "demo/midgard-sdk/src/fraud-proof/catalogue.ts",
  (text) =>
    text.replace(
      '  "doubleSpend",\n  "nonExistentInput",',
      '  "nonExistentInput",\n  "doubleSpend",',
    ),
);
mustReject(
  "SDK catalogue order drifts alone",
  /- ABI-02 is not frozen: sdkCatalogue\.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER disagrees with demo\/midgard-core/u,
  () => {},
  [`--source-sdkCatalogue=${seededSdkCatalogue}`],
);

/* ------------------------------------------------------------------ */
/* 9-11: derived relations and the catalogue/validator-tree binding.   */
/* ------------------------------------------------------------------ */

mustReject(
  "invented relation violation",
  /- transaction-order-contracts-are-frozen-contracts publishes \["ghostContract"\] but the tree measures \[\]/u,
  (candidate) => {
    const relation = candidate.derivedRelations.find(
      ({ id }) => id === "transaction-order-contracts-are-frozen-contracts",
    );
    relation.violations = ["ghostContract"];
    relation.status = "BLOCKED";
  },
);

mustReject(
  "clean relation published as blocked",
  /- declared-directories-still-exist measures no violation but is not published PASS/u,
  (candidate) => {
    candidate.derivedRelations.find(
      ({ id }) => id === "declared-directories-still-exist",
    ).status = "BLOCKED";
  },
);

mustReject(
  "catalogue category-id range broken",
  /- catalogueBinding\.categoryIds must be the dense 4-byte range 00000000\.\.0000000a/u,
  (candidate) => {
    candidate.catalogueBinding.categoryIds[10] = "0000000b";
  },
);

mustReject(
  "validator directory dropped from the catalogue binding",
  /- catalogueBinding\.validatorDirectories must equal the 16 directories the index carries/u,
  (candidate) => {
    candidate.catalogueBinding.validatorDirectories =
      candidate.catalogueBinding.validatorDirectories.filter(
        (directory) => directory !== "mpf-chunked-proof",
      );
  },
);

/* ------------------------------------------------------------------ */
/* 12-13: blueprint identity and bound-gate cardinalities.             */
/* ------------------------------------------------------------------ */

mustReject(
  "understated blueprint validator count",
  /- blueprintIdentity\.validatorCount is 392 but the built blueprint measures 393/u,
  (candidate) => {
    candidate.blueprintIdentity.validatorCount = 392;
  },
);

mustReject(
  "bound format-registry row count drifts",
  /- format-registry publishes .*"rows":131.* but the bound surface measures .*"rows":132/u,
  (candidate) => {
    candidate.boundGates.find(
      ({ id }) => id === "format-registry",
    ).counts.rows = 131;
  },
);

/* ------------------------------------------------------------------ */
/* 14-18: the IG2 proof-family gate.                                   */
/* ------------------------------------------------------------------ */

mustReject(
  "derived IG2 dimension contradicted",
  /- double-spend\.tooling publishes BLOCKED but the tree measures PASS/u,
  (candidate) => {
    family(candidate, "double-spend").evidence.tooling.status = "BLOCKED";
  },
);

mustReject(
  "IG2 dimension claimed PASS while citing a file that is not there",
  /- zero-input\.correction cites docs\/exec-plans\/evidence\/does-not-exist\.json, which is not in the index/u,
  (candidate) => {
    family(candidate, "zero-input").evidence.correction.paths = [
      "docs/exec-plans/evidence/does-not-exist.json",
    ];
  },
);

mustReject(
  "IG2 dimension claimed PASS while citing nothing",
  /- min-fee\.tooling is PASS and must cite at least one surface it was measured from/u,
  (candidate) => {
    family(candidate, "min-fee").evidence.tooling.status = "PASS";
  },
);

mustReject(
  "integrated family upgraded without upgrading its violated clauses",
  /- transition-trace publishes violates \[.*\] but measures \[.*\]/u,
  (candidate) => {
    family(candidate, "transition-trace").evidence.correction.status = "PASS";
  },
);

mustReject(
  "IG2 status promoted while families are not integrable",
  /- ig2ProofFamilies\.status is PASS while a family with verifier logic lacks a required evidence dimension/u,
  (candidate) => {
    candidate.ig2ProofFamilies.status = "PASS";
  },
);

mustReject(
  "family inventory truncated",
  /- ig2ProofFamilies\.families must enumerate exactly the 16 directories that carry verifier logic/u,
  (candidate) => {
    candidate.ig2ProofFamilies.families.pop();
  },
);

/* ------------------------------------------------------------------ */
/* 19-21: drift findings, waivers and the freeze status itself.        */
/* ------------------------------------------------------------------ */

mustReject(
  "measured drift finding deleted",
  /- driftFindings must equal the 2 findings this gate measures/u,
  (candidate) => {
    candidate.driftFindings = [];
  },
);

mustReject(
  "waiver introduced",
  /- an ABI freeze admits no waivers/u,
  (candidate) => {
    candidate.waivers = [{ id: "just-this-once" }];
  },
);

mustReject(
  "freeze promoted while blocked",
  /- freezeStatus is PASS while a relation, family dimension or drift finding is outstanding/u,
  (candidate) => {
    candidate.freezeStatus = "PASS";
  },
);

/* ------------------------------------------------------------------ */
/* Closing positive control.                                           */
/* ------------------------------------------------------------------ */

mustAccept("closing control");

rmSync(workspace, { recursive: true, force: true });

process.stdout.write(
  `abi-freeze:self-test: PASS\ncontrol runs accepted: ${String(accepted)}; hostile mutations rejected: ${String(rejected)}\n`,
);
