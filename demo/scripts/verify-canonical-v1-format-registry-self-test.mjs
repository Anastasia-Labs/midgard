#!/usr/bin/env node

// Negative self-test for the canonical V1 format-registry gate's bounded
// not-applicable excuses, its exact-field pins (V-6, issue #530) and its
// refusal to accept a verifier's own output string as a test (V-8, #531). The
// gate
// is tested by behavior, not by reading its source: each hostile mutation is
// written to a throwaway copy of the registry, the real gate is invoked against
// that copy through its `--registry-under-test=` hook, and the run must exit
// non-zero carrying the expected diagnostic. Only the artifact is redirected —
// the Aiken corpus, the canonical Markdown and every repository source the gate
// checks against stay the real ones, so a seeded excuse is judged against
// reality. A clean copy is run first and last, so a gate that rejected
// everything (or nothing) could not pass this suite.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(scriptDir, "../..");
const gatePath = resolve(scriptDir, "verify-canonical-v1-format-registry.mjs");
const registryPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-format-registry-v1.json",
);
const registry = JSON.parse(readFileSync(registryPath, "utf8"));
const clone = (value) => structuredClone(value);
const rowOf = (candidate, id) => {
  const row = candidate.formats.find((format) => format.id === id);
  assert.ok(row !== undefined, `${id} must exist in the registry under test`);
  return row;
};
const formOf = (candidate, id, name) => {
  const form = rowOf(candidate, id).canonicalForms.find(
    (canonicalForm) => canonicalForm.name === name,
  );
  assert.ok(form !== undefined, `${id}/${name} must exist in the registry`);
  return form;
};

const workspace = mkdtempSync(resolve(tmpdir(), "format-registry-self-test-"));
const candidatePath = resolve(workspace, "registry.json");

const runGate = (candidate, extraArguments = []) => {
  writeFileSync(candidatePath, `${JSON.stringify(candidate, null, 2)}\n`);
  const result = spawnSync(
    process.execPath,
    [gatePath, `--registry-under-test=${candidatePath}`, ...extraArguments],
    { cwd: repoRoot, encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
  );
  assert.ok(
    result.status !== null,
    `gate did not run: ${result.error?.message ?? "unknown failure"}`,
  );
  return result;
};

// Positive control: the registry as published must be accepted.
const control = runGate(clone(registry));
assert.equal(
  control.status,
  0,
  `published registry must pass: ${control.stderr}`,
);

let rejectedMutations = 0;
const mustReject = (label, expected, mutate, extraArguments = []) => {
  const candidate = clone(registry);
  mutate(candidate);
  const { status, stderr } = runGate(candidate, extraArguments);
  assert.notEqual(status, 0, `${label}: gate exited 0 on a seeded defect`);
  assert.match(stderr, expected, `${label}: missing diagnostic in:\n${stderr}`);
  const matched = stderr.match(expected);
  process.stdout.write(`rejected: ${label} -> exit ${status}, ${matched[0]}\n`);
  rejectedMutations += 1;
};

// --- Bounded not-applicable excuses ---------------------------------------

// Free text alone is not evidence: an uncategorised excuse cannot be checked.
mustReject(
  "D06 excuses cross-language agreement with prose and no category",
  /D06\.crossLanguageEvidence: free-text notApplicableReason is not evidence/u,
  (candidate) => {
    delete rowOf(candidate, "D06").crossLanguageEvidence.notApplicableCategory;
  },
);

// The category set is closed, so a new excuse cannot be invented in the data.
mustReject(
  "D06 invents an unrecognised justification category",
  /D06\.crossLanguageEvidence\.notApplicableCategory "no-plutus-datum" is not one of/u,
  (candidate) => {
    rowOf(candidate, "D06").crossLanguageEvidence.notApplicableCategory =
      "no-plutus-datum";
  },
);

// The exact defect V-6 found: a row with a real Aiken counterpart excusing
// itself out of cross-language evidence. The excuse is checked against the
// Aiken corpus, so it fails on this row's own identity symbols.
mustReject(
  "N05 drops its cross-language tests and claims no Aiken counterpart",
  /N05\.crossLanguageEvidence claims no-onchain-counterpart, but the Aiken corpus names \d+ of this row's \d+ identity symbols/u,
  (candidate) => {
    const row = rowOf(candidate, "N05");
    row.crossLanguageEvidence = {
      status: "PASS",
      tests: [],
      notApplicableCategory: "no-onchain-counterpart",
      notApplicableReason:
        "Aiken never decodes the compact witness set; the TypeScript encoder is the only implementation.",
    };
  },
);

// The deleted-identity excuse is bound to the row's own disposition.
mustReject(
  "C01 claims absence from Aiken for a format it also declares deleted",
  /C01\.crossLanguageEvidence claims no-onchain-counterpart for a deleted format/u,
  (candidate) => {
    rowOf(candidate, "C01").crossLanguageEvidence.notApplicableCategory =
      "no-onchain-counterpart";
  },
);

// A retained row cannot borrow the deletion excuse.
mustReject(
  "D06 claims deleted-identity while the row is retained",
  /D06\.crossLanguageEvidence claims deleted-identity but the row disposition is "retain"/u,
  (candidate) => {
    rowOf(candidate, "D06").crossLanguageEvidence.notApplicableCategory =
      "deleted-identity";
  },
);

// Tests and an excuse are mutually exclusive: a row may not hold both so that
// deleting the tests later leaves a silently passing excuse behind.
mustReject(
  "L01 carries both cross-language tests and a not-applicable category",
  /L01\.crossLanguageEvidence carries cross-language tests and must not also claim a not-applicable category/u,
  (candidate) => {
    rowOf(candidate, "L01").crossLanguageEvidence.notApplicableCategory =
      "no-onchain-counterpart";
  },
);

// --- Verifier output strings are not tests ---------------------------------

// The exact defect V-8 found (issue #531): L01's cross-language claim cited, as
// a test name, the literal argument of a `process.stdout.write` in the very
// verifier it named, so the claim was satisfied by that verifier's own success
// message in its own source. Restoring that shape must be rejected.
mustReject(
  "L01 cites the ABI verifier's own success message as a cross-language test",
  /cites "header-v1-abi: PASS[^"]*" as a test name, but demo\/scripts\/verify-canonical-v1-header-v1-abi\.mjs only ever writes it as output/u,
  (candidate) => {
    rowOf(candidate, "L01").crossLanguageEvidence.tests[1] = {
      path: "demo/scripts/verify-canonical-v1-header-v1-abi.mjs",
      testNames: ["header-v1-abi: PASS constructor "],
    };
  },
);

// The same mechanism in a positive-evidence slot is rejected too, so the rule
// is not a special case for one row's cross-language field.
mustReject(
  "L01 cites a verifier output string as positive evidence",
  /cites "header-v1-abi: PASS[^"]*" as a test name, but demo\/scripts\/verify-canonical-v1-header-v1-abi\.mjs only ever writes it as output/u,
  (candidate) => {
    rowOf(candidate, "L01").positiveEvidence[2] = {
      path: "demo/scripts/verify-canonical-v1-header-v1-abi.mjs",
      testNames: ["header-v1-abi: PASS constructor "],
    };
  },
);

// --- Exact-field pins ------------------------------------------------------

// The N05 layout pin used to be guarded by `isPass("N05")`, so demoting the
// row's own auditStatus switched the pin off. It is unconditional now.
mustReject(
  "N05 is demoted to UNVERIFIED to shed its layout pin",
  /N05 must preserve the stable compact witness tuple/u,
  (candidate) => {
    const row = rowOf(candidate, "N05");
    row.auditStatus = "UNVERIFIED";
    row.disposition = "UNVERIFIED";
    for (const field of [
      "sourceEvidence",
      "canonicalForms",
      "positiveEvidence",
      "rejectionEvidence",
      "obsoleteBranchEvidence",
      "notes",
    ]) {
      row[field] = [];
    }
    row.crossLanguageEvidence = {
      status: "UNVERIFIED",
      tests: [],
      notApplicableReason: null,
    };
  },
  ["--allow-incomplete"],
);

// #532 (residual of #519). The mutation above is the only one that opts into
// `--allow-incomplete`. The same seeded demotion is replayed WITHOUT the flag
// here, which keeps strict rejection available to anyone running this retired
// registry's standalone self-test.
mustReject(
  "N05 is demoted to UNVERIFIED and strict mode must reject it",
  /- N05\.auditStatus must be PASS/u,
  (candidate) => {
    const row = rowOf(candidate, "N05");
    row.auditStatus = "UNVERIFIED";
    row.disposition = "UNVERIFIED";
    for (const field of [
      "sourceEvidence",
      "canonicalForms",
      "positiveEvidence",
      "rejectionEvidence",
      "obsoleteBranchEvidence",
      "notes",
    ]) {
      row[field] = [];
    }
    row.crossLanguageEvidence = {
      status: "UNVERIFIED",
      tests: [],
      notApplicableReason: null,
    };
  },
);

// A positional field list must cover a dense index range.
mustReject(
  "L16 renumbers a member so the pinned layout skips an index",
  /L16.*\.exactFields pins index 6 where index 5 was required/su,
  (candidate) => {
    const form = formOf(candidate, "L16", "LedgerOutputCommitmentV1");
    form.exactFields[5] = form.exactFields[5].replace(/^5:/u, "6:");
  },
);

// The pinned member count must equal the declared arity.
mustReject(
  "V02 drops a member while keeping its declared arity",
  /V02.*\.exactFields pins 7 members for declared arrayArity 8/su,
  (candidate) => {
    formOf(candidate, "V02", "ValidationTraceDescriptorV1").exactFields.pop();
  },
);

// A row with a cross-language counterpart may not revert to unpinned labels.
mustReject(
  "V01 replaces its per-index pins with bare labels",
  /V01.*has a cross-language counterpart and a declared arrayArity 15, so its exactFields must pin each member by index/su,
  (candidate) => {
    const form = formOf(candidate, "V01", "ValidationMachineStateV1");
    form.exactFields = form.exactFields.map((field) =>
      field.replace(/^\d+:\s*/u, ""),
    );
  },
);

// The deficiency register is verifier-owned and derived-equal: a form that has
// been pinned may not be left in it as a standing excuse.
mustReject(
  "V16 is pinned but left in the exact-field pin register",
  /exact-field pin register lists forms that now pin their members[^\n]*V16/u,
  (candidate) => {
    const form = formOf(
      candidate,
      "V16",
      "Ledger delta controls and operations V1",
    );
    form.exactFields = Array.from(
      { length: form.arrayArity },
      (_unused, index) => `${index}: ledger delta control field ${index}`,
    );
  },
);

// The published split is re-derived, so it cannot be edited to hide movement.
mustReject(
  "the published cross-language split is overstated",
  /registry\.crossLanguageSummary does not equal the derived split/u,
  (candidate) => {
    candidate.crossLanguageSummary.withCounterpartTests = 132;
    candidate.crossLanguageSummary.notApplicable = 0;
  },
);

// Closing positive control: the same harness still accepts the published
// registry, so the rejections above cannot be a gate that rejects everything.
const closingControl = runGate(clone(registry));
assert.equal(
  closingControl.status,
  0,
  `published registry must still pass: ${closingControl.stderr}`,
);

rmSync(workspace, { recursive: true, force: true });
process.stdout.write(
  `format-registry:self-test: PASS\ncontrol runs accepted: 2; hostile mutations rejected: ${rejectedMutations}\n`,
);
