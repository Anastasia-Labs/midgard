#!/usr/bin/env node
/**
 * CG1 — P1 aggregate control-publication fit gate.
 *
 * GOAL_SPEC §8.1 P0/P1 control-plane gate, row CG1: "Every parameterized
 * hub/control validator fits a real 16,384-byte publication transaction;
 * evidence is bound to final validator hashes."
 *
 * This gate does not re-run the emulator. The expensive part — building,
 * signing, submitting and measuring one real publication transaction per
 * roster validator against a genuinely real `MidgardValidators` bundle — was
 * done once, offline, to produce the pinned artifact. What this verifier
 * checks is that the artifact tells the truth about that measurement:
 *
 *   - the roster of publication targets is re-derived from
 *     `nodeRuntimeReferenceScriptTargets`'s own source text (not hand-listed),
 *     so the artifact cannot silently drop, add or reorder a validator;
 *   - the one deliberate exclusion (the oversized CEK direct resolver) is
 *     checked against the same source for genuine absence, checked against
 *     the reference-script auth-token map for being a real role (not an
 *     invented name), and its necessity citation is checked against the
 *     document it claims to quote;
 *   - every roster entry's fit arithmetic (`l1ByteMarginBytes = 16384 -
 *     completeSignedTransactionBytes`) is re-derived and must be positive;
 *   - the blueprint this artifact's hashes are bound to is re-hashed from the
 *     working tree (it is gitignored, so there is no committed copy to check
 *     against) and must match the pin exactly.
 *
 * A gate that could green a stale, invented or mismeasured claim would be
 * worse than no gate, so every check here fails closed rather than trusting
 * the artifact's own arithmetic or citations.
 */

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");

const GATE_PATH =
  "docs/exec-plans/evidence/canonical-v1-cg1-control-publication-fit-v1.json";
const BLUEPRINT_PATH = "onchain/aiken/plutus.json";
/* The blueprint is gitignored, so a fresh checkout (CI) structurally     */
/* cannot carry it. --blueprint-optional skips ONLY the working-tree hash */
/* comparison in that case; every index-bound check still enforces, and a */
/* PRESENT blueprint is always compared regardless of the flag.           */
const blueprintOptional = process.argv.includes("--blueprint-optional");
const ROSTER_SOURCE_PATH =
  "demo/midgard-node/src/transactions/reference-scripts.ts";
const AUTH_TOKEN_MAP_PATH = "demo/midgard-sdk/src/reference-scripts.ts";
const NECESSITY_DOC_PATH =
  "docs/exec-plans/evidence/necessity/cek-program-material-v1.md";
const RESOLVER_APPLIED_HASHES_TEST_PATH =
  "demo/midgard-sdk/tests/validation-resolver-applied-hashes.test.ts";
const L1_MAX_TX_SIZE = 16384;

const flagValue = (name) => {
  const prefix = `--${name}=`;
  const found = process.argv.find((argument) => argument.startsWith(prefix));
  return found === undefined ? null : found.slice(prefix.length);
};
const gatePath = (() => {
  const explicit =
    flagValue("gate-under-test") ?? process.env.MIDGARD_CG1_GATE_PATH ?? null;
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

const sameJson = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

/* ------------------------------------------------------------------ */
/* A minimal, dependency-free extractor for the two source shapes this  */
/* gate reads: an arrow function returning an array literal, and a      */
/* `export const X = { ... } as const` string-to-string object literal. */
/* Both walk bracket depth with quote-awareness rather than regexing the */
/* whole file, so a literal brace/bracket inside a string cannot desync  */
/* the scan.                                                             */
/* ------------------------------------------------------------------ */

const skipStringLiteral = (text, start) => {
  const quote = text[start];
  let index = start + 1;
  while (index < text.length) {
    const character = text[index];
    if (character === "\\") {
      index += 2;
      continue;
    }
    if (character === quote) return index + 1;
    index += 1;
  }
  throw new Error("unterminated string literal");
};

/** Scans forward from `openIndex` (which must point at an opening bracket)
 * and returns the index just past its matching close, tracking string
 * literals and `//`/`/* *\/` comments so bracket and quote characters inside
 * either are never counted. Without comment-awareness a stray apostrophe in
 * an English comment (e.g. "midgard-core's") reads as an unterminated
 * single-quote string and desyncs the whole scan. */
const matchingBracketEnd = (text, openIndex, openChar, closeChar) => {
  let depth = 0;
  let index = openIndex;
  while (index < text.length) {
    const character = text[index];
    const next = text[index + 1];
    if (character === "/" && next === "/") {
      const lineEnd = text.indexOf("\n", index);
      index = lineEnd < 0 ? text.length : lineEnd + 1;
      continue;
    }
    if (character === "/" && next === "*") {
      const blockEnd = text.indexOf("*/", index + 2);
      if (blockEnd < 0) throw new Error("unterminated block comment");
      index = blockEnd + 2;
      continue;
    }
    if (character === '"' || character === "'" || character === "`") {
      index = skipStringLiteral(text, index);
      continue;
    }
    if (character === openChar) depth += 1;
    else if (character === closeChar) {
      depth -= 1;
      if (depth === 0) return index + 1;
    }
    index += 1;
  }
  throw new Error(
    `unbalanced ${openChar}${closeChar} starting at ${String(openIndex)}`,
  );
};

/** Extracts the ordered `name: "..."` literals from an
 * `export const X = (...): T => [ ... ];` array-literal function body. */
const extractArrayLiteralNames = (source, exportedConstName) => {
  const declMarker = `export const ${exportedConstName} = (`;
  const declStart = source.indexOf(declMarker);
  if (declStart < 0) {
    throw new Error(`${exportedConstName} declaration not found in source`);
  }
  const arrowMarker = "=> [";
  const arrowIndex = source.indexOf(arrowMarker, declStart);
  if (arrowIndex < 0) {
    throw new Error(`${exportedConstName} does not return an array literal`);
  }
  const openIndex = arrowIndex + arrowMarker.length - 1;
  const closeEnd = matchingBracketEnd(source, openIndex, "[", "]");
  const body = source.slice(openIndex, closeEnd);
  return [...body.matchAll(/\bname:\s*"([^"]+)"/gu)].map((match) => match[1]);
};

/** Extracts the ordered `"key": "value"` string-to-string entries from an
 * `export const X = { ... } as const;` object literal. */
const extractStringRecord = (source, exportedConstName) => {
  const declMarker = `export const ${exportedConstName} = {`;
  const declStart = source.indexOf(declMarker);
  if (declStart < 0) {
    throw new Error(`${exportedConstName} declaration not found in source`);
  }
  const openIndex = declStart + declMarker.length - 1;
  const closeEnd = matchingBracketEnd(source, openIndex, "{", "}");
  const body = source.slice(openIndex, closeEnd);
  const record = new Map();
  for (const match of body.matchAll(/"([^"]+)":\s*"([^"]+)"/gu)) {
    record.set(match[1], match[2]);
  }
  return record;
};

/* ------------------------------------------------------------------ */
/* Derivation from source.                                             */
/* ------------------------------------------------------------------ */

if (!indexHas(ROSTER_SOURCE_PATH)) {
  fail(`Roster source ${ROSTER_SOURCE_PATH} is not in the index`);
}
if (!indexHas(AUTH_TOKEN_MAP_PATH)) {
  fail(`Auth-token map source ${AUTH_TOKEN_MAP_PATH} is not in the index`);
}

const rosterSourceText = indexHas(ROSTER_SOURCE_PATH)
  ? readIndexed(ROSTER_SOURCE_PATH)
  : "";
const authTokenMapSourceText = indexHas(AUTH_TOKEN_MAP_PATH)
  ? readIndexed(AUTH_TOKEN_MAP_PATH)
  : "";

const derivedRosterNames = rosterSourceText
  ? extractArrayLiteralNames(
      rosterSourceText,
      "nodeRuntimeReferenceScriptTargets",
    )
  : [];
const derivedAuthTokenMap = authTokenMapSourceText
  ? extractStringRecord(
      authTokenMapSourceText,
      "REFERENCE_SCRIPT_AUTH_TOKEN_NAMES",
    )
  : new Map();

// The two conditional blocks in `nodeRuntimeReferenceScriptTargets` are only
// included under a real (non-stub) contract set. The array-literal extractor
// reads every `name:` occurrence textually regardless of which branch a
// ternary takes, so a passing extraction of 34 names is not by itself proof
// the guards still read the way this gate assumes. Check the two guard
// expressions verbatim so a silent change to the inclusion condition is
// caught even though it would not change which names appear in source text.
const CEK_MATERIAL_GUARD =
  "contracts.cekProgramMaterial.spendingScriptHash ===\n  contracts.txOrder.spendingScriptHash";
const VALIDATION_TRACE_GUARD =
  "contracts.fraudProofs.validationTraceDispute === undefined";
if (rosterSourceText && !rosterSourceText.includes(CEK_MATERIAL_GUARD)) {
  fail(
    "nodeRuntimeReferenceScriptTargets's CEK program-material inclusion guard no longer reads as expected; the roster derivation may no longer match a real contract set",
  );
}
if (rosterSourceText && !rosterSourceText.includes(VALIDATION_TRACE_GUARD)) {
  fail(
    "nodeRuntimeReferenceScriptTargets's validation-trace-dispute inclusion guard no longer reads as expected; the roster derivation may no longer match a real contract set",
  );
}

const rosterSourceSha256 = rosterSourceText
  ? createHash("sha256").update(rosterSourceText, "utf8").digest("hex")
  : null;
const authTokenMapSourceSha256 = authTokenMapSourceText
  ? createHash("sha256").update(authTokenMapSourceText, "utf8").digest("hex")
  : null;

const necessityDocText = indexHas(NECESSITY_DOC_PATH)
  ? readIndexed(NECESSITY_DOC_PATH)
  : "";

/* ------------------------------------------------------------------ */
/* Blueprint hash basis. `onchain/aiken/plutus.json` is gitignored: there  */
/* is no committed copy to measure against, so this is the one surface   */
/* this gate reads from the working tree rather than the index.          */
/* ------------------------------------------------------------------ */

const blueprintAbsolutePath = resolve(repositoryRoot, BLUEPRINT_PATH);
const blueprintExists = existsSync(blueprintAbsolutePath);
let blueprintSha256 = null;
let blueprintMd5 = null;
let blueprintValidatorCount = -1;
if (blueprintExists) {
  const blueprintBytes = readFileSync(blueprintAbsolutePath);
  blueprintSha256 = createHash("sha256").update(blueprintBytes).digest("hex");
  blueprintMd5 = createHash("md5").update(blueprintBytes).digest("hex");
  try {
    const parsed = JSON.parse(blueprintBytes.toString("utf8"));
    blueprintValidatorCount = Array.isArray(parsed.validators)
      ? parsed.validators.length
      : -1;
  } catch {
    blueprintValidatorCount = -1;
  }
} else if (!blueprintOptional) {
  fail(
    `Blueprint ${BLUEPRINT_PATH} does not exist in the working tree; CG1 cannot measure a hash basis without it (pass --blueprint-optional only where the gitignored blueprint is structurally absent, e.g. CI)`,
  );
}
const blueprintIsIndexed = indexHas(BLUEPRINT_PATH);

/* ------------------------------------------------------------------ */
/* Artifact.                                                           */
/* ------------------------------------------------------------------ */

if (!existsSync(gatePath)) {
  process.stderr.write(`CG1 gate artifact does not exist: ${gatePath}\n`);
  process.exit(1);
}
const gate = JSON.parse(readFileSync(gatePath, "utf8"));

if (
  gate.schemaVersion !==
    "midgard-canonical-v1-cg1-control-publication-fit-v1" ||
  !Array.isArray(gate.goalIds) ||
  !sameJson(gate.goalIds, ["CG1"])
) {
  fail("the gate artifact must be the CG1 control-publication fit review");
}
if (!Array.isArray(gate.waivers) || gate.waivers.length !== 0) {
  fail("CG1 admits no waivers; the waivers array must exist and be empty");
}
if (gate.createsC70Snapshots !== false) {
  fail("CG1 is a local gate and must never create a C70 trusted snapshot");
}
if (gate.liveOrReadinessClaims !== 0) {
  fail("CG1 must publish exactly 0 live or readiness claims");
}

/* --- 1. Hash basis: the blueprint this artifact is bound to ---------- */

const hashBasis = gate.hashBasis;
if (hashBasis === undefined || hashBasis === null) {
  fail(
    "hashBasis is required: every hash in this artifact is bound to a blueprint",
  );
} else {
  if (hashBasis.blueprintPath !== BLUEPRINT_PATH) {
    fail(`hashBasis.blueprintPath must be ${BLUEPRINT_PATH}`);
  }
  if (hashBasis.blueprintTrackedInGit !== blueprintIsIndexed) {
    fail(
      `hashBasis.blueprintTrackedInGit is ${String(hashBasis.blueprintTrackedInGit)} but the index measures ${String(blueprintIsIndexed)}`,
    );
  }
  if (blueprintExists) {
    if (hashBasis.blueprintSha256 !== blueprintSha256) {
      fail(
        `hashBasis.blueprintSha256 is stale: the working tree measures ${String(blueprintSha256)}`,
      );
    }
    if (hashBasis.blueprintMd5 !== blueprintMd5) {
      fail(
        `hashBasis.blueprintMd5 is stale: the working tree measures ${String(blueprintMd5)}`,
      );
    }
    if (hashBasis.validatorCount !== blueprintValidatorCount) {
      fail(
        `hashBasis.validatorCount is stale: the working tree measures ${String(blueprintValidatorCount)}`,
      );
    }
  }
  if (
    typeof hashBasis.invalidationTrigger !== "string" ||
    !hashBasis.invalidationTrigger.includes("#510")
  ) {
    fail(
      "hashBasis.invalidationTrigger must name issue #510's blueprint freeze-event regeneration",
    );
  }
  if (
    indexHas(RESOLVER_APPLIED_HASHES_TEST_PATH) &&
    !readIndexed(RESOLVER_APPLIED_HASHES_TEST_PATH).includes(BLUEPRINT_PATH)
  ) {
    fail(
      `hashBasis.crossCheckedAgainst claims ${RESOLVER_APPLIED_HASHES_TEST_PATH} hashes ${BLUEPRINT_PATH} directly, but that path string is absent from it`,
    );
  }
}

/* --- 3. Roster source binding ----------------------------------------- */

const rosterSource = gate.rosterSource;
if (rosterSource === undefined || rosterSource === null) {
  fail("rosterSource is required: the roster must cite its derivation source");
} else {
  if (rosterSource.function !== "nodeRuntimeReferenceScriptTargets") {
    fail("rosterSource.function must be nodeRuntimeReferenceScriptTargets");
  }
  if (rosterSource.path !== ROSTER_SOURCE_PATH) {
    fail(`rosterSource.path must be ${ROSTER_SOURCE_PATH}`);
  }
  if (rosterSource.sha256AtMeasurement !== rosterSourceSha256) {
    fail(
      `rosterSource.sha256AtMeasurement is stale: the index measures ${String(rosterSourceSha256)} for ${ROSTER_SOURCE_PATH}; the roster must be re-measured after this source changed`,
    );
  }
  if (rosterSource.authTokenMap?.path !== AUTH_TOKEN_MAP_PATH) {
    fail(`rosterSource.authTokenMap.path must be ${AUTH_TOKEN_MAP_PATH}`);
  }
  if (
    rosterSource.authTokenMap?.exportName !==
    "REFERENCE_SCRIPT_AUTH_TOKEN_NAMES"
  ) {
    fail(
      "rosterSource.authTokenMap.exportName must be REFERENCE_SCRIPT_AUTH_TOKEN_NAMES",
    );
  }
  if (
    rosterSource.authTokenMap?.sha256AtMeasurement !== authTokenMapSourceSha256
  ) {
    fail(
      `rosterSource.authTokenMap.sha256AtMeasurement is stale: the index measures ${String(authTokenMapSourceSha256)} for ${AUTH_TOKEN_MAP_PATH}`,
    );
  }
}

/* --- 4. Roster inventory: re-derived from source, not restated -------- */

const publishedRoster = Array.isArray(gate.roster) ? gate.roster : [];
const publishedNames = publishedRoster.map((entry) => entry?.name);
const missingRosterEntries = derivedRosterNames.filter(
  (name) => !publishedNames.includes(name),
);
const extraRosterEntries = publishedNames.filter(
  (name) => !derivedRosterNames.includes(name),
);

if (!sameJson(publishedNames, derivedRosterNames)) {
  fail(
    `roster names/order must equal nodeRuntimeReferenceScriptTargets's source order exactly: missing ${JSON.stringify(missingRosterEntries)}, extra ${JSON.stringify(extraRosterEntries)}`,
  );
}
if (gate.rosterCount !== publishedRoster.length) {
  fail("rosterCount must equal the published roster's length");
}

const seenRosterNames = new Set();
let rosterEntriesNotFitting = 0;
for (const [index, entry] of publishedRoster.entries()) {
  const label = entry?.name ?? `roster[${String(index)}]`;
  if (seenRosterNames.has(entry?.name)) {
    fail(`roster entry "${label}" is duplicated`);
  }
  seenRosterNames.add(entry?.name);
  if (!derivedAuthTokenMap.has(entry?.name)) {
    fail(
      `roster entry "${label}" has no corresponding key in REFERENCE_SCRIPT_AUTH_TOKEN_NAMES; it cannot be authenticated as a reference-script role`,
    );
  }
  if (
    typeof entry?.appliedScriptHash !== "string" ||
    !/^[0-9a-f]{56,64}$/u.test(entry.appliedScriptHash)
  ) {
    fail(`roster entry "${label}" must publish a real hex applied script hash`);
  }
  if (
    typeof entry?.serializedScriptBytes !== "number" ||
    entry.serializedScriptBytes <= 0
  ) {
    fail(
      `roster entry "${label}" must publish a positive serializedScriptBytes`,
    );
  }
  if (
    typeof entry?.completeSignedTransactionBytes !== "number" ||
    entry.completeSignedTransactionBytes <= 0
  ) {
    fail(
      `roster entry "${label}" must publish a positive completeSignedTransactionBytes`,
    );
  }
  const expectedMargin =
    L1_MAX_TX_SIZE - (entry?.completeSignedTransactionBytes ?? 0);
  if (entry?.l1ByteMarginBytes !== expectedMargin) {
    fail(
      `roster entry "${label}" l1ByteMarginBytes must equal ${String(L1_MAX_TX_SIZE)} - completeSignedTransactionBytes = ${String(expectedMargin)}, found ${String(entry?.l1ByteMarginBytes)}`,
    );
  }
  const fits = expectedMargin > 0;
  if (entry?.fits !== fits) {
    fail(`roster entry "${label}" fits must equal ${String(fits)}`);
  }
  if (!fits) {
    rosterEntriesNotFitting += 1;
    fail(
      `roster entry "${label}" does not fit the ${String(L1_MAX_TX_SIZE)}-byte L1 publication envelope (margin ${String(expectedMargin)}); CG1 requires every included validator to fit`,
    );
  }
  if (typeof entry?.feeLovelace !== "number" || entry.feeLovelace <= 0) {
    fail(`roster entry "${label}" must publish a positive feeLovelace`);
  }
}
if (gate.rosterAllFit !== (rosterEntriesNotFitting === 0)) {
  fail("rosterAllFit must equal (every roster entry fits)");
}

/* --- 5. Exclusions: named, justified, and genuinely absent from source */

const CEK_RESOLVER_NAME = "V1 validation-trace CEK direct resolver";
const publishedExclusions = Array.isArray(gate.exclusions)
  ? gate.exclusions
  : [];
let unjustifiedExclusions = 0;

if (publishedExclusions.length !== 1) {
  fail(
    `exclusions must record exactly one entry, the oversized CEK direct resolver ("${CEK_RESOLVER_NAME}")`,
  );
  unjustifiedExclusions += 1;
} else {
  const exclusion = publishedExclusions[0];
  let exclusionOk = true;
  if (exclusion.name !== CEK_RESOLVER_NAME) {
    fail(
      `exclusions must record exactly one entry, the oversized CEK direct resolver ("${CEK_RESOLVER_NAME}"), found "${String(exclusion.name)}"`,
    );
    exclusionOk = false;
  }
  if (!derivedAuthTokenMap.has(exclusion.name)) {
    fail(
      `exclusion "${exclusion.name}" is not a key in REFERENCE_SCRIPT_AUTH_TOKEN_NAMES; it cannot be a real role to exclude`,
    );
    exclusionOk = false;
  }
  if (exclusion.tokenName !== derivedAuthTokenMap.get(exclusion.name)) {
    fail(
      `exclusion "${exclusion.name}" tokenName must equal REFERENCE_SCRIPT_AUTH_TOKEN_NAMES's value ${JSON.stringify(derivedAuthTokenMap.get(exclusion.name))}`,
    );
    exclusionOk = false;
  }
  if (derivedRosterNames.includes(exclusion.name)) {
    fail(
      `exclusion "${exclusion.name}" is claimed excluded but IS present in nodeRuntimeReferenceScriptTargets's derived roster; the exclusion is no longer justified`,
    );
    exclusionOk = false;
  }
  if (
    typeof exclusion.measuredCompleteSignedTransactionBytes !== "number" ||
    exclusion.measuredCompleteSignedTransactionBytes <= L1_MAX_TX_SIZE
  ) {
    fail(
      `exclusion "${exclusion.name}" must publish a measuredCompleteSignedTransactionBytes above ${String(L1_MAX_TX_SIZE)}; otherwise it belongs in the roster, not the exclusions`,
    );
    exclusionOk = false;
  }
  if (
    exclusion.measuredL1ByteMarginBytes !==
    L1_MAX_TX_SIZE - (exclusion.measuredCompleteSignedTransactionBytes ?? 0)
  ) {
    fail(
      `exclusion "${exclusion.name}" measuredL1ByteMarginBytes arithmetic is wrong`,
    );
    exclusionOk = false;
  }
  const citations = Array.isArray(exclusion.recordedIn)
    ? exclusion.recordedIn
    : [];
  if (citations.length === 0) {
    fail(`exclusion "${exclusion.name}" must cite where it is recorded`);
    exclusionOk = false;
  }
  for (const citation of citations) {
    const citationText =
      citation?.path === NECESSITY_DOC_PATH ? necessityDocText : null;
    if (citationText === null) {
      fail(
        `exclusion "${exclusion.name}" cites unsupported path ${JSON.stringify(citation?.path)}`,
      );
      exclusionOk = false;
      continue;
    }
    if (
      typeof citation.quote !== "string" ||
      citation.quote.length === 0 ||
      !citationText.includes(citation.quote)
    ) {
      fail(
        `exclusion "${exclusion.name}" cites a quote not found verbatim in ${String(citation?.path)}`,
      );
      exclusionOk = false;
    }
  }
  if (!exclusionOk) unjustifiedExclusions += 1;
}

/* --- 6. Oversize canary: points at the self-test that proves it -------- */

const canary = gate.oversizeCanary;
const SELF_TEST_PATH =
  "demo/scripts/verify-canonical-v1-cg1-control-publication-fit-self-test.mjs";
if (canary === undefined || canary === null) {
  fail(
    "oversizeCanary is required: CG1 must prove it enforces the real byte bound",
  );
} else {
  if (canary.selfTestPath !== SELF_TEST_PATH) {
    fail(`oversizeCanary.selfTestPath must be ${SELF_TEST_PATH}`);
  }
  if (!indexHas(SELF_TEST_PATH)) {
    fail(`oversizeCanary names ${SELF_TEST_PATH}, which is not in the index`);
  }
}

/* --- 7. Zero-debt report ------------------------------------------- */

const measuredZeroDebt = {
  rosterEntriesNotFitting,
  missingRosterEntries: missingRosterEntries.length,
  extraRosterEntries: extraRosterEntries.length,
  unjustifiedExclusions,
  staleBlueprintPins: blueprintExists
    ? gate.hashBasis?.blueprintSha256 === blueprintSha256
      ? 0
      : 1
    : blueprintOptional
      ? 0
      : 1,
};

if (!sameJson(gate.zeroDebt, measuredZeroDebt)) {
  fail(
    `zeroDebt must equal the measured report ${JSON.stringify(measuredZeroDebt)}`,
  );
}

/* --- 8. Gate status -------------------------------------------------- */

const everythingMeasuredPass =
  measuredZeroDebt.rosterEntriesNotFitting === 0 &&
  measuredZeroDebt.missingRosterEntries === 0 &&
  measuredZeroDebt.extraRosterEntries === 0 &&
  measuredZeroDebt.unjustifiedExclusions === 0 &&
  measuredZeroDebt.staleBlueprintPins === 0 &&
  publishedRoster.length === derivedRosterNames.length &&
  derivedRosterNames.length > 0;

if (gate.gateStatus !== "PASS" && gate.gateStatus !== "BLOCKED") {
  fail("gateStatus must be measured as PASS or BLOCKED");
}
if (gate.gateStatus === "PASS" && !everythingMeasuredPass) {
  fail(
    "gateStatus is PASS while the roster, exclusions, or blueprint hash basis are not all measured clean",
  );
}
if (
  gate.gateStatus !== "PASS" &&
  everythingMeasuredPass &&
  errors.length === 0
) {
  fail(
    "every roster entry fits, the exclusion is justified, and the blueprint hash basis matches, but gateStatus is not PASS: the artifact is stale",
  );
}

if (errors.length > 0) {
  process.stderr.write(
    `Canonical V1 CG1 control-publication fit gate verification failed (${String(errors.length)}):\n`,
  );
  for (const error of errors) process.stderr.write(`- ${error}\n`);
  process.exit(1);
}

process.stdout.write(
  [
    "Canonical V1 CG1 control-publication fit gate verified.",
    `gateStatus: ${gate.gateStatus}`,
    `roster: ${String(publishedRoster.length)} validators, all fit: ${String(gate.rosterAllFit)}`,
    `exclusions: ${String(publishedExclusions.length)} (${publishedExclusions.map((entry) => entry.name).join(", ")})`,
    `blueprint: sha256 ${String(blueprintSha256)}, md5 ${String(blueprintMd5)}, ${String(blueprintValidatorCount)} validators, tracked in git: ${String(blueprintIsIndexed)}`,
    `waivers: ${String(gate.waivers.length)}; C70 snapshots: ${String(gate.createsC70Snapshots)}; live/readiness claims: ${String(gate.liveOrReadinessClaims)}`,
    "",
  ].join("\n"),
);
