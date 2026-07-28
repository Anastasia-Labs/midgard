#!/usr/bin/env node

import { readdirSync, readFileSync, statSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const markdownPath = resolve(
  repoRoot,
  "docs/exec-plans/canonical-v1-format-registry.md",
);
const registryPath = resolve(
  repoRoot,
  "docs/exec-plans/evidence/canonical-v1-format-registry-v1.json",
);
const allowIncomplete = process.argv.includes("--allow-incomplete");
const printBootstrap = process.argv.includes("--print-bootstrap");

const markdown = readFileSync(markdownPath, "utf8");
const rowPattern = /^\|\s*((?:C|N|D|L|S|K|V|P|A)\d{2})\s*\|(.+)$/gm;

const normalizeCell = (value) =>
  value
    .trim()
    .replace(/\s+/g, " ")
    .replace(/^`|`$/g, "");

const markdownRows = [...markdown.matchAll(rowPattern)].map((match) => {
  const cells = match[2]
    .split("|")
    .map(normalizeCell)
    .filter((cell, index, all) => index < all.length - 1 || cell.length > 0);
  const classification =
    cells.find((cell) =>
      [
        "schema",
        "nested-schema",
        "semantic",
        "sentinel",
        "external",
        "artifact",
        "post-launch seam",
        "semantic/operational",
      ].includes(cell),
    ) ?? (match[1].startsWith("A") ? "artifact" : "UNVERIFIED");

  return {
    id: match[1],
    family: match[1][0],
    sourceOwnerAndCurrentIdentity: cells[0] ?? "UNVERIFIED",
    classification,
    canonicalResult: cells[2] ?? cells[1] ?? "UNVERIFIED",
    boundaryBindingAndPersistence: cells[3] ?? "UNVERIFIED",
    requiredEvidence: cells[4] ?? "UNVERIFIED",
    disposition: "UNVERIFIED",
    auditStatus: "UNVERIFIED",
    sourceEvidence: [],
    canonicalForms: [],
    positiveEvidence: [],
    rejectionEvidence: [],
    crossLanguageEvidence: {
      status: "UNVERIFIED",
      tests: [],
      notApplicableReason: null,
    },
    obsoleteBranchEvidence: [],
    notes: [],
  };
});

const expectedIds = markdownRows.map(({ id }) => id);
const bootstrapAbsenceScans = [
  {
    id: "C01-obsolete-consensus-profiles",
    paths: [
      "demo/da-committee-node/src",
      "demo/lucid-midgard/src",
      "demo/lucid-midgard/tests",
      "demo/midgard-core/src",
      "demo/midgard-core/tests",
      "demo/midgard-fault-proofs/src",
      "demo/midgard-fault-proofs/tests",
      "demo/midgard-node/src",
      "demo/midgard-node/tests",
      "demo/midgard-sdk/src",
      "demo/midgard-sdk/tests",
      "demo/midgard-validation/src",
      "demo/midgard-validation/tests",
    ],
    patterns: [
      "midgard-launch-consensus-v1",
      "midgard-proof-consensus-v3",
    ],
  },
  {
    id: "A22-retired-phase5-da-artifacts",
    paths: [
      "demo/midgard-node/package.json",
      "demo/midgard-node/scripts",
      "demo/midgard-node/src",
      "demo/midgard-node/tests",
    ],
    patterns: [
      "MIDGARD_DA_LARGE_OPERATIONAL_ENVELOPE",
      "MIDGARD_DA_LARGE_OPERATIONAL_REPORT",
      "midgard-phase-5-live-da-50k-v1",
      "midgard-phase-5-live-da-v1",
      "test:da-phase5-e2e",
    ],
  },
];

if (printBootstrap) {
  await new Promise((resolveWrite, rejectWrite) =>
    process.stdout.write(
      `${JSON.stringify(
        {
          schema: "midgard-canonical-v1-format-registry-v1",
          version: 1,
          source: "docs/exec-plans/canonical-v1-format-registry.md",
          unknownFormatPolicy: "reject",
          forbiddenActivePatterns: bootstrapAbsenceScans,
          formats: markdownRows,
        },
        null,
        2,
      )}\n`,
      (error) => (error == null ? resolveWrite() : rejectWrite(error)),
    ),
  );
  process.exit(0);
}

const errors = [];
const fail = (message) => errors.push(message);
const isNonEmptyString = (value) =>
  typeof value === "string" && value.trim().length > 0;
const requireNonEmptyString = (value, label) => {
  if (!isNonEmptyString(value)) {
    fail(`${label} must be a non-empty string`);
  }
};
const loadRepoText = (relativePath, label) => {
  if (
    !isNonEmptyString(relativePath) ||
    relativePath.startsWith("/") ||
    relativePath.includes("..")
  ) {
    fail(`${label} must be a repository-relative path without '..'`);
    return null;
  }
  try {
    return readFileSync(resolve(repoRoot, relativePath), "utf8");
  } catch {
    fail(`${label} does not exist: ${relativePath}`);
    return null;
  }
};
const verifyReferences = (references, label, contentField) => {
  if (!Array.isArray(references)) {
    fail(`${label} must be an array`);
    return;
  }
  for (const [index, reference] of references.entries()) {
    if (reference === null || typeof reference !== "object") {
      fail(`${label}[${index}] must be an object`);
      continue;
    }
    const text = loadRepoText(reference.path, `${label}[${index}].path`);
    const names = reference[contentField];
    if (!Array.isArray(names) || names.length === 0) {
      fail(`${label}[${index}].${contentField} must be a non-empty array`);
      continue;
    }
    for (const [nameIndex, name] of names.entries()) {
      requireNonEmptyString(
        name,
        `${label}[${index}].${contentField}[${nameIndex}]`,
      );
      if (text !== null && isNonEmptyString(name) && !text.includes(name)) {
        fail(
          `${label}[${index}] does not contain ${contentField} entry ${JSON.stringify(name)}: ${reference.path}`,
        );
      }
    }
  }
};
const activeSourceExtensions = new Set([
  ".cjs",
  ".js",
  ".json",
  ".mjs",
  ".sql",
  ".ts",
  ".tsx",
]);
const ignoredDirectoryNames = new Set([
  ".turbo",
  "coverage",
  "dist",
  "logs",
  "node_modules",
]);
const collectActiveFiles = (relativePath, label) => {
  if (
    !isNonEmptyString(relativePath) ||
    relativePath.startsWith("/") ||
    relativePath.includes("..")
  ) {
    fail(`${label} must be a repository-relative path without '..'`);
    return [];
  }
  const absolutePath = resolve(repoRoot, relativePath);
  let stat;
  try {
    stat = statSync(absolutePath);
  } catch {
    fail(`${label} does not exist: ${relativePath}`);
    return [];
  }
  if (stat.isFile()) {
    return [absolutePath];
  }
  if (!stat.isDirectory()) {
    fail(`${label} is neither a file nor a directory: ${relativePath}`);
    return [];
  }

  const files = [];
  const visit = (directory) => {
    for (const entry of readdirSync(directory, { withFileTypes: true })) {
      if (entry.isDirectory() && ignoredDirectoryNames.has(entry.name)) {
        continue;
      }
      const child = resolve(directory, entry.name);
      if (entry.isDirectory()) {
        visit(child);
      } else if (
        entry.isFile() &&
        activeSourceExtensions.has(entry.name.slice(entry.name.lastIndexOf(".")))
      ) {
        files.push(child);
      }
    }
  };
  visit(absolutePath);
  return files;
};
const passedAbsenceScans = new Set();
const verifyAbsenceScans = (scans) => {
  if (!Array.isArray(scans)) {
    fail("registry.forbiddenActivePatterns must be an array");
    return;
  }
  const seenScanIds = new Set();
  for (const [scanIndex, scan] of scans.entries()) {
    const label = `forbiddenActivePatterns[${scanIndex}]`;
    requireNonEmptyString(scan?.id, `${label}.id`);
    if (seenScanIds.has(scan?.id)) {
      fail(`${label}.id is duplicated: ${scan.id}`);
      continue;
    }
    seenScanIds.add(scan?.id);
    if (!Array.isArray(scan?.paths) || scan.paths.length === 0) {
      fail(`${label}.paths must be a non-empty array`);
      continue;
    }
    if (!Array.isArray(scan?.patterns) || scan.patterns.length === 0) {
      fail(`${label}.patterns must be a non-empty array`);
      continue;
    }
    const files = scan.paths.flatMap((path, pathIndex) =>
      collectActiveFiles(path, `${label}.paths[${pathIndex}]`),
    );
    let scanPassed = true;
    for (const [patternIndex, pattern] of scan.patterns.entries()) {
      requireNonEmptyString(pattern, `${label}.patterns[${patternIndex}]`);
      if (!isNonEmptyString(pattern)) {
        scanPassed = false;
        continue;
      }
      const expression = new RegExp(pattern, "u");
      for (const file of files) {
        const text = readFileSync(file, "utf8");
        const match = expression.exec(text);
        if (match !== null) {
          const line = text.slice(0, match.index).split("\n").length;
          fail(
            `${scan.id} found /${pattern}/ at ${relative(repoRoot, file)}:${line}`,
          );
          scanPassed = false;
        }
      }
    }
    if (scanPassed && isNonEmptyString(scan?.id)) {
      passedAbsenceScans.add(scan.id);
    }
  }
};
const verifyAbsenceEvidence = (references, label) => {
  if (!Array.isArray(references)) {
    fail(`${label} must be an array`);
    return;
  }
  for (const [index, reference] of references.entries()) {
    requireNonEmptyString(reference?.scanId, `${label}[${index}].scanId`);
    if (
      isNonEmptyString(reference?.scanId) &&
      !passedAbsenceScans.has(reference.scanId)
    ) {
      fail(`${label}[${index}] references an absent or failed scan`);
    }
  }
};

let registry;
try {
  registry = JSON.parse(readFileSync(registryPath, "utf8"));
} catch (error) {
  fail(`registry is missing or invalid JSON: ${error.message}`);
}

if (registry !== undefined) {
  if (registry.schema !== "midgard-canonical-v1-format-registry-v1") {
    fail("registry.schema must be midgard-canonical-v1-format-registry-v1");
  }
  if (registry.version !== 1) {
    fail("registry.version must be the number 1");
  }
  if (registry.unknownFormatPolicy !== "reject") {
    fail("registry.unknownFormatPolicy must be reject");
  }
  verifyAbsenceScans(registry.forbiddenActivePatterns);
  if (!Array.isArray(registry.formats)) {
    fail("registry.formats must be an array");
  } else {
    const actualIds = registry.formats.map(({ id }) => id);
    const duplicateIds = actualIds.filter(
      (id, index) => actualIds.indexOf(id) !== index,
    );
    if (duplicateIds.length > 0) {
      fail(`registry has duplicate IDs: ${[...new Set(duplicateIds)].join(", ")}`);
    }
    if (JSON.stringify(actualIds) !== JSON.stringify(expectedIds)) {
      const missing = expectedIds.filter((id) => !actualIds.includes(id));
      const extra = actualIds.filter((id) => !expectedIds.includes(id));
      fail(
        `registry IDs/order differ from Markdown (missing: ${missing.join(", ") || "none"}; extra: ${extra.join(", ") || "none"})`,
      );
    }

    for (const row of registry.formats) {
      const label = row.id ?? "<missing-id>";
      requireNonEmptyString(row.sourceOwnerAndCurrentIdentity, `${label}.source`);
      requireNonEmptyString(row.classification, `${label}.classification`);
      requireNonEmptyString(row.canonicalResult, `${label}.canonicalResult`);
      requireNonEmptyString(
        row.boundaryBindingAndPersistence,
        `${label}.boundaryBindingAndPersistence`,
      );
      requireNonEmptyString(row.requiredEvidence, `${label}.requiredEvidence`);

      if (allowIncomplete && row.auditStatus !== "PASS") {
        continue;
      }
      if (row.auditStatus !== "PASS") {
        fail(`${label}.auditStatus must be PASS`);
        continue;
      }
      if (JSON.stringify(row).includes("UNVERIFIED")) {
        fail(`${label} is PASS but still contains UNVERIFIED`);
      }
      if (
        !["retain", "reset-to-v1", "delete", "external", "semantic", "sentinel"].includes(
          row.disposition,
        )
      ) {
        fail(`${label}.disposition is not a final disposition`);
      }

      verifyReferences(row.sourceEvidence, `${label}.sourceEvidence`, "symbols");
      verifyReferences(
        row.positiveEvidence,
        `${label}.positiveEvidence`,
        "testNames",
      );
      verifyReferences(
        row.rejectionEvidence,
        `${label}.rejectionEvidence`,
        "testNames",
      );
      verifyAbsenceEvidence(
        row.obsoleteBranchEvidence,
        `${label}.obsoleteBranchEvidence`,
      );

      if (row.disposition === "delete") {
        if (!Array.isArray(row.canonicalForms) || row.canonicalForms.length !== 0) {
          fail(`${label}.canonicalForms must be empty for a deleted format`);
        }
        if (
          !Array.isArray(row.obsoleteBranchEvidence) ||
          row.obsoleteBranchEvidence.length === 0
        ) {
          fail(`${label} deletion requires obsoleteBranchEvidence`);
        }
      } else {
        if (!Array.isArray(row.sourceEvidence) || row.sourceEvidence.length === 0) {
          fail(`${label} requires sourceEvidence`);
        }
        if (!Array.isArray(row.canonicalForms) || row.canonicalForms.length === 0) {
          fail(`${label} requires at least one canonical form`);
        } else {
          for (const [index, form] of row.canonicalForms.entries()) {
            const formLabel = `${label}.canonicalForms[${index}]`;
            for (const field of [
              "name",
              "kind",
              "wireRepresentation",
              "versionPolicy",
              "boundary",
              "binding",
              "persistence",
            ]) {
              requireNonEmptyString(form[field], `${formLabel}.${field}`);
            }
            if (
              (!Array.isArray(form.exactFields) ||
                form.exactFields.length === 0) &&
              !isNonEmptyString(form.exactFieldsNotApplicable)
            ) {
              fail(
                `${formLabel} requires exactFields or exactFieldsNotApplicable`,
              );
            }
            if (
              (!Array.isArray(form.constructorTags) ||
                form.constructorTags.length === 0) &&
              !isNonEmptyString(form.constructorTagsNotApplicable)
            ) {
              fail(
                `${formLabel} requires constructorTags or constructorTagsNotApplicable`,
              );
            }
            if (
              !Number.isInteger(form.arrayArity) &&
              !isNonEmptyString(form.arrayArityNotApplicable)
            ) {
              fail(
                `${formLabel} requires integer arrayArity or arrayArityNotApplicable`,
              );
            }
            verifyReferences(
              form.encoders,
              `${formLabel}.encoders`,
              "symbols",
            );
            verifyReferences(
              form.parsers,
              `${formLabel}.parsers`,
              "symbols",
            );
          }
        }
        if (!Array.isArray(row.positiveEvidence) || row.positiveEvidence.length === 0) {
          fail(`${label} requires positiveEvidence`);
        }
        if (
          row.classification !== "external" &&
          (!Array.isArray(row.rejectionEvidence) ||
            row.rejectionEvidence.length === 0)
        ) {
          fail(`${label} requires rejectionEvidence`);
        }
      }

      const crossLanguage = row.crossLanguageEvidence;
      if (crossLanguage?.status !== "PASS") {
        fail(`${label}.crossLanguageEvidence.status must be PASS`);
      } else if (
        Array.isArray(crossLanguage.tests) &&
        crossLanguage.tests.length > 0
      ) {
        verifyReferences(
          crossLanguage.tests,
          `${label}.crossLanguageEvidence.tests`,
          "testNames",
        );
      } else if (!isNonEmptyString(crossLanguage.notApplicableReason)) {
        fail(
          `${label}.crossLanguageEvidence needs tests or a non-empty notApplicableReason`,
        );
      }
    }
  }
}

if (errors.length > 0) {
  process.stderr.write(
    `Canonical V1 format registry verification failed (${errors.length}):\n`,
  );
  for (const error of errors) {
    process.stderr.write(`- ${error}\n`);
  }
  process.exit(1);
}

process.stdout.write(
  `Canonical V1 format registry verified: ${expectedIds.length} rows${allowIncomplete ? " (incomplete rows allowed)" : ""}.\n`,
);
