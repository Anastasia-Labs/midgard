#!/usr/bin/env node

import fs from "node:fs";

import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "../../../demo/midgard-core/dist/index.js";

const [generatedReportPath, fieldReportPath, orderReportPath] =
  process.argv.slice(2);
if (
  generatedReportPath === undefined ||
  fieldReportPath === undefined ||
  orderReportPath === undefined
) {
  throw new Error(
    "usage: verify-proof-v1-envelope.mjs <generated.json> <fields.json> <order.json>",
  );
}

const readJson = (reportPath) =>
  JSON.parse(fs.readFileSync(reportPath, "utf8"));

const generated = readJson(generatedReportPath);

const testsFrom = (report) =>
  report.modules.flatMap((module) => module.tests);

const fieldReport = readJson(fieldReportPath);
const orderReport = readJson(orderReportPath);
const fieldTests = testsFrom(fieldReport);
const orderTests = testsFrom(orderReport);
const fieldProofs = fieldTests.filter((test) =>
  /^maximum_profile_field_[0-8]_item_[0-9]+_chunk_[0-9]+_verifies_independently_on_l1$/u.test(
    test.title,
  ),
);
const expectedChunkProofs = generated.representativeChunkProofs;

if (fieldProofs.length !== expectedChunkProofs) {
  throw new Error(
    `V1 envelope gate expected ${expectedChunkProofs.toString()} field-chunk proofs, found ${fieldProofs.length.toString()}`,
  );
}
if (
  !fieldTests.some(
    (test) =>
      test.title ===
      "receipt_publication_without_the_referenced_fragment_fails_closed",
  )
) {
  throw new Error(
    "V1 envelope gate is missing its absent-fragment rejection",
  );
}

const memoryCeiling = Math.floor(
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits * 0.8,
);
const cpuCeiling = Math.floor(
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits * 0.8,
);
for (const test of fieldProofs) {
  if (
    test.execution_units.mem > memoryCeiling ||
    test.execution_units.cpu > cpuCeiling
  ) {
    throw new Error(
      `${test.title} exceeds the V1 20% execution reserve: mem=${test.execution_units.mem.toString()},cpu=${test.execution_units.cpu.toString()}`,
    );
  }
}

const maxFieldMemory = Math.max(
  ...fieldProofs.map((test) => test.execution_units.mem),
);
const maxFieldCpu = Math.max(
  ...fieldProofs.map((test) => test.execution_units.cpu),
);
if (
  maxFieldMemory !==
    MIDGARD_V1_ENVELOPE_MEASUREMENTS
      .maxFieldChunkReceiptPublicationMemoryUnits ||
  maxFieldCpu !==
    MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxFieldChunkReceiptPublicationCpuUnits
) {
  throw new Error(
    `V1 field execution measurements drifted: mem=${maxFieldMemory.toString()},cpu=${maxFieldCpu.toString()}`,
  );
}

const orderVerification = fieldTests.find(
  (test) =>
    test.title ===
    "maximum_profile_terminal_receipt_authenticates_complete_material_chain",
);
if (orderVerification === undefined) {
  throw new Error("V1 envelope gate is missing final receipt verification");
}
if (
  orderVerification.execution_units.mem !==
    MIDGARD_V1_ENVELOPE_MEASUREMENTS
      .canonicalReceiptOrderVerificationMemoryUnits ||
  orderVerification.execution_units.cpu !==
    MIDGARD_V1_ENVELOPE_MEASUREMENTS
      .canonicalReceiptOrderVerificationCpuUnits
) {
  throw new Error(
    `V1 order receipt measurements drifted: mem=${orderVerification.execution_units.mem.toString()},cpu=${orderVerification.execution_units.cpu.toString()}`,
  );
}

if (
  generated.canonicalTransactionBytes >
  MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes
) {
  throw new Error("generated canonical transaction exceeds the derived profile");
}

process.stdout.write(
  `${JSON.stringify(
    {
      status: "pass",
      canonicalTransactionBytes: generated.canonicalTransactionBytes,
      maximumCanonicalTransactionBytes:
        MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes,
      maximumFieldProof: {
        memoryUnits: maxFieldMemory,
        cpuUnits: maxFieldCpu,
      },
      finalOrderReceiptVerification: {
        memoryUnits: orderVerification.execution_units.mem,
        cpuUnits: orderVerification.execution_units.cpu,
      },
      executionReserveCeilings: {
        memoryUnits: memoryCeiling,
        cpuUnits: cpuCeiling,
      },
    },
    null,
    2,
  )}\n`,
);
