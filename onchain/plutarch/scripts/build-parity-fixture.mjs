#!/usr/bin/env node

// Test fixture only. Unported families retain target Aiken code, so this file
// must never be used as a deployment blueprint or evidence of whole-tree parity.
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import {
  buildOffchainBlueprint,
  offchainTitleForGeneratedFile,
} from "./build-offchain-blueprint.mjs";

const [targetPath, generatedDir, outputPath, ...families] =
  process.argv.slice(2);
if (!targetPath || !generatedDir || !outputPath || families.length === 0) {
  throw new Error(
    "Usage: build-parity-fixture.mjs TARGET_AIKEN_JSON TESTNET_GENERATED_DIR OUTPUT_JSON FAMILY...",
  );
}
const targetBytes = await readFile(targetPath);
const target = JSON.parse(targetBytes);
const portedPath = `${outputPath}.plutarch.json`;
await buildOffchainBlueprint({ generatedDir, outputPath: portedPath });
const ported = JSON.parse(await readFile(portedPath, "utf8"));
const operationalFiles = [
  ...[
    "state-queue",
    "availability-challenge",
    "da-attestation",
    "settlement",
    "active-operators",
    "retired-operators",
  ].flatMap((family) =>
    ["mint", "spend"].map((purpose) => `${family}-${purpose}`),
  ),
  "correction-lock-spend",
  ...[
    "commit",
    "remove-unattested",
    "remove-unavailable",
    "remove-fraudulent",
    "merge",
  ].map((arm) => `state-queue-yield-${arm}`),
  ...["bond", "open", "settle", "close", "timeout"].map(
    (arm) => `availability-challenge-yield-${arm}`,
  ),
];
const outputProofFiles = [
  "fraud-proof-validation-trace-ledger-output-descriptor-datum-summary-yield",
  "fraud-proof-validation-trace-ledger-output-descriptor-reference-script-yield",
  "fraud-proof-validation-trace-ledger-output-descriptor-scan-facts-yield",
  "fraud-proof-validation-trace-ledger-output-descriptor-value-summary-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-advance-bytes-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-advance-integer-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-attach-bytes-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-attach-integer-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-close-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-finalize-frame-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-finish-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-fold-list-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-fold-map-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-head-large-constructor-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-head-map-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-head-scalar-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-head-sequence-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-large-constructor-yield",
  "fraud-proof-validation-trace-ledger-output-proof-datum-large-fields-yield",
  "fraud-proof-validation-trace-ledger-output-proof-native-script-yield",
  "fraud-proof-validation-trace-ledger-output-proof-reference-script-yield",
  "fraud-proof-validation-trace-ledger-output-proof-scalar-bytes-yield",
  "fraud-proof-validation-trace-ledger-output-proof-scalar-integer-yield",
  "fraud-proof-validation-trace-ledger-output-proof-script-hash-yield",
  "fraud-proof-validation-trace-ledger-output-proof-span-yield",
  "fraud-proof-validation-trace-ledger-output-proof-structure-assets-yield",
  "fraud-proof-validation-trace-ledger-output-proof-structure-finish-yield",
  "fraud-proof-validation-trace-ledger-output-proof-structure-optional-yield",
  "fraud-proof-validation-trace-ledger-output-proof-structure-yield",
  "fraud-proof-validation-trace-ledger-output-proof-value-yield",
  "fraud-proof-validation-trace-resolve-inputs-membership-step-semantic-v1",
  "fraud-proof-validation-trace-resolve-inputs-membership-finalize-semantic-v1",
  "fraud-proof-validation-trace-script-sources-output-proof-step-semantic-v1",
  "fraud-proof-validation-trace-script-sources-output-proof-finalize-semantic-v1",
];
const directFamilies = new Map([
  ["zero-input", 2],
  ["invalid-range", 2],
  ["min-fee", 2],
  ["invalid-signature", 2],
  ["field-preimage-length-mismatch", 4],
  ["field-item-width-illegal", 3],
  ["observers-forbidden-on-untagged-network", 2],
  ["observer-order-invalid", 4],
  ["redeemer-canonicity", 3],
  ["distinct-asset-accumulation-limit", 6],
  ["mint-declared-asset-limit", 4],
  ["witness-script-decoding", 4],
  ["output-reference-script-decoding", 6],
  ["execution-source-script-decoding", 5],
  ["execution-native-script-invalid", 13],
  ["missing-redeemer", 7],
  ["missing-script-source", 6],
  ["script-integrity-hash-missing", 7],
  ["unused-redeemer", 9],
  ["unused-script-witness", 6],
  ["protected-output-signer-missing", 5],
  ["resolved-output-non-canonical", 5],
  ["spend-input-signer-missing", 5],
  ["receive-purpose-language", 3],
  ["script-integrity-hash-mismatch", 5],
  ["no-input", 4],
  ["no-reference-input", 4],
  ["input-set-uniqueness", 4],
  ["missing-signature", 4],
  ["cross-block-duplicate-event", 2],
  ["native-script-invalid", 5],
  ["network-id", 2],
  ["min-ada", 5],
  ["withdrawal-mistag", 5],
  ["mint-authorization", 5],
  ["value-not-preserved", 4],
  ["transaction-output-non-canonical", 4],
]);
assert.equal(new Set(families).size, families.length, "Repeated family");
const files = families.flatMap((family) => {
  if (family === "execution-native-script-invalid")
    return Array.from({ length: 6 }, (_, index) =>
      String(index + 1).padStart(2, "0"),
    )
      .map((step) => `fraud-proof-execution-native-script-invalid-step-${step}`)
      .concat(
        [
          "accepted-reconstruction-init",
          "accepted-spend-prefix",
          "accepted-mint-prefix",
          "accepted-observer-prefix",
          "accepted-receive-prefix",
          "accepted-inline-source",
          "accepted-reference-source",
        ].map(
          (stage) => `fraud-proof-execution-native-script-invalid-${stage}`,
        ),
      );
  if (family === "field-preimage-length-mismatch")
    return [
      "fraud-proof-field-preimage-length-mismatch-step-01",
      "fraud-proof-field-preimage-length-mismatch-step-02-accepted",
      "fraud-proof-field-preimage-length-mismatch-step-02-forced",
      "fraud-proof-field-preimage-length-mismatch-step-03",
    ];
  if (family === "script-sources-stage-seven")
    return [
      "fraud-proof-validation-trace-script-sources-stage-seven-observer-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-seven-receive-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-seven-finish-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-seven-observer-item-yield-v1",
      "fraud-proof-validation-trace-script-sources-stage-seven-observer-bound-yield-v1",
    ];
  if (family === "script-sources-late")
    return [
      "fraud-proof-validation-trace-script-sources-stage-eight-finish-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-eight-purpose-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-nine-missing-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-nine-mismatch-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-nine-native-match-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-nine-effectful-match-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-ten-missing-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-ten-match-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-ten-mismatch-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-eleven-finish-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-eleven-source-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-twelve-finish-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-twelve-redeemer-semantic-v1",
      "fraud-proof-validation-trace-script-sources-redeemer-item-step-yield-v1",
    ];
  if (family === "value-and-mint")
    return [
      "v1",
      "begin-semantic-v1",
      "replay-begin-semantic-v1",
      "replay-input-semantic-v1",
      "replay-asset-semantic-v1",
      "replay-finish-semantic-v1",
      "output-descriptor-semantic-v1",
      "output-asset-semantic-v1",
      "output-finish-semantic-v1",
      "mint-asset-semantic-v1",
      "mint-finish-semantic-v1",
      "finalize-semantic-v1",
    ]
      .map((name) => `fraud-proof-validation-trace-value-and-mint-${name}`)
      .concat("fraud-proof-validation-trace-value-and-mint-asset-fold-yield");
  if (family === "script-sources-output")
    return ["begin", "finish"].map(
      (name) =>
        `fraud-proof-validation-trace-script-sources-output-proof-${name}-semantic-v1`,
    );
  if (
    family === "script-sources-output" ? 2 : family === "script-sources-middle"
  )
    return [
      "fraud-proof-validation-trace-script-sources-middle-stage-two-advance-yield",
      "fraud-proof-validation-trace-script-sources-middle-stage-three-replay-yield",
      "fraud-proof-validation-trace-script-sources-middle-stage-three-finish-yield",
      "fraud-proof-validation-trace-script-sources-middle-stage-four-begin-yield",
      "fraud-proof-validation-trace-script-sources-middle-stage-four-finish-yield",
      "fraud-proof-validation-trace-script-sources-middle-stage-six-begin-policy-yield",
      "fraud-proof-validation-trace-script-sources-middle-stage-six-fold-asset-yield",
      "fraud-proof-validation-trace-script-sources-middle-stage-six-finish-yield",
      "fraud-proof-validation-trace-script-sources-non-output-semantic-v1",
    ];
  if (
    family === "script-sources-output"
      ? 2
      : family === "script-sources-middle"
        ? 9
        : family === "resolve-inputs"
  )
    return [
      "v1",
      "initial-semantic-v1",
      "finish-semantic-v1",
      "membership-begin-semantic-v1",
      "non-membership-semantic-v1",
    ].map((name) => `fraud-proof-validation-trace-resolve-inputs-${name}`);
  if (
    family === "script-sources-output"
      ? 2
      : family === "script-sources-middle"
        ? 9
        : family === "resolve-inputs"
          ? 5
          : family === "native-descriptors"
  )
    return [
      "fraud-proof-validation-trace-native-scripts-effectful-semantic-v1",
      "fraud-proof-validation-trace-native-scripts-native-semantic-v1",
      "fraud-proof-validation-trace-native-scripts-terminal-semantic-v1",
      "fraud-proof-validation-trace-native-scripts-v1",
    ];
  if (family === "phase-a-native")
    return [
      "fraud-proof-validation-trace-phase-a-native-scripts-advance-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-all-or-any-container-frame-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-all-or-any-empty-container-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-at-least-container-frame-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-at-least-empty-container-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-frame-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-item-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-item-foreign-yield",
      "fraud-proof-validation-trace-phase-a-native-scripts-item-native-yield",
      "fraud-proof-validation-trace-phase-a-native-scripts-signature-above-last-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-signature-below-first-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-signature-empty-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-signature-membership-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-timelock-payload-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-token-head-semantic-v1",
      "fraud-proof-validation-trace-phase-a-native-scripts-v1",
    ];
  if (family === "phase-a-preconditions")
    return [
      "fraud-proof-validation-trace-phase-a-script-preconditions-v1",
      "fraud-proof-validation-trace-phase-a-script-preconditions-semantic-v1",
      "fraud-proof-validation-trace-phase-a-script-preconditions-item-semantic-v1",
    ];
  if (family === "signatures-advance-handoff")
    return [
      "fraud-proof-validation-trace-signatures-advance-semantic-v1",
      "fraud-proof-validation-trace-signatures-handoff-semantic-v1",
    ];
  if (family === "signatures-required-item")
    return [
      "fraud-proof-validation-trace-signatures-required-item-semantic-v1",
    ];
  if (family === "shared-item")
    return [
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-semantic-v1",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-envelope-v1",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-traversal-normalizer-v1",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-outer-normalizer-v1",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-fold-map-executor-v1",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-finalize-frame-executor-v1",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-execution-settlement-v1",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-cek-envelope",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-cek-settlement",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-source-authenticator",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-open-header-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-open-tail-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-scalar-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-sequence-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-map-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-large-constructor-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-attach-integer-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-attach-bytes-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-fold-list-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-integer-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-bytes-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-large-constructor-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-large-fields-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-close-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-finish-data-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-invalid-header-executor",
      "fraud-proof-validation-trace-script-sources-stage-one-redeemer-invalid-tail-executor",
    ];
  if (family === "cek-context")
    return [
      "fraud-proof-validation-trace-cek-context-step-semantic-v1",
      "fraud-proof-validation-trace-cek-context-control",
      "fraud-proof-validation-trace-cek-context-settle",
      "fraud-proof-validation-trace-cek-context-reference",
      "fraud-proof-validation-trace-cek-context-spend",
      "fraud-proof-validation-trace-cek-context-output",
      "fraud-proof-validation-trace-cek-context-signer",
      "fraud-proof-validation-trace-cek-context-mint-init",
      "fraud-proof-validation-trace-cek-context-mint-item",
      "fraud-proof-validation-trace-cek-context-assemble",
      "fraud-proof-validation-trace-cek-context-tx-info",
      "fraud-proof-validation-trace-cek-context-seed",
      "fraud-proof-validation-trace-cek-context-redeemer-begin",
      "fraud-proof-validation-trace-cek-context-redeemer-select-authenticate",
      "fraud-proof-validation-trace-cek-context-redeemer-select-initialize",
      "fraud-proof-validation-trace-cek-context-redeemer-select-hash",
      "fraud-proof-validation-trace-cek-context-redeemer-select-finish",
      "fraud-proof-validation-trace-cek-context-finalize-authenticate",
      "fraud-proof-validation-trace-cek-context-finalize-spend",
      "fraud-proof-validation-trace-cek-context-finalize-mint",
      "fraud-proof-validation-trace-cek-context-finalize-withdraw",
      "fraud-proof-validation-trace-cek-context-finalize-observe",
      "fraud-proof-validation-trace-cek-context-finalize-midgard",
      "fraud-proof-validation-trace-cek-context-observer-authenticate",
      "fraud-proof-validation-trace-cek-context-observer-fold",
      "fraud-proof-validation-trace-cek-context-item-bind",
      "fraud-proof-validation-trace-cek-context-item-return",
      "fraud-proof-validation-trace-cek-context-item-hash",
      "fraud-proof-validation-trace-cek-context-item-finalize",
      "fraud-proof-validation-trace-cek-context-item-selection-continue",
      "fraud-proof-validation-trace-cek-context-item-selection-finish",
      "fraud-proof-validation-trace-cek-context-item-data-continue",
      "fraud-proof-validation-trace-cek-context-item-data-finish-descriptor",
      "fraud-proof-validation-trace-cek-context-item-data-finish-value",
    ];
  if (family === "cek-selection")
    return [
      "fraud-proof-validation-trace-cek-execution-selection-semantic-v1",
      "fraud-proof-validation-trace-cek-execution-selection-authenticate-yield",
      "fraud-proof-validation-trace-cek-execution-selection-successor-yield",
      "fraud-proof-validation-trace-cek-execution-selection-material-program-yield",
      "fraud-proof-validation-trace-cek-execution-selection-material-data-yield",
    ];
  if (family === "cek-core-boundaries")
    return [
      "fraud-proof-validation-trace-cek-core-step-semantic-v1",
      "fraud-proof-validation-trace-cek-core-settle",
    ];
  if (family === "cek-core")
    return [
      "fraud-proof-validation-trace-cek-core-arm-compute",
      "fraud-proof-validation-trace-cek-core-builtin-roots",
      "fraud-proof-validation-trace-cek-core-semantic-result",
      "fraud-proof-validation-trace-cek-core-builtin-budget",
      "fraud-proof-validation-trace-cek-core-direct-scalar",
      "fraud-proof-validation-trace-cek-core-direct-structured",
      "fraud-proof-validation-trace-cek-core-arm-machine",
      "fraud-proof-validation-trace-cek-core-arm-map-conversion",
      "fraud-proof-validation-trace-cek-core-semantic-pair",
      "fraud-proof-validation-trace-cek-core-semantic-list-construct",
      "fraud-proof-validation-trace-cek-core-semantic-list-select",
      "fraud-proof-validation-trace-cek-core-semantic-choose",
      "fraud-proof-validation-trace-cek-core-semantic-data-construct",
      "fraud-proof-validation-trace-cek-core-semantic-data-scalar",
      "fraud-proof-validation-trace-cek-core-semantic-data-misc",
      "fraud-proof-validation-trace-cek-core-failure-known",
      "fraud-proof-validation-trace-cek-core-failure-budget",
      "fraud-proof-validation-trace-cek-core-semantic-failure-roots",
      "fraud-proof-validation-trace-cek-core-semantic-failure-material",
      "fraud-proof-validation-trace-cek-core-type-failure-roots",
      "fraud-proof-validation-trace-cek-core-type-failure-kinds",
      "fraud-proof-validation-trace-cek-core-bls-budget",
      "fraud-proof-validation-trace-cek-core-bls-roots",
      "fraud-proof-validation-trace-cek-core-bls-final",
      "fraud-proof-validation-trace-cek-core-map-start-roots",
      "fraud-proof-validation-trace-cek-core-map-start-budget",
      "fraud-proof-validation-trace-cek-core-map-start-nodes",
      "fraud-proof-validation-trace-cek-core-settle",
      "fraud-proof-validation-trace-cek-core-step-semantic-v1",
    ];
  if (family === "cek-material-traversal")
    return [
      "fraud-proof-validation-trace-cek-material-traversal-v1",
      "fraud-proof-validation-trace-cek-material-traversal-program-yield",
      "fraud-proof-validation-trace-cek-material-traversal-data-yield",
    ];
  if (family === "operational") return operationalFiles;
  if (family === "ledger-output-proof") return outputProofFiles;
  if (family === "transition-trace")
    return [
      "fraud-proof-transition-trace-control-v1",
      "fraud-proof-transition-trace-source-v1",
      "fraud-proof-transition-trace-withdrawal-v1",
      "fraud-proof-transition-trace-forced-v1",
      "fraud-proof-transition-trace-accepted-transaction-v1",
      "fraud-proof-transition-trace-deposit-v1",
      "fraud-proof-transition-trace-l1-event-v1",
      "fraud-proof-transition-trace-duplicate-v1",
      "fraud-proof-transition-trace-route-v1",
      "fraud-proof-transition-trace-l2-open",
      "fraud-proof-transition-trace-l2-replay",
      "fraud-proof-transition-trace-claim-structure",
      "fraud-proof-transition-trace-claim-source",
      "fraud-proof-transition-trace-claim-endpoints",
      "fraud-proof-transition-trace-output-scan",
      "fraud-proof-transition-trace-output-value",
      "fraud-proof-transition-trace-output-summaries",
      "fraud-proof-transition-trace-output-assembly",
      "fraud-proof-transition-trace-deposit-projection",
      "fraud-proof-transition-trace-deposit-value",
      "fraud-proof-transition-trace-deposit-summaries",
    ];
  if (family === "missing-redeemer")
    return ["01", "02", "02a", "02b", "03", "04", "05"].map(
      (step) => `fraud-proof-${family}-step-${step}`,
    );
  if (family === "script-integrity-hash-missing")
    return [
      "fraud-proof-script-integrity-hash-missing-step-01",
      "fraud-proof-script-integrity-hash-missing-step-02",
      "fraud-proof-script-integrity-hash-missing-step-03",
      "fraud-proof-script-integrity-hash-missing-script-grammar",
      "fraud-proof-script-integrity-hash-missing-script-scan",
      "fraud-proof-script-integrity-hash-missing-redeemer-grammar",
      "fraud-proof-script-integrity-hash-missing-step-04",
    ];
  if (family === "unused-redeemer")
    return ["01", "02", "02a", "02b", "02c", "03", "04", "05", "06"].map(
      (step) => `fraud-proof-${family}-step-${step}`,
    );
  assert(directFamilies.has(family), `Unreviewed family: ${family}`);
  return Array.from({ length: directFamilies.get(family) }, (_, index) =>
    String(index + 1).padStart(2, "0"),
  )
    .map((step) => `fraud-proof-${family}-step-${step}`)
    .concat(
      family === "value-not-preserved"
        ? [
            "accepted-source",
            "forced-source",
            "event",
            "pre-state",
            "inputs",
            "input-value",
            "assets",
            "field-grammar",
            "outputs",
            "output-scan",
            "mint",
            "update",
            "terminal",
          ].map((stage) => `fraud-proof-value-not-preserved-union-${stage}`)
        : family === "mint-authorization"
          ? ["evaluate", "witness-scan"].map(
              (stage) => `fraud-proof-mint-authorization-${stage}`,
            )
          : family === "missing-signature"
            ? ["forced-step", "forced-signer", "forced-witness"].map(
                (stage) => `fraud-proof-missing-signature-${stage}`,
              )
            : family === "min-ada"
              ? ["tx-yield", "utxo-yield"].map(
                  (stage) => `fraud-proof-min-ada-${stage}`,
                )
              : family === "network-id"
                ? ["forced-step", "forced-scan"].map(
                    (stage) => `fraud-proof-network-id-${stage}`,
                  )
                : [],
    );
});
const expectedParameterChecks = families.reduce(
  (total, family) =>
    total +
    (family === "script-sources-stage-seven"
      ? 5
      : family === "script-sources-late"
        ? 14
        : family === "value-and-mint"
          ? 13
          : family === "script-sources-output"
            ? 2
            : family === "script-sources-middle"
              ? 9
              : family === "resolve-inputs"
                ? 5
                : family === "native-descriptors"
                  ? 4
                  : family === "phase-a-native"
                    ? 17
                    : family === "phase-a-preconditions"
                      ? 3
                      : family === "signatures-advance-handoff"
                        ? 2
                        : family === "signatures-required-item"
                          ? 1
                          : family === "shared-item"
                            ? 27
                            : family === "cek-context"
                              ? 34
                              : family === "cek-selection"
                                ? 5
                                : family === "cek-core-boundaries"
                                  ? 2
                                  : family === "cek-core"
                                    ? 29
                                    : family === "cek-material-traversal"
                                      ? 3
                                      : family === "ledger-output-proof"
                                        ? 34
                                        : family === "operational"
                                          ? 14
                                          : family === "transition-trace"
                                            ? 21
                                            : family === "value-not-preserved"
                                              ? 17
                                              : family ===
                                                    "missing-signature" ||
                                                  family ===
                                                    "mint-authorization"
                                                ? 7
                                                : family === "min-ada"
                                                  ? 7
                                                  : family === "network-id"
                                                    ? 4
                                                    : directFamilies.get(
                                                        family,
                                                      )),
  0,
);
let parameterChecks = 0;
const replacements = [];
for (const name of files) {
  const file = `${name}.unapplied.plutus.json`;
  const title = offchainTitleForGeneratedFile(file);
  const original = target.validators.find((entry) => entry.title === title);
  const replacement = ported.validators.find((entry) => entry.title === title);
  assert(original && replacement, `Missing validator: ${title}`);
  if (replacement.parameters) {
    assert.deepEqual(
      replacement.parameters.map((parameter) => parameter.title),
      original.parameters.map((parameter) => parameter.title),
      title,
    );
    parameterChecks++;
  }
  original.compiledCode = replacement.compiledCode;
  delete original.hash;
  replacements.push({
    title,
    artifact: path.resolve(generatedDir, file),
    bytes: replacement.compiledCode.length / 2,
    sha256: createHash("sha256")
      .update(Buffer.from(replacement.compiledCode, "hex"))
      .digest("hex"),
  });
}
assert.equal(parameterChecks, expectedParameterChecks);
assert.equal(replacements.length, files.length);
target.preamble.description = `TEST FIXTURE ONLY: ${replacements.length} testnet Plutarch scripts (${families.join(", ")}); other families remain target Aiken. Not a deployment blueprint.`;
await writeFile(outputPath, `${JSON.stringify(target)}\n`);
const evidence = {
  target: path.resolve(targetPath),
  targetSha256: createHash("sha256").update(targetBytes).digest("hex"),
  parameterChecks,
  families,
  replacements,
};
await writeFile(
  `${outputPath}.manifest.json`,
  `${JSON.stringify(evidence, null, 2)}\n`,
);
console.log(
  `Wrote test fixture ${outputPath}: ${replacements.length} Plutarch replacements, ${parameterChecks} parameter lists checked.`,
);
