import { expect } from "vitest";

import { type CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";

export const network = "Custom" as const;

export const REASON_ARM = "FieldItemWidthIllegal";

export const CATEGORY_ID = "00000021";

/**
 * Every place a prover-supplied value is authenticated on chain. Each is
 * mutated once against a real bound thread and must be refused by a
 * validator, not by a builder.
 */
export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "forced_leaf_header",
  "forced_leaf_membership",
  "forced_leaf_reason_coordinate",
  "forced_direction",
  "native_tx_source",
  "field_preimage_bytes",
  "field_certificate",
  "field_chunks",
  "successor_script",
] as const;

export const CANCELLABLE_STEPS = ["step-01", "step-02", "step-03"] as const;

export const coverage = createLifecycleCoverageRecorder();

export const record = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  // Chunk publications and reference-script publications run no script; every
  // transaction that does must have been evaluated locally.
  if (measurement.redeemerCount > 0) {
    expect(measurement.executionMemory, name).toBeGreaterThan(0n);
    expect(measurement.executionSteps, name).toBeGreaterThan(0n);
  }
};
