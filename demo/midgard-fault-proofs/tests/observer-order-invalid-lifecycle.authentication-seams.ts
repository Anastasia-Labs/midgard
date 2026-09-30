import { fileURLToPath } from "node:url";

import { expect } from "vitest";

import { OBSERVER_ORDER_INVALID_ITEM_BUDGET } from "../src/observer-order-invalid/family.js";
import { type VanRossemFitMeasurement } from "../src/proof-fit/van-rossem-fit-ledger.js";
import { type CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";

export const network = "Custom" as const;

export const REASON_ARM = "ObserverOrderInvalid";

export const CATEGORY_ID = "00000025";

/**
 * The largest field 3 the §5.4 aggregate field bound admits: a three-byte
 * array header plus 1,092 fixed-stride 30-byte items is 32,763 of the
 * 32,768 admissible bytes, carried as three certified chunks. One more item
 * is not an encodable transaction.
 */
export const MAXIMUM_OBSERVERS = 1092;

export const MAXIMUM_FIELD_BYTES = 32_763;

export const MAXIMUM_CHUNKS = 3;

export const LAST_ORDINAL = MAXIMUM_OBSERVERS - 1;

export const MAXIMUM_SCANS = Math.ceil(
  MAXIMUM_OBSERVERS / OBSERVER_ORDER_INVALID_ITEM_BUDGET,
);

/**
 * Every place a prover-supplied value is authenticated on chain. Each is
 * mutated once against a real bound thread and must be refused by a
 * validator, not by a builder.
 */
export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "forced_leaf_header",
  "forced_leaf_membership",
  "forced_leaf_reason",
  "forced_reason_coordinate",
  "forced_subject_transaction",
  "forced_direction",
  "successor_script",
  "native_tx_source",
  "field_raw_utxo",
  "field_certificate",
  "field_chunks",
  "scan_checkpoint",
  "scan_successor_state",
  "scan_wrong_successor",
  "scan_budget",
  "premature_decision",
  "decision_polarity",
] as const;

export type Seam = (typeof AUTHENTICATION_SEAMS)[number];

export const CANCELLABLE_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
] as const;

export const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/observer-order-invalid-v1-fit-ledger.json",
    import.meta.url,
  ),
);

export const coverage = createLifecycleCoverageRecorder();

export const measurements: VanRossemFitMeasurement[] = [];

export const record = (
  name: string,
  maximumShape: string,
  measurement: CompleteSignedTransactionMeasurement,
  kind: VanRossemFitMeasurement["kind"] = "lifecycle",
): void => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  // Chunk publications and reference-script publications run no script; every
  // transaction that does must have been evaluated locally.
  if (measurement.redeemerCount > 0) {
    expect(measurement.executionMemory, name).toBeGreaterThan(0n);
    expect(measurement.executionSteps, name).toBeGreaterThan(0n);
  }
  measurements.push({
    name,
    kind,
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

export const scanLabel = (prefix: string, ordinal: number) =>
  `${prefix}-step03-scan${(ordinal + 1).toString().padStart(2, "0")}`;
