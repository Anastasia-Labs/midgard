import { fileURLToPath } from "node:url";

import { expect } from "vitest";

import { MIDGARD_ABSENT_SCRIPT_INTEGRITY_HASH } from "../src/observers-forbidden-on-untagged-network/family.js";
import { type VanRossemFitMeasurement } from "../src/proof-fit/van-rossem-fit-ledger.js";
import { type CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";

export const network = "Custom" as const;

export const REASON_ARM = "ObserversForbiddenOnUntaggedNetwork";

export const CATEGORY_ID = "00000024";

/** A present integrity hash: the phase-A observer arm is reachable. */
export const PRESENT_HASH = "ab".repeat(32);

export const ABSENT_HASH = MIDGARD_ABSENT_SCRIPT_INTEGRITY_HASH;

/**
 * The observer frontier the shared bounded field surface accepts: a
 * three-byte array header plus 505 canonical 28-byte hashes, 15,153 bytes,
 * which forces certified carriage over two chunks.
 */
export const MAXIMUM_OBSERVERS = 505;

export const MAXIMUM_FIELD_BYTES = 15_153;

export const MAXIMUM_CHUNKS = 2;

/**
 * Every place a prover-supplied value is authenticated on chain. Each is
 * mutated once against a real bound thread and must be refused by a
 * validator, not by a builder.
 */
export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "accepted_network_scalar",
  "forced_leaf_header",
  "forced_leaf_membership",
  "forced_leaf_reason",
  "forced_subject_transaction",
  "forced_direction",
  "successor_script",
  "native_tx_source",
  "field_raw_utxo",
  "field_certificate",
  "field_chunks",
] as const;

export const CANCELLABLE_STEPS = ["step-01", "step-02"] as const;

export const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/observers-forbidden-on-untagged-network-v1-fit-ledger.json",
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
