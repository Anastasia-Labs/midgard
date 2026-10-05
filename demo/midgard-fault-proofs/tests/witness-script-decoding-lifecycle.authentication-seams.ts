import { fileURLToPath } from "node:url";

import { type MidgardNativeTxFull } from "@al-ft/midgard-core/codec";
import { expect } from "vitest";

import { type VanRossemFitMeasurement } from "../src/proof-fit/van-rossem-fit-ledger.js";
import { type CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  DEEP_MAXIMUM_DEPTH,
  type WitnessSetCarriage,
} from "./support/witness-script-decoding-raw.js";

export const CATEGORY_ID = "00000022";

export const REASON_ARMS = [
  "WitnessScriptHeaderMalformed",
  "WitnessNativeScriptMalformed",
  "WitnessNativeScriptNodeLimit",
  "WitnessNativeScriptDepthLimit",
] as const;

export type ReasonArm = (typeof REASON_ARMS)[number];

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
  "forced_direction",
  "bound_witness_set_hash",
  "bound_accused_class",
  "script_coordinate",
  "successor_script",
  "native_tx_source",
  "witness_set",
  "field_raw_utxo",
  "field_certificate",
  "field_chunks",
  "item_commitment",
  "scan_chunk",
  "scan_control",
  "scan_frame",
  "scan_budget",
  "scan_checkpoint",
] as const;

export const CANCELLABLE_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
] as const;

/**
 * The deep shape is 1,369 sixteen-step scan transactions at the field
 * bound; a shallower depth can be requested for a quick local pass, but the
 * ledger is written only at the maximum.
 */
export const DEEP_DEPTH = Number(
  process.env.MIDGARD_WSD_DEEP_DEPTH ?? DEEP_MAXIMUM_DEPTH,
);

export const WRITE_LEDGER = process.env.MIDGARD_WRITE_FIT_LEDGER === "1";

export const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/witness-script-decoding-v1-fit-ledger.json",
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
  if (measurement.redeemerCount > 0) {
    expect(measurement.executionMemory, name).toBeGreaterThan(0n);
    expect(measurement.executionSteps, name).toBeGreaterThan(0n);
  }
  expect(measurement.executionMemory, name).toBeLessThanOrEqual(16_500_000n);
  expect(measurement.executionSteps, name).toBeLessThanOrEqual(10_000_000_000n);
  measurements.push({
    name,
    kind,
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

export type Shape = Readonly<{
  label: string;
  /** Field 6: the transaction's script-witness items, in order. */
  items: readonly Buffer[];
  nativeTx: MidgardNativeTxFull;
  txId: string;
  carriage: WitnessSetCarriage;
  fieldBytes: number;
  certified: boolean;
}>;
