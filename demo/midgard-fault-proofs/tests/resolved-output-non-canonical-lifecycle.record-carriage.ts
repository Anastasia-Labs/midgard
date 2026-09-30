import { fileURLToPath } from "node:url";

import { expect } from "vitest";

import { type VanRossemFitMeasurement } from "../src/proof-fit/van-rossem-fit-ledger.js";
import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  MAXIMUM_INPUT_ITEM_COUNT,
  RESOLVED_OUTPUT_MAXIMUM_BYTES,
} from "./support/resolved-output-non-canonical-emulator.js";

export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "prior_root",
  "prior_output_descriptor",
  "prior_output_membership",
  "field_certificate",
  "forced_leaf_reason_coordinate",
  "forced_leaf_header",
  "forced_leaf_root",
  "forced_leaf_direction",
  "output_chunk",
  "scan_checkpoint",
  "wrong_successor",
  "finishable_advance",
  "premature_finalize",
] as const;

export const CANCELLABLE_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;

const MAXIMUM_SHAPE = `${RESOLVED_OUTPUT_MAXIMUM_BYTES.toLocaleString("en-US")}-byte prior-ledger output at adversarial membership depth and a Certified ${MAXIMUM_INPUT_ITEM_COUNT.toString()}-item input field`;

export const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/resolved-output-non-canonical-v1-fit-ledger.json",
    import.meta.url,
  ),
);

export const coverage = createLifecycleCoverageRecorder();

export const measurements: VanRossemFitMeasurement[] = [];

export let publicationsRecorded = false;

const expectFit = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
  runsScripts: boolean,
) => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  if (!runsScripts) return;
  expect(measurement.executionMemory, name).toBeGreaterThan(0n);
  expect(measurement.executionSteps, name).toBeGreaterThan(0n);
};

export const record = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
  { runsScripts = true, maximumShape = MAXIMUM_SHAPE } = {},
) => {
  expectFit(name, measurement, runsScripts);
  measurements.push({
    name,
    kind: "lifecycle",
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

export const recordPublication = (
  stepIndex: number,
  measurement: CompleteSignedTransactionMeasurement,
) => {
  expect(
    measurement.completeSignedBytes,
    `step ${(stepIndex + 1).toString()} publication`,
  ).toBeLessThanOrEqual(15_872);
  if (publicationsRecorded) return;
  measurements.push({
    name: `publish-step0${(stepIndex + 1).toString()}`,
    kind: "publication",
    maximumShape: "fully applied testnet validator",
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
  if (stepIndex === 4) publicationsRecorded = true;
};

/** Step 02 submits its carriage chunks and certificate before the step itself. */
export const recordCarriage = (
  prefix: string,
  captured: {
    readonly measurements: readonly CompleteSignedTransactionMeasurement[];
  },
) => {
  const auxiliary = captured.measurements.slice(0, -1);
  expect(auxiliary.length).toBeGreaterThanOrEqual(2);
  auxiliary.forEach((measurement, index) => {
    const last = index === auxiliary.length - 1;
    // Chunk publications carry bytes only; the certificate runs its mint.
    record(
      last
        ? `${prefix}-carriage-certificate`
        : `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
      measurement,
      { runsScripts: last },
    );
  });
};

export const progress = (message: string) =>
  console.info(`[resolved-output-non-canonical-progress] ${message}`);
