import { expect } from "vitest";

import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";

/** The envelope suite's seams; the datum suite's are {@link DATUM_SEAMS}. */

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

export const DATUM_SEAMS = [
  "datum_chunk",
  "datum_checkpoint",
  "datum_stack",
] as const;

/** The envelope suite's cancels; the datum suite cancels `step-04-datum`. */

export const CANCELLABLE_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;

/** The registered steps' publication rows, in chain order. */
const PUBLICATION_NAMES = [
  "publish-step01",
  "publish-step02",
  "publish-step03",
  "publish-step04",
  "publish-step04-datum",
  "publish-step05",
] as const;

export const coverage = createLifecycleCoverageRecorder();

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
  { runsScripts = true } = {},
) => {
  expectFit(name, measurement, runsScripts);
};

export const recordPublication = (
  stepIndex: number,
  measurement: CompleteSignedTransactionMeasurement,
) => {
  const name = PUBLICATION_NAMES[stepIndex];
  if (name === undefined)
    throw new Error(`no step at index ${stepIndex.toString()}`);
  expect(measurement.completeSignedBytes, name).toBeLessThanOrEqual(15_872);
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
