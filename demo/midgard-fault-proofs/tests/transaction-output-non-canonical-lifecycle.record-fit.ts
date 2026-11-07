import { expect } from "vitest";

import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import {
  type StepIndex,
  stepLabel,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

/**
 * The per-transaction checks the lifecycle and datum suites make on what they
 * submit. Every emulator submission is also held to the Van Rossem limits by
 * the suite's submit guard (tests/support/emulator/van-rossem-submit-guard.ts).
 */

export const record = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
};

/** A carriage's chunk publications, then its certificate, named by `prefix`. */
export const recordCarriage = (
  prefix: string,
  submitted: readonly CompleteSignedTransactionMeasurement[],
): void =>
  submitted.forEach((measurement, index) =>
    record(
      index === submitted.length - 1
        ? `${prefix}-carriage-certificate`
        : `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
      measurement,
    ),
  );

/** A registered step's reference-script publication, by physical step. */
export const recordPublication = (
  stepIndex: StepIndex,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  const name = `step${stepLabel(stepIndex)}-reference-publication`;
  expect(measurement.completeSignedBytes, name).toBeLessThanOrEqual(15_872);
};

/** Labels the chunk, certificate and step transactions one builder call submitted. */
export const recordAll = (
  prefix: string,
  submitted: readonly CompleteSignedTransactionMeasurement[],
  certified: boolean,
): void => {
  const stepIndex = submitted.length - 1;
  const certificateIndex = certified ? stepIndex - 1 : -1;
  submitted.forEach((measurement, index) => {
    if (index === stepIndex) record(prefix, measurement);
    else if (index === certificateIndex)
      record(`${prefix}-carriage-certificate`, measurement);
    else
      record(
        `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
        measurement,
      );
  });
};
