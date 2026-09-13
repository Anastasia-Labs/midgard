import { expect } from "vitest";

import { type CompleteSignedTransactionMeasurement } from "./measurement.js";

/**
 * GOAL_SPEC.md 3.3 proof-fit thresholds, shared by every emulator suite that
 * measures a complete correction path.
 *
 * 1. Byte fit: `l1ByteMargin` is computed against the real
 *    `PROTOCOL_PARAMETERS_DEFAULT.maxTxSize` (16,384), not the emulator's
 *    relaxed 65,536 ceiling, so a non-negative margin is exactly the 3.3
 *    item-1 check.
 * 2. Execution fit: memory and CPU are at or below the deployment's measured
 *    limits with at least a 20% reserve. A path that fits the raw limit but
 *    not the reserve is a FAILING result, not a smaller margin.
 */
export const EXECUTION_RESERVE_FRACTION = 20n;

export const expectProofFit = ({
  stage,
  measurement,
  maxTxExMem,
  maxTxExSteps,
}: {
  readonly stage: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly maxTxExMem: bigint;
  readonly maxTxExSteps: bigint;
}): void => {
  // 3.3 item 1 - byte fit against the real L1 envelope.
  expect(
    measurement.l1ByteMargin,
    `${stage} exceeds the 16,384-byte L1 envelope`,
  ).toBeGreaterThanOrEqual(0);
  // 3.3 item 2 - execution fit with a 20% reserve.
  const memoryCeiling =
    (maxTxExMem * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  const stepCeiling =
    (maxTxExSteps * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  expect(
    measurement.executionMemory <= memoryCeiling,
    `${stage} execution memory ${measurement.executionMemory.toString()} exceeds the 20%-reserve ceiling ${memoryCeiling.toString()}`,
  ).toBe(true);
  expect(
    measurement.executionSteps <= stepCeiling,
    `${stage} execution steps ${measurement.executionSteps.toString()} exceeds the 20%-reserve ceiling ${stepCeiling.toString()}`,
  ).toBe(true);
};

/**
 * Debug-only proof-fit dump, gated on `MIDGARD_PRINT_PROOF_FIT=1`. `headline`
 * is the caller's own label text, so each suite prints exactly the line it
 * printed before; `extra` is merged into the same object the stage map
 * produces, and `includeReferenceInputs` adds the reference-input count the
 * published-chunk carriage suite reports.
 */
export const printProofFit = ({
  headline,
  stages,
  extra,
  includeReferenceInputs = false,
}: {
  readonly headline: string;
  readonly stages: Record<string, CompleteSignedTransactionMeasurement>;
  readonly extra?: Record<string, unknown>;
  readonly includeReferenceInputs?: boolean;
}): void => {
  if (process.env["MIDGARD_PRINT_PROOF_FIT"] !== "1") {
    return;
  }
  const stageEntries = Object.fromEntries(
    Object.entries(stages).map(([stage, measurement]) => [
      stage,
      {
        bytes: measurement.completeSignedBytes,
        l1ByteMargin: measurement.l1ByteMargin,
        memory: measurement.executionMemory.toString(),
        steps: measurement.executionSteps.toString(),
        ...(includeReferenceInputs
          ? { referenceInputs: measurement.referenceInputCount }
          : {}),
      },
    ]),
  );
  console.log(
    `${headline}: ${JSON.stringify(
      extra === undefined ? stageEntries : { ...stageEntries, ...extra },
      null,
      2,
    )}`,
  );
};
