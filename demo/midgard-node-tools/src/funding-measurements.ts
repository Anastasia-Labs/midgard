import { createHash } from "node:crypto";
import { open } from "node:fs/promises";

import {
  createWorkflowFundingMeasurementContext,
  type LocallyEvaluatedTransaction,
  measureComputationThreadInitFunding,
  type RecordedWorkflowFundingActionMeasurement,
  workflowFundingMeasurementArtifactBytes,
} from "@al-ft/midgard-fault-proofs";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";

/**
 * Declared measurement producer output. Only a genuine in-process recorder
 * can write; existing evidence is never replaced. Keep this directory private:
 * it contains signed CBOR, custody values and complete resolved UTxOs.
 */
export const writeWorkflowFundingMeasurement = async (input: {
  readonly outputFile: string;
  readonly measurement: RecordedWorkflowFundingActionMeasurement;
}) => {
  const bytes = workflowFundingMeasurementArtifactBytes(input.measurement);
  const hash = createHash("sha256").update(bytes).digest("hex");
  if (hash !== input.measurement.measurementArtifactSha256)
    throw new Error("workflow funding measurement artifact hash differs");
  const file = await open(input.outputFile, "wx", 0o600);
  try {
    await file.writeFile(bytes);
    await file.sync();
  } finally {
    await file.close();
  }
  return Object.freeze({
    outputFile: input.outputFile,
    sha256: hash,
    bytes: bytes.length,
    status: "partial" as const,
  });
};

/** Complete producer call stays in one compiled FP module instance. */
export const captureAndWriteComputationThreadInitFunding = async (input: {
  readonly identity: Parameters<
    typeof createWorkflowFundingMeasurementContext
  >[0];
  readonly lucid: LucidEvolution;
  readonly transaction: LocallyEvaluatedTransaction;
  readonly category: FraudProofCatalogueCategoryName;
  readonly outputFile: string;
}) => {
  const context = createWorkflowFundingMeasurementContext(input.identity);
  const measurement = await measureComputationThreadInitFunding({
    context,
    lucid: input.lucid,
    transaction: input.transaction,
    category: input.category,
  });
  return writeWorkflowFundingMeasurement({
    outputFile: input.outputFile,
    measurement,
  });
};
