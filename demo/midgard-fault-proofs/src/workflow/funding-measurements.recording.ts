import { createHash } from "node:crypto";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { type evaluateWorkflowFundingTransaction } from "./funding-measurements.evaluate.js";
import {
  WORKFLOW_FUNDING_MEASUREMENT_TOOL,
  type WorkflowFundingMeasurementIdentity,
} from "./funding-measurements.identity.js";
import { type WorkflowFundingAction } from "./funding-requirements.workflow-funding-controlled-output.js";

export type WorkflowFundingActionMeasurementArtifact = Readonly<{
  schemaVersion: typeof WORKFLOW_FUNDING_MEASUREMENT_TOOL;
  status: "partial";
  transactionStatus: "locally_evaluated_prepared";
  identity: WorkflowFundingMeasurementIdentity;
  category: FraudProofCatalogueCategoryName;
  shape: string;
  action: WorkflowFundingAction;
  evidence: Awaited<
    ReturnType<typeof evaluateWorkflowFundingTransaction>
  >["evidence"];
  missingCoverage: readonly string[];
}>;
export type RecordedWorkflowFundingActionMeasurement = Readonly<{
  artifact: WorkflowFundingActionMeasurementArtifact;
  measurementArtifactSha256: string;
  normalizedAction: WorkflowFundingAction;
}>;
const records = new WeakMap<object, string>();
const encoded = (artifact: WorkflowFundingActionMeasurementArtifact) =>
  canonicalJson(artifact, "workflow funding measurement");

/** Internal producer boundary; callers cannot install a spread/cast artifact. */
export const recordWorkflowFundingActionMeasurement = (
  artifact: WorkflowFundingActionMeasurementArtifact,
): RecordedWorkflowFundingActionMeasurement => {
  const snapshot = structuredClone(artifact);
  const bytes = encoded(snapshot);
  const measurement = Object.freeze({
    artifact: snapshot,
    measurementArtifactSha256: createHash("sha256").update(bytes).digest("hex"),
    normalizedAction: snapshot.action,
  });
  records.set(measurement, bytes);
  return measurement;
};

/** Exact canonical bytes: the writer adds no self-hash field or newline. */
export const workflowFundingMeasurementArtifactBytes = (
  measurement: RecordedWorkflowFundingActionMeasurement,
): Uint8Array => {
  const bytes = records.get(measurement);
  if (bytes === undefined || encoded(measurement.artifact) !== bytes)
    throw new Error("workflow funding measurement is unrecorded or changed");
  return Buffer.from(bytes, "utf8");
};
