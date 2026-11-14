import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  readWorkflowFundingMeasurementContext,
  type WorkflowFundingMeasurementContext,
} from "./funding-measurements.identity.js";
import {
  type RecordedWorkflowFundingActionMeasurement,
  workflowFundingMeasurementArtifactBytes,
} from "./funding-measurements.recording.js";

/** Active catalogue comes from the actual selected deployment, including PD. */
export const workflowFundingMeasurementCoverage = (input: {
  readonly context: WorkflowFundingMeasurementContext;
  readonly measurements: readonly RecordedWorkflowFundingActionMeasurement[];
}) => {
  const { identity } = readWorkflowFundingMeasurementContext(input.context);
  const hashes = new Set<string>();
  for (const measurement of input.measurements) {
    workflowFundingMeasurementArtifactBytes(measurement);
    if (
      hashes.has(measurement.measurementArtifactSha256) ||
      computeDeploymentManifestJsonDigest(measurement.artifact.identity) !==
        computeDeploymentManifestJsonDigest(identity) ||
      !identity.categories.includes(measurement.artifact.category)
    )
      throw new Error(
        "funding coverage contains duplicate or foreign deployment/credential measurements",
      );
    hashes.add(measurement.measurementArtifactSha256);
  }
  const categories = identity.categories.map((category) => {
    const captured = input.measurements.filter(
      (measurement) => measurement.artifact.category === category,
    );
    return {
      category,
      measurements: captured.length,
      missing:
        captured.length === 0
          ? ["no-genuine-workflow-capture"]
          : [
              ...new Set(
                captured.flatMap(
                  (measurement) => measurement.artifact.missingCoverage,
                ),
              ),
            ].sort(),
    };
  });
  // Current producer captures Init. No amount of relabeling or repetition can
  // assert worst-shape or remaining lifecycle coverage that was not measured.
  return Object.freeze({ complete: false as const, categories });
};

export const assertCompleteWorkflowFundingMeasurements = (
  input: Parameters<typeof workflowFundingMeasurementCoverage>[0],
): never => {
  const report = workflowFundingMeasurementCoverage(input);
  throw new Error(
    `Incomplete genuine workflow funding coverage: ${report.categories.filter(({ missing }) => missing.length !== 0).length.toString()} active categories; remaining lifecycle and maximum shapes require measurements`,
  );
};
