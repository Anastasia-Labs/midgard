import {
  type E2EL2StressConfig,
  type E2EL2StressMeasurementPolicy,
} from "./types.js";

export const measurementPolicyForConfig = (
  config: Pick<E2EL2StressConfig, "loadModel" | "workloadProfile">,
): E2EL2StressMeasurementPolicy => ({
  loadModel: config.loadModel,
  workloadProfile: config.workloadProfile,
  syntheticVsProduction:
    config.workloadProfile === "synthetic-admission"
      ? "synthetic_admission_diagnostic"
      : "production_end_user_path",
  advanceOn:
    config.loadModel === "open-loop-upper-bound"
      ? "scheduled_submit"
      : "accepted",
  primaryStageMetric:
    config.loadModel === "open-loop-upper-bound"
      ? "metrics.durableAdmission.perSecond"
      : "metrics.l2Admission.perSecond",
  finalityObservation:
    config.loadModel === "open-loop-upper-bound"
      ? "aggregate-window"
      : "post-submit-bounded",
  submissionWindowExcludesCommitDrain: true,
  fullFinalityRequiresDrainProof: true,
});
