export const COMPLETE_LIFECYCLE_BASE_SCENARIOS_V1 = Object.freeze([
  "wrongful_acceptance_success",
  "wrongful_forced_rejection_success",
  "honest_accepted_block_refusal",
  "honest_forced_rejection_refusal",
  "reason_or_subject_coordinate_mutation",
  "permanent_proof_token_and_descendant_removal",
  "maximum_supported_evidence",
] as const);

export type CompleteLifecycleBaseScenarioV1 =
  (typeof COMPLETE_LIFECYCLE_BASE_SCENARIOS_V1)[number];

export type CompleteLifecycleCoverageV1 = {
  readonly reasonArms: readonly string[];
  readonly successfulDirectionByReason: Readonly<
    Record<string, readonly ("accepted_invalid" | "forced_rejection_wrong")[]>
  >;
  readonly scenarios: readonly CompleteLifecycleBaseScenarioV1[];
  readonly authenticatedSeamsMutated: readonly string[];
  readonly cancelledPhysicalSteps: readonly string[];
  readonly resumedAfterCheckpoint: boolean;
  readonly adjacentOverBoundRefused: boolean;
};

const missingFrom = (
  expected: readonly string[],
  actual: readonly string[],
): readonly string[] => {
  const observed = new Set(actual);
  return expected.filter((item) => !observed.has(item));
};

/** Shared gate used by family lifecycle suites; throws with every omission. */
export const assertCompleteLifecycleCoverageV1 = ({
  coverage,
  expectedReasonArms,
  authenticationSeams,
  cancellablePhysicalSteps,
  resumable,
  hasAdjacentConsensusBound,
}: {
  readonly coverage: CompleteLifecycleCoverageV1;
  readonly expectedReasonArms: readonly string[];
  readonly authenticationSeams: readonly string[];
  readonly cancellablePhysicalSteps: readonly string[];
  readonly resumable: boolean;
  readonly hasAdjacentConsensusBound: boolean;
}): void => {
  const failures: string[] = [];
  const missingReasons = missingFrom(expectedReasonArms, coverage.reasonArms);
  if (missingReasons.length > 0)
    failures.push(`reason arms: ${missingReasons.join(", ")}`);
  for (const reason of expectedReasonArms) {
    const directions = coverage.successfulDirectionByReason[reason] ?? [];
    const missingDirections = missingFrom(
      ["accepted_invalid", "forced_rejection_wrong"],
      directions,
    );
    if (missingDirections.length > 0) {
      failures.push(
        `${reason} success directions: ${missingDirections.join(", ")}`,
      );
    }
  }
  const missingScenarios = missingFrom(
    COMPLETE_LIFECYCLE_BASE_SCENARIOS_V1,
    coverage.scenarios,
  );
  if (missingScenarios.length > 0)
    failures.push(`scenarios: ${missingScenarios.join(", ")}`);
  const missingSeams = missingFrom(
    authenticationSeams,
    coverage.authenticatedSeamsMutated,
  );
  if (missingSeams.length > 0)
    failures.push(`authentication seams: ${missingSeams.join(", ")}`);
  const missingCancels = missingFrom(
    cancellablePhysicalSteps,
    coverage.cancelledPhysicalSteps,
  );
  if (missingCancels.length > 0)
    failures.push(`cancel steps: ${missingCancels.join(", ")}`);
  if (resumable && !coverage.resumedAfterCheckpoint)
    failures.push("checkpoint resume");
  if (hasAdjacentConsensusBound && !coverage.adjacentOverBoundRefused) {
    failures.push("adjacent-over-bound refusal");
  }
  if (failures.length > 0) {
    throw new Error(
      `incomplete fault-proof lifecycle coverage: ${failures.join("; ")}`,
    );
  }
};
