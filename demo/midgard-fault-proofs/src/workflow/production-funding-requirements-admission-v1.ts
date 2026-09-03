import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { WorkflowFundingRequirements } from "./production-funding-requirements-v1.js";

const fundingByRunner = new WeakMap<object, WorkflowFundingRequirements>();
const admittedFundingRequirements = new WeakSet<object>();

/** Internal-only fixed-factory admission. Not exported from the package barrel. */
export const bindWorkflowFundingRequirementsToRunner = ({
  category,
  runner,
  requirements,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: object;
  readonly requirements: WorkflowFundingRequirements;
}): void => {
  if (
    requirements.scope.kind !== "fraud_proof_category" ||
    requirements.scope.category !== category
  ) {
    throw new Error(
      "production funding requirements scope differs from its fixed runner",
    );
  }
  if (fundingByRunner.has(runner)) {
    throw new Error("production runner already has funding requirements");
  }
  fundingByRunner.set(runner, requirements);
  admittedFundingRequirements.add(requirements);
};

/** Internal-only fixed Q58 application admission. */
export const admitAvailabilityFundingRequirements = (
  requirements: WorkflowFundingRequirements,
): void => {
  if (requirements.scope.kind !== "da_availability_lifecycle") {
    throw new Error("availability funding requirements have another scope");
  }
  admittedFundingRequirements.add(requirements);
};

export const fundingRequirementsForRunnerIdentity = (
  runner: object,
): WorkflowFundingRequirements | null => fundingByRunner.get(runner) ?? null;

export const isAdmittedFundingRequirementsIdentity = (
  requirements: object,
): boolean => admittedFundingRequirements.has(requirements);
