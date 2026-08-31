import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { ProductionWorkflowFundingRequirementsV1 } from "./production-funding-requirements-v1.js";

const fundingByRunner = new WeakMap<
  object,
  ProductionWorkflowFundingRequirementsV1
>();
const admittedFundingRequirements = new WeakSet<object>();

/** Internal-only fixed-factory admission. Not exported from the package barrel. */
export const bindProductionWorkflowFundingRequirementsToRunnerV1 = ({
  category,
  runner,
  requirements,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: object;
  readonly requirements: ProductionWorkflowFundingRequirementsV1;
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
export const admitProductionAvailabilityFundingRequirementsV1 = (
  requirements: ProductionWorkflowFundingRequirementsV1,
): void => {
  if (requirements.scope.kind !== "da_availability_lifecycle") {
    throw new Error("availability funding requirements have another scope");
  }
  admittedFundingRequirements.add(requirements);
};

export const fundingRequirementsForRunnerIdentityV1 = (
  runner: object,
): ProductionWorkflowFundingRequirementsV1 | null =>
  fundingByRunner.get(runner) ?? null;

export const isAdmittedProductionFundingRequirementsIdentityV1 = (
  requirements: object,
): boolean => admittedFundingRequirements.has(requirements);
