import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  digestInput,
  normalizedRequirements,
} from "./funding-requirements.normalized-requirements.js";
import {
  digest,
  WORKFLOW_FUNDING_REQUIREMENTS,
  type WorkflowFundingRequirements,
  type WorkflowFundingRequirementsInput,
} from "./funding-requirements.workflow-funding-controlled-output.js";
import {
  fundingRequirementsForRunnerIdentity,
  isAdmittedFundingRequirementsIdentity,
} from "./funding-requirements-admission.js";
import { isAdmittedWorkflowRunner } from "./runner-admission.js";

export const createWorkflowFundingRequirements = (
  value: WorkflowFundingRequirementsInput,
): WorkflowFundingRequirements => {
  const normalized = normalizedRequirements(value, false);
  return Object.freeze({
    schemaVersion: WORKFLOW_FUNDING_REQUIREMENTS,
    scope: normalized.scope,
    deploymentFingerprint: normalized.deploymentFingerprint,
    blueprintSha256: normalized.blueprintSha256,
    protocolParametersDigest: normalized.protocolParametersDigest,
    economicsPolicyDigest: normalized.economicsPolicyDigest,
    fundingPaymentKeyHash: normalized.fundingPaymentKeyHash,
    measurementToolVersion: normalized.measurementToolVersion,
    measurementArtifactSha256: normalized.measurementArtifactSha256,
    actions: normalized.actions,
    profileDigest: digest(digestInput(normalized)),
  });
};

/**
 * Strict structural parser for a measured profile. This does not make the
 * profile production authority: a fixed category factory must separately bind
 * its exact admitted runner to the measured profile identity.
 */
export const admitWorkflowFundingRequirements = (
  value: unknown,
): WorkflowFundingRequirements => {
  const normalized = normalizedRequirements(value, true);
  if (normalized.profileDigest !== digest(digestInput(normalized))) {
    throw new Error("funding requirements profile digest mismatch");
  }
  return Object.freeze({
    schemaVersion: WORKFLOW_FUNDING_REQUIREMENTS,
    scope: normalized.scope,
    deploymentFingerprint: normalized.deploymentFingerprint,
    blueprintSha256: normalized.blueprintSha256,
    protocolParametersDigest: normalized.protocolParametersDigest,
    economicsPolicyDigest: normalized.economicsPolicyDigest,
    fundingPaymentKeyHash: normalized.fundingPaymentKeyHash,
    measurementToolVersion: normalized.measurementToolVersion,
    measurementArtifactSha256: normalized.measurementArtifactSha256,
    actions: normalized.actions,
    profileDigest: normalized.profileDigest!,
  });
};

/**
 * Returns only a profile selected by the fixed module-admitted runner factory.
 * A structurally valid measurement profile is deliberately insufficient.
 */
export const workflowFundingRequirementsForRunner = ({
  category,
  runner,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: object;
}): WorkflowFundingRequirements => {
  if (!isAdmittedWorkflowRunner({ category, runner })) {
    throw new Error("funding requirements runner is not category-admitted");
  }
  const requirements = fundingRequirementsForRunnerIdentity(runner);
  if (requirements === null) {
    throw new Error(
      `${category} production runner has no admitted measured funding profile`,
    );
  }
  return requirements;
};

/** Used by the Q58 application after its non-catalogue fixed factory admits it. */
export const assertAdmittedWorkflowFundingRequirements = (
  requirements: WorkflowFundingRequirements,
): void => {
  if (!isAdmittedFundingRequirementsIdentity(requirements)) {
    throw new Error("production funding requirements are not factory-admitted");
  }
};
