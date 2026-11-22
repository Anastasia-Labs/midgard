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

export const createWorkflowFundingRequirements = (
  value: WorkflowFundingRequirementsInput,
): WorkflowFundingRequirements => {
  const normalized = normalizedRequirements(value, false);
  return Object.freeze({
    schemaVersion: WORKFLOW_FUNDING_REQUIREMENTS,
    scope: normalized.scope,
    hubOraclePolicyId: normalized.hubOraclePolicyId,
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
 * Strict structural parser for a measured profile.
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
    hubOraclePolicyId: normalized.hubOraclePolicyId,
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
