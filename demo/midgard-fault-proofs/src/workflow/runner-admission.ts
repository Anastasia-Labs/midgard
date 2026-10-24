import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { WorkflowFundingRequirements } from "./funding-requirements.js";
import { bindWorkflowFundingRequirementsToRunner } from "./funding-requirements-admission.js";

export const WORKFLOW_ADAPTER_RUNNER =
  "midgard-production-fraud-proof-workflow-runner-v1" as const;

type AdmittedRunner<Input> = Readonly<{
  runnerVersion: typeof WORKFLOW_ADAPTER_RUNNER;
  runOrResume(input: Input): Promise<unknown>;
}>;

const categoryByRunner = new WeakMap<object, FraudProofCatalogueCategoryName>();

/**
 * Internal constructor for executable production runners. This module is not
 * part of the package barrel, so callers cannot mint the runtime identity by
 * copying the public version string onto an arbitrary function.
 */
export const createAdmittedWorkflowRunner = <Input>({
  category,
  runOrResume,
  fundingRequirements,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runOrResume: (input: Input) => Promise<unknown>;
  /**
   * Optional measured profile, bound to this runner for
   * `workflowFundingRequirementsForRunner`. Omitting it changes nothing at
   * runtime: the runner is installed and ready, and its reservations are
   * priced from the runtime funding policy, which never reads this profile.
   */
  readonly fundingRequirements?: WorkflowFundingRequirements;
}): AdmittedRunner<Input> => {
  const runner = Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume,
  });
  categoryByRunner.set(runner, category);
  if (fundingRequirements !== undefined) {
    bindWorkflowFundingRequirementsToRunner({
      category,
      runner,
      requirements: fundingRequirements,
    });
  }
  return runner;
};

export const isAdmittedWorkflowRunner = ({
  category,
  runner,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: object;
}): boolean => categoryByRunner.get(runner) === category;
