import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { bindProductionWorkflowFundingRequirementsToRunnerV1 } from "./production-funding-requirements-admission-v1.js";
import type { ProductionWorkflowFundingRequirementsV1 } from "./production-funding-requirements-v1.js";

export const PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1 =
  "midgard-production-fraud-proof-workflow-runner-v1" as const;

type AdmittedRunnerV1<Input> = Readonly<{
  runnerVersion: typeof PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1;
  runOrResume(input: Input): Promise<unknown>;
}>;

const categoryByRunner = new WeakMap<object, FraudProofCatalogueCategoryName>();

/**
 * Internal constructor for executable production runners. This module is not
 * part of the package barrel, so callers cannot mint the runtime identity by
 * copying the public version string onto an arbitrary function.
 */
export const createAdmittedProductionWorkflowRunnerV1 = <Input>({
  category,
  runOrResume,
  fundingRequirements,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runOrResume: (input: Input) => Promise<unknown>;
  /** Fixed measured profile; omission keeps W31 readiness fail-closed. */
  readonly fundingRequirements?: ProductionWorkflowFundingRequirementsV1;
}): AdmittedRunnerV1<Input> => {
  const runner = Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
    runOrResume,
  });
  categoryByRunner.set(runner, category);
  if (fundingRequirements !== undefined) {
    bindProductionWorkflowFundingRequirementsToRunnerV1({
      category,
      runner,
      requirements: fundingRequirements,
    });
  }
  return runner;
};

export const isAdmittedProductionWorkflowRunnerV1 = ({
  category,
  runner,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: object;
}): boolean => categoryByRunner.get(runner) === category;
