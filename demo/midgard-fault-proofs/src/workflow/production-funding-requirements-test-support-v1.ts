import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { WorkflowAdapterRunner } from "./production-adapters-v1.js";
import type { WorkflowFundingRequirements } from "./production-funding-requirements-v1.js";
import { createAdmittedWorkflowRunner } from "./production-runner-admission-v1.js";

/**
 * Test-only seam for consumers that must exercise the opaque runner/profile
 * binding. Production code must obtain runners from fixed family factories.
 */
export const unsafeCreateMeasuredWorkflowRunnerForTest = ({
  category,
  fundingRequirements,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly fundingRequirements: WorkflowFundingRequirements;
}): WorkflowAdapterRunner => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error(
      "unsafe measured workflow runner construction is available only under the test runtime",
    );
  }
  return createAdmittedWorkflowRunner({
    category,
    fundingRequirements,
    runOrResume: async () => {
      throw new Error("test-only measured workflow runner is not executable");
    },
  });
};
