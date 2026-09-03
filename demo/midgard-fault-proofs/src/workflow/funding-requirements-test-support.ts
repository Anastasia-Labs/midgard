import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { WorkflowAdapterRunner } from "./adapters.js";
import type { WorkflowFundingRequirements } from "./funding-requirements.js";
import { createAdmittedWorkflowRunner } from "./runner-admission.js";

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
