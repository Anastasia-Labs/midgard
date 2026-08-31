import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { ProductionWorkflowAdapterRunnerV1 } from "./production-adapters-v1.js";
import type { ProductionWorkflowFundingRequirementsV1 } from "./production-funding-requirements-v1.js";
import { createAdmittedProductionWorkflowRunnerV1 } from "./production-runner-admission-v1.js";

/**
 * Test-only seam for consumers that must exercise the opaque runner/profile
 * binding. Production code must obtain runners from fixed family factories.
 */
export const unsafeCreateMeasuredProductionWorkflowRunnerForTestV1 = ({
  category,
  fundingRequirements,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly fundingRequirements: ProductionWorkflowFundingRequirementsV1;
}): ProductionWorkflowAdapterRunnerV1 => {
  if (process.env.NODE_ENV !== "test") {
    throw new Error(
      "unsafe measured workflow runner construction is available only under the test runtime",
    );
  }
  return createAdmittedProductionWorkflowRunnerV1({
    category,
    fundingRequirements,
    runOrResume: async () => {
      throw new Error("test-only measured workflow runner is not executable");
    },
  });
};
