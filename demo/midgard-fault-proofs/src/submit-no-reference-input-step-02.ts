/** Event-relative no-reference-input stage 2. */
import { createInputExistenceDeploymentStep } from "./input-existence/deployment-step.js";

export const submitNoReferenceInputStep02 = createInputExistenceDeploymentStep(
  1,
  1,
);

import { createInputExistenceFileSubmitter } from "./input-existence/submit-from-files.js";

export const submitNoReferenceInputStep02FromFiles =
  createInputExistenceFileSubmitter(1, 1);
