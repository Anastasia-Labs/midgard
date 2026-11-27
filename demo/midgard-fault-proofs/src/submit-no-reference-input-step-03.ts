/** Event-relative no-reference-input stage 3. */
import { createInputExistenceDeploymentStep } from "./input-existence/deployment-step.js";

export const submitNoReferenceInputStep03 = createInputExistenceDeploymentStep(
  1,
  2,
);

import { createInputExistenceFileSubmitter } from "./input-existence/submit-from-files.js";

export const submitNoReferenceInputStep03FromFiles =
  createInputExistenceFileSubmitter(1, 2);
