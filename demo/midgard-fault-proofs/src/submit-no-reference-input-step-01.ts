/** Event-relative no-reference-input stage 1. */
import { createInputExistenceDeploymentStep } from "./input-existence/deployment-step.js";

export const submitNoReferenceInputStep01 = createInputExistenceDeploymentStep(
  1,
  0,
);

import { createInputExistenceFileSubmitter } from "./input-existence/submit-from-files.js";

export const submitNoReferenceInputStep01FromFiles =
  createInputExistenceFileSubmitter(1, 0);
