/** Current event-relative no-reference-input stage 3; obsolete loose-root
 * and creator-transaction evidence is not an accepted input. */
import { createInputExistenceDeploymentStep } from "./input-existence/deployment-step.js";

export const submitNoReferenceInputStep03 = createInputExistenceDeploymentStep(
  1,
  2,
);

import { createInputExistenceFileSubmitter } from "./input-existence/submit-from-files.js";

export const submitNoReferenceInputStep03FromFiles =
  createInputExistenceFileSubmitter(1, 2);
