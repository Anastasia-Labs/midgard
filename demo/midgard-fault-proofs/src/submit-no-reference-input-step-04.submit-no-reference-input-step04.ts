/** Current event-relative no-reference-input stage 4; obsolete loose-root
 * and creator-transaction evidence is not an accepted input. */
import { createInputExistenceDeploymentStep } from "./input-existence/deployment-step.js";

export const submitNoReferenceInputStep04 = createInputExistenceDeploymentStep(
  1,
  3,
);
