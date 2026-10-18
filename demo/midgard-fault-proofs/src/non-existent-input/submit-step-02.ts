/** Current event-relative non-existent-input stage 2; obsolete loose-root
 * and creator-transaction evidence is not an accepted input. */
import { createInputExistenceDeploymentStep } from "../input-existence/deployment-step.js";

export const neSubmitStep02 = createInputExistenceDeploymentStep(0, 1);

export type NeSubmitStep02Result = Awaited<ReturnType<typeof neSubmitStep02>>;
