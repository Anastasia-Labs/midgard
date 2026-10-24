/** Current event-relative non-existent-input stage 1; obsolete loose-root
 * and creator-transaction evidence is not an accepted input. */
import { createInputExistenceDeploymentStep } from "../input-existence/deployment-step.js";

export const neSubmitStep01 = createInputExistenceDeploymentStep(0, 0);

export type NeSubmitStep01Result = Awaited<ReturnType<typeof neSubmitStep01>>;
