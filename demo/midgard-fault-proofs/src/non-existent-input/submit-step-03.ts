/** Current event-relative non-existent-input stage 3; obsolete loose-root
 * and creator-transaction evidence is not an accepted input. */
import { createInputExistenceDeploymentStep } from "../input-existence/deployment-step.js";

export const neSubmitStep03 = createInputExistenceDeploymentStep(0, 2);

export type NeSubmitStep03Result = Awaited<ReturnType<typeof neSubmitStep03>>;
