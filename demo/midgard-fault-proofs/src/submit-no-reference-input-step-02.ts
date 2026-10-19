/** Current event-relative no-reference-input stage 2; obsolete loose-root
 * and creator-transaction evidence is not an accepted input. */
import { createInputExistenceDeploymentStep } from "./input-existence/deployment-step.js";

export const submitNoReferenceInputStep02 = createInputExistenceDeploymentStep(
  1,
  1,
);

import { createInputExistenceFileSubmitter } from "./input-existence/submit-from-files.js";

export const submitNoReferenceInputStep02FromFiles =
  createInputExistenceFileSubmitter(1, 1);

export type SubmitNoReferenceInputStep02Result = Awaited<
  ReturnType<typeof submitNoReferenceInputStep02>
>;
export type SubmitNoReferenceInputStep02CliConfig = Parameters<
  typeof submitNoReferenceInputStep02FromFiles
>[0];
