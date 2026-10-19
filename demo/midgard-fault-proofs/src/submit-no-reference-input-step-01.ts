/** Current event-relative no-reference-input stage 1; obsolete loose-root
 * and creator-transaction evidence is not an accepted input. */
import { createInputExistenceDeploymentStep } from "./input-existence/deployment-step.js";

export const submitNoReferenceInputStep01 = createInputExistenceDeploymentStep(
  1,
  0,
);

import { createInputExistenceFileSubmitter } from "./input-existence/submit-from-files.js";

export const submitNoReferenceInputStep01FromFiles =
  createInputExistenceFileSubmitter(1, 0);

export type SubmitNoReferenceInputStep01Result = Awaited<
  ReturnType<typeof submitNoReferenceInputStep01>
>;
export type SubmitNoReferenceInputStep01CliConfig = Parameters<
  typeof submitNoReferenceInputStep01FromFiles
>[0];
