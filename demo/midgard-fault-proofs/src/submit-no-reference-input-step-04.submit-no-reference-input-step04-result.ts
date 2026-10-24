import type { InputExistenceStepFileConfig } from "./input-existence/submit-from-files.js";
import type { submitNoReferenceInputStep04 } from "./submit-no-reference-input-step-04.submit-no-reference-input-step04.js";

export type SubmitNoReferenceInputStep04CliConfig =
  InputExistenceStepFileConfig;
export type SubmitNoReferenceInputStep04Result = Awaited<
  ReturnType<typeof submitNoReferenceInputStep04>
>;
