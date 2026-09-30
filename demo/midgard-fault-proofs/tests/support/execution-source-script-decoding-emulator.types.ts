import { makeExecutionSourceStages } from "./execution-source-script-decoding-emulator.make-execution-source-stages.js";

export type ExecutionSourceStages = ReturnType<
  typeof makeExecutionSourceStages
>;
