/** The `executionSourceScriptDecoding` family's emulator prover: its plan
 * with the kit's field plans, as the walk-keyed driver takes it. */
import { directPurposeFieldPlans } from "../../src/direct-purpose-family/index.js";
import {
  type ExecutionSourceScriptDecodingProofPlan,
  prepareExecutionSourceScriptDecodingProof,
} from "../../src/execution-source-script-decoding/index.js";

/** The family's plan with the kit's field plans, for the kit driver. */
export const prepareExecutionSourceScriptDecodingPlan = ({
  material,
  prover,
}: Parameters<
  typeof prepareExecutionSourceScriptDecodingProof
>[0]): ExecutionSourceScriptDecodingProofPlan & {
  readonly plan: ReturnType<typeof directPurposeFieldPlans>;
} => ({
  ...prepareExecutionSourceScriptDecodingProof({ material, prover }),
  plan: directPurposeFieldPlans(material, prover),
});
