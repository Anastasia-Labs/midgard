/** The `executionSourceScriptDecoding` family definition and its
 * manifest-bound runner. */
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import {
  CHUNKED_VERIFY_WITNESS_ROLES,
  defineFamily,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import {
  createExecutionSourceScriptDecodingTransactionPort,
  executionSourceScriptDecodingWorkflowContracts,
  type ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
} from "./workflow.transaction-port.js";

export type ManifestBoundExecutionSourceScriptDecodingWorkflow =
  ManifestBoundFamilyWorkflow<"executionSourceScriptDecoding", true, 3>;

const WITNESS_ROLES = CHUNKED_VERIFY_WITNESS_ROLES;

const S = FraudProofComputationThreadStepDatum;

export const EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION = defineFamily({
  category: "executionSourceScriptDecoding",
  stepDatumSchemas: [S, S, S],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: EXECUTION_SOURCE_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofExecutionSourceScriptDecoding",
      "fraudProofExecutionSourceScriptDecodingBindEvent",
      "fraudProofExecutionSourceScriptDecodingTerminal",
    ],
    transactionPort: (context) =>
      createExecutionSourceScriptDecodingTransactionPort({
        ...context,
        contracts: executionSourceScriptDecodingWorkflowContracts(
          context.binding,
          context.certificate,
        ),
      }),
  },
});

export const createManifestBoundExecutionSourceScriptDecodingWorkflow = (
  config: ManifestBoundExecutionSourceScriptDecodingWorkflowConfig,
): Promise<ManifestBoundExecutionSourceScriptDecodingWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
