/** The `executionSourceScriptDecoding` direct family the submitters and
 * prover read: the shared step 01 and event selection, then a terminal that
 * decides from the selected leaf's recorded reason alone. */
import * as SDK from "@al-ft/midgard-sdk";

import type {
  DirectPurposeContracts,
  DirectPurposeFamily,
} from "../direct-purpose-family/family.js";

export const EXECUTION_SOURCE_SCRIPT_DECODING_CATEGORY_LABEL =
  "execution-source-script-decoding";

/** Named roles; fails closed when the chain is not the 3-role graph. */
export const executionSourceScriptDecodingContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.ExecutionSourceScriptDecodingRoles =>
  SDK.executionSourceScriptDecodingRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY: DirectPurposeFamily<SDK.ExecutionSourceScriptDecodingFinalState> =
  {
    label: EXECUTION_SOURCE_SCRIPT_DECODING_CATEGORY_LABEL,
    roleOrder: SDK.EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER,
    roles: executionSourceScriptDecodingContractRoles,
    verdict: {
      schema: SDK.ExecutionSourceScriptDecodingFinalDatum,
      convicts: SDK.executionSourceScriptDecodingConvicts,
    },
  };
