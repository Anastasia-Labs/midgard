/** The `executionNativeScriptInvalid` direct walk family the submitters and
 * prover read. */
import * as SDK from "@al-ft/midgard-sdk";

import type {
  DirectPurposeContracts,
  DirectPurposeFamily,
} from "../direct-purpose-family/family.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL =
  "execution-native-script-invalid";

/** Named roles; fails closed when the chain is not the 28-role graph. */
export const executionNativeScriptInvalidContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.ExecutionNativeScriptInvalidRoles =>
  SDK.executionNativeScriptInvalidRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY: DirectPurposeFamily<SDK.ExecutionNativeScriptInvalidFinalState> =
  {
    label: EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
    roleOrder: SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER,
    roles: executionNativeScriptInvalidContractRoles,
    verdict: {
      schema: SDK.ExecutionNativeScriptInvalidFinalDatum,
      convicts: SDK.executionNativeScriptInvalidConvicts,
    },
  };

export type ExecutionNativeScriptInvalidRole =
  SDK.ExecutionNativeScriptInvalidRole;
