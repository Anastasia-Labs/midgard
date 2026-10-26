/** The `missingScriptSource` direct purpose family the submitters and prover
 * read. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  DIRECT_CLAIM_VERDICT,
  type DirectPurposeContracts,
  type DirectPurposeFamily,
} from "../direct-purpose-family/family.js";

export const MISSING_SCRIPT_SOURCE_CATEGORY_LABEL = "missing-script-source";

/** Named roles; fails closed when the chain is not the 37-role graph. */
export const missingScriptSourceContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.MissingScriptSourceRoles =>
  SDK.missingScriptSourceRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const MISSING_SCRIPT_SOURCE_FAMILY: DirectPurposeFamily = {
  label: MISSING_SCRIPT_SOURCE_CATEGORY_LABEL,
  roleOrder: SDK.MISSING_SCRIPT_SOURCE_ROLE_ORDER,
  roles: missingScriptSourceContractRoles,
  verdict: DIRECT_CLAIM_VERDICT,
};
