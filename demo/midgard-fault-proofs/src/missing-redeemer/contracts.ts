/** The `missingRedeemer` direct purpose family the submitters and prover read. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  DIRECT_CLAIM_VERDICT,
  type DirectPurposeContracts,
  type DirectPurposeFamily,
} from "../direct-purpose-family/family.js";

export const MISSING_REDEEMER_CATEGORY_LABEL = "missing-redeemer";

/** Named roles; fails closed when the chain is not the 39-role graph. */
export const missingRedeemerContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.MissingRedeemerRoles =>
  SDK.missingRedeemerRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const MISSING_REDEEMER_FAMILY: DirectPurposeFamily = {
  label: MISSING_REDEEMER_CATEGORY_LABEL,
  roleOrder: SDK.MISSING_REDEEMER_ROLE_ORDER,
  roles: missingRedeemerContractRoles,
  verdict: DIRECT_CLAIM_VERDICT,
};
