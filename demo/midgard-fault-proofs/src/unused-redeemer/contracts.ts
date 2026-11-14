/** The `unusedRedeemer` direct purpose family the submitters and prover
 * read. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  DIRECT_CLAIM_VERDICT,
  type DirectPurposeContracts,
  type DirectPurposeFamily,
} from "../direct-purpose-family/family.js";

export const UNUSED_REDEEMER_CATEGORY_LABEL = "unused-redeemer";

/** Named roles; fails closed when the chain is not the 25-role graph. */
export const unusedRedeemerContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.UnusedRedeemerRoles =>
  SDK.unusedRedeemerRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const UNUSED_REDEEMER_FAMILY: DirectPurposeFamily = {
  label: UNUSED_REDEEMER_CATEGORY_LABEL,
  roleOrder: SDK.UNUSED_REDEEMER_ROLE_ORDER,
  roles: unusedRedeemerContractRoles,
  verdict: DIRECT_CLAIM_VERDICT,
};
