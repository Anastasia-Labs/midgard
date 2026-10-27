/** The `receivePurposeLanguage` direct purpose family the submitters and
 * prover read. Its terminal decides with its own verdict over the shared
 * claim final state. */
import * as SDK from "@al-ft/midgard-sdk";

import type {
  DirectPurposeContracts,
  DirectPurposeFamily,
} from "../direct-purpose-family/family.js";

export const RECEIVE_PURPOSE_LANGUAGE_CATEGORY_LABEL =
  "receive-purpose-language";

/** Named roles; fails closed when the chain is not the 21-role graph. */
export const receivePurposeLanguageContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.ReceivePurposeLanguageRoles =>
  SDK.receivePurposeLanguageRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const RECEIVE_PURPOSE_LANGUAGE_FAMILY: DirectPurposeFamily = {
  label: RECEIVE_PURPOSE_LANGUAGE_CATEGORY_LABEL,
  roleOrder: SDK.RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER,
  roles: receivePurposeLanguageContractRoles,
  verdict: {
    schema: SDK.DirectClaimFinalDatum,
    convicts: SDK.receivePurposeLanguageConvicts,
  },
};
