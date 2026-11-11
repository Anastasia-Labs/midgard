/** The `unusedScriptWitness` direct walk family the submitters and prover
 * read. */
import * as SDK from "@al-ft/midgard-sdk";

import type {
  DirectPurposeContracts,
  DirectPurposeCustodyFamily,
} from "../direct-purpose-family/family.js";

export const UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL = "unused-script-witness";

/** Named roles; fails closed when the chain is not the 24-role graph. */
export const unusedScriptWitnessContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.UnusedScriptWitnessRoles =>
  SDK.unusedScriptWitnessRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const UNUSED_SCRIPT_WITNESS_FAMILY: DirectPurposeCustodyFamily = {
  label: UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
  roleOrder: SDK.UNUSED_SCRIPT_WITNESS_ROLE_ORDER,
  roles: unusedScriptWitnessContractRoles,
};

export type UnusedScriptWitnessRole = SDK.UnusedScriptWitnessRole;
