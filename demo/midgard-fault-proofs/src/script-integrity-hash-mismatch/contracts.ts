/** The `scriptIntegrityHashMismatch` direct walk family the submitters and
 * prover read. */
import * as SDK from "@al-ft/midgard-sdk";

import type {
  DirectPurposeContracts,
  DirectPurposeCustodyFamily,
} from "../direct-purpose-family/family.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL =
  "script-integrity-hash-mismatch";

/** Named roles; fails closed when the chain is not the 21-role graph. */
export const scriptIntegrityHashMismatchContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.ScriptIntegrityHashMismatchRoles =>
  SDK.scriptIntegrityHashMismatchRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY: DirectPurposeCustodyFamily =
  {
    label: SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL,
    roleOrder: SDK.SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER,
    roles: scriptIntegrityHashMismatchContractRoles,
    step01Redeemer: SDK.ScriptIntegrityHashMismatchStep01Redeemer,
  };

export type ScriptIntegrityHashMismatchRole =
  SDK.ScriptIntegrityHashMismatchRole;
