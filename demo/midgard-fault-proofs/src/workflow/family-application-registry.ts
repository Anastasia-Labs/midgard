/**
 * The family application registry: one record per fault family, keyed by
 * catalogue category. Its keys are the installed set; a host installs every
 * registered family through the one shared loop in `family-application.ts`
 * rather than branching per family.
 *
 * A linear family's record is derived from its `FamilyDefinition`: the step
 * contract names come from its linear spec, the witness roles and the
 * field-preimage certificate flag from the definition itself, so nothing is
 * restated. A decision-digest cursor family's record is derived the same way,
 * with the state-queue removal set read from the definition's auxiliary
 * reference scripts; a decision-digest family on the authenticated certificate
 * shape derives its record the same way, laying each step into the config key
 * that names that step's contract. A cursor family without a decision digest
 * derives its record from its definition too, declaring beside its roster the
 * optional infrastructure it requires — the predecessor replay context or the
 * public retained-DA sources — and the config fields it reads from it. A family whose config shape is its own writes a short record by hand:
 * `doubleSpend`, `transitionTrace`, `mintItemNonCanonical`, `missingSignature`,
 * `networkId`, `valueNotPreserved` and `validationTraceDispute`, the last of
 * which is the one family that requires the host's validation-challenge port.
 *
 * The `satisfies` guard requires exactly one record per catalogue category,
 * with the record's own category as its key, so an omitted or misnamed family
 * fails typecheck.
 */

import "../distinct-asset-accumulation-limit/v1.js";
import "../execution-native-script-invalid/v1.js";
import "../execution-source-script-decoding/v1.js";
import "../field-item-width-illegal/workflow.js";
import "../field-preimage-length-mismatch/authenticated-workflow.js";
import "../field-preimage-length-mismatch/config.js";
import "../min-ada/workflow.js";
import "../mint-authorization/workflow.js";
import "../mint-declared-asset-limit/v1.js";
import "../mint-item-non-canonical/workflow.js";
import "../missing-redeemer/v1.js";
import "../missing-script-source/v1.js";
import "../native-script-decoding/workflow.js";
import "../native-script-invalid/workflow.js";
import "../network-id/workflow-adapter.js";
import "../observer-order-invalid/v1.js";
import "../observers-forbidden-on-untagged-network/v1.js";
import "../output-reference-script-decoding/authenticated-workflow.js";
import "../protected-output-signer-missing/authenticated-workflow.js";
import "../receive-purpose-language/manifest-workflow.js";
import "../redeemer-canonicity/runtime.js";
import "../remove-fraudulent-block.js";
import "../resolved-output-non-canonical/authenticated-workflow.js";
import "../runtime.js";
import "../script-integrity-hash-mismatch/manifest-workflow.js";
import "../script-integrity-hash-missing/v1.js";
import "../spend-input-signer-missing/authenticated-workflow.js";
import "../transaction-output-non-canonical/workflow.js";
import "../transition-trace/workflow.js";
import "../unused-redeemer/v1.js";
import "../unused-script-witness/v1.js";
import "../validation-dispute/workflow-family.js";
import "../validation-dispute/workflow-v1.js";
import "../value-not-preserved/contracts.js";
import "../value-not-preserved/workflow.js";
import "../withdrawal-mistag/workflow.js";
import "../witness-script-decoding/workflow.js";
import "./cursor-family-runtime.js";
import "./da-hash-preimage.js";
import "./double-spend-adapter.js";
import "./family-application.js";
import "./family-definition.js";
import "./linear-family-definitions.js";
import "./linear-family-spec.js";
import "./manifest-bound-family-assembly.js";
import "./missing-signature.js";
import "./family-application-registry.resolve-roster-parts.js";
import "./family-application-registry.linear-family-application-record.js";
import "./family-application-registry.decision-digest-cursor-family-application-record.js";
import "./family-application-registry.authenticated-certificate-family-application-record.js";
import "./family-application-registry.bundle-cursor-family-application-record.js";
import "./family-application-registry.value-not-preserved-family-application-record.js";
import "./family-application-registry.family-application-registry.js";
export {
  FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_APPLICATION_RECORD,
  FIELD_PREIMAGE_LENGTH_MISMATCH_FAMILY_APPLICATION_RECORD,
  OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_APPLICATION_RECORD,
  TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  WITNESS_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
} from "./family-application-registry.authenticated-certificate-family-application-record.js";
export {
  EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD,
  MIN_ADA_FAMILY_APPLICATION_RECORD,
  MINT_AUTHORIZATION_FAMILY_APPLICATION_RECORD,
  MINT_ITEM_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  NATIVE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD,
  RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_APPLICATION_RECORD,
  SPEND_INPUT_SIGNER_MISSING_FAMILY_APPLICATION_RECORD,
  TRANSITION_TRACE_FAMILY_APPLICATION_RECORD,
  WITHDRAWAL_MISTAG_FAMILY_APPLICATION_RECORD,
} from "./family-application-registry.bundle-cursor-family-application-record.js";
export {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_APPLICATION_RECORD,
  EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD,
  MINT_DECLARED_ASSET_LIMIT_FAMILY_APPLICATION_RECORD,
  MISSING_REDEEMER_FAMILY_APPLICATION_RECORD,
  MISSING_SCRIPT_SOURCE_FAMILY_APPLICATION_RECORD,
  OBSERVER_ORDER_INVALID_FAMILY_APPLICATION_RECORD,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FAMILY_APPLICATION_RECORD,
  RECEIVE_PURPOSE_LANGUAGE_FAMILY_APPLICATION_RECORD,
  REDEEMER_CANONICITY_FAMILY_APPLICATION_RECORD,
  SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_APPLICATION_RECORD,
  SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_APPLICATION_RECORD,
  UNUSED_REDEEMER_FAMILY_APPLICATION_RECORD,
  UNUSED_SCRIPT_WITNESS_FAMILY_APPLICATION_RECORD,
} from "./family-application-registry.decision-digest-cursor-family-application-record.js";
export { FAMILY_APPLICATION_REGISTRY } from "./family-application-registry.family-application-registry.js";
export { DOUBLE_SPEND_FAMILY_APPLICATION_RECORD } from "./family-application-registry.linear-family-application-record.js";
export {
  type FamilyApplicationRegistryEntry,
  MISSING_SIGNATURE_FAMILY_APPLICATION_RECORD,
  NETWORK_ID_FAMILY_APPLICATION_RECORD,
  VALIDATION_TRACE_DISPUTE_FAMILY_APPLICATION_RECORD,
  VALUE_NOT_PRESERVED_FAMILY_APPLICATION_RECORD,
} from "./family-application-registry.value-not-preserved-family-application-record.js";
