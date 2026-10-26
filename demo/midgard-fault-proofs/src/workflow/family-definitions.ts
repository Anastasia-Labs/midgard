import { FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION } from "../field-item-width-illegal/workflow.js";
import { FIELD_PREIMAGE_LENGTH_FAMILY_DEFINITION } from "../field-preimage-length-mismatch/authenticated-workflow.js";
import { MIN_ADA_FAMILY_DEFINITION } from "../min-ada/workflow.js";
import { MINT_AUTHORIZATION_FAMILY_DEFINITION } from "../mint-authorization/workflow.js";
import { MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION } from "../mint-declared-asset-limit/v1.js";
import { MISSING_REDEEMER_FAMILY_DEFINITION } from "../missing-redeemer/workflow.js";
import { NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION } from "../native-script-decoding/workflow.js";
import { NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION } from "../native-script-invalid/workflow.js";
import { OBSERVER_ORDER_INVALID_FAMILY_DEFINITION } from "../observer-order-invalid/v1.js";
import { OBSERVERS_FORBIDDEN_FAMILY_DEFINITION } from "../observers-forbidden-on-untagged-network/v1.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION } from "../output-reference-script-decoding/authenticated-workflow.js";
import { PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION } from "../protected-output-signer-missing/authenticated-workflow.js";
import { REDEEMER_CANONICITY_FAMILY_DEFINITION } from "../redeemer-canonicity/runtime.js";
import { RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION } from "../resolved-output-non-canonical/authenticated-workflow.js";
import { SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_DEFINITION } from "../script-integrity-hash-missing/v1.js";
import { SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION } from "../spend-input-signer-missing/authenticated-workflow.js";
import { TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION } from "../transaction-output-non-canonical/workflow.js";
import { TRANSITION_TRACE_FAMILY_DEFINITION } from "../transition-trace/workflow.js";
import { WITHDRAWAL_MISTAG_FAMILY_DEFINITION } from "../withdrawal-mistag/workflow.js";
import { WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION } from "../witness-script-decoding/workflow.js";
import { CANONICAL_DECODABILITY_FAMILY_DEFINITION } from "./canonical-decodability.js";
import { LINEAR_FAMILY_DEFINITIONS } from "./linear-family-definitions.js";

/**
 * The manifest-bound definitions whose adapter arm is a cursor spec, keyed
 * by category. Cursor categories have no closed list to guard against; the
 * table-driven assembly test checks each key against its definition.
 */
type CursorFamilyDefinitions = Readonly<{
  canonicalDecodability: typeof CANONICAL_DECODABILITY_FAMILY_DEFINITION;
  witnessScriptDecoding: typeof WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION;
  transactionOutputNonCanonical: typeof TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION;
  resolvedOutputNonCanonical: typeof RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION;
  spendInputSignerMissing: typeof SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION;
  protectedOutputSignerMissing: typeof PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION;
  outputReferenceScriptDecoding: typeof OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION;
  fieldItemWidthIllegal: typeof FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION;
  fieldPreimageLengthMismatch: typeof FIELD_PREIMAGE_LENGTH_FAMILY_DEFINITION;
  redeemerCanonicity: typeof REDEEMER_CANONICITY_FAMILY_DEFINITION;
  transitionTrace: typeof TRANSITION_TRACE_FAMILY_DEFINITION;
  minAda: typeof MIN_ADA_FAMILY_DEFINITION;
  nativeScriptInvalid: typeof NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION;
  mintDeclaredAssetLimit: typeof MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION;
  mintAuthorization: typeof MINT_AUTHORIZATION_FAMILY_DEFINITION;
  nativeScriptDecoding: typeof NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION;
  observersForbiddenOnUntaggedNetwork: typeof OBSERVERS_FORBIDDEN_FAMILY_DEFINITION;
  observerOrderInvalid: typeof OBSERVER_ORDER_INVALID_FAMILY_DEFINITION;
  scriptIntegrityHashMissing: typeof SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_DEFINITION;
  withdrawalMistag: typeof WITHDRAWAL_MISTAG_FAMILY_DEFINITION;
  missingRedeemer: typeof MISSING_REDEEMER_FAMILY_DEFINITION;
}>;

export const CURSOR_FAMILY_DEFINITIONS: CursorFamilyDefinitions = Object.freeze(
  {
    canonicalDecodability: CANONICAL_DECODABILITY_FAMILY_DEFINITION,
    witnessScriptDecoding: WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION,
    transactionOutputNonCanonical:
      TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    resolvedOutputNonCanonical: RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    spendInputSignerMissing: SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION,
    protectedOutputSignerMissing:
      PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION,
    outputReferenceScriptDecoding:
      OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION,
    fieldItemWidthIllegal: FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION,
    fieldPreimageLengthMismatch: FIELD_PREIMAGE_LENGTH_FAMILY_DEFINITION,
    redeemerCanonicity: REDEEMER_CANONICITY_FAMILY_DEFINITION,
    transitionTrace: TRANSITION_TRACE_FAMILY_DEFINITION,
    minAda: MIN_ADA_FAMILY_DEFINITION,
    nativeScriptInvalid: NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
    mintDeclaredAssetLimit: MINT_DECLARED_ASSET_LIMIT_FAMILY_DEFINITION,
    mintAuthorization: MINT_AUTHORIZATION_FAMILY_DEFINITION,
    nativeScriptDecoding: NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION,
    observersForbiddenOnUntaggedNetwork: OBSERVERS_FORBIDDEN_FAMILY_DEFINITION,
    observerOrderInvalid: OBSERVER_ORDER_INVALID_FAMILY_DEFINITION,
    scriptIntegrityHashMissing: SCRIPT_INTEGRITY_HASH_MISSING_FAMILY_DEFINITION,
    withdrawalMistag: WITHDRAWAL_MISTAG_FAMILY_DEFINITION,
    missingRedeemer: MISSING_REDEEMER_FAMILY_DEFINITION,
  },
);

/** Every definition the assembly builds, linear and cursor, keyed by category. */
export const FAMILY_DEFINITIONS: typeof LINEAR_FAMILY_DEFINITIONS &
  CursorFamilyDefinitions = Object.freeze({
  ...LINEAR_FAMILY_DEFINITIONS,
  ...CURSOR_FAMILY_DEFINITIONS,
});

export type AssembledFamilyCategory = keyof typeof FAMILY_DEFINITIONS;
