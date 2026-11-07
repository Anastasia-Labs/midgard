import {
  type MidgardValidators,
  type ReferenceScriptAuthPolicy,
} from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

export type RemovalDeploymentReference = {
  readonly scriptHash: string;
  readonly utxo: UTxO;
};

/**
 * Manifest entry pinning the `native-script-decoding` step-01 script hash for
 * removal (#635). The name is caller-chosen because the family predates its
 * catalogue registration: `submitRemoveFraudulentBlock` checks the explicit
 * category record's step-01 hash against whatever entry the record names, and
 * this is the name the emulator manifests use.
 */
export const NATIVE_SCRIPT_DECODING_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofNativeScriptDecoding";

export const MISSING_SIGNATURE_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofMissingSignature";

export const WITHDRAWN_REFERENCE_INPUT_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofWithdrawnReferenceInput";

export const CANONICAL_DECODABILITY_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofCanonicalDecodability";

export const COMMITTED_FIELD_SHAPE_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofCommittedFieldShape";

export const MIN_FEE_REMOVAL_DEPLOYMENT_ENTRY = "fraudProofMinFee";

export const DOUBLE_WITHDRAW_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofDoubleWithdraw";

export const L2_TX_MISTAG_REMOVAL_DEPLOYMENT_ENTRY = "fraudProofL2TxMistag";

export const WITHDRAWN_INPUT_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofWithdrawnInput";

export const WITHDRAWAL_MISTAG_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofWithdrawalMistag";

/**
 * Manifest entry pinning the `input-set-uniqueness` step-01 script hash for
 * removal.
 */
export const INPUT_SET_UNIQUENESS_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofInputSetUniqueness";

/**
 * Manifest entry pinning the `value-not-preserved` step-01 script hash for
 * removal.
 */
export const VALUE_NOT_PRESERVED_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofValueNotPreserved";

/**
 * Manifest entry pinning the `mint-authorization` step-01 script hash for
 * removal.
 */
export const MINT_AUTHORIZATION_REMOVAL_DEPLOYMENT_ENTRY =
  "fraudProofMintAuthorization";

export const requireReferenceScriptAuthPolicy = (
  policy: MidgardValidators["referenceScriptAuth"],
): ReferenceScriptAuthPolicy => {
  const candidate = policy as Partial<ReferenceScriptAuthPolicy>;
  if (
    policy.mintingScript.type !== "Native" ||
    candidate.expiresAtSlot === undefined ||
    candidate.expiresAtUnixTime === undefined ||
    candidate.timelockDurationMs === undefined
  ) {
    throw new Error(
      "Removal deployment fixture requires the harness native reference-script auth policy",
    );
  }
  return candidate as ReferenceScriptAuthPolicy;
};
