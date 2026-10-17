import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentManifestFraudProofCatalogueCategory,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  Assets,
  Data,
  fromText,
  LucidEvolution,
  toUnit,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type {
  AuthenticatedValidator,
  FraudProofs,
  MerkleRoot,
} from "../common.js";

export const FRAUD_PROOF_CATALOGUE_ASSET_NAME = fromText(
  "MIDGARD_FRAUD_PROOF_CATALOGUE",
);

export const FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT = 4;

// Category order and four-byte identities are owned by the core deployment
// registry. Identity is carried by the id map, never by array position.
export const FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER =
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER satisfies readonly (keyof FraudProofs)[];

export type FraudProofCatalogueCategoryName =
  DeploymentManifestFraudProofCatalogueCategory;

export const FRAUD_PROOF_CATALOGUE_CATEGORY_IDS =
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS;

export const FIELD_PREIMAGE_LENGTH_MISMATCH_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.fieldPreimageLengthMismatch;
export const FIELD_ITEM_WIDTH_ILLEGAL_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.fieldItemWidthIllegal;
export const WITNESS_SCRIPT_DECODING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.witnessScriptDecoding;
export const SCRIPT_INTEGRITY_HASH_MISSING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.scriptIntegrityHashMissing;
export const MINT_ITEM_NON_CANONICAL_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.mintItemNonCanonical;
export const TRANSACTION_OUTPUT_NON_CANONICAL_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.transactionOutputNonCanonical;
export const RESOLVED_OUTPUT_NON_CANONICAL_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.resolvedOutputNonCanonical;
export const MINT_DECLARED_ASSET_LIMIT_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.mintDeclaredAssetLimit;
export const SPEND_INPUT_SIGNER_MISSING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.spendInputSignerMissing;
export const PROTECTED_OUTPUT_SIGNER_MISSING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.protectedOutputSignerMissing;
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.observersForbiddenOnUntaggedNetwork;
export const OBSERVER_ORDER_INVALID_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.observerOrderInvalid;
export const REDEEMER_CANONICITY_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.redeemerCanonicity;
export const OUTPUT_REFERENCE_SCRIPT_DECODING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.outputReferenceScriptDecoding;
export const EXECUTION_SOURCE_SCRIPT_DECODING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.executionSourceScriptDecoding;
export const RECEIVE_PURPOSE_LANGUAGE_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.receivePurposeLanguage;
export const UNUSED_SCRIPT_WITNESS_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.unusedScriptWitness;
export const MISSING_SCRIPT_SOURCE_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingScriptSource;
export const MISSING_REDEEMER_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingRedeemer;
export const UNUSED_REDEEMER_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.unusedRedeemer;
export const EXECUTION_NATIVE_SCRIPT_INVALID_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.executionNativeScriptInvalid;
export const SCRIPT_INTEGRITY_HASH_MISMATCH_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.scriptIntegrityHashMismatch;
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.distinctAssetAccumulationLimit;

export const NATIVE_SCRIPT_DECODING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.nativeScriptDecoding;
export const MISSING_SIGNATURE_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingSignature;
export const WITHDRAWN_REFERENCE_INPUT_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawnReferenceInput;
export const CANONICAL_DECODABILITY_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.canonicalDecodability;
export const COMMITTED_FIELD_SHAPE_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.committedFieldShape;
export const MIN_FEE_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.minFee;
export const WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawalMistag;
export const DOUBLE_WITHDRAW_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleWithdraw;
export const L2_TX_MISTAG_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.l2TxMistag;
export const WITHDRAWN_INPUT_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawnInput;
export const VALUE_NOT_PRESERVED_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.valueNotPreserved;
export const INPUT_SET_UNIQUENESS_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.inputSetUniqueness;
export const MINT_AUTHORIZATION_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.mintAuthorization;
export const NETWORK_ID_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.networkId;
export const NATIVE_SCRIPT_INVALID_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.nativeScriptInvalid;
export const MIN_ADA_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.minAda;

export type FraudProofCatalogueCategoryDeploymentInfo = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

/**
 * A deployment's catalogue. `categories` holds the categories its profile
 * carries; an excluded category is absent and has no membership proof.
 */
export type FraudProofCatalogueDeploymentInfo<
  CategoryName extends string = FraudProofCatalogueCategoryName,
> = {
  readonly root: MerkleRoot;
  readonly categories: Readonly<
    Partial<Record<CategoryName, FraudProofCatalogueCategoryDeploymentInfo>>
  >;
};

export const FraudProofCatalogueDatumSchema = Data.Bytes();

export type FraudProofCatalogueDatum = Data.Static<
  typeof FraudProofCatalogueDatumSchema
>;
export const FraudProofCatalogueDatum = asDataType<FraudProofCatalogueDatum>(
  FraudProofCatalogueDatumSchema,
);

export const FraudProofCatalogueMintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Literal("NewFraudCategory"),
  Data.Literal("RemoveFraudCategory"),
]);
export type FraudProofCatalogueMintRedeemer = Data.Static<
  typeof FraudProofCatalogueMintRedeemerSchema
>;
export const FraudProofCatalogueMintRedeemer =
  asDataType<FraudProofCatalogueMintRedeemer>(
    FraudProofCatalogueMintRedeemerSchema,
  );

export const FraudProofCatalogueSpendRedeemerSchema = Data.Object({
  fraudProofCatalogueAssetName: Data.Bytes(),
});
export type FraudProofCatalogueSpendRedeemer = Data.Static<
  typeof FraudProofCatalogueSpendRedeemerSchema
>;
export const FraudProofCatalogueSpendRedeemer =
  asDataType<FraudProofCatalogueSpendRedeemer>(
    FraudProofCatalogueSpendRedeemerSchema,
  );

export type FraudProofCatalogueInitParams = {
  validator: AuthenticatedValidator;
  mptRootHash: string;
};

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofCatalogueInitTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueInitParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const assets: Assets = {
      [toUnit(params.validator.policyId, FRAUD_PROOF_CATALOGUE_ASSET_NAME)]: 1n,
    };

    return lucid
      .newTx()
      .mintAssets(assets, Data.to("Init", FraudProofCatalogueMintRedeemer))
      .pay.ToAddressWithData(
        params.validator.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(params.mptRootHash, FraudProofCatalogueDatum),
        },
        assets,
      )
      .attach.Script(params.validator.mintingScript);
  });
