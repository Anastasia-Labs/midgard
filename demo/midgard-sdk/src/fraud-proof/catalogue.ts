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

// Canonical presentation order. Identity is carried by the explicit map below,
// never re-derived from array position: program waves intentionally integrate
// some sparse IDs before the intervening families are ready.
export const FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = [
  "doubleSpend",
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "invalidRange",
  "transitionTrace",
  "zeroInput",
  "validationTraceDispute",
  "daHashPreimage",
  "noReferenceInput",
  "referenceInputNoIdx",
  "invalidSignature",
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "nativeScriptDecoding",
  "missingSignature",
  "missingNativeScriptTx",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "withdrawalMistag",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "l2TxMistag",
  "withdrawnInput",
  "valueNotPreserved",
  "inputSetUniqueness",
  "mintAuthorization",
  "networkId",
  "missingNativeScriptUtxo",
  "nativeScriptInvalid",
  "minAda",
  "fieldPreimageLengthMismatch",
  "fieldItemWidthIllegal",
  "witnessScriptDecoding",
  "scriptIntegrityHashMissing",
  "transactionOutputNonCanonical",
  "resolvedOutputNonCanonical",
  "mintDeclaredAssetLimit",
  "spendInputSignerMissing",
  "protectedOutputSignerMissing",
  "observersForbiddenOnUntaggedNetwork",
  "observerOrderInvalid",
  "redeemerCanonicity",
  "outputReferenceScriptDecoding",
  "executionSourceScriptDecoding",
  "receivePurposeLanguage",
  "unusedScriptWitness",
  "missingScriptSource",
  "missingRedeemer",
  "unusedRedeemer",
  "executionNativeScriptInvalid",
  "scriptIntegrityHashMismatch",
  "distinctAssetAccumulationLimit",
] as const satisfies readonly (keyof FraudProofs)[];

export type FraudProofCatalogueCategoryName =
  (typeof FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number];

/**
 * Canonical four-byte big-endian category identifiers. The mapping is pinned
 * next to the append-only order so registration, thread-token construction,
 * deployment manifests, and catalogue proofs share one authority.
 */
export const FRAUD_PROOF_CATALOGUE_CATEGORY_IDS = {
  doubleSpend: "00000000",
  nonExistentInput: "00000001",
  nonExistentInputNoIndex: "00000002",
  invalidRange: "00000003",
  transitionTrace: "00000004",
  zeroInput: "00000005",
  validationTraceDispute: "00000006",
  daHashPreimage: "00000007",
  noReferenceInput: "00000008",
  referenceInputNoIdx: "00000009",
  invalidSignature: "0000000a",
  fabricatedDeposit: "0000000b",
  fabricatedWithdrawal: "0000000c",
  nativeScriptDecoding: "0000000d",
  missingSignature: "0000000e",
  missingNativeScriptTx: "0000000f",
  withdrawnReferenceInput: "00000010",
  canonicalDecodability: "00000011",
  committedFieldShape: "00000012",
  minFee: "00000013",
  withdrawalMistag: "00000014",
  doubleWithdraw: "00000015",
  crossBlockDuplicateEvent: "00000016",
  l2TxMistag: "00000017",
  withdrawnInput: "00000018",
  valueNotPreserved: "00000019",
  inputSetUniqueness: "0000001a",
  mintAuthorization: "0000001b",
  networkId: "0000001c",
  missingNativeScriptUtxo: "0000001d",
  nativeScriptInvalid: "0000001e",
  minAda: "0000001f",
  fieldPreimageLengthMismatch: "00000020",
  fieldItemWidthIllegal: "00000021",
  witnessScriptDecoding: "00000022",
  scriptIntegrityHashMissing: "00000023",
  transactionOutputNonCanonical: "00000029",
  resolvedOutputNonCanonical: "00000026",
  mintDeclaredAssetLimit: "0000002c",
  spendInputSignerMissing: "00000027",
  protectedOutputSignerMissing: "0000002b",
  observersForbiddenOnUntaggedNetwork: "00000024",
  observerOrderInvalid: "00000025",
  redeemerCanonicity: "00000028",
  outputReferenceScriptDecoding: "0000002a",
  executionSourceScriptDecoding: "00000031",
  receivePurposeLanguage: "00000034",
  unusedScriptWitness: "0000002f",
  missingScriptSource: "0000002d",
  missingRedeemer: "0000002e",
  unusedRedeemer: "00000030",
  executionNativeScriptInvalid: "00000032",
  scriptIntegrityHashMismatch: "00000033",
  distinctAssetAccumulationLimit: "00000035",
} as const satisfies Readonly<Record<FraudProofCatalogueCategoryName, string>>;

export const FIELD_PREIMAGE_LENGTH_MISMATCH_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.fieldPreimageLengthMismatch;
export const FIELD_ITEM_WIDTH_ILLEGAL_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.fieldItemWidthIllegal;
export const WITNESS_SCRIPT_DECODING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.witnessScriptDecoding;
export const SCRIPT_INTEGRITY_HASH_MISSING_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.scriptIntegrityHashMissing;
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
export const MISSING_NATIVE_SCRIPT_TX_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingNativeScriptTx;
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
export const MISSING_NATIVE_SCRIPT_UTXO_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingNativeScriptUtxo;
export const NATIVE_SCRIPT_INVALID_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.nativeScriptInvalid;
export const MIN_ADA_FRAUD_CATEGORY_ID =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.minAda;

export type FraudProofCatalogueCategoryDeploymentInfo = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export type FraudProofCatalogueDeploymentInfo<
  CategoryName extends string = FraudProofCatalogueCategoryName,
> = {
  readonly root: MerkleRoot;
  readonly categories: Readonly<
    Record<CategoryName, FraudProofCatalogueCategoryDeploymentInfo>
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
