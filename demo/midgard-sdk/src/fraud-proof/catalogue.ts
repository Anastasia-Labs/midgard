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
} from "@/common.js";

export const FRAUD_PROOF_CATALOGUE_ASSET_NAME = fromText(
  "MIDGARD_FRAUD_PROOF_CATALOGUE",
);

export const FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT = 4;

// Category IDs are the positional index in this list (see
// `fraudProofsToIndexedValidators`), so new categories must be appended:
// inserting one shifts the ID of every category after it.
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
} as const satisfies Readonly<Record<FraudProofCatalogueCategoryName, string>>;

export const NATIVE_SCRIPT_DECODING_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.nativeScriptDecoding;
export const MISSING_SIGNATURE_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingSignature;
export const MISSING_NATIVE_SCRIPT_TX_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingNativeScriptTx;
export const WITHDRAWN_REFERENCE_INPUT_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawnReferenceInput;
export const CANONICAL_DECODABILITY_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.canonicalDecodability;
export const COMMITTED_FIELD_SHAPE_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.committedFieldShape;
export const MIN_FEE_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.minFee;
export const WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawalMistag;
export const DOUBLE_WITHDRAW_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleWithdraw;
export const L2_TX_MISTAG_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.l2TxMistag;
export const WITHDRAWN_INPUT_FRAUD_CATEGORY_ID_V1 =
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawnInput;

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
export const FraudProofCatalogueDatum =
  FraudProofCatalogueDatumSchema as unknown as FraudProofCatalogueDatum;

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
  FraudProofCatalogueMintRedeemerSchema as unknown as FraudProofCatalogueMintRedeemer;

export const FraudProofCatalogueSpendRedeemerSchema = Data.Object({
  fraudProofCatalogueAssetName: Data.Bytes(),
});
export type FraudProofCatalogueSpendRedeemer = Data.Static<
  typeof FraudProofCatalogueSpendRedeemerSchema
>;
export const FraudProofCatalogueSpendRedeemer =
  FraudProofCatalogueSpendRedeemerSchema as unknown as FraudProofCatalogueSpendRedeemer;

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
