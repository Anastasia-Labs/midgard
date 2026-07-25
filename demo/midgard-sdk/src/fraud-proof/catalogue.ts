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

export const FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = [
  "doubleSpend",
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "invalidRange",
  "transitionTrace",
  "validationTraceDispute",
] as const satisfies readonly (keyof FraudProofs)[];

export type FraudProofCatalogueCategoryName =
  (typeof FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number];

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
