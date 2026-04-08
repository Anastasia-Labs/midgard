import { AuthenticatedValidator } from "@/common.js";
import { FRAUD_PROOF_CATALOGUE_ASSET_NAME } from "@/constants.js";
import { Assets, Data, toUnit } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";

/**
 * SDK helpers for the fraud-proof catalogue state machine.
 *
 * The catalogue anchors the set of recognized fraud-proof categories, so its
 * initialization path is part of the protocol's safety-critical bootstrap
 * sequence.
 */
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

/**
 * Spend redeemer selecting a catalogue entry by asset name.
 */
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

export type FraudProofCatalogueDeinitParams = {};
export type FraudProofCatalogueNewCategoryParams = {};
export type FraudProofCatalogueRemoveCategoryParams = {};

/**
 * Builds the fraud-proof catalogue initialization transaction fragment.
 *
 * The initial datum stores the Merkle Patricia Trie root that identifies the
 * catalogue contents expected by the rest of the protocol.
 */
export const incompleteFraudProofCatalogueInitTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueInitParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const assets: Assets = {
      [toUnit(params.validator.policyId, FRAUD_PROOF_CATALOGUE_ASSET_NAME)]: 1n,
    };

    const datum = Data.to(params.mptRootHash, FraudProofCatalogueDatum);

    const tx = lucid
      .newTx()
      .mintAssets(assets, Data.to("Init", FraudProofCatalogueMintRedeemer))
      .pay.ToAddressWithData(
        params.validator.spendingScriptAddress,
        { kind: "inline", value: datum },
        assets,
      )
      .attach.Script(params.validator.mintingScript);

    return tx;
  });
/**
 * Stub entry point for tearing down the fraud-proof catalogue.
 */
export const incompleteFraudProofDeinitTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueDeinitParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for adding a new fraud-proof category.
 */
export const incompleteFraudProofNewCategoryTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueNewCategoryParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for removing a fraud-proof category.
 */
export const incompleteFraudProofRemoveCategoryTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueRemoveCategoryParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
