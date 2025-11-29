import { AuthenticatedValidator, POSIXTimeSchema } from "@/common.js";
import { Data } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export const FraudProofCatalogueDatumSchema = Data.Object({
  insertTime: POSIXTimeSchema,
  initStepScriptHash: Data.Bytes(),
});
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

export type FraudProofCatalogueDeinitParams = {};
export type FraudProofCatalogueNewCategoryParams = {};
export type FraudProofCatalogueRemoveCategoryParams = {};

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofInitTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueInitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Deinit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofDeinitTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueDeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * NewCategory
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofNewCategoryTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueNewCategoryParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * RemoveCategory
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofRemoveCategoryTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofCatalogueRemoveCategoryParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
