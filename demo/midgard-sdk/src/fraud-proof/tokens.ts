import { Data } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export const FraudProofTokenDatumSchema = Data.Object({
  fraudProver: Data.Bytes(),
});
export type FraudProofTokenDatum = Data.Static<
  typeof FraudProofTokenDatumSchema
>;
export const FraudProofTokenDatum =
  FraudProofTokenDatumSchema as unknown as FraudProofTokenDatum;

export const FraudProofTokenSpendRedeemerSchema = Data.Enum([
  Data.Literal("Never"),
]);
export type FraudProofTokenSpendRedeemer = Data.Static<
  typeof FraudProofTokenSpendRedeemerSchema
>;
export const FraudProofTokenSpendRedeemer =
  FraudProofTokenSpendRedeemerSchema as unknown as FraudProofTokenSpendRedeemer;

export const FraudProofTokenMintRedeemerSchema = Data.Object({
  hubOracleRefInputIndex: Data.Integer(),
  fraudProofLastStepInputIndex: Data.Integer(),
  computationThreadTokenAssetName: Data.Bytes(),
  outputWithFraudProofIndex: Data.Integer(),
  hubOracleAssetName: Data.Bytes(),
});
export type FraudProofTokenMintRedeemer = Data.Static<
  typeof FraudProofTokenMintRedeemerSchema
>;
export const FraudProofTokenMintRedeemer =
  FraudProofTokenMintRedeemerSchema as unknown as FraudProofTokenMintRedeemer;

export type FraudProofTokenMintParams = {};
export type FraudProofTokenBurnParams = {};

/**
 * Mint
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofTokenMintTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofTokenMintParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Burn
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteFraudProofTokenBurnTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofTokenBurnParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
