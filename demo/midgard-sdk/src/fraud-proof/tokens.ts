import { Data } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

/**
 * SDK types and builders for the terminal fraud-proof token.
 *
 * This token represents the finalized output of a successful fraud-proof flow,
 * separate from the stepwise computation-thread tokens used during execution.
 */
export const FraudProofTokenDatumSchema = Data.Object({
  fraudProver: Data.Bytes(),
});
export type FraudProofTokenDatum = Data.Static<
  typeof FraudProofTokenDatumSchema
>;
export const FraudProofTokenDatum =
  FraudProofTokenDatumSchema as unknown as FraudProofTokenDatum;

/**
 * Spend redeemer for the terminal fraud-proof token.
 */
export const FraudProofTokenSpendRedeemerSchema = Data.Enum([
  Data.Literal("Never"),
]);
export type FraudProofTokenSpendRedeemer = Data.Static<
  typeof FraudProofTokenSpendRedeemerSchema
>;
export const FraudProofTokenSpendRedeemer =
  FraudProofTokenSpendRedeemerSchema as unknown as FraudProofTokenSpendRedeemer;

/**
 * Mint redeemer describing the transition from the final computation-thread
 * step into the terminal fraud-proof token.
 */
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
 * Stub entry point for minting the terminal fraud-proof token.
 */
export const incompleteFraudProofTokenMintTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofTokenMintParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for burning the terminal fraud-proof token.
 */
export const incompleteFraudProofTokenBurnTxProgram = (
  lucid: LucidEvolution,
  params: FraudProofTokenBurnParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
