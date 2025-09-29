import { Data, fromText, LucidEvolution, Script, TxBuilder} from "@lucid-evolution/lucid";
import { DepositDatum, DepositEvent, MintRedeemer } from "./types.js";

export type DepositParams = {
  mintingPolicy: Script,
  policyId: string,
  witnessScript: Script,
  depositEvent: DepositEvent,
  assetName: string,
  deposit_address: string
  validity_range: number
  mintRedeemer: MintRedeemer,
  refund_address: string,
  refund_datum: string
};

/**
 * Deposit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */

export const depositTxBuilder = (
  lucid: LucidEvolution,
  params: DepositParams,
): TxBuilder => {

  const currDatum: DepositDatum = {
  event: params.depositEvent,
  inclusion_time: 900000n,
  witness: "00".repeat(28),
  refund_address: params.refund_address,
  refund_datum: params.refund_datum
};
  const depositDatum = Data.to<DepositDatum>(currDatum, DepositDatum);
  const depositNFT = params.policyId+params.assetName;
  const authenticateEvent = Data.to(params.mintRedeemer, MintRedeemer);
  const tx = lucid
    .newTx()
    .mintAssets({
      [depositNFT]: 1n
    },authenticateEvent)
    .pay.ToAddressWithData(params.deposit_address,{
      kind: "inline",
      value: depositDatum,
    },{[depositNFT]: 1n })
    .validTo(params.validity_range)
    .attach.MintingPolicy(params.mintingPolicy)
    return tx;
};

export * from "./types.js";
