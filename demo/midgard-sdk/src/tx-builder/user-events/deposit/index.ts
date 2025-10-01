import { Data, Integer, LucidEvolution, Script, TxBuilder} from "@lucid-evolution/lucid";
import { DepositDatum, DepositInfo, MintRedeemer } from "./types.js";
import { POSIXTime } from "@/tx-builder/common.js";
import { sha3_256 } from "@noble/hashes/sha3";
import { Int } from "effect/Schema";


export type DepositParams = {
  depositScriptAddress: string,
  mintingPolicy: Script,
  policyId: string,
  depositInfo: DepositInfo,
  inclusionTime: POSIXTime,
};

/**
 * Deposit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */

export const depositTxBuilder = async (
  lucid: LucidEvolution,
  params: DepositParams,
): Promise<TxBuilder> => {

  const redeemer: MintRedeemer = {
    AuthenticateEvent: {
        nonce_input_index: 0n,
        event_output_index: 0n,
        hub_ref_input_index: 0n,
        witness_registration_redeemer_index: 0n
    }
  };
  const authenticateEvent = Data.to(redeemer,MintRedeemer);
  const utxos = await lucid.wallet().getUtxos();
  if (utxos.length === 0) {
    throw new Error("No UTxOs found in wallet");
  }
  const inputUtxo = utxos[0];
  const assetName = sha3_256(inputUtxo.txHash);
  const depositNFT = params.policyId+assetName;
  const currDatum: DepositDatum = {
  event: {id: { txHash: { hash: inputUtxo.txHash }, outputIndex: BigInt(inputUtxo.outputIndex)}, info : params.depositInfo},
  inclusion_time: params.inclusionTime,
 };
   const depositDatum = Data.to(currDatum, DepositDatum);
   const tx = lucid
    .newTx()
    .collectFrom([inputUtxo])
    .mintAssets({
      [depositNFT]: 1n
    },authenticateEvent)
    .pay.ToAddressWithData(params.depositScriptAddress,{
      kind: "inline",
      value: depositDatum,
    },{[depositNFT]: 1n })
    .validTo(Number(params.inclusionTime))
    .attach.MintingPolicy(params.mintingPolicy)
    return tx;
};

export * from "./types.js";
