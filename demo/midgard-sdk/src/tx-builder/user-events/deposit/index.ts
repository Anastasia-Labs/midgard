import { Data, Integer, LucidEvolution, RedeemerBuilder, Script, toUnit, TransactionError, TxBuilder} from "@lucid-evolution/lucid";
import { Datum, DepositInfo, MintRedeemer } from "./types.js";
import { POSIXTime } from "@/tx-builder/common.js";
import { bufferToHex, hashHexWithBlake2b256 } from "@/utils/common.js";
import { Effect } from "effect";

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

export const depositTxBuilder = (
  lucid: LucidEvolution,
  params: DepositParams,
): Effect.Effect<TxBuilder, TransactionError | unknown > =>
  Effect.gen(function* () {

  const utxos = yield* Effect.promise(() => lucid.wallet().getUtxos());
  if (utxos.length === 0) {
    throw new Error("No UTxOs found in wallet");
  }
  const inputUtxo = utxos[0];
  const assetNameBuffer = yield* hashHexWithBlake2b256(inputUtxo.txHash);
  const assetName = bufferToHex(Buffer.from(assetNameBuffer,'hex'));
  const depositNFT = toUnit(params.policyId,assetName);
  const currDatum: Datum = {
  event: {id: { txHash: { hash: inputUtxo.txHash }, outputIndex: BigInt(inputUtxo.outputIndex)}, info : params.depositInfo},
  inclusion_time: params.inclusionTime,
 };
   const depositDatum = Data.to(currDatum, Datum);
   const authenticateEvent: RedeemerBuilder = {
    kind: "selected",
    makeRedeemer: (inputIdxs: bigint[]) => {
      const tupleList = inputIdxs.map((inputIdx, i) => [inputIdx, BigInt(i)]);
      
      const redeemer: MintRedeemer = {
        AuthenticateEvent: {
          nonce_input_index: tupleList[0][0],      
          event_output_index: 0n,       
          hub_ref_input_index: 0n,                   
          witness_registration_redeemer_index: 0n    
        }
      };
      
      return Data.to(redeemer, MintRedeemer);
    },
    inputs: [inputUtxo], 
  };
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
});

export * from "./types.js";
