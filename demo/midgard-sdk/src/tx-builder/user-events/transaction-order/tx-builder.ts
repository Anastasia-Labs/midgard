import {
  Data,
  LucidEvolution,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Datum, TransactionOrderParams } from "./types.js";
import {
  bufferToHex,
  hashHexWithBlake2b256,
  HashingError,
  LucidError,
} from "@/utils/common.js";
import { Effect } from "effect";
import { MintRedeemer } from "../deposit/types.js";


/**
 * TransactionOrder
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const transactionOrderTxBuilder = (
  lucid: LucidEvolution,
  params: TransactionOrderParams,
): Effect.Effect<TxBuilder, HashingError | LucidError> =>
  Effect.gen(function* () {
    const redeemer: MintRedeemer = {
      AuthenticateEvent: {
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      },
    };
    const authenticateEvent = Data.to(redeemer, MintRedeemer);
    const utxos: UTxO[] = yield* Effect.promise(() =>
      lucid.wallet().getUtxos(),
    );
    if (utxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the deposit transaction",
        cause: "No UTxOs found in wallet",
      });
    }
    const inputUtxo = utxos[0];
    const assetNameBuffer = yield* hashHexWithBlake2b256(inputUtxo.txHash);
    const assetName = bufferToHex(Buffer.from(assetNameBuffer, "hex"));
    const txOrderNFT = toUnit(params.policyId, assetName);
    const currDatum: Datum = {
      event: {
        txOrderId: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        midgardTx:  {
          body:params.midgardTxBody,
          wits: params.midgardTxWits,
          is_valid: true
        }
      },
      inclusionTime: params.inclusionTime, //Txn's time-validity upper bound event_wait_duration,
      refundAddress: params.refundAddress,
      refundDatum: params.refundDatum
    };
    const txOrderDatum = Data.to(currDatum, Datum);
    const tx = lucid
      .newTx()
      .collectFrom([inputUtxo])
      .mintAssets(
        {
          [txOrderNFT]: 1n,
        },
        authenticateEvent,
      )
      .pay.ToAddressWithData(
        params.txOrderAddress,
        {
          kind: "inline",
          value: txOrderDatum,
        },
        { [txOrderNFT]: 1n },
      )
      .validTo(Number(params.inclusionTime))
      .attach.MintingPolicy(params.mintingPolicy);
    return tx;
  });
