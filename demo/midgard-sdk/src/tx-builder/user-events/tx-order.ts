import {
  CML,
  Data,
  LucidEvolution,
  Script,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { POSIXTime, POSIXTimeSchema } from "@/tx-builder/common.js";
import {
  HashingError,
  LucidError,
  hashHexWithBlake2b256,
} from "@/utils/common.js";
import { TxOrderEventSchema } from "@/tx-builder/ledger-state.js";
import { Effect } from "effect";

export type TransactionOrderParams = {
  txOrderAddress: string; 
  mintingPolicy: Script;
  policyId: string;
  refundAddress: string,
  refundDatum: string;
  inclusionTime: POSIXTime;
  midgardTxBody: string;
  midgardTxWits: string;
};

export const DatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusionTime: POSIXTimeSchema,
  refundAddress: Data.Bytes(),
  refundDatum: Data.Nullable(Data.Bytes())
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;


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
    const transactionInput = CML.TransactionInput.new(
      CML.TransactionHash.from_hex(inputUtxo.txHash),
      BigInt(inputUtxo.outputIndex),
    );

    const assetName = yield* hashHexWithBlake2b256(
      transactionInput.to_cbor_hex(),
    );
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
