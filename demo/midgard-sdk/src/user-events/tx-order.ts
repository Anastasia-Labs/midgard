import {
  CML,
  Data,
  LucidEvolution,
  Script,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { AddressData, AddressSchema, POSIXTime, POSIXTimeSchema } from "@/tx-builder/common.js";
import {
  HashingError,
  LucidError,
  hashHexWithBlake2b256,
} from "@/common.js";
import { TxOrderEventSchema } from "@/ledger-state.js";
import { Effect } from "effect";

export type TxOrderParams = {
  txOrderAddress: string;
  mintingPolicy: Script;
  policyId: string;
  refundAddress: AddressData;
  refundDatum: string;
  inclusionTime: POSIXTime;
  midgardTxBody: string;
  midgardTxWits: string; 
  cardanoTx: CML.Transaction; // temporary until midgard tx conversion is done
};

export const TxOrderDatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusionTime: POSIXTimeSchema,
  refundAddress: AddressSchema,
  refundDatum: Data.Nullable(Data.Bytes()),
});
export type TxOrderDatum = Data.Static<typeof TxOrderDatumSchema>;
export const TxOrderDatum = TxOrderDatumSchema as unknown as TxOrderDatum;

export const MintRedeemerSchema = Data.Enum([
  Data.Object({
    AuthenticateEvent: Data.Object({
      nonceInputIndex: Data.Integer(),
      eventOutputIndex: Data.Integer(),
      hubRefInputIndex: Data.Integer(),
      witnessRegistrationRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    BurnEventNFT: Data.Object({
      nonceAssetName: Data.Bytes(),
      witnessUnregistrationRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;
/**
 * TransactionOrder
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const transactionOrderTxBuilder = (
  lucid: LucidEvolution,
  params: TxOrderParams,
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
    const midgardTxBody = params.cardanoTx.body().to_cbor_hex();
    const midgardTxWits = params.cardanoTx.witness_set().to_cbor_hex();

    const currDatum: TxOrderDatum = {
      event: {
        txOrderId: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        midgardTx: {
          body: midgardTxBody,
          wits: midgardTxWits,
          is_valid: true,
        },
      },
      inclusionTime: params.inclusionTime, //Txn's time-validity upper bound event_wait_duration,
      refundAddress: params.refundAddress,
      refundDatum: params.refundDatum,
    };
    const txOrderDatum = Data.to(currDatum, TxOrderDatum);
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
