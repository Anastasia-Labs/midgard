import {
  Address,
  CML,
  Data,
  fromHex,
  LucidEvolution,
  PolicyId,
  Script,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import {
  DataCoercionError,
  getStateToken,
  makeReturn,
  OutputReference,
  UnauthenticUtxoError,
  utxosAtByNFTPolicyId,
} from "@/common.js";
import {
  AddressData,
  AddressSchema,
  POSIXTime,
  POSIXTimeSchema,
  HashingError,
  LucidError,
  hashHexWithBlake2b256,
} from "@/common.js";
import { MidgardTxCompact, TxOrderEventSchema } from "@/ledger-state.js";
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

export type TxOrderUTxO = {
  utxo: UTxO;
  datum: TxOrderDatum;
  assetName: string;
  idCbor: Buffer;
  infoCbor: Buffer;
  inclusionTime: Date;
};

export const TxOrderMintRedeemerSchema = Data.Enum([
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
export type TxOrderMintRedeemer = Data.Static<typeof TxOrderMintRedeemerSchema>;
export const TxOrderMintRedeemer =
  TxOrderMintRedeemerSchema as unknown as TxOrderMintRedeemer;

export type TxOrderFetchConfig = {
  txOrderAddress: Address;
  txOrderPolicyId: PolicyId;
  inclusionTimeUpperBound: POSIXTime;
  inclusionTimeLowerBound: POSIXTime;
};

export const getTxOrderDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<TxOrderDatum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const txOrderDatum = Data.from(datumCBOR, TxOrderDatum);
      return Effect.succeed(txOrderDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a tx order datum`,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Tx order datum coercion failed`,
        cause: `No datum found`,
      }),
    );
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToTxOrderUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<TxOrderUTxO, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const datum = yield* getTxOrderDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `TxOrderUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the tx order's",
        }),
      );
    }
    return {
      utxo,
      datum,
      assetName,
      idCbor: Buffer.from(
        fromHex(Data.to(datum.event.txOrderId, OutputReference)),
      ),
      infoCbor: Buffer.from(
        fromHex(Data.to(datum.event.midgardTx, MidgardTxCompact)),
      ),
      inclusionTime: new Date(Number(datum.inclusionTime)),
    };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToTxOrderUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<TxOrderUTxO[]> => {
  const effects = utxos.map((u) => utxoToTxOrderUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

const isUTxOTimeValid = (
  txOrderUTxO: TxOrderUTxO,
  inclusionStartTime: POSIXTime,
  inclusionEndTime: POSIXTime,
): boolean => {
  const txOrderData = txOrderUTxO.datum;
  return (
    inclusionStartTime < txOrderData.inclusionTime &&
    txOrderData.inclusionTime <= inclusionEndTime
  );
};

export const fetchTxOrderUTxOsProgram = (
  lucid: LucidEvolution,
  config: TxOrderFetchConfig,
): Effect.Effect<TxOrderUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.txOrderAddress,
      config.txOrderPolicyId,
    );
    const txOrderUTxOs = yield* utxosToTxOrderUTxOs(
      allUTxOs,
      config.txOrderPolicyId,
    );

    const validTxOrderUTxOs = txOrderUTxOs.filter((utxo) =>
      isUTxOTimeValid(
        utxo,
        config.inclusionTimeLowerBound,
        config.inclusionTimeUpperBound,
      ),
    );
    return validTxOrderUTxOs;
  });

export const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  config: TxOrderFetchConfig,
) => makeReturn(fetchTxOrderUTxOsProgram(lucid, config));

/**
 * TransactionOrder
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteTxOrderProgram = (
  lucid: LucidEvolution,
  params: TxOrderParams,
): Effect.Effect<TxBuilder, HashingError | LucidError> =>
  Effect.gen(function* () {
    const redeemer: TxOrderMintRedeemer = {
      AuthenticateEvent: {
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      },
    };
    const authenticateEvent = Data.to(redeemer, TxOrderMintRedeemer);
    const utxos: UTxO[] = yield* Effect.promise(() =>
      lucid.wallet().getUtxos(),
    );
    if (utxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the tx order transaction",
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
