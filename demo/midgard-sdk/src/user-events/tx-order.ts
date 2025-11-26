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
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import {
  AddressData,
  AddressSchema,
  DataCoercionError,
  GenericErrorFields,
  getStateToken,
  isEventUTxOInclusionTimeInBounds,
  makeReturn,
  MidgardAddressSchema,
  OutputReference,
  UnauthenticUtxoError,
  utxosAtByNFTPolicyId,
} from "@/common.js";
import {
  POSIXTime,
  POSIXTimeSchema,
  HashingError,
  LucidError,
  hashHexWithBlake2b256,
} from "@/common.js";
import { TxOrderEventSchema } from "@/ledger-state.js";
import {
  buildUserEventMintTransaction,
  UserEventMintRedeemer,
} from "./index.js";
import { Data as EffectData, Effect } from "effect";
import { getProtocolParameters } from "@/protocol-parameters.js";

export type TxOrderParams = {
  txOrderScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  refundAddress: AddressData;
  refundDatum: string;
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
      infoCbor: Buffer.from(fromHex(datum.event.midgardTx.tx)),
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
      isEventUTxOInclusionTimeInBounds(
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
export const incompleteTxOrderTxProgram = (
  lucid: LucidEvolution,
  params: TxOrderParams,
): Effect.Effect<TxBuilder, HashingError | LucidError> =>
  Effect.gen(function* () {
    const mintRedeemer: UserEventMintRedeemer = {
      AuthenticateEvent: {
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      },
    };
    const mintRedeemerCBOR = Data.to(mintRedeemer, UserEventMintRedeemer);
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

    const currTime = Date.now();
    const network = lucid.config().network ?? "Mainnet";
    const waitTime = getProtocolParameters(network).event_wait_duration;
    const inclusionTime = currTime + waitTime;

    const txOrderDatum: TxOrderDatum = {
      event: {
        txOrderId: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        midgardTx: {
          tx: params.cardanoTx.to_cbor_hex(),
          is_valid: true,
        },
      },
      inclusionTime: BigInt(inclusionTime), //Txn's time-validity upper bound event_wait_duration,
      refundAddress: params.refundAddress,
      refundDatum: params.refundDatum,
    };
    const txOrderDatumCBOR = Data.to(txOrderDatum, TxOrderDatum);
    const tx = buildUserEventMintTransaction({
      lucid,
      inputUtxo,
      nft: txOrderNFT,
      mintRedeemer: mintRedeemerCBOR,
      scriptAddress: params.txOrderScriptAddress,
      datum: txOrderDatumCBOR,
      validTo: inclusionTime,
      mintingPolicy: params.mintingPolicy,
    });
    return tx;
  });

export const unsignedTxOrderTxProgram = (
  lucid: LucidEvolution,
  depositParams: TxOrderParams,
): Effect.Effect<TxSignBuilder, HashingError | LucidError | TxOrderError> =>
  Effect.gen(function* () {
    const commitTx = yield* incompleteTxOrderTxProgram(lucid, depositParams);
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: false }),
      catch: (e) =>
        new TxOrderError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for submitting tx order using the provided
 * `LucidEvolution` instance and a tx order config.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param txOrderParams - Parameters required for commiting tx orders.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedTxOrderTx = (
  lucid: LucidEvolution,
  txOrderParams: TxOrderParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedTxOrderTxProgram(lucid, txOrderParams)).unsafeRun();

export class TxOrderError extends EffectData.TaggedError(
  "TxOrderError",
)<GenericErrorFields> {}
