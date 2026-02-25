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
  AuthenticUTxO,
  GenericErrorFields,
  isEventUTxOInclusionTimeInBounds,
  makeReturn,
  OutputReference,
  utxosToAuthenticUTxOs,
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
import { TxOrderEventSchema } from "@/ledger-state.js";
import {
  buildUserEventMintTransaction,
  UserEventExtraFields,
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

export type TxOrderUTxO = AuthenticUTxO<TxOrderDatum, UserEventExtraFields>;

export type TxOrderFetchConfig = {
  txOrderAddress: Address;
  txOrderPolicyId: PolicyId;
  inclusionTimeUpperBound: POSIXTime;
  inclusionTimeLowerBound: POSIXTime;
};

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToTxOrderUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
  datum: TxOrderDatum,
): Effect.Effect<TxOrderUTxO[]> => {
  const calculateExtraFields = (datum: TxOrderDatum): UserEventExtraFields => ({
    idCbor: Buffer.from(
      fromHex(Data.to(datum.event.txOrderId, OutputReference)),
    ),
    infoCbor: Buffer.from(fromHex(datum.event.midgardTx.tx)),
    inclusionTime: new Date(Number(datum.inclusionTime)),
  });

  return utxosToAuthenticUTxOs<TxOrderDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    datum,
    calculateExtraFields,
  );
};

export const fetchTxOrderUTxOsProgram = (
  lucid: LucidEvolution,
  config: TxOrderFetchConfig,
): Effect.Effect<TxOrderUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(config.txOrderAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch tx order UTxOs",
          cause: err,
        }),
    });
    const txOrderUTxOs = yield* utxosToTxOrderUTxOs(
      allUTxOs,
      config.txOrderPolicyId,
      TxOrderDatum,
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
    const utxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch wallet UTxOs",
          cause: err,
        }),
    });

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

    const mintRedeemer: UserEventMintRedeemer = {
      AuthenticateEvent: {
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      },
    };
    const mintRedeemerCBOR = Data.to(mintRedeemer, UserEventMintRedeemer);

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
  }).pipe(
    Effect.catchAllDefect((defect) => {
      return Effect.fail(
        new LucidError({
          message: "Caught defect from txOrderTxBuilder",
          cause: defect,
        }),
      );
    }),
  );

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
