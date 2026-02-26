import {
  CML,
  Data,
  fromHex,
  LucidEvolution,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { GenericErrorFields, makeReturn, OutputReference } from "@/common.js";
import { AuthenticUTxO, authenticateUTxOs } from "@/internals.js";
import {
  AddressData,
  AddressSchema,
  POSIXTimeSchema,
  HashingError,
  LucidError,
  UnspecifiedNetworkError,
} from "@/common.js";
import { TxOrderEventSchema } from "@/ledger-state.js";
import {
  buildUserEventMintTransaction,
  fetchUserEventUTxOsProgram,
  findInclusionTimeForUserEvent,
  getNonceInputAndAssetName,
  UserEventExtraFields,
  UserEventFetchConfig,
  UserEventMintRedeemer,
} from "./internals.js";
import { Data as EffectData, Effect } from "effect";

export const TxOrderDatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusionTime: POSIXTimeSchema,
  refundAddress: AddressSchema,
  refundDatum: Data.Nullable(Data.Any()),
});
export type TxOrderDatum = Data.Static<typeof TxOrderDatumSchema>;
export const TxOrderDatum = TxOrderDatumSchema as unknown as TxOrderDatum;

export type TxOrderUTxO = AuthenticUTxO<TxOrderDatum, UserEventExtraFields>;

export type TxOrderFetchConfig = UserEventFetchConfig;

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToTxOrderUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<TxOrderUTxO[]> => {
  const calculateExtraFields = (datum: TxOrderDatum): UserEventExtraFields => ({
    idCbor: Buffer.from(fromHex(Data.to(datum.event.id, OutputReference))),
    infoCbor: Buffer.from(fromHex(datum.event.tx)),
    inclusionTime: new Date(Number(datum.inclusionTime)),
  });

  return authenticateUTxOs<TxOrderDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    TxOrderDatum,
    calculateExtraFields,
  );
};

export const fetchTxOrderUTxOsProgram = (
  lucid: LucidEvolution,
  config: TxOrderFetchConfig,
): Effect.Effect<TxOrderUTxO[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToTxOrderUTxOs(utxos, config.eventPolicyId),
  );

export const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  config: TxOrderFetchConfig,
) => makeReturn(fetchTxOrderUTxOsProgram(lucid, config));

export type TxOrderParams = {
  txOrderScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  cardanoTx: CML.Transaction; // temporary until midgard tx conversion is done
  refundAddress: AddressData;
  refundDatum?: Data;
};

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
): Effect.Effect<
  TxBuilder,
  HashingError | LucidError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const { inputUtxo, assetName } = yield* getNonceInputAndAssetName(
      lucid,
      "tx order",
    );
    const txOrderNFT = toUnit(params.policyId, assetName);

    const inclusionTime = yield* findInclusionTimeForUserEvent(lucid);

    const txOrderDatum: TxOrderDatum = {
      event: {
        id: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        tx: params.cardanoTx.to_cbor_hex(),
      },
      inclusionTime: BigInt(inclusionTime),
      refundAddress: params.refundAddress,
      refundDatum: params.refundDatum ?? null,
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
): Effect.Effect<
  TxSignBuilder,
  HashingError | LucidError | UnspecifiedNetworkError | TxOrderError
> =>
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
