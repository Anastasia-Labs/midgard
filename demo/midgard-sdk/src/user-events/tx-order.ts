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
import {
  GenericErrorFields,
  makeReturn,
  OutputReference,
  ProofSchema,
} from "@/common.js";
import { AuthenticUTxO, authenticateUTxOs } from "@/internals.js";
import {
  AddressData,
  AddressSchema,
  POSIXTimeSchema,
  HashingError,
  LucidError,
  UnspecifiedNetworkError,
} from "@/common.js";
import {
  CardanoDatum,
  CardanoDatumSchema,
  MidgardTxValiditySchema,
  TxOrderEventSchema,
} from "@/ledger-state.js";
import {
  buildUserEventMintTransaction,
  fetchUserEventUTxOsProgram,
  findInclusionTimeForUserEvent,
  getNonceInputAndAssetName,
  UserEventExtraFields,
  UserEventFetchConfig,
  UserEventMintRedeemer,
  UserEventMintRedeemerSchema,
  userEventWitnessScriptHash,
} from "./internals.js";
import { Data as EffectData, Effect } from "effect";

export const TxOrderDatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
  refund_address: AddressSchema,
  refund_datum: CardanoDatumSchema,
});
export type TxOrderDatum = Data.Static<typeof TxOrderDatumSchema>;
export const TxOrderDatum = TxOrderDatumSchema as unknown as TxOrderDatum;
export const TxOrderMintRedeemerSchema = UserEventMintRedeemerSchema;
export type TxOrderMintRedeemer = UserEventMintRedeemer;
export const TxOrderMintRedeemer =
  UserEventMintRedeemer as unknown as TxOrderMintRedeemer;
export const TxOrderSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  burn_redeemer_index: Data.Integer(),
  membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
  validity_override: MidgardTxValiditySchema,
});
export type TxOrderSpendRedeemer = Data.Static<
  typeof TxOrderSpendRedeemerSchema
>;
export const TxOrderSpendRedeemer =
  TxOrderSpendRedeemerSchema as unknown as TxOrderSpendRedeemer;

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
    inclusionTime: new Date(Number(datum.inclusion_time)),
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
  nonceUTxO?: UTxO;
  cardanoTx: CML.Transaction; // temporary until midgard tx conversion is done
  refundAddress: AddressData;
  refundDatum?: CardanoDatum;
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
      params.nonceUTxO,
    );
    const txOrderNFT = toUnit(params.policyId, assetName);

    const inclusionTime = yield* findInclusionTimeForUserEvent(lucid);

    const txOrderDatum: TxOrderDatum = {
      event: {
        id: {
          transactionId: inputUtxo.txHash,
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        tx: params.cardanoTx.to_cbor_hex(),
      },
      inclusion_time: BigInt(inclusionTime),
      witness: userEventWitnessScriptHash(assetName),
      refund_address: params.refundAddress,
      refund_datum: params.refundDatum ?? "NoDatum",
    };
    const txOrderDatumCBOR = Data.to(txOrderDatum, TxOrderDatum);

    const mintRedeemer: UserEventMintRedeemer = {
      AuthenticateEvent: {
        nonce_input_index: 0n,
        event_output_index: 0n,
        hub_ref_input_index: 0n,
        witness_registration_redeemer_index: 0n,
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
      try: () => commitTx.complete({ localUPLCEval: true }),
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
