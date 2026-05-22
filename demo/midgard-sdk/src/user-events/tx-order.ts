import {
  Data,
  fromHex,
  LucidEvolution,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { GenericErrorFields, makeReturn, ProofSchema } from "@/common.js";
import {
  AddressData,
  AddressSchema,
  H32,
  HashingError,
  LucidError,
  POSIXTimeSchema,
  UnspecifiedNetworkError,
} from "@/common.js";
import { authenticateUTxOs, AuthenticUTxO } from "@/internals.js";
import {
  CardanoDatum,
  CardanoDatumSchema,
  MidgardTxCompact,
  MidgardTxValiditySchema,
  TxOrderEventSchema,
} from "@/ledger-state.js";
import { completeTxWithLocalUPLCEvalProgram } from "@/tx-completion.js";

import {
  buildUserEventMintTransaction,
  encodeUserEventAuthenticateMintRedeemer,
  fetchUserEventUTxOsProgram,
  findInclusionTimeForUserEvent,
  getNonceInputAndAssetName,
  UserEventExtraFields,
  UserEventFetchConfig,
  userEventWitnessScriptHash,
} from "./internals.js";

export const TxOrderDatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
  refund_address: AddressSchema,
  refund_datum: CardanoDatumSchema,
});
export type TxOrderDatum = Data.Static<typeof TxOrderDatumSchema>;
export const TxOrderDatum = TxOrderDatumSchema as unknown as TxOrderDatum;
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

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToTxOrderUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<TxOrderUTxO[]> =>
  authenticateUTxOs<TxOrderDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    TxOrderDatum,
    (datum) => ({
      idCbor: Buffer.from(fromHex(Data.to(datum.event.id, H32))),
      infoCbor: Buffer.from(fromHex(Data.to(datum.event.tx, MidgardTxCompact))),
      inclusionTime: new Date(Number(datum.inclusion_time)),
    }),
  );

export const fetchTxOrderUTxOsProgram = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
): Effect.Effect<TxOrderUTxO[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToTxOrderUTxOs(utxos, config.eventPolicyId),
  );

export const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
) => makeReturn(fetchTxOrderUTxOsProgram(lucid, config));

export type TxOrderParams = {
  txOrderScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  nonceUTxO?: UTxO;
  txId: H32;
  tx: MidgardTxCompact;
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
        id: params.txId,
        tx: params.tx,
      },
      inclusion_time: BigInt(inclusionTime),
      witness: userEventWitnessScriptHash(assetName),
      refund_address: params.refundAddress,
      refund_datum: params.refundDatum ?? "NoDatum",
    };
    const txOrderDatumCBOR = Data.to(txOrderDatum, TxOrderDatum);

    return buildUserEventMintTransaction({
      lucid,
      inputUtxo,
      nft: txOrderNFT,
      mintRedeemer: encodeUserEventAuthenticateMintRedeemer({
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      }),
      scriptAddress: params.txOrderScriptAddress,
      datum: txOrderDatumCBOR,
      validTo: inclusionTime,
      mintingPolicy: params.mintingPolicy,
    });
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
    return yield* completeTxWithLocalUPLCEvalProgram(
      commitTx,
      (e) =>
        new TxOrderError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    );
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
