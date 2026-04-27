import {
  AddressData,
  AddressSchema,
  GenericErrorFields,
  HashingError,
  LucidError,
  OutputReference,
  POSIXTimeSchema,
  ProofSchema,
  UnspecifiedNetworkError,
  makeReturn,
} from "@/common.js";
import { AuthenticUTxO, authenticateUTxOs } from "@/internals.js";
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
} from "./common.js";
import {
  CardanoDatum,
  CardanoDatumSchema,
  WithdrawalBody,
  WithdrawalEventSchema,
  WithdrawalInfo,
  WithdrawalSignature,
  WithdrawalValiditySchema,
} from "@/ledger-state.js";
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

export const WithdrawalOrderDatumSchema = Data.Object({
  event: WithdrawalEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
  refund_address: AddressSchema,
  refund_datum: CardanoDatumSchema,
});
export type WithdrawalOrderDatum = Data.Static<
  typeof WithdrawalOrderDatumSchema
>;
export const WithdrawalOrderDatum =
  WithdrawalOrderDatumSchema as unknown as WithdrawalOrderDatum;
export const WithdrawalMintRedeemerSchema = UserEventMintRedeemerSchema;
export type WithdrawalMintRedeemer = UserEventMintRedeemer;
export const WithdrawalMintRedeemer =
  UserEventMintRedeemer as unknown as WithdrawalMintRedeemer;
export const WithdrawalSpendPurposeSchema = Data.Enum([
  Data.Literal("InitializePayout"),
  Data.Object({
    Refund: Data.Object({
      validity_override: WithdrawalValiditySchema,
    }),
  }),
]);
export type WithdrawalSpendPurpose = Data.Static<
  typeof WithdrawalSpendPurposeSchema
>;
export const WithdrawalSpendPurpose =
  WithdrawalSpendPurposeSchema as unknown as WithdrawalSpendPurpose;
export const WithdrawalSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  burn_redeemer_index: Data.Integer(),
  payout_mint_redeemer_index: Data.Integer(),
  membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
  purpose: WithdrawalSpendPurposeSchema,
});
export type WithdrawalSpendRedeemer = Data.Static<
  typeof WithdrawalSpendRedeemerSchema
>;
export const WithdrawalSpendRedeemer =
  WithdrawalSpendRedeemerSchema as unknown as WithdrawalSpendRedeemer;

export type WithdrawalUTxO = AuthenticUTxO<
  WithdrawalOrderDatum,
  UserEventExtraFields
>;

export type WithdrawalFetchConfig = UserEventFetchConfig;

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToWithdrawalUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<WithdrawalUTxO[]> => {
  const calculateExtraFields = (
    datum: WithdrawalOrderDatum,
  ): UserEventExtraFields => ({
    idCbor: Buffer.from(fromHex(Data.to(datum.event.id, OutputReference))),
    infoCbor: Buffer.from(fromHex(Data.to(datum.event.info, WithdrawalInfo))),
    inclusionTime: new Date(Number(datum.inclusion_time)),
  });

  return authenticateUTxOs<WithdrawalOrderDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    WithdrawalOrderDatum,
    calculateExtraFields,
  );
};

export const fetchWithdrawalUTxOsProgram = (
  lucid: LucidEvolution,
  config: WithdrawalFetchConfig,
): Effect.Effect<WithdrawalUTxO[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToWithdrawalUTxOs(utxos, config.eventPolicyId),
  );

export const fetchWithdrawalUTxOs = (
  lucid: LucidEvolution,
  config: WithdrawalFetchConfig,
) => makeReturn(fetchWithdrawalUTxOsProgram(lucid, config));

export type WithdrawalOrderParams = {
  withdrawalScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  nonceUTxO?: UTxO;
  withdrawalBody: WithdrawalBody;
  withdrawalSignature: WithdrawalSignature;
  refundAddress: AddressData;
  refundDatum?: CardanoDatum;
};

/**
 * WithdrawalOrder
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteWithdrawalTxProgram = (
  lucid: LucidEvolution,
  params: WithdrawalOrderParams,
): Effect.Effect<
  TxBuilder,
  HashingError | LucidError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const { inputUtxo, assetName } = yield* getNonceInputAndAssetName(
      lucid,
      "withdrawal",
      params.nonceUTxO,
    );

    const withdrawalNFT = toUnit(params.policyId, assetName);

    const inclusionTime = yield* findInclusionTimeForUserEvent(lucid);

    const withdrawalOrderDatum: WithdrawalOrderDatum = {
      event: {
        id: {
          transactionId: inputUtxo.txHash,
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        info: {
          body: params.withdrawalBody,
          signature: params.withdrawalSignature,
          validity: "WithdrawalIsValid",
        },
      },
      inclusion_time: BigInt(inclusionTime),
      witness: userEventWitnessScriptHash(assetName),
      refund_address: params.refundAddress,
      refund_datum: params.refundDatum ?? "NoDatum",
    };
    const withdrawalOrderDatumCBOR = Data.to(
      withdrawalOrderDatum,
      WithdrawalOrderDatum,
    );

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
      nft: withdrawalNFT,
      mintRedeemer: mintRedeemerCBOR,
      scriptAddress: params.withdrawalScriptAddress,
      datum: withdrawalOrderDatumCBOR,
      validTo: inclusionTime,
      mintingPolicy: params.mintingPolicy,
    });
    return tx;
  }).pipe(
    Effect.catchAllDefect((defect) => {
      return Effect.fail(
        new LucidError({
          message: "Caught defect from withdrawalTxBuilder",
          cause: defect,
        }),
      );
    }),
  );

export const unsignedWithdrawalTxProgram = (
  lucid: LucidEvolution,
  withdrawalParams: WithdrawalOrderParams,
): Effect.Effect<
  TxSignBuilder,
  HashingError | LucidError | UnspecifiedNetworkError | WithdrawalError
> =>
  Effect.gen(function* () {
    const commitTx = yield* incompleteWithdrawalTxProgram(
      lucid,
      withdrawalParams,
    );
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: true }),
      catch: (e) =>
        new WithdrawalError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for submitting withdrawal order using the provided
 * `LucidEvolution` instance and a withdrawal order config.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param withdrawalParams - Parameters required for committing withdrawal orders.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedWithdrawalTx = (
  lucid: LucidEvolution,
  withdrawalParams: WithdrawalOrderParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedWithdrawalTxProgram(lucid, withdrawalParams)).unsafeRun();

export class WithdrawalError extends EffectData.TaggedError(
  "WithdrawalError",
)<GenericErrorFields> {}
