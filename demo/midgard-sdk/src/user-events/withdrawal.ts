import {
  AddressData,
  AddressSchema,
  AuthenticUTxO,
  GenericErrorFields,
  HashingError,
  LucidError,
  OutputReference,
  POSIXTimeSchema,
  UnspecifiedNetworkError,
  makeReturn,
  utxosToAuthenticUTxOs,
} from "@/common.js";
import {
  buildUserEventMintTransaction,
  fetchUserEventUTxOsProgram,
  findInclusionTimeForUserEvent,
  getNonceInputAndAssetName,
  UserEventExtraFields,
  UserEventFetchConfig,
  UserEventMintRedeemer,
} from "./common.js";
import {
  WithdrawalBody,
  WithdrawalEventSchema,
  WithdrawalInfo,
  WithdrawalSignature,
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
  inclusionTime: POSIXTimeSchema,
  refundAddress: AddressSchema,
  refundDatum: Data.Nullable(Data.Any()),
});
export type WithdrawalOrderDatum = Data.Static<
  typeof WithdrawalOrderDatumSchema
>;
export const WithdrawalOrderDatum =
  WithdrawalOrderDatumSchema as unknown as WithdrawalOrderDatum;

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
    inclusionTime: new Date(Number(datum.inclusionTime)),
  });

  return utxosToAuthenticUTxOs<WithdrawalOrderDatum, UserEventExtraFields>(
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
  withdrawalBody: WithdrawalBody;
  withdrawalSignature: WithdrawalSignature;
  refundAddress: AddressData;
  refundDatum?: Data;
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
    );

    const withdrawalNFT = toUnit(params.policyId, assetName);

    const inclusionTime = yield* findInclusionTimeForUserEvent(lucid);

    const withdrawalOrderDatum: WithdrawalOrderDatum = {
      event: {
        id: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        info: {
          body: params.withdrawalBody,
          signature: params.withdrawalSignature,
          validity: "WithdrawalIsValid",
        },
      },
      inclusionTime: BigInt(inclusionTime),
      refundAddress: params.refundAddress,
      refundDatum: params.refundDatum ?? null,
    };
    const withdrawalOrderDatumCBOR = Data.to(
      withdrawalOrderDatum,
      WithdrawalOrderDatum,
    );

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
      try: () => commitTx.complete({ localUPLCEval: false }),
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
