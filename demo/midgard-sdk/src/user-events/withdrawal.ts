import {
  AddressData,
  AddressSchema,
  AuthenticUTxO,
  GenericErrorFields,
  HashingError,
  LucidError,
  OutputReference,
  POSIXTime,
  POSIXTimeSchema,
  hashHexWithBlake2b256,
  isEventUTxOInclusionTimeInBounds,
  makeReturn,
  utxosToAuthenticUTxOs,
} from "@/common.js";
import {
  buildUserEventMintTransaction,
  UserEventExtraFields,
  UserEventMintRedeemer,
} from "./common.js";
import {
  WithdrawalBody,
  WithdrawalEventSchema,
  WithdrawalInfo,
  WithdrawalSignature,
} from "@/ledger-state.js";
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
import { getProtocolParameters } from "@/protocol-parameters.js";
import { Data as EffectData, Effect } from "effect";

export type WithdrawalOrderParams = {
  withdrawalScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  withdrawalBody: WithdrawalBody;
  withdrawalSignature: WithdrawalSignature;
  refundAddress: AddressData;
  refundDatum: Data;
};

export const WithdrawalOrderDatumSchema = Data.Object({
  event: WithdrawalEventSchema,
  inclusionTime: POSIXTimeSchema,
  refundAddress: AddressSchema,
  refundDatum: Data.Any(),
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

export type WithdrawalFetchConfig = {
  withdrawalAddress: Address;
  withdrawalPolicyId: PolicyId;
  inclusionTimeUpperBound?: POSIXTime;
  inclusionTimeLowerBound?: POSIXTime;
};

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToWithdrawalUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
  datum: WithdrawalOrderDatum,
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
    datum,
    calculateExtraFields,
  );
};

export const fetchWithdrawalUTxOsProgram = (
  lucid: LucidEvolution,
  config: WithdrawalFetchConfig,
): Effect.Effect<WithdrawalUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(config.withdrawalAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch withdrawal UTxOs",
          cause: err,
        }),
    });
    const withdrawalUTxOs = yield* utxosToWithdrawalUTxOs(
      allUTxOs,
      config.withdrawalPolicyId,
      WithdrawalOrderDatum,
    );

    const validWithdrawalUTxOs = withdrawalUTxOs.filter((utxo) =>
      isEventUTxOInclusionTimeInBounds(
        utxo,
        config.inclusionTimeLowerBound,
        config.inclusionTimeUpperBound,
      ),
    );
    return validWithdrawalUTxOs;
  });

export const fetchWithdrawalUTxOs = (
  lucid: LucidEvolution,
  config: WithdrawalFetchConfig,
) => makeReturn(fetchWithdrawalUTxOsProgram(lucid, config));

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
        message: "Failed to build the withdrawal transaction",
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

    const withdrawalNFT = toUnit(params.policyId, assetName);

    const currTime = Date.now();
    const network = lucid.config().network ?? "Mainnet";
    const waitTime = getProtocolParameters(network).event_wait_duration;
    const inclusionTime = currTime + waitTime;

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
      refundDatum: params.refundDatum,
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
): Effect.Effect<TxSignBuilder, HashingError | LucidError | WithdrawalError> =>
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
