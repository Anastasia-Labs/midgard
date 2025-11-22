import {
  AddressData,
  AddressSchema,
  GenericErrorFields,
  HashingError,
  LucidError,
  POSIXTime,
  POSIXTimeSchema,
  hashHexWithBlake2b256,
  makeReturn,
  DataCoercionError,
  getStateToken,
  isEventUTxOInclusionTimeInBounds,
  OutputReference,
  UnauthenticUtxoError,
  utxosAtByNFTPolicyId,
} from "@/common.js";
import {
  buildUserEventMintTransaction,
  UserEventMintRedeemer,
} from "@/index.js";
import {
  WithdrawalBody,
  WithdrawalEventSchema,
  WithdrawalInfo,
  WithdrawalSignature,
} from "@/ledger-state.js";
import {
  Address,
  PolicyId,
  fromHex,
  CML,
  Data,
  LucidEvolution,
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

export type WithdrawalUTxO = {
  utxo: UTxO;
  datum: WithdrawalOrderDatum;
  assetName: string;
  idCbor: Buffer;
  infoCbor: Buffer;
  inclusionTime: Date;
};

export type WithdrawalFetchConfig = {
  withdrawalAddress: Address;
  withdrawalPolicyId: PolicyId;
  inclusionTimeUpperBound?: POSIXTime;
  inclusionTimeLowerBound?: POSIXTime;
};

export const getWithdrawalDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<WithdrawalOrderDatum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const withdrawalDatum = Data.from(datumCBOR, WithdrawalOrderDatum);
      return Effect.succeed(withdrawalDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a withdrawal datum`,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Withdrawal datum coercion failed`,
        cause: `No datum found`,
      }),
    );
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToWithdrawalUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<WithdrawalUTxO, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const datum = yield* getWithdrawalDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `WithdrawalUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the withdrawal's",
        }),
      );
    }
    return {
      utxo,
      datum,
      assetName,
      idCbor: Buffer.from(fromHex(Data.to(datum.event.id, OutputReference))),
      infoCbor: Buffer.from(fromHex(Data.to(datum.event.info, WithdrawalInfo))),
      inclusionTime: new Date(Number(datum.inclusionTime)),
    };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToWithdrawalUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<WithdrawalUTxO[]> => {
  const effects = utxos.map((u) => utxoToWithdrawalUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

export const fetchWithdrawalUTxOsProgram = (
  lucid: LucidEvolution,
  config: WithdrawalFetchConfig,
): Effect.Effect<WithdrawalUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.withdrawalAddress,
      config.withdrawalPolicyId,
    );
    const withdrawalUTxOs = yield* utxosToWithdrawalUTxOs(
      allUTxOs,
      config.withdrawalPolicyId,
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
      yield* Effect.fail(
        new LucidError({
          message: "Failed to build the withdrawal transaction",
          cause: "No UTxOs found in wallet",
        }),
      );
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
  });

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
