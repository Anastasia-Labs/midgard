import {
  HashingError,
  hashHexWithBlake2b256,
  LucidError,
  POSIXTime,
  UnspecifiedNetworkError,
} from "@/common.js";
import { Effect } from "effect";
import {
  Address,
  Assets,
  CML,
  Data,
  LucidEvolution,
  MintingPolicy,
  PolicyId,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { getProtocolParameters } from "@/protocol-parameters.js";

const eventIInclusionTimeInBounds = (
  inclusionTime: bigint,
  inclusionTimeLowerBound?: POSIXTime,
  inclusionTimeUpperBound?: POSIXTime,
): boolean => {
  const biggerThanLower =
    inclusionTimeLowerBound === undefined ||
    inclusionTimeLowerBound <= inclusionTime;
  const smallerThanUpper =
    inclusionTimeUpperBound === undefined ||
    inclusionTime < inclusionTimeUpperBound;
  return biggerThanLower && smallerThanUpper;
};

export type UserEventFetchConfig = {
  eventAddress: Address;
  eventPolicyId: PolicyId;
  inclusionTimeUpperBound?: POSIXTime;
  inclusionTimeLowerBound?: POSIXTime;
};
export const fetchUserEventUTxOsProgram = <
  TEventUTxO extends { datum: { inclusionTime: bigint } },
>(
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
  conversionFunction: (utxo: UTxO[]) => Effect.Effect<TEventUTxO[]>,
): Effect.Effect<TEventUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(config.eventAddress),
      catch: (e) => {
        return new LucidError({
          message: `Failed to fetch user event UTxOs at: ${config.eventAddress}`,
          cause: e,
        });
      },
    });
    const eventUTxOs = yield* conversionFunction(allUTxOs);

    const validEventUTxOs = eventUTxOs.filter((utxo) =>
      eventIInclusionTimeInBounds(
        utxo.datum.inclusionTime,
        config.inclusionTimeLowerBound,
        config.inclusionTimeUpperBound,
      ),
    );
    return validEventUTxOs;
  });

export const UserEventMintRedeemerSchema = Data.Enum([
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
export type UserEventMintRedeemer = Data.Static<
  typeof UserEventMintRedeemerSchema
>;
export const UserEventMintRedeemer =
  UserEventMintRedeemerSchema as unknown as UserEventMintRedeemer;

export type UserEventMintTransactionParams = {
  lucid: LucidEvolution;
  inputUtxo: UTxO;
  nft: string;
  mintRedeemer: string;
  scriptAddress: string;
  datum: string;
  extraAssets?: Assets;
  validTo: number;
  mintingPolicy: MintingPolicy;
};

export type UserEventExtraFields = {
  idCbor: Buffer;
  infoCbor: Buffer;
  inclusionTime: Date;
};

export const buildUserEventMintTransaction = (
  params: UserEventMintTransactionParams,
): TxBuilder => {
  const {
    lucid,
    inputUtxo,
    nft,
    mintRedeemer,
    scriptAddress,
    datum,
    extraAssets,
    validTo,
    mintingPolicy,
  } = params;

  return lucid
    .newTx()
    .collectFrom([inputUtxo])
    .mintAssets(
      {
        [nft]: 1n,
      },
      mintRedeemer,
    )
    .pay.ToAddressWithData(
      scriptAddress,
      {
        kind: "inline",
        value: datum,
      },
      {
        [nft]: 1n,
        ...(extraAssets || {}),
      },
    )
    .validTo(validTo)
    .attach.MintingPolicy(mintingPolicy);
};

export const findInclusionTimeForUserEvent = (
  lucid: LucidEvolution,
): Effect.Effect<number, UnspecifiedNetworkError> =>
  Effect.gen(function* () {
    const currTime = Date.now();
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* new UnspecifiedNetworkError({
        message: "Failed to build the deposit transaction",
        cause: "Unknown",
      });
    }
    const waitTime = getProtocolParameters(network).event_wait_duration;
    return currTime + waitTime;
  });

export const getNonceInputAndAssetName = (
  lucid: LucidEvolution,
  eventName: "deposit" | "tx order" | "withdrawal",
  utxo?: UTxO,
): Effect.Effect<
  { inputUtxo: UTxO; assetName: string },
  LucidError | HashingError
> =>
  Effect.gen(function* () {
    const nonceUTxOEffect: Effect.Effect<UTxO, LucidError> = Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch wallet UTxOs",
          cause: err,
        }),
    }).pipe(Effect.andThen((utxos) => {
      if (utxos.length <= 0) {
        return new LucidError({
          message: `Failed to build the ${eventName} transaction`,
          cause: "No UTxOs found in wallet",
        });
      } else {
        return Effect.succeed(utxos[0]);
      }
    }));
    const inputUtxo = utxo ?? (yield* nonceUTxOEffect);
    const transactionInput = CML.TransactionInput.new(
      CML.TransactionHash.from_hex(inputUtxo.txHash),
      BigInt(inputUtxo.outputIndex),
    );

    const assetName = yield* hashHexWithBlake2b256(
      transactionInput.to_cbor_hex(),
    );

    return { inputUtxo, assetName };
  });
