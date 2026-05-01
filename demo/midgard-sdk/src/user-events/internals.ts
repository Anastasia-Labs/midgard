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
  CertificateValidator,
  Data,
  LucidEvolution,
  MintingPolicy,
  PolicyId,
  TxBuilder,
  UTxO,
  applyDoubleCborEncoding,
  validatorToScriptHash,
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
  TEventUTxO extends { datum: { inclusion_time: bigint } },
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
        utxo.datum.inclusion_time,
        config.inclusionTimeLowerBound,
        config.inclusionTimeUpperBound,
      ),
    );
    return validEventUTxOs;
  });

export const UserEventMintRedeemerSchema = Data.Enum([
  Data.Object({
    AuthenticateEvent: Data.Object({
      nonce_input_index: Data.Integer(),
      event_output_index: Data.Integer(),
      hub_ref_input_index: Data.Integer(),
      witness_registration_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    BurnEventNFT: Data.Object({
      nonce_asset_name: Data.Bytes(),
      witness_unregistration_redeemer_index: Data.Integer(),
    }),
  }),
]);
export type UserEventMintRedeemer = Data.Static<
  typeof UserEventMintRedeemerSchema
>;
export const UserEventMintRedeemer =
  UserEventMintRedeemerSchema as unknown as UserEventMintRedeemer;

export const UserEventWitnessPublishRedeemerSchema = Data.Enum([
  Data.Object({
    MintOrBurn: Data.Object({
      targetPolicy: Data.Bytes(),
    }),
  }),
  Data.Object({
    RegisterToProveNotRegistered: Data.Object({
      registrationCertificateIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    UnregisterToProveNotRegistered: Data.Object({
      registrationCertificateIndex: Data.Integer(),
    }),
  }),
]);
export type UserEventWitnessPublishRedeemer = Data.Static<
  typeof UserEventWitnessPublishRedeemerSchema
>;
export const UserEventWitnessPublishRedeemer =
  UserEventWitnessPublishRedeemerSchema as unknown as UserEventWitnessPublishRedeemer;

export type UserEventAuthenticateMintRedeemerParams = {
  readonly nonceInputIndex: bigint;
  readonly eventOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly witnessRegistrationRedeemerIndex: bigint;
};

export const makeUserEventAuthenticateMintRedeemer = (
  params: UserEventAuthenticateMintRedeemerParams,
): UserEventMintRedeemer => ({
  AuthenticateEvent: {
    nonce_input_index: params.nonceInputIndex,
    event_output_index: params.eventOutputIndex,
    hub_ref_input_index: params.hubRefInputIndex,
    witness_registration_redeemer_index:
      params.witnessRegistrationRedeemerIndex,
  },
});

export const encodeUserEventAuthenticateMintRedeemer = (
  params: UserEventAuthenticateMintRedeemerParams,
): string =>
  Data.to(makeUserEventAuthenticateMintRedeemer(params), UserEventMintRedeemer);

export const makeUserEventWitnessMintOrBurnRedeemer = (
  targetPolicy: PolicyId,
): UserEventWitnessPublishRedeemer => ({
  MintOrBurn: {
    targetPolicy,
  },
});

export const encodeUserEventWitnessMintOrBurnRedeemer = (
  targetPolicy: PolicyId,
): string =>
  Data.to(
    makeUserEventWitnessMintOrBurnRedeemer(targetPolicy),
    UserEventWitnessPublishRedeemer,
  );

export const USER_EVENT_WITNESS_SCRIPT_POSTFIX = "0001";

// Mirrors `witness_script_prefix` in
// `onchain/aiken/lib/midgard/user-events/witness.ak`.
export const USER_EVENT_WITNESS_SCRIPT_PREFIX =
  "59030601010033232323232323223225333004323232323253330093370e900318051baa00113233223232323253330103003001132325333015301800200416375c602c00260246ea802454ccc040c0100044c8c94ccc054c06000801058dd6980b00098091baa009153330103370e900200089919299980a980c0010020b1bad3016001301237540122c60206ea80204c8c8c8c94ccc048c0140044c8c8c94ccc054c0200044c02400854ccc054c0240044cdc3801240022940c054dd500499299980a1804180a9baa0011480004dd6980c980b1baa001325333014300830153754002298103d87a80001323300100137566034602e6ea8008894ccc064004530103d87a8000132333222533301a337220300062a66603466e3c06000c4cdd2a40006603c6ea00092f5c02980103d87a8000133006006001375c60300026eb4c064004c074008c06c004c8cc004004dd5980c980d180d180d180d180b1baa00e22533301800114c103d87a800013233322253330193372200e0062a66603266e3c01c00c4cdd2a40006603a6e980092f5c02980103d87a8000133006006001375c602e0026eacc060004c070008c068004dd7180b980a1baa00b153330123006001132323253330153375e0040142a66602a6010602c6ea80084c94ccc058c028c05cdd5001099baf001301b301837540042c6034602e6ea80085858c064c068008c060004cc00cdd61801180a1baa00c375a602e60286ea802c4c94ccc04cc01cc050dd500409919299980a9804180b1baa00113375e6034602e6ea800400858c064cc014dd61802180b1baa00e0023018301537540102c6eb4c05cc050dd500598091baa00a23016301730173017301730170013001001222533301133712002900008010a99980a0010a5eb804ccc00c00cc05c008cdc0000a40026e1d2000370e90011bad300f001300f3010001300b37540022c601a601c006601800460160046016002600c6ea800452613656375c002ae6955ceaab9e5573eae815d0aba24c1225820";

export const buildUserEventWitnessCertificateValidator = (
  nonceAssetName: string,
): CertificateValidator => ({
  type: "PlutusV3",
  script: applyDoubleCborEncoding(
    USER_EVENT_WITNESS_SCRIPT_PREFIX +
      nonceAssetName.toLowerCase() +
      USER_EVENT_WITNESS_SCRIPT_POSTFIX,
  ),
});

export const userEventWitnessScriptHash = (nonceAssetName: string): string =>
  validatorToScriptHash(
    buildUserEventWitnessCertificateValidator(nonceAssetName),
  );

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

export const resolveEventInclusionTime = (
  validTo: number,
  network: NonNullable<ReturnType<LucidEvolution["config"]>["network"]>,
): number => validTo + getProtocolParameters(network).event_wait_duration - 1;

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
    }).pipe(
      Effect.andThen((utxos) => {
        if (utxos.length <= 0) {
          return new LucidError({
            message: `Failed to build the ${eventName} transaction`,
            cause: "No UTxOs found in wallet",
          });
        } else {
          return Effect.succeed(utxos[0]);
        }
      }),
    );
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
