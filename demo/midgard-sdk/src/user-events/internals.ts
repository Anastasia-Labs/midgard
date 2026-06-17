import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import { aikenSerialisedPlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  Address,
  applyDoubleCborEncoding,
  Assets,
  type BuildTxWithRedeemer,
  CertificateValidator,
  CML,
  Data,
  fromUnit,
  LucidEvolution,
  MintingPolicy,
  type Network,
  PolicyId,
  slotToUnixTime,
  type TxSignBuilder,
  UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { scriptRewardAddress } from "@/cardano-addresses.js";
import {
  GenericErrorFields,
  hashHexWithBlake2b,
  HashingError,
  LucidError,
  OutputReference,
  POSIXTime,
  UnspecifiedNetworkError,
} from "@/common.js";
import { getProtocolParameters } from "@/protocol-parameters.js";
import {
  requireInputIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireSinglePublishRedeemerIndex,
  requireUniqueOutputIndex,
} from "@/tx-context-redeemer.js";

export class UserEventBuildError extends EffectData.TaggedError(
  "UserEventBuildError",
)<GenericErrorFields> {}

const USER_EVENT_TX_TTL_MS = 60_000;

const eventInclusionTimeInBounds = (
  inclusionTime: bigint,
  inclusionTimeLowerBound?: POSIXTime,
  inclusionTimeUpperBound?: POSIXTime,
): boolean =>
  (inclusionTimeLowerBound === undefined ||
    inclusionTimeLowerBound <= inclusionTime) &&
  (inclusionTimeUpperBound === undefined ||
    inclusionTime < inclusionTimeUpperBound);

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

    return eventUTxOs.filter((utxo) =>
      eventInclusionTimeInBounds(
        utxo.datum.inclusion_time,
        config.inclusionTimeLowerBound,
        config.inclusionTimeUpperBound,
      ),
    );
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

type UserEventAuthenticateMintRedeemerParams = {
  readonly nonceInputIndex: bigint;
  readonly eventOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly witnessRegistrationRedeemerIndex: bigint;
};

const encodeUserEventAuthenticateMintRedeemer = (
  params: UserEventAuthenticateMintRedeemerParams,
): string =>
  Data.to(
    {
      AuthenticateEvent: {
        nonce_input_index: params.nonceInputIndex,
        event_output_index: params.eventOutputIndex,
        hub_ref_input_index: params.hubRefInputIndex,
        witness_registration_redeemer_index:
          params.witnessRegistrationRedeemerIndex,
      },
    } satisfies UserEventMintRedeemer,
    UserEventMintRedeemer,
  );

export const encodeUserEventWitnessMintOrBurnRedeemer = (
  targetPolicy: PolicyId,
): string =>
  Data.to(
    { MintOrBurn: { targetPolicy } } satisfies UserEventWitnessPublishRedeemer,
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

export type UserEventExtraFields = {
  idCbor: Buffer;
  infoCbor: Buffer;
  inclusionTime: Date;
};

export const userEventCborFieldsFromInlineDatum = (
  utxo: Pick<UTxO, "datum" | "txHash" | "outputIndex">,
): Pick<UserEventExtraFields, "idCbor" | "infoCbor"> => {
  if (utxo.datum === undefined || utxo.datum === null) {
    throw new Error(
      `Missing inline datum for user event ${utxo.txHash}#${utxo.outputIndex.toString()}`,
    );
  }
  return {
    idCbor: Buffer.from(
      aikenSerialisedPlutusConstrFieldCbor(utxo.datum, [0, 0]),
      "hex",
    ),
    infoCbor: Buffer.from(
      aikenSerialisedPlutusConstrFieldCbor(utxo.datum, [0, 1]),
      "hex",
    ),
  };
};

export const outputReferenceToPlutusDataCbor = (
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): string =>
  Data.to(
    {
      transactionId: utxo.txHash,
      outputIndex: BigInt(utxo.outputIndex),
    },
    OutputReference,
  );

export const slotToUnixTimeForLucid = (
  lucid: LucidEvolution,
  slot: number,
): number | undefined => {
  const network = lucid.config().network;
  if (network === undefined) {
    return undefined;
  }
  if (network === "Custom") {
    const provider = lucid.config().provider as {
      time?: number;
      slot?: number;
    };
    if (
      typeof provider.time !== "number" ||
      typeof provider.slot !== "number"
    ) {
      return undefined;
    }
    const slotLength = 1000;
    const zeroTime = provider.time - provider.slot * slotLength;
    return zeroTime + slot * slotLength;
  }
  return slotToUnixTime(network as Exclude<Network, "Custom">, slot);
};

export const slotToUnixTimeForLucidOrEmulatorFallback = (
  lucid: LucidEvolution,
  slot: number,
): number => slotToUnixTimeForLucid(lucid, slot) ?? slot * 1000;

export const resolveUserEventValidTo = (
  lucid: LucidEvolution,
  ttlMs = USER_EVENT_TX_TTL_MS,
): number => {
  const targetUnixTime = Date.now() + ttlMs;
  const slot = lucid.unixTimeToSlot(targetUnixTime);
  const alignedUnixTime = slotToUnixTimeForLucidOrEmulatorFallback(lucid, slot);
  return alignedUnixTime > targetUnixTime
    ? alignedUnixTime
    : slotToUnixTimeForLucidOrEmulatorFallback(lucid, slot + 1);
};

export const fetchSortedWalletUtxosProgram = (
  lucid: LucidEvolution,
  label: string,
): Effect.Effect<readonly UTxO[], LucidError> =>
  Effect.tryPromise({
    try: async () =>
      [...(await lucid.wallet().getUtxos())].sort(compareOutRefs),
    catch: (cause) =>
      new LucidError({
        message: `Failed to fetch wallet UTxOs for ${label} submission`,
        cause,
      }),
  });

export const selectWalletNonceInputProgram = (
  lucid: LucidEvolution,
  label: string,
): Effect.Effect<UTxO, LucidError | UserEventBuildError> =>
  fetchSortedWalletUtxosProgram(lucid, label).pipe(
    Effect.andThen((utxos) => {
      const nonceInput = utxos[0];
      return nonceInput === undefined
        ? Effect.fail(
            new UserEventBuildError({
              message: `Failed to build ${label} transaction`,
              cause: "No UTxOs found in wallet",
            }),
          )
        : Effect.succeed(nonceInput);
    }),
  );

export type BuildCompletedUserEventMintTxParams = {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly nonceInput: UTxO;
  readonly eventUnit: string;
  readonly eventAddress: string;
  readonly eventDatumCbor: string;
  readonly outputAssets: Assets;
  readonly validTo: number;
  readonly mintingPolicy: MintingPolicy;
  readonly attachMintingPolicy: boolean;
  readonly referenceInputs: readonly UTxO[];
  readonly hubOracleRefInput: UTxO;
  readonly witnessScript: CertificateValidator;
  readonly witnessRegistrationRedeemer: string;
  readonly label: string;
};

type UserEventMintRedeemerParams = Pick<
  BuildCompletedUserEventMintTxParams,
  "eventUnit" | "hubOracleRefInput" | "label" | "nonceInput"
>;

type UserEventMintRedeemerLayout = UserEventAuthenticateMintRedeemerParams;

const deriveUserEventMintRedeemerLayout = (
  params: UserEventMintRedeemerParams,
  ctx: Parameters<BuildTxWithRedeemer>[0],
): UserEventMintRedeemerLayout => {
  requireOwnMintPurpose(ctx, fromUnit(params.eventUnit).policyId, params.label);

  return {
    nonceInputIndex: requireInputIndex(ctx, params.nonceInput, params.label),
    eventOutputIndex: requireUniqueOutputIndex(
      ctx.outputs,
      (output) => (output.assets[params.eventUnit] ?? 0n) === 1n,
      `${params.label} event`,
    ),
    hubRefInputIndex: requireReferenceInputIndex(
      ctx,
      params.hubOracleRefInput,
      params.label,
    ),
    witnessRegistrationRedeemerIndex: requireSinglePublishRedeemerIndex(
      ctx,
      params.label,
    ),
  };
};

const makeUserEventMintRedeemer =
  (
    params: UserEventMintRedeemerParams,
    onLayout?: (layout: UserEventMintRedeemerLayout) => void,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    const layout = deriveUserEventMintRedeemerLayout(params, ctx);
    onLayout?.(layout);
    return encodeUserEventAuthenticateMintRedeemer(layout);
  };

export const buildCompletedUserEventMintTxProgram = (
  params: BuildCompletedUserEventMintTxParams,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      const buildTx = (
        mintRedeemer: BuildTxWithRedeemer | string,
      ): ReturnType<LucidEvolution["newTx"]> => {
        const baseTx = params.lucid
          .newTx()
          .collectFrom([params.nonceInput])
          .readFrom([...params.referenceInputs]);
        const txWithMintWitness = params.attachMintingPolicy
          ? baseTx.attach.MintingPolicy(params.mintingPolicy)
          : baseTx;

        return txWithMintWitness.attach
          .CertificateValidator(params.witnessScript)
          .mintAssets({ [params.eventUnit]: 1n }, mintRedeemer)
          .pay.ToAddressWithData(
            params.eventAddress,
            {
              kind: "inline",
              value: params.eventDatumCbor,
            },
            params.outputAssets,
          )
          .validTo(params.validTo)
          .register.Stake(
            scriptRewardAddress(params.network, params.witnessScript),
            params.witnessRegistrationRedeemer,
          );
      };

      let resolvedLayout: UserEventMintRedeemerLayout | undefined;
      await buildTx(
        makeUserEventMintRedeemer(params, (layout) => {
          resolvedLayout = layout;
        }),
      ).complete({ localUPLCEval: true });
      if (resolvedLayout === undefined) {
        throw new Error(
          `Failed to resolve ${params.label} mint redeemer context`,
        );
      }
      return buildTx(
        encodeUserEventAuthenticateMintRedeemer(resolvedLayout),
      ).complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: `Failed to build ${params.label} transaction: ${String(cause)}`,
        cause,
      }),
  });

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
        if (utxos.length === 0) {
          return new LucidError({
            message: `Failed to build the ${eventName} transaction`,
            cause: "No UTxOs found in wallet",
          });
        }

        return Effect.succeed(utxos[0]);
      }),
    );
    const inputUtxo = utxo ?? (yield* nonceUTxOEffect);
    const transactionInput = CML.TransactionInput.new(
      CML.TransactionHash.from_hex(inputUtxo.txHash),
      BigInt(inputUtxo.outputIndex),
    );

    const assetName = yield* hashHexWithBlake2b(
      transactionInput.to_cbor_hex(),
      32,
    );

    return { inputUtxo, assetName };
  });
