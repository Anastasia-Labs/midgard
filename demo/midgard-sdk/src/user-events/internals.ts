import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
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
  slotToUnixTime,
  PolicyId,
  type TxSignBuilder,
  UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  GenericErrorFields,
  hashHexWithBlake2b,
  HashingError,
  LucidError,
  OutputReference,
  POSIXTime,
  UnspecifiedNetworkError,
} from "@/common.js";
import { scriptRewardAddress } from "@/cardano-addresses.js";
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

// Mirrors `user_events_witness_script_prefix` in `onchain/aiken/env/default.ak`.
export const USER_EVENT_WITNESS_SCRIPT_PREFIX =
  "5902d30101003229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e201c40382653001300100198071baa0099180918099809980998099809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc38012400314a0809901319199119801001000912cc00400600713233225980099b910160028acc004cdc780b0014400600c80ba26600a00a603800880b8dd7180b0009bad30170013018001405c6464660020026eacc060c064c064c064c064c058dd5007112cc00400600713233225980099b910080028acc004cdc78040014400600c80c226600a00a603a00880c0dd7180b8009bab301800130190014060297adef6c60148000c048dd50031bae301430123754019159800980400144c8c96600266ebc00401e2b3001300930133754003132598009805980a1baa0018acc004cdd7980b980a9baa00230173015375400314a316404d16404c602c602e005164049164048602a002660066eb0c004c048dd50051bad301430123754019159800980418089baa0058992cc004c020c048dd5000c56600266ebcc054c04cdd5000980a98099baa0068a518b20228b20223014330033758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0000a40028019012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c01225820";

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
    try: async () => [...(await lucid.wallet().getUtxos())].sort(compareOutRefs),
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

const makeUserEventMintRedeemer =
  (params: UserEventMintRedeemerParams): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnMintPurpose(ctx, fromUnit(params.eventUnit).policyId, params.label);

    return encodeUserEventAuthenticateMintRedeemer({
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
    });
  };

export const buildCompletedUserEventMintTxProgram = (
  params: BuildCompletedUserEventMintTxParams,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: () => {
      const baseTx = params.lucid
        .newTx()
        .collectFrom([params.nonceInput])
        .readFrom([...params.referenceInputs]);
      const txWithMintWitness = params.attachMintingPolicy
        ? baseTx.attach.MintingPolicy(params.mintingPolicy)
        : baseTx;

      return txWithMintWitness
        .attach.CertificateValidator(params.witnessScript)
        .mintAssets(
          { [params.eventUnit]: 1n },
          makeUserEventMintRedeemer(params),
        )
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
        )
        .complete({ localUPLCEval: true });
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
