import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import { aikenSerialisedPlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  Address,
  applyDoubleCborEncoding,
  Assets,
  type BuildTxWithRedeemer,
  CertificateValidator,
  credentialToAddress,
  Data,
  fromUnit,
  LucidEvolution,
  MintingPolicy,
  type Network,
  PolicyId,
  scriptHashToCredential,
  toUnit,
  type TxSignBuilder,
  UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { scriptRewardAddress } from "../cardano-addresses.js";
import {
  Bech32DeserializationError,
  GenericErrorFields,
  hashHexWithBlake2b,
  HashingError,
  LucidError,
  MidgardValidators,
  OutputReference,
  POSIXTime,
} from "../common.js";
import {
  fetchHubOracleUTxOProgram,
  HubOracleDatum,
  HubOracleError,
  makeHubOracleDatum,
} from "../hub-oracle.js";
import { getProtocolParameters } from "../protocol-parameters.js";
import {
  requireInputIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireSinglePublishRedeemerIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";

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

export type UserEventAuthenticateMintRedeemerParams = {
  readonly nonceInputIndex: bigint;
  readonly eventOutputIndex: bigint;
  readonly hubRefInputIndex: bigint;
  readonly witnessRegistrationRedeemerIndex: bigint;
};

/**
 * The typed `user_events.MintRedeemer` for an authenticating mint.
 *
 * Exported as a *value* rather than only as encoded bytes because one policy — the
 * tx-order minting policy — carries this redeemer nested inside its own
 * `MintRedeemer` beside the §8 carriage vector (#594), so it needs the value and
 * cannot use the encoder. Hand-copying the four-field mapping there instead is how
 * a renamed or reordered field ends up spelled two ways.
 */
export const userEventAuthenticateMintRedeemer = (
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

const encodeUserEventAuthenticateMintRedeemer = (
  params: UserEventAuthenticateMintRedeemerParams,
): string =>
  Data.to(userEventAuthenticateMintRedeemer(params), UserEventMintRedeemer);

export const encodeUserEventWitnessMintOrBurnRedeemer = (
  targetPolicy: PolicyId,
): string =>
  Data.to(
    { MintOrBurn: { targetPolicy } } satisfies UserEventWitnessPublishRedeemer,
    UserEventWitnessPublishRedeemer,
  );

export const USER_EVENT_WITNESS_SCRIPT_POSTFIX = "0001";

// Mirrors `user_events_witness_script_prefix` in
// `onchain/aiken/env/testnet.ak`.
export const USER_EVENT_WITNESS_SCRIPT_PREFIX =
  "5902e20101003229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e201c40382653001300100198071baa0099180918099809980998099809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc3a400200514a0809901319199119801001000912cc004006007132325980099b910150018acc004cdc780a800c4dd6980c001401501644cc010010c06c00d0161bae30160013018001405c6464660020026eacc060c064c064c064c064c058dd5007112cc004006007132325980099b910070018acc004cdc7803800c4dd5980c801401501744cc010010c07000d0171bae301700130190014060297adef6c60148000c048dd50031bae301430123754019159800980400144c8c966002003168992cc004cdd78008044566002601460286ea8006264b3001300c3015375400315980099baf3018301637540046030602c6ea800629462c80a22c80a0c05c00a2c809a2c8098c058009015180b000998019bac3001301237540146eb4c050c048dd50064566002601060226ea8016264b30010018b44c966002601260266ea80062b30013375e602c60286ea8004c058c050dd5003c528c59012459012180a800a028330033758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0240020028019012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c1225820";

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
  try {
    const unixTime = (
      lucid as unknown as { slotToUnixTime(value: number): number }
    ).slotToUnixTime(slot);
    return Number.isSafeInteger(unixTime) ? unixTime : undefined;
  } catch {
    return undefined;
  }
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

type HubOraclePolicyField = Extract<
  keyof HubOracleDatum,
  "deposit" | "tx_order" | "withdrawal"
>;

type HubOracleAddressField = Extract<
  keyof HubOracleDatum,
  "deposit_addr" | "tx_order_addr" | "withdrawal_addr"
>;

export type PrepareUserEventMintContextParams = {
  readonly lucid: LucidEvolution;
  readonly contracts: MidgardValidators;
  readonly label: "deposit" | "tx order" | "withdrawal";
  readonly eventPolicyId: PolicyId;
  readonly hubOraclePolicyField: HubOraclePolicyField;
  readonly hubOracleAddressField: HubOracleAddressField;
  /**
   * A previously reserved nonce is required by staged V1 tx orders,
   * whose field fragments are published before the final event is minted.
   */
  readonly nonceInput?: UTxO;
};

export type UserEventMintContext = {
  readonly network: NonNullable<
    ReturnType<LucidEvolution["config"]>["network"]
  >;
  readonly hubOracleRefInput: UTxO;
  readonly nonceInput: UTxO;
  readonly nonceAssetName: string;
  readonly eventUnit: string;
  readonly witnessScript: CertificateValidator;
  readonly witnessScriptHash: string;
  readonly validTo: number;
  readonly inclusionTime: number;
};

export const prepareUserEventMintContext = ({
  lucid,
  contracts,
  label,
  eventPolicyId,
  hubOraclePolicyField,
  hubOracleAddressField,
  nonceInput: requestedNonceInput,
}: PrepareUserEventMintContextParams): Effect.Effect<
  UserEventMintContext,
  | HubOracleError
  | LucidError
  | Bech32DeserializationError
  | HashingError
  | UserEventBuildError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new UserEventBuildError({
          message: `Cardano network not found while preparing ${label} transaction`,
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const actual = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      hubOraclePolicyId: contracts.hubOracle.policyId,
    });
    const expectedDatum = yield* makeHubOracleDatum(contracts);
    if (
      actual.datum[hubOraclePolicyField] !==
        expectedDatum[hubOraclePolicyField] ||
      JSON.stringify(actual.datum[hubOracleAddressField]) !==
        JSON.stringify(expectedDatum[hubOracleAddressField])
    ) {
      return yield* Effect.fail(
        new UserEventBuildError({
          message: `On-chain hub oracle deployment does not match the locally configured ${label} contract`,
          cause: {
            expectedPolicyId: expectedDatum[hubOraclePolicyField],
            actualPolicyId: actual.datum[hubOraclePolicyField],
            expectedAddress: expectedDatum[hubOracleAddressField],
            actualAddress: actual.datum[hubOracleAddressField],
          },
        }),
      );
    }

    const nonceInput =
      requestedNonceInput ??
      (yield* selectWalletNonceInputProgram(lucid, label));
    const eventIdCbor = outputReferenceToPlutusDataCbor(nonceInput);
    const nonceAssetName = yield* hashHexWithBlake2b(eventIdCbor, 32);
    const witnessScript =
      buildUserEventWitnessCertificateValidator(nonceAssetName);
    const validTo = resolveUserEventValidTo(lucid);

    return {
      network,
      hubOracleRefInput: actual.utxo,
      nonceInput,
      nonceAssetName,
      eventUnit: toUnit(eventPolicyId, nonceAssetName),
      witnessScript,
      witnessScriptHash: validatorToScriptHash(witnessScript),
      validTo,
      inclusionTime: resolveEventInclusionTime(validTo, network),
    };
  });

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
  /**
   * Wraps or replaces the mint redeemer this event policy expects.
   *
   * Every user-event policy but one takes `user_events.MintRedeemer` unchanged,
   * which is the default. The tx-order policy takes it inside its own
   * `MintRedeemer` beside the §8 carriage vector for the order's material
   * (#594), and that vector's reference-input indices are positional in the
   * *final* transaction — so the hook is handed the resolved redeemer context
   * rather than being allowed to guess them from the order the builder collected
   * reference inputs in.
   */
  readonly encodeMintRedeemer?: (params: {
    readonly layout: UserEventAuthenticateMintRedeemerParams;
    readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  }) => string;
};

type UserEventMintRedeemerParams = Pick<
  BuildCompletedUserEventMintTxParams,
  | "encodeMintRedeemer"
  | "eventUnit"
  | "hubOracleRefInput"
  | "label"
  | "nonceInput"
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
    onEncoded?: (redeemer: string) => void,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    const layout = deriveUserEventMintRedeemerLayout(params, ctx);
    const redeemer =
      params.encodeMintRedeemer === undefined
        ? encodeUserEventAuthenticateMintRedeemer(layout)
        : params.encodeMintRedeemer({ layout, ctx });
    onEncoded?.(redeemer);
    return redeemer;
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

      // Two passes: the first resolves the positional redeemer against the
      // completed transaction, the second rebuilds with that redeemer as a fixed
      // string so nothing can shift under it. The encoded bytes are captured
      // rather than re-derived from the layout, because a redeemer may carry more
      // than the layout does — the tx-order policy's carries the §8 carriage
      // vector — and re-deriving would silently drop it in the second pass.
      let resolvedRedeemer: string | undefined;
      await buildTx(
        makeUserEventMintRedeemer(params, (redeemer) => {
          resolvedRedeemer = redeemer;
        }),
      ).complete({ localUPLCEval: true });
      if (resolvedRedeemer === undefined) {
        throw new Error(
          `Failed to resolve ${params.label} mint redeemer context`,
        );
      }
      return buildTx(resolvedRedeemer).complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: `Failed to build ${params.label} transaction: ${String(cause)}`,
        cause,
      }),
  });

export const resolveEventInclusionTime = (
  validTo: number,
  network: NonNullable<ReturnType<LucidEvolution["config"]>["network"]>,
): number => validTo + getProtocolParameters(network).event_wait_duration - 1;
