import {
  Assets,
  type BuildTxWithRedeemer,
  CertificateValidator,
  credentialToAddress,
  fromUnit,
  LucidEvolution,
  MintingPolicy,
  type Network,
  scriptHashToCredential,
  toUnit,
  type TxSignBuilder,
  UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "../cardano-addresses.js";
import {
  Bech32DeserializationError,
  hashHexWithBlake2b,
  HashingError,
  LucidError,
} from "../common.js";
import {
  fetchHubOracleUTxOProgram,
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
import {
  buildUserEventWitnessCertificateValidator,
  encodeUserEventAuthenticateMintRedeemer,
  outputReferenceToPlutusDataCbor,
  type PrepareUserEventMintContextParams,
  resolveUserEventValidTo,
  selectWalletNonceInputProgram,
  type UserEventAuthenticateMintRedeemerParams,
  UserEventBuildError,
} from "./internals.fetch-user-event-utx-os-program.js";

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
