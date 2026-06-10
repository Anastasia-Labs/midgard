import {
  compareOutRefs,
  findOutRefIndex,
  type OutRefLike,
} from "@al-ft/midgard-core/out-ref";
import {
  Address,
  applyDoubleCborEncoding,
  Assets,
  CertificateValidator,
  CML,
  coreToTxOutput,
  Data,
  LucidEvolution,
  MintingPolicy,
  type Network,
  type RedeemerBuilder,
  slotToUnixTime,
  PolicyId,
  TxBuilder,
  type TxSignBuilder,
  UTxO,
  validatorToRewardAddress,
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
import {
  getRedeemerPointersInContextOrder,
  getTxInfoRedeemerIndexes,
} from "@/cardano-redeemers.js";
import { getProtocolParameters } from "@/protocol-parameters.js";

type UserEventTxProvider = {
  getProtocolParameters?: () => Promise<{
    keyDeposit: bigint;
  }>;
};

export type UserEventDraftLayout = {
  readonly eventOutputIndex: bigint;
  readonly witnessRegistrationRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

export class UserEventBuildError extends EffectData.TaggedError(
  "UserEventBuildError",
)<GenericErrorFields> {}

const USER_EVENT_TX_TTL_MS = 60_000;
const USER_EVENT_OUTPUT_INDEX = 0n;
const WITNESS_REGISTRATION_REDEEMER_TX_INFO_INDEX = 1n;

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

export const encodeUserEventAuthenticateMintRedeemer = (
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

const collectSortedInputOutRefs = (
  inputs: CML.TransactionInputList,
): readonly OutRefLike[] =>
  [...Array(inputs.len()).keys()]
    .map((index) => {
      const input = inputs.get(index);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    })
    .sort(compareOutRefs);

const resolveOutRefIndexFromSet = (
  target: OutRefLike,
  outRefs: readonly OutRefLike[],
): bigint => {
  const index = findOutRefIndex([...outRefs].sort(compareOutRefs), target);
  if (index === undefined) {
    throw new Error("Hub-oracle reference input is missing from reference set");
  }
  return BigInt(index);
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

export const fetchStakeCredentialDepositProgram = (
  lucid: LucidEvolution,
  label: string,
): Effect.Effect<bigint, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      const provider = lucid.config().provider as UserEventTxProvider;
      if (typeof provider.getProtocolParameters !== "function") {
        throw new Error(
          "Cardano provider does not expose protocol parameters required for certificate deposit",
        );
      }

      const { keyDeposit } = await provider.getProtocolParameters();
      if (typeof keyDeposit !== "bigint") {
        throw new Error(
          "Provider protocol parameters did not include keyDeposit",
        );
      }

      return keyDeposit;
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: `Failed to resolve stake credential deposit for ${label} transaction`,
        cause,
      }),
  });

// Registers the witness stake credential through lucid's public TxBuilder API
// so the certificate (and its plutus redeemer) becomes a first-class builder
// action. The earlier approach hand-pushed the certificate into
// `rawConfig().programs` / `rawConfig().txBuilder`, which lucid 0.4.x executed
// during `complete()` but lucid 0.5.x silently drops (it replays only
// `actions` into a fresh builder). Dropping the certificate left the deposit
// mint validator's `witness_redeemer_index` pointing at a missing redeemer,
// crashing the on-chain `expect Some(... list.at(...))`. The certificate's
// `keyDeposit` is sourced from lucid's protocol parameters by `register.Stake`.
const addScriptStakeRegistrationCertificate = (
  tx: TxBuilder,
  network: Network,
  witnessScript: CertificateValidator,
  witnessRedeemer: string,
): TxBuilder => {
  const witnessRewardAddress = validatorToRewardAddress(network, witnessScript);
  return tx.attach
    .CertificateValidator(witnessScript)
    .register.Stake(witnessRewardAddress, witnessRedeemer);
};

const deriveUserEventDraftLayout = ({
  tx,
  eventAddress,
  eventUnit,
  hubOracleRefInput,
  label,
}: {
  readonly tx: CML.Transaction;
  readonly eventAddress: string;
  readonly eventUnit: string;
  readonly hubOracleRefInput: UTxO;
  readonly label: string;
}): UserEventDraftLayout => {
  const outputs = tx.body().outputs();
  let eventOutputIndex: bigint | null = null;
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === eventAddress &&
      (output.assets[eventUnit] ?? 0n) === 1n
    ) {
      eventOutputIndex = BigInt(index);
      break;
    }
  }
  if (eventOutputIndex === null) {
    throw new Error(
      `Failed to locate ${label} event output for unit=${eventUnit} at address=${eventAddress}`,
    );
  }

  const pointers = getRedeemerPointersInContextOrder(tx);
  const certContextIndex = pointers.findIndex(
    (pointer) => pointer.tag === CML.RedeemerTag.Cert,
  );
  if (certContextIndex < 0) {
    throw new Error(`Failed to locate certificate redeemer in ${label} draft`);
  }

  const witnessRegistrationRedeemerIndex =
    getTxInfoRedeemerIndexes(pointers)[certContextIndex];
  if (witnessRegistrationRedeemerIndex === undefined) {
    throw new Error(
      `Failed to resolve certificate redeemer index in ${label} draft`,
    );
  }

  const referenceInputs = tx.body().reference_inputs();
  if (referenceInputs === undefined) {
    throw new Error(`${label} draft did not include reference inputs`);
  }
  const hubRefInputIndex = findOutRefIndex(
    collectSortedInputOutRefs(referenceInputs),
    hubOracleRefInput,
  );
  if (hubRefInputIndex === undefined) {
    throw new Error(`${label} draft did not include hub-oracle reference input`);
  }

  return {
    eventOutputIndex,
    witnessRegistrationRedeemerIndex: BigInt(
      witnessRegistrationRedeemerIndex,
    ),
    hubRefInputIndex: BigInt(hubRefInputIndex),
  };
};

export type BuildCompletedUserEventMintTxParams = {
  readonly lucid: LucidEvolution;
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

export const buildCompletedUserEventMintTxProgram = (
  params: BuildCompletedUserEventMintTxParams,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly layout: UserEventDraftLayout;
  },
  UserEventBuildError
> =>
  Effect.gen(function* () {
    const assumedLayout = yield* Effect.try({
      try: () => ({
        eventOutputIndex: USER_EVENT_OUTPUT_INDEX,
        witnessRegistrationRedeemerIndex:
          WITNESS_REGISTRATION_REDEEMER_TX_INFO_INDEX,
        hubRefInputIndex: resolveOutRefIndexFromSet(
          params.hubOracleRefInput,
          params.referenceInputs,
        ),
      }),
      catch: (cause) =>
        new UserEventBuildError({
          message: `Failed to resolve ${params.label} transaction layout`,
          cause,
        }),
    });

    const makeMintRedeemerBuilder = (
      layout: UserEventDraftLayout,
    ): RedeemerBuilder => ({
      kind: "selected",
      inputs: [params.nonceInput],
      makeRedeemer: (inputIndices) => {
        const nonceInputIndex = inputIndices[0];
        if (nonceInputIndex === undefined || inputIndices.length !== 1) {
          throw new Error(
            `${params.label} redeemer builder expected exactly one selected nonce input, got ${inputIndices.length.toString()}`,
          );
        }
        return encodeUserEventAuthenticateMintRedeemer({
          nonceInputIndex,
          eventOutputIndex: layout.eventOutputIndex,
          hubRefInputIndex: layout.hubRefInputIndex,
          witnessRegistrationRedeemerIndex:
            layout.witnessRegistrationRedeemerIndex,
        });
      },
    });

    const buildTx = (layout: UserEventDraftLayout) => {
      const network = params.lucid.config().network;
      if (network === undefined) {
        throw new Error(
          `Cannot build ${params.label} transaction: lucid network is undefined`,
        );
      }
      const tx = params.lucid
        .newTx()
        .collectFrom([params.nonceInput])
        .readFrom([...params.referenceInputs])
        .mintAssets(
          { [params.eventUnit]: 1n },
          makeMintRedeemerBuilder(layout),
        )
        .pay.ToAddressWithData(
          params.eventAddress,
          {
            kind: "inline",
            value: params.eventDatumCbor,
          },
          params.outputAssets,
        )
        .validTo(params.validTo);

      return addScriptStakeRegistrationCertificate(
        params.attachMintingPolicy
          ? tx.attach.MintingPolicy(params.mintingPolicy)
          : tx,
        network,
        params.witnessScript,
        params.witnessRegistrationRedeemer,
      );
    };

    const tx = yield* Effect.tryPromise({
      try: () => buildTx(assumedLayout).complete({ localUPLCEval: true }),
      catch: (cause) =>
        new UserEventBuildError({
          message: `Failed to build ${params.label} transaction: ${String(cause)}`,
          cause,
        }),
    });

    const resolvedLayout = yield* Effect.try({
      try: () =>
        deriveUserEventDraftLayout({
          tx: tx.toTransaction(),
          eventAddress: params.eventAddress,
          eventUnit: params.eventUnit,
          hubOracleRefInput: params.hubOracleRefInput,
          label: params.label,
        }),
      catch: (cause) =>
        new UserEventBuildError({
          message: `Failed to verify ${params.label} transaction layout`,
          cause,
        }),
    });

    if (
      resolvedLayout.eventOutputIndex !== assumedLayout.eventOutputIndex ||
      resolvedLayout.witnessRegistrationRedeemerIndex !==
        assumedLayout.witnessRegistrationRedeemerIndex ||
      resolvedLayout.hubRefInputIndex !== assumedLayout.hubRefInputIndex
    ) {
      return yield* Effect.fail(
        new UserEventBuildError({
          message: `Built ${params.label} transaction layout drifted from expected form`,
          cause: {
            assumedLayout,
            resolvedLayout,
          },
        }),
      );
    }

    return { tx, layout: resolvedLayout };
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
