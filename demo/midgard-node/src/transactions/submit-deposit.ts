import { Effect, Data as EffectData } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  type Assets,
  type CertificateValidator,
  Data as LucidData,
  type LucidEvolution,
  type Network,
  type RedeemerBuilder,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  applyDoubleCborEncoding,
  coreToTxOutput,
  credentialToAddress,
  fromText,
  getAddressDetails,
  scriptHashToCredential,
  slotToUnixTime,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { REAL_DEPOSIT_SCRIPT_TITLES } from "@/services/midgard-contracts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

type TxBuilderInternals = {
  txBuilder: {
    add_cert: (builder: unknown) => void;
  };
  programs: Array<Effect.Effect<void>>;
};

type InternalTxBuilder = TxBuilder & {
  rawConfig: () => TxBuilderInternals;
};

type DepositTxProvider = {
  getProtocolParameters?: () => Promise<{
    keyDeposit: bigint;
  }>;
};

type ProviderRedeemerTag =
  | "spend"
  | "mint"
  | "publish"
  | "withdraw"
  | "vote"
  | "propose";

type RedeemerPointer = {
  readonly tag: number;
  readonly index: bigint;
};

type DepositDraftLayout = {
  readonly eventOutputIndex: bigint;
  readonly witnessRegistrationRedeemerIndex: bigint;
};

export type SubmitDepositConfig = {
  readonly l2Address: string;
  readonly l2Datum: string | null;
  readonly lovelace: bigint;
  readonly additionalAssets: Readonly<Assets>;
};

export class SubmitDepositError extends EffectData.TaggedError(
  "SubmitDepositError",
)<{
  message: string;
  cause: unknown;
}> {}

const DEPOSIT_TX_TTL_MS = 60_000;
const HUB_REFERENCE_INPUT_INDEX = 0n;
const LOVELACE_INTEGER_PATTERN = /^(?:0|[1-9]\d*)$/;
const HEX_PATTERN = /^[0-9a-fA-F]*$/;
const POLICY_ID_PATTERN = /^[0-9a-fA-F]{56}$/;
const DUMMY_REDEEMER_EX_UNITS = {
  mem: 1_000_000,
  steps: 1_000_000,
} as const;
const WITNESS_SCRIPT_POSTFIX = "0001";
// Mirror the on-chain `witness_script_prefix` from
// `onchain/aiken/lib/midgard/user-events/witness.ak`. This witness uses the
// prehashed parameter flow, so generic `applyParamsToScript` produces a
// different script hash than the mint validator derives on-chain. We first
// reconstruct the single-CBOR applied bytes the protocol hashes on-chain, then
// wrap them into the double-CBOR form Lucid/CML expects for attached scripts.
const REAL_USER_EVENTS_WITNESS_SCRIPT_PREFIX =
  "59030601010033232323232323223225333004323232323253330093370e900318051baa00113233223232323253330103003001132325333015301800200416375c602c00260246ea802454ccc040c0100044c8c94ccc054c06000801058dd6980b00098091baa009153330103370e900200089919299980a980c0010020b1bad3016001301237540122c60206ea80204c8c8c8c94ccc048c0140044c8c8c94ccc054c0200044c02400854ccc054c0240044cdc3801240022940c054dd500499299980a1804180a9baa0011480004dd6980c980b1baa001325333014300830153754002298103d87a80001323300100137566034602e6ea8008894ccc064004530103d87a8000132333222533301a337220300062a66603466e3c06000c4cdd2a40006603c6ea00092f5c02980103d87a8000133006006001375c60300026eb4c064004c074008c06c004c8cc004004dd5980c980d180d180d180d180b1baa00e22533301800114c103d87a800013233322253330193372200e0062a66603266e3c01c00c4cdd2a40006603a6e980092f5c02980103d87a8000133006006001375c602e0026eacc060004c070008c068004dd7180b980a1baa00b153330123006001132323253330153375e0040142a66602a6010602c6ea80084c94ccc058c028c05cdd5001099baf001301b301837540042c6034602e6ea80085858c064c068008c060004cc00cdd61801180a1baa00c375a602e60286ea802c4c94ccc04cc01cc050dd500409919299980a9804180b1baa00113375e6034602e6ea800400858c064cc014dd61802180b1baa00e0023018301537540102c6eb4c05cc050dd500598091baa00a23016301730173017301730170013001001222533301133712002900008010a99980a0010a5eb804ccc00c00cc05c008cdc0000a40026e1d2000370e90011bad300f001300f3010001300b37540022c601a601c006601800460160046016002600c6ea800452613656375c002ae6955ceaab9e5573eae815d0aba24c1225820";

const WitnessPublishRedeemer = LucidData.Enum([
  LucidData.Object({
    MintOrBurn: LucidData.Object({
      targetPolicy: LucidData.Bytes(),
    }),
  }),
  LucidData.Object({
    RegisterToProveNotRegistered: LucidData.Object({
      registrationCertificateIndex: LucidData.Integer(),
    }),
  }),
  LucidData.Object({
    UnregisterToProveNotRegistered: LucidData.Object({
      registrationCertificateIndex: LucidData.Integer(),
    }),
  }),
]);

const DepositOutputReferenceSchema = LucidData.Object({
  transactionId: LucidData.Bytes(),
  outputIndex: LucidData.Integer(),
});

const DepositCredentialSchema = LucidData.Enum([
  LucidData.Object({
    PublicKeyCredential: LucidData.Tuple([LucidData.Bytes()]),
  }),
  LucidData.Object({
    ScriptCredential: LucidData.Tuple([LucidData.Bytes()]),
  }),
]);

const DepositAddressSchema = LucidData.Object({
  paymentCredential: DepositCredentialSchema,
  stakeCredential: LucidData.Nullable(
    LucidData.Enum([
      LucidData.Object({
        Inline: LucidData.Tuple([DepositCredentialSchema]),
      }),
      LucidData.Object({
        Pointer: LucidData.Tuple([
          LucidData.Object({
            slotNumber: LucidData.Integer(),
            transactionIndex: LucidData.Integer(),
            certificateIndex: LucidData.Integer(),
          }),
        ]),
      }),
    ]),
  ),
});

const DepositInfoSchema = LucidData.Object({
  l2Address: DepositAddressSchema,
  l2Datum: LucidData.Nullable(LucidData.Any()),
});

const DepositEventSchema = LucidData.Object({
  id: DepositOutputReferenceSchema,
  info: DepositInfoSchema,
});

const DepositDatumWithWitnessSchema = LucidData.Object({
  event: DepositEventSchema,
  inclusionTime: LucidData.Integer(),
  witness: LucidData.Bytes(),
});

const outputReferenceToPlutusDataCbor = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  LucidData.to(
    {
      transactionId: utxo.txHash,
      outputIndex: BigInt(utxo.outputIndex),
    },
    DepositOutputReferenceSchema,
  );

const toProviderRedeemerTag = (tag: number): ProviderRedeemerTag => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "publish";
    case CML.RedeemerTag.Reward:
      return "withdraw";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      throw new Error(`Unsupported redeemer tag: ${tag}`);
  }
};

// Aiken exposes `self.redeemers` in ScriptPurpose order:
// Spend < Mint < Publish < Withdraw < Vote < Propose.
const txInfoRedeemerPurposeRank = (tag: number): number => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return 0;
    case CML.RedeemerTag.Mint:
      return 1;
    case CML.RedeemerTag.Cert:
      return 2;
    case CML.RedeemerTag.Reward:
      return 3;
    case CML.RedeemerTag.Voting:
      return 4;
    case CML.RedeemerTag.Proposing:
      return 5;
    default:
      return Number.MAX_SAFE_INTEGER;
  }
};

const getRedeemerPointersInContextOrder = (
  tx: CML.Transaction,
): readonly RedeemerPointer[] => {
  const redeemers = tx.witness_set().redeemers();
  if (redeemers === undefined) {
    return [];
  }

  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    const pointers: RedeemerPointer[] = [];
    for (let i = 0; i < legacy.len(); i += 1) {
      const redeemer = legacy.get(i);
      pointers.push({
        tag: redeemer.tag(),
        index: redeemer.index(),
      });
    }
    return pointers;
  }

  const map = redeemers.as_map_redeemer_key_to_redeemer_val();
  if (map === undefined) {
    return [];
  }
  const pointers: RedeemerPointer[] = [];
  const keys = map.keys();
  for (let i = 0; i < keys.len(); i += 1) {
    const key = keys.get(i);
    pointers.push({
      tag: key.tag(),
      index: key.index(),
    });
  }
  return pointers;
};

const getTxInfoRedeemerIndexes = (
  pointers: readonly RedeemerPointer[],
): readonly number[] => {
  const inContextOrder = pointers.map((pointer, contextIndex) => ({
    pointer,
    contextIndex,
  }));
  const inTxInfoOrder = [...inContextOrder].sort((a, b) => {
    const rankA = txInfoRedeemerPurposeRank(a.pointer.tag);
    const rankB = txInfoRedeemerPurposeRank(b.pointer.tag);
    if (rankA !== rankB) {
      return rankA - rankB;
    }
    if (a.pointer.index !== b.pointer.index) {
      return a.pointer.index < b.pointer.index ? -1 : 1;
    }
    return a.contextIndex - b.contextIndex;
  });

  const txInfoIndexes = Array<number>(pointers.length).fill(-1);
  for (
    let txInfoIndex = 0;
    txInfoIndex < inTxInfoOrder.length;
    txInfoIndex += 1
  ) {
    txInfoIndexes[inTxInfoOrder[txInfoIndex].contextIndex] = txInfoIndex;
  }
  return txInfoIndexes;
};

const withStubbedProviderEvaluation = async <A>(
  lucid: LucidEvolution,
  run: () => Promise<A>,
): Promise<A> => {
  const provider = lucid.config().provider as {
    evaluateTx?: (
      tx: string,
      additionalUTxOs?: readonly UTxO[],
    ) => Promise<
      readonly {
        redeemer_tag: ProviderRedeemerTag;
        redeemer_index: number;
        ex_units: { mem: number; steps: number };
      }[]
    >;
  };
  if (typeof provider.evaluateTx !== "function") {
    return run();
  }

  const originalEvaluateTx = provider.evaluateTx.bind(provider);
  provider.evaluateTx = async (txCbor) => {
    const tx = CML.Transaction.from_cbor_hex(txCbor);
    return getRedeemerPointersInContextOrder(tx).map((pointer) => ({
      redeemer_tag: toProviderRedeemerTag(pointer.tag),
      redeemer_index: Number(pointer.index),
      ex_units: DUMMY_REDEEMER_EX_UNITS,
    }));
  };
  try {
    return await run();
  } finally {
    provider.evaluateTx = originalEvaluateTx;
  }
};

const deriveDepositDraftLayout = ({
  tx,
  depositAddress,
  depositUnit,
}: {
  readonly tx: CML.Transaction;
  readonly depositAddress: string;
  readonly depositUnit: string;
}): DepositDraftLayout => {
  const outputs = tx.body().outputs();
  let eventOutputIndex: bigint | null = null;
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === depositAddress &&
      (output.assets[depositUnit] ?? 0n) === 1n
    ) {
      eventOutputIndex = BigInt(index);
      break;
    }
  }
  if (eventOutputIndex === null) {
    throw new Error(
      `Failed to locate deposit event output for unit=${depositUnit} at address=${depositAddress}`,
    );
  }

  const pointers = getRedeemerPointersInContextOrder(tx);
  const certContextIndex = pointers.findIndex(
    (pointer) => pointer.tag === CML.RedeemerTag.Cert,
  );
  if (certContextIndex < 0) {
    throw new Error("Failed to locate certificate redeemer in deposit draft");
  }
  const txInfoIndexes = getTxInfoRedeemerIndexes(pointers);
  const witnessRegistrationRedeemerIndex = txInfoIndexes[certContextIndex];
  if (witnessRegistrationRedeemerIndex === undefined || witnessRegistrationRedeemerIndex < 0) {
    throw new Error(
      "Failed to derive tx-info redeemer index for witness registration",
    );
  }

  return {
    eventOutputIndex,
    witnessRegistrationRedeemerIndex: BigInt(witnessRegistrationRedeemerIndex),
  };
};

const buildWitnessCertificateValidator = (
  nonceAssetName: string,
): Effect.Effect<CertificateValidator, SubmitDepositError> =>
  Effect.succeed({
    type: "PlutusV3",
    script: applyDoubleCborEncoding(
      REAL_USER_EVENTS_WITNESS_SCRIPT_PREFIX +
        nonceAssetName.toLowerCase() +
        WITNESS_SCRIPT_POSTFIX,
    ),
  });

const slotToUnixTimeForLucid = (
  lucid: LucidEvolution,
  slot: number,
): number => {
  const network = lucid.config().network;
  if (network === "Custom") {
    const provider = lucid.config().provider as {
      time?: number;
      slot?: number;
    };
    if (typeof provider.time === "number" && typeof provider.slot === "number") {
      const slotLength = 1000;
      const zeroTime = provider.time - provider.slot * slotLength;
      return zeroTime + slot * slotLength;
    }
    return slot * 1000;
  }
  return slotToUnixTime(network as Exclude<Network, "Custom">, slot);
};

const resolveDepositValidTo = (lucid: LucidEvolution): number => {
  const targetUnixTime = Date.now() + DEPOSIT_TX_TTL_MS;
  const slot = lucid.unixTimeToSlot(targetUnixTime);
  const alignedUnixTime = slotToUnixTimeForLucid(lucid, slot);
  if (alignedUnixTime > targetUnixTime) {
    return alignedUnixTime;
  }
  return slotToUnixTimeForLucid(lucid, slot + 1);
};

const fetchStakeCredentialDeposit = (
  lucid: LucidEvolution,
): Effect.Effect<bigint, SubmitDepositError> =>
  Effect.tryPromise({
    try: async () => {
      const provider = lucid.config().provider as DepositTxProvider;
      if (typeof provider.getProtocolParameters !== "function") {
        throw new Error(
          "Cardano provider does not expose protocol parameters required for certificate deposit",
        );
      }

      const protocolParameters = await provider.getProtocolParameters();
      if (typeof protocolParameters.keyDeposit !== "bigint") {
        throw new Error("Provider protocol parameters did not include keyDeposit");
      }

      return protocolParameters.keyDeposit;
    },
    catch: (cause) =>
      new SubmitDepositError({
        message: "Failed to resolve stake credential deposit for deposit transaction",
        cause,
      }),
  });

const addScriptStakeRegistrationCertificate = (
  tx: TxBuilder,
  witnessScript: CertificateValidator,
  witnessRedeemer: string,
  stakeCredentialDeposit: bigint,
): TxBuilder => {
  const rawConfig = (tx as InternalTxBuilder).rawConfig();
  rawConfig.programs.push(
    Effect.sync(() => {
      const witnessScriptHash = validatorToScriptHash(witnessScript);
      const credential = CML.Credential.new_script(
        CML.ScriptHash.from_hex(witnessScriptHash),
      );
      const certBuilder = CML.SingleCertificateBuilder.new(
        CML.Certificate.new_reg_cert(credential, stakeCredentialDeposit),
      );
      const plutusWitness = CML.PartialPlutusWitness.new(
        CML.PlutusScriptWitness.new_script(
          CML.PlutusScript.from_v3(
            CML.PlutusV3Script.from_cbor_hex(witnessScript.script),
          ),
        ),
        CML.PlutusData.from_cbor_hex(witnessRedeemer),
      );

      rawConfig.txBuilder.add_cert(
        certBuilder.plutus_script(plutusWitness, CML.Ed25519KeyHashList.new()),
      );
    }),
  );
  return tx;
};

const canonicalUtxoOrder = (a: UTxO, b: UTxO): number => {
  const hashOrder = a.txHash.localeCompare(b.txHash);
  if (hashOrder !== 0) {
    return hashOrder;
  }
  return a.outputIndex - b.outputIndex;
};

const fetchHubOracleReferenceProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<UTxO, SDK.HubOracleError | SDK.LucidError | SDK.Bech32DeserializationError | SubmitDepositError> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message: "Cardano network not found while preparing deposit transaction",
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const actual = yield* SDK.fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      hubOraclePolicyId: contracts.hubOracle.policyId,
    });
    const expectedDatum = yield* SDK.makeHubOracleDatum(contracts);

    if (
      actual.datum.deposit !== expectedDatum.deposit ||
      JSON.stringify(actual.datum.depositAddr) !==
        JSON.stringify(expectedDatum.depositAddr)
    ) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message:
            "On-chain hub oracle deployment does not match the locally configured deposit contract",
          cause: {
            expectedPolicyId: expectedDatum.deposit,
            actualPolicyId: actual.datum.deposit,
            expectedAddress: expectedDatum.depositAddr,
            actualAddress: actual.datum.depositAddr,
          },
        }),
      );
    }

    return actual.utxo;
  });

export const buildUnsignedDepositTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  TxSignBuilder,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message: "Cardano network not found while preparing deposit transaction",
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const hubOracleRefInput = yield* fetchHubOracleReferenceProgram(
      lucid,
      contracts,
    );

    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch wallet UTxOs for deposit submission",
          cause,
        }),
    });
    const sortedWalletUtxos = [...walletUtxos].sort(canonicalUtxoOrder);
    const nonceInput = sortedWalletUtxos[0];
    if (nonceInput === undefined) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message: "Failed to build deposit transaction",
          cause: "No UTxOs found in wallet",
        }),
      );
    }

    const nonceAssetName = yield* SDK.hashHexWithBlake2b256(
      outputReferenceToPlutusDataCbor(nonceInput),
    );
    const depositUnit = toUnit(contracts.deposit.policyId, nonceAssetName);
    if ((config.additionalAssets[depositUnit] ?? 0n) !== 0n) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message:
            "Additional asset list must not include the deposit authentication NFT unit",
          cause: depositUnit,
        }),
      );
    }

    const witnessScript = yield* buildWitnessCertificateValidator(nonceAssetName);
    const witnessScriptHash = validatorToScriptHash(witnessScript);
    const stakeCredentialDeposit = yield* fetchStakeCredentialDeposit(lucid);
    const validTo = resolveDepositValidTo(lucid);
    const inclusionTime = SDK.resolveEventInclusionTime(validTo, network);
    const l2AddressData = yield* SDK.addressDataFromBech32(config.l2Address);
    const l2DatumData =
      config.l2Datum === null ? null : LucidData.from(config.l2Datum);
    const depositDatum = {
      event: {
        id: {
          transactionId: nonceInput.txHash,
          outputIndex: BigInt(nonceInput.outputIndex),
        },
        info: {
          l2Address: l2AddressData,
          l2Datum: l2DatumData,
        },
      },
      inclusionTime: BigInt(inclusionTime),
      witness: witnessScriptHash,
    } satisfies SDK.DepositDatum;
    const depositDatumCbor = LucidData.to(
      depositDatum,
      DepositDatumWithWitnessSchema,
    );

    const mkDepositMintRedeemerBuilder = (
      layout: DepositDraftLayout,
    ): RedeemerBuilder => ({
      kind: "selected",
      inputs: [nonceInput],
      makeRedeemer: (inputIndices) => {
        const nonceInputIndex = inputIndices[0];
        if (nonceInputIndex === undefined || inputIndices.length !== 1) {
          throw new Error(
            `Deposit redeemer builder expected exactly one selected nonce input, got ${inputIndices.length.toString()}`,
          );
        }
        return LucidData.to(
          {
            AuthenticateEvent: {
              nonceInputIndex,
              eventOutputIndex: layout.eventOutputIndex,
              hubRefInputIndex: HUB_REFERENCE_INPUT_INDEX,
              witnessRegistrationRedeemerIndex:
                layout.witnessRegistrationRedeemerIndex,
            },
          },
          SDK.DepositMintRedeemer,
        );
      },
    });
    const witnessRegistrationRedeemer = yield* Effect.try({
      try: () =>
        LucidData.to(
          {
            MintOrBurn: {
              targetPolicy: contracts.deposit.policyId,
            },
          },
          WitnessPublishRedeemer,
        ),
      catch: (cause) =>
        new SubmitDepositError({
          message:
            "Failed to encode witness-registration redeemer for deposit transaction",
          cause: {
            depositPolicyId: contracts.deposit.policyId,
            rawCause: cause instanceof Error ? cause.message : String(cause),
          },
        }),
    });

    const outputAssets: Assets = {
      ...config.additionalAssets,
      lovelace: config.lovelace,
      [depositUnit]: 1n,
    };

    const mkDepositTx = (layout: DepositDraftLayout) =>
      addScriptStakeRegistrationCertificate(
        lucid
          .newTx()
          .collectFrom([nonceInput])
          .readFrom([hubOracleRefInput])
          .mintAssets({ [depositUnit]: 1n }, mkDepositMintRedeemerBuilder(layout))
          .pay.ToAddressWithData(
            contracts.deposit.spendingScriptAddress,
            {
              kind: "inline",
              value: depositDatumCbor,
            },
            outputAssets,
          )
          .validTo(validTo)
          .attach.MintingPolicy(contracts.deposit.mintingScript),
        witnessScript,
        witnessRegistrationRedeemer,
        stakeCredentialDeposit,
      );

    const initialLayout: DepositDraftLayout = {
      eventOutputIndex: 0n,
      witnessRegistrationRedeemerIndex: 0n,
    };
    const draftUnsigned = yield* Effect.tryPromise({
      try: () =>
        withStubbedProviderEvaluation(lucid, () =>
          mkDepositTx(initialLayout).complete({ localUPLCEval: false }),
        ),
      catch: (cause) =>
        new SubmitDepositError({
          message: "Failed to build deposit draft transaction",
          cause,
        }),
    });
    const resolvedLayout = deriveDepositDraftLayout({
      tx: draftUnsigned.toTransaction(),
      depositAddress: contracts.deposit.spendingScriptAddress,
      depositUnit,
    });

    return yield* Effect.tryPromise({
      try: () => mkDepositTx(resolvedLayout).complete({ localUPLCEval: true }),
      catch: (cause) =>
        new SubmitDepositError({
          message: "Failed to build deposit transaction",
          cause,
        }),
    });
  });

export const submitDepositProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  string,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
  | TxSubmitError
  | TxConfirmError
  | TxSignError
> =>
  Effect.gen(function* () {
    const tx = yield* buildUnsignedDepositTxProgram(lucid, contracts, config);
    return yield* handleSignSubmit(lucid, tx);
  });

export const parseLovelace = (value: string): bigint => {
  const normalized = value.trim();
  if (!LOVELACE_INTEGER_PATTERN.test(normalized)) {
    throw new Error(
      `Invalid lovelace amount "${value}". Use a positive integer number of lovelace.`,
    );
  }

  const lovelace = BigInt(normalized);

  if (lovelace <= 0n) {
    throw new Error("Deposit lovelace amount must be greater than zero.");
  }

  return lovelace;
};

const normalizeHex = (value: string, field: string): string => {
  const normalized = value.trim();
  if (!HEX_PATTERN.test(normalized) || normalized.length % 2 !== 0) {
    throw new Error(`Invalid ${field}: expected even-length hex, got "${value}".`);
  }
  return normalized.toLowerCase();
};

export const parseAdditionalAssetSpec = (
  spec: string,
): {
  readonly unit: string;
  readonly amount: bigint;
} => {
  const trimmed = spec.trim();
  const colonIndex = trimmed.lastIndexOf(":");
  if (colonIndex <= 0 || colonIndex === trimmed.length - 1) {
    throw new Error(
      `Invalid asset spec "${spec}". Expected policyId.assetName:amount.`,
    );
  }

  const assetId = trimmed.slice(0, colonIndex);
  const amountString = trimmed.slice(colonIndex + 1).trim();
  const dotIndex = assetId.indexOf(".");
  if (dotIndex <= 0) {
    throw new Error(
      `Invalid asset spec "${spec}". Expected policyId.assetName:amount.`,
    );
  }

  const policyId = assetId.slice(0, dotIndex).trim();
  const assetName = assetId.slice(dotIndex + 1).trim();
  if (!POLICY_ID_PATTERN.test(policyId)) {
    throw new Error(
      `Invalid policy ID in asset spec "${spec}". Expected 56 hex characters.`,
    );
  }

  const normalizedAssetName = normalizeHex(assetName, "asset name");
  if (normalizedAssetName.length > 64) {
    throw new Error(
      `Invalid asset name in asset spec "${spec}". Cardano asset names must be at most 32 bytes.`,
    );
  }

  if (!/^\d+$/.test(amountString)) {
    throw new Error(
      `Invalid asset amount in "${spec}". Expected a positive integer quantity.`,
    );
  }
  const amount = BigInt(amountString);
  if (amount <= 0n) {
    throw new Error(
      `Invalid asset amount in "${spec}". Quantity must be greater than zero.`,
    );
  }

  return {
    unit: `${policyId.toLowerCase()}${normalizedAssetName}`,
    amount,
  };
};

export const parseAdditionalAssetSpecs = (
  specs: readonly string[],
): Readonly<Assets> => {
  const assets: Assets = {};
  for (const spec of specs) {
    const { unit, amount } = parseAdditionalAssetSpec(spec);
    if (assets[unit] !== undefined) {
      throw new Error(
        `Duplicate additional asset "${unit}" provided. Combine quantities before submitting the command.`,
      );
    }
    assets[unit] = amount;
  }
  return assets;
};

export const parseSubmitDepositConfig = ({
  l2Address,
  l2Datum,
  lovelace,
  assetSpecs,
}: {
  readonly l2Address: string;
  readonly l2Datum?: string;
  readonly lovelace: string;
  readonly assetSpecs: readonly string[];
}): SubmitDepositConfig => {
  const normalizedL2Address = l2Address.trim();
  if (normalizedL2Address.length === 0) {
    throw new Error("L2 address must not be empty.");
  }
  try {
    getAddressDetails(normalizedL2Address);
  } catch (cause) {
    throw new Error(
      `Invalid L2 address "${normalizedL2Address}": ${String(cause)}`,
    );
  }

  const normalizedDatum =
    l2Datum === undefined || l2Datum.trim().length === 0
      ? null
      : normalizeHex(l2Datum, "L2 datum");

  return {
    l2Address: normalizedL2Address,
    l2Datum: normalizedDatum,
    lovelace: parseLovelace(lovelace),
    additionalAssets: parseAdditionalAssetSpecs(assetSpecs),
  };
};
