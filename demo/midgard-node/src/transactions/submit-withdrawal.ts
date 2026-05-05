/**
 * Withdrawal submission flow for creating authenticated withdrawal-order
 * events on L1.
 */
import { Effect, Data as EffectData } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  type Assets,
  type CertificateValidator,
  Data as LucidData,
  type LucidEvolution,
  type RedeemerBuilder,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  coreToTxOutput,
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "@/lucid-time.js";
import {
  getRedeemerPointersInContextOrder,
  getTxInfoRedeemerIndexes,
} from "@/cml-redeemers.js";
import {
  collectSortedInputOutRefs,
  compareOutRefs,
  findOutRefIndex,
} from "@/tx-context.js";

type TxBuilderInternals = {
  txBuilder: {
    add_cert: (builder: unknown) => void;
  };
  programs: Array<Effect.Effect<void>>;
};

type InternalTxBuilder = TxBuilder & {
  rawConfig: () => TxBuilderInternals;
};

type WithdrawalTxProvider = {
  getProtocolParameters?: () => Promise<{
    keyDeposit: bigint;
  }>;
};

type WithdrawalDraftLayout = {
  readonly eventOutputIndex: bigint;
  readonly witnessRegistrationRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

export type SubmitWithdrawalReferenceScripts = {
  readonly withdrawalMinting: UTxO;
};

export type SubmitWithdrawalConfig = {
  readonly body: SDK.WithdrawalBody;
  readonly signature: SDK.WithdrawalSignature;
  readonly refundAddress: SDK.AddressData;
  readonly refundDatum?: SDK.CardanoDatum;
  readonly lovelace?: bigint;
  readonly referenceScripts?: SubmitWithdrawalReferenceScripts;
};

type WithdrawalBuildMetadata = {
  readonly withdrawalAddress: string;
  readonly withdrawalAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

export class SubmitWithdrawalError extends EffectData.TaggedError(
  "SubmitWithdrawalError",
)<{
  message: string;
  cause: unknown;
}> {}

const WITHDRAWAL_TX_TTL_MS = 60_000;
const WITHDRAWAL_EVENT_OUTPUT_INDEX = 0n;
const WITNESS_REGISTRATION_REDEEMER_TX_INFO_INDEX = 1n;
const DEFAULT_WITHDRAWAL_ORDER_LOVELACE = 3_000_000n;

const outputReferenceToPlutusDataCbor = (
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): string =>
  LucidData.to(
    {
      transactionId: utxo.txHash,
      outputIndex: BigInt(utxo.outputIndex),
    },
    SDK.OutputReference,
  );

const deriveWithdrawalDraftLayout = ({
  tx,
  withdrawalAddress,
  withdrawalUnit,
  hubOracleRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly withdrawalAddress: string;
  readonly withdrawalUnit: string;
  readonly hubOracleRefInput: UTxO;
}): WithdrawalDraftLayout => {
  const outputs = tx.body().outputs();
  let eventOutputIndex: bigint | null = null;
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === withdrawalAddress &&
      (output.assets[withdrawalUnit] ?? 0n) === 1n
    ) {
      eventOutputIndex = BigInt(index);
      break;
    }
  }
  if (eventOutputIndex === null) {
    throw new Error(
      `Failed to locate withdrawal event output for unit=${withdrawalUnit} at address=${withdrawalAddress}`,
    );
  }

  const pointers = getRedeemerPointersInContextOrder(tx);
  const certContextIndex = pointers.findIndex(
    (pointer) => pointer.tag === CML.RedeemerTag.Cert,
  );
  if (certContextIndex < 0) {
    throw new Error("Failed to locate certificate redeemer in withdrawal draft");
  }
  const txInfoIndexes = getTxInfoRedeemerIndexes(pointers);
  const witnessRegistrationRedeemerIndex = txInfoIndexes[certContextIndex];
  if (
    witnessRegistrationRedeemerIndex === undefined ||
    witnessRegistrationRedeemerIndex < 0
  ) {
    throw new Error(
      "Failed to derive tx-info redeemer index for withdrawal witness registration",
    );
  }

  const referenceInputs = tx.body().reference_inputs();
  if (referenceInputs === undefined) {
    throw new Error("Withdrawal draft did not include reference inputs");
  }
  const hubRefInputIndex = findOutRefIndex(
    collectSortedInputOutRefs(referenceInputs),
    hubOracleRefInput,
  );
  if (hubRefInputIndex === undefined) {
    throw new Error(
      "Withdrawal draft did not include hub-oracle reference input",
    );
  }

  return {
    eventOutputIndex,
    witnessRegistrationRedeemerIndex: BigInt(witnessRegistrationRedeemerIndex),
    hubRefInputIndex: BigInt(hubRefInputIndex),
  };
};

const resolveWithdrawalValidTo = (lucid: LucidEvolution): number => {
  const targetUnixTime = Date.now() + WITHDRAWAL_TX_TTL_MS;
  const slot = lucid.unixTimeToSlot(targetUnixTime);
  const alignedUnixTime = slotToUnixTimeForLucidOrEmulatorFallback(lucid, slot);
  if (alignedUnixTime > targetUnixTime) {
    return alignedUnixTime;
  }
  return slotToUnixTimeForLucidOrEmulatorFallback(lucid, slot + 1);
};

const fetchStakeCredentialDeposit = (
  lucid: LucidEvolution,
): Effect.Effect<bigint, SubmitWithdrawalError> =>
  Effect.tryPromise({
    try: async () => {
      const provider = lucid.config().provider as WithdrawalTxProvider;
      if (typeof provider.getProtocolParameters !== "function") {
        throw new Error(
          "Cardano provider does not expose protocol parameters required for certificate deposit",
        );
      }

      const protocolParameters = await provider.getProtocolParameters();
      if (typeof protocolParameters.keyDeposit !== "bigint") {
        throw new Error(
          "Provider protocol parameters did not include keyDeposit",
        );
      }

      return protocolParameters.keyDeposit;
    },
    catch: (cause) =>
      new SubmitWithdrawalError({
        message:
          "Failed to resolve stake credential deposit for withdrawal transaction",
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
): Effect.Effect<
  UTxO,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SubmitWithdrawalError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SubmitWithdrawalError({
          message:
            "Cardano network not found while preparing withdrawal transaction",
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
      actual.datum.withdrawal !== expectedDatum.withdrawal ||
      JSON.stringify(actual.datum.withdrawal_addr) !==
        JSON.stringify(expectedDatum.withdrawal_addr)
    ) {
      return yield* Effect.fail(
        new SubmitWithdrawalError({
          message:
            "On-chain hub oracle deployment does not match the locally configured withdrawal contract",
          cause: {
            expectedPolicyId: expectedDatum.withdrawal,
            actualPolicyId: actual.datum.withdrawal,
            expectedAddress: expectedDatum.withdrawal_addr,
            actualAddress: actual.datum.withdrawal_addr,
          },
        }),
      );
    }

    return actual.utxo;
  });

const buildUnsignedWithdrawalTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitWithdrawalConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: WithdrawalBuildMetadata;
  },
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitWithdrawalError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SubmitWithdrawalError({
          message:
            "Cardano network not found while preparing withdrawal transaction",
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
          message: "Failed to fetch wallet UTxOs for withdrawal submission",
          cause,
        }),
    });
    const sortedWalletUtxos = [...walletUtxos].sort(canonicalUtxoOrder);
    const nonceInput = sortedWalletUtxos[0];
    if (nonceInput === undefined) {
      return yield* Effect.fail(
        new SubmitWithdrawalError({
          message: "Failed to build withdrawal transaction",
          cause: "No UTxOs found in wallet",
        }),
      );
    }

    const nonceAssetName = yield* SDK.hashHexWithBlake2b256(
      outputReferenceToPlutusDataCbor(nonceInput),
    );
    const withdrawalUnit = toUnit(contracts.withdrawal.policyId, nonceAssetName);
    const witnessScript =
      SDK.buildUserEventWitnessCertificateValidator(nonceAssetName);
    const witnessScriptHash = SDK.userEventWitnessScriptHash(nonceAssetName);
    const stakeCredentialDeposit = yield* fetchStakeCredentialDeposit(lucid);
    const validTo = resolveWithdrawalValidTo(lucid);
    const inclusionTime = SDK.resolveEventInclusionTime(validTo, network);

    const withdrawalDatum: SDK.WithdrawalOrderDatum = {
      event: {
        id: {
          transactionId: nonceInput.txHash,
          outputIndex: BigInt(nonceInput.outputIndex),
        },
        info: {
          body: config.body,
          signature: config.signature,
          validity: "WithdrawalIsValid",
        },
      },
      inclusion_time: BigInt(inclusionTime),
      witness: witnessScriptHash,
      refund_address: config.refundAddress,
      refund_datum: config.refundDatum ?? "NoDatum",
    };
    const withdrawalDatumCbor = LucidData.to(
      withdrawalDatum,
      SDK.WithdrawalOrderDatum,
    );

    const mkWithdrawalMintRedeemerBuilder = (
      layout: WithdrawalDraftLayout,
    ): RedeemerBuilder => ({
      kind: "selected",
      inputs: [nonceInput],
      makeRedeemer: (inputIndices) => {
        const nonceInputIndex = inputIndices[0];
        if (nonceInputIndex === undefined || inputIndices.length !== 1) {
          throw new Error(
            `Withdrawal redeemer builder expected exactly one selected nonce input, got ${inputIndices.length.toString()}`,
          );
        }
        return SDK.encodeUserEventAuthenticateMintRedeemer({
          nonceInputIndex,
          eventOutputIndex: layout.eventOutputIndex,
          hubRefInputIndex: layout.hubRefInputIndex,
          witnessRegistrationRedeemerIndex:
            layout.witnessRegistrationRedeemerIndex,
        });
      },
    });
    const witnessRegistrationRedeemer = yield* Effect.try({
      try: () =>
        SDK.encodeUserEventWitnessMintOrBurnRedeemer(
          contracts.withdrawal.policyId,
        ),
      catch: (cause) =>
        new SubmitWithdrawalError({
          message:
            "Failed to encode witness-registration redeemer for withdrawal transaction",
          cause: {
            withdrawalPolicyId: contracts.withdrawal.policyId,
            rawCause: cause instanceof Error ? cause.message : String(cause),
          },
        }),
    });

    const outputAssets: Assets = {
      lovelace: config.lovelace ?? DEFAULT_WITHDRAWAL_ORDER_LOVELACE,
      [withdrawalUnit]: 1n,
    };

    const mkWithdrawalTx = (
      builderLucid: LucidEvolution,
      layout: WithdrawalDraftLayout,
    ) => {
      const referenceInputs =
        config.referenceScripts === undefined
          ? [hubOracleRefInput]
          : [hubOracleRefInput, config.referenceScripts.withdrawalMinting];
      const tx = builderLucid
        .newTx()
        .collectFrom([nonceInput])
        .readFrom(referenceInputs)
        .mintAssets(
          { [withdrawalUnit]: 1n },
          mkWithdrawalMintRedeemerBuilder(layout),
        )
        .pay.ToAddressWithData(
          contracts.withdrawal.spendingScriptAddress,
          {
            kind: "inline",
            value: withdrawalDatumCbor,
          },
          outputAssets,
        )
        .validTo(validTo);
      return addScriptStakeRegistrationCertificate(
        config.referenceScripts === undefined
          ? tx.attach.MintingPolicy(contracts.withdrawal.mintingScript)
          : tx,
        witnessScript,
        witnessRegistrationRedeemer,
        stakeCredentialDeposit,
      );
    };

    const referenceInputs =
      config.referenceScripts === undefined
        ? [hubOracleRefInput]
        : [hubOracleRefInput, config.referenceScripts.withdrawalMinting];
    const initialHubRefInputIndex = [...referenceInputs]
      .sort(compareOutRefs)
      .findIndex(
        (utxo) =>
          utxo.txHash === hubOracleRefInput.txHash &&
          utxo.outputIndex === hubOracleRefInput.outputIndex,
      );
    if (initialHubRefInputIndex < 0) {
      return yield* Effect.fail(
        new SubmitWithdrawalError({
          message:
            "Failed to derive initial withdrawal hub reference input index",
          cause:
            "hub-oracle reference input missing from withdrawal reference set",
        }),
      );
    }

    const assumedLayout: WithdrawalDraftLayout = {
      eventOutputIndex: WITHDRAWAL_EVENT_OUTPUT_INDEX,
      witnessRegistrationRedeemerIndex:
        WITNESS_REGISTRATION_REDEEMER_TX_INFO_INDEX,
      hubRefInputIndex: BigInt(initialHubRefInputIndex),
    };

    const tx = yield* Effect.tryPromise({
      try: () =>
        mkWithdrawalTx(lucid, assumedLayout).complete({ localUPLCEval: true }),
      catch: (cause) =>
        new SubmitWithdrawalError({
          message: `Failed to build withdrawal transaction: ${String(cause)}`,
          cause,
        }),
    });

    const resolvedLayout = yield* Effect.try({
      try: () =>
        deriveWithdrawalDraftLayout({
          tx: tx.toTransaction(),
          withdrawalAddress: contracts.withdrawal.spendingScriptAddress,
          withdrawalUnit,
          hubOracleRefInput,
        }),
      catch: (cause) =>
        new SubmitWithdrawalError({
          message: "Failed to verify withdrawal transaction layout",
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
        new SubmitWithdrawalError({
          message:
            "Built withdrawal transaction layout drifted from expected form",
          cause: {
            assumedLayout,
            resolvedLayout,
          },
        }),
      );
    }

    return {
      tx,
      metadata: {
        withdrawalAddress: contracts.withdrawal.spendingScriptAddress,
        withdrawalAuthUnit: withdrawalUnit,
        nonceInput,
        validTo,
        inclusionTime,
      },
    };
  });

export const buildUnsignedWithdrawalTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitWithdrawalConfig,
): Effect.Effect<
  TxSignBuilder,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitWithdrawalError
> =>
  buildUnsignedWithdrawalTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );
