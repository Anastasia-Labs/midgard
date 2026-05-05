/**
 * Deposit submission flow for projecting deposit observations into Midgard
 * state.
 * This module owns the off-chain deposit transaction builder and reuses the
 * shared time and ledger-order helpers extracted during cleanup.
 */
import { Effect, Data as EffectData } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Lucid as makeLucid,
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
  getAddressDetails,
  scriptHashToCredential,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import {
  parseAdditionalAssetSpec as parseAdditionalAssetSpecShared,
  parseAdditionalAssetSpecs as parseAdditionalAssetSpecsShared,
  parseLovelaceAmount,
} from "@/asset-specs.js";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "@/lucid-time.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
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

type DepositTxProvider = {
  getProtocolParameters?: () => Promise<{
    keyDeposit: bigint;
  }>;
};

type DepositDraftLayout = {
  readonly eventOutputIndex: bigint;
  readonly witnessRegistrationRedeemerIndex: bigint;
  readonly hubRefInputIndex: bigint;
};

export type SubmitDepositReferenceScripts = {
  readonly depositMinting: UTxO;
};

export type SubmitDepositConfig = {
  readonly l2Address: string;
  readonly l2Datum: string | null;
  readonly lovelace: bigint;
  readonly additionalAssets: Readonly<Assets>;
  readonly referenceScripts?: SubmitDepositReferenceScripts;
};

export type BuildDepositRequest = SubmitDepositConfig & {
  readonly fundingAddress: string;
  readonly fundingUtxos: readonly UTxO[];
};

export type BuiltUnsignedDepositTx = {
  readonly unsignedTxCbor: string;
};

export type DepositBuildMetadata = {
  readonly depositAddress: string;
  readonly depositEventId: string;
  readonly depositAssetName: string;
  readonly depositAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

export type SubmittedDeposit = {
  readonly txHash: string;
  readonly metadata: DepositBuildMetadata;
};

export class SubmitDepositError extends EffectData.TaggedError(
  "SubmitDepositError",
)<{
  message: string;
  cause: unknown;
}> {}

const DEPOSIT_TX_TTL_MS = 60_000;
const HEX_PATTERN = /^[0-9a-fA-F]*$/;
const TX_HASH_PATTERN = /^[0-9a-fA-F]{64}$/;
const DATUM_HASH_PATTERN = /^[0-9a-fA-F]{64}$/;
const ASSET_UNIT_PATTERN = /^[0-9a-fA-F]{56}(?:[0-9a-fA-F]{0,64})$/;
const MAX_DEPOSIT_BUILD_FUNDING_UTXOS = 128;
const MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES = 64;
const MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS = 64;
const DEPOSIT_EVENT_OUTPUT_INDEX = 0n;
const WITNESS_REGISTRATION_REDEEMER_TX_INFO_INDEX = 1n;

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

const deriveDepositDraftLayout = ({
  tx,
  depositAddress,
  depositUnit,
  hubOracleRefInput,
}: {
  readonly tx: CML.Transaction;
  readonly depositAddress: string;
  readonly depositUnit: string;
  readonly hubOracleRefInput: UTxO;
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
  if (
    witnessRegistrationRedeemerIndex === undefined ||
    witnessRegistrationRedeemerIndex < 0
  ) {
    throw new Error(
      "Failed to derive tx-info redeemer index for witness registration",
    );
  }
  const referenceInputs = tx.body().reference_inputs();
  if (referenceInputs === undefined) {
    throw new Error("Deposit draft did not include reference inputs");
  }
  const hubRefInputIndex = findOutRefIndex(
    collectSortedInputOutRefs(referenceInputs),
    hubOracleRefInput,
  );
  if (hubRefInputIndex === undefined) {
    throw new Error("Deposit draft did not include hub-oracle reference input");
  }

  return {
    eventOutputIndex,
    witnessRegistrationRedeemerIndex: BigInt(witnessRegistrationRedeemerIndex),
    hubRefInputIndex: BigInt(hubRefInputIndex),
  };
};

const resolveDepositValidTo = (lucid: LucidEvolution): number => {
  const targetUnixTime = Date.now() + DEPOSIT_TX_TTL_MS;
  const slot = lucid.unixTimeToSlot(targetUnixTime);
  const alignedUnixTime = slotToUnixTimeForLucidOrEmulatorFallback(lucid, slot);
  if (alignedUnixTime > targetUnixTime) {
    return alignedUnixTime;
  }
  return slotToUnixTimeForLucidOrEmulatorFallback(lucid, slot + 1);
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
        throw new Error(
          "Provider protocol parameters did not include keyDeposit",
        );
      }

      return protocolParameters.keyDeposit;
    },
    catch: (cause) =>
      new SubmitDepositError({
        message:
          "Failed to resolve stake credential deposit for deposit transaction",
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
  | SubmitDepositError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message:
            "Cardano network not found while preparing deposit transaction",
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
      JSON.stringify(actual.datum.deposit_addr) !==
        JSON.stringify(expectedDatum.deposit_addr)
    ) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message:
            "On-chain hub oracle deployment does not match the locally configured deposit contract",
          cause: {
            expectedPolicyId: expectedDatum.deposit,
            actualPolicyId: actual.datum.deposit,
            expectedAddress: expectedDatum.deposit_addr,
            actualAddress: actual.datum.deposit_addr,
          },
        }),
      );
    }

    return actual.utxo;
  });

const serializeBuiltUnsignedDepositTx = ({
  tx,
}: {
  readonly tx: TxSignBuilder;
}): BuiltUnsignedDepositTx => {
  return {
    unsignedTxCbor: tx.toCBOR(),
  };
};

const buildUnsignedDepositTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: DepositBuildMetadata;
  },
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
          message:
            "Cardano network not found while preparing deposit transaction",
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

    const depositEventId = outputReferenceToPlutusDataCbor(nonceInput);
    const nonceAssetName = yield* SDK.hashHexWithBlake2b256(depositEventId);
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

    const witnessScript =
      SDK.buildUserEventWitnessCertificateValidator(nonceAssetName);
    const witnessScriptHash = SDK.userEventWitnessScriptHash(nonceAssetName);
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
          l2_address: l2AddressData,
          l2_datum: l2DatumData,
        },
      },
      inclusion_time: BigInt(inclusionTime),
      witness: witnessScriptHash,
    } satisfies SDK.DepositDatum;
    const depositDatumCbor = LucidData.to(depositDatum, SDK.DepositDatum);

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
          contracts.deposit.policyId,
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

    /**
     * Builds the deposit submission transaction.
     */
    const mkDepositTx = (
      builderLucid: LucidEvolution,
      layout: DepositDraftLayout,
    ) => {
      const referenceInputs =
        config.referenceScripts === undefined
          ? [hubOracleRefInput]
          : [hubOracleRefInput, config.referenceScripts.depositMinting];
      const tx = builderLucid
        .newTx()
        .collectFrom([nonceInput])
        .readFrom(referenceInputs)
        .mintAssets({ [depositUnit]: 1n }, mkDepositMintRedeemerBuilder(layout))
        .pay.ToAddressWithData(
          contracts.deposit.spendingScriptAddress,
          {
            kind: "inline",
            value: depositDatumCbor,
          },
          outputAssets,
        )
        .validTo(validTo);
      return addScriptStakeRegistrationCertificate(
        config.referenceScripts === undefined
          ? tx.attach.MintingPolicy(contracts.deposit.mintingScript)
          : tx,
        witnessScript,
        witnessRegistrationRedeemer,
        stakeCredentialDeposit,
      );
    };

    const referenceInputs =
      config.referenceScripts === undefined
        ? [hubOracleRefInput]
        : [hubOracleRefInput, config.referenceScripts.depositMinting];
    const initialHubRefInputIndex = [...referenceInputs]
      .sort(compareOutRefs)
      .findIndex(
        (utxo) =>
          utxo.txHash === hubOracleRefInput.txHash &&
          utxo.outputIndex === hubOracleRefInput.outputIndex,
      );
    if (initialHubRefInputIndex < 0) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message: "Failed to derive initial deposit hub reference input index",
          cause:
            "hub-oracle reference input missing from deposit reference set",
        }),
      );
    }

    const assumedLayout: DepositDraftLayout = {
      eventOutputIndex: DEPOSIT_EVENT_OUTPUT_INDEX,
      witnessRegistrationRedeemerIndex:
        WITNESS_REGISTRATION_REDEEMER_TX_INFO_INDEX,
      hubRefInputIndex: BigInt(initialHubRefInputIndex),
    };

    const tx = yield* Effect.tryPromise({
      try: () =>
        mkDepositTx(lucid, assumedLayout).complete({ localUPLCEval: true }),
      catch: (cause) =>
        new SubmitDepositError({
          message: `Failed to build deposit transaction: ${String(cause)}`,
          cause,
        }),
    });

    const resolvedLayout = yield* Effect.try({
      try: () =>
        deriveDepositDraftLayout({
          tx: tx.toTransaction(),
          depositAddress: contracts.deposit.spendingScriptAddress,
          depositUnit,
          hubOracleRefInput,
        }),
      catch: (cause) =>
        new SubmitDepositError({
          message: "Failed to verify deposit transaction layout",
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
        new SubmitDepositError({
          message:
            "Built deposit transaction layout drifted from expected form",
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
        depositAddress: contracts.deposit.spendingScriptAddress,
        depositEventId,
        depositAssetName: nonceAssetName,
        depositAuthUnit: depositUnit,
        nonceInput,
        validTo,
        inclusionTime,
      },
    };
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
  buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

export const buildUnsignedDepositTxFromFundingContextProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  request: BuildDepositRequest,
): Effect.Effect<
  BuiltUnsignedDepositTx,
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
          message:
            "Cardano network not found while preparing deposit transaction",
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const externalLucid = yield* Effect.tryPromise({
      try: () => makeLucid(lucid.config().provider, network),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to initialize external-wallet deposit builder",
          cause,
        }),
    });
    yield* Effect.sync(() =>
      externalLucid.selectWallet.fromAddress(request.fundingAddress, [
        ...request.fundingUtxos,
      ]),
    );

    const { tx } = yield* buildUnsignedDepositTxWithMetadataProgram(
      externalLucid,
      contracts,
      request,
    );
    return serializeBuiltUnsignedDepositTx({
      tx,
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
  submitDepositWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map((result) => result.txHash),
  );

export const submitDepositWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  SubmittedDeposit,
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
    const { tx, metadata } = yield* buildUnsignedDepositTxWithMetadataProgram(
      lucid,
      contracts,
      config,
    );
    const txHash = yield* handleSignSubmit(lucid, tx);
    return { txHash, metadata };
  });

export const parseLovelace = (value: string): bigint => {
  return parseLovelaceAmount(
    value,
    "Deposit lovelace amount must be greater than zero.",
  );
};

const normalizeHex = (value: string, field: string): string => {
  const normalized = value.trim();
  if (!HEX_PATTERN.test(normalized) || normalized.length % 2 !== 0) {
    throw new Error(
      `Invalid ${field}: expected even-length hex, got "${value}".`,
    );
  }
  return normalized.toLowerCase();
};

export const parseAdditionalAssetSpec = (
  spec: string,
): {
  readonly unit: string;
  readonly amount: bigint;
} => parseAdditionalAssetSpecShared(spec);

export const parseAdditionalAssetSpecs = (
  specs: readonly string[],
): Readonly<Assets> => parseAdditionalAssetSpecsShared(specs);

type UnknownRecord = Record<string, unknown>;

const asObject = (value: unknown, field: string): UnknownRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object.`);
  }
  return value as UnknownRecord;
};

const parseRequiredString = (value: unknown, field: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${field} must be a string.`);
  }
  const normalized = value.trim();
  if (normalized.length === 0) {
    throw new Error(`${field} must not be empty.`);
  }
  return normalized;
};

const parseOptionalString = (
  value: unknown,
  field: string,
): string | null | undefined => {
  if (value === undefined || value === null) {
    return null;
  }
  if (typeof value !== "string") {
    throw new Error(`${field} must be a string when provided.`);
  }
  const normalized = value.trim();
  return normalized.length === 0 ? null : normalized;
};

const parsePositiveIntegerString = (value: string, field: string): bigint => {
  const normalized = value.trim();
  if (!/^(?:0|[1-9]\d*)$/.test(normalized)) {
    throw new Error(`${field} must be a positive integer string.`);
  }
  const amount = BigInt(normalized);
  if (amount <= 0n) {
    throw new Error(`${field} must be greater than zero.`);
  }
  return amount;
};

const parseNonNegativeInteger = (value: unknown, field: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${field} must be a non-negative integer.`);
  }
  return value;
};

const expectedNetworkIdForAddressValidation = (
  network: string | undefined,
): number | undefined => {
  if (network === undefined || network === "Custom") {
    return undefined;
  }
  return network === "Mainnet" ? 1 : 0;
};

const parseAddressString = ({
  value,
  field,
  expectedNetwork,
}: {
  readonly value: unknown;
  readonly field: string;
  readonly expectedNetwork?: string;
}): string => {
  const normalized = parseRequiredString(value, field);
  let details: ReturnType<typeof getAddressDetails>;
  try {
    details = getAddressDetails(normalized);
  } catch (cause) {
    throw new Error(`Invalid ${field} "${normalized}": ${String(cause)}`);
  }
  const expectedNetworkId =
    expectedNetworkIdForAddressValidation(expectedNetwork);
  if (
    expectedNetworkId !== undefined &&
    details.networkId !== expectedNetworkId
  ) {
    throw new Error(
      `${field} must target the configured ${expectedNetwork} network.`,
    );
  }
  return details.address.bech32;
};

const normalizeAssetUnit = (value: string, field: string): string => {
  const normalized = value.trim();
  if (!ASSET_UNIT_PATTERN.test(normalized)) {
    throw new Error(
      `${field} must be a Cardano unit string (56 hex policy id plus optional asset-name hex).`,
    );
  }
  return normalized.toLowerCase();
};

const normalizeOptionalHexField = (
  value: unknown,
  field: string,
  pattern?: RegExp,
): string | null | undefined => {
  if (value === undefined || value === null) {
    return null;
  }
  if (typeof value !== "string") {
    throw new Error(`${field} must be a hex string when provided.`);
  }
  const normalized = value.trim();
  if (normalized.length === 0) {
    return null;
  }
  if (pattern !== undefined) {
    if (!pattern.test(normalized)) {
      throw new Error(`Invalid ${field}: expected hex, got "${value}".`);
    }
    return normalized.toLowerCase();
  }
  return normalizeHex(normalized, field);
};

const parseFundingAssets = (value: unknown, field: string): Assets => {
  const rawAssets = asObject(value, field);
  const entries = Object.entries(rawAssets);
  if (entries.length === 0) {
    throw new Error(`${field} must include at least lovelace.`);
  }
  if (entries.length > MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES) {
    throw new Error(
      `${field} exceeds the maximum asset entry count (${entries.length} > ${MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES}).`,
    );
  }

  const assets: Assets = {};
  for (const [unitKey, amountValue] of entries) {
    const unit =
      unitKey === "lovelace"
        ? "lovelace"
        : normalizeAssetUnit(unitKey, `${field}.${unitKey}`);
    if (assets[unit] !== undefined) {
      throw new Error(`Duplicate asset unit "${unit}" in ${field}.`);
    }
    assets[unit] = parsePositiveIntegerString(
      parseRequiredString(amountValue, `${field}.${unit}`),
      `${field}.${unit}`,
    );
  }
  if (assets.lovelace === undefined) {
    throw new Error(`${field} must include lovelace.`);
  }
  return assets;
};

const parseAdditionalAssetsFromRequest = (value: unknown): Readonly<Assets> => {
  if (value === undefined || value === null) {
    return {};
  }
  if (!Array.isArray(value)) {
    throw new Error("additionalAssets must be an array when provided.");
  }
  if (value.length > MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS) {
    throw new Error(
      `additionalAssets exceeds the maximum entry count (${value.length} > ${MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS}).`,
    );
  }

  const assets: Assets = {};
  for (const [index, entry] of value.entries()) {
    const raw = asObject(entry, `additionalAssets[${index.toString()}]`);
    const unit = normalizeAssetUnit(
      parseRequiredString(
        raw.unit,
        `additionalAssets[${index.toString()}].unit`,
      ),
      `additionalAssets[${index.toString()}].unit`,
    );
    if (assets[unit] !== undefined) {
      throw new Error(`Duplicate additional asset "${unit}" provided.`);
    }
    assets[unit] = parsePositiveIntegerString(
      parseRequiredString(
        raw.amount,
        `additionalAssets[${index.toString()}].amount`,
      ),
      `additionalAssets[${index.toString()}].amount`,
    );
  }
  return assets;
};

const parseFundingUtxos = ({
  value,
  fundingAddress,
  expectedNetwork,
}: {
  readonly value: unknown;
  readonly fundingAddress: string;
  readonly expectedNetwork?: string;
}): readonly UTxO[] => {
  if (!Array.isArray(value)) {
    throw new Error("fundingUtxos must be an array.");
  }
  if (value.length === 0) {
    throw new Error("fundingUtxos must not be empty.");
  }
  if (value.length > MAX_DEPOSIT_BUILD_FUNDING_UTXOS) {
    throw new Error(
      `fundingUtxos exceeds the maximum count (${value.length} > ${MAX_DEPOSIT_BUILD_FUNDING_UTXOS}).`,
    );
  }

  const seenOutRefs = new Set<string>();
  return value.map((entry, index) => {
    const raw = asObject(entry, `fundingUtxos[${index.toString()}]`);
    const txHash = parseRequiredString(
      raw.txHash,
      `fundingUtxos[${index.toString()}].txHash`,
    ).toLowerCase();
    if (!TX_HASH_PATTERN.test(txHash)) {
      throw new Error(
        `fundingUtxos[${index.toString()}].txHash must be a 32-byte transaction hash.`,
      );
    }
    const outputIndex = parseNonNegativeInteger(
      raw.outputIndex,
      `fundingUtxos[${index.toString()}].outputIndex`,
    );
    const outRefKey = `${txHash}#${outputIndex.toString()}`;
    if (seenOutRefs.has(outRefKey)) {
      throw new Error(`Duplicate funding UTxO "${outRefKey}" provided.`);
    }
    seenOutRefs.add(outRefKey);

    const utxoAddress = parseAddressString({
      value: raw.address,
      field: `fundingUtxos[${index.toString()}].address`,
      expectedNetwork,
    });
    if (utxoAddress !== fundingAddress) {
      throw new Error(
        `fundingUtxos[${index.toString()}].address must match fundingAddress.`,
      );
    }

    const datumHash = normalizeOptionalHexField(
      raw.datumHash,
      `fundingUtxos[${index.toString()}].datumHash`,
      DATUM_HASH_PATTERN,
    );
    const datum = normalizeOptionalHexField(
      raw.datum,
      `fundingUtxos[${index.toString()}].datum`,
    );
    const scriptRef = normalizeOptionalHexField(
      raw.scriptRef,
      `fundingUtxos[${index.toString()}].scriptRef`,
    );
    if (scriptRef !== null) {
      throw new Error(
        `fundingUtxos[${index.toString()}].scriptRef is not supported for deposit build funding inputs.`,
      );
    }

    return {
      txHash,
      outputIndex,
      address: utxoAddress,
      assets: parseFundingAssets(
        raw.assets,
        `fundingUtxos[${index.toString()}].assets`,
      ),
      datumHash: datumHash ?? undefined,
      datum: datum ?? undefined,
      scriptRef: undefined,
    };
  });
};

const buildSubmitDepositConfig = ({
  l2Address,
  l2Datum,
  lovelace,
  additionalAssets,
  expectedNetwork,
}: {
  readonly l2Address: string;
  readonly l2Datum?: string | null;
  readonly lovelace: string;
  readonly additionalAssets: Readonly<Assets>;
  readonly expectedNetwork?: string;
}): SubmitDepositConfig => {
  const normalizedL2Address = parseAddressString({
    value: l2Address,
    field: "l2Address",
    expectedNetwork,
  });
  const normalizedDatum =
    l2Datum === undefined || l2Datum === null || l2Datum.trim().length === 0
      ? null
      : normalizeHex(l2Datum, "L2 datum");

  return {
    l2Address: normalizedL2Address,
    l2Datum: normalizedDatum,
    lovelace: parseLovelace(lovelace),
    additionalAssets,
  };
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
}): SubmitDepositConfig =>
  buildSubmitDepositConfig({
    l2Address,
    l2Datum,
    lovelace,
    additionalAssets: parseAdditionalAssetSpecs(assetSpecs),
  });

export const parseBuildDepositRequest = (
  payload: unknown,
  options?: {
    readonly expectedNetwork?: string;
  },
): BuildDepositRequest => {
  const body = asObject(payload, "Deposit build request");
  const fundingAddress = parseAddressString({
    value: body.fundingAddress,
    field: "fundingAddress",
    expectedNetwork: options?.expectedNetwork,
  });
  const fundingUtxos = parseFundingUtxos({
    value: body.fundingUtxos,
    fundingAddress,
    expectedNetwork: options?.expectedNetwork,
  });

  return {
    ...buildSubmitDepositConfig({
      l2Address: parseRequiredString(body.l2Address, "l2Address"),
      l2Datum: parseOptionalString(body.l2Datum, "l2Datum"),
      lovelace: parseRequiredString(body.lovelace, "lovelace"),
      additionalAssets: parseAdditionalAssetsFromRequest(body.additionalAssets),
      expectedNetwork: options?.expectedNetwork,
    }),
    fundingAddress,
    fundingUtxos,
  };
};
