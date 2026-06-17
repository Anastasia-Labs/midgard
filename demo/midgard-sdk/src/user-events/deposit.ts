import {
  type Assets,
  credentialToAddress,
  Data,
  LucidEvolution,
  PolicyId,
  scriptHashToCredential,
  toUnit,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  addressDataFromBech32,
  Bech32DeserializationError,
  hashHexWithBlake2b,
  HashingError,
  LucidError,
  makeReturn,
  MidgardValidators,
  ProofSchema,
} from "@/common.js";
import { POSIXTimeSchema } from "@/common.js";
import {
  fetchHubOracleUTxOProgram,
  HubOracleError,
  makeHubOracleDatum,
} from "@/hub-oracle.js";
import { authenticateUTxOs, AuthenticUTxO } from "@/internals.js";
import { DepositEventSchema } from "@/ledger-state.js";

import {
  buildCompletedUserEventMintTxProgram,
  buildUserEventWitnessCertificateValidator,
  encodeUserEventWitnessMintOrBurnRedeemer,
  fetchUserEventUTxOsProgram,
  outputReferenceToPlutusDataCbor,
  resolveEventInclusionTime,
  resolveUserEventValidTo,
  selectWalletNonceInputProgram,
  UserEventBuildError,
  userEventCborFieldsFromInlineDatum,
  UserEventExtraFields,
  UserEventFetchConfig,
  userEventWitnessScriptHash,
} from "./internals.js";

export const DepositDatumSchema = Data.Object({
  event: DepositEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
});
export type DepositDatum = Data.Static<typeof DepositDatumSchema>;
export const DepositDatum = DepositDatumSchema as unknown as DepositDatum;
export const DepositSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  mint_redeemer_index: Data.Integer(),
  membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
});
export type DepositSpendRedeemer = Data.Static<
  typeof DepositSpendRedeemerSchema
>;
export const DepositSpendRedeemer =
  DepositSpendRedeemerSchema as unknown as DepositSpendRedeemer;

export type DepositUTxO = AuthenticUTxO<DepositDatum, UserEventExtraFields>;

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToDepositUTxOs = (
  utxos: UTxO[],
  nftPolicy: PolicyId,
): Effect.Effect<DepositUTxO[]> =>
  authenticateUTxOs<DepositDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    DepositDatum,
    (datum, utxo) => ({
      ...userEventCborFieldsFromInlineDatum(utxo),
      inclusionTime: new Date(Number(datum.inclusion_time)),
    }),
  );

export const fetchDepositUTxOsProgram = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
): Effect.Effect<DepositUTxO[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToDepositUTxOs(utxos, config.eventPolicyId),
  );

export const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
) => makeReturn(fetchDepositUTxOsProgram(lucid, config));

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

export type DepositBuildMetadata = {
  readonly depositAddress: string;
  readonly depositEventId: string;
  readonly depositAssetName: string;
  readonly depositAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

const fetchDepositHubOracleReferenceProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  network: NonNullable<ReturnType<LucidEvolution["config"]>["network"]>,
): Effect.Effect<
  UTxO,
  HubOracleError | LucidError | Bech32DeserializationError | UserEventBuildError
> =>
  Effect.gen(function* () {
    const actual = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      hubOraclePolicyId: contracts.hubOracle.policyId,
    });
    const expectedDatum = yield* makeHubOracleDatum(contracts);

    if (
      actual.datum.deposit !== expectedDatum.deposit ||
      JSON.stringify(actual.datum.deposit_addr) !==
        JSON.stringify(expectedDatum.deposit_addr)
    ) {
      return yield* Effect.fail(
        new UserEventBuildError({
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

export const buildUnsignedDepositTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: DepositBuildMetadata;
  },
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
          message:
            "Cardano network not found while preparing deposit transaction",
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const hubOracleRefInput = yield* fetchDepositHubOracleReferenceProgram(
      lucid,
      contracts,
      network,
    );

    const nonceInput = yield* selectWalletNonceInputProgram(lucid, "deposit");
    const depositEventId = outputReferenceToPlutusDataCbor(nonceInput);
    const nonceAssetName = yield* hashHexWithBlake2b(depositEventId, 32);
    const depositUnit = toUnit(contracts.deposit.policyId, nonceAssetName);
    if ((config.additionalAssets[depositUnit] ?? 0n) !== 0n) {
      return yield* Effect.fail(
        new UserEventBuildError({
          message:
            "Additional asset list must not include the deposit authentication NFT unit",
          cause: depositUnit,
        }),
      );
    }

    const witnessScript =
      buildUserEventWitnessCertificateValidator(nonceAssetName);
    const witnessScriptHash = userEventWitnessScriptHash(nonceAssetName);
    const validTo = resolveUserEventValidTo(lucid);
    const inclusionTime = resolveEventInclusionTime(validTo, network);
    const l2AddressData = yield* addressDataFromBech32(config.l2Address);
    const l2DatumData =
      config.l2Datum === null ? null : Data.from(config.l2Datum);

    const depositDatum: DepositDatum = {
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
    };
    const depositDatumCBOR = Data.to(depositDatum, DepositDatum);
    const outputAssets: Assets = {
      ...config.additionalAssets,
      lovelace: config.lovelace,
      [depositUnit]: 1n,
    };
    const referenceInputs =
      config.referenceScripts === undefined
        ? [hubOracleRefInput]
        : [hubOracleRefInput, config.referenceScripts.depositMinting];
    const witnessRegistrationRedeemer =
      encodeUserEventWitnessMintOrBurnRedeemer(contracts.deposit.policyId);

    const tx = yield* buildCompletedUserEventMintTxProgram({
      lucid,
      network,
      nonceInput,
      eventUnit: depositUnit,
      eventAddress: contracts.deposit.spendingScriptAddress,
      eventDatumCbor: depositDatumCBOR,
      outputAssets,
      validTo,
      mintingPolicy: contracts.deposit.mintingScript,
      attachMintingPolicy: config.referenceScripts === undefined,
      referenceInputs,
      hubOracleRefInput,
      witnessScript,
      witnessRegistrationRedeemer,
      label: "deposit",
    });

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

export const unsignedDepositTxProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  TxSignBuilder,
  | HubOracleError
  | LucidError
  | Bech32DeserializationError
  | HashingError
  | UserEventBuildError
> =>
  buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

export const buildUnsignedDepositTxProgram = unsignedDepositTxProgram;

/**
 * Builds a completed tx for submitting deposits using the provided
 * `LucidEvolution` instance and a deposit config.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param contracts - Midgard validator configuration.
 * @param config - Parameters required for committing deposits.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedDepositTx = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitDepositConfig,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedDepositTxProgram(lucid, contracts, config)).unsafeRun();
