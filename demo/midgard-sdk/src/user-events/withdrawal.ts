import {
  type Assets,
  credentialToAddress,
  Data,
  LucidEvolution,
  scriptHashToCredential,
  toUnit,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  AddressSchema,
  Bech32DeserializationError,
  hashHexWithBlake2b,
  HashingError,
  LucidError,
  makeReturn,
  MidgardValidators,
  POSIXTimeSchema,
  ProofSchema,
} from "@/common.js";
import {
  fetchHubOracleUTxOProgram,
  HubOracleError,
  makeHubOracleDatum,
} from "@/hub-oracle.js";
import { authenticateUTxOs, AuthenticUTxO } from "@/internals.js";
import {
  CardanoDatum,
  CardanoDatumSchema,
  WithdrawalBody,
  WithdrawalEventSchema,
  WithdrawalSignature,
  WithdrawalValiditySchema,
} from "@/ledger-state.js";

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

export const WithdrawalOrderDatumSchema = Data.Object({
  event: WithdrawalEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
  refund_address: AddressSchema,
  refund_datum: CardanoDatumSchema,
});
export type WithdrawalOrderDatum = Data.Static<
  typeof WithdrawalOrderDatumSchema
>;
export const WithdrawalOrderDatum =
  WithdrawalOrderDatumSchema as unknown as WithdrawalOrderDatum;
export const WithdrawalSpendPurposeSchema = Data.Enum([
  Data.Literal("InitializePayout"),
  Data.Object({
    Refund: Data.Object({
      validity_override: WithdrawalValiditySchema,
    }),
  }),
]);
export type WithdrawalSpendPurpose = Data.Static<
  typeof WithdrawalSpendPurposeSchema
>;
export const WithdrawalSpendPurpose =
  WithdrawalSpendPurposeSchema as unknown as WithdrawalSpendPurpose;
export const WithdrawalSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  burn_redeemer_index: Data.Integer(),
  payout_mint_redeemer_index: Data.Integer(),
  membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
  purpose: WithdrawalSpendPurposeSchema,
});
export type WithdrawalSpendRedeemer = Data.Static<
  typeof WithdrawalSpendRedeemerSchema
>;
export const WithdrawalSpendRedeemer =
  WithdrawalSpendRedeemerSchema as unknown as WithdrawalSpendRedeemer;

export type WithdrawalUTxO = AuthenticUTxO<
  WithdrawalOrderDatum,
  UserEventExtraFields
>;

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToWithdrawalUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<WithdrawalUTxO[]> =>
  authenticateUTxOs<WithdrawalOrderDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    WithdrawalOrderDatum,
    (datum, utxo) => ({
      ...userEventCborFieldsFromInlineDatum(utxo),
      inclusionTime: new Date(Number(datum.inclusion_time)),
    }),
  );

export const fetchWithdrawalUTxOsProgram = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
): Effect.Effect<WithdrawalUTxO[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToWithdrawalUTxOs(utxos, config.eventPolicyId),
  );

export const fetchWithdrawalUTxOs = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
) => makeReturn(fetchWithdrawalUTxOsProgram(lucid, config));

export type SubmitWithdrawalReferenceScripts = {
  readonly withdrawalMinting: UTxO;
};

export type SubmitWithdrawalConfig = {
  readonly body: WithdrawalBody;
  readonly signature: WithdrawalSignature;
  readonly refundAddress: AddressData;
  readonly refundDatum?: CardanoDatum;
  readonly lovelace?: bigint;
  readonly referenceScripts?: SubmitWithdrawalReferenceScripts;
};

export type WithdrawalBuildMetadata = {
  readonly withdrawalAddress: string;
  readonly withdrawalEventIdCbor: string;
  readonly withdrawalAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

const DEFAULT_WITHDRAWAL_ORDER_LOVELACE = 3_000_000n;

const fetchWithdrawalHubOracleReferenceProgram = (
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
      actual.datum.withdrawal !== expectedDatum.withdrawal ||
      JSON.stringify(actual.datum.withdrawal_addr) !==
        JSON.stringify(expectedDatum.withdrawal_addr)
    ) {
      return yield* Effect.fail(
        new UserEventBuildError({
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

export const buildUnsignedWithdrawalTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitWithdrawalConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: WithdrawalBuildMetadata;
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
            "Cardano network not found while preparing withdrawal transaction",
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const hubOracleRefInput = yield* fetchWithdrawalHubOracleReferenceProgram(
      lucid,
      contracts,
      network,
    );

    const nonceInput = yield* selectWalletNonceInputProgram(
      lucid,
      "withdrawal",
    );
    const withdrawalEventIdCbor = outputReferenceToPlutusDataCbor(nonceInput);
    const nonceAssetName = yield* hashHexWithBlake2b(withdrawalEventIdCbor, 32);
    const withdrawalUnit = toUnit(
      contracts.withdrawal.policyId,
      nonceAssetName,
    );

    const witnessScript =
      buildUserEventWitnessCertificateValidator(nonceAssetName);
    const witnessScriptHash = userEventWitnessScriptHash(nonceAssetName);
    const validTo = resolveUserEventValidTo(lucid);
    const inclusionTime = resolveEventInclusionTime(validTo, network);

    const withdrawalOrderDatum: WithdrawalOrderDatum = {
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
    const withdrawalOrderDatumCBOR = Data.to(
      withdrawalOrderDatum,
      WithdrawalOrderDatum,
    );
    const outputAssets: Assets = {
      lovelace: config.lovelace ?? DEFAULT_WITHDRAWAL_ORDER_LOVELACE,
      [withdrawalUnit]: 1n,
    };
    const referenceInputs =
      config.referenceScripts === undefined
        ? [hubOracleRefInput]
        : [hubOracleRefInput, config.referenceScripts.withdrawalMinting];
    const witnessRegistrationRedeemer =
      encodeUserEventWitnessMintOrBurnRedeemer(contracts.withdrawal.policyId);

    const tx = yield* buildCompletedUserEventMintTxProgram({
      lucid,
      network,
      nonceInput,
      eventUnit: withdrawalUnit,
      eventAddress: contracts.withdrawal.spendingScriptAddress,
      eventDatumCbor: withdrawalOrderDatumCBOR,
      outputAssets,
      validTo,
      mintingPolicy: contracts.withdrawal.mintingScript,
      attachMintingPolicy: config.referenceScripts === undefined,
      referenceInputs,
      hubOracleRefInput,
      witnessScript,
      witnessRegistrationRedeemer,
      label: "withdrawal",
    });

    return {
      tx,
      metadata: {
        withdrawalAddress: contracts.withdrawal.spendingScriptAddress,
        withdrawalEventIdCbor,
        withdrawalAuthUnit: withdrawalUnit,
        nonceInput,
        validTo,
        inclusionTime,
      },
    };
  });

export const unsignedWithdrawalTxProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitWithdrawalConfig,
): Effect.Effect<
  TxSignBuilder,
  | HubOracleError
  | LucidError
  | Bech32DeserializationError
  | HashingError
  | UserEventBuildError
> =>
  buildUnsignedWithdrawalTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

export const buildUnsignedWithdrawalTxProgram = unsignedWithdrawalTxProgram;

/**
 * Builds completed tx for submitting withdrawal order using the provided
 * `LucidEvolution` instance and a withdrawal order config.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param contracts - Midgard validator configuration.
 * @param config - Parameters required for committing withdrawal orders.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedWithdrawalTx = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitWithdrawalConfig,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedWithdrawalTxProgram(lucid, contracts, config)).unsafeRun();
