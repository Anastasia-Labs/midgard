import {
  type Assets,
  credentialToAddress,
  Data,
  fromHex,
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
  H32,
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
  MidgardTxCompact,
  MidgardTxValiditySchema,
  TxOrderEventSchema,
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
  UserEventExtraFields,
  UserEventFetchConfig,
  userEventWitnessScriptHash,
} from "./internals.js";

export const TxOrderDatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
  refund_address: AddressSchema,
  refund_datum: CardanoDatumSchema,
});
export type TxOrderDatum = Data.Static<typeof TxOrderDatumSchema>;
export const TxOrderDatum = TxOrderDatumSchema as unknown as TxOrderDatum;
export const TxOrderSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  burn_redeemer_index: Data.Integer(),
  membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
  validity_override: MidgardTxValiditySchema,
});
export type TxOrderSpendRedeemer = Data.Static<
  typeof TxOrderSpendRedeemerSchema
>;
export const TxOrderSpendRedeemer =
  TxOrderSpendRedeemerSchema as unknown as TxOrderSpendRedeemer;

export type TxOrderUTxO = AuthenticUTxO<TxOrderDatum, UserEventExtraFields>;

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToTxOrderUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<TxOrderUTxO[]> =>
  authenticateUTxOs<TxOrderDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    TxOrderDatum,
    (datum) => ({
      idCbor: Buffer.from(fromHex(Data.to(datum.event.id, H32))),
      infoCbor: Buffer.from(fromHex(Data.to(datum.event.tx, MidgardTxCompact))),
      inclusionTime: new Date(Number(datum.inclusion_time)),
    }),
  );

export const fetchTxOrderUTxOsProgram = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
): Effect.Effect<TxOrderUTxO[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToTxOrderUTxOs(utxos, config.eventPolicyId),
  );

export const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
) => makeReturn(fetchTxOrderUTxOsProgram(lucid, config));

export type SubmitTxOrderReferenceScripts = {
  readonly txOrderMinting: UTxO;
};

export type SubmitTxOrderConfig = {
  readonly txId: H32;
  readonly tx: MidgardTxCompact;
  readonly refundAddress: AddressData;
  readonly refundDatum?: CardanoDatum;
  readonly lovelace?: bigint;
  readonly referenceScripts?: SubmitTxOrderReferenceScripts;
};

export type TxOrderBuildMetadata = {
  readonly txOrderAddress: string;
  readonly txOrderId: H32;
  readonly authNonceCbor: string;
  readonly txOrderAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

const DEFAULT_TX_ORDER_LOVELACE = 3_000_000n;

const fetchTxOrderHubOracleReferenceProgram = (
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
      actual.datum.tx_order !== expectedDatum.tx_order ||
      JSON.stringify(actual.datum.tx_order_addr) !==
        JSON.stringify(expectedDatum.tx_order_addr)
    ) {
      return yield* Effect.fail(
        new UserEventBuildError({
          message:
            "On-chain hub oracle deployment does not match the locally configured tx-order contract",
          cause: {
            expectedPolicyId: expectedDatum.tx_order,
            actualPolicyId: actual.datum.tx_order,
            expectedAddress: expectedDatum.tx_order_addr,
            actualAddress: actual.datum.tx_order_addr,
          },
        }),
      );
    }

    return actual.utxo;
  });

export const buildUnsignedTxOrderTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitTxOrderConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: TxOrderBuildMetadata;
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
            "Cardano network not found while preparing tx-order transaction",
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const hubOracleRefInput = yield* fetchTxOrderHubOracleReferenceProgram(
      lucid,
      contracts,
      network,
    );

    const nonceInput = yield* selectWalletNonceInputProgram(lucid, "tx order");
    const authNonceCbor = outputReferenceToPlutusDataCbor(nonceInput);
    const nonceAssetName = yield* hashHexWithBlake2b(authNonceCbor, 32);
    const txOrderUnit = toUnit(contracts.txOrder.policyId, nonceAssetName);

    const witnessScript =
      buildUserEventWitnessCertificateValidator(nonceAssetName);
    const witnessScriptHash = userEventWitnessScriptHash(nonceAssetName);
    const validTo = resolveUserEventValidTo(lucid);
    const inclusionTime = resolveEventInclusionTime(validTo, network);

    const txOrderDatum: TxOrderDatum = {
      event: {
        id: config.txId,
        tx: config.tx,
      },
      inclusion_time: BigInt(inclusionTime),
      witness: witnessScriptHash,
      refund_address: config.refundAddress,
      refund_datum: config.refundDatum ?? "NoDatum",
    };
    const txOrderDatumCBOR = Data.to(txOrderDatum, TxOrderDatum);
    const outputAssets: Assets = {
      lovelace: config.lovelace ?? DEFAULT_TX_ORDER_LOVELACE,
      [txOrderUnit]: 1n,
    };
    const referenceInputs =
      config.referenceScripts === undefined
        ? [hubOracleRefInput]
        : [hubOracleRefInput, config.referenceScripts.txOrderMinting];
    const witnessRegistrationRedeemer = encodeUserEventWitnessMintOrBurnRedeemer(
      contracts.txOrder.policyId,
    );

    const tx = yield* buildCompletedUserEventMintTxProgram({
      lucid,
      network,
      nonceInput,
      eventUnit: txOrderUnit,
      eventAddress: contracts.txOrder.spendingScriptAddress,
      eventDatumCbor: txOrderDatumCBOR,
      outputAssets,
      validTo,
      mintingPolicy: contracts.txOrder.mintingScript,
      attachMintingPolicy: config.referenceScripts === undefined,
      referenceInputs,
      hubOracleRefInput,
      witnessScript,
      witnessRegistrationRedeemer,
      label: "tx order",
    });

    return {
      tx,
      metadata: {
        txOrderAddress: contracts.txOrder.spendingScriptAddress,
        txOrderId: config.txId,
        authNonceCbor,
        txOrderAuthUnit: txOrderUnit,
        nonceInput,
        validTo,
        inclusionTime,
      },
    };
  }).pipe(
    Effect.catchAllDefect((defect) => {
      return Effect.fail(
        new LucidError({
          message: "Caught defect from txOrderTxBuilder",
          cause: defect,
        }),
      );
    }),
  );

export const unsignedTxOrderTxProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitTxOrderConfig,
): Effect.Effect<
  TxSignBuilder,
  | HubOracleError
  | LucidError
  | Bech32DeserializationError
  | HashingError
  | UserEventBuildError
> =>
  buildUnsignedTxOrderTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

export const buildUnsignedTxOrderTxProgram = unsignedTxOrderTxProgram;

/**
 * Builds completed tx for submitting tx order using the provided
 * `LucidEvolution` instance and a tx order config.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param contracts - Midgard validator configuration.
 * @param txOrderParams - Parameters required for commiting tx orders.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedTxOrderTx = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  txOrderParams: SubmitTxOrderConfig,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedTxOrderTxProgram(lucid, contracts, txOrderParams),
  ).unsafeRun();
