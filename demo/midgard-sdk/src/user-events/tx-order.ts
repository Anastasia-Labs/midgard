import {
  type Assets,
  Data,
  LucidEvolution,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  AddressSchema,
  Bech32DeserializationError,
  HashingError,
  LucidError,
  makeReturn,
  MidgardValidators,
  OutputReference,
  outputReferenceFromUTxO,
  POSIXTimeSchema,
} from "@/common.js";
import { HubOracleError } from "@/hub-oracle.js";
import { authenticateUTxOs, AuthenticUTxO } from "@/internals.js";
import {
  CardanoDatum,
  CardanoDatumSchema,
  MidgardTxCompact,
  MidgardTxValiditySchema,
  TxOrderEventSchema,
} from "@/ledger-state.js";
import { RawRootMembershipProofSchema } from "@/transition-trace.js";

import {
  buildCompletedUserEventMintTxProgram,
  encodeUserEventWitnessMintOrBurnRedeemer,
  fetchUserEventUTxOsProgram,
  outputReferenceToPlutusDataCbor,
  prepareUserEventMintContext,
  UserEventBuildError,
  userEventCborFieldsFromInlineDatum,
  UserEventExtraFields,
  UserEventFetchConfig,
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
  membership_proof: RawRootMembershipProofSchema,
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
    (datum, utxo) => ({
      ...userEventCborFieldsFromInlineDatum(utxo),
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
  readonly tx: MidgardTxCompact;
  readonly refundAddress: AddressData;
  readonly refundDatum?: CardanoDatum;
  readonly lovelace?: bigint;
  readonly referenceScripts?: SubmitTxOrderReferenceScripts;
};

export type TxOrderBuildMetadata = {
  readonly txOrderAddress: string;
  readonly txOrderId: OutputReference;
  readonly authNonceCbor: string;
  readonly txOrderAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

const DEFAULT_TX_ORDER_LOVELACE = 3_000_000n;

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
    const context = yield* prepareUserEventMintContext({
      lucid,
      contracts,
      label: "tx order",
      eventPolicyId: contracts.txOrder.policyId,
      hubOraclePolicyField: "tx_order",
      hubOracleAddressField: "tx_order_addr",
    });
    const {
      eventUnit: txOrderUnit,
      hubOracleRefInput,
      inclusionTime,
      network,
      nonceInput,
      validTo,
      witnessScript,
      witnessScriptHash,
    } = context;
    const txOrderId = outputReferenceFromUTxO(nonceInput);
    const authNonceCbor = outputReferenceToPlutusDataCbor(nonceInput);

    const txOrderDatum: TxOrderDatum = {
      event: {
        id: txOrderId,
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
    const witnessRegistrationRedeemer =
      encodeUserEventWitnessMintOrBurnRedeemer(contracts.txOrder.policyId);

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
        txOrderId,
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
