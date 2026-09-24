import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
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
  POSIXTimeSchema,
} from "../common.js";
import { HubOracleError } from "../hub-oracle.js";
import {
  CardanoDatum,
  CardanoDatumSchema,
  WithdrawalBody,
  WithdrawalEventSchema,
  WithdrawalSignature,
  WithdrawalValiditySchema,
} from "../ledger-state.js";
import { RawRootMembershipProofSchema } from "../transition-trace.js";
import { EventHistoryPayload } from "./history.js";
import { buildEventHistoryAdmission } from "./history-build.js";
import {
  type EventHistoryFetchConfig,
  fetchHistoryEventsProgram,
  historyEventFromPresence,
  historyEventReadError,
  type WithdrawalUTxO,
} from "./history-events.js";
import {
  type EventHistoryDeployment,
  type EventHistoryPresence,
  readEventHistoryOrders,
} from "./history-query.js";
import {
  historyUserBuildError,
  prepareUserHistoryContextProgram,
  quoteUserHistoryFunding,
  type UserHistoryBuildOptions,
  type UserHistoryContracts,
  userHistoryValidity,
} from "./history-user.js";
import {
  outputReferenceToPlutusDataCbor,
  UserEventBuildError,
} from "./internals.js";
export type { WithdrawalUTxO } from "./history-events.js";

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
export const WithdrawalOrderDatum = asDataType<WithdrawalOrderDatum>(
  WithdrawalOrderDatumSchema,
);
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
export const WithdrawalSpendPurpose = asDataType<WithdrawalSpendPurpose>(
  WithdrawalSpendPurposeSchema,
);
export const WithdrawalSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  burn_redeemer_index: Data.Integer(),
  payout_mint_redeemer_index: Data.Integer(),
  membership_proof: RawRootMembershipProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
  purpose: WithdrawalSpendPurposeSchema,
});
export type WithdrawalSpendRedeemer = Data.Static<
  typeof WithdrawalSpendRedeemerSchema
>;
export const WithdrawalSpendRedeemer = asDataType<WithdrawalSpendRedeemer>(
  WithdrawalSpendRedeemerSchema,
);

const toWithdrawalUTxO = (
  history: EventHistoryPresence,
  deployment: EventHistoryDeployment,
): WithdrawalUTxO => {
  const event = historyEventFromPresence(history, deployment);
  if (event.kind !== "Withdrawal")
    throw new Error(
      "Authenticated withdrawal history contains a different event kind",
    );
  return event;
};

/** Reads a complete authenticated list and actual retained data; rejects partial
 * or malformed authenticated state instead of silently dropping events. */
export const utxosToWithdrawalUTxOs = (
  utxos: readonly UTxO[],
  retainedUtxos: readonly UTxO[],
  deployment: EventHistoryDeployment,
): Effect.Effect<WithdrawalUTxO[], LucidError> =>
  Effect.try({
    try: () =>
      readEventHistoryOrders(utxos, retainedUtxos, deployment).map((history) =>
        toWithdrawalUTxO(history, deployment),
      ),
    catch: historyEventReadError,
  });

export const fetchWithdrawalUTxOsProgram = (
  lucid: { utxosAt(address: string): Promise<UTxO[]> },
  config: EventHistoryFetchConfig,
): Effect.Effect<WithdrawalUTxO[], LucidError> =>
  fetchHistoryEventsProgram(lucid, config, toWithdrawalUTxO);

export const fetchWithdrawalUTxOs = (
  lucid: { utxosAt(address: string): Promise<UTxO[]> },
  config: EventHistoryFetchConfig,
) => makeReturn(fetchWithdrawalUTxOsProgram(lucid, config));

export type SubmitWithdrawalReferenceScripts = {
  readonly withdrawalMinting: UTxO;
};

export type WithdrawalBodyInput =
  | { readonly body: WithdrawalBody; readonly bodyCbor?: never }
  | { readonly body?: never; readonly bodyCbor: string };

export type WithdrawalRefundDatumInput =
  | { readonly refundDatum?: CardanoDatum; readonly refundDatumCbor?: never }
  | { readonly refundDatum?: never; readonly refundDatumCbor: string };

export type SubmitWithdrawalConfig = UserHistoryBuildOptions &
  WithdrawalBodyInput &
  WithdrawalRefundDatumInput & {
    readonly signature: WithdrawalSignature;
    readonly refundAddress: AddressData;
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
  readonly lockedLovelace: bigint;
  readonly orderOutputIndex: number;
};

/** Stable withdrawal payload and nonce shared by unsigned and automatic flows. */
export const prepareWithdrawalSubmissionProgram = (
  lucid: LucidEvolution,
  contracts: UserHistoryContracts,
  config: SubmitWithdrawalConfig,
) =>
  Effect.gen(function* () {
    const prepared = yield* prepareUserHistoryContextProgram(
      lucid,
      contracts,
      "Withdrawal",
      config,
      config.referenceScripts?.withdrawalMinting,
    );
    const payloadCbor = yield* Effect.try({
      try: () => {
        if (
          (config.body !== undefined && config.bodyCbor !== undefined) ||
          (config.refundDatum !== undefined &&
            config.refundDatumCbor !== undefined)
        )
          throw new Error(
            "Withdrawal body and refund datum must each have one encoding source",
          );
        const bodyCbor =
          config.bodyCbor ?? Data.to(config.body, WithdrawalBody);
        const refundCbor =
          config.refundDatumCbor ??
          Data.to(config.refundDatum ?? "NoDatum", CardanoDatum);
        const payload: EventHistoryPayload = {
          WithdrawalPayload: {
            event: {
              id: {
                transactionId: prepared.nonce.txHash,
                outputIndex: BigInt(prepared.nonce.outputIndex),
              },
              info: {
                body: Data.from(bodyCbor, WithdrawalBody),
                signature: config.signature,
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: config.refundAddress,
            refund_datum: Data.from(refundCbor, CardanoDatum),
          },
        };
        return replacePlutusConstrFieldCbor(
          replacePlutusConstrFieldCbor(
            Data.to(payload, EventHistoryPayload),
            [0, 1, 0],
            bodyCbor,
          ),
          [2],
          refundCbor,
        );
      },
      catch: historyUserBuildError,
    });
    const funding = yield* Effect.try({
      try: () =>
        quoteUserHistoryFunding({
          context: prepared.context,
          payloadCbor,
          reclaimAuth: prepared.reclaimAuth,
          structuralRefundKey: prepared.structuralRefundKey,
          withdrawalLovelace: config.lovelace,
        }),
      catch: historyUserBuildError,
    });
    return {
      context: prepared.context,
      plan: funding.plan,
      request: {
        payloadCbor,
        reclaimAuth: prepared.reclaimAuth,
        nonce: prepared.nonce,
        assets: funding.assets,
        structuralLovelace: funding.structuralLovelace,
        structuralRefundKey: prepared.structuralRefundKey,
      },
    };
  });

export const buildUnsignedWithdrawalTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: UserHistoryContracts,
  config: SubmitWithdrawalConfig,
) =>
  Effect.gen(function* () {
    const prepared = yield* prepareWithdrawalSubmissionProgram(
      lucid,
      contracts,
      config,
    );
    const validity = userHistoryValidity(lucid, config);
    const built = yield* Effect.tryPromise({
      try: () =>
        buildEventHistoryAdmission(prepared.context, {
          ...prepared.request,
          ...validity,
          externalData: config.externalData,
        }),
      catch: historyUserBuildError,
    });
    if (
      built.node.payload === "RootContent" ||
      !("Order" in built.node.payload)
    )
      return yield* Effect.fail(
        historyUserBuildError("Missing admitted Order facts"),
      );
    const metadata: WithdrawalBuildMetadata = {
      withdrawalAddress: prepared.context.applied.address,
      withdrawalEventIdCbor: outputReferenceToPlutusDataCbor(
        prepared.request.nonce,
      ),
      withdrawalAuthUnit: prepared.context.applied.policyId + built.plan.key,
      nonceInput: {
        txHash: prepared.request.nonce.txHash,
        outputIndex: prepared.request.nonce.outputIndex,
      },
      validTo: validity.validTo,
      inclusionTime: Number(built.node.payload.Order.facts.inclusion_time),
      lockedLovelace: prepared.request.assets.lovelace,
      orderOutputIndex: built.orderOutputIndex,
    };
    return { tx: built.tx, metadata };
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
