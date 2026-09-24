import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type Assets,
  Data,
  LucidEvolution,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  addressDataFromBech32,
  Bech32DeserializationError,
  HashingError,
  LucidError,
  makeReturn,
  MidgardValidators,
} from "../common.js";
import { POSIXTimeSchema } from "../common.js";
import { HubOracleError } from "../hub-oracle.js";
import { DepositEventSchema } from "../ledger-state.js";
import { RawRootMembershipProofSchema } from "../transition-trace.js";
import { EventHistoryPayload } from "./history.js";
import { buildEventHistoryAdmission } from "./history-build.js";
import {
  type DepositUTxO,
  type EventHistoryFetchConfig,
  fetchHistoryEventsProgram,
  historyEventFromPresence,
  historyEventReadError,
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
export type { DepositUTxO } from "./history-events.js";

export const DepositDatumSchema = Data.Object({
  event: DepositEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
});
export type DepositDatum = Data.Static<typeof DepositDatumSchema>;
export const DepositDatum = asDataType<DepositDatum>(DepositDatumSchema);
export const DepositSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  mint_redeemer_index: Data.Integer(),
  membership_proof: RawRootMembershipProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
});
export type DepositSpendRedeemer = Data.Static<
  typeof DepositSpendRedeemerSchema
>;
export const DepositSpendRedeemer = asDataType<DepositSpendRedeemer>(
  DepositSpendRedeemerSchema,
);

const midgardNativeNetworkId = (
  network: NonNullable<ReturnType<LucidEvolution["config"]>["network"]>,
): bigint => (network === "Mainnet" ? 1n : 0n);

const toDepositUTxO = (
  history: EventHistoryPresence,
  deployment: EventHistoryDeployment,
): DepositUTxO => {
  const event = historyEventFromPresence(history, deployment);
  if (event.kind !== "Deposit")
    throw new Error(
      "Authenticated deposit history contains a different event kind",
    );
  return event;
};

/** Reads a complete authenticated list and actual retained data; rejects partial
 * or malformed authenticated state instead of silently dropping events. */
export const utxosToDepositUTxOs = (
  utxos: readonly UTxO[],
  retainedUtxos: readonly UTxO[],
  deployment: EventHistoryDeployment,
): Effect.Effect<DepositUTxO[], LucidError> =>
  Effect.try({
    try: () =>
      readEventHistoryOrders(utxos, retainedUtxos, deployment).map((history) =>
        toDepositUTxO(history, deployment),
      ),
    catch: historyEventReadError,
  });

export const fetchDepositUTxOsProgram = (
  lucid: { utxosAt(address: string): Promise<UTxO[]> },
  config: EventHistoryFetchConfig,
): Effect.Effect<DepositUTxO[], LucidError> =>
  fetchHistoryEventsProgram(lucid, config, toDepositUTxO);

export const fetchDepositUTxOs = (
  lucid: { utxosAt(address: string): Promise<UTxO[]> },
  config: EventHistoryFetchConfig,
) => makeReturn(fetchDepositUTxOsProgram(lucid, config));

export type SubmitDepositReferenceScripts = {
  readonly depositMinting: UTxO;
};

export type SubmitDepositConfig = UserHistoryBuildOptions & {
  /** Additional ADA kept separate from the Value projected to L2. Omit to quote it automatically. */
  readonly structuralLovelace?: bigint;
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
  readonly structuralLovelace: bigint;
  readonly orderOutputIndex: number;
};

/** Prepare the stable nonce, complete payload and funding before any signature.
 * For External plans, publish with buildEventHistoryPublication and confirm its
 * exact output before unsigned admission, or pass this request to submitEventHistory. */
export const prepareDepositSubmissionProgram = (
  lucid: LucidEvolution,
  contracts: UserHistoryContracts,
  config: SubmitDepositConfig,
) =>
  Effect.gen(function* () {
    const prepared = yield* prepareUserHistoryContextProgram(
      lucid,
      contracts,
      "Deposit",
      config,
      config.referenceScripts?.depositMinting,
    );
    const l2AddressData = yield* addressDataFromBech32(config.l2Address);
    const payloadCbor = yield* Effect.try({
      try: () => {
        const payload: EventHistoryPayload = {
          DepositPayload: {
            event: {
              id: {
                transactionId: prepared.nonce.txHash,
                outputIndex: BigInt(prepared.nonce.outputIndex),
              },
              info: {
                l2_address: l2AddressData,
                l2_network_id: midgardNativeNetworkId(prepared.network),
                l2_datum: config.l2Datum === null ? null : 0n,
              },
            },
          },
        };
        const encoded = Data.to(payload, EventHistoryPayload);
        return config.l2Datum === null
          ? encoded
          : replacePlutusConstrFieldCbor(encoded, [0, 1, 2, 0], config.l2Datum);
      },
      catch: historyUserBuildError,
    });
    const funding = yield* Effect.try({
      try: () => {
        if (
          Object.keys(config.additionalAssets).some(
            (unit) =>
              unit === "lovelace" ||
              unit.startsWith(prepared.context.applied.policyId),
          )
        )
          throw new Error(
            "Additional deposit assets cannot contain lovelace or history authentication tokens",
          );
        return quoteUserHistoryFunding({
          context: prepared.context,
          payloadCbor,
          reclaimAuth: prepared.reclaimAuth,
          structuralRefundKey: prepared.structuralRefundKey,
          originalAssets: {
            ...config.additionalAssets,
            lovelace: config.lovelace,
          },
          structuralLovelace: config.structuralLovelace,
        });
      },
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

export const buildUnsignedDepositTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: UserHistoryContracts,
  config: SubmitDepositConfig,
) =>
  Effect.gen(function* () {
    const prepared = yield* prepareDepositSubmissionProgram(
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
    const metadata: DepositBuildMetadata = {
      depositAddress: prepared.context.applied.address,
      depositEventId: outputReferenceToPlutusDataCbor(prepared.request.nonce),
      depositAssetName: built.plan.key,
      depositAuthUnit: prepared.context.applied.policyId + built.plan.key,
      nonceInput: {
        txHash: prepared.request.nonce.txHash,
        outputIndex: prepared.request.nonce.outputIndex,
      },
      validTo: validity.validTo,
      inclusionTime: Number(built.node.payload.Order.facts.inclusion_time),
      structuralLovelace: prepared.request.structuralLovelace,
      orderOutputIndex: built.orderOutputIndex,
    };
    return { tx: built.tx, metadata };
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
