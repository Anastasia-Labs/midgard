import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData, toUnit, type Assets } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  addressDataToBech32,
  formatJson,
  parseEventId,
  parseHexBytes,
} from "@/commands/withdrawal-utils.js";
import { valueToAssets } from "@/transactions/reserve-payout.js";

export type WithdrawalStatusLookup = {
  readonly eventId?: Buffer;
  readonly l1TxHash?: Buffer;
};

export type WithdrawalStatusResult = {
  readonly eventId: string;
  readonly withdrawalL1OutRef: string;
  readonly inclusionTime: string;
  readonly l2OutRef: string;
  readonly l2Owner: string;
  readonly l2Value: Readonly<Assets>;
  readonly l1Address: string;
  readonly l1Datum: string;
  readonly refundAddress: string;
  readonly refundDatum: string;
  readonly status: WithdrawalsDB.Status;
  readonly projectedHeaderHash: string | null;
  readonly settlementOutRef: string | null;
  readonly settlementUtxoCount: number;
  readonly validity: WithdrawalsDB.Validity | null;
  readonly validityDetail: unknown;
  readonly payoutUnit: string;
  readonly payoutUtxoCount: number;
};

export const parseWithdrawalStatusLookup = ({
  eventId,
  l1TxHash,
}: {
  readonly eventId?: string;
  readonly l1TxHash?: string;
}): WithdrawalStatusLookup => {
  if (eventId === undefined && l1TxHash === undefined) {
    throw new Error("withdrawal-status requires --event-id or --l1-tx-hash.");
  }
  return {
    ...(eventId === undefined
      ? {}
      : { eventId: parseEventId(eventId, "--event-id") }),
    ...(l1TxHash === undefined
      ? {}
      : { l1TxHash: parseHexBytes(l1TxHash, "--l1-tx-hash", 32) }),
  };
};

const requireSingle = (
  rows: readonly WithdrawalsDB.Entry[],
  label: string,
): Effect.Effect<WithdrawalsDB.Entry, Error> => {
  if (rows.length !== 1) {
    return Effect.fail(
      new Error(
        `Expected exactly one withdrawal for ${label}, found ${rows.length.toString()}.`,
      ),
    );
  }
  return Effect.succeed(rows[0]!);
};

const outRef = (utxo: {
  readonly txHash: string;
  readonly outputIndex: number;
}): string => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const withdrawalStatusProgram = (
  lookup: WithdrawalStatusLookup,
): Effect.Effect<
  WithdrawalStatusResult,
  DatabaseError | Error | SDK.LucidError,
  Database | NodeConfig | MidgardContracts | Lucid
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const contracts = yield* MidgardContracts;
    const { api: lucid } = yield* Lucid;
    let entry: WithdrawalsDB.Entry;
    if (lookup.eventId !== undefined) {
      const maybe = yield* WithdrawalsDB.retrieveByEventId(lookup.eventId);
      if (Option.isNone(maybe)) {
        return yield* Effect.fail(
          new Error(
            `Withdrawal not found for event id ${lookup.eventId.toString("hex")}.`,
          ),
        );
      }
      entry = maybe.value;
    } else {
      const l1TxHash = lookup.l1TxHash!;
      entry = yield* requireSingle(
        yield* WithdrawalsDB.retrieveByCardanoTxHash(l1TxHash),
        `L1 tx hash ${l1TxHash.toString("hex")}`,
      );
    }

    const l2OutRef = LucidData.from(
      entry[WithdrawalsDB.Columns.L2_OUTREF].toString("hex"),
      SDK.OutputReference,
    ) as SDK.OutputReference;
    const l2Value = LucidData.from(
      entry[WithdrawalsDB.Columns.L2_VALUE].toString("hex"),
      SDK.Value,
    ) as SDK.Value;
    const l1Address = LucidData.from(
      entry[WithdrawalsDB.Columns.L1_ADDRESS].toString("hex"),
      SDK.AddressData,
    ) as SDK.AddressData;
    const refundAddress = LucidData.from(
      entry[WithdrawalsDB.Columns.REFUND_ADDRESS].toString("hex"),
      SDK.AddressData,
    ) as SDK.AddressData;
    const payoutUnit = toUnit(
      contracts.payout.policyId,
      entry[WithdrawalsDB.Columns.ASSET_NAME].toString("hex"),
    );
    const projectedHeaderHash =
      entry[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH] === null
        ? null
        : entry[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]!.toString("hex");
    const settlementUtxos =
      projectedHeaderHash === null
        ? []
        : yield* Effect.tryPromise({
            try: () =>
              lucid.utxosAtWithUnit(
                contracts.settlement.spendingScriptAddress,
                toUnit(contracts.settlement.policyId, projectedHeaderHash),
              ),
            catch: (cause) =>
              new SDK.LucidError({
                message:
                  "Failed to fetch settlement UTxOs for withdrawal status",
                cause,
              }),
          });
    const payoutUtxos = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.payout.spendingScriptAddress,
          payoutUnit,
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch payout UTxOs for withdrawal status",
          cause,
        }),
    });

    return {
      eventId: entry[WithdrawalsDB.Columns.ID].toString("hex"),
      withdrawalL1OutRef: `${entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH].toString("hex")}#${entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX].toString()}`,
      inclusionTime: entry[WithdrawalsDB.Columns.INCLUSION_TIME].toISOString(),
      l2OutRef: `${l2OutRef.transactionId}#${l2OutRef.outputIndex.toString()}`,
      l2Owner: entry[WithdrawalsDB.Columns.L2_OWNER].toString("hex"),
      l2Value: valueToAssets(l2Value),
      l1Address: addressDataToBech32(nodeConfig.NETWORK, l1Address),
      l1Datum: entry[WithdrawalsDB.Columns.L1_DATUM].toString("hex"),
      refundAddress: addressDataToBech32(nodeConfig.NETWORK, refundAddress),
      refundDatum: entry[WithdrawalsDB.Columns.REFUND_DATUM].toString("hex"),
      status: entry[WithdrawalsDB.Columns.STATUS],
      projectedHeaderHash,
      settlementOutRef:
        settlementUtxos.length === 1 ? outRef(settlementUtxos[0]!) : null,
      settlementUtxoCount: settlementUtxos.length,
      validity: entry[WithdrawalsDB.Columns.VALIDITY],
      validityDetail: entry[WithdrawalsDB.Columns.VALIDITY_DETAIL],
      payoutUnit,
      payoutUtxoCount: payoutUtxos.length,
    };
  });

export const formatWithdrawalStatusResult = (
  result: WithdrawalStatusResult,
): string => formatJson(result);
