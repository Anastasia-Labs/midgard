import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { type Assets, Data, type Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as DepositsDB from "./database/deposits.js";
import * as UserEventsUtils from "./database/utils/user-events.js";
import * as WithdrawalsDB from "./database/withdrawals.js";
import type { HistoryIncarnation } from "./l1-event-history-provenance.js";

// SQL row conversion consumes authenticated bytes and a separate location. It
// does not require an unspent Order, and grants no selection or L2 authority.
type EventEntryData = Readonly<{
  idCbor: Buffer;
  infoCbor: Buffer;
  inclusionTime: Date;
  location: OutRefLike;
}>;
type DepositEntryData = EventEntryData & Readonly<{ originalAssets: Assets }>;
type WithdrawalEntryData = EventEntryData &
  Readonly<{ assetName: string; payloadCbor: string }>;

export const depositDataToEntry = (
  deposit: DepositEntryData,
  network: Network,
): Effect.Effect<DepositsDB.Entry, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const info = Data.from(deposit.infoCbor.toString("hex"), SDK.DepositInfo);
      const id = Data.from(deposit.idCbor.toString("hex"), SDK.OutputReference);
      const l2Datum = info.l2_datum;
      const effect = deriveCanonicalOriginalDepositTransitionEffect({
        configuredNetwork: network,
        eventId: id,
        l2NetworkId: info.l2_network_id,
        l2Address: info.l2_address,
        l2DatumCbor:
          l2Datum === null
            ? null
            : Buffer.from(
                plutusConstrFieldCbor(deposit.infoCbor.toString("hex"), [2, 0]),
                "hex",
              ),
        originalAssets: deposit.originalAssets,
      });
      const operation = effect.operations[0];
      if (operation === undefined || operation.type !== "insert") {
        throw new Error(
          "canonical deposit projection did not produce an insert",
        );
      }
      const output = Buffer.from(operation.outputCbor);
      const l2Address = encodeMidgardAddressText(
        decodeMidgardTxOutput(output).address,
      );

      return {
        [UserEventsUtils.Columns.ID]: deposit.idCbor,
        [UserEventsUtils.Columns.INFO]: deposit.infoCbor,
        [UserEventsUtils.Columns.INCLUSION_TIME]: deposit.inclusionTime,
        [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: Buffer.from(
          deposit.location.txHash,
          "hex",
        ),
        [DepositsDB.Columns.LEDGER_TX_ID]: computeHash32(deposit.idCbor),
        [DepositsDB.Columns.LEDGER_OUTPUT]: Buffer.from(output),
        [DepositsDB.Columns.LEDGER_ADDRESS]: l2Address,
        [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
        [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
      };
    },
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to project deposit history data into an offchain ledger entry",
        cause,
      }),
  });

const cborBuffer = (value: unknown, schema: unknown): Buffer =>
  Buffer.from(Data.to(value as never, schema as never), "hex");

export const withdrawalDataToEntry = (
  withdrawal: WithdrawalEntryData,
): Effect.Effect<WithdrawalsDB.Entry, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const infoCbor = withdrawal.infoCbor.toString("hex");
      const rawField = (path: readonly number[]) =>
        Buffer.from(plutusConstrFieldCbor(infoCbor, path), "hex");
      const { body } = Data.from(infoCbor, SDK.WithdrawalInfo);
      return {
        [WithdrawalsDB.Columns.ID]: Buffer.from(withdrawal.idCbor),
        [WithdrawalsDB.Columns.RAW_EVENT_INFO]: Buffer.from(
          withdrawal.infoCbor,
        ),
        [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: null,
        [WithdrawalsDB.Columns.INCLUSION_TIME]: withdrawal.inclusionTime,
        [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: Buffer.from(
          withdrawal.location.txHash,
          "hex",
        ),
        [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]:
          withdrawal.location.outputIndex,
        [WithdrawalsDB.Columns.ASSET_NAME]: Buffer.from(
          withdrawal.assetName,
          "hex",
        ),
        [WithdrawalsDB.Columns.L2_OUTREF]: cborBuffer(
          body.l2_outref,
          SDK.OutputReference,
        ),
        [WithdrawalsDB.Columns.L2_OWNER]: Buffer.from(body.l2_owner, "hex"),
        [WithdrawalsDB.Columns.L2_VALUE]: rawField([0, 2]),
        [WithdrawalsDB.Columns.L1_ADDRESS]: cborBuffer(
          body.l1_address,
          SDK.AddressData,
        ),
        [WithdrawalsDB.Columns.L1_DATUM]: rawField([0, 4]),
        [WithdrawalsDB.Columns.REFUND_ADDRESS]: cborBuffer(
          Data.from(
            plutusConstrFieldCbor(withdrawal.payloadCbor, [1]),
            SDK.AddressData,
          ),
          SDK.AddressData,
        ),
        [WithdrawalsDB.Columns.REFUND_DATUM]: Buffer.from(
          plutusConstrFieldCbor(withdrawal.payloadCbor, [2]),
          "hex",
        ),
        [WithdrawalsDB.Columns.VALIDITY]: null,
        [WithdrawalsDB.Columns.CLASSIFICATION_REVISION]: 0,
        [WithdrawalsDB.Columns.REOPENED_FROM_HEADER_HASH]: null,
        [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {},
        [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]: null,
        [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Awaiting,
      };
    },
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to project withdrawal history data into an offchain withdrawal entry",
        cause,
      }),
  });

/** Convert freshly verified journal facts without inventing a live UTxO.
 * Retirement keeps the last real Order location. Orphan eligibility and existing
 * classifications must be reconciled by the SQL caller before these defaults
 * can be persisted; this function only derives immutable entry content.
 */
export const historyIncarnationEntry = (
  incarnation: HistoryIncarnation,
  network: Network,
) =>
  Effect.gen(function* () {
    const base = yield* Effect.try({
      try: () => {
        const event = incarnation.event;
        const payload = Data.from(event.payloadCbor, SDK.EventHistoryPayload);
        if ((incarnation.kind === "deposit") !== "DepositPayload" in payload)
          throw new Error("History kind disagrees with its payload");
        const time = Number(event.inclusionTime);
        const inclusionTime = new Date(time);
        if (
          !Number.isSafeInteger(time) ||
          !Number.isFinite(inclusionTime.getTime())
        )
          throw new Error(
            "History inclusion time cannot be represented by a row",
          );
        const idCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
          plutusConstrFieldCbor(event.payloadCbor, [0, 0]),
        );
        if (idCbor !== event.idCbor)
          throw new Error("History identity disagrees with its payload");
        const location =
          incarnation.placement?.current?.outRef ??
          incarnation.placement?.retirement?.outRef ??
          event.outRef;
        return {
          idCbor: Buffer.from(idCbor, "hex"),
          infoCbor: Buffer.from(
            aikenSerialisedPlutusDataCborPreservingMapOrder(
              plutusConstrFieldCbor(event.payloadCbor, [0, 1]),
            ),
            "hex",
          ),
          inclusionTime,
          location,
        };
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to read history incarnation entry",
          cause,
        }),
    });
    if (incarnation.kind === "deposit") {
      const originalAssets = yield* Effect.try({
        try: () =>
          SDK.valueToAssets(
            Data.from(incarnation.event.originalAssetsCbor, SDK.Value),
          ),
        catch: (cause) =>
          new SDK.LucidError({
            message: "Failed to read original deposit Value",
            cause,
          }),
      });
      return {
        kind: "deposit" as const,
        entry: yield* depositDataToEntry({ ...base, originalAssets }, network),
      };
    }
    return {
      kind: "withdrawal" as const,
      entry: yield* withdrawalDataToEntry({
        ...base,
        assetName: incarnation.event.key,
        payloadCbor: incarnation.event.payloadCbor,
      }),
    };
  });
