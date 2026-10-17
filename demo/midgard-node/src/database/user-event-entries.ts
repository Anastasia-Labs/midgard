/**
 * Converts the authenticated bytes of an L1 deposit or withdrawal event, with
 * its location, into the node's SQL row (`deposits_utxos`,
 * `withdrawal_utxos`). It does not require an unspent order, and grants no
 * selection or L2 authority.
 */
import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { type Assets, Data, type Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as DepositsDB from "./deposits.js";
import * as UserEventsUtils from "./utils/user-events.js";
import * as WithdrawalsDB from "./withdrawals.js";

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
