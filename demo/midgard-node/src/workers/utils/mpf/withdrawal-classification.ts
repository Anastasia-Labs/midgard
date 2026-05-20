import { CML, Data as LucidData, valueToAssets } from "@lucid-evolution/lucid";
import type { Assets, UTxO } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  decodeMidgardAddressText,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import { verifyWithdrawalSignature } from "@/withdrawal-signature.js";

export type ClassifiedWithdrawal = {
  readonly entry: WithdrawalsDB.Entry;
  readonly ledgerOutRef: Buffer;
  readonly validity: WithdrawalsDB.Validity;
  readonly validityDetail: unknown;
  readonly settlementEventInfo: Buffer;
  readonly shouldDeleteLedgerUtxo: boolean;
};

export const LOVELACE_UNIT = "lovelace";
const ADA_POLICY_ID = "";
const ADA_ASSET_NAME = "";

export const normalizeAssets = (assets: Assets): Assets => {
  const result: Record<string, bigint> = {};
  for (const [unit, quantity] of Object.entries(assets)) {
    if (quantity === 0n) {
      continue;
    }
    result[unit] = (result[unit] ?? 0n) + quantity;
  }
  return result as Assets;
};

export const assetsToValue = (assets: Assets): SDK.Value => {
  const outer = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(normalizeAssets(assets))) {
    const policyId =
      unit === LOVELACE_UNIT ? ADA_POLICY_ID : unit.slice(0, 56).toLowerCase();
    const assetName =
      unit === LOVELACE_UNIT ? ADA_ASSET_NAME : unit.slice(56).toLowerCase();
    const inner = outer.get(policyId) ?? new Map<string, bigint>();
    inner.set(assetName, (inner.get(assetName) ?? 0n) + quantity);
    outer.set(policyId, inner);
  }
  return outer;
};

export const withdrawalValidityToSdk = (
  validity: WithdrawalsDB.Validity,
  detail: unknown,
): SDK.WithdrawalValidity => {
  if (validity !== WithdrawalsDB.Validity.SpentWithdrawalUtxo) {
    return validity as SDK.WithdrawalValidity;
  }
  const detailRecord =
    typeof detail === "object" && detail !== null
      ? (detail as { readonly l2_tx_id?: unknown })
      : {};
  const l2TxId =
    typeof detailRecord.l2_tx_id === "string"
      ? detailRecord.l2_tx_id
      : "00".repeat(32);
  return {
    SpentWithdrawalUtxo: {
      l2_tx_id: l2TxId,
    },
  } as SDK.WithdrawalValidity;
};

export const decodeWithdrawalInfo = (
  entry: WithdrawalsDB.Entry,
): Effect.Effect<SDK.WithdrawalInfo, DatabaseError, never> =>
  Effect.try({
    try: () =>
      LucidData.from(
        entry[WithdrawalsDB.Columns.RAW_EVENT_INFO].toString("hex"),
        SDK.WithdrawalInfo,
      ) as SDK.WithdrawalInfo,
    catch: (cause) =>
      new DatabaseError({
        table: WithdrawalsDB.tableName,
        message: "Failed to decode withdrawal event info",
        cause,
      }),
  });

export const encodeWithdrawalSettlementInfo = (
  entry: WithdrawalsDB.Entry,
  validity: WithdrawalsDB.Validity,
  validityDetail: unknown,
): Effect.Effect<Buffer, DatabaseError, never> =>
  Effect.gen(function* () {
    const rawInfo = yield* decodeWithdrawalInfo(entry);
    return yield* Effect.try({
      try: () =>
        Buffer.from(
          LucidData.to(
            {
              ...rawInfo,
              validity: withdrawalValidityToSdk(validity, validityDetail),
            } satisfies SDK.WithdrawalInfo,
            SDK.WithdrawalInfo,
          ),
          "hex",
        ),
      catch: (cause) =>
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message: "Failed to encode withdrawal settlement event info",
          cause,
        }),
    });
  });

export const decodeLedgerUtxo = ({
  outRef,
  output,
}: {
  readonly outRef: Buffer;
  readonly output: Buffer;
}): Effect.Effect<UTxO, DatabaseError, never> =>
  Effect.try({
    try: () => {
      const input = CML.TransactionInput.from_cbor_bytes(outRef);
      const decodedOutput = decodeMidgardTxOutput(output);
      const outputIndex = Number(input.index());
      if (!Number.isSafeInteger(outputIndex)) {
        throw new Error("output index exceeds JavaScript safe integer range");
      }
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex,
        address: encodeMidgardAddressText(decodedOutput.address),
        assets: valueToAssets(
          midgardValueToCmlValue(decodedOutput.value),
        ) as Assets,
        ...(decodedOutput.datum === undefined
          ? {}
          : { datum: decodedOutput.datum.cbor.toString("hex") }),
      } satisfies UTxO;
    },
    catch: (cause) =>
      new DatabaseError({
        table: WithdrawalsDB.tableName,
        message: "Failed to decode ledger UTxO for withdrawal classification",
        cause,
      }),
  });

export const valuesEqual = (
  left: SDK.Value,
  right: SDK.Value,
): Effect.Effect<boolean, DatabaseError, never> =>
  Effect.try({
    try: () =>
      Buffer.from(LucidData.to(left, SDK.Value), "hex").equals(
        Buffer.from(LucidData.to(right, SDK.Value), "hex"),
      ),
    catch: (cause) =>
      new DatabaseError({
        table: WithdrawalsDB.tableName,
        message: "Failed to compare withdrawal value CBOR",
        cause,
      }),
  });

export const classifyWithdrawal = ({
  entry,
  ledgerOutRef,
  ledgerOutput,
}: {
  readonly entry: WithdrawalsDB.Entry;
  readonly ledgerOutRef: Buffer;
  readonly ledgerOutput: Option.Option<Buffer>;
}): Effect.Effect<ClassifiedWithdrawal, DatabaseError, never> =>
  Effect.gen(function* () {
    let validity: WithdrawalsDB.Validity;
    let validityDetail: unknown = {};

    if (Option.isNone(ledgerOutput)) {
      validity = WithdrawalsDB.Validity.NonExistentWithdrawalUtxo;
    } else {
      const utxo = yield* decodeLedgerUtxo({
        outRef: ledgerOutRef,
        output: ledgerOutput.value,
      });
      const paymentCredential = decodeMidgardAddressText(
        utxo.address,
      ).paymentCredential;
      if (
        paymentCredential.hash.toString("hex").toLowerCase() !==
        entry[WithdrawalsDB.Columns.L2_OWNER].toString("hex").toLowerCase()
      ) {
        validity = WithdrawalsDB.Validity.IncorrectWithdrawalOwner;
      } else {
        const requestedValue = yield* Effect.try({
          try: () =>
            LucidData.from(
              entry[WithdrawalsDB.Columns.L2_VALUE].toString("hex"),
              SDK.Value,
            ) as SDK.Value,
          catch: (cause) =>
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message: "Failed to decode withdrawal l2_value",
              cause,
            }),
        });
        const actualValue = assetsToValue(utxo.assets);
        const valueMatches = yield* valuesEqual(requestedValue, actualValue);
        if (!valueMatches) {
          validity = WithdrawalsDB.Validity.IncorrectWithdrawalValue;
          validityDetail = {
            requested_value_cbor:
              entry[WithdrawalsDB.Columns.L2_VALUE].toString("hex"),
            actual_assets: Object.fromEntries(
              Object.entries(normalizeAssets(utxo.assets)).map(
                ([unit, quantity]) => [unit, quantity.toString()],
              ),
            ),
          };
        } else if (Object.keys(normalizeAssets(utxo.assets)).length > 100) {
          validity = WithdrawalsDB.Validity.TooManyTokensInWithdrawal;
        } else {
          const withdrawalInfo = yield* decodeWithdrawalInfo(entry);
          const verification = verifyWithdrawalSignature(
            withdrawalInfo.body,
            withdrawalInfo.signature,
            entry[WithdrawalsDB.Columns.L2_OWNER].toString("hex"),
          );
          if (!verification.valid) {
            validity = WithdrawalsDB.Validity.IncorrectWithdrawalSignature;
            validityDetail = {
              reason: verification.reason,
              ...(verification.publicKeyHash === undefined
                ? {}
                : { public_key_hash: verification.publicKeyHash }),
            };
          } else {
            validity = WithdrawalsDB.Validity.WithdrawalIsValid;
          }
        }
      }
    }

    const settlementEventInfo = yield* encodeWithdrawalSettlementInfo(
      entry,
      validity,
      validityDetail,
    );
    return {
      entry,
      ledgerOutRef,
      validity,
      validityDetail,
      settlementEventInfo,
      shouldDeleteLedgerUtxo:
        validity === WithdrawalsDB.Validity.WithdrawalIsValid,
    };
  });
