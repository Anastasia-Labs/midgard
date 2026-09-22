import { assetUnitParts, normalizeAssets } from "@al-ft/midgard-core/assets";
import {
  decodeMidgardAddressText,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import type { Assets, UTxO } from "@lucid-evolution/lucid";
import { Data as LucidData, valueToAssets } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect, type Either } from "effect";

import { Value } from "./common.js";
import { committedWithdrawalValueBytes } from "./fraud-proof/fabricated-withdrawal.js";
import { WithdrawalInfo, type WithdrawalValidity } from "./ledger-state.js";
import { verifyWithdrawalSignature } from "./withdrawal-signature.js";

/** A deterministic computation over caller-authenticated event and selected-base ledger bytes.
 * Callers must bind the projected owner/value to the originating event. This
 * function neither authenticates provenance nor reads a provider or database.
 */
export type WithdrawalLedgerClassificationInput = Readonly<{
  l2Owner: string;
  l2ValueCbor: string;
  eventInfoCbor: string;
  ledgerOutRef: Buffer;
  ledgerOutput: Buffer | null;
}>;

export type WithdrawalLedgerClassification = Readonly<{
  validity: Extract<WithdrawalValidity, string>;
  validityDetail: unknown;
  settlementEventInfo: Buffer;
  shouldDeleteLedgerUtxo: boolean;
}>;

export class WithdrawalValidationError extends EffectData.TaggedError(
  "WithdrawalValidationError",
)<{ readonly message: string; readonly cause: unknown }> {}

const assetsToValue = (assets: Assets): Value => {
  const outer = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(normalizeAssets(assets))) {
    const { policyId, assetName } = assetUnitParts(unit);
    const inner = outer.get(policyId) ?? new Map<string, bigint>();
    inner.set(assetName, (inner.get(assetName) ?? 0n) + quantity);
    outer.set(policyId, inner);
  }
  return outer;
};

const decodeWithdrawalInfo = (
  input: WithdrawalLedgerClassificationInput,
): Effect.Effect<WithdrawalInfo, WithdrawalValidationError, never> =>
  Effect.try({
    try: () =>
      LucidData.from(input.eventInfoCbor, WithdrawalInfo) as WithdrawalInfo,
    catch: (cause) =>
      new WithdrawalValidationError({
        message: "Failed to decode withdrawal event info",
        cause,
      }),
  });

const encodeWithdrawalSettlementInfo = (
  input: WithdrawalLedgerClassificationInput,
  validity: WithdrawalLedgerClassification["validity"],
): Effect.Effect<Buffer, WithdrawalValidationError, never> =>
  Effect.gen(function* () {
    const rawInfo = yield* decodeWithdrawalInfo(input);
    return yield* Effect.try({
      try: () =>
        Buffer.from(
          committedWithdrawalValueBytes({
            ...rawInfo,
            validity,
          }),
          "hex",
        ),
      catch: (cause) =>
        new WithdrawalValidationError({
          message: "Failed to encode withdrawal settlement event info",
          cause,
        }),
    });
  });

const decodeLedgerUtxo = ({
  outRef,
  output,
}: {
  readonly outRef: Buffer;
  readonly output: Buffer;
}): Effect.Effect<UTxO, WithdrawalValidationError, never> =>
  Effect.try({
    try: () => {
      // Ledger out-refs are the §5.3 field-0/1 item form — 38 bytes,
      // `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` — matching on-chain
      // `ledger_outref_key`, not CML's minimal-index `TransactionInput` CBOR.
      // The decoder bounds the index to a CBOR uint16, so no safe-integer
      // check is needed on the way out.
      const input = decodeMidgardSpendInputItem(outRef);
      const decodedOutput = decodeMidgardTxOutput(output);
      return {
        txHash: Buffer.from(input.txId).toString("hex"),
        outputIndex: input.outputIndex,
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
      new WithdrawalValidationError({
        message: "Failed to decode ledger UTxO for withdrawal classification",
        cause,
      }),
  });

const valuesEqual = (
  left: Value,
  right: Value,
): Effect.Effect<boolean, WithdrawalValidationError, never> =>
  Effect.try({
    try: () =>
      Buffer.from(LucidData.to(left, Value), "hex").equals(
        Buffer.from(LucidData.to(right, Value), "hex"),
      ),
    catch: (cause) =>
      new WithdrawalValidationError({
        message: "Failed to compare withdrawal value CBOR",
        cause,
      }),
  });

export const classifyWithdrawalFromLedger = (
  input: WithdrawalLedgerClassificationInput,
): Effect.Effect<
  WithdrawalLedgerClassification,
  WithdrawalValidationError,
  never
> =>
  Effect.gen(function* () {
    const { ledgerOutRef, ledgerOutput } = input;
    let validity: WithdrawalLedgerClassification["validity"];
    let validityDetail: unknown = {};

    if (ledgerOutput === null) {
      validity = "NonExistentWithdrawalUtxo";
    } else {
      const utxo = yield* decodeLedgerUtxo({
        outRef: ledgerOutRef,
        output: ledgerOutput,
      });
      const paymentCredential = decodeMidgardAddressText(
        utxo.address,
      ).paymentCredential;
      if (paymentCredential.hash.toString("hex") !== input.l2Owner) {
        validity = "IncorrectWithdrawalOwner";
      } else {
        const requestedValue = yield* Effect.try({
          try: () => LucidData.from(input.l2ValueCbor, Value) as Value,
          catch: (cause) =>
            new WithdrawalValidationError({
              message: "Failed to decode withdrawal l2_value",
              cause,
            }),
        });
        const actualAssets = normalizeAssets(utxo.assets);
        const actualValue = assetsToValue(actualAssets);
        const valueMatches = yield* valuesEqual(requestedValue, actualValue);
        if (!valueMatches) {
          validity = "IncorrectWithdrawalValue";
          validityDetail = {
            requested_value_cbor: input.l2ValueCbor,
            actual_assets: Object.fromEntries(
              Object.entries(actualAssets).map(([unit, quantity]) => [
                unit,
                quantity.toString(),
              ]),
            ),
          };
        } else if (Object.keys(actualAssets).length > 100) {
          validity = "TooManyTokensInWithdrawal";
        } else {
          const withdrawalInfo = yield* decodeWithdrawalInfo(input);
          const verification = verifyWithdrawalSignature(
            withdrawalInfo.body,
            withdrawalInfo.signature,
            input.l2Owner,
          );
          if (!verification.valid) {
            validity = "IncorrectWithdrawalSignature";
            validityDetail = {
              reason: verification.reason,
              ...(verification.publicKeyHash === undefined
                ? {}
                : { public_key_hash: verification.publicKeyHash }),
            };
          } else {
            validity = "WithdrawalIsValid";
          }
        }
      }
    }

    const settlementEventInfo = yield* encodeWithdrawalSettlementInfo(
      input,
      validity,
    );
    return {
      validity,
      validityDetail,
      settlementEventInfo,
      shouldDeleteLedgerUtxo: validity === "WithdrawalIsValid",
    };
  });

/** Runs the synchronous classifier with the SDK's Effect runtime, preserving
 * its typed failure channel for callers that do not own an Effect runtime.
 */
export const classifyWithdrawalFromLedgerSync = (
  input: WithdrawalLedgerClassificationInput,
): Either.Either<WithdrawalLedgerClassification, WithdrawalValidationError> =>
  Effect.runSync(Effect.either(classifyWithdrawalFromLedger(input)));
