import { assetUnitFromParts } from "@al-ft/midgard-core/assets";
import {
  decodeMidgardAddressText,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  midgardCardanoValueCborLength,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect, type Either } from "effect";

import { Value } from "./common.js";
import { WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT } from "./fraud-proof/withdrawal-mistag.js";
import { withdrawalMistagPayable } from "./fraud-proof/withdrawal-payability.js";
import { WithdrawalInfo, WithdrawalValidity } from "./ledger-state.js";
import { verifyWithdrawalSignatureCbor } from "./withdrawal-signature.js";
import {
  withdrawalLedgerAssetCount,
  withdrawalValueMatchesLedgerOutput,
} from "./withdrawal-value.js";

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
    yield* decodeWithdrawalInfo(input);
    return yield* Effect.try({
      try: () =>
        Buffer.from(
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            replacePlutusConstrFieldCbor(
              input.eventInfoCbor,
              [2],
              LucidData.to(validity, WithdrawalValidity),
            ),
          ),
          "hex",
        ),
      catch: (cause) =>
        new WithdrawalValidationError({
          message: "Failed to encode withdrawal settlement event info",
          cause,
        }),
    });
  });

const decodeLedgerOutput = ({
  outRef,
  output,
}: {
  readonly outRef: Buffer;
  readonly output: Buffer;
}): Effect.Effect<
  { readonly address: string; readonly value: MidgardValue },
  WithdrawalValidationError,
  never
> =>
  Effect.try({
    try: () => {
      // Ledger out-refs are the §5.3 field-0/1 item form — 38 bytes,
      // `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` — matching on-chain
      // `ledger_outref_key`, not CML's minimal-index `TransactionInput` CBOR.
      decodeMidgardSpendInputItem(outRef);
      const decodedOutput = decodeMidgardTxOutput(output);
      return {
        address: encodeMidgardAddressText(decodedOutput.address),
        value: decodedOutput.value,
      };
    },
    catch: (cause) =>
      new WithdrawalValidationError({
        message: "Failed to decode ledger UTxO for withdrawal classification",
        cause,
      }),
  });

/** The ledger output's assets by unit, for the mismatch detail. */
const assetQuantities = (value: MidgardValue): Record<string, string> =>
  Object.fromEntries([
    ...(value.lovelace === 0n
      ? []
      : [[assetUnitFromParts("", ""), value.lovelace.toString()]]),
    ...[...value.assets.entries()].flatMap(([policy, assets]) =>
      [...assets.entries()].map(([name, quantity]) => [
        assetUnitFromParts(policy, name),
        quantity.toString(),
      ]),
    ),
  ]);

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
      const ledger = yield* decodeLedgerOutput({
        outRef: ledgerOutRef,
        output: ledgerOutput,
      });
      const paymentCredential = decodeMidgardAddressText(
        ledger.address,
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
        // The chain's rule (withdrawal-mistag step-03): decoded values
        // compared as sets, and lovelace excluded from the asset count.
        if (!withdrawalValueMatchesLedgerOutput(requestedValue, ledger.value)) {
          validity = "IncorrectWithdrawalValue";
          validityDetail = {
            requested_value_cbor: input.l2ValueCbor,
            actual_assets: assetQuantities(ledger.value),
          };
        } else if (
          withdrawalLedgerAssetCount(ledger.value) >
          WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT
        ) {
          validity = "TooManyTokensInWithdrawal";
        } else {
          const withdrawalInfo = yield* decodeWithdrawalInfo(input);
          const verification = verifyWithdrawalSignatureCbor(
            plutusConstrFieldCbor(input.eventInfoCbor, [0]),
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
            // The chain's rule (withdrawal-mistag step-04): the value fits
            // one L1 output and meets that output's min-ADA.
            const cardanoValueSize = BigInt(
              midgardCardanoValueCborLength(ledger.value),
            );
            const payable = yield* Effect.try({
              try: () =>
                withdrawalMistagPayable({
                  bodyCbor: plutusConstrFieldCbor(input.eventInfoCbor, [0]),
                  lovelace: ledger.value.lovelace,
                  cardanoValueSize,
                }),
              catch: (cause) =>
                new WithdrawalValidationError({
                  message: "Failed to read the withdrawal payout target",
                  cause,
                }),
            });
            if (payable) {
              validity = "WithdrawalIsValid";
            } else {
              validity = "UnpayableWithdrawalValue";
              validityDetail = {
                cardano_value_size: cardanoValueSize.toString(),
                lovelace: ledger.value.lovelace.toString(),
              };
            }
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
