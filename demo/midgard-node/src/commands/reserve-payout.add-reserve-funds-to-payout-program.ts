import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assetsEqual,
  removeAssetUnit,
  subtractAssets,
  valueToAssets,
} from "@al-ft/midgard-sdk";
import { Data as LucidData, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import * as WithdrawalsDB from "../database/withdrawals.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { type IntentJournal, openPlan } from "../services/intent-journal.js";
import {
  ReservePayoutTransport,
  submitAddReserveFundsToPayoutProgram,
  submitConcludePayoutProgram,
} from "../transactions/reserve-payout.js";
import { outRefLabel } from "../tx-context.js";
import { formatJson, parseEventId } from "./command-utils.js";
import { resolveEventSettlementProofProgram } from "./event-settlement-proof.js";
import {
  type AddReserveFundsConfig,
  type EventIdConfig,
  fetchReferenceScripts,
  fetchWithdrawalUtxoByEventId,
  type PayoutByWithdrawalEvent,
  type PayoutCommandResult,
  requireResolution,
  submitInitializePayoutAfterProtectionProgram,
} from "./reserve-payout.retry-after-retirement-protection.js";
import { addressDataToBech32 } from "./withdrawal-utils.js";

export const initializePayoutProgram = (
  config: EventIdConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts | IntentJournal
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    // S5: the plan opens before the command's first L1 read.
    const plan = yield* openPlan;
    yield* lucidService.switchToOperatorsMainWallet;
    const resolution = requireResolution(
      yield* resolveEventSettlementProofProgram({
        kind: "withdrawal",
        eventId,
      }),
      "withdrawal",
    );
    if (resolution.validity !== "WithdrawalIsValid") {
      return yield* Effect.fail(
        new Error(
          `Withdrawal ${eventId.toString("hex")} is not valid; validity=${resolution.validity ?? "null"}.`,
        ),
      );
    }
    const withdrawal = yield* fetchWithdrawalUtxoByEventId(eventId);
    const history = SDK.requireEventHistoryContracts(contracts);
    const refs = yield* fetchReferenceScripts([
      {
        name: "withdrawal spending",
        script: history.withdrawal.list.spendingScript,
      },
      {
        name: "withdrawal history retirement",
        script: history.withdrawal.retirement.withdrawalScript,
      },
      { name: "payout minting", script: contracts.payout.mintingScript },
    ]);
    const txHash = yield* submitInitializePayoutAfterProtectionProgram(
      lucidService.api,
      contracts,
      {
        withdrawal,
        settlementRefInput: resolution.settlementRefInput,
        membershipProof: resolution.proof,
        referenceScripts: refs,
      },
      plan,
    );
    const payoutUnit = toUnit(contracts.payout.policyId, withdrawal.assetName);
    return {
      txHash,
      eventId: eventId.toString("hex"),
      details: {
        settlementOutRef: outRefLabel(resolution.settlementRefInput),
        withdrawalOutRef: outRefLabel(withdrawal.utxo),
        payoutUnit,
      },
    };
  });

const payoutUnitFromWithdrawalEventId = (
  eventId: Buffer,
): Effect.Effect<string, Error, Database | MidgardContracts> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const maybeEntry = yield* WithdrawalsDB.retrieveByEventId(eventId);
    if (Option.isNone(maybeEntry)) {
      return yield* Effect.fail(
        new Error(`Withdrawal event ${eventId.toString("hex")} not found.`),
      );
    }
    return toUnit(
      contracts.payout.policyId,
      maybeEntry.value[WithdrawalsDB.Columns.ASSET_NAME].toString("hex"),
    );
  });

const fetchPayoutByWithdrawalEvent = (
  eventId: Buffer,
): Effect.Effect<
  PayoutByWithdrawalEvent,
  SDK.LucidError | Error,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const payoutUnit = yield* payoutUnitFromWithdrawalEventId(eventId);
    const payouts = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.payout.spendingScriptAddress,
          payoutUnit,
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch payout UTxOs",
          cause,
        }),
    });
    const matches = payouts.filter(
      (utxo) => (utxo.assets[payoutUnit] ?? 0n) === 1n,
    );
    if (matches.length !== 1) {
      return yield* Effect.fail(
        new Error(
          `Expected exactly one payout UTxO for ${payoutUnit}, found ${matches.length.toString()}.`,
        ),
      );
    }
    return { payout: matches[0]!, payoutUnit };
  });

const decodePayoutDatum = (payout: UTxO): SDK.PayoutDatum => {
  if (payout.datum == null) {
    throw new Error(`Payout UTxO ${outRefLabel(payout)} has no inline datum.`);
  }
  return LucidData.from(payout.datum, SDK.PayoutDatum) as SDK.PayoutDatum;
};

export const addReserveFundsToPayoutProgram = (
  config: AddReserveFundsConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts | IntentJournal
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    // S5: the plan opens before the command's first L1 read.
    const plan = yield* openPlan;
    yield* lucidService.switchToOperatorsMainWallet;
    const { payout, payoutUnit } = yield* fetchPayoutByWithdrawalEvent(eventId);
    const payoutDatum = decodePayoutDatum(payout);
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentAssets = removeAssetUnit(payout.assets, payoutUnit, 1n);
    const remaining = subtractAssets(targetAssets, currentAssets);
    const reserveUtxos = yield* Effect.tryPromise({
      try: () =>
        lucidService.api.utxosAt(contracts.reserve.spendingScriptAddress),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch reserve UTxOs",
          cause,
        }),
    });
    const coinsPerUtxoByte =
      lucidService.api.config().protocolParameters?.coinsPerUtxoByte;
    if (coinsPerUtxoByte === undefined) {
      return yield* Effect.fail(
        new Error("Reserve funding needs live protocol parameters."),
      );
    }
    let reserve: UTxO | undefined;
    if (config.reserveOutRef === undefined) {
      reserve = SDK.selectReserveFundingInput(
        reserveUtxos,
        remaining,
        coinsPerUtxoByte,
      );
      if (reserve === undefined) {
        return yield* Effect.fail(
          new Error(
            "No spendable reserve UTxO can fund the payout's remaining target.",
          ),
        );
      }
    } else {
      const requestedOutRef = config.reserveOutRef;
      const requested = yield* Effect.try({
        try: () => outRefLabel(parseOutRefLabel(requestedOutRef)),
        catch: (cause) =>
          new Error(
            `--reserve-out-ref: ${cause instanceof Error ? cause.message : String(cause)}`,
          ),
      });
      reserve = reserveUtxos.find((utxo) => outRefLabel(utxo) === requested);
      if (reserve === undefined) {
        return yield* Effect.fail(
          new Error(`Reserve UTxO ${requested} is not at the reserve address.`),
        );
      }
      const rejection = SDK.reserveFundingRejection(
        reserve,
        remaining,
        coinsPerUtxoByte,
      );
      if (rejection !== undefined) {
        return yield* Effect.fail(
          new Error(`Reserve UTxO ${requested} ${rejection}.`),
        );
      }
    }
    const refs = yield* fetchReferenceScripts([
      { name: "reserve spending", script: contracts.reserve.spendingScript },
      { name: "payout spending", script: contracts.payout.spendingScript },
    ]);
    const txHash = yield* submitAddReserveFundsToPayoutProgram(
      lucidService.api,
      contracts,
      {
        payoutInput: payout,
        reserveInput: reserve,
        referenceScripts: refs,
        ...(Option.isSome(yield* Effect.serviceOption(ReservePayoutTransport))
          ? { validTo: Date.now() + 180_000 }
          : {}),
      },
      eventId,
      plan,
    );
    return {
      txHash,
      eventId: eventId.toString("hex"),
      details: {
        payoutOutRef: outRefLabel(payout),
        reserveOutRef: outRefLabel(reserve),
        targetAssets,
        currentAssets,
        remainingAssetsBeforeFunding: remaining,
      },
    };
  });

export const concludePayoutProgram = (
  config: EventIdConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
    // S5: the plan opens before the command's first L1 read.
    const plan = yield* openPlan;
    yield* lucidService.switchToOperatorsMainWallet;
    const { payout, payoutUnit } = yield* fetchPayoutByWithdrawalEvent(eventId);
    const payoutDatum = decodePayoutDatum(payout);
    const targetAssets = valueToAssets(payoutDatum.l2_value);
    const currentAssets = removeAssetUnit(payout.assets, payoutUnit, 1n);
    if (!assetsEqual(currentAssets, targetAssets)) {
      return yield* Effect.fail(
        new Error(
          `Payout is not exactly funded. target=${formatJson(targetAssets)}, current=${formatJson(currentAssets)}`,
        ),
      );
    }
    const refs = yield* fetchReferenceScripts([
      { name: "payout spending", script: contracts.payout.spendingScript },
      { name: "payout minting", script: contracts.payout.mintingScript },
    ]);
    const txHash = yield* submitConcludePayoutProgram(
      lucidService.api,
      contracts,
      {
        payoutInput: payout,
        referenceScripts: refs,
        ...(Option.isSome(yield* Effect.serviceOption(ReservePayoutTransport))
          ? { validTo: Date.now() + 180_000 }
          : {}),
      },
      eventId,
      plan,
    );
    return {
      txHash,
      eventId: eventId.toString("hex"),
      details: {
        payoutOutRef: outRefLabel(payout),
        payoutUnit,
        l1Address: addressDataToBech32(
          nodeConfig.NETWORK,
          payoutDatum.l1_address,
        ),
        paidAssets: targetAssets,
      },
    };
  });
