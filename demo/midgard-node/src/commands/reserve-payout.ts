import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assetsEqual,
  mergeReferenceScripts,
  removeAssetUnit,
  subtractAssets,
  valueToAssets,
} from "@al-ft/midgard-sdk";
import { Data as LucidData, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Clock, Effect, Option } from "effect";

import * as WithdrawalsDB from "../database/withdrawals.js";
import { SUBMIT_SLOT_LENGTH_MS } from "../local-ledger-slot.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  fetchReferenceScriptUtxosProgram,
  type ReferenceScriptTarget,
} from "../transactions/reference-scripts.js";
import {
  type ReservePayoutReferenceScripts,
  submitAbsorbConfirmedDepositToReserveProgram,
  submitAddReserveFundsToPayoutProgram,
  submitConcludePayoutProgram,
  submitInitializePayoutProgram,
} from "../transactions/reserve-payout.js";
import { outRefLabel } from "../tx-context.js";
import { formatJson, parseEventId } from "./command-utils.js";
import {
  type EventSettlementProofResolution,
  resolveEventSettlementProofProgram,
} from "./event-settlement-proof.js";
import { addressDataToBech32 } from "./withdrawal-utils.js";

export type EventIdConfig = {
  readonly eventId: string;
};

export type AddReserveFundsConfig = EventIdConfig & {
  /** Reserve UTxO to spend instead of the automatic selection. */
  readonly reserveOutRef?: string;
};

export type PayoutCommandResult = {
  readonly txHash: string;
  readonly eventId: string;
  readonly details: Record<string, unknown>;
};

type PayoutByWithdrawalEvent = {
  readonly payout: UTxO;
  readonly payoutUnit: string;
};

/** Sleeps until the clock reads `wakeAtMs`. A timer can fire early against the
 * clock (the event loop's cached time), so it re-arms for what remains. */
const sleepUntil = (wakeAtMs: number): Effect.Effect<void> =>
  Effect.flatMap(Clock.currentTimeMillis, (nowMs) =>
    nowMs >= wakeAtMs
      ? Effect.void
      : Effect.zipRight(
          Effect.sleep(wakeAtMs - nowMs),
          Effect.suspend(() => sleepUntil(wakeAtMs)),
        ),
  );

/** Rebuilds a retirement once after its protection bound passes. The wait is
 * bounded by the longest protection an honest mutation can set from now: a
 * full validity window plus the list's protection duration. */
export const retryAfterRetirementProtection = <A, E, R>(
  retirement: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | Error, R> => {
  const protection = (
    error: E,
  ): SDK.HistoryRetirementProtectedError | undefined =>
    error instanceof SDK.ReservePayoutTxError &&
    error.cause instanceof SDK.HistoryRetirementProtectedError
      ? error.cause
      : undefined;
  const stillProtected = (until: bigint, cause: unknown) =>
    new Error(
      `History retirement is still protected; protected_until=${until.toString()}`,
      { cause },
    );
  return retirement.pipe(
    Effect.catchIf(
      (error) => protection(error) !== undefined,
      (error) =>
        Effect.gen(function* () {
          const { protectedUntilMs, protectionDurationMs } = protection(error)!;
          const wakeAtMs = Number(protectedUntilMs) + SUBMIT_SLOT_LENGTH_MS;
          const waitMs = wakeAtMs - (yield* Clock.currentTimeMillis);
          const maxWaitMs =
            Number(SDK.MAX_VALIDITY_RANGE_LENGTH_MS + protectionDurationMs) +
            SUBMIT_SLOT_LENGTH_MS;
          if (waitMs > maxWaitMs)
            return yield* Effect.fail(stillProtected(protectedUntilMs, error));
          yield* Effect.logInfo(
            `History retirement is protected until ${protectedUntilMs.toString()}; waiting ${waitMs.toString()}ms before rebuilding`,
          );
          yield* sleepUntil(wakeAtMs);
          return yield* retirement.pipe(
            Effect.catchIf(
              (retry) => protection(retry) !== undefined,
              (retry) =>
                Effect.fail(
                  stillProtected(protection(retry)!.protectedUntilMs, retry),
                ),
            ),
          );
        }),
    ),
  );
};

/** Deposit absorption, waiting once for the protection bound it was refused
 * below. */
export const submitAbsorbAfterProtectionProgram = (
  ...submission: Parameters<typeof submitAbsorbConfirmedDepositToReserveProgram>
) =>
  retryAfterRetirementProtection(
    submitAbsorbConfirmedDepositToReserveProgram(...submission),
  );

/** Payout initialization, waiting once for the protection bound it was refused
 * below. */
export const submitInitializePayoutAfterProtectionProgram = (
  ...submission: Parameters<typeof submitInitializePayoutProgram>
) =>
  retryAfterRetirementProtection(submitInitializePayoutProgram(...submission));

const fetchReferenceScripts = (
  targets: readonly ReferenceScriptTarget[],
): Effect.Effect<
  ReservePayoutReferenceScripts,
  SDK.StateQueueError,
  Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const resolved = yield* fetchReferenceScriptUtxosProgram(
      lucidService.api,
      lucidService.referenceScriptsAddress,
      targets,
      contracts.referenceScriptAuth,
    );
    return mergeReferenceScripts(undefined, resolved);
  });

const fetchDepositUtxoByEventId = (
  eventId: Buffer,
): Effect.Effect<
  SDK.DepositUTxO,
  SDK.LucidError | Error,
  Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const deposits = yield* SDK.fetchDepositUTxOsProgram(lucid, {
      ...SDK.eventHistoryDeploymentFromContracts(
        SDK.requireEventHistoryContracts(contracts).deposit,
      ),
    });
    const match = deposits.find((deposit) =>
      Buffer.from(deposit.idCbor).equals(eventId),
    );
    if (match === undefined) {
      return yield* Effect.fail(
        new Error(
          `Deposit UTxO for event ${eventId.toString("hex")} is not present on L1.`,
        ),
      );
    }
    return match;
  });

const fetchWithdrawalUtxoByEventId = (
  eventId: Buffer,
): Effect.Effect<
  SDK.WithdrawalUTxO,
  SDK.LucidError | Error,
  Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const withdrawals = yield* SDK.fetchWithdrawalUTxOsProgram(lucid, {
      ...SDK.eventHistoryDeploymentFromContracts(
        SDK.requireEventHistoryContracts(contracts).withdrawal,
      ),
    });
    const match = withdrawals.find((withdrawal) =>
      Buffer.from(withdrawal.idCbor).equals(eventId),
    );
    if (match === undefined) {
      return yield* Effect.fail(
        new Error(
          `Withdrawal UTxO for event ${eventId.toString("hex")} is not present on L1.`,
        ),
      );
    }
    return match;
  });

const requireResolution = <Kind extends EventSettlementProofResolution["kind"]>(
  resolution: EventSettlementProofResolution,
  kind: Kind,
): Extract<EventSettlementProofResolution, { readonly kind: Kind }> => {
  if (resolution.kind !== kind) {
    throw new Error(`Expected ${kind} event settlement proof resolution.`);
  }
  return resolution as Extract<
    EventSettlementProofResolution,
    { readonly kind: Kind }
  >;
};

export const absorbConfirmedDepositToReserveProgram = (
  config: EventIdConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--deposit-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    yield* lucidService.switchToOperatorsMainWallet;
    const resolution = requireResolution(
      yield* resolveEventSettlementProofProgram({
        kind: "deposit",
        eventId,
      }),
      "deposit",
    );
    const deposit = yield* fetchDepositUtxoByEventId(eventId);
    const history = SDK.requireEventHistoryContracts(contracts);
    const refs = yield* fetchReferenceScripts([
      { name: "deposit spending", script: history.deposit.list.spendingScript },
      {
        name: "deposit history retirement",
        script: history.deposit.retirement.withdrawalScript,
      },
    ]);
    const txHash = yield* submitAbsorbAfterProtectionProgram(
      lucidService.api,
      contracts,
      {
        deposit,
        settlementRefInput: resolution.settlementRefInput,
        membershipProof: resolution.proof,
        referenceScripts: refs,
      },
    );
    return {
      txHash,
      eventId: eventId.toString("hex"),
      details: {
        settlementOutRef: outRefLabel(resolution.settlementRefInput),
        depositOutRef: outRefLabel(deposit.utxo),
        depositAssets: deposit.utxo.assets,
      },
    };
  });

export const initializePayoutProgram = (
  config: EventIdConfig,
): Effect.Effect<
  PayoutCommandResult,
  unknown,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
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
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
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
      },
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
  Database | Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--withdrawal-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
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
      },
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
