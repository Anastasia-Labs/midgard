import * as SDK from "@al-ft/midgard-sdk";
import { mergeReferenceScripts } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Clock, Effect, Option } from "effect";

import { SUBMIT_SLOT_LENGTH_MS } from "../local-ledger-slot.js";
import { Database, Lucid, MidgardContracts } from "../services/index.js";
import {
  fetchReferenceScriptUtxosProgram,
  type ReferenceScriptTarget,
} from "../transactions/reference-scripts.js";
import {
  type ReservePayoutReferenceScripts,
  ReservePayoutTransport,
  submitAbsorbConfirmedDepositToReserveProgram,
  submitInitializePayoutProgram,
} from "../transactions/reserve-payout.js";
import { outRefLabel } from "../tx-context.js";
import { parseEventId } from "./command-utils.js";
import {
  type EventSettlementProofResolution,
  resolveEventSettlementProofProgram,
} from "./event-settlement-proof.js";

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

export type PayoutByWithdrawalEvent = {
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
          // The automatic scheduler records a due time and releases its slot;
          // it must not sleep behind an individual protected history entry.
          if (
            Option.isSome(yield* Effect.serviceOption(ReservePayoutTransport))
          )
            return yield* Effect.fail(error);
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

export const fetchReferenceScripts = (
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

export const fetchWithdrawalUtxoByEventId = (
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

export const requireResolution = <
  Kind extends EventSettlementProofResolution["kind"],
>(
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
