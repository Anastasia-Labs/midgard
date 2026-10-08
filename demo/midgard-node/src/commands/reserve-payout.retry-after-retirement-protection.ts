import { SUBMIT_SLOT_LENGTH_MS } from "@al-ft/midgard-core/ogmios-slot";
import { postgresDialect } from "@al-ft/midgard-l1-follower";
import { eventOrderByIdIn } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { mergeReferenceScripts } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Clock, Effect, Option } from "effect";

import { inFollowerSnapshot } from "../database/follower-schema.js";
import { Database, Lucid, MidgardContracts } from "../services/index.js";
import { type IntentJournal, openPlan } from "../services/intent-journal.js";
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

/**
 * The live Order of the `kind` event with id CBOR `eventId`, opened as the
 * SDK opens it, read by key from the follower's event projection in the
 * node database (NC13): never a scan of the list or its retention address.
 * The settlement retries while the follower has not admitted the event yet.
 */
const readEventOrderById = (
  kind: "deposit" | "withdrawal",
  eventId: Buffer,
): Effect.Effect<
  Readonly<{ order: UTxO; retained: readonly UTxO[] }>,
  Error,
  Database | MidgardContracts
> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const policyId =
      SDK.requireEventHistoryContracts(contracts)[kind].list.policyId;
    const read = yield* inFollowerSnapshot((tx) =>
      eventOrderByIdIn(tx, postgresDialect, { kind, policyId }, eventId),
    ).pipe(
      Effect.mapError(
        (cause) =>
          new Error(`The ${kind} event projection is unreadable`, { cause }),
      ),
    );
    const label = `${kind === "deposit" ? "Deposit" : "Withdrawal"} UTxO for event ${eventId.toString("hex")}`;
    if (read.kind === "absent")
      return yield* Effect.fail(
        new Error(`${label} is not live at the follower's tip.`),
      );
    if (read.kind === "unavailable")
      return yield* Effect.fail(
        new Error(`${label} is unreadable: ${read.detail}.`),
      );
    return read;
  });

const eventDeployment = (
  contracts: SDK.MidgardValidators,
  kind: "deposit" | "withdrawal",
) =>
  SDK.eventHistoryDeploymentFromContracts(
    SDK.requireEventHistoryContracts(contracts)[kind],
  );

const fetchDepositUtxoByEventId = (
  eventId: Buffer,
): Effect.Effect<
  SDK.DepositUTxO,
  SDK.LucidError | Error,
  Database | MidgardContracts
> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const { order, retained } = yield* readEventOrderById("deposit", eventId);
    return yield* SDK.orderToDepositUTxO(
      order,
      retained,
      eventDeployment(contracts, "deposit"),
    );
  });

export const fetchWithdrawalUtxoByEventId = (
  eventId: Buffer,
): Effect.Effect<
  SDK.WithdrawalUTxO,
  SDK.LucidError | Error,
  Database | MidgardContracts
> =>
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const { order, retained } = yield* readEventOrderById(
      "withdrawal",
      eventId,
    );
    return yield* SDK.orderToWithdrawalUTxO(
      order,
      retained,
      eventDeployment(contracts, "withdrawal"),
    );
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
  Database | Lucid | MidgardContracts | IntentJournal
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(config.eventId, "--deposit-event-id");
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    // S5: the plan opens before the command's first L1 read.
    const plan = yield* openPlan;
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
      plan,
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
