import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Context, Effect, Option } from "effect";

import {
  type IntentJournal,
  type IntentPlan,
  journaledIntent,
} from "../services/intent-journal.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";

export type {
  AbsorbConfirmedDepositConfig,
  AddReserveFundsConfig,
  BuiltReservePayoutTx,
  ConcludePayoutConfig,
  InitializePayoutConfig,
  RefundInvalidWithdrawalConfig,
  ReservePayoutReferenceScripts,
} from "@al-ft/midgard-sdk";
export {
  __reservePayoutTest,
  assetsToValue,
  buildAbsorbConfirmedDepositToReserveTxProgram,
  buildAddReserveFundsToPayoutTxProgram,
  buildConcludePayoutTxProgram,
  buildInitializePayoutTxProgram,
  buildRefundInvalidWithdrawalTxProgram,
  ReservePayoutTxError,
  valueToAssets,
} from "@al-ft/midgard-sdk";

type ReservePayoutSubmitError =
  | SDK.ReservePayoutTxError
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.StateQueueError
  | TxSubmitError
  | TxConfirmError
  | TxSignError;

/** The automatic worker checkpoints a signed body before transporting it.
 * Operator commands retain their synchronous submit/confirm behavior. */
export const ReservePayoutTransport = Context.GenericTag<{
  readonly prepare: (
    tx: SDK.BuiltReservePayoutTx<unknown>["tx"],
    requiredOutputIndexes: readonly number[],
  ) => Effect.Effect<string, ReservePayoutSubmitError>;
}>("midgard/ReservePayoutTransport");

/** The reserve payout steps, as journaled workflow keys. */
type ReservePayoutStep =
  | "absorb_deposit"
  | "initialize"
  | "add_funds"
  | "conclude";

/**
 * Sends one step. `eventId` is the settled event's id CBOR, the intent's
 * content reference (the §8.4 predicate reads the event by it); `plan` is
 * the command's plan, opened before its first L1 read (S5).
 */
const send = (
  lucid: LucidEvolution,
  tx: SDK.BuiltReservePayoutTx<unknown>["tx"],
  step: ReservePayoutStep,
  eventId: Buffer,
  plan: IntentPlan,
  requiredOutputIndexes: readonly number[] = [],
  evidenceOutputIndexes: readonly number[] = requiredOutputIndexes,
): Effect.Effect<string, ReservePayoutSubmitError, IntentJournal> =>
  Effect.gen(function* () {
    const transport = yield* Effect.serviceOption(ReservePayoutTransport);
    return yield* Option.isSome(transport)
      ? transport.value.prepare(tx, evidenceOutputIndexes)
      : handleSignSubmit(
          lucid,
          tx,
          journaledIntent(
            "reserve_payout",
            `reserve_payout:${eventId.toString("hex")}:${step}`,
            plan,
            eventId,
          ),
          { requiredOutputIndexes },
        );
  });

export const submitAbsorbConfirmedDepositToReserveProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SDK.AbsorbConfirmedDepositConfig,
  plan: IntentPlan,
): Effect.Effect<string, ReservePayoutSubmitError, IntentJournal> =>
  Effect.gen(function* () {
    const built = yield* SDK.buildAbsorbConfirmedDepositToReserveTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* send(
      lucid,
      built.tx,
      "absorb_deposit",
      config.deposit.idCbor,
      plan,
      [Number(built.layout.reserveOutputIndex)],
    );
  });

export const submitInitializePayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SDK.InitializePayoutConfig,
  plan: IntentPlan,
): Effect.Effect<string, ReservePayoutSubmitError, IntentJournal> =>
  Effect.gen(function* () {
    const built = yield* SDK.buildInitializePayoutTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* send(
      lucid,
      built.tx,
      "initialize",
      config.withdrawal.idCbor,
      plan,
      [Number(built.layout.payoutOutputIndex)],
    );
  });

export const submitAddReserveFundsToPayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SDK.AddReserveFundsConfig,
  /** The withdrawal event id CBOR the payout settles. */
  eventId: Buffer,
  plan: IntentPlan,
): Effect.Effect<string, ReservePayoutSubmitError, IntentJournal> =>
  Effect.gen(function* () {
    const built = yield* SDK.buildAddReserveFundsToPayoutTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* send(lucid, built.tx, "add_funds", eventId, plan, [
      Number(built.layout.payoutOutputIndex),
    ]);
  });

export const submitConcludePayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SDK.ConcludePayoutConfig,
  /** The withdrawal event id CBOR the payout settles. */
  eventId: Buffer,
  plan: IntentPlan,
): Effect.Effect<string, ReservePayoutSubmitError, IntentJournal> =>
  Effect.gen(function* () {
    const built = yield* SDK.buildConcludePayoutTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* send(
      lucid,
      built.tx,
      "conclude",
      eventId,
      plan,
      [],
      [Number(built.layout.l1OutputIndex)],
    );
  });
