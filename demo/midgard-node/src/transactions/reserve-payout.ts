import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Context, Effect, Option } from "effect";

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

const send = (
  lucid: LucidEvolution,
  tx: SDK.BuiltReservePayoutTx<unknown>["tx"],
  requiredOutputIndexes: readonly number[] = [],
  evidenceOutputIndexes: readonly number[] = requiredOutputIndexes,
) =>
  Effect.gen(function* () {
    const transport = yield* Effect.serviceOption(ReservePayoutTransport);
    return yield* Option.isSome(transport)
      ? transport.value.prepare(tx, evidenceOutputIndexes)
      : handleSignSubmit(lucid, tx, { requiredOutputIndexes });
  });

export const submitAbsorbConfirmedDepositToReserveProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SDK.AbsorbConfirmedDepositConfig,
): Effect.Effect<string, ReservePayoutSubmitError> =>
  Effect.gen(function* () {
    const built = yield* SDK.buildAbsorbConfirmedDepositToReserveTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* send(lucid, built.tx, [
      Number(built.layout.reserveOutputIndex),
    ]);
  });

export const submitInitializePayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SDK.InitializePayoutConfig,
): Effect.Effect<string, ReservePayoutSubmitError> =>
  Effect.gen(function* () {
    const built = yield* SDK.buildInitializePayoutTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* send(lucid, built.tx, [
      Number(built.layout.payoutOutputIndex),
    ]);
  });

export const submitAddReserveFundsToPayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SDK.AddReserveFundsConfig,
): Effect.Effect<string, ReservePayoutSubmitError> =>
  Effect.gen(function* () {
    const built = yield* SDK.buildAddReserveFundsToPayoutTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* send(lucid, built.tx, [
      Number(built.layout.payoutOutputIndex),
    ]);
  });

export const submitConcludePayoutProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SDK.ConcludePayoutConfig,
): Effect.Effect<string, ReservePayoutSubmitError> =>
  Effect.gen(function* () {
    const built = yield* SDK.buildConcludePayoutTxProgram(
      lucid,
      contracts,
      config,
    );
    return yield* send(
      lucid,
      built.tx,
      [],
      [Number(built.layout.l1OutputIndex)],
    );
  });
