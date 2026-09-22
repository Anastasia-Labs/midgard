import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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
  MembershipProofWithdrawalWitness,
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
    return yield* handleSignSubmit(lucid, built.tx);
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
    return yield* handleSignSubmit(lucid, built.tx);
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
    return yield* handleSignSubmit(lucid, built.tx);
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
    return yield* handleSignSubmit(lucid, built.tx);
  });
