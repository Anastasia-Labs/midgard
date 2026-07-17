/**
 * Withdrawal submission flow for creating authenticated withdrawal-order
 * events on L1.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { awaitExactTransactionConfirmation } from "@/transactions/utils.js";

export type SubmitWithdrawalReferenceScripts =
  SDK.SubmitWithdrawalReferenceScripts;
export type SubmitWithdrawalConfig = SDK.SubmitWithdrawalConfig;
export type WithdrawalBuildMetadata = SDK.WithdrawalBuildMetadata;

export class SubmitWithdrawalError extends EffectData.TaggedError(
  "SubmitWithdrawalError",
)<{
  message: string;
  cause: unknown;
}> {}

export const buildUnsignedWithdrawalTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitWithdrawalConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: WithdrawalBuildMetadata;
  },
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitWithdrawalError
> =>
  SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
    lucid,
    contracts,
    config,
  ).pipe(
    Effect.catchTag("UserEventBuildError", (error) =>
      Effect.fail(
        new SubmitWithdrawalError({
          message: error.message,
          cause: error.cause,
        }),
      ),
    ),
  );

export const submitWithdrawalProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitWithdrawalConfig,
): Effect.Effect<
  {
    readonly txHash: string;
    readonly metadata: WithdrawalBuildMetadata;
  },
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitWithdrawalError,
  never
> =>
  Effect.gen(function* () {
    const built = yield* buildUnsignedWithdrawalTxWithMetadataProgram(
      lucid,
      contracts,
      config,
    );
    const signed = yield* Effect.tryPromise({
      try: () => built.tx.sign.withWallet().complete(),
      catch: (cause) =>
        new SubmitWithdrawalError({
          message: "Failed to sign withdrawal transaction",
          cause,
        }),
    });
    const txHash = yield* Effect.tryPromise({
      try: () => signed.submit(),
      catch: (cause) =>
        new SubmitWithdrawalError({
          message: "Failed to submit withdrawal transaction",
          cause,
        }),
    });
    yield* Effect.tryPromise({
      try: () => awaitExactTransactionConfirmation(lucid, txHash),
      catch: (cause) =>
        new SubmitWithdrawalError({
          message: "Failed to confirm withdrawal transaction",
          cause,
        }),
    });
    return {
      txHash,
      metadata: built.metadata,
    };
  });
