import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { StateQueue } from "@/tx-builder/index.js";
import { Effect } from "effect";
import { DataCoercionError, HashingError, LucidError } from "@/utils/common.js";

export const mergeToConfirmedStateProgram = (
  lucid: LucidEvolution,
  fetchConfig: StateQueue.FetchConfig,
  mergeParams: StateQueue.MergeParams,
): Effect.Effect<
  TxSignBuilder,
  DataCoercionError | LucidError | HashingError
> =>
  Effect.gen(function* () {
    const completedTx = yield* StateQueue.mergeTxBuilder(
      lucid,
      fetchConfig,
      mergeParams,
    );
    return yield* completedTx.completeProgram().pipe(
      Effect.mapError(
        (e) =>
          new LucidError({
            message:
              "Failed to finalize the transaction for merging oldest block into confirmed state",
            cause: e,
          }),
      ),
    );
  });

/**
 * Builds completed tx for merging the first block in queue to be merged into
 * the confirmed state.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param mergeParams - Parameters needed for building the merge transaction.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const mergeToConfirmedState = (
  lucid: LucidEvolution,
  fetchConfig: StateQueue.FetchConfig,
  mergeParams: StateQueue.MergeParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    mergeToConfirmedStateProgram(lucid, fetchConfig, mergeParams),
  ).unsafeRun();
