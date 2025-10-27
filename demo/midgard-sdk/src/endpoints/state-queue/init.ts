import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { initTxBuilder } from "@/tx-builder/state-queue/init.js";
import { Effect } from "effect";
import { StateQueue } from "@/tx-builder/index.js";
import { LucidError } from "@/utils/common.js";

export const initStateQueueProgram = (
  lucid: LucidEvolution,
  initParams: StateQueue.InitParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const completedTx = yield* initTxBuilder(lucid, initParams);
    return yield* completedTx.completeProgram().pipe(
      Effect.mapError(
        (e) =>
          new LucidError({
            message: "Failed to build state queue initialization transaction",
            cause: e,
          }),
      ),
    );
  });

/**
 * Builds completed tx for initializing the state queue.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for minting the initialization NFT.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const initStateQueue = (
  lucid: LucidEvolution,
  initParams: StateQueue.InitParams,
): Promise<TxSignBuilder> =>
  makeReturn(initStateQueueProgram(lucid, initParams)).unsafeRun();
