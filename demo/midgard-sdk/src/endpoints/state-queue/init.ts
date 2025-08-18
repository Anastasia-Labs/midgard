import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { initTxBuilder } from "@/tx-builder/state-queue/init.js";
import { Effect } from "effect";
import { StateQueue } from "@/tx-builder/index.js";

export const initTxProgram = (
  lucid: LucidEvolution,
  initParams: StateQueue.InitParams,
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.gen(function* () {
    const completedTx = yield* initTxBuilder(lucid, initParams);
    return yield* Effect.tryPromise({
      try: () => completedTx.complete(),
      catch: (e) => new Error(`${e}`),
    });
  });

/**
 * Builds completed tx for iniitalizing the state queue.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for menting the initialization NFT.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */

export const initTx = (
  lucid: LucidEvolution,
  initParams: StateQueue.InitParams,
): Promise<TxSignBuilder> =>
  makeReturn(initTxProgram(lucid, initParams)).unsafeRun();
