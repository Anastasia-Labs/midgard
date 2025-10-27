import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { LucidError } from "@/utils/common.js";
import { InitializationParams, initTxBuilder } from "@/tx-builder/initialization.js";

export const initTxProgram = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const txBuilder = yield* initTxBuilder(lucid, initParams);
    return yield* Effect.tryPromise({
      try: () => txBuilder.complete({ localUPLCEval: false }),
      catch: (e) =>
        new LucidError({
          message: "Failed to build Midgard initialization transaction",
          cause: e,
        }),
    });
  });

/**
 * Builds completed tx for initializing all Midgard contracts.
 * This includes: Hub Oracle, State Queue, Settlement Queue, 
 * Registered/Active/Retired Operators, Scheduler, Escape Hatch, 
 * and Fraud Proof Catalogue.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for initializing all Midgard contracts.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const initTx = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Promise<TxSignBuilder> =>
  makeReturn(initTxProgram(lucid, initParams)).unsafeRun();