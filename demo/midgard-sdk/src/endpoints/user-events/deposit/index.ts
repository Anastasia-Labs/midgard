import { makeReturn } from "@/core.js";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { Deposit } from "@/tx-builder/index.js";
import { Effect } from "effect";
import { HashingError, LucidError } from "@/utils/common.js";
import { DepositError } from "@/utils/index.js";

export const depositsBuilderProgram = (
  lucid: LucidEvolution,
  depositParams: Deposit.DepositParams,
): Effect.Effect<TxSignBuilder, HashingError | LucidError | DepositError> =>
  Effect.gen(function* () {
    const commitTx = yield* Deposit.depositTxBuilder(lucid, depositParams);
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: false }),
      catch: (e) =>
        new DepositError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for submitting deposits using the provided
 * `LucidEvolution` instance and a deposit config.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param depositParams - Parameters required for commiting deposits.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const depositsBuilder = (
  lucid: LucidEvolution,
  depositParams: Deposit.DepositParams,
): Promise<TxSignBuilder> =>
  makeReturn(depositsBuilderProgram(lucid, depositParams)).unsafeRun();

export * from "./fetch.js";
