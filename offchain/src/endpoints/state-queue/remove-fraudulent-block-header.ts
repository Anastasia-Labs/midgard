import { makeReturn } from "@/core";
import {
  LucidEvolution,
  TransactionError,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type RemoveParams = {};

export const remove = (
  lucid: LucidEvolution,
  params: RemoveParams
): Promise<TxSignBuilder> => {
  return makeReturn(removeProgram(lucid, params)).unsafeRun();
};

const removeProgram = (
  lucid: LucidEvolution,
  params: RemoveParams
): Effect.Effect<TxSignBuilder, TransactionError, never> =>
  Effect.gen(function* () {
    const tx = yield* lucid.newTx().completeProgram();
    return tx;
  });
