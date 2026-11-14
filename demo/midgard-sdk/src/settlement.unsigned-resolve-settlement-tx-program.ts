import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AssetError,
  DataCoercionError,
  HashingError,
  LucidError,
  makeReturn,
} from "./common.js";
import { SettlementError } from "./settlement.incomplete-attach-resolution-claim-tx-program.js";
import {
  incompleteResolveSettlementProgram,
  type ResolveSettlementParams,
} from "./settlement.incomplete-resolve-settlement-program.js";
import { completeTxWithLocalUPLCEvalProgram } from "./tx-completion.js";

export const unsignedResolveSettlementTxProgram = (
  lucid: LucidEvolution,
  params: ResolveSettlementParams,
): Effect.Effect<
  TxSignBuilder,
  HashingError | DataCoercionError | LucidError | SettlementError | AssetError
> =>
  Effect.gen(function* () {
    const resolveSettlementTx = yield* incompleteResolveSettlementProgram(
      lucid,
      params,
    );
    const completedTx: TxSignBuilder =
      yield* completeTxWithLocalUPLCEvalProgram(
        resolveSettlementTx,
        (e) =>
          new SettlementError({
            message: `Failed to build the transaction: ${String(e)}`,
            cause: e,
          }),
      );
    return completedTx;
  });

/**
 * Builds completed tx for resolving settlement using the provided
 * `LucidEvolution` instance, `ResolveSettlementParams` parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param params - Parameters required for resolving settlement.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedResolveSettlementTx = (
  lucid: LucidEvolution,
  params: ResolveSettlementParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedResolveSettlementTxProgram(lucid, params)).unsafeRun();
