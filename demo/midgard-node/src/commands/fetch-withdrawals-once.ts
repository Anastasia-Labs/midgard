import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { DatabaseError } from "../database/utils/common.js";
import { reconcileVisibleWithdrawalUTxOs } from "../fibers/fetch-and-insert-withdrawal-utxos.js";
import { Database, Lucid, MidgardContracts } from "../services/index.js";

export type FetchWithdrawalsOnceResult = {
  readonly reconciledCount: number;
  readonly completedAt: string;
};

export const fetchWithdrawalsOnceProgram: Effect.Effect<
  FetchWithdrawalsOnceResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> = Effect.gen(function* () {
  const result = yield* reconcileVisibleWithdrawalUTxOs();
  return {
    reconciledCount: result.reconciledCount,
    completedAt: result.completedAt.toISOString(),
  };
});
