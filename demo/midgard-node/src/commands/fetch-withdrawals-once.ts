import { Effect, Ref } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { DatabaseError } from "@/database/utils/common.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
} from "@/services/index.js";
import { reconcileVisibleWithdrawalUTxOs } from "@/fibers/fetch-and-insert-withdrawal-utxos.js";
import { formatJson } from "@/commands/withdrawal-utils.js";

export type FetchWithdrawalsOnceResult = {
  readonly reconciledCount: number;
  readonly completedAt: string;
};

export const fetchWithdrawalsOnceProgram: Effect.Effect<
  FetchWithdrawalsOnceResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.HEARTBEAT_WITHDRAWAL_FETCH, Date.now());
  const result = yield* reconcileVisibleWithdrawalUTxOs();
  return {
    reconciledCount: result.reconciledCount,
    completedAt: result.completedAt.toISOString(),
  };
});

export const formatFetchWithdrawalsOnceResult = (
  result: FetchWithdrawalsOnceResult,
): string => formatJson(result);
