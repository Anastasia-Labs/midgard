import { Effect } from "effect";

import { MempoolDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Tx from "../database/utils/tx.js";
import type { ProcessMpfsConfig } from "./process-config.js";

/** Resolve the fixed commit clock and reject a source window that cannot carry the selected normal transactions. */
export const resolveMpfBlockTime = (
  config: Pick<
    ProcessMpfsConfig,
    "fixedBlockTime" | "fixedBlockEndTime" | "currentBlockStartTime"
  >,
  effectiveEndTime: Date | undefined,
  decodedMempoolTxs: readonly { readonly entry: Tx.EntryWithTimeStamp }[],
  normalTxCount: number,
): Effect.Effect<Date | undefined, DatabaseError> =>
  Effect.gen(function* () {
    const effectiveBlockTime = config.fixedBlockTime ?? effectiveEndTime;
    if (
      effectiveEndTime !== undefined &&
      effectiveBlockTime !== undefined &&
      (!Number.isSafeInteger(effectiveBlockTime.getTime()) ||
        effectiveEndTime.getTime() > effectiveBlockTime.getTime() ||
        (effectiveEndTime.getTime() < effectiveBlockTime.getTime() &&
          normalTxCount > 0))
    )
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolDB.tableName,
          message:
            "Catch-up coverage cannot exceed its commit clock or carry normal transactions",
          cause: {
            effectiveEndTime,
            effectiveBlockTime,
            transactionCount: normalTxCount,
          },
        }),
      );

    if (config.fixedBlockEndTime !== undefined) {
      const fixedEnd = config.fixedBlockEndTime.getTime();
      if (
        !Number.isSafeInteger(fixedEnd) ||
        (config.currentBlockStartTime !== undefined &&
          fixedEnd <= config.currentBlockStartTime.getTime()) ||
        decodedMempoolTxs.some(
          ({ entry }) => entry[Tx.Columns.TIMESTAMPTZ].getTime() > fixedEnd,
        )
      )
        return yield* Effect.fail(
          new DatabaseError({
            table: MempoolDB.tableName,
            message:
              "Fixed authenticated block window does not cover the selected transaction timestamps",
            cause: `fixed_end=${fixedEnd}`,
          }),
        );
    }

    return effectiveBlockTime;
  });
