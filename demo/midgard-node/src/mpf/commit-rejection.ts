/**
 * Commit-stage rejection codes and the resolution of per-transaction deltas for commit.
 */

import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";

import * as CekProgramMaterialDB from "../database/cekProgramMaterial.js";
import * as MempoolDB from "../database/mempool.js";
import * as MempoolTxDeltasDB from "../database/mempoolTxDeltas.js";
import * as TxRejectionsDB from "../database/txRejections.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import { Database } from "../services/index.js";
import { findSpentAndProducedUTxOs } from "../utils.js";

export const COMMIT_REJECT_CODE_DECODE_FAILED = "E_COMMIT_CBOR_DESERIALIZATION";

export const COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT =
  "E_COMMIT_WITHDRAWN_REFERENCE_INPUT";

export const COMMIT_REJECT_CODE_SAME_BLOCK_DEPOSIT_INPUT =
  "E_COMMIT_SAME_BLOCK_DEPOSIT_INPUT";

export const COMMIT_REJECT_CODE_FORCED_TRANSACTION_INPUT =
  "E_COMMIT_FORCED_TRANSACTION_INPUT";

export type ResolvedTxDeltaForCommit =
  | {
      readonly _tag: "Decoded";
      readonly spent: readonly Buffer[];
      readonly produced: readonly Ledger.MinimalEntry[];
    }
  | {
      readonly _tag: "Rejected";
      readonly rejection: TxRejectionsDB.EntryNoTimestamp;
    };

export const commitTxDeltaCacheHitCounter = Metric.counter(
  "commit_tx_delta_cache_hit_total",
  {
    description:
      "Commit candidates resolved from the best-effort mempool tx-delta cache",
    bigint: true,
    incremental: true,
  },
);

export const commitTxDeltaFallbackDecodedCounter = Metric.counter(
  "commit_tx_delta_fallback_decoded_total",
  {
    description:
      "Commit candidates successfully decoded from canonical CBOR after a tx-delta cache miss",
    bigint: true,
    incremental: true,
  },
);

export const resolveTxDeltaForCommit = (
  entry: Tx.EntryWithTimeStamp,
  existingDelta: MempoolTxDeltasDB.TxDelta | undefined,
): Effect.Effect<ResolvedTxDeltaForCommit, never> =>
  Effect.gen(function* () {
    if (existingDelta !== undefined) {
      return {
        _tag: "Decoded",
        spent: existingDelta.spent.map((outRef) => Buffer.from(outRef)),
        produced: existingDelta.produced.map((deltaEntry) => ({
          [Ledger.Columns.OUTREF]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTREF],
          ),
          [Ledger.Columns.OUTPUT]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTPUT],
          ),
        })),
      };
    }

    const txId = entry[Tx.Columns.TX_ID];
    const txCbor = entry[Tx.Columns.TX];
    const decoded = yield* findSpentAndProducedUTxOs(txCbor, txId).pipe(
      Effect.either,
    );
    if (decoded._tag === "Left") {
      return {
        _tag: "Rejected",
        rejection: {
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(txId),
          [TxRejectionsDB.Columns.REJECT_CODE]:
            COMMIT_REJECT_CODE_DECODE_FAILED,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: decoded.left.message,
        },
      };
    }

    return {
      _tag: "Decoded",
      spent: decoded.right.spent,
      produced: decoded.right.produced,
    };
  });

/**
 * Applies a commit-stage rejection as one database mutation. CEK admission
 * ownership is released only if both the mempool removal and durable rejection
 * record commit successfully.
 */
export const persistCommitStageRejectedTransactions = ({
  rejectedTxHashes,
  rejectionEntries,
}: {
  readonly rejectedTxHashes: readonly Buffer[];
  readonly rejectionEntries: readonly TxRejectionsDB.EntryNoTimestamp[];
}): Effect.Effect<void, DatabaseError, Database> => {
  if (rejectedTxHashes.length === 0) return Effect.void;
  return Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* MempoolDB.clearTxs([...rejectedTxHashes]);
        yield* TxRejectionsDB.insertMany(rejectionEntries);
        yield* CekProgramMaterialDB.releaseAdmissionOwnership(rejectedTxHashes);
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      MempoolDB.tableName,
      "Failed to persist commit-stage transaction rejections",
    ),
  );
};
