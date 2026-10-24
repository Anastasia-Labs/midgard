/**
 * Decoded mempool block candidates: structural refusal and the effective end time.
 */

import { Effect } from "effect";

import * as MempoolDB from "../database/mempool.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";

export type DecodedMempoolTxForCommit = {
  readonly entry: Tx.EntryWithTimeStamp;
  readonly txHash: Buffer;
  readonly txCbor: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly Ledger.MinimalEntry[];
};

export const establishEffectiveEndTimeFromDecodedMempool = (
  decodedMempoolTxs: readonly {
    readonly entry: Tx.EntryWithTimeStamp;
  }[],
  processedOnlyEndTime?: Date,
  depositOnlyEndTime?: Date,
): Date | undefined =>
  decodedMempoolTxs.at(-1)?.entry[Tx.Columns.TIMESTAMPTZ] ??
  processedOnlyEndTime ??
  depositOnlyEndTime;

/**
 * Refuses a block candidate whose decoded mempool transactions cannot form a
 * block: a repeated transaction id, an out-ref produced twice, or a
 * transaction that spends one out-ref twice or spends its own output. It
 * does not order the candidates. Phase B validates them sequentially, as on
 * Cardano, and the block commits its accepted transactions in exactly that
 * application order (`runPhaseBValidationWithPatch`).
 */
export const refuseMalformedMempoolCandidates = (
  decodedMempoolTxs: readonly DecodedMempoolTxForCommit[],
): Effect.Effect<void, DatabaseError, never> =>
  Effect.gen(function* () {
    const txHashes = new Set<string>();
    const producerByOutRef = new Map<string, string>();

    for (const decoded of decodedMempoolTxs) {
      const txHashHex = decoded.txHash.toString("hex");
      if (txHashes.has(txHashHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: MempoolDB.tableName,
            message:
              "Refusing to build a block because the mempool candidate contains duplicate transaction ids",
            cause: `tx_id=${txHashHex}`,
          }),
        );
      }
      txHashes.add(txHashHex);

      for (const produced of decoded.produced) {
        const outRefHex = produced[Ledger.Columns.OUTREF].toString("hex");
        const priorProducer = producerByOutRef.get(outRefHex);
        if (priorProducer !== undefined) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because multiple mempool transactions produce the same outref",
              cause: `outref=${outRefHex},first_tx_id=${priorProducer},duplicate_tx_id=${txHashHex}`,
            }),
          );
        }
        producerByOutRef.set(outRefHex, txHashHex);
      }
    }

    for (const decoded of decodedMempoolTxs) {
      const txHashHex = decoded.txHash.toString("hex");
      const spentByThisTx = new Set<string>();
      for (const spent of decoded.spent) {
        const spentHex = spent.toString("hex");
        if (spentByThisTx.has(spentHex)) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because a mempool transaction spends the same outref more than once",
              cause: `tx_id=${txHashHex},outref=${spentHex}`,
            }),
          );
        }
        spentByThisTx.add(spentHex);
        if (producerByOutRef.get(spentHex) === txHashHex) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because a mempool transaction spends an outref it also produces",
              cause: `tx_id=${txHashHex},outref=${spentHex}`,
            }),
          );
        }
      }
    }
  });
