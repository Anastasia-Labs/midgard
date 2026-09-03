/**
 * Ordering decoded mempool transactions for ledger application and establishing the effective end time.
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

export const orderDecodedMempoolTxsForLedgerApplication = (
  decodedMempoolTxs: readonly DecodedMempoolTxForCommit[],
): Effect.Effect<readonly DecodedMempoolTxForCommit[], DatabaseError, never> =>
  Effect.gen(function* () {
    if (decodedMempoolTxs.length <= 1) {
      return decodedMempoolTxs;
    }

    const txByHash = new Map<string, DecodedMempoolTxForCommit>();
    const originalIndexByTxHash = new Map<string, number>();
    const producerByOutRef = new Map<string, string>();

    for (const [index, decoded] of decodedMempoolTxs.entries()) {
      const txHashHex = decoded.txHash.toString("hex");
      if (txByHash.has(txHashHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: MempoolDB.tableName,
            message:
              "Refusing to build a block because the mempool candidate contains duplicate transaction ids",
            cause: `tx_id=${txHashHex}`,
          }),
        );
      }
      txByHash.set(txHashHex, decoded);
      originalIndexByTxHash.set(txHashHex, index);

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

    const dependenciesByTxHash = new Map<string, Set<string>>();
    const dependentsByTxHash = new Map<string, Set<string>>();
    for (const txHashHex of txByHash.keys()) {
      dependenciesByTxHash.set(txHashHex, new Set());
      dependentsByTxHash.set(txHashHex, new Set());
    }

    for (const decoded of decodedMempoolTxs) {
      const txHashHex = decoded.txHash.toString("hex");
      const dependencies = dependenciesByTxHash.get(txHashHex)!;
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

        const producerTxHash = producerByOutRef.get(spentHex);
        if (producerTxHash === undefined) {
          continue;
        }
        if (producerTxHash === txHashHex) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because a mempool transaction spends an outref it also produces",
              cause: `tx_id=${txHashHex},outref=${spentHex}`,
            }),
          );
        }

        dependencies.add(producerTxHash);
        dependentsByTxHash.get(producerTxHash)!.add(txHashHex);
      }
    }

    const byOriginalIndex = (left: string, right: string) =>
      originalIndexByTxHash.get(left)! - originalIndexByTxHash.get(right)!;
    const ready = [...dependenciesByTxHash.entries()]
      .filter(([, dependencies]) => dependencies.size === 0)
      .map(([txHashHex]) => txHashHex)
      .sort(byOriginalIndex);
    const queued = new Set(ready);
    const ordered: DecodedMempoolTxForCommit[] = [];

    while (ready.length > 0) {
      const txHashHex = ready.shift()!;
      ordered.push(txByHash.get(txHashHex)!);

      for (const dependentTxHash of dependentsByTxHash.get(txHashHex)!) {
        const dependencies = dependenciesByTxHash.get(dependentTxHash)!;
        dependencies.delete(txHashHex);
        if (dependencies.size === 0 && !queued.has(dependentTxHash)) {
          ready.push(dependentTxHash);
          queued.add(dependentTxHash);
          ready.sort(byOriginalIndex);
        }
      }
    }

    if (ordered.length !== decodedMempoolTxs.length) {
      const blockedTxIds = [...dependenciesByTxHash.entries()]
        .filter(([, dependencies]) => dependencies.size > 0)
        .map(([txHashHex, dependencies]) => ({
          tx_id: txHashHex,
          depends_on: [...dependencies].sort(),
        }));
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolDB.tableName,
          message:
            "Refusing to build a block because same-block mempool dependencies are cyclic",
          cause: JSON.stringify(blockedTxIds),
        }),
      );
    }

    return ordered;
  });
