import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "@/database/utils/tx.js";

export type SuccessfulCommitBatch = {
  readonly txsToInsertImmutable: readonly EntryWithTimeStamp[];
  readonly blockTxHashes: readonly Buffer[];
  readonly clearMempoolTxHashes: readonly Buffer[];
};

export const buildSuccessfulCommitBatches = (
  mempoolTxs: readonly EntryWithTimeStamp[],
  mempoolTxHashes: readonly Buffer[],
  processedMempoolTxs: readonly EntryWithTimeStamp[],
  batchSize: number,
): readonly SuccessfulCommitBatch[] => {
  const allTxs: readonly EntryWithTimeStamp[] = [
    ...mempoolTxs,
    ...processedMempoolTxs,
  ];
  const allBlockHashes: readonly Buffer[] = [
    ...mempoolTxHashes,
    ...processedMempoolTxs.map((tx) => tx[TxColumns.TX_ID]),
  ];

  if (allTxs.length === 0) {
    return [];
  }

  const batches: SuccessfulCommitBatch[] = [];
  const step = Math.max(1, batchSize);

  for (let start = 0; start < allTxs.length; start += step) {
    const end = Math.min(start + step, allTxs.length);
    const clearStart = Math.min(start, mempoolTxHashes.length);
    const clearEnd = Math.min(end, mempoolTxHashes.length);

    batches.push({
      txsToInsertImmutable: allTxs.slice(start, end),
      blockTxHashes: allBlockHashes.slice(start, end),
      clearMempoolTxHashes: mempoolTxHashes.slice(clearStart, clearEnd),
    });
  }

  return batches;
};

export type CommitRootSelectionInput = {
  readonly hasTxRequests: boolean;
  readonly computedUtxoRoot: string;
  readonly computedTxRoot: string;
  readonly emptyRoot: string;
};

export const selectCommitRoots = ({
  computedUtxoRoot,
  computedTxRoot,
  emptyRoot,
}: CommitRootSelectionInput): {
  readonly utxoRoot: string;
  readonly txRoot: string;
} => {
  if (computedUtxoRoot.length > 0 && computedTxRoot.length > 0) {
    return {
      utxoRoot: computedUtxoRoot,
      txRoot: computedTxRoot,
    };
  }
  return {
    utxoRoot: emptyRoot,
    txRoot: emptyRoot,
  };
};

export const shouldAttemptLocalFinalizationRecovery = (input: {
  readonly localFinalizationPending: boolean;
  readonly hasAvailableConfirmedBlock: boolean;
}): boolean =>
  input.localFinalizationPending && input.hasAvailableConfirmedBlock;

export const shouldDeferCommitSubmission = (input: {
  readonly localFinalizationPending: boolean;
  readonly hasAvailableConfirmedBlock: boolean;
}): boolean =>
  input.localFinalizationPending && !input.hasAvailableConfirmedBlock;

export const shouldSkipIdleCommitBehindUnmergedTail = (input: {
  readonly localFinalizationPending: boolean;
  readonly stateQueueHasUnmergedTail: boolean;
  readonly mempoolTxCount: number;
  readonly processedTxCount: number;
  readonly pendingUserEventCount: number;
}): boolean =>
  !input.localFinalizationPending &&
  input.stateQueueHasUnmergedTail &&
  input.mempoolTxCount === 0 &&
  input.processedTxCount === 0 &&
  input.pendingUserEventCount === 0;

export const rootsMatchConfirmedHeader = (input: {
  readonly computedUtxoRoot: string;
  readonly computedTxRoot: string;
  readonly confirmedUtxoRoot: string;
  readonly confirmedTxRoot: string;
}): boolean =>
  input.computedUtxoRoot === input.confirmedUtxoRoot &&
  input.computedTxRoot === input.confirmedTxRoot;
