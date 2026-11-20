import { parentPort, workerData } from "node:worker_threads";

import type { Network } from "@lucid-evolution/lucid";

import { createShardRowWriter } from "@/commands/stress-corpus/assemble.js";
import {
  buildCorpusChain,
  type CorpusFeeParams,
  type CorpusFundingUtxo,
} from "@/commands/stress-corpus/build-chain.js";
import type { OpenLoopCorpusRow } from "@/commands/stress-open-loop.js";

export type CorpusWorkerWallet = {
  readonly seedPhrase: string;
  readonly walletId: string;
  readonly fundingUtxo: CorpusFundingUtxo;
  readonly corpusSliceId: string;
  /** Overrides the batch-wide depth for variable-depth corpus schedules. */
  readonly depth?: number;
  /**
   * Links a chain built from an already-retained terminal output to that exact
   * retained parent. The funding UTxO must be the parent's output #1.
   */
  readonly retainedParentTxHash?: string;
};

export type CorpusWorkerInput = {
  readonly shardPath: string;
  readonly walletBatch: readonly CorpusWorkerWallet[];
  readonly depth: number;
  readonly amountLovelace: string;
  readonly feeParams: {
    readonly minFeeA: string;
    readonly minFeeB: string;
  };
  readonly network: Network;
  readonly networkId: string;
  readonly maxSubmitTxCborBytes: number;
  readonly planShape: OpenLoopCorpusRow["planShape"];
  readonly terminalChangeFloorLovelace: string;
};

export type CorpusWorkerOutput =
  | {
      readonly type: "progress";
      readonly walletId: string;
      readonly rowsDone: number;
    }
  | {
      readonly type: "done";
      readonly shardPath: string;
      readonly rowCount: number;
      readonly sha256: string;
      readonly walletIds: readonly string[];
    }
  | {
      readonly type: "failure";
      readonly error: string;
    };

const post = (message: CorpusWorkerOutput): void => {
  parentPort?.postMessage(message);
};

const feeParamsFromWire = (
  feeParams: CorpusWorkerInput["feeParams"],
): CorpusFeeParams => ({
  minFeeA: BigInt(feeParams.minFeeA),
  minFeeB: BigInt(feeParams.minFeeB),
});

export const runCorpusChainWorker = async (
  input: CorpusWorkerInput,
): Promise<Extract<CorpusWorkerOutput, { readonly type: "done" }>> => {
  const writer = await createShardRowWriter(input.shardPath);
  for (const wallet of input.walletBatch) {
    const depth = wallet.depth ?? input.depth;
    const retainedParentTxHash = wallet.retainedParentTxHash;
    if (retainedParentTxHash !== undefined) {
      const normalizedParent = retainedParentTxHash.trim().toLowerCase();
      if (
        !/^[0-9a-f]{64}$/u.test(normalizedParent) ||
        wallet.fundingUtxo.txHash !== normalizedParent ||
        wallet.fundingUtxo.outputIndex !== 1
      ) {
        throw new Error(
          `retained parent for ${wallet.walletId} must be 64-byte hex and exactly match funding output #1.`,
        );
      }
    }
    if (!Number.isSafeInteger(depth) || depth <= 0) {
      throw new Error(
        `depth for ${wallet.walletId} must be a positive safe integer.`,
      );
    }
    const chain = await buildCorpusChain({
      seedPhrase: wallet.seedPhrase,
      walletId: wallet.walletId,
      fundingUtxo: wallet.fundingUtxo,
      depth,
      amountLovelace: BigInt(input.amountLovelace),
      feeParams: feeParamsFromWire(input.feeParams),
      network: input.network,
      networkId: BigInt(input.networkId),
      maxSubmitTxCborBytes: input.maxSubmitTxCborBytes,
      corpusSliceId: wallet.corpusSliceId,
      planShape: input.planShape,
      terminalChangeFloorLovelace: BigInt(input.terminalChangeFloorLovelace),
    });
    const rows = chain.rows.map((row, index) =>
      index === 0 && retainedParentTxHash !== undefined
        ? { ...row, parentTxHash: retainedParentTxHash.trim().toLowerCase() }
        : row,
    );
    if (
      retainedParentTxHash !== undefined &&
      (rows[0]?.selectedInputOutref !==
        `${retainedParentTxHash.trim().toLowerCase()}#1` ||
        rows[0]?.parentTxHash !== retainedParentTxHash.trim().toLowerCase())
    ) {
      throw new Error(
        `first continuation for ${wallet.walletId} does not spend and name its exact retained terminal parent.`,
      );
    }
    await writer.writeRows(rows);
    post({
      type: "progress",
      walletId: wallet.walletId,
      rowsDone: rows.length,
    });
  }
  const written = await writer.close();
  return {
    type: "done",
    shardPath: input.shardPath,
    rowCount: written.rowCount,
    sha256: written.sha256,
    walletIds: input.walletBatch.map((wallet) => wallet.walletId),
  };
};

if (parentPort !== null) {
  const input = (workerData as { readonly data?: unknown }).data;
  runCorpusChainWorker(input as CorpusWorkerInput)
    .then(post)
    .catch((error: unknown) => {
      post({
        type: "failure",
        error: error instanceof Error ? error.message : String(error),
      });
    });
}
