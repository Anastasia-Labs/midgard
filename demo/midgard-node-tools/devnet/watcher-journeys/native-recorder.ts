import { appendFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  admitWatcherNativeRollForwardBlock,
  startWatcherNativeChainSync,
  type WatcherConfig,
  type WatcherNativeBlockAdmission,
  type WatcherNativeChainSyncEvent,
} from "midgard-watcher";

export const startJourneyNativeRecorder = async (input: {
  directory: string;
  watcherConfig: WatcherConfig;
  binaryPath: string;
  onBlock?(block: WatcherNativeBlockAdmission): Promise<void>;
  onRollback?(
    point: Extract<
      WatcherNativeChainSyncEvent,
      { kind: "roll_backward" }
    >["point"],
  ): Promise<void>;
}) => {
  const transactions = new Map<
    string,
    {
      cbor: string;
      point: { blockHash: string; blockNo: string; slot: string };
    }
  >();
  let failure: unknown;
  const native = await startWatcherNativeChainSync({
    watcherConfig: input.watcherConfig,
    binaryPath: input.binaryPath,
    intersection: { kind: "origin" },
    startupTimeoutMs: 30_000,
    onEvent: async (event) => {
      await appendFile(
        join(input.directory, "native-chain.ndjson"),
        `${JSON.stringify(event)}\n`,
      );
      if (event.kind === "roll_backward") {
        await input.onRollback?.(event.point);
        for (const [id, { point }] of transactions) {
          if (
            event.point.kind === "origin" ||
            BigInt(point.slot) > BigInt(event.point.slot) ||
            (point.slot === event.point.slot &&
              point.blockHash !== event.point.blockHash)
          )
            transactions.delete(id);
        }
        return;
      }
      const block = admitWatcherNativeRollForwardBlock(event);
      await input.onBlock?.(block);
      const point = {
        blockHash: block.blockHash,
        blockNo: block.blockNo,
        slot: block.slot,
      };
      block.transactionIds.forEach((id, index) =>
        transactions.set(id, { cbor: block.transactionCbors[index]!, point }),
      );
    },
  });
  void native.done.catch((cause) => {
    failure = cause;
  });
  const assertHealthy = () => {
    if (failure !== undefined)
      throw new Error("Independent native recorder failed", { cause: failure });
  };
  return {
    assertHealthy,
    transaction: async (txHash: string) => {
      const deadline = Date.now() + 90_000;
      for (;;) {
        assertHealthy();
        const transaction = transactions.get(txHash);
        if (transaction !== undefined) return transaction;
        if (Date.now() >= deadline)
          throw new Error(`Native node did not include transaction ${txHash}`);
        await pause(250);
      }
    },
    close: native.close,
  };
};
