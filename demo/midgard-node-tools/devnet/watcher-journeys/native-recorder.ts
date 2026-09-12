import { appendFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  admitWatcherNativeRollForwardBlock,
  startWatcherNativeChainSync,
  type WatcherConfig,
  type WatcherNativeBlockAdmission,
  watcherNativeChainSyncAuthorityDetails,
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
  let latestTip: WatcherNativeChainSyncEvent["tip"] | undefined;
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
        latestTip = event.tip;
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
      latestTip = event.tip;
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
    tip: () => {
      assertHealthy();
      const authority = watcherNativeChainSyncAuthorityDetails(
        native.authority,
      );
      if (authority === null)
        throw new Error("Native recorder timing authority is inactive");
      const tip = latestTip ?? authority.currentTip;
      if (tip.kind === "origin")
        throw new Error("Native recorder timing tip is still origin");
      const blockNo = Number(tip.blockNo);
      if (!Number.isSafeInteger(blockNo) || blockNo < 0)
        throw new Error("Native recorder timing tip exceeds safe block bounds");
      return { blockNo, blockHash: tip.blockHash, slot: tip.slot };
    },
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
