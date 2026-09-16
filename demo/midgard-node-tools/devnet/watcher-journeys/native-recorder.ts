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
import { NativeTransactionNotIncludedError } from "midgard-watcher/tests/support/published-da-target-consumption";

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
  const nativeEvidencePath = join(input.directory, "native-chain.ndjson");
  const transactions = new Map<
    string,
    {
      cbor: string;
      point: { blockHash: string; blockNo: string; slot: string };
    }
  >();
  let failure: unknown;
  let rollbackGeneration = 0;
  let latestTip: WatcherNativeChainSyncEvent["tip"] | undefined;
  let latestBlockNo: bigint | undefined;
  const native = await startWatcherNativeChainSync({
    watcherConfig: input.watcherConfig,
    binaryPath: input.binaryPath,
    intersection: { kind: "origin" },
    startupTimeoutMs: 30_000,
    onEvent: async (event) => {
      // Revoke the discarded fork's height before any journal or recovery I/O.
      // The next admitted forward block establishes a canonical height again.
      if (event.kind === "roll_backward") {
        latestBlockNo = undefined;
        rollbackGeneration += 1;
        for (const [id, { point }] of transactions) {
          if (
            event.point.kind === "origin" ||
            BigInt(point.slot) > BigInt(event.point.slot) ||
            (point.slot === event.point.slot &&
              point.blockHash !== event.point.blockHash)
          )
            transactions.delete(id);
        }
      }
      latestTip = event.tip;
      await appendFile(nativeEvidencePath, `${JSON.stringify(event)}\n`);
      if (event.kind === "roll_backward") {
        await input.onRollback?.(event.point);
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
      latestBlockNo = BigInt(block.blockNo);
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
    nativeEvidencePath,
    assertHealthy,
    observedBlockNo: () => latestBlockNo,
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
      // The recorder replays from origin, so a transaction a resumed stage
      // already submitted is only missing once the replay has reached the tip.
      let deadline: number | undefined;
      let generation = rollbackGeneration;
      for (;;) {
        assertHealthy();
        if (watcherNativeChainSyncAuthorityDetails(native.authority) === null)
          throw new Error("Native recorder transaction authority is inactive");
        if (generation !== rollbackGeneration) {
          deadline = undefined;
          generation = rollbackGeneration;
        }
        const transaction = transactions.get(txHash);
        if (transaction !== undefined) return transaction;
        if (
          latestTip !== undefined &&
          latestTip.kind !== "origin" &&
          latestBlockNo !== undefined &&
          latestBlockNo >= BigInt(latestTip.blockNo)
        ) {
          deadline ??= Date.now() + 90_000;
          if (Date.now() >= deadline)
            throw new NativeTransactionNotIncludedError(txHash);
        } else deadline = undefined;
        await pause(250);
      }
    },
    close: native.close,
  } as const;
};
