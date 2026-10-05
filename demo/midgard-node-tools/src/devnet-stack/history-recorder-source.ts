import { parseWatcherConfig } from "midgard-watcher";
import {
  readWatcherNativeChainSyncEventReceipt,
  startWatcherNativeChainSyncWithRetry,
  type WatcherNativeChainSyncEvent,
  watcherNativeChainSyncEventReceipt,
} from "midgard-watcher/native-chain-sync";

import { type HistoryChildActor } from "./history-child-evidence.js";
import { HistoryConfigurationRefusal } from "./history-configuration-refusal.js";
import {
  createHistoryWindowSealer,
  type HistoryWindowActor,
} from "./history-native-window-proof.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "./history-proof-deadline.js";
import { rowAt, rowFromEvent } from "./history-window-canonical.js";
import { DEFAULT_POLICY } from "./supervisor.js";
import type { createHistoryChainFollower } from "./watcher-history-chain.js";

/** The recorder's actual main stream admits and maintains its process-local window. */
export const startHistoryRecorderSource = async (input: {
  readonly actor: HistoryChildActor;
  readonly directories: readonly string[];
  readonly chain: ReturnType<typeof createHistoryChainFollower>;
  readonly watcherConfig: unknown;
  readonly binaryPath: string;
  readonly signal?: AbortSignal;
}) => {
  if (input.actor.role !== "history-recorder")
    throw new HistoryConfigurationRefusal(
      "history recorder actor role differs",
    );
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  const actor: HistoryWindowActor = {
    role: "history-recorder",
    runId: input.actor.runId,
    deploymentFingerprint: input.actor.deploymentFingerprint,
    codeStamp: input.actor.codeStamp,
    serviceSpecsDigest: input.actor.serviceSpecsDigest,
    attemptId: input.actor.attemptId,
  };
  const sealer = createHistoryWindowSealer({
    actor,
    directories: input.directories,
    watcherConfig,
    binaryPath: input.binaryPath,
  });
  let initialized = false;
  const revoke = () => {
    initialized = false;
    sealer.revoke();
  };
  const onEvent = async (event: WatcherNativeChainSyncEvent) => {
    // Both rollback and native authority loss revoke before archive mutation.
    if (event.kind === "roll_backward") revoke();
    const append = event.kind === "roll_forward" && initialized;
    try {
      const receipt = watcherNativeChainSyncEventReceipt(event);
      if (receipt === null)
        throw new Error("history recorder native acquisition is unknown");
      readWatcherNativeChainSyncEventReceipt(receipt);
      if (append) sealer.prepareAppend(event);
      const deadline = initialized
        ? null
        : historyProofDeadline(DEFAULT_POLICY.probeTimeoutMs);
      await input.chain.onEvent(event);
      if (append) {
        sealer.completeAppend();
        return;
      }
      if (event.kind === "roll_backward" && event.point.kind === "origin")
        return; // A genuinely empty archive waits for actual admitted RF0.
      if (deadline === null || historyProofRemaining(deadline) < 100)
        throw new Error("history recorder source proof deadline elapsed");
      const blockNo = input.chain.latestBlockNo();
      if (blockNo === undefined && event.kind === "roll_backward") return; // A live rewind reacquires only from the next actual admitted RF.
      if (blockNo === undefined)
        throw new Error("history recorder source checkpoint is unknown");
      const target =
        event.kind === "roll_forward"
          ? rowFromEvent(event).point
          : rowAt(input.directories, blockNo).point;
      if (historyProofRemaining(deadline) < 100)
        throw new Error("history recorder source proof deadline elapsed");
      await sealer.capture(event, target, historyProofRemaining(deadline));
      if (historyProofRemaining(deadline) === 0)
        throw new Error("history recorder source proof deadline elapsed");
      initialized = true;
    } catch (error) {
      revoke();
      throw error;
    }
  };
  let native: Awaited<ReturnType<typeof startWatcherNativeChainSyncWithRetry>>;
  try {
    native = await startWatcherNativeChainSyncWithRetry({
      binaryPath: input.binaryPath,
      watcherConfig,
      intersectionCandidates: input.chain.intersectionCandidates,
      startupTimeoutMs: 60_000,
      signal: input.signal,
      onEvent,
      onAuthorityRevoked: revoke,
    });
  } catch (error) {
    revoke();
    throw error;
  }
  void native.done.then(revoke, revoke);
  let closing: Promise<void> | undefined;
  return {
    sealer,
    done: native.done,
    close: () =>
      (closing ??= Promise.resolve().then(async () => {
        revoke();
        await native.close();
      })),
  };
};
