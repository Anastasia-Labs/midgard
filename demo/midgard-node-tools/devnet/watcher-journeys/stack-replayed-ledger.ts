import type { LedgerAt } from "@al-ft/midgard-fault-proofs";
import type { WatcherConfig } from "midgard-watcher";

import { readStackJourneyWatcherConfig } from "./public-da-preflight.js";
import { openJourneyReplayedLedger } from "./replayed-ledger.js";
import type { StackJourneyContext } from "./stack-context.js";

type ReplayedLedger = Readonly<{
  ledgerAt: LedgerAt;
  close(): Promise<void>;
}>;

/**
 * The stack watcher's configuration as the journey replays with it on the
 * host: the stack's own L1 origin, DA peers and request bounds, read through
 * the host's end of the stack node's socket.
 */
export const stackJourneyReplayWatcherConfig = (
  watcherConfig: WatcherConfig,
  hostSocketPath: string,
): WatcherConfig => ({
  ...watcherConfig,
  l1: {
    ...watcherConfig.l1,
    source: {
      ...watcherConfig.l1.source,
      chainSync: {
        ...watcherConfig.l1.source.chainSync,
        socketPath: hostSocketPath,
      },
    },
  },
});

/**
 * A replayed ledger opened on its first read, so a session that stages
 * nothing opens no follower; closing closes it once opened. A failed open
 * is not kept: the next read opens again, and close has nothing to close.
 */
export const lazyReplayedLedger = (
  open: () => Promise<ReplayedLedger>,
): ReplayedLedger => {
  let opened: Promise<ReplayedLedger> | undefined;
  let closed = false;
  const opening = () => {
    const attempt = open();
    attempt.catch(() => {
      if (opened === attempt) opened = undefined;
    });
    return attempt;
  };
  return {
    ledgerAt: async (headerHash) => {
      if (closed) throw new Error("Journey replay host is closed");
      return (await (opened ??= opening())).ledgerAt(headerHash);
    },
    close: async () => {
      closed = true;
      const ledger = await opened?.catch(() => undefined);
      await ledger?.close();
    },
  };
};

/**
 * An attached stack journey's independently replayed ledger: a read-only
 * follower of the host's stack node and the stack's public DA, replaying as
 * the stack's watcher does. The journey's staging reads its ledgers from it.
 */
export const openStackJourneyReplayedLedger = (
  context: StackJourneyContext,
  directory: string,
): ReplayedLedger =>
  lazyReplayedLedger(async () =>
    openJourneyReplayedLedger({
      authority: context.release,
      blueprintPath: context.deployment.blueprintPath,
      watcherConfig: stackJourneyReplayWatcherConfig(
        await readStackJourneyWatcherConfig(context.watcher.processConfigPath),
        context.layout.node.socketPath,
      ),
      binaryPath: context.layout.node.binaryPath,
      directory,
      checkpointDirectory: directory,
      networkMagic: context.customNetwork.networkMagic,
    }),
  );
