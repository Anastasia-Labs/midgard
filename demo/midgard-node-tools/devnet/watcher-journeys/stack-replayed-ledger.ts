import type { LedgerAt } from "@al-ft/midgard-fault-proofs";
import type { WatcherConfig } from "midgard-watcher";

import {
  type JourneyReplayStoreIdentity,
  openJourneyReplayStore,
  requireJourneyReplayStore,
} from "./journey-replay-store.js";
import { warmJourneyReplayStore } from "./journey-replay-warm-up.js";
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

/** What the run's replay store follows: the stack's deployment, from its origin. */
const stackJourneyReplayIdentity = (
  context: StackJourneyContext,
  watcherConfig: WatcherConfig,
): JourneyReplayStoreIdentity => ({
  deploymentManifestId: context.release.deploymentIdentity.manifestId,
  blueprintHash: context.release.deploymentIdentity.blueprintHash,
  origin: watcherConfig.l1.origin ?? null,
});

/** The stack's replayed ledger, keeping its follower store in `directory`. */
const openStackReplayedLedgerIn = (
  context: StackJourneyContext,
  watcherConfig: WatcherConfig,
  directory: string,
) =>
  openJourneyReplayedLedger({
    authority: context.release,
    blueprintPath: context.deployment.blueprintPath,
    watcherConfig: stackJourneyReplayWatcherConfig(
      watcherConfig,
      context.layout.node.socketPath,
    ),
    binaryPath: context.layout.node.binaryPath,
    directory,
    checkpointDirectory: directory,
    networkMagic: context.customNetwork.networkMagic,
  });

/**
 * An attached stack journey's independently replayed ledger: a read-only
 * follower of the host's stack node and the stack's public DA, replaying as
 * the stack's watcher does. The journey's staging reads its ledgers from it.
 * It keeps its follower store in the run's replay store, which the warm-up
 * command fills before the drill (`warmStackJourneyReplay`): opening refuses
 * a run whose store the warm-up has not filled for this deployment, so a
 * family follows on from where the last stopped and never walks L1 from the
 * stack's origin. It holds the store from its first read until it closes.
 */
export const openStackJourneyReplayedLedger = async (
  context: StackJourneyContext,
): Promise<ReplayedLedger> => {
  const { deploymentIdentity } = context.release;
  await requireJourneyReplayStore(context.runDirectory, {
    deploymentManifestId: deploymentIdentity.manifestId,
    blueprintHash: deploymentIdentity.blueprintHash,
  });
  return lazyReplayedLedger(async () => {
    const watcherConfig = await readStackJourneyWatcherConfig(
      context.watcher.processConfigPath,
    );
    const store = await openJourneyReplayStore(
      context.runDirectory,
      stackJourneyReplayIdentity(context, watcherConfig),
    );
    try {
      const ledger = await openStackReplayedLedgerIn(
        context,
        watcherConfig,
        store.directory,
      );
      return {
        ledgerAt: ledger.ledgerAt,
        close: async () => {
          try {
            await ledger.close();
          } finally {
            store.release();
          }
        },
      };
    } catch (error) {
      store.release();
      throw error;
    }
  });
};

/**
 * The warm-up command's work: fills the run's replay store from the stack's
 * own replayed ledger, the one the drill then opens, driven to the node's tip.
 */
export const warmStackJourneyReplay = async (context: StackJourneyContext) => {
  const watcherConfig = await readStackJourneyWatcherConfig(
    context.watcher.processConfigPath,
  );
  return await warmJourneyReplayStore({
    runDirectory: context.runDirectory,
    identity: stackJourneyReplayIdentity(context, watcherConfig),
    openLedger: (directory) =>
      openStackReplayedLedgerIn(context, watcherConfig, directory),
  });
};
