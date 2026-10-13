import { mkdir } from "node:fs/promises";
import { dirname } from "node:path";

import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  createReplayedLedger,
  fileLedgerCheckpointStore,
  FraudProofL1CheckpointChangedError,
  isFraudProofL1WaitError,
  isRetainedDaPayloadUnavailableError,
  type LedgerAt,
  type LedgerCheckpointStore,
  type RetainedDaPayloadSource,
  retainedPayloadReplaySource,
} from "@al-ft/midgard-fault-proofs";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import type { WatcherFollowerReplaySource } from "../l1-follower/replay-source.js";
import type { WatcherDecisionReadiness } from "../runtime/watcher-runtime.decision-driver.js";

export type WatcherReplayedLedger = Readonly<{
  ledgerAt(
    sources: readonly RetainedDaPayloadSource[],
    retries?: number,
  ): LedgerAt;
  advance(
    observation: WatcherAuthenticatedStateQueueObservation,
    sources: readonly RetainedDaPayloadSource[],
  ): Promise<readonly WatcherDecisionReadiness[]>;
}>;

/** One keeper per production application, never one per classification. */
export const createWatcherReplayedLedger = (
  input: Readonly<{
    follower: WatcherFollowerReplaySource;
    l1SlotConfig: L1SlotConfig;
    checkpointPath: string;
    unsafeCheckpointStoreForTest?: LedgerCheckpointStore;
  }>,
): WatcherReplayedLedger => {
  const checkpoints =
    input.unsafeCheckpointStoreForTest ??
    fileLedgerCheckpointStore(input.checkpointPath);
  let epoch = input.follower.epoch();
  let keeper = createReplayedLedger({ checkpoints });
  let restoredKeeper: typeof keeper | undefined;
  // L1 event IDs do not bind every admission byte. A rewind fences the
  // ephemeral derivations; only the stable root-checked file survives it.
  const currentKeeper = () => {
    if (epoch !== input.follower.epoch()) {
      epoch = input.follower.epoch();
      keeper = createReplayedLedger({ checkpoints });
    }
    return keeper;
  };
  const source = (
    sources: readonly RetainedDaPayloadSource[],
    retries?: number,
  ) => {
    const retained = retainedPayloadReplaySource({
      sources,
      ...(retries === undefined ? {} : { retries }),
    });
    return {
      payloadAt: retained.payloadAt,
      materialFor: input.follower.materialFor,
      l1SlotConfig: input.l1SlotConfig,
    };
  };
  return {
    ledgerAt: (sources, retries) => {
      const operationSource = source(sources, retries);
      return async (headerHash) => {
        const before = input.follower.epoch();
        const entries =
          await currentKeeper().ledgerAt(operationSource)(headerHash);
        if (before !== input.follower.epoch())
          throw new FraudProofL1CheckpointChangedError(
            "the follower changed during ledger replay",
          );
        return entries;
      };
    },
    advance: async (observation, sources) => {
      try {
        const confirmed = await input.follower.confirmed(observation);
        const head = {
          headerHash: confirmed.state.headerHash,
          utxosRoot: confirmed.state.utxoRoot,
        };
        const activeKeeper = currentKeeper();
        if (restoredKeeper !== activeKeeper) {
          await activeKeeper.restore(head);
          restoredKeeper = activeKeeper;
        }
        await mkdir(dirname(input.checkpointPath), {
          recursive: true,
          mode: 0o700,
        });
        if (!confirmed.current())
          throw new FraudProofL1CheckpointChangedError(
            "replay checkpoint changed during restore",
          );
        const durable = await activeKeeper.advance(head, source(sources));
        if (!confirmed.current())
          throw new FraudProofL1CheckpointChangedError(
            "replay checkpoint changed during persistence",
          );
        if (!durable)
          return [
            {
              reason: "replayed_ledger_checkpoint_pending",
              detail: `ledger for confirmed head ${head.headerHash} does not open its authenticated root yet`,
            },
          ];
        input.follower.checkpointDurable(confirmed.state.endTime);
        return [];
      } catch (error) {
        if (
          !isWatcherL1TransientFailure(error) &&
          !isFraudProofL1WaitError(error) &&
          !isRetainedDaPayloadUnavailableError(error)
        )
          throw error;
        return [
          {
            reason: "replayed_ledger_source_pending",
            detail: error instanceof Error ? error.message : String(error),
          },
        ];
      }
    },
  };
};
