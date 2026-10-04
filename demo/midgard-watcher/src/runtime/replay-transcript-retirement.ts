import type { WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import type { WatcherStateQueueObservationSource } from "../indexers/authenticated-state-queue-observation.js";
import { WATCHER_STATE_QUEUE_PROGRESS_INTERVAL_BLOCKS } from "../indexers/authenticated-state-queue-observation.js";
import { RELEASE_FINALITY_DEPTH } from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";
import type { WatcherReplayTranscriptStore } from "../storage/replay-transcript-store.js";
import type { WatcherChainCoordinatorHooks } from "./chain-coordinator.js";
import type { WatcherConfig } from "./config.js";
import type { createWatcherHistoryRecovery } from "./history-recovery.js";
import type { WatcherStateQueueRuntime } from "./state-queue-runtime.js";

/** Sweeps only after serialized canonical delivery, with no dependent work. */
export const watcherReplayTranscriptRetirementHooks = (input: {
  readonly recovery: ReturnType<typeof createWatcherHistoryRecovery>;
  readonly durable: WatcherDurableRuntime;
  readonly supervisor: WatcherFaultProofSupervisor;
  readonly stateQueueSource: WatcherStateQueueObservationSource;
  readonly stateQueueRuntime: WatcherStateQueueRuntime;
  readonly store: WatcherReplayTranscriptStore;
  readonly localObservationRuntime: WatcherLocalKupmiosNativeObservationRuntime;
  /** Native startup has already verified custom-network clocks against genesis. */
  readonly config: WatcherConfig;
}): WatcherChainCoordinatorHooks =>
  Object.freeze({
    ...input.recovery.hooks,
    onRollback: async (...arguments_) => {
      await input.store.resetRetirementWitnesses();
      await input.recovery.hooks.onRollback(...arguments_);
    },
    onFinalized: async (finalized) => {
      await input.recovery.hooks.onFinalized(finalized);
      const ready = () => {
        const status = input.supervisor.status();
        const finality = input.durable.readFinality();
        return !(
          input.recovery.status().pending ||
          finality.phase === "quarantined" ||
          finality.incident !== null ||
          !status.recovered ||
          status.phase !== "accepting" ||
          status.unfinishedObjectiveCount !== 0 ||
          status.queuedJobCount !== 0 ||
          status.activeJob !== null ||
          status.blockedJob !== null ||
          finality.finalized === null
        );
      };
      if (!ready()) {
        await input.store.resetRetirementWitnesses();
        return;
      }
      // Ordinary quiet deliveries can precede the next durable checkpoint.
      // Deferring a sweep is not a discontinuity in admitted native history.
      if (
        BigInt(finalized.nativeBlock.blockNo) >
        BigInt(input.durable.readFinality().finalized!.blockNo)
      )
        return;
      let observation =
        input.stateQueueSource.latestFinalizedObservation?.() ??
        input.stateQueueRuntime.current();
      if (
        observation.nativePoint.blockHash !== finalized.nativeBlock.blockHash &&
        BigInt(finalized.nativeBlock.blockNo) -
          BigInt(observation.nativePoint.blockNo) >=
          BigInt(WATCHER_STATE_QUEUE_PROGRESS_INTERVAL_BLOCKS)
      ) {
        const previous = input.stateQueueRuntime.current();
        if (
          BigInt(previous.nativePoint.blockNo) >=
          BigInt(finalized.nativeBlock.blockNo)
        )
          return;
        const localObservation = await input.localObservationRuntime.observe({
          block: finalized.nativeBlock,
          depth: RELEASE_FINALITY_DEPTH.toString(),
        });
        if (!ready() || input.stateQueueRuntime.current() !== previous) {
          await input.store.resetRetirementWitnesses();
          return;
        }
        await input.stateQueueSource.observe({
          nativeBlock: finalized.nativeBlock,
          localObservation,
          previous,
        });
        if (!ready() || input.stateQueueRuntime.current() !== previous) {
          await input.store.resetRetirementWitnesses();
          return;
        }
        observation =
          input.stateQueueSource.latestFinalizedObservation?.() ?? previous;
      }
      // Replay/catch-up may deliver an older point than the durable anchor. Never
      // replace that point with wall time or an unverified newest database row.
      if (
        observation.nativePoint.blockHash !== finalized.nativeBlock.blockHash ||
        observation.nativePoint.slot !== finalized.nativeBlock.slot ||
        observation.nativePoint.blockNo !== finalized.nativeBlock.blockNo
      )
        return;
      await input.store.retireExpired({
        observation,
        network: input.config.targetNetwork,
        ...(input.config.customNetwork === undefined
          ? {}
          : { customSlotConfig: input.config.customNetwork.slotConfig }),
      });
    },
  });
