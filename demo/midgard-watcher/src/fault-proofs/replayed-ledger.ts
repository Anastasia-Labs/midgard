import { mkdir } from "node:fs/promises";
import { dirname } from "node:path";

import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  createReplayedLedger,
  fileLedgerCheckpointStore,
  FraudProofL1CheckpointChangedError,
  isFraudProofL1WaitError,
  isReplayLocalFault,
  isRetainedDaPayloadUnavailableError,
  type LedgerAt,
  type LedgerCheckpointStore,
  type RetainedDaPayloadSource,
  type RetainedPayloadReleases,
  retainedPayloadReplaySource,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import type { WatcherFollowerReplaySource } from "../l1-follower/replay-source.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentAvailabilityChallengeAuthority,
  watcherDeploymentProtocolScriptAuthority,
} from "../runtime/deployment-identity.js";
import type { WatcherDecisionReadiness } from "../runtime/watcher-runtime.decision-driver.js";
import { replayLocalFaultHolds } from "./replay-local-fault-bound.js";

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

/** What the committee signs a deployment's payload commitments with. */
export const watcherReplayAvailabilityAuthority =
  (identity: VerifiedWatcherDeploymentIdentity) =>
  (): SDK.DaPayloadAvailabilityAuthority => ({
    deploymentIdentity:
      watcherDeploymentProtocolScriptAuthority(identity).protocolScriptHashes
        .hubOracleMint,
    responseGeometry: SDK.availabilityResponseGeometry(
      watcherDeploymentAvailabilityChallengeAuthority(identity).parameters
        .responseGeometry,
    ),
  });

/** One keeper per production application, never one per classification. */
export const createWatcherReplayedLedger = (
  input: Readonly<{
    follower: WatcherFollowerReplaySource;
    l1SlotConfig: L1SlotConfig;
    checkpointPath: string;
    /** The deployment's availability commitment authority. */
    availabilityAuthority: () => SDK.DaPayloadAvailabilityAuthority;
    unsafeCheckpointStoreForTest?: LedgerCheckpointStore;
    /** The local-fault holds' clock, in ms (default `Date.now`). */
    nowMs?: () => number;
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
  // Unbound copies a replay refused, kept across operations so a later fetch
  // asks past the peers that served them.
  const releases: RetainedPayloadReleases = new Map();
  const localFaults = replayLocalFaultHolds(input.nowMs);
  const source = (
    sources: readonly RetainedDaPayloadSource[],
    retries?: number,
  ) => {
    // Each payload is checked against the availability commitment its
    // block's node carries; one no commitment binds yet only waits.
    const retained = retainedPayloadReplaySource({
      sources,
      ...(retries === undefined ? {} : { retries }),
      availability: {
        status: input.follower.availabilityStatus,
        authority: input.availabilityAuthority,
        releases,
      },
    });
    return {
      payloadAt: retained.payloadAt,
      materialFor: input.follower.materialFor,
      l1SlotConfig: input.l1SlotConfig,
      binding: retained.binding,
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
      let head: Readonly<{ headerHash: string; utxosRoot: string }> | undefined;
      try {
        const confirmed = await input.follower.confirmed(observation);
        head = {
          headerHash: confirmed.state.headerHash,
          utxosRoot: confirmed.state.utxoRoot,
        };
        // A head held on a local fault waits out its backoff.
        const waiting = localFaults.pass()(head.headerHash);
        if (waiting !== undefined) return [waiting];
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
        localFaults.progressed(head.headerHash);
        input.follower.checkpointDurable(confirmed.state.endTime);
        return [];
      } catch (error) {
        // This watcher's own replay failed: held under its own reason, and
        // replayed again once its backoff passed, for as long as it recurs.
        if (isReplayLocalFault(error))
          return [
            localFaults.fault(
              head?.headerHash ?? "the confirmed head",
              error.message,
            ),
          ];
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
