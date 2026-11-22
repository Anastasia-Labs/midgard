import { AsyncLocalStorage } from "node:async_hooks";

import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { isReplayLocalFault } from "@al-ft/midgard-fault-proofs/block-replay";
import type { LedgerAt } from "@al-ft/midgard-fault-proofs/ledger-at";
import type { LedgerStore } from "@al-ft/midgard-fault-proofs/ledger-store";
import {
  createReplayedLedger,
  type ReplayedLedgerRefusal,
  type ReplayedLedgerSource,
} from "@al-ft/midgard-fault-proofs/replayed-ledger";
import {
  type RetainedPayloadReleases,
  retainedPayloadReplaySource,
} from "@al-ft/midgard-fault-proofs/retained-payload-replay-source";
import {
  isRetainedDaPayloadUnavailableError,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs/transition-trace/fetch";
import {
  FraudProofL1CheckpointChangedError,
  isFraudProofL1WaitError,
} from "@al-ft/midgard-fault-proofs/workflow/l1-source";
import * as SDK from "@al-ft/midgard-sdk/da";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import type { WatcherFollowerReplaySource } from "../l1-follower/replay-source.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.parse-trust-roots.js";
import { watcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.verify-watcher-user-event-script-binding.js";
import { watcherDeploymentAvailabilityChallengeAuthority } from "../runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
import type { WatcherDecisionReadiness } from "../runtime/watcher-runtime.decision-driver.js";
import { replayLocalFaultHolds } from "./replay-local-fault-bound.js";
import type { WatcherReplayLostRoots } from "./replay-lost-roots.js";
import type {
  WatcherReplayRefusalPhase,
  WatcherReplayRefusals,
} from "./replay-refusals.js";

export type WatcherReplayedLedger = Readonly<{
  ledgerAt(
    sources: readonly RetainedDaPayloadSource[],
    retries?: number,
  ): LedgerAt;
  advance(
    observation: WatcherAuthenticatedStateQueueObservation,
    sources: readonly RetainedDaPayloadSource[],
  ): Promise<readonly WatcherDecisionReadiness[]>;
  /** The headers a replay asked a payload for whose ledger the store has not
   * been found to hold, while the follower still shows them
   * (`registerWatcherDaFetcher`). */
  mayStillFetch(): Iterable<string>;
}>;

/**
 * A replay failure that says only that its L1 or DA source has not answered
 * yet: the follower or node is not ready, the chain moved under the read, or
 * no retained-DA peer served the payload yet. A replay host holds it as
 * pending and replays again; every other failure stands.
 */
export const isWatcherReplaySourcePending = (error: unknown): boolean =>
  isWatcherL1TransientFailure(error) ||
  isFraudProofL1WaitError(error) ||
  isRetainedDaPayloadUnavailableError(error);

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

/**
 * One keeper per production application, never one per classification. The
 * ledgers live in `store`, named by root; each was checked against its
 * header's root before it was persisted, so a follower rewind cannot make
 * one wrong and the keeper outlives it. Only an operation the rewind
 * interrupts is fenced: it fails with `FraudProofL1CheckpointChangedError`.
 */
export const createWatcherReplayedLedger = (
  input: Readonly<{
    follower: WatcherFollowerReplaySource;
    l1SlotConfig: L1SlotConfig;
    /** The deployment's ledger store (`openSqliteLedgerStorage`). */
    store: LedgerStore;
    /** The deployment's availability commitment authority. */
    availabilityAuthority: () => SDK.DaPayloadAvailabilityAuthority;
    /** The local-fault holds' clock, in ms (default `Date.now`). */
    nowMs?: () => number;
    /** Where the keeper's refusals are recorded (`replay-refusals.ts`). */
    refusals?: WatcherReplayRefusals;
    /** Where ledgers the store lost are recorded (`replay-lost-roots.ts`). */
    lostRoots?: WatcherReplayLostRoots;
  }>,
): WatcherReplayedLedger => {
  // A refusal met while ensuring the confirmed head's ledger runs in that
  // advance's async context; any other is a classification's replay.
  const phase = new AsyncLocalStorage<WatcherReplayRefusalPhase>();
  const { refusals, lostRoots, follower } = input;
  // The headers a replay asked a payload for. One leaves once its ledger is
  // found held, or once the follower no longer shows it (a rolled-back
  // header, or one pruned after it left the window).
  const asked = new Set<string>();
  const keeper = createReplayedLedger({
    store: input.store,
    ...(refusals === undefined
      ? {}
      : {
          onRefusal: (refusal: ReplayedLedgerRefusal) =>
            refusals.record(refusal, phase.getStore() ?? "classification"),
        }),
    onHeld: (headerHash, root) => {
      asked.delete(headerHash);
      lostRoots?.held(root);
    },
  });
  // Unbound copies a replay refused, kept across operations so a later fetch
  // asks past the peers that served them.
  const releases: RetainedPayloadReleases = new Map();
  const localFaults = replayLocalFaultHolds(input.nowMs);
  const source = (
    sources: readonly RetainedDaPayloadSource[],
    retries?: number,
  ): ReplayedLedgerSource => {
    // Each payload is checked against the availability commitment its
    // block's node carries; one no commitment binds yet only waits.
    const retained = retainedPayloadReplaySource({
      sources,
      ...(retries === undefined ? {} : { retries }),
      availability: {
        status: follower.availabilityStatus,
        authority: input.availabilityAuthority,
        releases,
      },
    });
    return {
      headerAt: follower.headerAt,
      payloadAt: (headerHash) => {
        asked.add(headerHash);
        return retained.payloadAt(headerHash);
      },
      materialFor: follower.materialFor,
      l1SlotConfig: input.l1SlotConfig,
      binding: retained.binding,
    };
  };
  /** Keeps the window's ledgers and lets go of headers the follower no
   * longer shows. */
  const retain = async (confirmedRoot: string) => {
    const { blocks, newestRoot } = await follower.ledgerWindow();
    const window = {
      blocks,
      newestRoot: newestRoot ?? confirmedRoot,
      confirmedRoot,
    };
    const notHeld = await keeper.retain(window);
    const kept = new Set([window.newestRoot, confirmedRoot]);
    for (const block of blocks) {
      kept.add(block.utxosRoot);
      kept.add(block.prevUtxosRoot);
    }
    lostRoots?.retained(kept, notHeld);
    for (const headerHash of [...asked])
      if ((await follower.headerAt(headerHash)) === undefined)
        asked.delete(headerHash);
  };
  return {
    mayStillFetch: () => asked.values(),
    ledgerAt: (sources, retries) => {
      const operationSource = source(sources, retries);
      return async (headerHash) => {
        const before = follower.epoch();
        const entries = await keeper.ledgerAt(operationSource)(headerHash);
        if (before !== follower.epoch())
          throw new FraudProofL1CheckpointChangedError(
            "the follower changed during ledger replay",
          );
        return entries;
      };
    },
    advance: async (observation, sources) => {
      let head: Readonly<{ headerHash: string; utxosRoot: string }> | undefined;
      try {
        const confirmed = await follower.confirmed(observation);
        if (confirmed.state === null) {
          // The checkpoint point precedes the deployment's protocol init:
          // there is no head to check, and every ledger replays from
          // genesis. Admission history stays retained from the origin.
          if (!confirmed.current())
            throw new FraudProofL1CheckpointChangedError(
              "replay checkpoint changed while it was read",
            );
          return [];
        }
        head = {
          headerHash: confirmed.state.headerHash,
          utxosRoot: confirmed.state.utxoRoot,
        };
        // A head held on a local fault waits out its backoff.
        const waiting = localFaults.pass()(head.headerHash);
        if (waiting !== undefined) return [waiting];
        const confirmedHead = head;
        const held = await phase.run("confirmed", () =>
          keeper.ensure(confirmedHead, source(sources)),
        );
        if (!confirmed.current())
          throw new FraudProofL1CheckpointChangedError(
            "replay checkpoint changed while its ledger was derived",
          );
        if (!held)
          return [
            {
              reason: "replayed_ledger_checkpoint_pending",
              detail: `ledger for confirmed head ${head.headerHash} does not open its authenticated root yet`,
            },
          ];
        localFaults.progressed(head.headerHash);
        refusals?.progressed();
        // The ledger the head commits is durably held: the follower may
        // prune the admissions it needed.
        follower.checkpointDurable(confirmed.state.endTime);
        await retain(head.utxosRoot);
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
        if (!isWatcherReplaySourcePending(error)) throw error;
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
