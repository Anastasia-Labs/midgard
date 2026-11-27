import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import type { HeaderDecision } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import { StateQueueNode } from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherReleasedHeaderProof,
  WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type { classificationMissRecorder } from "./fault-decision-bridge.classification-miss.js";
import { verificationRecorder } from "./fault-decision-bridge.classification-record.js";
import { unverifiedMergedRecorder } from "./fault-decision-bridge.merged-headers.js";
import {
  authenticatedHeaderObservation,
  type BridgeDependencies,
} from "./fault-decision-bridge.selected-target.js";

const maturedAtMs = (header: WatcherStateQueueHeaderObservation): number =>
  Number(Data.from(header.stateQueueNodeCborHex, StateQueueNode).block_time) +
  MIDGARD_RETENTION_WINDOW.maturityMs;

/**
 * The finalized headers a pass never classifies: the merged ones, and those
 * matured at the L1 time of the follower's tip,
 * `block_time + block_maturity_ms <= tip`. No fraud proof against a matured
 * header can land, so classifying one changes nothing: it is skipped like a
 * merged header, its predecessor's ledger is never asked for, and no later
 * header waits on it. L1 time, never the wall clock, decides it: the tip is
 * never ahead of real time, so the skip can come late, never early.
 *
 * Released headers are recorded by `unverifiedMergedRecorder`. A matured
 * header this watcher never decided on, in this process since the last
 * rollback or in a durable fault decision, and did not already skip past its
 * horizon, is recorded `unverified_past_horizon` like a header skipped past
 * its horizon, and warned `unverified_matured` once. Its later merge is then
 * recorded `unverified_merged`, as a past-horizon header's is.
 */
export const skippedHeadersRecorder = (
  dependencies: Parameters<typeof unverifiedMergedRecorder>[0] &
    Pick<BridgeDependencies, "l1TipTimeMs">,
  misses: Pick<
    ReturnType<typeof classificationMissRecorder>,
    "skippedPastHorizon"
  >,
) => {
  const merged = unverifiedMergedRecorder(dependencies);
  const decided = new Set<string>();
  const recorded = new Set<string>();
  return Object.freeze({
    /** Records the released and the newly matured unverified headers of
     * `observation`, in queue order, and resolves to every header it skips. */
    recordAll: async (
      observation: WatcherAuthenticatedStateQueueObservation,
      released: ReadonlyMap<string, WatcherReleasedHeaderProof>,
    ): Promise<ReadonlySet<string>> => {
      const tipMs = await dependencies.l1TipTimeMs?.();
      await merged.recordAll(observation, released);
      const queued = new Set(
        observation.finalizedHeaders.map(({ headerHash }) => headerHash),
      );
      for (const headers of [decided, recorded])
        for (const headerHash of headers)
          if (!queued.has(headerHash)) headers.delete(headerHash);
      const skipped = new Set(released.keys());
      if (tipMs === undefined) return skipped;
      const unverified: [WatcherStateQueueHeaderObservation, number][] = [];
      for (const header of observation.finalizedHeaders) {
        const { headerHash } = header;
        const at = maturedAtMs(header);
        if (at > tipMs) continue;
        skipped.add(headerHash);
        if (
          !released.has(headerHash) &&
          !decided.has(headerHash) &&
          !recorded.has(headerHash) &&
          !misses.skippedPastHorizon(headerHash)
        )
          unverified.push([header, at]);
      }
      if (unverified.length === 0) return skipped;
      const faults = new Set(
        (await dependencies.readRecords()).flatMap(({ decision }) =>
          decision.decision === "fault_detected" ? [decision.headerHash] : [],
        ),
      );
      for (const [header, at] of unverified) {
        if (faults.has(header.headerHash)) continue;
        verificationRecorder(dependencies, header.headerHash).recordPastHorizon(
          await dependencies.observationDigest(
            authenticatedHeaderObservation(observation, header),
          ),
        );
        dependencies.warn?.({
          event: "unverified_matured",
          headerHash: header.headerHash,
          maturedAtMs: at.toString(),
        });
        recorded.add(header.headerHash);
      }
      return skipped;
    },
    /** The header's latest classification decision in this process. */
    decided: (headerHash: string, decision: HeaderDecision): void => {
      decided.add(headerHash);
      merged.decided(headerHash, decision);
    },
    /** A rollback may return a header to the queue: it is recorded afresh. */
    reset: (): void => {
      decided.clear();
      recorded.clear();
      merged.reset();
    },
  });
};
