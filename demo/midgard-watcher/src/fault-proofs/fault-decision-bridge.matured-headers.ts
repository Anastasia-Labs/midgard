import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { StateQueueNode } from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { BridgeDependencies } from "./fault-decision-bridge.selected-target.js";

/**
 * The finalized headers a pass never classifies: the merged ones, and those
 * matured at the L1 time of the follower's tip,
 * `block_time + block_maturity_ms <= tip`. No fraud proof against a matured
 * header can land, so classifying one changes nothing: it is skipped like a
 * merged header, its predecessor's ledger is never asked for, and no later
 * header waits on it. L1 time, never the wall clock, decides it: the tip is
 * never ahead of real time, so the skip can come late, never early.
 */
export const skippedHeaders = async (
  observation: WatcherAuthenticatedStateQueueObservation,
  dependencies: Pick<BridgeDependencies, "l1TipTimeMs">,
  merged: ReadonlyMap<string, unknown>,
): Promise<ReadonlySet<string>> => {
  const skipped = new Set(merged.keys());
  const tipMs = await dependencies.l1TipTimeMs?.();
  if (tipMs === undefined) return skipped;
  for (const header of observation.finalizedHeaders) {
    const blockTimeMs = Number(
      Data.from(header.stateQueueNodeCborHex, StateQueueNode).block_time,
    );
    if (blockTimeMs + MIDGARD_RETENTION_WINDOW.maturityMs <= tipMs)
      skipped.add(header.headerHash);
  }
  return skipped;
};
