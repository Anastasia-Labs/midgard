import type { TransportReadiness } from "@al-ft/l1-node-transport";

import type { Intervention } from "../types.js";
import { type FollowStatus, readinessOf } from "./status.js";

/** The L1 node's unreadiness as the status reports it: null when ready. */
export const nodeOf = (readiness: TransportReadiness): FollowStatus["node"] =>
  readiness.ready
    ? null
    : { reason: readiness.reason, detail: readiness.detail };

/**
 * The follow loop's status before its first step. `hold` is the R3 the
 * configured origin already implies (`recordedOriginHold`): the store
 * follows from that origin or the loop stops (`origin_mismatch`), so the
 * first status reports it, before any status reads at the tip.
 */
export const initialFollowStatus = (
  readiness: TransportReadiness,
  hold: Intervention | null,
): FollowStatus => {
  const initial: Omit<FollowStatus, "readiness"> = {
    state: "starting",
    interventions:
      hold === null ? [] : [{ reason: hold.reason, detail: hold.detail }],
    waiting: null,
    stuck: null,
    protocolInit: hold === null ? "unknown" : "pending",
    cursor: null,
    node: nodeOf(readiness),
    tip: null,
    nodeBehind: null,
    atTip: false,
    replaying: false,
    events: 0,
    lastError: null,
    prune: {
      steps: 0,
      prunedThroughSlot: null,
      lastError: null,
      floorLags: [],
      failures: 0,
    },
  };
  return { ...initial, readiness: readinessOf(initial) };
};
