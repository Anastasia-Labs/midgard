import {
  depth,
  type DepthParameters,
  isFinal,
  isSafe,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import type { CommitteeTickResult } from "./committee-service.ingest-da-conflict-evidence.js";
import type { CommitteeConfig } from "./config.js";
import type { StateQueueHeaderRecord } from "./domain.js";
import type { OnChainDaParams } from "./l1/da-attestation-reader.js";
import type { LandedQueueNode } from "./l1/follower/landed-queue.js";
import type { CommitteeView, QueueExit } from "./l1/follower/projection.js";
import { stateQueueNodeOf } from "./l1/follower/queue-derivation.js";
import type { CommitteeStore } from "./store.js";

/** The committee read the header from its L1 follower's facts. */
export const L1_FOLLOWER_SOURCE = "l1_follower";

/**
 * A terminal record's chain point: the final block that spent the header's
 * last queue output. Retention releases a payload only on this source.
 */
export const TERMINAL_TRANSITION_SOURCE =
  "authenticated_state_queue_transition_v1";

/** The stored records contradict the facts: only store corruption explains it. */
export const STORE_INTEGRITY = "store_integrity";
/** The landed queue is not one list from one root. */
export const L1_STATE_QUEUE_UNHEALTHY = "l1_state_queue_unhealthy";
/** The follower store holds no cursor yet. */
export const L1_FOLLOWER_NOT_INITIALIZED = "l1_follower_not_initialized";
/** The on-chain DA params differ from the configured ones. */
export const L1_DA_PARAMS_MISMATCH = "l1_da_params_mismatch";
/** The on-chain DA params could not be read this tick. */
export const L1_DA_PARAMS_UNAVAILABLE = "l1_da_params_unavailable";
/** A rollback crossed the retirement floor, or one was recorded before. */
export const RETIREMENT_FLOOR_BREACHED = "retirement_floor_breached";

/**
 * The record of a header in the landed queue, as the committee stores and
 * decides on it. `finalized` means signable: the queue is healthy and the
 * node's output is safe (depth ≥ cd, plan §9). Null for a node whose datum
 * does not decode, which the queue derivation never lands as a node.
 */
export const headerRecordOf = (
  node: LandedQueueNode,
  view: CommitteeView,
  parameters: DepthParameters,
  deploymentFingerprint: string,
  now: string,
): StateQueueHeaderRecord | null => {
  const decoded = stateQueueNodeOf(node.datumHex);
  if (decoded === null) return null;
  const atDepth = depth(view.at.height, node.createdHeight);
  const finalized = view.queue.healthy && isSafe(atDepth, parameters);
  const blockHash = view.nodeBlocks.get(node.createdSlot);
  return {
    deploymentFingerprint,
    headerHash: node.headerHash,
    stateQueueOutRef: node.outRef,
    blockAssetName: node.assetName,
    rawStateQueueDatumCbor: node.datumHex,
    header: decoded.header,
    computedHeaderHash: node.headerHash,
    daAttestation: decoded.da_attestation,
    observedChainPoint: {
      slot: node.createdSlot,
      ...(blockHash === undefined ? {} : { blockHash }),
      blockHeight: node.createdHeight,
      depth: atDepth,
      finalized,
      providerSource: L1_FOLLOWER_SOURCE,
      observedAt: now,
    },
    finalized,
    status: node.status,
    validationErrors: [...node.problems],
    updatedAt: now,
  };
};

/**
 * The terminal record of a stored header whose exit from the queue is final
 * (depth > k), or null while the exit could still be rolled back.
 */
export const terminalRecordOf = (
  stored: StateQueueHeaderRecord,
  exit: QueueExit,
  parameters: DepthParameters,
  now: string,
): StateQueueHeaderRecord | null =>
  isFinal(exit.depth, parameters)
    ? {
        ...stored,
        status: exit.status,
        finalized: true,
        observedChainPoint: {
          slot: exit.slot,
          blockHash: exit.blockHash,
          blockHeight: exit.blockHeight,
          depth: exit.depth,
          finalized: true,
          providerSource: TERMINAL_TRANSITION_SOURCE,
          observedAt: now,
        },
        updatedAt: now,
      }
    : null;

/**
 * Where a live header's L1 reconciliation stands, read from this tick's
 * record of its landed output (plan §8.2), never from the stored
 * `l1_reconcile` outbox record: that record only caches what was last
 * submitted.
 *
 * - `owed`: the output is unattested or attesting at a safe depth in a
 *   healthy queue; the member reconciles it on this tick.
 * - `attested`: the output is attested at a safe depth, not yet final. A
 *   later tick that reads the header's output unattested again reads it
 *   `owed`.
 * - `final`: the attested output is final (depth > k).
 * - `waiting`: the output is not at a safe depth, or its status is outside
 *   the submitter's scope; nothing is submitted against it.
 */
export type ReconcileStanding = "owed" | "attested" | "final" | "waiting";

export const reconcileStandingOf = (
  record: StateQueueHeaderRecord,
  parameters: Pick<DepthParameters, "securityParameter">,
): ReconcileStanding => {
  if (!record.finalized) return "waiting";
  if (record.status === "unattested" || record.status === "attesting")
    return "owed";
  if (record.status !== "attested") return "waiting";
  return isFinal(record.observedChainPoint.depth ?? 0, parameters)
    ? "final"
    : "attested";
};

/**
 * The first record whose DA status identity differs from the stored record
 * of the same output. An output's datum never changes, so the store
 * recorded something the chain never held.
 */
export const statusIdentityMismatch = (
  stored: readonly StateQueueHeaderRecord[],
  records: readonly StateQueueHeaderRecord[],
): string | undefined => {
  const byOutRef = new Map(
    stored
      .filter(({ status }) => status !== "merged" && status !== "removed")
      .map((record) => [record.stateQueueOutRef, record]),
  );
  for (const record of records) {
    const previous = byOutRef.get(record.stateQueueOutRef);
    if (previous === undefined) continue;
    const before = SDK.daAvailabilityStateQueueStatusIdentity(
      previous.daAttestation,
    );
    const after = SDK.daAvailabilityStateQueueStatusIdentity(
      record.daAttestation,
    );
    if (before !== after)
      return `state-queue status at unchanged output ${record.stateQueueOutRef}: stored=${before}, observed=${after}`;
  }
  return undefined;
};

/** Why the on-chain DA params do not back this member's config, if they do not. */
export const daParamsMismatch = (
  onChain: OnChainDaParams,
  configured: CommitteeConfig["daParams"],
): string | undefined =>
  onChain.committeeHex !== configured.committeeHex
    ? "on-chain DA committee does not match committee node config"
    : onChain.committeeSignersHash !== configured.committeeSignersHash
      ? "on-chain DA committee_signers_hash does not match committee node config"
      : onChain.threshold !== configured.threshold
        ? "on-chain DA threshold does not match committee node config"
        : undefined;

/**
 * Checks the retirement floor against the follower's chain: a floor point
 * that is no longer canonical inside the retained window was rolled back,
 * which records the breach. Returns the holding reason, if any.
 */
export const retirementFloorHold = async (
  store: Pick<CommitteeStore, "getRetirementFloor" | "recordRetirementBreach">,
  l1: Readonly<{
    cursorSlot(): number | null;
    pointStatus(
      point: Readonly<{ slot: number; blockHash: string }>,
    ): Promise<Readonly<{ kind: string }>>;
  }>,
): Promise<string | undefined> => {
  const floor = await store.getRetirementFloor();
  if (floor?.breach !== undefined)
    return `${RETIREMENT_FLOOR_BREACHED}: ${floor.breach.reason}`;
  const point = floor?.point;
  const cursorSlot = l1.cursorSlot();
  if (point === undefined || cursorSlot === null || point.slot > cursorSlot)
    return undefined;
  const status = await l1.pointStatus(point);
  if (status.kind !== "point_not_canonical") return undefined;
  const reason = `l1_source_retirement_floor_crossed:${point.slot.toString()}:${point.blockHash}`;
  await store.recordRetirementBreach(reason, {
    slot: point.slot,
    blockHash: point.blockHash,
  });
  return `${RETIREMENT_FLOOR_BREACHED}: ${reason}`;
};

/** A tick that made no decision because the L1 source holds the committee. */
export const heldTickResult = (
  reasons: readonly string[],
): CommitteeTickResult => ({
  scannedHeaders: 0,
  signedHeaders: 0,
  reconciledHeaders: 0,
  skippedHeaders: 0,
  payloadFetches: [],
  errors: [],
  held: reasons,
});
