import { DA_ATTESTATION_TIMEOUT_MS } from "@al-ft/midgard-sdk";

import {
  parseWatcherStateQueueSnapshotV1,
  type WatcherStateQueueSnapshotV1,
} from "./state-queue-indexer.js";

export type WatcherAttestationTimeoutObservationV1 = Readonly<
  | {
      status: "queue_empty";
      snapshotDigest: string;
    }
  | {
      status: "attested";
      snapshotDigest: string;
      headerHash: string;
      headerEndTimeMs: string;
      daAttestationApplied: true;
    }
  | {
      status: "waiting" | "near_timeout" | "timed_out";
      snapshotDigest: string;
      headerHash: string;
      headerEndTimeMs: string;
      deadlineMs: string;
      remainingMs: string;
      daAttestationApplied: false;
    }
>;

/**
 * Derives a read-only liveness alert from the authenticated state-queue
 * indexer's digest-bound snapshot. This module intentionally exposes no
 * transaction builder, signer, wallet, provider submitter, or mutation hook.
 */
export const deriveWatcherAttestationTimeoutObservationV1 = ({
  snapshot: rawSnapshot,
  nowMs,
  alertLeadMs,
}: {
  readonly snapshot: unknown;
  readonly nowMs: bigint;
  readonly alertLeadMs: bigint;
}): WatcherAttestationTimeoutObservationV1 | null => {
  if (
    nowMs < 0n ||
    alertLeadMs <= 0n ||
    alertLeadMs >= DA_ATTESTATION_TIMEOUT_MS
  ) {
    return null;
  }
  const snapshot: WatcherStateQueueSnapshotV1 | null =
    parseWatcherStateQueueSnapshotV1(rawSnapshot);
  if (snapshot === null) return null;
  const head = snapshot.queue[0];
  if (head === undefined) {
    return {
      status: "queue_empty",
      snapshotDigest: snapshot.snapshotDigest,
    };
  }
  if (head.daAttestationPolicyId !== null) {
    return {
      status: "attested",
      snapshotDigest: snapshot.snapshotDigest,
      headerHash: head.headerHash,
      headerEndTimeMs: head.endTime,
      daAttestationApplied: true,
    };
  }
  const deadlineMs = BigInt(head.endTime) + DA_ATTESTATION_TIMEOUT_MS;
  const remainingMs = deadlineMs - nowMs;
  return {
    status:
      remainingMs <= 0n
        ? "timed_out"
        : remainingMs <= alertLeadMs
          ? "near_timeout"
          : "waiting",
    snapshotDigest: snapshot.snapshotDigest,
    headerHash: head.headerHash,
    headerEndTimeMs: head.endTime,
    deadlineMs: deadlineMs.toString(),
    remainingMs: remainingMs.toString(),
    daAttestationApplied: false,
  };
};
