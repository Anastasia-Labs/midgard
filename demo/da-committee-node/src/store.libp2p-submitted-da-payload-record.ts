import type { DaStoredPayloadRecord } from "./domain.js";
import {
  type L1ObservedDecision,
  type L1ObservedStatus,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "./store.committee-store.js";
import { knownStatus } from "./store.parse-decision-outbox-record.js";

export const libp2pSubmittedDaPayloadRecord = (args: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly payloadSchemaVersion: 1;
  readonly payloadCbor: Uint8Array;
  readonly payloadSha256: string;
  readonly receivedAt: Date;
}): DaStoredPayloadRecord => ({
  deploymentFingerprint: args.deploymentFingerprint,
  headerHash: args.headerHash,
  payloadSchemaVersion: args.payloadSchemaVersion,
  payloadCborHex: Buffer.from(args.payloadCbor).toString("hex"),
  payloadSha256: args.payloadSha256,
  sourcePeerId: "libp2p:payload-submit",
  fetchedAt: args.receivedAt.toISOString(),
  payloadFetchStatus: "available",
  // A payload-submit ACK proves retention only.  The watcher must promote
  // this to "verified" after strict inner payload/header validation.
  validationStatus: "fetched",
});

/**
 * `observation` with `status`, which for an unknown status carries the status
 * `observation` was last known by.
 */
export const withObservedStatus = (
  observation: L1ObservedDecision,
  status: L1ObservedStatus,
): L1ObservedDecision => {
  const { lastKnownStatus: _lastKnownStatus, ...rest } = observation;
  const lastKnownStatus = knownStatus(observation);
  return status === UNKNOWN_STATE_QUEUE_STATUS && lastKnownStatus !== undefined
    ? { ...rest, stateQueueStatus: status, lastKnownStatus }
    : { ...rest, stateQueueStatus: status };
};
