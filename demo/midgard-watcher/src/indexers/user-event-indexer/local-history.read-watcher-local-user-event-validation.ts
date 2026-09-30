import {
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherProtectedUserEventCheckpoint,
} from "../../storage/durable-runtime.js";
import {
  WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION,
  type WatcherUserEventCheckpoint,
  type WatcherUserEventValidation,
} from "../../storage/user-event-checkpoint.js";
import { readWatcherLocalUserEventTransition } from "./local-history.assert-watcher-local-user-event-head-current.js";
import {
  localOwner,
  localReadmissions,
  localRefuse,
  localTransitions,
  type WatcherLocalUserEventAnchor,
  type WatcherLocalUserEventHistory,
  type WatcherLocalUserEventReadmission,
  type WatcherLocalUserEventTransition,
} from "./local-history.local-history-owner.js";
import { localAnchors } from "./local-history.local-stable-snapshot.js";
import {
  commitLocalUserEventAnchor,
  readWatcherLocalUserEventAnchor,
} from "./local-history.prepare-local-user-event-anchor.js";
import { readWatcherLocalUserEventReadmission } from "./local-history.prepare-watcher-local-user-event-canonical-replay.js";
import { same, sha256Bytes } from "./policy.js";

export const acceptWatcherLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
  publication: WatcherProtectedUserEventCheckpoint,
): WatcherLocalUserEventHistory => {
  const prepared = readWatcherLocalUserEventAnchor(receipt);
  const anchor = localAnchors.get(receipt)!;
  if (localOwner(anchor.history).semanticReplay)
    return localRefuse("provisional anchor requires semantic readmission");
  const observed = readWatcherProtectedUserEventCheckpointReceipt(publication);
  if (
    !same(observed.checkpoint, prepared.nextCheckpoint) ||
    observed.payload === null ||
    sha256Bytes(observed.payload) !== prepared.nextCheckpoint.payloadDigest
  )
    return localRefuse(
      "anchor publication differs from the exact materialization",
    );
  return commitLocalUserEventAnchor(receipt);
};

/** The durable owner records semantic completion only for a live candidate
 * admitted by this module. Descriptive JSON and copied handles cannot mint it. */
export const readWatcherLocalUserEventValidation = (
  candidate: unknown,
  checkpoint: WatcherUserEventCheckpoint,
): WatcherUserEventValidation => {
  if (typeof candidate !== "object" || candidate === null)
    return localRefuse("semantic validation candidate is absent");
  const next = localTransitions.has(
    candidate as WatcherLocalUserEventTransition,
  )
    ? readWatcherLocalUserEventTransition(
        candidate as WatcherLocalUserEventTransition,
      ).nextCheckpoint
    : localAnchors.has(candidate as WatcherLocalUserEventAnchor)
      ? readWatcherLocalUserEventAnchor(
          candidate as WatcherLocalUserEventAnchor,
        ).nextCheckpoint
      : localReadmissions.has(candidate as WatcherLocalUserEventReadmission)
        ? readWatcherLocalUserEventReadmission(
            candidate as WatcherLocalUserEventReadmission,
          ).nextCheckpoint
        : localRefuse("semantic validation candidate was not admitted");
  if (!same(next, checkpoint))
    return localRefuse("semantic validation candidate checkpoint differs");
  return Object.freeze({
    schemaVersion: WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION,
    checkpointDigest: next.checkpointDigest,
    payloadDigest: next.payloadDigest,
    policyDigest: next.userEventPolicyDigest,
  });
};
