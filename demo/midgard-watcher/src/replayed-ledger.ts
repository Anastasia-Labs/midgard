/** Supported replay host for the watcher and native watcher journeys. */
export {
  createWatcherReplayedLedger,
  type WatcherReplayedLedger,
} from "./fault-proofs/replayed-ledger.js";
export {
  watcherFollowerProjections,
  watcherRetainedDepth,
} from "./l1-follower/follower-runtime.js";
export {
  readWatcherObservation,
  type WatcherObservationAuthority,
} from "./l1-follower/observation.js";
export {
  watcherProjection,
  type WatcherProjectionDeployment,
  watcherUnitHistoryPolicies,
} from "./l1-follower/projection.js";
export { createWatcherProofRetention } from "./l1-follower/proof-retention.js";
export { createFollowerRawReads } from "./l1-follower/raw-reads.js";
export {
  createWatcherReplayRetention,
  type WatcherReplayRetention,
} from "./l1-follower/replay-retention.js";
export {
  createWatcherFollowerReplaySource,
  type WatcherFollowerReplaySource,
} from "./l1-follower/replay-source.js";
export {
  createWatcherFollowerUserEvents,
  type WatcherUserEventIdentity,
  type WatcherUserEventScripts,
} from "./l1-follower/user-events.js";
