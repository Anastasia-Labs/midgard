/**
 * Structural state-queue header and snapshot records.
 *
 * These are the canonical JSON shapes the watcher uses to carry an
 * L1-authenticated state-queue header (W22 header-root reconstruction, W24
 * Phase A) and a queue snapshot (attestation-timeout observation). `make*`
 * builds a digest-bound record; `parse*` admits one only when it is exactly
 * structural and its header hash or snapshot digest recomputes. Neither
 * performs chain observation:
 * production state-queue authority is the local node, via
 * `authenticated-state-queue-observation.ts`.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "@noble/hashes/blake2.js";
import "../storage/durable-store.js";
import "./state-queue-snapshot.header-view.js";
import "./state-queue-snapshot.parse-watcher-state-queue-header.js";
import "./state-queue-snapshot.parse-watcher-state-queue-snapshot.js";
export {
  WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
  type WatcherConfirmedState,
  type WatcherIndexedActiveOperator,
  type WatcherIndexedRetiredOperator,
  type WatcherIndexedScheduler,
  type WatcherStateQueueHeader,
  type WatcherStateQueueSnapshot,
} from "./state-queue-snapshot.header-view.js";
export {
  makeWatcherStateQueueHeader,
  parseWatcherStateQueueHeader,
} from "./state-queue-snapshot.parse-watcher-state-queue-header.js";
export {
  makeWatcherStateQueueSnapshot,
  parseWatcherStateQueueSnapshot,
} from "./state-queue-snapshot.parse-watcher-state-queue-snapshot.js";
