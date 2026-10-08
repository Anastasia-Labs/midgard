/**
 * Canonical block/proof store (GOAL_SPEC 10.3 W21).
 *
 * Nothing in the watcher writes or reads this store: its producer, the public
 * DA client, was removed in W2b, and its persist and load functions with it.
 * What remains are the retention-window helpers the deployment authority and
 * replay transcripts use, the record and snapshot shapes and their codec, and
 * the retention prune, which only ever finds an empty store.
 *
 * Design constraints, all deliberate:
 *
 * - It is a separate durable authority. It owns its own snapshot schema over
 *   the `WatcherDurableAtomicBackend` boundary (the rollback engine's pattern)
 *   instead of widening the W03 durable store, so a block-store change cannot
 *   perturb the protocol journal's migration identity.
 * - It never transforms bytes. A record's stored `cborHex` is exactly what the
 *   peer served; the client's `inputId` remains the addressing key.
 * - It records BOTH digests explicitly. `envelopeSha256` is the digest of the
 *   stored byte string; `innerSha256` is the digest of the second, dependent
 *   byte string the artifact commits to - the unwrapped inner payload for a DA
 *   envelope, the accompanying membership witness for a trace step or an
 *   event-to-step entry - and is `null` when the artifact is atomic.
 * - It never invents a retention window. The window is derived from the signed
 *   deployment identity (`da.transportProfile.retentionDays`) and validated
 *   against the Q54 core contract; a caller-supplied window is not accepted.
 * - It is backend agnostic and restart safe. Every state change is one
 *   compare-and-swap of one complete snapshot, so a crash at any point leaves
 *   either the prior snapshot or the next one, never a partial write.
 */

import "node:crypto";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "../runtime/deployment-identity.js";
import "./durable-store.js";
import "./canonical-block-store.parse-provenance.js";
import "./canonical-block-store.parse-watcher-canonical-block-record.js";
import "./canonical-block-store.parse-watcher-canonical-block-store-snapshot.js";
import "./canonical-block-store.snapshot-codec.js";
import "./canonical-block-store.prune-watcher-canonical-block-store.js";
export {
  WATCHER_CANONICAL_BLOCK_STORE_ALERT_CODES,
  WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS,
  WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
  WATCHER_CANONICAL_CONTENT_KINDS,
  WATCHER_CANONICAL_PRUNE_REASON_CODES,
  WATCHER_CANONICAL_RECORD_KIND_BY_CONTENT_KIND,
  WATCHER_CANONICAL_SLOT_LENGTH_MS,
  type WatcherCanonicalBlockRecord,
  type WatcherCanonicalBlockStoreAlertCode,
  WatcherCanonicalBlockStoreError,
  type WatcherCanonicalBlockStoreErrorCode,
  type WatcherCanonicalBlockStoreSnapshot,
  type WatcherCanonicalContentKind,
  type WatcherCanonicalPruneReasonCode,
  type WatcherCanonicalRecordMetadata,
} from "./canonical-block-store.parse-provenance.js";
export {
  parseWatcherCanonicalBlockRecord,
  verifyWatcherCanonicalRecord,
  type WatcherCanonicalRetentionWindow,
  watcherCanonicalRetentionWindowFromVerifiedManifest,
} from "./canonical-block-store.parse-watcher-canonical-block-record.js";
export {
  assertWatcherCanonicalRetentionWindow,
  makeWatcherCanonicalDaPayloadRecord,
  makeWatcherCanonicalEventToStepRecord,
  makeWatcherCanonicalProofBundleRecord,
  makeWatcherCanonicalTraceStepRecord,
  parseWatcherCanonicalBlockStoreSnapshot,
  resolveWatcherCanonicalRetentionWindow,
  WATCHER_CANONICAL_MIN_RETENTION_DAYS,
  type WatcherCanonicalRecordContext,
  watcherCanonicalRetainUntilSlot,
} from "./canonical-block-store.parse-watcher-canonical-block-store-snapshot.js";
export { pruneWatcherCanonicalBlockStore } from "./canonical-block-store.prune-watcher-canonical-block-store.js";
export {
  decodeWatcherCanonicalBlockStoreSnapshot,
  encodeWatcherCanonicalBlockStoreSnapshot,
  makeEmptyWatcherCanonicalBlockStoreSnapshot,
  type WatcherCanonicalPruneDecision,
  type WatcherCanonicalPruneResult,
} from "./canonical-block-store.snapshot-codec.js";
