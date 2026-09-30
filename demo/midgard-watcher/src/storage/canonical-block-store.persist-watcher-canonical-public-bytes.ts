import { type DeploymentMarker } from "@al-ft/midgard-core";

import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  fail,
  markersMatch,
  UTF8_DECODER,
  UTF8_ENCODER,
  WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS,
  WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
  type WatcherCanonicalBlockRecord,
  type WatcherCanonicalBlockStoreAlertCode,
  type WatcherCanonicalBlockStoreSnapshot,
  type WatcherCanonicalPruneReasonCode,
} from "./canonical-block-store.parse-provenance.js";
import {
  verifyWatcherCanonicalRecord,
  type WatcherCanonicalRetentionWindow,
} from "./canonical-block-store.parse-watcher-canonical-block-record.js";
import {
  assertWatcherCanonicalRetentionWindow,
  parseWatcherCanonicalBlockStoreSnapshot,
} from "./canonical-block-store.parse-watcher-canonical-block-store-snapshot.js";
import {
  compareAndSwapWatcherDurableAtomicSnapshot,
  readWatcherDurableAtomicSnapshot,
  watcherCanonicalJson,
  type WatcherDurableAtomicBackend,
  watcherDurableStoreBytesSha256,
  WatcherDurableStoreError,
} from "./durable-store.js";

export const encodeWatcherCanonicalBlockStoreSnapshot = (
  snapshot: WatcherCanonicalBlockStoreSnapshot,
): Uint8Array =>
  UTF8_ENCODER.encode(
    watcherCanonicalJson(parseWatcherCanonicalBlockStoreSnapshot(snapshot)),
  );

/** Decodes one snapshot, refusing anything that is not its canonical encoding. */
export const decodeWatcherCanonicalBlockStoreSnapshot = (
  bytes: Uint8Array,
): WatcherCanonicalBlockStoreSnapshot => {
  let text = "";
  let decoded: unknown;
  try {
    text = UTF8_DECODER.decode(bytes);
    decoded = JSON.parse(text) as unknown;
  } catch {
    return fail("invalid_encoding", "$");
  }
  const parsed = parseWatcherCanonicalBlockStoreSnapshot(decoded);
  if (text !== watcherCanonicalJson(parsed)) {
    fail("noncanonical_encoding", "$");
  }
  return parsed;
};

export const makeEmptyWatcherCanonicalBlockStoreSnapshot = (
  deploymentMarker: DeploymentMarker,
): WatcherCanonicalBlockStoreSnapshot =>
  parseWatcherCanonicalBlockStoreSnapshot({
    schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
    revision: "0",
    deploymentMarker: {
      schemaVersion: deploymentMarker.schemaVersion,
      manifestId: deploymentMarker.manifestId,
    },
    records: [],
  });

export const nextRevision = (revision: string): string =>
  (BigInt(revision) + 1n).toString();

// ---------------------------------------------------------------------------
// Durable authority
// ---------------------------------------------------------------------------

export type WatcherCanonicalBlockStoreLoad = Readonly<{
  snapshot: WatcherCanonicalBlockStoreSnapshot;
  snapshotSha256: string;
}>;

export const readSnapshotBytes = async (
  backend: WatcherDurableAtomicBackend,
): Promise<Readonly<{ bytes: Uint8Array; sha256: string }> | null> => {
  try {
    return await readWatcherDurableAtomicSnapshot(backend);
  } catch (error) {
    if (
      error instanceof WatcherDurableStoreError &&
      error.code === "persistence_failure"
    ) {
      return fail("persistence_failure", "$.backend.read");
    }
    throw error;
  }
};

export const commitSnapshot = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly expectedSha256: string | null;
  readonly next: Uint8Array;
}): Promise<string | null> => {
  let result: Awaited<
    ReturnType<typeof compareAndSwapWatcherDurableAtomicSnapshot>
  >;
  try {
    result = await compareAndSwapWatcherDurableAtomicSnapshot(input);
  } catch (error) {
    if (
      error instanceof WatcherDurableStoreError &&
      error.code === "persistence_failure"
    ) {
      return fail("persistence_failure", "$.backend.compareAndSwap");
    }
    throw error;
  }
  return result.committed ? result.sha256 : null;
};

export const verifySnapshot = async (
  snapshot: WatcherCanonicalBlockStoreSnapshot,
  expectedMarker: DeploymentMarker,
): Promise<WatcherCanonicalBlockStoreSnapshot> => {
  if (!markersMatch(expectedMarker, snapshot.deploymentMarker)) {
    fail("deployment_marker_mismatch", "$.deploymentMarker");
  }
  for (let index = 0; index < snapshot.records.length; index += 1) {
    await verifyWatcherCanonicalRecord(
      snapshot.records[index],
      `$.records[${String(index)}]`,
    );
  }
  return snapshot;
};

/**
 * Loads and fully re-verifies the store. Every record's digests and deployment
 * marker are re-derived from the stored bytes, so a snapshot mutated underneath
 * the process is rejected rather than trusted.
 */
export const loadWatcherCanonicalBlockStore = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly retentionWindow?: WatcherCanonicalRetentionWindow;
}): Promise<WatcherCanonicalBlockStoreLoad | null> => {
  if (input.retentionWindow !== undefined) {
    assertWatcherCanonicalRetentionWindow(input.retentionWindow);
  }
  const stored = await readSnapshotBytes(input.backend);
  if (stored === null) {
    return null;
  }
  const snapshot = decodeWatcherCanonicalBlockStoreSnapshot(stored.bytes);
  await verifySnapshot(snapshot, input.deploymentIdentity.durableMarker);
  return Object.freeze({ snapshot, snapshotSha256: stored.sha256 });
};

export type WatcherCanonicalPersistResult = Readonly<{
  committed: boolean;
  alreadyPresent: boolean;
  inputId: string;
  snapshotSha256: string;
  revision: string;
}>;

const sameRecord = (
  left: WatcherCanonicalBlockRecord,
  right: WatcherCanonicalBlockRecord,
): boolean => watcherCanonicalJson(left) === watcherCanonicalJson(right);

/**
 * Persists one public byte string exactly as received, before any verification
 * or submission decision.
 *
 * Idempotent for a byte-identical re-persist. A different byte string under an
 * existing key is a hard `content_conflict`: the store never overwrites, and
 * the only way a record leaves is `pruneWatcherCanonicalBlockStore`.
 */
export const persistWatcherCanonicalPublicBytes = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly record: WatcherCanonicalBlockRecord;
}): Promise<WatcherCanonicalPersistResult> => {
  const marker = input.deploymentIdentity.durableMarker;
  const record = await verifyWatcherCanonicalRecord(input.record, "$.record");
  if (!markersMatch(marker, record.metadata.deploymentMarker)) {
    fail("deployment_marker_mismatch", "$.record.metadata.deploymentMarker");
  }
  for (
    let attempt = 0;
    attempt < WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS;
    attempt += 1
  ) {
    const stored = await readSnapshotBytes(input.backend);
    const current =
      stored === null
        ? makeEmptyWatcherCanonicalBlockStoreSnapshot(marker)
        : await verifySnapshot(
            decodeWatcherCanonicalBlockStoreSnapshot(stored.bytes),
            marker,
          );
    const existing = current.records.find(
      (candidate) => candidate.input.inputId === record.input.inputId,
    );
    if (existing !== undefined) {
      if (!sameRecord(existing, record)) {
        fail("content_conflict", "$.record.input.inputId");
      }
      return Object.freeze({
        committed: false,
        alreadyPresent: true,
        inputId: record.input.inputId,
        snapshotSha256:
          stored === null
            ? watcherDurableStoreBytesSha256(
                encodeWatcherCanonicalBlockStoreSnapshot(current),
              )
            : stored.sha256,
        revision: current.revision,
      });
    }
    const next = parseWatcherCanonicalBlockStoreSnapshot({
      schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
      revision: nextRevision(current.revision),
      deploymentMarker: {
        schemaVersion: current.deploymentMarker.schemaVersion,
        manifestId: current.deploymentMarker.manifestId,
      },
      records: [...current.records, record].sort((left, right) =>
        left.input.inputId < right.input.inputId ? -1 : 1,
      ),
    });
    const sha256 = await commitSnapshot({
      backend: input.backend,
      expectedSha256: stored === null ? null : stored.sha256,
      next: encodeWatcherCanonicalBlockStoreSnapshot(next),
    });
    if (sha256 !== null) {
      return Object.freeze({
        committed: true,
        alreadyPresent: false,
        inputId: record.input.inputId,
        snapshotSha256: sha256,
        revision: next.revision,
      });
    }
  }
  return fail("cas_contention", "$.backend.compareAndSwap");
};

// ---------------------------------------------------------------------------
// Retention prune
// ---------------------------------------------------------------------------

export type WatcherCanonicalPruneDecision = Readonly<{
  inputId: string;
  decision: "pruned" | "retained";
  reasonCode: WatcherCanonicalPruneReasonCode;
  retainUntilSlot: number | null;
  remainingSlots: number | null;
  alertCode: WatcherCanonicalBlockStoreAlertCode | null;
}>;

export type WatcherCanonicalPruneResult = Readonly<{
  committed: boolean;
  revision: string;
  snapshotSha256: string | null;
  prunedInputIds: readonly string[];
  decisions: readonly WatcherCanonicalPruneDecision[];
  alerts: readonly WatcherCanonicalPruneDecision[];
}>;
