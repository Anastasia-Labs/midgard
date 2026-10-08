import { type DeploymentMarker } from "@al-ft/midgard-core";

import {
  fail,
  markersMatch,
  UTF8_DECODER,
  UTF8_ENCODER,
  WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
  type WatcherCanonicalBlockStoreAlertCode,
  type WatcherCanonicalBlockStoreSnapshot,
  type WatcherCanonicalPruneReasonCode,
} from "./canonical-block-store.parse-provenance.js";
import { verifyWatcherCanonicalRecord } from "./canonical-block-store.parse-watcher-canonical-block-record.js";
import { parseWatcherCanonicalBlockStoreSnapshot } from "./canonical-block-store.parse-watcher-canonical-block-store-snapshot.js";
import {
  compareAndSwapWatcherDurableAtomicSnapshot,
  readWatcherDurableAtomicSnapshot,
  watcherCanonicalJson,
  type WatcherDurableAtomicBackend,
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
