import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  type CanonicalJson,
  canonicalJson,
  fail,
  HEX_32,
  immutableCanonicalJson,
  sha256Bytes,
} from "./durable-store.canonical-json.js";
import {
  immutableStoreEncodings,
  makeEmptyWatcherDurableStore,
  parseMarker,
  parseWatcherDurableStore,
  UTF8_DECODER,
  UTF8_ENCODER,
} from "./durable-store.journal-watcher-protocol-utxo-transition.js";
import {
  type WatcherDurableCaches,
  type WatcherDurableStore,
} from "./durable-store.parse-l1-observation.js";

/** Package-internal receipt for this exact, fully validated immutable store.
 * Clones and frozen containers with mutable children must take the normal path. */
export const readValidatedWatcherDurableStoreCaches = (
  value: unknown,
): WatcherDurableCaches | undefined =>
  typeof value === "object" && value !== null
    ? immutableStoreEncodings.get(value)?.caches
    : undefined;

export const encodeWatcherDurableStore = (
  value: WatcherDurableStore,
): Uint8Array => {
  const cached = immutableStoreEncodings.get(value);
  if (cached !== undefined) return UTF8_ENCODER.encode(cached.encoded);
  const encoded = canonicalJson(parseWatcherDurableStore(value));
  // Parsing above checks records, references, payload digests and rebuilt
  // caches. Retain that result only if the original input is also immutable;
  // caller-owned mutable stores must be checked again on every encoding.
  if (Object.isFrozen(value)) {
    canonicalJson(value);
    if (immutableCanonicalJson.has(value)) {
      immutableStoreEncodings.set(value, { encoded, caches: value.caches });
    }
  }
  // Never hand out a cached mutable byte buffer.
  return UTF8_ENCODER.encode(encoded);
};

export const watcherDurableStoreBytesSha256 = (value: Uint8Array): string =>
  sha256Bytes(value);

export const decodeWatcherDurableStore = (
  value: Uint8Array,
): WatcherDurableStore => {
  let text = "";
  let decoded: unknown;
  try {
    text = UTF8_DECODER.decode(value);
    decoded = JSON.parse(text) as unknown;
  } catch {
    return fail("invalid_encoding", "$");
  }
  const parsed = parseWatcherDurableStore(decoded);
  if (text !== canonicalJson(parsed as CanonicalJson)) {
    fail("noncanonical_encoding", "$");
  }
  return parsed;
};

/**
 * The backend boundary is intentionally minimal. Implementations must compare
 * the digest of the complete current byte string and durably replace the
 * complete byte string as one atomic operation. They must never expose a
 * partially written value after process or host failure.
 */
export type WatcherDurableAtomicBackend = Readonly<{
  read: () => Promise<Uint8Array | null>;
  compareAndSwap: (
    expectedSha256: string | null,
    next: Uint8Array,
    /** Optional encoding hint; a backend must check it against the exact bytes. */
    canonicalValue?: unknown,
  ) => Promise<boolean>;
}>;

export type WatcherDurableAtomicSnapshot = Readonly<{
  bytes: Uint8Array;
  sha256: string;
}>;

export type WatcherDurableAtomicCommit =
  | Readonly<{
      committed: true;
      sha256: string;
    }>
  | Readonly<{
      committed: false;
    }>;

/**
 * Reads one complete immutable backend snapshot. The returned bytes are copied
 * so a backend cannot mutate the value after the digest has been established.
 */
export const readWatcherDurableAtomicSnapshot = async (
  backend: WatcherDurableAtomicBackend,
): Promise<WatcherDurableAtomicSnapshot | null> => {
  const bytes = await readBackend(backend);
  if (bytes === null) {
    return null;
  }
  const copy = Uint8Array.from(bytes);
  return Object.freeze({
    bytes: copy,
    sha256: watcherDurableStoreBytesSha256(copy),
  });
};

/**
 * Durably replaces one complete backend snapshot iff its exact prior digest is
 * still current. A false result is an ordinary stale/concurrent-writer
 * conflict; backend failures remain explicit persistence failures.
 */
export const compareAndSwapWatcherDurableAtomicSnapshot = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly expectedSha256: string | null;
  readonly next: Uint8Array;
  readonly canonicalValue?: unknown;
}): Promise<WatcherDurableAtomicCommit> => {
  if (input.expectedSha256 !== null && !HEX_32.test(input.expectedSha256)) {
    return fail("invalid_field", "$.expectedSha256");
  }
  if (!(input.next instanceof Uint8Array) || input.next.byteLength === 0) {
    return fail("invalid_field", "$.next");
  }
  const next = Uint8Array.from(input.next);
  const sha256 = watcherDurableStoreBytesSha256(next);
  let committed: boolean;
  try {
    committed = await input.backend.compareAndSwap(
      input.expectedSha256,
      next,
      input.canonicalValue,
    );
  } catch {
    return fail("persistence_failure", "$.backend.compareAndSwap");
  }
  return committed
    ? Object.freeze({ committed: true, sha256 })
    : Object.freeze({ committed: false });
};

export type WatcherDurableMigrationResult = Readonly<{
  initialized: boolean;
  snapshot: WatcherDurableStore;
  encodedSha256: string;
}>;

const assertMarkerMatches = (
  expected: DeploymentMarker,
  actual: DeploymentMarker,
): void => {
  if (
    expected.schemaVersion !== actual.schemaVersion ||
    expected.manifestId !== actual.manifestId
  ) {
    fail("deployment_marker_mismatch", "$.deploymentMarker");
  }
};

const readBackend = async (
  backend: WatcherDurableAtomicBackend,
): Promise<Uint8Array | null> => {
  try {
    return await backend.read();
  } catch {
    return fail("persistence_failure", "$.backend.read");
  }
};

export const migrateWatcherDurableStore = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly deploymentMarker: DeploymentMarker;
  readonly maxConflicts?: number;
}): Promise<WatcherDurableMigrationResult> => {
  const expectedMarker = parseMarker(input.deploymentMarker);
  const maxConflicts = input.maxConflicts ?? 8;
  if (!Number.isSafeInteger(maxConflicts) || maxConflicts < 1) {
    fail("invalid_field", "$.maxConflicts");
  }
  for (let attempt = 0; attempt < maxConflicts; attempt += 1) {
    const current = await readBackend(input.backend);
    if (current !== null) {
      const snapshot = decodeWatcherDurableStore(current);
      assertMarkerMatches(expectedMarker, snapshot.deploymentMarker);
      return {
        initialized: false,
        snapshot,
        encodedSha256: watcherDurableStoreBytesSha256(current),
      };
    }

    const snapshot = makeEmptyWatcherDurableStore(expectedMarker);
    const encoded = encodeWatcherDurableStore(snapshot);
    let written: boolean;
    try {
      written = await input.backend.compareAndSwap(null, encoded);
    } catch {
      return fail("persistence_failure", "$.backend.compareAndSwap");
    }
    if (written) {
      return {
        initialized: true,
        snapshot,
        encodedSha256: watcherDurableStoreBytesSha256(encoded),
      };
    }
  }
  return fail("migration_conflict", "$.backend.compareAndSwap");
};
