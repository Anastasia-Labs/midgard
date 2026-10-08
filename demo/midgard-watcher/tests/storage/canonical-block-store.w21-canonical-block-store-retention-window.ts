import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core/retention-window";
import { describe, expect, it } from "vitest";

import {
  decodeWatcherCanonicalBlockStoreSnapshot,
  pruneWatcherCanonicalBlockStore,
  resolveWatcherCanonicalRetentionWindow,
  WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
  WATCHER_CANONICAL_SLOT_LENGTH_MS,
  WatcherCanonicalBlockStoreError,
  watcherCanonicalRetainUntilSlot,
  type WatcherCanonicalRetentionWindow,
  watcherCanonicalRetentionWindowFromVerifiedManifest,
} from "../../src/storage/canonical-block-store.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  FINGERPRINT,
  identityOf,
  MARKER,
} from "./canonical-block-store.config-of.js";
import {
  cloned,
  expectStoreError,
  manifestWith,
  OBSERVED_AT_SLOT,
  payloadRecord,
  storeOf,
  windowFor,
} from "./canonical-block-store.w21-canonical-block-store-hash-addressed-persistence.js";

// ---------------------------------------------------------------------------
// 2. Immutability
// ---------------------------------------------------------------------------

describe("W21 canonical block store: immutability", () => {
  it("refuses a snapshot that carries the same inputId twice", () => {
    const duplicated = {
      schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
      revision: "2",
      deploymentMarker: { ...MARKER },
      records: [cloned(payloadRecord), cloned(payloadRecord)],
    };
    expect(() =>
      decodeWatcherCanonicalBlockStoreSnapshot(
        new TextEncoder().encode(watcherCanonicalJson(duplicated)),
      ),
    ).toThrowError(WatcherCanonicalBlockStoreError);
  });
});

// ---------------------------------------------------------------------------
// 3. Retention window (Q54 binding) and prune boundaries
// ---------------------------------------------------------------------------

describe("W21 canonical block store: retention window", () => {
  it("derives the window from the manifest with the Q54 arithmetic", () => {
    const window = windowFor();
    expect(window.retentionDays).toBe(DA_TRANSPORT_LIMITS.minimumRetentionDays);
    expect(window.deployedRetentionMs).toBe(
      window.retentionDays * RETENTION_MS_PER_DAY,
    );
    expect(window.requiredRetentionMs).toBe(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    );
    expect(window.maturityMs).toBe(MIDGARD_RETENTION_WINDOW.maturityMs);
    expect(window.worstCaseProofTimeBoundMs).toBe(
      MIDGARD_RETENTION_WINDOW.maturityMs / 2,
    );
    expect(window.retentionSlots).toBe(
      window.deployedRetentionMs / WATCHER_CANONICAL_SLOT_LENGTH_MS,
    );
    expect(window.marginMs).toBeGreaterThan(0);
  });

  it("accepts the manifest floor and one day above it, and rejects one day below", () => {
    const floor = DA_TRANSPORT_LIMITS.minimumRetentionDays;
    expect(windowFor(floor).retentionDays).toBe(floor);
    expect(windowFor(floor + 1).retentionDays).toBe(floor + 1);
    expect(() => windowFor(floor - 1)).toThrowError(
      WatcherCanonicalBlockStoreError,
    );
  });

  it("fails closed on a window that cannot cover maturity plus the proof-time bound", () => {
    const shortDays = Math.floor(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs / RETENTION_MS_PER_DAY,
    );
    expect(() => windowFor(shortDays)).toThrowError(
      WatcherCanonicalBlockStoreError,
    );
    expect(() => windowFor(0)).toThrowError(WatcherCanonicalBlockStoreError);
  });

  it("rejects malformed retention values and manifest shapes", async () => {
    for (const value of [undefined, null, "15", 15.5, -15, Number.NaN]) {
      await expectStoreError(
        () =>
          watcherCanonicalRetentionWindowFromVerifiedManifest({
            manifest: manifestWith(value),
            manifestId: FINGERPRINT,
            deploymentMarker: MARKER,
          }),
        "retention_window_insufficient",
      );
    }
    await expectStoreError(
      () =>
        watcherCanonicalRetentionWindowFromVerifiedManifest({
          manifest: { da: {} },
          manifestId: FINGERPRINT,
          deploymentMarker: MARKER,
        }),
      "invalid_field",
    );
  });

  it("never accepts a caller-supplied window: the resolver verifies the identity first", async () => {
    await expect(
      Promise.resolve().then(() =>
        resolveWatcherCanonicalRetentionWindow({
          signedIdentity: {
            schemaVersion: "midgard-watcher-signed-deployment-identity-v1",
            manifest: manifestWith(9_000),
            releaseBindings: {},
            attestation: {},
          },
          policy: {} as never,
          trustRoots: [],
          durableMarker: MARKER,
        }),
      ),
    ).rejects.toThrowError();
  });

  it("refuses to prune a store under a doctored retention window", async () => {
    const backend = storeOf(payloadRecord);
    const doctored = {
      ...windowFor(),
      retentionDays: 1,
    } as WatcherCanonicalRetentionWindow;
    await expectStoreError(
      async () =>
        pruneWatcherCanonicalBlockStore({
          backend,
          deploymentIdentity: identityOf(),
          atSlot: payloadRecord.metadata.retainUntilSlot + 1,
          stillChallengeableInputIds: [],
          retentionWindow: doctored,
        }),
      "retention_window_insufficient",
    );
    expect(backend.writes).toBe(0);
  });

  it("refuses a window whose derived slot arithmetic has been tampered with", async () => {
    const doctored = {
      ...windowFor(),
      retentionSlots: 1,
    } as WatcherCanonicalRetentionWindow;
    await expectStoreError(
      () =>
        watcherCanonicalRetainUntilSlot({
          window: doctored,
          observedAtSlot: OBSERVED_AT_SLOT,
        }),
      "retention_window_insufficient",
    );
  });
});
