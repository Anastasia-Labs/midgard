import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  MIDGARD_RETENTION_WINDOW,
  RETENTION_MS_PER_DAY,
} from "@al-ft/midgard-core/retention-window";
import { describe, expect, it } from "vitest";

import {
  assertWatcherCanonicalRetentionWindow,
  resolveWatcherCanonicalRetentionWindow,
  WATCHER_RETENTION_SLOT_LENGTH_MS,
  type WatcherCanonicalRetentionWindow,
  watcherCanonicalRetentionWindowFromVerifiedManifest,
  WatcherRetentionWindowError,
  type WatcherRetentionWindowErrorCode,
} from "../../src/storage/retention-window.js";

const FINGERPRINT = "1a".repeat(32);

const MARKER = makeDeploymentMarker(FINGERPRINT);

const manifestWith = (retentionDays: unknown): unknown => ({
  da: { transportProfile: { retentionDays } },
});

const windowFor = (
  retentionDays: unknown = DA_TRANSPORT_LIMITS.minimumRetentionDays,
): WatcherCanonicalRetentionWindow =>
  watcherCanonicalRetentionWindowFromVerifiedManifest({
    manifest: manifestWith(retentionDays),
    manifestId: FINGERPRINT,
    deploymentMarker: MARKER,
  });

const expectWindowError = (
  operation: () => unknown,
  code: WatcherRetentionWindowErrorCode,
): void => {
  try {
    operation();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherRetentionWindowError);
    expect((error as WatcherRetentionWindowError).code).toBe(code);
    return;
  }
  throw new Error(`Expected retention window rejection ${code}`);
};

describe("watcher retention window (Q54 binding)", () => {
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
      window.deployedRetentionMs / WATCHER_RETENTION_SLOT_LENGTH_MS,
    );
    expect(window.marginMs).toBeGreaterThan(0);
    expect(assertWatcherCanonicalRetentionWindow(window)).toBe(window);
  });

  it("accepts the manifest floor and one day above it, and rejects one day below", () => {
    const floor = DA_TRANSPORT_LIMITS.minimumRetentionDays;
    expect(windowFor(floor).retentionDays).toBe(floor);
    expect(windowFor(floor + 1).retentionDays).toBe(floor + 1);
    expectWindowError(
      () => windowFor(floor - 1),
      "retention_window_insufficient",
    );
  });

  it("fails closed on a window that cannot cover maturity plus the proof-time bound", () => {
    const shortDays = Math.floor(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs / RETENTION_MS_PER_DAY,
    );
    expectWindowError(
      () => windowFor(shortDays),
      "retention_window_insufficient",
    );
    expectWindowError(() => windowFor(0), "retention_window_insufficient");
  });

  it("rejects malformed retention values and manifest shapes", () => {
    for (const value of [undefined, null, "15", 15.5, -15, Number.NaN]) {
      expectWindowError(
        () =>
          watcherCanonicalRetentionWindowFromVerifiedManifest({
            manifest: manifestWith(value),
            manifestId: FINGERPRINT,
            deploymentMarker: MARKER,
          }),
        "retention_window_insufficient",
      );
    }
    expectWindowError(
      () =>
        watcherCanonicalRetentionWindowFromVerifiedManifest({
          manifest: { da: {} },
          manifestId: FINGERPRINT,
          deploymentMarker: MARKER,
        }),
      "invalid_field",
    );
  });

  it("never accepts a caller-supplied window: the resolver verifies the identity first", () => {
    expect(() =>
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
    ).toThrowError();
  });

  it("refuses a window whose retention days were doctored", () => {
    expectWindowError(
      () =>
        assertWatcherCanonicalRetentionWindow({
          ...windowFor(),
          retentionDays: 1,
        }),
      "retention_window_insufficient",
    );
  });

  it("refuses a window whose derived slot arithmetic has been tampered with", () => {
    expectWindowError(
      () =>
        assertWatcherCanonicalRetentionWindow({
          ...windowFor(),
          retentionSlots: 1,
        }),
      "retention_window_insufficient",
    );
  });

  it("refuses a window of another schema", () => {
    expectWindowError(
      () =>
        assertWatcherCanonicalRetentionWindow({
          ...windowFor(),
          schemaVersion: "other",
        } as unknown as WatcherCanonicalRetentionWindow),
      "unsupported_schema",
    );
  });
});
