/**
 * The watcher's public-DA retention window (R5), bound to the signed
 * deployment identity. It never invents a window: the window is derived from
 * `da.transportProfile.retentionDays` in the verified manifest and validated
 * against the Q54 core contract; a caller-supplied window is not accepted.
 */

import {
  assertRetentionDaysCoverWindow,
  DA_TRANSPORT_LIMITS,
  type DeploymentMarker,
  MIDGARD_RETENTION_WINDOW,
  parseDeploymentMarker,
  retentionDeadlineForBlock,
} from "@al-ft/midgard-core";

import {
  type VerifiedWatcherDeploymentIdentity,
  verifyWatcherDeploymentIdentity,
  type WatcherDeploymentIdentityPolicy,
  type WatcherDeploymentTrustRoot,
} from "../runtime/deployment-identity.js";

const WATCHER_RETENTION_WINDOW_SCHEMA_VERSION =
  "midgard-watcher-retention-window-v1" as const;

/**
 * Cardano slot length. Retention is a wall-clock quantity in the Q54 contract;
 * the window is also expressed in slots because that is the only clock the
 * watcher shares with L1. The conversion is a chain constant, not window
 * arithmetic - every duration still comes from the Q54 helpers.
 */
export const WATCHER_RETENTION_SLOT_LENGTH_MS = 1_000 as const;

export type WatcherRetentionWindowErrorCode =
  | "invalid_field"
  | "retention_window_insufficient"
  | "unsupported_schema";

export class WatcherRetentionWindowError extends Error {
  readonly code: WatcherRetentionWindowErrorCode;
  readonly path: string;

  constructor(code: WatcherRetentionWindowErrorCode, path: string) {
    super(`Watcher retention window rejected: ${code} at ${path}`);
    this.name = "WatcherRetentionWindowError";
    this.code = code;
    this.path = path;
  }
}

const fail = (code: WatcherRetentionWindowErrorCode, path: string): never => {
  throw new WatcherRetentionWindowError(code, path);
};

const plainRecord = (value: unknown, path: string): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const candidate = value as object;
  const prototype = Object.getPrototypeOf(candidate) as unknown;
  if (prototype !== Object.prototype && prototype !== null) {
    fail("invalid_field", path);
  }
  if (Reflect.ownKeys(candidate).length !== Object.keys(candidate).length) {
    fail("invalid_field", path);
  }
  return value as Record<string, unknown>;
};

const parseMarker = (value: unknown, path: string): DeploymentMarker => {
  try {
    return parseDeploymentMarker(value);
  } catch {
    return fail("invalid_field", path);
  }
};

export type WatcherCanonicalRetentionWindow = Readonly<{
  schemaVersion: typeof WATCHER_RETENTION_WINDOW_SCHEMA_VERSION;
  manifestId: string;
  deploymentMarker: DeploymentMarker;
  retentionDays: number;
  deployedRetentionMs: number;
  requiredRetentionMs: number;
  maturityMs: number;
  worstCaseProofTimeBoundMs: number;
  marginMs: number;
  retentionSlots: number;
  requiredRetentionSlots: number;
  maturitySlots: number;
  /** Slots of headroom at which a still-retained record raises the alert. */
  alertHeadroomSlots: number;
}>;

const msToSlots = (milliseconds: number): number =>
  Math.floor(milliseconds / WATCHER_RETENTION_SLOT_LENGTH_MS);

const manifestRetentionDays = (manifest: unknown, path: string): unknown => {
  const root = plainRecord(manifest, path);
  const da = plainRecord(root.da, `${path}.da`);
  const transportProfile = plainRecord(
    da.transportProfile,
    `${path}.da.transportProfile`,
  );
  return transportProfile.retentionDays;
};

const assertRetentionDays = (value: unknown, path: string): number => {
  let retentionDays: number;
  try {
    // Q54 is the single authority for maturity + worst-case proof-time bound.
    retentionDays = assertRetentionDaysCoverWindow(value, path);
  } catch {
    return fail("retention_window_insufficient", path);
  }
  // The deployed DA transport profile floor is stricter than the bare
  // challengeability horizon; both must hold.
  if (retentionDays < DA_TRANSPORT_LIMITS.minimumRetentionDays) {
    return fail("retention_window_insufficient", path);
  }
  return retentionDays;
};

const makeRetentionWindow = (
  manifestId: string,
  deploymentMarker: DeploymentMarker,
  retentionDays: number,
): WatcherCanonicalRetentionWindow => {
  // Duration arithmetic is delegated to the Q54 helper so a profile change
  // propagates instead of being restated here.
  const deadline = retentionDeadlineForBlock({
    blockEndTimeMs: 0,
    retentionDays,
  });
  return Object.freeze({
    schemaVersion: WATCHER_RETENTION_WINDOW_SCHEMA_VERSION,
    manifestId,
    deploymentMarker,
    retentionDays,
    deployedRetentionMs: deadline.deployedRetentionMs,
    requiredRetentionMs: MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    maturityMs: MIDGARD_RETENTION_WINDOW.maturityMs,
    worstCaseProofTimeBoundMs:
      MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
    marginMs:
      deadline.deployedRetentionMs -
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    retentionSlots: msToSlots(deadline.deployedRetentionMs),
    requiredRetentionSlots: msToSlots(
      MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
    ),
    maturitySlots: msToSlots(MIDGARD_RETENTION_WINDOW.maturityMs),
    alertHeadroomSlots: msToSlots(
      Math.max(
        deadline.deployedRetentionMs -
          MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
        0,
      ),
    ),
  });
};

/**
 * Reads the window out of a deployment manifest that has ALREADY been
 * verified. This is the second half of `resolveWatcherCanonicalRetentionWindow`
 * and exists separately only so the manifest-shape and floor behaviour can be
 * exercised directly; it performs no signature or policy checking of its own
 * and must never be called with an unverified manifest.
 */
export const watcherCanonicalRetentionWindowFromVerifiedManifest = (input: {
  readonly manifest: unknown;
  readonly manifestId: string;
  readonly deploymentMarker: DeploymentMarker;
  readonly path?: string;
}): WatcherCanonicalRetentionWindow => {
  const path = input.path ?? "$.manifest";
  const retentionDays = assertRetentionDays(
    manifestRetentionDays(input.manifest, path),
    `${path}.da.transportProfile.retentionDays`,
  );
  return makeRetentionWindow(
    input.manifestId,
    parseMarker(input.deploymentMarker, "$.deploymentMarker"),
    retentionDays,
  );
};

/**
 * Derives the retention window from the SIGNED deployment identity. The
 * identity is verified first; only then is `da.transportProfile.retentionDays`
 * read out of the manifest whose digest that verification bound. A window is
 * never accepted from a caller.
 */
export const resolveWatcherCanonicalRetentionWindow = (input: {
  readonly signedIdentity: unknown;
  readonly policy: WatcherDeploymentIdentityPolicy;
  readonly trustRoots: readonly WatcherDeploymentTrustRoot[];
  readonly durableMarker: unknown;
}): Readonly<{
  identity: VerifiedWatcherDeploymentIdentity;
  window: WatcherCanonicalRetentionWindow;
}> => {
  const identity = verifyWatcherDeploymentIdentity({
    signedIdentity: input.signedIdentity,
    policy: input.policy,
    trustRoots: input.trustRoots,
    durableMarker: input.durableMarker,
  });
  const envelope = plainRecord(input.signedIdentity, "$.signedIdentity");
  return Object.freeze({
    identity,
    window: watcherCanonicalRetentionWindowFromVerifiedManifest({
      manifest: envelope.manifest,
      manifestId: identity.manifestId,
      deploymentMarker: identity.durableMarker,
      path: "$.signedIdentity.manifest",
    }),
  });
};

/** Re-validates a window value before it is allowed to influence retention. */
export const assertWatcherCanonicalRetentionWindow = (
  window: WatcherCanonicalRetentionWindow,
  path = "$.retentionWindow",
): WatcherCanonicalRetentionWindow => {
  if (window.schemaVersion !== WATCHER_RETENTION_WINDOW_SCHEMA_VERSION) {
    fail("unsupported_schema", `${path}.schemaVersion`);
  }
  const retentionDays = assertRetentionDays(
    window.retentionDays,
    `${path}.retentionDays`,
  );
  const expected = makeRetentionWindow(
    window.manifestId,
    window.deploymentMarker,
    retentionDays,
  );
  if (
    window.deployedRetentionMs !== expected.deployedRetentionMs ||
    window.requiredRetentionMs !== expected.requiredRetentionMs ||
    window.maturityMs !== expected.maturityMs ||
    window.worstCaseProofTimeBoundMs !== expected.worstCaseProofTimeBoundMs ||
    window.retentionSlots !== expected.retentionSlots ||
    window.requiredRetentionSlots !== expected.requiredRetentionSlots ||
    window.maturitySlots !== expected.maturitySlots ||
    window.marginMs !== expected.marginMs
  ) {
    fail("retention_window_insufficient", path);
  }
  return window;
};
