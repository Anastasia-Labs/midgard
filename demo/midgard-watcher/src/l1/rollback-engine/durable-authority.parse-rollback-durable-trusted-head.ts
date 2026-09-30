import { createHmac, timingSafeEqual } from "node:crypto";

import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  readWatcherDurableAtomicSnapshot,
  watcherCanonicalJson,
  type WatcherDurableAtomicBackend,
  watcherDurableStoreBytesSha256,
} from "../../storage/durable-store.js";
import {
  parseWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from ".././finality-engine.js";
import {
  decodeRollbackDurableAuthoritySnapshot,
  makeRollbackDurableAuthorityHandle,
  type WatcherRollbackDurableTrustedHeadContent,
} from "./durable-authority.decode-rollback-durable-authority-snapshot.js";
import {
  parseRollbackAuthorityAuthenticationKey,
  rollbackAuthorityKeyId,
} from "./durable-authority.rollback-authority-canonical.js";
import { exactPlainRecord, sameMarker } from "./records.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  rollbackDurableAuthorityRuntime,
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthorityRuntime,
  type WatcherRollbackDurableAuthoritySnapshot,
  type WatcherRollbackDurableTrustedHead,
} from "./types.js";

const rollbackDurableTrustedHeadCanonical = (
  value: WatcherRollbackDurableTrustedHeadContent,
): WatcherRollbackDurableTrustedHeadContent => ({
  schemaVersion: value.schemaVersion,
  policyDigest: value.policyDigest,
  deploymentMarker: value.deploymentMarker,
  authenticationKeyId: value.authenticationKeyId,
  revision: value.revision,
  snapshotSha256: value.snapshotSha256,
  authorityDigest: value.authorityDigest,
});

const rollbackDurableTrustedHeadMac = (
  authenticationKey: Uint8Array,
  value: WatcherRollbackDurableTrustedHeadContent,
): string =>
  createHmac("sha256", authenticationKey)
    .update(
      `${WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION}:${watcherCanonicalJson(value)}`,
      "utf8",
    )
    .digest("hex");

export const makeRollbackDurableTrustedHead = (
  policy: WatcherFinalityPolicy,
  snapshot: WatcherRollbackDurableAuthoritySnapshot,
  snapshotSha256: string,
  authenticationKey: Uint8Array,
): WatcherRollbackDurableTrustedHead => {
  const canonical = rollbackDurableTrustedHeadCanonical({
    schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    deploymentMarker: policy.deploymentMarker,
    authenticationKeyId: snapshot.authenticationKeyId,
    revision: snapshot.revision,
    snapshotSha256,
    authorityDigest: snapshot.authorityDigest,
  });
  return Object.freeze({
    ...canonical,
    headMac: rollbackDurableTrustedHeadMac(authenticationKey, canonical),
  });
};

export const parseRollbackDurableTrustedHead = (
  value: unknown,
  policy: WatcherFinalityPolicy,
  authenticationKey: Uint8Array,
): WatcherRollbackDurableTrustedHead => {
  const record = exactPlainRecord(value, [
    "schemaVersion",
    "policyDigest",
    "deploymentMarker",
    "authenticationKeyId",
    "revision",
    "snapshotSha256",
    "authorityDigest",
    "headMac",
  ]);
  if (
    record === null ||
    record.schemaVersion !==
      WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION ||
    record.policyDigest !== policy.policyDigest ||
    record.authenticationKeyId !== rollbackAuthorityKeyId(authenticationKey) ||
    typeof record.revision !== "string" ||
    !CANONICAL_NATURAL.test(record.revision) ||
    typeof record.snapshotSha256 !== "string" ||
    !HEX_32.test(record.snapshotSha256) ||
    typeof record.authorityDigest !== "string" ||
    !HEX_32.test(record.authorityDigest) ||
    typeof record.headMac !== "string" ||
    !HEX_32.test(record.headMac)
  ) {
    throw new Error("invalid watcher rollback durable trusted head");
  }
  let deploymentMarker: DeploymentMarker;
  try {
    deploymentMarker = parseDeploymentMarker(record.deploymentMarker);
  } catch {
    throw new Error("invalid watcher rollback durable trusted head");
  }
  if (!sameMarker(deploymentMarker, policy.deploymentMarker)) {
    throw new Error("invalid watcher rollback durable trusted head");
  }
  const canonical = rollbackDurableTrustedHeadCanonical({
    schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    deploymentMarker,
    authenticationKeyId: record.authenticationKeyId,
    revision: record.revision,
    snapshotSha256: record.snapshotSha256,
    authorityDigest: record.authorityDigest,
  }) as WatcherRollbackDurableTrustedHeadContent;
  const expectedMac = rollbackDurableTrustedHeadMac(
    authenticationKey,
    canonical,
  );
  if (
    !timingSafeEqual(
      Buffer.from(expectedMac, "hex"),
      Buffer.from(record.headMac, "hex"),
    )
  ) {
    throw new Error("invalid watcher rollback durable trusted head");
  }
  return Object.freeze({
    ...canonical,
    headMac: record.headMac,
  }) as WatcherRollbackDurableTrustedHead;
};

/**
 * Strict public admission used by the independently persisted trusted-head
 * authority. It authenticates the exact release policy, deployment marker and
 * rollback-authority key; a structurally plausible caller-authored head is not
 * authority.
 */
export const admitWatcherRollbackDurableTrustedHead = (input: {
  readonly head: unknown;
  readonly policy: unknown;
  readonly authenticationKey: Uint8Array;
}): WatcherRollbackDurableTrustedHead | null => {
  try {
    const policy = parseWatcherFinalityPolicy(input.policy);
    if (policy === null) return null;
    const authenticationKey = parseRollbackAuthorityAuthenticationKey(
      input.authenticationKey,
    );
    return parseRollbackDurableTrustedHead(
      input.head,
      policy,
      authenticationKey,
    );
  } catch {
    return null;
  }
};

export const assertRollbackDurableTrustedHeadMatches = (
  trustedHead: WatcherRollbackDurableTrustedHead,
  snapshot: WatcherRollbackDurableAuthoritySnapshot,
  snapshotSha256: string,
): void => {
  if (
    trustedHead.revision !== snapshot.revision ||
    trustedHead.snapshotSha256 !== snapshotSha256 ||
    trustedHead.authorityDigest !== snapshot.authorityDigest ||
    trustedHead.authenticationKeyId !== snapshot.authenticationKeyId
  ) {
    throw new Error("watcher rollback durable trusted head mismatch");
  }
};

export const runtimeForRollbackDurableAuthority = (
  authority: WatcherRollbackDurableAuthority,
): WatcherRollbackDurableAuthorityRuntime => {
  const runtime = rollbackDurableAuthorityRuntime.get(authority);
  if (runtime === undefined) {
    throw new Error("unknown watcher rollback durable authority");
  }
  return runtime;
};

export const authorityFromEncodedSnapshot = (
  backend: WatcherDurableAtomicBackend,
  policy: WatcherFinalityPolicy,
  encoded: Uint8Array,
  snapshotSha256: string,
  authenticationKeyInput: unknown,
  trustedHeadInput: unknown,
): WatcherRollbackDurableAuthority => {
  const authenticationKey = parseRollbackAuthorityAuthenticationKey(
    authenticationKeyInput,
  );
  const trustedHead = parseRollbackDurableTrustedHead(
    trustedHeadInput,
    policy,
    authenticationKey,
  );
  const copy = Uint8Array.from(encoded);
  if (watcherDurableStoreBytesSha256(copy) !== snapshotSha256) {
    throw new Error("watcher rollback durable authority digest mismatch");
  }
  const snapshot = decodeRollbackDurableAuthoritySnapshot(
    copy,
    policy,
    authenticationKey,
  );
  if (snapshot === null) {
    throw new Error("invalid watcher rollback durable authority");
  }
  assertRollbackDurableTrustedHeadMatches(
    trustedHead,
    snapshot,
    snapshotSha256,
  );
  const authority = makeRollbackDurableAuthorityHandle(
    Object.freeze({
      backend,
      policy,
      snapshot,
      encoded: copy,
      snapshotSha256,
      authenticationKey,
    }),
  );
  return authority;
};

/**
 * Loads the rollback journal, its active epoch bootstrap, and its trust anchor
 * from one backend snapshot. `trustedHead` must come from independently
 * protected monotonic storage: the candidate snapshot can authenticate its
 * bytes but can never testify that it is the newest committed snapshot. An
 * ordinary row in the same rollbackable database is insufficient. The
 * returned handle is an in-process capability; a copied plain object cannot
 * authorize an epoch restart.
 */
export const loadWatcherRollbackDurableAuthority = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly policy: unknown;
  readonly authenticationKey: Uint8Array;
  readonly trustedHead: unknown;
}): Promise<WatcherRollbackDurableAuthority> => {
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null) {
    throw new Error("invalid watcher rollback durable authority policy");
  }
  const stored = await readWatcherDurableAtomicSnapshot(input.backend);
  if (stored === null) {
    throw new Error("watcher rollback durable authority missing");
  }
  return authorityFromEncodedSnapshot(
    input.backend,
    policy,
    stored.bytes,
    stored.sha256,
    input.authenticationKey,
    input.trustedHead,
  );
};

/** Checks that an admitted, durably validated result still matches both
 * durable owners. Fresh reads enforce exact bytes and monotonic publication;
 * the validation itself survives restart through its authenticated binding. */
export const revalidateWatcherRollbackDurableAuthority = async (input: {
  readonly authority: WatcherRollbackDurableAuthority;
  readonly trustedHead: unknown;
}): Promise<WatcherRollbackDurableAuthority> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  const trustedHead = parseRollbackDurableTrustedHead(
    input.trustedHead,
    runtime.policy,
    runtime.authenticationKey,
  );
  const stored = await readWatcherDurableAtomicSnapshot(runtime.backend);
  if (stored === null) {
    throw new Error("watcher rollback durable authority missing");
  }
  if (
    stored.sha256 !== runtime.snapshotSha256 ||
    Buffer.compare(stored.bytes, runtime.encoded) !== 0
  ) {
    throw new Error("watcher rollback durable authority bytes changed");
  }
  assertRollbackDurableTrustedHeadMatches(
    trustedHead,
    runtime.snapshot,
    stored.sha256,
  );
  return input.authority;
};
