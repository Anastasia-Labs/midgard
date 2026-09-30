import { createHash, createHmac } from "node:crypto";

import { watcherCanonicalJson } from "../../storage/durable-store.js";
import {
  ROLLBACK_AUTHORITY_KEY_BYTES,
  type WatcherRollbackDurableAuthoritySnapshot,
} from "./types.js";

const rollbackAuthorityEncoder = new TextEncoder();

export const rollbackAuthorityDecoder = new TextDecoder("utf-8", {
  fatal: true,
});

export const ownedRollbackSnapshotJson = new WeakSet<object>();

// Only used on process-owned JSON (decoded bytes or freshly parsed records).
// Freezing before repeated digest checks lets the canonical encoder reuse
// validated subtrees without trusting caller-owned objects or self-hashes.
export const freezeRollbackSnapshotJson = (value: unknown): void => {
  if (typeof value !== "object" || value === null) return;
  if (ownedRollbackSnapshotJson.has(value)) return;
  for (const child of Object.values(value)) freezeRollbackSnapshotJson(child);
  Object.freeze(value);
  ownedRollbackSnapshotJson.add(value);
};

// Call only after canonical JSON validation. Preserve privately owned immutable
// subtrees, and detach every new caller-owned value before the persistence await.
export const ownRollbackSnapshotJson = <T>(value: T): T => {
  if (typeof value !== "object" || value === null) return value;
  if (ownedRollbackSnapshotJson.has(value)) return value;
  const copy = Array.isArray(value)
    ? value.map((child: unknown) => ownRollbackSnapshotJson(child))
    : Object.fromEntries(
        Object.entries(value).map(([key, child]) => [
          key,
          ownRollbackSnapshotJson(child),
        ]),
      );
  Object.freeze(copy);
  ownedRollbackSnapshotJson.add(copy);
  return copy as T;
};

type WatcherRollbackDurableAuthorityContent = Omit<
  WatcherRollbackDurableAuthoritySnapshot,
  "authorityDigest" | "authorityMac"
>;

export const rollbackAuthorityCanonical = (
  value: WatcherRollbackDurableAuthorityContent,
): WatcherRollbackDurableAuthorityContent => ({
  validationSchemaVersion: value.validationSchemaVersion,
  schemaVersion: value.schemaVersion,
  revision: value.revision,
  priorSnapshotSha256: value.priorSnapshotSha256,
  policyDigest: value.policyDigest,
  deploymentMarker: value.deploymentMarker,
  currentStore: value.currentStore,
  consistencyHistory: value.consistencyHistory,
  rollbackState: value.rollbackState,
  rollbackBootstrapState: value.rollbackBootstrapState,
  trustedCheckpointStateDigest: value.trustedCheckpointStateDigest,
  userEventCheckpoint: value.userEventCheckpoint,
  userEventValidation: value.userEventValidation,
  authenticationKeyId: value.authenticationKeyId,
});

export const parseRollbackAuthorityAuthenticationKey = (
  value: unknown,
): Uint8Array => {
  if (
    !(value instanceof Uint8Array) ||
    value.byteLength !== ROLLBACK_AUTHORITY_KEY_BYTES
  ) {
    throw new Error("invalid watcher rollback authority authentication key");
  }
  return Uint8Array.from(value);
};

export const rollbackAuthorityKeyId = (key: Uint8Array): string =>
  createHash("sha256").update(key).digest("hex");

export const rollbackAuthorityMac = (
  key: Uint8Array,
  canonical: Readonly<
    WatcherRollbackDurableAuthorityContent & {
      authorityDigest: string;
    }
  >,
): string =>
  createHmac("sha256", key)
    .update(watcherCanonicalJson(canonical), "utf8")
    .digest("hex");

export const encodeRollbackDurableAuthoritySnapshot = (
  value: WatcherRollbackDurableAuthoritySnapshot,
): Uint8Array => rollbackAuthorityEncoder.encode(watcherCanonicalJson(value));
