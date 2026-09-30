import { createHash } from "node:crypto";

import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core";
import type { EvidenceProvenance } from "@al-ft/midgard-sdk";
import { assertSecurityGradeEvidence } from "@al-ft/midgard-sdk";

import { type WatcherDaProofInput } from "./durable-store.js";

export const WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION =
  "midgard-watcher-canonical-block-store-v1" as const;

/**
 * Cardano slot length. Retention is a wall-clock quantity in the Q54 contract;
 * the store keys records on slots because that is the only clock the watcher
 * shares with L1. The conversion is a chain constant, not window arithmetic -
 * every duration still comes from the Q54 helpers.
 */
export const WATCHER_CANONICAL_SLOT_LENGTH_MS = 1_000 as const;

export const WATCHER_CANONICAL_CONTENT_KINDS = [
  "da_payload",
  "event_to_step_entry",
  "event_to_step_nonmembership",
  "proof_bundle",
  "trace_step",
] as const;

export type WatcherCanonicalContentKind =
  (typeof WATCHER_CANONICAL_CONTENT_KINDS)[number];

/**
 * Every content kind maps onto exactly one reserved `WatcherDaProofInput`
 * record kind. Only the block body itself is a `da_payload`; every other
 * public artifact is proof input.
 */
export const WATCHER_CANONICAL_RECORD_KIND_BY_CONTENT_KIND: Readonly<
  Record<WatcherCanonicalContentKind, WatcherDaProofInput["kind"]>
> = Object.freeze({
  da_payload: "da_payload",
  event_to_step_entry: "proof_input",
  event_to_step_nonmembership: "proof_input",
  proof_bundle: "proof_input",
  trace_step: "proof_input",
});

export const WATCHER_CANONICAL_PRUNE_REASON_CODES = [
  "expired_and_not_challengeable",
  "retention_not_expired",
  "still_challengeable",
  "unknown_input_id",
] as const;

export type WatcherCanonicalPruneReasonCode =
  (typeof WATCHER_CANONICAL_PRUNE_REASON_CODES)[number];

export const WATCHER_CANONICAL_BLOCK_STORE_ALERT_CODES = [
  "deadline_at_risk",
] as const;

export type WatcherCanonicalBlockStoreAlertCode =
  (typeof WATCHER_CANONICAL_BLOCK_STORE_ALERT_CODES)[number];

/** Bounded retry budget for ordinary compare-and-swap contention. */
export const WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS = 8 as const;

export type WatcherCanonicalBlockStoreErrorCode =
  | "cas_contention"
  | "content_conflict"
  | "deployment_marker_mismatch"
  | "integrity_mismatch"
  | "invalid_encoding"
  | "invalid_field"
  | "missing_field"
  | "noncanonical_encoding"
  | "persistence_failure"
  | "provenance_not_public_da"
  | "retention_window_insufficient"
  | "unknown_field"
  | "unsupported_schema"
  | "unsorted_records";

export class WatcherCanonicalBlockStoreError extends Error {
  readonly code: WatcherCanonicalBlockStoreErrorCode;
  readonly path: string;

  constructor(code: WatcherCanonicalBlockStoreErrorCode, path: string) {
    super(`Watcher canonical block store rejected: ${code} at ${path}`);
    this.name = "WatcherCanonicalBlockStoreError";
    this.code = code;
    this.path = path;
  }
}

export const fail = (
  code: WatcherCanonicalBlockStoreErrorCode,
  path: string,
): never => {
  throw new WatcherCanonicalBlockStoreError(code, path);
};

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const LOWER_HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;

export const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const NON_EMPTY_TEXT = /^[\x21-\x7e]{1,256}$/u;

export const UTF8_ENCODER = new TextEncoder();

export const UTF8_DECODER = new TextDecoder("utf-8", { fatal: true });

export const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

type JsonRecord = Record<string, unknown>;

export const plainRecord = (value: unknown, path: string): JsonRecord => {
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
  return value as JsonRecord;
};

export const exactRecord = (
  value: unknown,
  path: string,
  requiredKeys: readonly string[],
): JsonRecord => {
  const record = plainRecord(value, path);
  const required = new Set(requiredKeys);
  for (const key of Object.keys(record)) {
    if (!required.has(key)) {
      fail("unknown_field", `${path}.${key}`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.prototype.hasOwnProperty.call(record, key)) {
      fail("missing_field", `${path}.${key}`);
    }
  }
  return record;
};

export const exactString = (
  value: unknown,
  path: string,
  pattern: RegExp,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    fail("invalid_field", path);
  }
  return value as string;
};

export const exactLiteral = <T extends string>(
  value: unknown,
  path: string,
  allowed: readonly T[],
): T => {
  if (typeof value !== "string" || !allowed.includes(value as T)) {
    fail("invalid_field", path);
  }
  return value as T;
};

export const exactSlot = (value: unknown, path: string): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < 0 ||
    Object.is(value, -0)
  ) {
    fail("invalid_field", path);
  }
  return value as number;
};

export const parseMarker = (value: unknown, path: string): DeploymentMarker => {
  try {
    return parseDeploymentMarker(value);
  } catch {
    return fail("invalid_field", path);
  }
};

export const markersMatch = (
  left: DeploymentMarker,
  right: DeploymentMarker,
): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

// ---------------------------------------------------------------------------
// Records
// ---------------------------------------------------------------------------

export type WatcherCanonicalRecordMetadata = Readonly<{
  /** Addressing key: the public DA client's `inputId` for this artifact. */
  inputId: string;
  kind: WatcherDaProofInput["kind"];
  contentKind: WatcherCanonicalContentKind;
  /** 28-byte L2 header hash the artifact belongs to. */
  headerHash: string;
  /** sha256 of the exact stored byte string. */
  envelopeSha256: string;
  /** sha256 of the dependent byte string, or null for atomic artifacts. */
  innerSha256: string | null;
  byteLength: number;
  sourcePeerIdentity: string;
  sourcePeerId: string;
  provenance: EvidenceProvenance;
  deploymentMarker: DeploymentMarker;
  observedAtSlot: number;
  retainUntilSlot: number;
}>;

export type WatcherCanonicalBlockRecord = Readonly<{
  input: WatcherDaProofInput;
  metadata: WatcherCanonicalRecordMetadata;
}>;

export type WatcherCanonicalBlockStoreSnapshot = Readonly<{
  schemaVersion: typeof WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION;
  revision: string;
  deploymentMarker: DeploymentMarker;
  records: readonly WatcherCanonicalBlockRecord[];
}>;

export const METADATA_KEYS = [
  "byteLength",
  "contentKind",
  "deploymentMarker",
  "envelopeSha256",
  "headerHash",
  "innerSha256",
  "inputId",
  "kind",
  "observedAtSlot",
  "provenance",
  "retainUntilSlot",
  "sourcePeerId",
  "sourcePeerIdentity",
] as const;

export const PROVENANCE_KEYS = ["grade", "sourceId", "trustClass"] as const;

export const parseProvenance = (
  value: unknown,
  path: string,
): EvidenceProvenance => {
  const record = exactRecord(value, path, PROVENANCE_KEYS);
  const provenance: EvidenceProvenance = {
    trustClass: exactString(
      record.trustClass,
      `${path}.trustClass`,
      NON_EMPTY_TEXT,
    ) as EvidenceProvenance["trustClass"],
    sourceId: exactString(record.sourceId, `${path}.sourceId`, NON_EMPTY_TEXT),
    grade: exactLiteral(record.grade, `${path}.grade`, [
      "security",
    ] as const) as EvidenceProvenance["grade"],
  };
  // Q03: only public/permissionless DA may back a submittable proof, and only
  // at security grade. Anything else is refused before it can be persisted.
  try {
    assertSecurityGradeEvidence(provenance);
  } catch {
    return fail("provenance_not_public_da", `${path}.trustClass`);
  }
  if (provenance.trustClass !== "public_or_permissionless_da") {
    return fail("provenance_not_public_da", `${path}.trustClass`);
  }
  return Object.freeze(provenance);
};
