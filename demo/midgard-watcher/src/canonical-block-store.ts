/**
 * Canonical block/proof store (GOAL_SPEC 10.3 W21).
 *
 * The watcher must be able to prove, after the fact, exactly which public
 * bytes it verified and submitted against. This module is that record: it
 * persists the byte strings returned by the public DA client BEFORE any
 * verification or submission decision is taken, addressed by their own digest,
 * immutable once written, and deletable only through an explicit retention
 * prune that is bound to the deployment manifest's retention window.
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

import { createHash } from "node:crypto";

import type { EvidenceProvenanceV1 } from "@al-ft/midgard-sdk";
import { assertSecurityGradeEvidenceV1 } from "@al-ft/midgard-sdk";

import { unwrapDaPayloadV1 } from "../../midgard-core/src/da-payload-envelope.js";
import {
  assertRetentionDaysCoverWindowV1,
  DA_TRANSPORT_LIMITS_V1,
  type DeploymentMarkerV1,
  MIDGARD_MIN_RETENTION_DAYS_V1,
  MIDGARD_RETENTION_WINDOW_V1,
  parseDeploymentMarkerV1,
  retentionDeadlineForBlockV1,
} from "../../midgard-core/src/index.js";
import {
  type VerifiedWatcherDeploymentIdentityV1,
  verifyWatcherDeploymentIdentityV1,
  type WatcherDeploymentIdentityPolicyV1,
  type WatcherDeploymentTrustRootV1,
} from "./deployment-identity.js";
import {
  compareAndSwapWatcherDurableAtomicSnapshotV1,
  readWatcherDurableAtomicSnapshotV1,
  watcherCanonicalJsonV1,
  type WatcherDaProofInputV1,
  type WatcherDurableAtomicBackend,
  watcherDurableStoreBytesSha256,
  WatcherDurableStoreError,
} from "./durable-store.js";
import type {
  WatcherPublicDaEventToStepV1,
  WatcherPublicDaPayloadV1,
  WatcherPublicDaProofBundleV1,
  WatcherPublicDaTraceStepV1,
} from "./public-da-client.js";

export const WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION =
  "midgard-watcher-canonical-block-store-v1" as const;

/**
 * Cardano slot length. Retention is a wall-clock quantity in the Q54 contract;
 * the store keys records on slots because that is the only clock the watcher
 * shares with L1. The conversion is a chain constant, not window arithmetic -
 * every duration still comes from the Q54 helpers.
 */
export const WATCHER_CANONICAL_SLOT_LENGTH_MS_V1 = 1_000 as const;

export const WATCHER_CANONICAL_CONTENT_KINDS_V1 = [
  "da_payload",
  "event_to_step_entry",
  "event_to_step_nonmembership",
  "proof_bundle",
  "trace_step",
] as const;

export type WatcherCanonicalContentKindV1 =
  (typeof WATCHER_CANONICAL_CONTENT_KINDS_V1)[number];

/**
 * Every content kind maps onto exactly one reserved `WatcherDaProofInputV1`
 * record kind. Only the block body itself is a `da_payload`; every other
 * public artifact is proof input.
 */
export const WATCHER_CANONICAL_RECORD_KIND_BY_CONTENT_KIND_V1: Readonly<
  Record<WatcherCanonicalContentKindV1, WatcherDaProofInputV1["kind"]>
> = Object.freeze({
  da_payload: "da_payload",
  event_to_step_entry: "proof_input",
  event_to_step_nonmembership: "proof_input",
  proof_bundle: "proof_input",
  trace_step: "proof_input",
});

export const WATCHER_CANONICAL_PRUNE_REASON_CODES_V1 = [
  "expired_and_not_challengeable",
  "retention_not_expired",
  "still_challengeable",
  "unknown_input_id",
] as const;

export type WatcherCanonicalPruneReasonCodeV1 =
  (typeof WATCHER_CANONICAL_PRUNE_REASON_CODES_V1)[number];

export const WATCHER_CANONICAL_BLOCK_STORE_ALERT_CODES_V1 = [
  "deadline_at_risk",
] as const;

export type WatcherCanonicalBlockStoreAlertCodeV1 =
  (typeof WATCHER_CANONICAL_BLOCK_STORE_ALERT_CODES_V1)[number];

/** Bounded retry budget for ordinary compare-and-swap contention. */
export const WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS_V1 = 8 as const;

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

const fail = (
  code: WatcherCanonicalBlockStoreErrorCode,
  path: string,
): never => {
  throw new WatcherCanonicalBlockStoreError(code, path);
};

const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;
const LOWER_HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const NON_EMPTY_TEXT = /^[\x21-\x7e]{1,256}$/u;

const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder("utf-8", { fatal: true });

const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

type JsonRecord = Record<string, unknown>;

const plainRecord = (value: unknown, path: string): JsonRecord => {
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

const exactRecord = (
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

const exactString = (value: unknown, path: string, pattern: RegExp): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    fail("invalid_field", path);
  }
  return value as string;
};

const exactLiteral = <T extends string>(
  value: unknown,
  path: string,
  allowed: readonly T[],
): T => {
  if (typeof value !== "string" || !allowed.includes(value as T)) {
    fail("invalid_field", path);
  }
  return value as T;
};

const exactSlot = (value: unknown, path: string): number => {
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

const parseMarker = (value: unknown, path: string): DeploymentMarkerV1 => {
  try {
    return parseDeploymentMarkerV1(value);
  } catch {
    return fail("invalid_field", path);
  }
};

const markersMatch = (
  left: DeploymentMarkerV1,
  right: DeploymentMarkerV1,
): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

// ---------------------------------------------------------------------------
// Records
// ---------------------------------------------------------------------------

export type WatcherCanonicalRecordMetadataV1 = Readonly<{
  /** Addressing key: the public DA client's `inputId` for this artifact. */
  inputId: string;
  kind: WatcherDaProofInputV1["kind"];
  contentKind: WatcherCanonicalContentKindV1;
  /** 28-byte L2 header hash the artifact belongs to. */
  headerHash: string;
  /** sha256 of the exact stored byte string. */
  envelopeSha256: string;
  /** sha256 of the dependent byte string, or null for atomic artifacts. */
  innerSha256: string | null;
  byteLength: number;
  sourcePeerIdentity: string;
  sourcePeerId: string;
  provenance: EvidenceProvenanceV1;
  deploymentMarker: DeploymentMarkerV1;
  observedAtSlot: number;
  retainUntilSlot: number;
}>;

export type WatcherCanonicalBlockRecordV1 = Readonly<{
  input: WatcherDaProofInputV1;
  metadata: WatcherCanonicalRecordMetadataV1;
}>;

export type WatcherCanonicalBlockStoreSnapshotV1 = Readonly<{
  schemaVersion: typeof WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION;
  revision: string;
  deploymentMarker: DeploymentMarkerV1;
  records: readonly WatcherCanonicalBlockRecordV1[];
}>;

const METADATA_KEYS = [
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

const PROVENANCE_KEYS = ["grade", "sourceId", "trustClass"] as const;

const parseProvenance = (
  value: unknown,
  path: string,
): EvidenceProvenanceV1 => {
  const record = exactRecord(value, path, PROVENANCE_KEYS);
  const provenance: EvidenceProvenanceV1 = {
    trustClass: exactString(
      record.trustClass,
      `${path}.trustClass`,
      NON_EMPTY_TEXT,
    ) as EvidenceProvenanceV1["trustClass"],
    sourceId: exactString(record.sourceId, `${path}.sourceId`, NON_EMPTY_TEXT),
    grade: exactLiteral(record.grade, `${path}.grade`, [
      "security",
    ] as const) as EvidenceProvenanceV1["grade"],
  };
  // Q03: only public/permissionless DA may back a submittable proof, and only
  // at security grade. Anything else is refused before it can be persisted.
  try {
    assertSecurityGradeEvidenceV1(provenance);
  } catch {
    return fail("provenance_not_public_da", `${path}.trustClass`);
  }
  if (provenance.trustClass !== "public_or_permissionless_da") {
    return fail("provenance_not_public_da", `${path}.trustClass`);
  }
  return Object.freeze(provenance);
};

const parseDurableInput = (
  value: unknown,
  path: string,
): WatcherDaProofInputV1 => {
  const record = exactRecord(value, path, ["inputId", "kind", "payload"]);
  const payload = exactRecord(record.payload, `${path}.payload`, [
    "cborHex",
    "sha256",
  ]);
  const cborHex = exactString(
    payload.cborHex,
    `${path}.payload.cborHex`,
    LOWER_HEX_BYTES,
  );
  const bytes = Buffer.from(cborHex, "hex");
  if (bytes.length === 0) {
    fail("invalid_field", `${path}.payload.cborHex`);
  }
  if (bytes.length > DA_TRANSPORT_LIMITS_V1.maxPayloadBytes) {
    fail("invalid_field", `${path}.payload.cborHex`);
  }
  const digest = exactString(payload.sha256, `${path}.payload.sha256`, HEX_32);
  if (sha256Bytes(bytes) !== digest) {
    fail("integrity_mismatch", `${path}.payload.sha256`);
  }
  return Object.freeze({
    inputId: exactString(record.inputId, `${path}.inputId`, HEX_32),
    kind: exactLiteral(record.kind, `${path}.kind`, [
      "da_payload",
      "proof_input",
    ] as const),
    payload: Object.freeze({ cborHex, sha256: digest }),
  });
};

/**
 * Structural and digest validation of one record. Everything checkable from
 * the stored bytes alone is checked here; the inner-payload digest of a DA
 * envelope additionally needs an unwrap and is verified by
 * `verifyWatcherCanonicalRecordV1`.
 */
export const parseWatcherCanonicalBlockRecordV1 = (
  value: unknown,
  path = "$",
): WatcherCanonicalBlockRecordV1 => {
  const record = exactRecord(value, path, ["input", "metadata"]);
  const input = parseDurableInput(record.input, `${path}.input`);
  const metadataRecord = exactRecord(
    record.metadata,
    `${path}.metadata`,
    METADATA_KEYS,
  );
  const metadataPath = `${path}.metadata`;
  const contentKind = exactLiteral(
    metadataRecord.contentKind,
    `${metadataPath}.contentKind`,
    WATCHER_CANONICAL_CONTENT_KINDS_V1,
  );
  const kind = exactLiteral(metadataRecord.kind, `${metadataPath}.kind`, [
    "da_payload",
    "proof_input",
  ] as const);
  if (kind !== WATCHER_CANONICAL_RECORD_KIND_BY_CONTENT_KIND_V1[contentKind]) {
    fail("invalid_field", `${metadataPath}.kind`);
  }
  if (kind !== input.kind) {
    fail("invalid_field", `${metadataPath}.kind`);
  }
  const inputId = exactString(
    metadataRecord.inputId,
    `${metadataPath}.inputId`,
    HEX_32,
  );
  if (inputId !== input.inputId) {
    fail("integrity_mismatch", `${metadataPath}.inputId`);
  }
  const envelopeSha256 = exactString(
    metadataRecord.envelopeSha256,
    `${metadataPath}.envelopeSha256`,
    HEX_32,
  );
  // Hash addressing: the key, the stored payload digest, and the recorded
  // envelope digest are one and the same value.
  if (envelopeSha256 !== input.payload.sha256 || envelopeSha256 !== inputId) {
    fail("integrity_mismatch", `${metadataPath}.envelopeSha256`);
  }
  const innerSha256 =
    metadataRecord.innerSha256 === null
      ? null
      : exactString(
          metadataRecord.innerSha256,
          `${metadataPath}.innerSha256`,
          HEX_32,
        );
  if (contentKind === "da_payload" && innerSha256 === null) {
    fail("missing_field", `${metadataPath}.innerSha256`);
  }
  const byteLength = exactSlot(
    metadataRecord.byteLength,
    `${metadataPath}.byteLength`,
  );
  if (byteLength !== input.payload.cborHex.length / 2) {
    fail("integrity_mismatch", `${metadataPath}.byteLength`);
  }
  const observedAtSlot = exactSlot(
    metadataRecord.observedAtSlot,
    `${metadataPath}.observedAtSlot`,
  );
  const retainUntilSlot = exactSlot(
    metadataRecord.retainUntilSlot,
    `${metadataPath}.retainUntilSlot`,
  );
  if (retainUntilSlot <= observedAtSlot) {
    fail("invalid_field", `${metadataPath}.retainUntilSlot`);
  }
  const metadata: WatcherCanonicalRecordMetadataV1 = Object.freeze({
    inputId,
    kind,
    contentKind,
    headerHash: exactString(
      metadataRecord.headerHash,
      `${metadataPath}.headerHash`,
      HEX_28,
    ),
    envelopeSha256,
    innerSha256,
    byteLength,
    sourcePeerIdentity: exactString(
      metadataRecord.sourcePeerIdentity,
      `${metadataPath}.sourcePeerIdentity`,
      NON_EMPTY_TEXT,
    ),
    sourcePeerId: exactString(
      metadataRecord.sourcePeerId,
      `${metadataPath}.sourcePeerId`,
      NON_EMPTY_TEXT,
    ),
    provenance: parseProvenance(
      metadataRecord.provenance,
      `${metadataPath}.provenance`,
    ),
    deploymentMarker: parseMarker(
      metadataRecord.deploymentMarker,
      `${metadataPath}.deploymentMarker`,
    ),
    observedAtSlot,
    retainUntilSlot,
  });
  return Object.freeze({ input, metadata });
};

/**
 * Full verification of one record, including the digests that can only be
 * confirmed by re-deriving them from the stored bytes.
 */
export const verifyWatcherCanonicalRecordV1 = async (
  value: unknown,
  path = "$",
): Promise<WatcherCanonicalBlockRecordV1> => {
  const record = parseWatcherCanonicalBlockRecordV1(value, path);
  if (record.metadata.contentKind === "da_payload") {
    const envelope = Buffer.from(record.input.payload.cborHex, "hex");
    let innerBytes: Buffer;
    try {
      innerBytes = (
        await unwrapDaPayloadV1(envelope, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
        })
      ).innerBytes;
    } catch {
      return fail("integrity_mismatch", `${path}.input.payload.cborHex`);
    }
    if (sha256Bytes(innerBytes) !== record.metadata.innerSha256) {
      fail("integrity_mismatch", `${path}.metadata.innerSha256`);
    }
  }
  return record;
};

// ---------------------------------------------------------------------------
// Retention window (R5)
// ---------------------------------------------------------------------------

export type WatcherCanonicalRetentionWindowV1 = Readonly<{
  schemaVersion: typeof WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION;
  manifestId: string;
  deploymentMarker: DeploymentMarkerV1;
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
  Math.floor(milliseconds / WATCHER_CANONICAL_SLOT_LENGTH_MS_V1);

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
    retentionDays = assertRetentionDaysCoverWindowV1(value, path);
  } catch {
    return fail("retention_window_insufficient", path);
  }
  // The deployed DA transport profile floor is stricter than the bare
  // challengeability horizon; both must hold.
  if (retentionDays < DA_TRANSPORT_LIMITS_V1.minimumRetentionDays) {
    return fail("retention_window_insufficient", path);
  }
  return retentionDays;
};

const makeRetentionWindow = (
  manifestId: string,
  deploymentMarker: DeploymentMarkerV1,
  retentionDays: number,
): WatcherCanonicalRetentionWindowV1 => {
  // Duration arithmetic is delegated to the Q54 helper so a profile change
  // propagates instead of being restated here.
  const deadline = retentionDeadlineForBlockV1({
    blockEndTimeMs: 0,
    retentionDays,
  });
  return Object.freeze({
    schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION,
    manifestId,
    deploymentMarker,
    retentionDays,
    deployedRetentionMs: deadline.deployedRetentionMs,
    requiredRetentionMs: MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
    maturityMs: MIDGARD_RETENTION_WINDOW_V1.maturityMs,
    worstCaseProofTimeBoundMs:
      MIDGARD_RETENTION_WINDOW_V1.worstCaseProofTimeBoundMs,
    marginMs:
      deadline.deployedRetentionMs -
      MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
    retentionSlots: msToSlots(deadline.deployedRetentionMs),
    requiredRetentionSlots: msToSlots(
      MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
    ),
    maturitySlots: msToSlots(MIDGARD_RETENTION_WINDOW_V1.maturityMs),
    alertHeadroomSlots: msToSlots(
      Math.max(
        deadline.deployedRetentionMs -
          MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
        0,
      ),
    ),
  });
};

/**
 * Reads the window out of a deployment manifest that has ALREADY been
 * verified. This is the second half of `resolveWatcherCanonicalRetentionWindowV1`
 * and exists separately only so the manifest-shape and floor behaviour can be
 * exercised directly; it performs no signature or policy checking of its own
 * and must never be called with an unverified manifest.
 */
export const watcherCanonicalRetentionWindowFromVerifiedManifestV1 = (input: {
  readonly manifest: unknown;
  readonly manifestId: string;
  readonly deploymentMarker: DeploymentMarkerV1;
  readonly path?: string;
}): WatcherCanonicalRetentionWindowV1 => {
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
export const resolveWatcherCanonicalRetentionWindowV1 = (input: {
  readonly signedIdentity: unknown;
  readonly policy: WatcherDeploymentIdentityPolicyV1;
  readonly trustRoots: readonly WatcherDeploymentTrustRootV1[];
  readonly durableMarker: unknown;
}): Readonly<{
  identity: VerifiedWatcherDeploymentIdentityV1;
  window: WatcherCanonicalRetentionWindowV1;
}> => {
  const identity = verifyWatcherDeploymentIdentityV1({
    signedIdentity: input.signedIdentity,
    policy: input.policy,
    trustRoots: input.trustRoots,
    durableMarker: input.durableMarker,
  });
  const envelope = plainRecord(input.signedIdentity, "$.signedIdentity");
  return Object.freeze({
    identity,
    window: watcherCanonicalRetentionWindowFromVerifiedManifestV1({
      manifest: envelope.manifest,
      manifestId: identity.manifestId,
      deploymentMarker: identity.durableMarker,
      path: "$.signedIdentity.manifest",
    }),
  });
};

/** Re-validates a window value before it is allowed to influence retention. */
export const assertWatcherCanonicalRetentionWindowV1 = (
  window: WatcherCanonicalRetentionWindowV1,
  path = "$.retentionWindow",
): WatcherCanonicalRetentionWindowV1 => {
  if (
    window.schemaVersion !== WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION
  ) {
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

/** The slot at which a record observed at `observedAtSlot` stops being kept. */
export const watcherCanonicalRetainUntilSlotV1 = (input: {
  readonly window: WatcherCanonicalRetentionWindowV1;
  readonly observedAtSlot: number;
}): number => {
  const window = assertWatcherCanonicalRetentionWindowV1(input.window);
  const observedAtSlot = exactSlot(input.observedAtSlot, "$.observedAtSlot");
  return observedAtSlot + window.retentionSlots;
};

/** Whole days of retention this deployment must promise, at minimum. */
export const WATCHER_CANONICAL_MIN_RETENTION_DAYS_V1 = Math.max(
  MIDGARD_MIN_RETENTION_DAYS_V1,
  DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
);

// ---------------------------------------------------------------------------
// Record builders over the W20 client's return values
// ---------------------------------------------------------------------------

export type WatcherCanonicalRecordContextV1 = Readonly<{
  window: WatcherCanonicalRetentionWindowV1;
  deploymentMarker: DeploymentMarkerV1;
  observedAtSlot: number;
}>;

const buildRecord = (input: {
  readonly durableInput: WatcherDaProofInputV1;
  readonly contentKind: WatcherCanonicalContentKindV1;
  readonly headerHash: string;
  readonly bytes: Uint8Array;
  readonly innerSha256: string | null;
  readonly sourcePeerIdentity: string;
  readonly sourcePeerId: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly context: WatcherCanonicalRecordContextV1;
}): WatcherCanonicalBlockRecordV1 =>
  parseWatcherCanonicalBlockRecordV1({
    input: {
      inputId: input.durableInput.inputId,
      kind: input.durableInput.kind,
      payload: {
        cborHex: input.durableInput.payload.cborHex,
        sha256: input.durableInput.payload.sha256,
      },
    },
    metadata: {
      inputId: input.durableInput.inputId,
      kind: input.durableInput.kind,
      contentKind: input.contentKind,
      headerHash: input.headerHash,
      envelopeSha256: sha256Bytes(input.bytes),
      innerSha256: input.innerSha256,
      byteLength: input.bytes.byteLength,
      sourcePeerIdentity: input.sourcePeerIdentity,
      sourcePeerId: input.sourcePeerId,
      provenance: {
        trustClass: input.provenance.trustClass,
        sourceId: input.provenance.sourceId,
        grade: input.provenance.grade,
      },
      deploymentMarker: {
        schemaVersion: input.context.deploymentMarker.schemaVersion,
        manifestId: input.context.deploymentMarker.manifestId,
      },
      observedAtSlot: input.context.observedAtSlot,
      retainUntilSlot: watcherCanonicalRetainUntilSlotV1({
        window: input.context.window,
        observedAtSlot: input.context.observedAtSlot,
      }),
    },
  });

/** Block-body payload: stored envelope bytes plus the unwrapped inner digest. */
export const makeWatcherCanonicalDaPayloadRecordV1 = (input: {
  readonly payload: WatcherPublicDaPayloadV1;
  readonly context: WatcherCanonicalRecordContextV1;
}): WatcherCanonicalBlockRecordV1 =>
  buildRecord({
    durableInput: input.payload.durableInput,
    contentKind: "da_payload",
    headerHash: input.payload.headerHash,
    bytes: input.payload.payloadEnvelopeCbor,
    innerSha256: sha256Bytes(input.payload.innerPayloadCbor),
    sourcePeerIdentity: input.payload.sourcePeerIdentity,
    sourcePeerId: input.payload.sourcePeerId,
    provenance: input.payload.provenance,
    context: input.context,
  });

/** Proof bundle: an atomic artifact, so `innerSha256` is null. */
export const makeWatcherCanonicalProofBundleRecordV1 = (input: {
  readonly proofBundle: WatcherPublicDaProofBundleV1;
  readonly context: WatcherCanonicalRecordContextV1;
}): WatcherCanonicalBlockRecordV1 =>
  buildRecord({
    durableInput: input.proofBundle.durableInput,
    contentKind: "proof_bundle",
    headerHash: input.proofBundle.headerHash,
    bytes: input.proofBundle.proofBundleBytes,
    innerSha256: null,
    sourcePeerIdentity: input.proofBundle.sourcePeerIdentity,
    sourcePeerId: input.proofBundle.sourcePeerId,
    provenance: input.proofBundle.provenance,
    context: input.context,
  });

/**
 * Trace step. W20 returns the step and its membership witness together and
 * builds no durable record for them, so the record is constructed here: the
 * step bytes are the stored bytes, the witness digest is `innerSha256`.
 */
export const makeWatcherCanonicalTraceStepRecordV1 = (input: {
  readonly traceStep: WatcherPublicDaTraceStepV1;
  readonly context: WatcherCanonicalRecordContextV1;
}): WatcherCanonicalBlockRecordV1 =>
  buildRecord({
    durableInput: {
      inputId: input.traceStep.transitionStepSha256,
      kind: "proof_input",
      payload: {
        cborHex: input.traceStep.transitionStepBytes.toString("hex"),
        sha256: sha256Bytes(input.traceStep.transitionStepBytes),
      },
    },
    contentKind: "trace_step",
    headerHash: input.traceStep.headerHash,
    bytes: input.traceStep.transitionStepBytes,
    innerSha256: input.traceStep.membershipProofSha256,
    sourcePeerIdentity: input.traceStep.sourcePeerIdentity,
    sourcePeerId: input.traceStep.sourcePeerId,
    provenance: input.traceStep.provenance,
    context: input.context,
  });

/**
 * Event-to-step result. A membership answer stores the entry bytes and binds
 * the witness digest; a non-membership answer has no entry, so the witness
 * itself is the stored artifact and `innerSha256` is null.
 */
export const makeWatcherCanonicalEventToStepRecordV1 = (input: {
  readonly eventToStep: WatcherPublicDaEventToStepV1;
  readonly context: WatcherCanonicalRecordContextV1;
}): WatcherCanonicalBlockRecordV1 => {
  const entryBytes = input.eventToStep.eventToStepEntryBytes;
  const membership = entryBytes !== null;
  const bytes = membership
    ? entryBytes
    : input.eventToStep.membershipOrNonmembershipProofBytes;
  return buildRecord({
    durableInput: {
      inputId: sha256Bytes(bytes),
      kind: "proof_input",
      payload: {
        cborHex: bytes.toString("hex"),
        sha256: sha256Bytes(bytes),
      },
    },
    contentKind: membership
      ? "event_to_step_entry"
      : "event_to_step_nonmembership",
    headerHash: input.eventToStep.headerHash,
    bytes,
    innerSha256: membership
      ? input.eventToStep.membershipOrNonmembershipProofSha256
      : null,
    sourcePeerIdentity: input.eventToStep.sourcePeerIdentity,
    sourcePeerId: input.eventToStep.sourcePeerId,
    provenance: input.eventToStep.provenance,
    context: input.context,
  });
};

// ---------------------------------------------------------------------------
// Snapshot encoding
// ---------------------------------------------------------------------------

const SNAPSHOT_KEYS = [
  "deploymentMarker",
  "records",
  "revision",
  "schemaVersion",
] as const;

export const parseWatcherCanonicalBlockStoreSnapshotV1 = (
  value: unknown,
): WatcherCanonicalBlockStoreSnapshotV1 => {
  const record = exactRecord(value, "$", SNAPSHOT_KEYS);
  if (
    record.schemaVersion !== WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION
  ) {
    fail("unsupported_schema", "$.schemaVersion");
  }
  const revision = exactString(
    record.revision,
    "$.revision",
    CANONICAL_NATURAL,
  );
  const deploymentMarker = parseMarker(
    record.deploymentMarker,
    "$.deploymentMarker",
  );
  if (!Array.isArray(record.records)) {
    fail("invalid_field", "$.records");
  }
  const members = record.records as readonly unknown[];
  const records = members.map((member, index) =>
    parseWatcherCanonicalBlockRecordV1(member, `$.records[${String(index)}]`),
  );
  for (let index = 0; index < records.length; index += 1) {
    const current = records[index] as WatcherCanonicalBlockRecordV1;
    if (!markersMatch(deploymentMarker, current.metadata.deploymentMarker)) {
      fail(
        "deployment_marker_mismatch",
        `$.records[${String(index)}].metadata.deploymentMarker`,
      );
    }
    if (index === 0) {
      continue;
    }
    const previous = records[index - 1] as WatcherCanonicalBlockRecordV1;
    if (current.input.inputId === previous.input.inputId) {
      fail("content_conflict", `$.records[${String(index)}].input.inputId`);
    }
    if (current.input.inputId < previous.input.inputId) {
      fail("unsorted_records", "$.records");
    }
  }
  return Object.freeze({
    schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION,
    revision,
    deploymentMarker,
    records: Object.freeze(records),
  });
};

export const encodeWatcherCanonicalBlockStoreSnapshotV1 = (
  snapshot: WatcherCanonicalBlockStoreSnapshotV1,
): Uint8Array =>
  UTF8_ENCODER.encode(
    watcherCanonicalJsonV1(parseWatcherCanonicalBlockStoreSnapshotV1(snapshot)),
  );

/** Decodes one snapshot, refusing anything that is not its canonical encoding. */
export const decodeWatcherCanonicalBlockStoreSnapshotV1 = (
  bytes: Uint8Array,
): WatcherCanonicalBlockStoreSnapshotV1 => {
  let text = "";
  let decoded: unknown;
  try {
    text = UTF8_DECODER.decode(bytes);
    decoded = JSON.parse(text) as unknown;
  } catch {
    return fail("invalid_encoding", "$");
  }
  const parsed = parseWatcherCanonicalBlockStoreSnapshotV1(decoded);
  if (text !== watcherCanonicalJsonV1(parsed)) {
    fail("noncanonical_encoding", "$");
  }
  return parsed;
};

export const makeEmptyWatcherCanonicalBlockStoreSnapshotV1 = (
  deploymentMarker: DeploymentMarkerV1,
): WatcherCanonicalBlockStoreSnapshotV1 =>
  parseWatcherCanonicalBlockStoreSnapshotV1({
    schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION,
    revision: "0",
    deploymentMarker: {
      schemaVersion: deploymentMarker.schemaVersion,
      manifestId: deploymentMarker.manifestId,
    },
    records: [],
  });

const nextRevision = (revision: string): string =>
  (BigInt(revision) + 1n).toString();

// ---------------------------------------------------------------------------
// Durable authority
// ---------------------------------------------------------------------------

export type WatcherCanonicalBlockStoreLoadV1 = Readonly<{
  snapshot: WatcherCanonicalBlockStoreSnapshotV1;
  snapshotSha256: string;
}>;

const readSnapshotBytes = async (
  backend: WatcherDurableAtomicBackend,
): Promise<Readonly<{ bytes: Uint8Array; sha256: string }> | null> => {
  try {
    return await readWatcherDurableAtomicSnapshotV1(backend);
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

const commitSnapshot = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly expectedSha256: string | null;
  readonly next: Uint8Array;
}): Promise<string | null> => {
  let result: Awaited<
    ReturnType<typeof compareAndSwapWatcherDurableAtomicSnapshotV1>
  >;
  try {
    result = await compareAndSwapWatcherDurableAtomicSnapshotV1(input);
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

const verifySnapshot = async (
  snapshot: WatcherCanonicalBlockStoreSnapshotV1,
  expectedMarker: DeploymentMarkerV1,
): Promise<WatcherCanonicalBlockStoreSnapshotV1> => {
  if (!markersMatch(expectedMarker, snapshot.deploymentMarker)) {
    fail("deployment_marker_mismatch", "$.deploymentMarker");
  }
  for (let index = 0; index < snapshot.records.length; index += 1) {
    await verifyWatcherCanonicalRecordV1(
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
export const loadWatcherCanonicalBlockStoreV1 = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  readonly retentionWindow?: WatcherCanonicalRetentionWindowV1;
}): Promise<WatcherCanonicalBlockStoreLoadV1 | null> => {
  if (input.retentionWindow !== undefined) {
    assertWatcherCanonicalRetentionWindowV1(input.retentionWindow);
  }
  const stored = await readSnapshotBytes(input.backend);
  if (stored === null) {
    return null;
  }
  const snapshot = decodeWatcherCanonicalBlockStoreSnapshotV1(stored.bytes);
  await verifySnapshot(snapshot, input.deploymentIdentity.durableMarker);
  return Object.freeze({ snapshot, snapshotSha256: stored.sha256 });
};

export type WatcherCanonicalPersistResultV1 = Readonly<{
  committed: boolean;
  alreadyPresent: boolean;
  inputId: string;
  snapshotSha256: string;
  revision: string;
}>;

const sameRecord = (
  left: WatcherCanonicalBlockRecordV1,
  right: WatcherCanonicalBlockRecordV1,
): boolean => watcherCanonicalJsonV1(left) === watcherCanonicalJsonV1(right);

/**
 * Persists one public byte string exactly as received, before any verification
 * or submission decision.
 *
 * Idempotent for a byte-identical re-persist. A different byte string under an
 * existing key is a hard `content_conflict`: the store never overwrites, and
 * the only way a record leaves is `pruneWatcherCanonicalBlockStoreV1`.
 */
export const persistWatcherCanonicalPublicBytesV1 = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  readonly record: WatcherCanonicalBlockRecordV1;
}): Promise<WatcherCanonicalPersistResultV1> => {
  const marker = input.deploymentIdentity.durableMarker;
  const record = await verifyWatcherCanonicalRecordV1(input.record, "$.record");
  if (!markersMatch(marker, record.metadata.deploymentMarker)) {
    fail("deployment_marker_mismatch", "$.record.metadata.deploymentMarker");
  }
  for (
    let attempt = 0;
    attempt < WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS_V1;
    attempt += 1
  ) {
    const stored = await readSnapshotBytes(input.backend);
    const current =
      stored === null
        ? makeEmptyWatcherCanonicalBlockStoreSnapshotV1(marker)
        : await verifySnapshot(
            decodeWatcherCanonicalBlockStoreSnapshotV1(stored.bytes),
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
                encodeWatcherCanonicalBlockStoreSnapshotV1(current),
              )
            : stored.sha256,
        revision: current.revision,
      });
    }
    const next = parseWatcherCanonicalBlockStoreSnapshotV1({
      schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION,
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
      next: encodeWatcherCanonicalBlockStoreSnapshotV1(next),
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

export type WatcherCanonicalPruneDecisionV1 = Readonly<{
  inputId: string;
  decision: "pruned" | "retained";
  reasonCode: WatcherCanonicalPruneReasonCodeV1;
  retainUntilSlot: number | null;
  remainingSlots: number | null;
  alertCode: WatcherCanonicalBlockStoreAlertCodeV1 | null;
}>;

export type WatcherCanonicalPruneResultV1 = Readonly<{
  committed: boolean;
  revision: string;
  snapshotSha256: string | null;
  prunedInputIds: readonly string[];
  decisions: readonly WatcherCanonicalPruneDecisionV1[];
  alerts: readonly WatcherCanonicalPruneDecisionV1[];
}>;

/**
 * The only deletion path. A record leaves the store only when its retention
 * deadline has strictly passed AND it is not in the still-challengeable set;
 * every other outcome is a refusal carrying a deterministic reason code. A
 * record whose remaining headroom has been consumed raises `deadline_at_risk`
 * before it can expire.
 */
export const pruneWatcherCanonicalBlockStoreV1 = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  readonly atSlot: number;
  readonly stillChallengeableInputIds: readonly string[];
  readonly inputIds?: readonly string[];
  readonly retentionWindow?: WatcherCanonicalRetentionWindowV1;
}): Promise<WatcherCanonicalPruneResultV1> => {
  const marker = input.deploymentIdentity.durableMarker;
  const atSlot = exactSlot(input.atSlot, "$.atSlot");
  const challengeable = new Set(
    input.stillChallengeableInputIds.map((inputId, index) =>
      exactString(
        inputId,
        `$.stillChallengeableInputIds[${String(index)}]`,
        HEX_32,
      ),
    ),
  );
  const alertHeadroomSlots =
    input.retentionWindow === undefined
      ? msToSlots(MIDGARD_RETENTION_WINDOW_V1.marginMs)
      : assertWatcherCanonicalRetentionWindowV1(input.retentionWindow)
          .alertHeadroomSlots;
  const requested =
    input.inputIds === undefined
      ? null
      : new Set(
          input.inputIds.map((inputId, index) =>
            exactString(inputId, `$.inputIds[${String(index)}]`, HEX_32),
          ),
        );

  for (
    let attempt = 0;
    attempt < WATCHER_CANONICAL_BLOCK_STORE_MAX_CAS_ATTEMPTS_V1;
    attempt += 1
  ) {
    const stored = await readSnapshotBytes(input.backend);
    if (stored === null) {
      const decisions = Object.freeze(
        [...(requested ?? [])].map((inputId) =>
          Object.freeze({
            inputId,
            decision: "retained" as const,
            reasonCode: "unknown_input_id" as const,
            retainUntilSlot: null,
            remainingSlots: null,
            alertCode: null,
          }),
        ),
      );
      return Object.freeze({
        committed: false,
        revision: "0",
        snapshotSha256: null,
        prunedInputIds: Object.freeze([]),
        decisions,
        alerts: Object.freeze([]),
      });
    }
    const current = await verifySnapshot(
      decodeWatcherCanonicalBlockStoreSnapshotV1(stored.bytes),
      marker,
    );
    const known = new Set(
      current.records.map((record) => record.input.inputId),
    );
    const decisions: WatcherCanonicalPruneDecisionV1[] = [];
    const kept: WatcherCanonicalBlockRecordV1[] = [];
    const pruned: string[] = [];
    for (const record of current.records) {
      const { inputId, retainUntilSlot } = record.metadata;
      if (requested !== null && !requested.has(inputId)) {
        kept.push(record);
        continue;
      }
      const remainingSlots = retainUntilSlot - atSlot;
      if (challengeable.has(inputId)) {
        kept.push(record);
        decisions.push(
          Object.freeze({
            inputId,
            decision: "retained" as const,
            reasonCode: "still_challengeable" as const,
            retainUntilSlot,
            remainingSlots,
            alertCode:
              remainingSlots >= 0 && remainingSlots <= alertHeadroomSlots
                ? ("deadline_at_risk" as const)
                : null,
          }),
        );
        continue;
      }
      if (!(retainUntilSlot < atSlot)) {
        kept.push(record);
        decisions.push(
          Object.freeze({
            inputId,
            decision: "retained" as const,
            reasonCode: "retention_not_expired" as const,
            retainUntilSlot,
            remainingSlots,
            alertCode:
              remainingSlots <= alertHeadroomSlots
                ? ("deadline_at_risk" as const)
                : null,
          }),
        );
        continue;
      }
      pruned.push(inputId);
      decisions.push(
        Object.freeze({
          inputId,
          decision: "pruned" as const,
          reasonCode: "expired_and_not_challengeable" as const,
          retainUntilSlot,
          remainingSlots,
          alertCode: null,
        }),
      );
    }
    for (const inputId of requested ?? []) {
      if (!known.has(inputId)) {
        decisions.push(
          Object.freeze({
            inputId,
            decision: "retained" as const,
            reasonCode: "unknown_input_id" as const,
            retainUntilSlot: null,
            remainingSlots: null,
            alertCode: null,
          }),
        );
      }
    }
    const alerts = Object.freeze(
      decisions.filter((decision) => decision.alertCode !== null),
    );
    if (pruned.length === 0) {
      return Object.freeze({
        committed: false,
        revision: current.revision,
        snapshotSha256: stored.sha256,
        prunedInputIds: Object.freeze([]),
        decisions: Object.freeze(decisions),
        alerts,
      });
    }
    const next = parseWatcherCanonicalBlockStoreSnapshotV1({
      schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION,
      revision: nextRevision(current.revision),
      deploymentMarker: {
        schemaVersion: current.deploymentMarker.schemaVersion,
        manifestId: current.deploymentMarker.manifestId,
      },
      records: kept,
    });
    const sha256 = await commitSnapshot({
      backend: input.backend,
      expectedSha256: stored.sha256,
      next: encodeWatcherCanonicalBlockStoreSnapshotV1(next),
    });
    if (sha256 !== null) {
      return Object.freeze({
        committed: true,
        revision: next.revision,
        snapshotSha256: sha256,
        prunedInputIds: Object.freeze(pruned),
        decisions: Object.freeze(decisions),
        alerts,
      });
    }
  }
  return fail("cas_contention", "$.backend.compareAndSwap");
};
