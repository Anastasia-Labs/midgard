import {
  assertRetentionDaysCoverWindow,
  DA_TRANSPORT_LIMITS,
  type DeploymentMarker,
  MIDGARD_RETENTION_WINDOW,
  retentionDeadlineForBlock,
} from "@al-ft/midgard-core";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";

import {
  exactLiteral,
  exactRecord,
  exactSlot,
  exactString,
  fail,
  HEX_28,
  HEX_32,
  LOWER_HEX_BYTES,
  METADATA_KEYS,
  NON_EMPTY_TEXT,
  parseMarker,
  parseProvenance,
  plainRecord,
  sha256Bytes,
  WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
  WATCHER_CANONICAL_CONTENT_KINDS,
  WATCHER_CANONICAL_RECORD_KIND_BY_CONTENT_KIND,
  WATCHER_CANONICAL_SLOT_LENGTH_MS,
  type WatcherCanonicalBlockRecord,
  type WatcherCanonicalRecordMetadata,
} from "./canonical-block-store.parse-provenance.js";
import { type WatcherDaProofInput } from "./durable-store.js";

const parseDurableInput = (
  value: unknown,
  path: string,
): WatcherDaProofInput => {
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
  if (bytes.length > DA_TRANSPORT_LIMITS.maxPayloadBytes) {
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
 * `verifyWatcherCanonicalRecord`.
 */
export const parseWatcherCanonicalBlockRecord = (
  value: unknown,
  path = "$",
): WatcherCanonicalBlockRecord => {
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
    WATCHER_CANONICAL_CONTENT_KINDS,
  );
  const kind = exactLiteral(metadataRecord.kind, `${metadataPath}.kind`, [
    "da_payload",
    "proof_input",
  ] as const);
  if (kind !== WATCHER_CANONICAL_RECORD_KIND_BY_CONTENT_KIND[contentKind]) {
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
  const metadata: WatcherCanonicalRecordMetadata = Object.freeze({
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
export const verifyWatcherCanonicalRecord = async (
  value: unknown,
  path = "$",
): Promise<WatcherCanonicalBlockRecord> => {
  const record = parseWatcherCanonicalBlockRecord(value, path);
  if (record.metadata.contentKind === "da_payload") {
    const envelope = Buffer.from(record.input.payload.cborHex, "hex");
    let innerBytes: Buffer;
    try {
      innerBytes = (
        await unwrapDaPayload(envelope, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
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

export type WatcherCanonicalRetentionWindow = Readonly<{
  schemaVersion: typeof WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION;
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

export const msToSlots = (milliseconds: number): number =>
  Math.floor(milliseconds / WATCHER_CANONICAL_SLOT_LENGTH_MS);

const manifestRetentionDays = (manifest: unknown, path: string): unknown => {
  const root = plainRecord(manifest, path);
  const da = plainRecord(root.da, `${path}.da`);
  const transportProfile = plainRecord(
    da.transportProfile,
    `${path}.da.transportProfile`,
  );
  return transportProfile.retentionDays;
};

export const assertRetentionDays = (value: unknown, path: string): number => {
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

export const makeRetentionWindow = (
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
    schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
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
