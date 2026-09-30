import {
  DA_TRANSPORT_LIMITS,
  type DeploymentMarker,
  MIDGARD_MIN_RETENTION_DAYS,
} from "@al-ft/midgard-core";
import type { EvidenceProvenance } from "@al-ft/midgard-sdk";

import {
  type VerifiedWatcherDeploymentIdentity,
  verifyWatcherDeploymentIdentity,
  type WatcherDeploymentIdentityPolicy,
  type WatcherDeploymentTrustRoot,
} from "../runtime/deployment-identity.js";
import {
  CANONICAL_NATURAL,
  exactRecord,
  exactSlot,
  exactString,
  fail,
  markersMatch,
  parseMarker,
  plainRecord,
  sha256Bytes,
  WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
  type WatcherCanonicalBlockRecord,
  type WatcherCanonicalBlockStoreSnapshot,
  type WatcherCanonicalContentKind,
} from "./canonical-block-store.parse-provenance.js";
import {
  assertRetentionDays,
  makeRetentionWindow,
  parseWatcherCanonicalBlockRecord,
  type WatcherCanonicalRetentionWindow,
  watcherCanonicalRetentionWindowFromVerifiedManifest,
} from "./canonical-block-store.parse-watcher-canonical-block-record.js";
import { type WatcherDaProofInput } from "./durable-store.js";
import type {
  WatcherPublicDaEventToStep,
  WatcherPublicDaPayload,
  WatcherPublicDaProofBundle,
  WatcherPublicDaTraceStep,
} from "./public-da-client.js";

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
  if (window.schemaVersion !== WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION) {
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
export const watcherCanonicalRetainUntilSlot = (input: {
  readonly window: WatcherCanonicalRetentionWindow;
  readonly observedAtSlot: number;
}): number => {
  const window = assertWatcherCanonicalRetentionWindow(input.window);
  const observedAtSlot = exactSlot(input.observedAtSlot, "$.observedAtSlot");
  return observedAtSlot + window.retentionSlots;
};

/** Whole days of retention this deployment must promise, at minimum. */
export const WATCHER_CANONICAL_MIN_RETENTION_DAYS = Math.max(
  MIDGARD_MIN_RETENTION_DAYS,
  DA_TRANSPORT_LIMITS.minimumRetentionDays,
);

// ---------------------------------------------------------------------------
// Record builders over the W20 client's return values
// ---------------------------------------------------------------------------

export type WatcherCanonicalRecordContext = Readonly<{
  window: WatcherCanonicalRetentionWindow;
  deploymentMarker: DeploymentMarker;
  observedAtSlot: number;
}>;

const buildRecord = (input: {
  readonly durableInput: WatcherDaProofInput;
  readonly contentKind: WatcherCanonicalContentKind;
  readonly headerHash: string;
  readonly bytes: Uint8Array;
  readonly innerSha256: string | null;
  readonly sourcePeerIdentity: string;
  readonly sourcePeerId: string;
  readonly provenance: EvidenceProvenance;
  readonly context: WatcherCanonicalRecordContext;
}): WatcherCanonicalBlockRecord =>
  parseWatcherCanonicalBlockRecord({
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
      retainUntilSlot: watcherCanonicalRetainUntilSlot({
        window: input.context.window,
        observedAtSlot: input.context.observedAtSlot,
      }),
    },
  });

/** Block-body payload: stored envelope bytes plus the unwrapped inner digest. */
export const makeWatcherCanonicalDaPayloadRecord = (input: {
  readonly payload: WatcherPublicDaPayload;
  readonly context: WatcherCanonicalRecordContext;
}): WatcherCanonicalBlockRecord =>
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
export const makeWatcherCanonicalProofBundleRecord = (input: {
  readonly proofBundle: WatcherPublicDaProofBundle;
  readonly context: WatcherCanonicalRecordContext;
}): WatcherCanonicalBlockRecord =>
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
export const makeWatcherCanonicalTraceStepRecord = (input: {
  readonly traceStep: WatcherPublicDaTraceStep;
  readonly context: WatcherCanonicalRecordContext;
}): WatcherCanonicalBlockRecord =>
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
export const makeWatcherCanonicalEventToStepRecord = (input: {
  readonly eventToStep: WatcherPublicDaEventToStep;
  readonly context: WatcherCanonicalRecordContext;
}): WatcherCanonicalBlockRecord => {
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

export const parseWatcherCanonicalBlockStoreSnapshot = (
  value: unknown,
): WatcherCanonicalBlockStoreSnapshot => {
  const record = exactRecord(value, "$", SNAPSHOT_KEYS);
  if (record.schemaVersion !== WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION) {
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
    parseWatcherCanonicalBlockRecord(member, `$.records[${String(index)}]`),
  );
  for (let index = 0; index < records.length; index += 1) {
    const current = records[index] as WatcherCanonicalBlockRecord;
    if (!markersMatch(deploymentMarker, current.metadata.deploymentMarker)) {
      fail(
        "deployment_marker_mismatch",
        `$.records[${String(index)}].metadata.deploymentMarker`,
      );
    }
    if (index === 0) {
      continue;
    }
    const previous = records[index - 1] as WatcherCanonicalBlockRecord;
    if (current.input.inputId === previous.input.inputId) {
      fail("content_conflict", `$.records[${String(index)}].input.inputId`);
    }
    if (current.input.inputId < previous.input.inputId) {
      fail("unsorted_records", "$.records");
    }
  }
  return Object.freeze({
    schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
    revision,
    deploymentMarker,
    records: Object.freeze(records),
  });
};
