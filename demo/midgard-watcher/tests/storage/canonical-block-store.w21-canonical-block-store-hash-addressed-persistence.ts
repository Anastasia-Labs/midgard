import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { encodeDaPayload } from "@al-ft/midgard-sdk";
import { beforeEach, describe, expect, it } from "vitest";

import {
  encodeWatcherCanonicalBlockStoreSnapshot,
  makeEmptyWatcherCanonicalBlockStoreSnapshot,
  makeWatcherCanonicalDaPayloadRecord,
  makeWatcherCanonicalEventToStepRecord,
  makeWatcherCanonicalProofBundleRecord,
  makeWatcherCanonicalTraceStepRecord,
  parseWatcherCanonicalBlockStoreSnapshot,
  type WatcherCanonicalBlockRecord,
  WatcherCanonicalBlockStoreError,
  type WatcherCanonicalBlockStoreErrorCode,
  type WatcherCanonicalRecordContext,
  type WatcherCanonicalRetentionWindow,
  watcherCanonicalRetentionWindowFromVerifiedManifest,
} from "../../src/storage/canonical-block-store.js";
import {
  type WatcherDurableAtomicBackend,
  watcherDurableStoreBytesSha256,
} from "../../src/storage/durable-store.js";
import {
  daPayload,
  EVENT_ENTRY_BYTES,
  EVENT_PROOF_BYTES,
  FINGERPRINT,
  HEADER_HASH,
  MARKER,
  PROOF_BUNDLE_BYTES,
  publicDaEventToStep,
  publicDaPayload,
  publicDaProofBundle,
  publicDaTraceStep,
  sha256Hex,
  TRACE_PROOF_BYTES,
  TRACE_STEP_BYTES,
} from "./canonical-block-store.config-of.js";

/** The only backend under test: a pure in-memory atomic snapshot cell. */
export class MemoryAtomicBackend implements WatcherDurableAtomicBackend {
  bytes: Uint8Array | null;
  writes = 0;
  reads = 0;
  failRead = false;
  failBeforeCommit = false;
  failAfterCommit = false;
  alwaysConflict = false;
  conflictOnce = false;

  constructor(bytes: Uint8Array | null = null) {
    this.bytes = bytes;
  }

  async read(): Promise<Uint8Array | null> {
    this.reads += 1;
    if (this.failRead) {
      throw new Error("simulated read fault");
    }
    return this.bytes === null ? null : Uint8Array.from(this.bytes);
  }

  async compareAndSwap(
    expectedSha256: string | null,
    next: Uint8Array,
  ): Promise<boolean> {
    if (this.alwaysConflict) {
      return false;
    }
    if (this.conflictOnce) {
      this.conflictOnce = false;
      return false;
    }
    const actualSha256 =
      this.bytes === null ? null : watcherDurableStoreBytesSha256(this.bytes);
    if (actualSha256 !== expectedSha256) {
      return false;
    }
    if (this.failBeforeCommit) {
      this.failBeforeCommit = false;
      throw new Error("simulated crash before atomic commit");
    }
    this.bytes = Uint8Array.from(next);
    this.writes += 1;
    if (this.failAfterCommit) {
      this.failAfterCommit = false;
      throw new Error("simulated process loss after atomic commit");
    }
    return true;
  }
}

export const manifestWith = (retentionDays: unknown): unknown => ({
  da: { transportProfile: { retentionDays } },
});

export const windowFor = (
  retentionDays: unknown = DA_TRANSPORT_LIMITS.minimumRetentionDays,
): WatcherCanonicalRetentionWindow =>
  watcherCanonicalRetentionWindowFromVerifiedManifest({
    manifest: manifestWith(retentionDays),
    manifestId: FINGERPRINT,
    deploymentMarker: MARKER,
  });

export const OBSERVED_AT_SLOT = 1_000;

export const contextOf = (
  overrides: Partial<WatcherCanonicalRecordContext> = {},
): WatcherCanonicalRecordContext => ({
  window: windowFor(),
  deploymentMarker: MARKER,
  observedAtSlot: OBSERVED_AT_SLOT,
  ...overrides,
});

export const expectStoreError = async (
  operation: () => unknown,
  code: WatcherCanonicalBlockStoreErrorCode,
): Promise<WatcherCanonicalBlockStoreError> => {
  try {
    await operation();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherCanonicalBlockStoreError);
    expect((error as WatcherCanonicalBlockStoreError).code).toBe(code);
    return error as WatcherCanonicalBlockStoreError;
  }
  throw new Error(`Expected canonical block store rejection ${code}`);
};

/** A store holding `records` at revision 1, written without a backend write. */
export const storeOf = (
  ...records: readonly WatcherCanonicalBlockRecord[]
): MemoryAtomicBackend =>
  new MemoryAtomicBackend(
    encodeWatcherCanonicalBlockStoreSnapshot(
      parseWatcherCanonicalBlockStoreSnapshot({
        ...makeEmptyWatcherCanonicalBlockStoreSnapshot(MARKER),
        revision: "1",
        records: [...records].sort((left, right) =>
          left.input.inputId < right.input.inputId ? -1 : 1,
        ),
      }),
    ),
  );

type MutableRecord = Record<string, any>;

export const cloned = (record: WatcherCanonicalBlockRecord): MutableRecord =>
  JSON.parse(JSON.stringify(record)) as MutableRecord;

export let envelope: Buffer;

export let innerCbor: Buffer;

export let payloadRecord: WatcherCanonicalBlockRecord;

beforeEach(async () => {
  innerCbor = encodeDaPayload(daPayload(HEADER_HASH));
  envelope = await wrapDaPayload(innerCbor, { mode: "identity" });
  const payload = await publicDaPayload(envelope);
  payloadRecord = makeWatcherCanonicalDaPayloadRecord({
    payload,
    context: contextOf(),
  });
});

// ---------------------------------------------------------------------------
// 1. Hash-addressed records of real public DA bytes
// ---------------------------------------------------------------------------

describe("W21 canonical block store: hash-addressed records", () => {
  it("records the exact envelope bytes of a verified public DA payload", () => {
    expect(payloadRecord.input.payload.cborHex).toBe(envelope.toString("hex"));
    expect(payloadRecord.metadata.byteLength).toBe(envelope.length);
  });

  it("records BOTH digests: envelopeSha256 addresses the record, innerSha256 binds the inner payload", () => {
    expect(payloadRecord.metadata.envelopeSha256).toBe(sha256Hex(envelope));
    expect(payloadRecord.metadata.innerSha256).toBe(sha256Hex(innerCbor));
    expect(payloadRecord.metadata.envelopeSha256).not.toBe(
      payloadRecord.metadata.innerSha256,
    );
    // Hash addressing: the record's inputId is the envelope digest.
    expect(payloadRecord.input.inputId).toBe(
      payloadRecord.metadata.envelopeSha256,
    );
    expect(payloadRecord.input.payload.sha256).toBe(
      payloadRecord.metadata.envelopeSha256,
    );
    expect(payloadRecord.metadata.contentKind).toBe("da_payload");
    expect(payloadRecord.input.kind).toBe("da_payload");
    expect(payloadRecord.metadata.provenance.trustClass).toBe(
      "public_or_permissionless_da",
    );
  });

  it("records proof-bundle, trace-step, and event-to-step artifacts under proof_input", () => {
    const bundle = makeWatcherCanonicalProofBundleRecord({
      proofBundle: publicDaProofBundle(),
      context: contextOf(),
    });
    const traceStep = makeWatcherCanonicalTraceStepRecord({
      traceStep: publicDaTraceStep(),
      context: contextOf(),
    });
    const entry = makeWatcherCanonicalEventToStepRecord({
      eventToStep: publicDaEventToStep(),
      context: contextOf(),
    });
    const nonmembership = makeWatcherCanonicalEventToStepRecord({
      eventToStep: publicDaEventToStep(null),
      context: contextOf(),
    });

    expect(bundle.metadata.contentKind).toBe("proof_bundle");
    expect(bundle.input.kind).toBe("proof_input");
    expect(bundle.metadata.innerSha256).toBeNull();
    expect(bundle.metadata.envelopeSha256).toBe(sha256Hex(PROOF_BUNDLE_BYTES));

    expect(traceStep.metadata.contentKind).toBe("trace_step");
    expect(traceStep.metadata.envelopeSha256).toBe(sha256Hex(TRACE_STEP_BYTES));
    expect(traceStep.metadata.innerSha256).toBe(sha256Hex(TRACE_PROOF_BYTES));

    expect(entry.metadata.contentKind).toBe("event_to_step_entry");
    expect(entry.metadata.envelopeSha256).toBe(sha256Hex(EVENT_ENTRY_BYTES));
    expect(entry.metadata.innerSha256).toBe(sha256Hex(EVENT_PROOF_BYTES));

    expect(nonmembership.metadata.contentKind).toBe(
      "event_to_step_nonmembership",
    );
    expect(nonmembership.metadata.envelopeSha256).toBe(
      sha256Hex(EVENT_PROOF_BYTES),
    );
    expect(nonmembership.metadata.innerSha256).toBeNull();
  });
});
