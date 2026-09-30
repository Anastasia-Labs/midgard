import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { encodeDaPayload } from "@al-ft/midgard-sdk";
import { beforeEach, describe, expect, it } from "vitest";

import {
  loadWatcherCanonicalBlockStore,
  makeWatcherCanonicalDaPayloadRecord,
  makeWatcherCanonicalEventToStepRecord,
  makeWatcherCanonicalProofBundleRecord,
  makeWatcherCanonicalTraceStepRecord,
  persistWatcherCanonicalPublicBytes,
  type WatcherCanonicalBlockRecord,
  WatcherCanonicalBlockStoreError,
  type WatcherCanonicalBlockStoreErrorCode,
  type WatcherCanonicalRecordContext,
  type WatcherCanonicalRetentionWindow,
  watcherCanonicalRetentionWindowFromVerifiedManifest,
} from "../../src/storage/canonical-block-store.js";
import {
  watcherCanonicalJson,
  type WatcherDurableAtomicBackend,
  watcherDurableStoreBytesSha256,
} from "../../src/storage/durable-store.js";
import {
  clientFor,
  daPayload,
  EVENT_ENTRY_BYTES,
  EVENT_KEY,
  EVENT_PROOF_BYTES,
  FINGERPRINT,
  HEADER_HASH,
  identityOf,
  MARKER,
  nonmembershipClient,
  PROOF_BUNDLE_BYTES,
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

type MutableRecord = Record<string, any>;

export const cloned = (record: WatcherCanonicalBlockRecord): MutableRecord =>
  JSON.parse(JSON.stringify(record)) as MutableRecord;

export let envelope: Buffer;

export let innerCbor: Buffer;

export let payloadRecord: WatcherCanonicalBlockRecord;

beforeEach(async () => {
  innerCbor = encodeDaPayload(daPayload(HEADER_HASH));
  envelope = await wrapDaPayload(innerCbor, { mode: "identity" });
  const payload = await clientFor(envelope).fetchPayloadByHeader({
    headerHash: HEADER_HASH,
  });
  payloadRecord = makeWatcherCanonicalDaPayloadRecord({
    payload,
    context: contextOf(),
  });
});

// ---------------------------------------------------------------------------
// 1. Hash-addressed persistence of real public DA bytes
// ---------------------------------------------------------------------------

describe("W21 canonical block store: hash-addressed persistence", () => {
  it("persists the exact envelope bytes returned by the public DA client", async () => {
    const backend = new MemoryAtomicBackend();
    const result = await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });

    expect(result.committed).toBe(true);
    expect(result.alreadyPresent).toBe(false);
    expect(result.revision).toBe("1");
    expect(backend.writes).toBe(1);
    expect(payloadRecord.input.payload.cborHex).toBe(envelope.toString("hex"));
    expect(payloadRecord.metadata.byteLength).toBe(envelope.length);
  });

  it("records BOTH digests: envelopeSha256 addresses the record, innerSha256 binds the inner payload", () => {
    expect(payloadRecord.metadata.envelopeSha256).toBe(sha256Hex(envelope));
    expect(payloadRecord.metadata.innerSha256).toBe(sha256Hex(innerCbor));
    expect(payloadRecord.metadata.envelopeSha256).not.toBe(
      payloadRecord.metadata.innerSha256,
    );
    // Hash addressing: the client's inputId is the envelope digest.
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

  it("reloads byte-identical bytes under the same inputId after a restart", async () => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });

    const loaded = await loadWatcherCanonicalBlockStore({
      backend,
      deploymentIdentity: identityOf(),
      retentionWindow: windowFor(),
    });
    expect(loaded).not.toBeNull();
    const stored = loaded!.snapshot.records[0]!;
    expect(stored.input.inputId).toBe(payloadRecord.input.inputId);
    expect(Buffer.from(stored.input.payload.cborHex, "hex")).toEqual(envelope);
    expect(watcherCanonicalJson(stored)).toBe(
      watcherCanonicalJson(payloadRecord),
    );
  });

  it("persists proof-bundle, trace-step, and event-to-step artifacts under proof_input", async () => {
    const client = clientFor(envelope);
    const backend = new MemoryAtomicBackend();
    const identity = identityOf();

    const bundle = makeWatcherCanonicalProofBundleRecord({
      proofBundle: await client.fetchProofBundleByHeader({
        headerHash: HEADER_HASH,
      }),
      context: contextOf(),
    });
    const traceStep = makeWatcherCanonicalTraceStepRecord({
      traceStep: await client.fetchTraceStepByIndex({
        headerHash: HEADER_HASH,
        stepIndex: 3,
      }),
      context: contextOf(),
    });
    const entry = makeWatcherCanonicalEventToStepRecord({
      eventToStep: await client.fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey: EVENT_KEY,
      }),
      context: contextOf(),
    });
    const nonmembership = makeWatcherCanonicalEventToStepRecord({
      eventToStep: await nonmembershipClient().fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey: EVENT_KEY,
      }),
      context: contextOf(),
    });

    for (const record of [
      payloadRecord,
      bundle,
      traceStep,
      entry,
      nonmembership,
    ]) {
      await persistWatcherCanonicalPublicBytes({
        backend,
        deploymentIdentity: identity,
        record,
      });
    }

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

    const loaded = await loadWatcherCanonicalBlockStore({
      backend,
      deploymentIdentity: identity,
    });
    expect(loaded!.snapshot.records).toHaveLength(5);
    const inputIds = loaded!.snapshot.records.map(
      (record) => record.input.inputId,
    );
    expect([...inputIds].sort()).toEqual(inputIds);
  });

  it("is an idempotent no-op when the identical bytes are persisted twice", async () => {
    const backend = new MemoryAtomicBackend();
    const identity = identityOf();
    const first = await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identity,
      record: payloadRecord,
    });
    const before = Uint8Array.from(backend.bytes!);

    const second = await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identity,
      record: payloadRecord,
    });

    expect(second.committed).toBe(false);
    expect(second.alreadyPresent).toBe(true);
    expect(second.inputId).toBe(first.inputId);
    expect(second.snapshotSha256).toBe(first.snapshotSha256);
    expect(backend.writes).toBe(1);
    expect(backend.bytes).toEqual(before);
  });
});
