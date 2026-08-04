import { createHash } from "node:crypto";

import {
  DA_PAYLOAD_V1_VERSION,
  type DaPayloadV1,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayloadV1,
} from "@al-ft/midgard-sdk";
import { beforeEach, describe, expect, it } from "vitest";

import {
  DA_PAYLOAD_INNER_V1_SCHEMA_VERSION,
  DaPayloadContentEncoding,
  wrapDaPayloadV1,
} from "../../midgard-core/src/da-payload-envelope.js";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
  type DaCapabilitiesResponseV1,
  daDeploymentFingerprintFromHex,
  type DaPayloadByHeaderResponseV1,
  encodeDaCapabilitiesResponseV1Cbor,
  encodeDaEventToStepByEventResponseV1Cbor,
  encodeDaPayloadByHeaderResponseV1Cbor,
  encodeDaProofBundleByHeaderResponseV1Cbor,
  encodeDaTraceStepByIndexResponseV1Cbor,
} from "../../midgard-core/src/da-transport.js";
import { makeDeploymentMarkerV1 } from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import {
  MIDGARD_RETENTION_WINDOW_V1,
  RETENTION_MS_PER_DAY_V1,
} from "../../midgard-core/src/retention-window-v1.js";
import {
  decodeWatcherCanonicalBlockStoreSnapshotV1,
  encodeWatcherCanonicalBlockStoreSnapshotV1,
  loadWatcherCanonicalBlockStoreV1,
  makeWatcherCanonicalDaPayloadRecordV1,
  makeWatcherCanonicalEventToStepRecordV1,
  makeWatcherCanonicalProofBundleRecordV1,
  makeWatcherCanonicalTraceStepRecordV1,
  parseWatcherCanonicalBlockRecordV1,
  persistWatcherCanonicalPublicBytesV1,
  pruneWatcherCanonicalBlockStoreV1,
  resolveWatcherCanonicalRetentionWindowV1,
  WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION,
  WATCHER_CANONICAL_SLOT_LENGTH_MS_V1,
  type WatcherCanonicalBlockRecordV1,
  WatcherCanonicalBlockStoreError,
  type WatcherCanonicalBlockStoreErrorCode,
  type WatcherCanonicalRecordContextV1,
  watcherCanonicalRetainUntilSlotV1,
  watcherCanonicalRetentionWindowFromVerifiedManifestV1,
  type WatcherCanonicalRetentionWindowV1,
} from "../src/canonical-block-store.js";
import {
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
} from "../src/config.js";
import type { VerifiedWatcherDeploymentIdentityV1 } from "../src/deployment-identity.js";
import {
  watcherCanonicalJsonV1,
  type WatcherDurableAtomicBackend,
  watcherDurableStoreBytesSha256,
} from "../src/durable-store.js";
import {
  WatcherPublicDaClientV1,
  type WatcherPublicDaLibp2pTransportV1,
  type WatcherPublicDaRequestV1,
} from "../src/public-da-client.js";

// ---------------------------------------------------------------------------
// Fixtures (public DA client wiring mirrors tests/public-da-client.test.ts)
// ---------------------------------------------------------------------------

const repeatHex = (value: number, length: number): string =>
  value.toString(16).padStart(2, "0").repeat(length);

const FINGERPRINT = repeatHex(0x1a, 32);
const HEADER_HASH = repeatHex(0xab, 28);
const OTHER_MANIFEST_ID = repeatHex(0x7e, 32);
const PEER = "da-peer-a";
const MULTIADDR =
  "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz1234A";
const EVENT_KEY = "0a1b2c3d";

const MARKER = makeDeploymentMarkerV1(FINGERPRINT);

const sha256Hex = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const configOf = (): WatcherConfig =>
  ({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: {
      source: {
        sourceMode: "external_providers",
        providers: [
          {
            identity: "provider-a",
            operatorIdentitySha256: repeatHex(0x11, 32),
            endpoint: "https://cardano-a.example",
          },
          {
            identity: "provider-b",
            operatorIdentitySha256: repeatHex(0x22, 32),
            endpoint: "https://cardano-b.example",
          },
        ],
      },
      requestTimeoutMs: 10_000,
      maxConcurrency: 8,
      finality: {
        depth: 15,
        rollback: {
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: 15,
        },
      },
    },
    da: {
      peers: [{ identity: PEER, multiaddr: MULTIADDR }],
      requestTimeoutMs: 10_000,
      maxConcurrency: 8,
    },
    storage: {
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
      rollbackAuthorityKeySource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
      },
    },
    proverWallet: {
      keySource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_PROVER_KEY",
      },
    },
    deadlines: {
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    },
  }) as unknown as WatcherConfig;

const identityOf = (
  manifestId: string = FINGERPRINT,
): VerifiedWatcherDeploymentIdentityV1 => ({
  manifestId,
  network: "Preprod",
  trustRootId: "trust-root-a",
  releaseEvidenceDigest: repeatHex(0x33, 32),
  ruleBundleCommitment: repeatHex(0x44, 32),
  programCommitments: {},
  durableMarker: makeDeploymentMarkerV1(manifestId),
});

const daPayload = (headerHash: string): DaPayloadV1 => {
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  return {
    version: DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: headerHash,
      header: {
        prevUtxosRoot: repeatHex(0x01, 32),
        utxosRoot: repeatHex(0x02, 32),
        withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: repeatHex(0x03, 32),
        depositsRoot: EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: repeatHex(0x04, 32),
        eventToStepRoot: repeatHex(0x05, 32),
        validationTracesRoot: repeatHex(0x06, 32),
        ...counts,
        startTime: 1_000n,
        endTime: 1_999n,
        blockSlot: 42n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: repeatHex(0x07, 28),
        operatorVkey: repeatHex(0x08, 28),
        protocolVersion: 1n,
      },
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [[repeatHex(0x09, 32), repeatHex(0x0a, 40)]],
      transaction_preimages: [[repeatHex(0x09, 32), repeatHex(0x0b, 64)]],
      forced_transaction_preimages: [],
      cek_program_material: [],
      deposits: [],
      transition_trace: [[repeatHex(0x0c, 32), repeatHex(0x0d, 48)]],
      event_to_step: [[repeatHex(0x0e, 32), repeatHex(0x0f, 8)]],
      validation_traces: [[repeatHex(0x10, 32), repeatHex(0x11, 24)]],
      counts,
    },
  };
};

type ProtocolHandler = (
  request: WatcherPublicDaRequestV1,
) => Promise<Uint8Array> | Uint8Array;

class ScriptedTransport implements WatcherPublicDaLibp2pTransportV1 {
  constructor(private readonly script: Record<string, ProtocolHandler>) {}

  async request(request: WatcherPublicDaRequestV1): Promise<Uint8Array> {
    const handler = this.script[request.protocol];
    if (handler === undefined) {
      throw new Error(`unscripted protocol ${request.protocol}`);
    }
    return handler(request);
  }
}

const capabilitiesBytes = (
  overrides: Partial<DaCapabilitiesResponseV1> = {},
): Buffer =>
  encodeDaCapabilitiesResponseV1Cbor({
    deploymentFingerprint: daDeploymentFingerprintFromHex(FINGERPRINT),
    transportProtocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
    payloadSchemaVersions: [DA_PAYLOAD_INNER_V1_SCHEMA_VERSION],
    envelopeContentEncodings: [
      DaPayloadContentEncoding.identity,
      DaPayloadContentEncoding.zstd,
    ],
    maxPayloadBytes: 1_000_000,
    maxInlineResponseBytes: 500_000,
    maxChunkBytes: 250_000,
    maxStreamsPerPeer: 8,
    requestTimeoutMs: 10_000,
    ...overrides,
  });

const payloadByHeaderBytes = (
  overrides: Partial<DaPayloadByHeaderResponseV1>,
): Buffer =>
  encodeDaPayloadByHeaderResponseV1Cbor({
    status: "found_inline",
    headerHash: Buffer.from(HEADER_HASH, "hex"),
    payloadHash: null,
    payloadBytes: null,
    chunkManifest: null,
    reasonCode: null,
    ...overrides,
  });

const PROOF_BUNDLE_BYTES = Buffer.alloc(96, 0x5a);
const TRACE_STEP_BYTES = Buffer.alloc(64, 0x6b);
const TRACE_PROOF_BYTES = Buffer.alloc(48, 0x7c);
const EVENT_ENTRY_BYTES = Buffer.alloc(32, 0x8d);
const EVENT_PROOF_BYTES = Buffer.alloc(24, 0x9e);

const clientFor = (envelope: Buffer): WatcherPublicDaClientV1 =>
  new WatcherPublicDaClientV1({
    config: configOf(),
    deploymentIdentity: identityOf(),
    transport: new ScriptedTransport({
      capabilities: () => capabilitiesBytes(),
      "payload-by-header": () =>
        payloadByHeaderBytes({
          payloadHash: computeDaSha256Hash(envelope),
          payloadBytes: envelope,
        }),
      "proof-bundle-by-header": () =>
        encodeDaProofBundleByHeaderResponseV1Cbor({
          status: "found_inline",
          headerHash: Buffer.from(HEADER_HASH, "hex"),
          proofBundleHash: computeDaSha256Hash(PROOF_BUNDLE_BYTES),
          proofBundleBytes: PROOF_BUNDLE_BYTES,
          chunkManifest: null,
          reasonCode: null,
        }),
      "trace-step-by-index": () =>
        encodeDaTraceStepByIndexResponseV1Cbor({
          status: "found",
          headerHash: Buffer.from(HEADER_HASH, "hex"),
          stepIndex: 3,
          transitionStepBytes: TRACE_STEP_BYTES,
          membershipProofBytes: TRACE_PROOF_BYTES,
        }),
      "event-to-step-by-event": () =>
        encodeDaEventToStepByEventResponseV1Cbor({
          status: "found",
          headerHash: Buffer.from(HEADER_HASH, "hex"),
          eventKey: Buffer.from(EVENT_KEY, "hex"),
          eventToStepEntryBytes: EVENT_ENTRY_BYTES,
          membershipOrNonmembershipProofBytes: EVENT_PROOF_BYTES,
        }),
    }),
  });

const nonmembershipClient = (): WatcherPublicDaClientV1 =>
  new WatcherPublicDaClientV1({
    config: configOf(),
    deploymentIdentity: identityOf(),
    transport: new ScriptedTransport({
      capabilities: () => capabilitiesBytes(),
      "event-to-step-by-event": () =>
        encodeDaEventToStepByEventResponseV1Cbor({
          status: "found",
          headerHash: Buffer.from(HEADER_HASH, "hex"),
          eventKey: Buffer.from(EVENT_KEY, "hex"),
          eventToStepEntryBytes: null,
          membershipOrNonmembershipProofBytes: EVENT_PROOF_BYTES,
        }),
    }),
  });

/** The only backend under test: a pure in-memory atomic snapshot cell. */
class MemoryAtomicBackend implements WatcherDurableAtomicBackend {
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

const manifestWith = (retentionDays: unknown): unknown => ({
  da: { transportProfile: { retentionDays } },
});

const windowFor = (
  retentionDays: unknown = DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
): WatcherCanonicalRetentionWindowV1 =>
  watcherCanonicalRetentionWindowFromVerifiedManifestV1({
    manifest: manifestWith(retentionDays),
    manifestId: FINGERPRINT,
    deploymentMarker: MARKER,
  });

const OBSERVED_AT_SLOT = 1_000;

const contextOf = (
  overrides: Partial<WatcherCanonicalRecordContextV1> = {},
): WatcherCanonicalRecordContextV1 => ({
  window: windowFor(),
  deploymentMarker: MARKER,
  observedAtSlot: OBSERVED_AT_SLOT,
  ...overrides,
});

const expectStoreError = async (
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

const cloned = (record: WatcherCanonicalBlockRecordV1): MutableRecord =>
  JSON.parse(JSON.stringify(record)) as MutableRecord;

let envelope: Buffer;
let innerCbor: Buffer;
let payloadRecord: WatcherCanonicalBlockRecordV1;

beforeEach(async () => {
  innerCbor = encodeDaPayloadV1(daPayload(HEADER_HASH));
  envelope = await wrapDaPayloadV1(innerCbor, { mode: "identity" });
  const payload = await clientFor(envelope).fetchPayloadByHeader({
    headerHash: HEADER_HASH,
  });
  payloadRecord = makeWatcherCanonicalDaPayloadRecordV1({
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
    const result = await persistWatcherCanonicalPublicBytesV1({
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
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });

    const loaded = await loadWatcherCanonicalBlockStoreV1({
      backend,
      deploymentIdentity: identityOf(),
      retentionWindow: windowFor(),
    });
    expect(loaded).not.toBeNull();
    const stored = loaded!.snapshot.records[0]!;
    expect(stored.input.inputId).toBe(payloadRecord.input.inputId);
    expect(Buffer.from(stored.input.payload.cborHex, "hex")).toEqual(envelope);
    expect(watcherCanonicalJsonV1(stored)).toBe(
      watcherCanonicalJsonV1(payloadRecord),
    );
  });

  it("persists proof-bundle, trace-step, and event-to-step artifacts under proof_input", async () => {
    const client = clientFor(envelope);
    const backend = new MemoryAtomicBackend();
    const identity = identityOf();

    const bundle = makeWatcherCanonicalProofBundleRecordV1({
      proofBundle: await client.fetchProofBundleByHeader({
        headerHash: HEADER_HASH,
      }),
      context: contextOf(),
    });
    const traceStep = makeWatcherCanonicalTraceStepRecordV1({
      traceStep: await client.fetchTraceStepByIndex({
        headerHash: HEADER_HASH,
        stepIndex: 3,
      }),
      context: contextOf(),
    });
    const entry = makeWatcherCanonicalEventToStepRecordV1({
      eventToStep: await client.fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey: EVENT_KEY,
      }),
      context: contextOf(),
    });
    const nonmembership = makeWatcherCanonicalEventToStepRecordV1({
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
      await persistWatcherCanonicalPublicBytesV1({
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

    const loaded = await loadWatcherCanonicalBlockStoreV1({
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
    const first = await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identity,
      record: payloadRecord,
    });
    const before = Uint8Array.from(backend.bytes!);

    const second = await persistWatcherCanonicalPublicBytesV1({
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

// ---------------------------------------------------------------------------
// 2. Immutability
// ---------------------------------------------------------------------------

describe("W21 canonical block store: immutability", () => {
  it("refuses a different record under an existing inputId and keeps the original", async () => {
    const backend = new MemoryAtomicBackend();
    const identity = identityOf();
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identity,
      record: payloadRecord,
    });
    const before = Uint8Array.from(backend.bytes!);

    const restated = cloned(payloadRecord);
    restated.metadata.observedAtSlot = OBSERVED_AT_SLOT + 1;
    restated.metadata.retainUntilSlot = watcherCanonicalRetainUntilSlotV1({
      window: windowFor(),
      observedAtSlot: OBSERVED_AT_SLOT + 1,
    });

    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identity,
          record: restated as unknown as WatcherCanonicalBlockRecordV1,
        }),
      "content_conflict",
    );
    expect(backend.bytes).toEqual(before);
    expect(backend.writes).toBe(1);
  });

  it("refuses different bytes claiming an existing inputId and keeps the original", async () => {
    const backend = new MemoryAtomicBackend();
    const identity = identityOf();
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identity,
      record: payloadRecord,
    });
    const before = Uint8Array.from(backend.bytes!);

    const forged = cloned(payloadRecord);
    forged.input.payload.cborHex = Buffer.alloc(64, 0x01).toString("hex");

    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identity,
          record: forged as unknown as WatcherCanonicalBlockRecordV1,
        }),
      "integrity_mismatch",
    );
    expect(backend.bytes).toEqual(before);
    expect(backend.writes).toBe(1);
  });

  it("refuses a snapshot that carries the same inputId twice", () => {
    const duplicated = {
      schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION,
      revision: "2",
      deploymentMarker: { ...MARKER },
      records: [cloned(payloadRecord), cloned(payloadRecord)],
    };
    expect(() =>
      decodeWatcherCanonicalBlockStoreSnapshotV1(
        new TextEncoder().encode(watcherCanonicalJsonV1(duplicated)),
      ),
    ).toThrowError(WatcherCanonicalBlockStoreError);
  });
});

// ---------------------------------------------------------------------------
// 3. Retention window (Q54 binding) and prune boundaries
// ---------------------------------------------------------------------------

describe("W21 canonical block store: retention window", () => {
  it("derives the window from the manifest with the Q54 arithmetic", () => {
    const window = windowFor();
    expect(window.retentionDays).toBe(
      DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
    );
    expect(window.deployedRetentionMs).toBe(
      window.retentionDays * RETENTION_MS_PER_DAY_V1,
    );
    expect(window.requiredRetentionMs).toBe(
      MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
    );
    expect(window.maturityMs).toBe(MIDGARD_RETENTION_WINDOW_V1.maturityMs);
    expect(window.worstCaseProofTimeBoundMs).toBe(
      MIDGARD_RETENTION_WINDOW_V1.maturityMs / 2,
    );
    expect(window.retentionSlots).toBe(
      window.deployedRetentionMs / WATCHER_CANONICAL_SLOT_LENGTH_MS_V1,
    );
    expect(window.marginMs).toBeGreaterThan(0);
  });

  it("accepts the manifest floor and one day above it, and rejects one day below", () => {
    const floor = DA_TRANSPORT_LIMITS_V1.minimumRetentionDays;
    expect(windowFor(floor).retentionDays).toBe(floor);
    expect(windowFor(floor + 1).retentionDays).toBe(floor + 1);
    expect(() => windowFor(floor - 1)).toThrowError(
      WatcherCanonicalBlockStoreError,
    );
  });

  it("fails closed on a window that cannot cover maturity plus the proof-time bound", () => {
    const shortDays = Math.floor(
      MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs / RETENTION_MS_PER_DAY_V1,
    );
    expect(() => windowFor(shortDays)).toThrowError(
      WatcherCanonicalBlockStoreError,
    );
    expect(() => windowFor(0)).toThrowError(WatcherCanonicalBlockStoreError);
  });

  it("rejects malformed retention values and manifest shapes", async () => {
    for (const value of [undefined, null, "15", 15.5, -15, Number.NaN]) {
      await expectStoreError(
        () =>
          watcherCanonicalRetentionWindowFromVerifiedManifestV1({
            manifest: manifestWith(value),
            manifestId: FINGERPRINT,
            deploymentMarker: MARKER,
          }),
        "retention_window_insufficient",
      );
    }
    await expectStoreError(
      () =>
        watcherCanonicalRetentionWindowFromVerifiedManifestV1({
          manifest: { da: {} },
          manifestId: FINGERPRINT,
          deploymentMarker: MARKER,
        }),
      "invalid_field",
    );
  });

  it("never accepts a caller-supplied window: the resolver verifies the identity first", async () => {
    await expect(
      Promise.resolve().then(() =>
        resolveWatcherCanonicalRetentionWindowV1({
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
      ),
    ).rejects.toThrowError();
  });

  it("refuses to load a store under a doctored retention window", async () => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    const doctored = {
      ...windowFor(),
      retentionDays: 1,
    } as WatcherCanonicalRetentionWindowV1;
    await expectStoreError(
      async () =>
        loadWatcherCanonicalBlockStoreV1({
          backend,
          deploymentIdentity: identityOf(),
          retentionWindow: doctored,
        }),
      "retention_window_insufficient",
    );
  });

  it("refuses a window whose derived slot arithmetic has been tampered with", async () => {
    const doctored = {
      ...windowFor(),
      retentionSlots: 1,
    } as WatcherCanonicalRetentionWindowV1;
    await expectStoreError(
      () =>
        watcherCanonicalRetainUntilSlotV1({
          window: doctored,
          observedAtSlot: OBSERVED_AT_SLOT,
        }),
      "retention_window_insufficient",
    );
  });
});

describe("W21 canonical block store: prune boundaries", () => {
  const persisted = async (): Promise<MemoryAtomicBackend> => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    return backend;
  };

  const pruneAt = async (
    backend: MemoryAtomicBackend,
    atSlot: number,
    stillChallengeableInputIds: readonly string[] = [],
  ) =>
    pruneWatcherCanonicalBlockStoreV1({
      backend,
      deploymentIdentity: identityOf(),
      atSlot,
      stillChallengeableInputIds,
      retentionWindow: windowFor(),
    });

  it("retains one slot before the deadline, at the deadline, and prunes one slot after", async () => {
    const retainUntilSlot = payloadRecord.metadata.retainUntilSlot;

    const early = await pruneAt(await persisted(), retainUntilSlot - 1);
    expect(early.committed).toBe(false);
    expect(early.prunedInputIds).toEqual([]);
    expect(early.decisions[0]!.reasonCode).toBe("retention_not_expired");

    const exact = await pruneAt(await persisted(), retainUntilSlot);
    expect(exact.committed).toBe(false);
    expect(exact.prunedInputIds).toEqual([]);
    expect(exact.decisions[0]!.reasonCode).toBe("retention_not_expired");

    const backend = await persisted();
    const late = await pruneAt(backend, retainUntilSlot + 1);
    expect(late.committed).toBe(true);
    expect(late.prunedInputIds).toEqual([payloadRecord.input.inputId]);
    expect(late.decisions[0]!.reasonCode).toBe("expired_and_not_challengeable");
    const loaded = await loadWatcherCanonicalBlockStoreV1({
      backend,
      deploymentIdentity: identityOf(),
    });
    expect(loaded!.snapshot.records).toEqual([]);
    expect(loaded!.snapshot.revision).toBe("2");
  });

  it("refuses to prune a still-challengeable record even after its deadline", async () => {
    const backend = await persisted();
    const result = await pruneAt(
      backend,
      payloadRecord.metadata.retainUntilSlot + 10_000,
      [payloadRecord.input.inputId],
    );
    expect(result.committed).toBe(false);
    expect(result.prunedInputIds).toEqual([]);
    expect(result.decisions[0]!.reasonCode).toBe("still_challengeable");
    expect(backend.writes).toBe(1);
    const loaded = await loadWatcherCanonicalBlockStoreV1({
      backend,
      deploymentIdentity: identityOf(),
    });
    expect(loaded!.snapshot.records).toHaveLength(1);
  });

  it("raises deadline_at_risk before expiry and stays quiet outside the headroom", async () => {
    const window = windowFor();
    const retainUntilSlot = payloadRecord.metadata.retainUntilSlot;

    const quiet = await pruneAt(
      await persisted(),
      retainUntilSlot - window.alertHeadroomSlots - 1,
    );
    expect(quiet.alerts).toEqual([]);

    const alerting = await pruneAt(
      await persisted(),
      retainUntilSlot - window.alertHeadroomSlots,
    );
    expect(alerting.alerts).toHaveLength(1);
    expect(alerting.alerts[0]!.alertCode).toBe("deadline_at_risk");
    expect(alerting.alerts[0]!.decision).toBe("retained");
    expect(alerting.alerts[0]!.remainingSlots).toBe(window.alertHeadroomSlots);
  });

  it("reports an unknown inputId instead of silently succeeding", async () => {
    const result = await pruneWatcherCanonicalBlockStoreV1({
      backend: await persisted(),
      deploymentIdentity: identityOf(),
      atSlot: payloadRecord.metadata.retainUntilSlot + 1,
      stillChallengeableInputIds: [],
      inputIds: [repeatHex(0xee, 32)],
      retentionWindow: windowFor(),
    });
    expect(result.committed).toBe(false);
    expect(result.decisions).toHaveLength(1);
    expect(result.decisions[0]!.reasonCode).toBe("unknown_input_id");
  });
});

// ---------------------------------------------------------------------------
// 4. Mutation and integrity
// ---------------------------------------------------------------------------

describe("W21 canonical block store: mutation rejection", () => {
  const storedBytes = async (): Promise<MemoryAtomicBackend> => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    return backend;
  };

  it("rejects a proof_input record whose stored bytes were flipped underneath the digest", async () => {
    const backend = new MemoryAtomicBackend();
    const bundle = makeWatcherCanonicalProofBundleRecordV1({
      proofBundle: await clientFor(envelope).fetchProofBundleByHeader({
        headerHash: HEADER_HASH,
      }),
      context: contextOf(),
    });
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: bundle,
    });
    const text = new TextDecoder().decode(backend.bytes!);
    const hex = bundle.input.payload.cborHex;
    const flipped = `${hex.slice(0, hex.length - 2)}ff`;
    expect(flipped).not.toBe(hex);
    backend.bytes = new TextEncoder().encode(text.replace(hex, flipped));

    await expectStoreError(
      async () =>
        loadWatcherCanonicalBlockStoreV1({
          backend,
          deploymentIdentity: identityOf(),
        }),
      "integrity_mismatch",
    );
  });

  it("rejects a snapshot with one flipped stored byte", async () => {
    const backend = await storedBytes();
    const text = new TextDecoder().decode(backend.bytes!);
    const hex = payloadRecord.input.payload.cborHex;
    const flipped = `${hex.slice(0, hex.length - 1)}${hex.endsWith("0") ? "1" : "0"}`;
    backend.bytes = new TextEncoder().encode(text.replace(hex, flipped));

    await expectStoreError(
      async () =>
        loadWatcherCanonicalBlockStoreV1({
          backend,
          deploymentIdentity: identityOf(),
        }),
      "integrity_mismatch",
    );
  });

  it("rejects a lie about payload.sha256", async () => {
    const forged = cloned(payloadRecord);
    forged.input.payload.sha256 = repeatHex(0xaa, 32);
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(forged),
      "integrity_mismatch",
    );
  });

  it("rejects a lie about envelopeSha256 while innerSha256 stays correct", async () => {
    const forged = cloned(payloadRecord);
    forged.metadata.envelopeSha256 = repeatHex(0xbb, 32);
    expect(forged.metadata.innerSha256).toBe(sha256Hex(innerCbor));
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(forged),
      "integrity_mismatch",
    );
  });

  it("rejects a lie about innerSha256 while envelopeSha256 stays correct", async () => {
    const backend = new MemoryAtomicBackend();
    const forged = cloned(payloadRecord);
    forged.metadata.innerSha256 = repeatHex(0xcc, 32);
    expect(forged.metadata.envelopeSha256).toBe(sha256Hex(envelope));
    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identityOf(),
          record: forged as unknown as WatcherCanonicalBlockRecordV1,
        }),
      "integrity_mismatch",
    );
    expect(backend.writes).toBe(0);
  });

  it("rejects an inputId that does not address the stored bytes", async () => {
    const forged = cloned(payloadRecord);
    forged.input.inputId = repeatHex(0xdd, 32);
    forged.metadata.inputId = repeatHex(0xdd, 32);
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(forged),
      "integrity_mismatch",
    );
  });

  it("rejects a byteLength that disagrees with the stored bytes", async () => {
    const forged = cloned(payloadRecord);
    forged.metadata.byteLength = envelope.length + 1;
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(forged),
      "integrity_mismatch",
    );
  });
});

// ---------------------------------------------------------------------------
// 5. Malformed inputs
// ---------------------------------------------------------------------------

describe("W21 canonical block store: malformed inputs", () => {
  it("rejects non-hex, odd-length, and zero-length cborHex", async () => {
    for (const cborHex of ["zz", "abc", ""]) {
      const forged = cloned(payloadRecord);
      forged.input.payload.cborHex = cborHex;
      await expectStoreError(
        () => parseWatcherCanonicalBlockRecordV1(forged),
        "invalid_field",
      );
    }
  });

  it("rejects a payload above the DA transport payload ceiling", async () => {
    const oversize = Buffer.alloc(DA_TRANSPORT_LIMITS_V1.maxPayloadBytes + 1);
    const forged = cloned(payloadRecord);
    forged.input.payload.cborHex = oversize.toString("hex");
    forged.input.payload.sha256 = sha256Hex(oversize);
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(forged),
      "invalid_field",
    );
  });

  it("rejects an unknown kind and an unknown contentKind", async () => {
    const badKind = cloned(payloadRecord);
    badKind.input.kind = "surprise";
    badKind.metadata.kind = "surprise";
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(badKind),
      "invalid_field",
    );

    const badContentKind = cloned(payloadRecord);
    badContentKind.metadata.contentKind = "surprise";
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(badContentKind),
      "invalid_field",
    );
  });

  it("rejects a contentKind that contradicts the reserved record kind", async () => {
    const forged = cloned(payloadRecord);
    forged.metadata.contentKind = "proof_bundle";
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(forged),
      "invalid_field",
    );
  });

  it("rejects missing and extra metadata keys", async () => {
    const missing = cloned(payloadRecord);
    delete missing.metadata.headerHash;
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(missing),
      "missing_field",
    );

    const extra = cloned(payloadRecord);
    extra.metadata.surprise = 1;
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(extra),
      "unknown_field",
    );
  });

  it("rejects a non-canonical, a truncated, and a non-UTF8 snapshot encoding", async () => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    const canonical = new TextDecoder().decode(backend.bytes!);

    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshotV1(
          new TextEncoder().encode(` ${canonical}`),
        ),
      "noncanonical_encoding",
    );
    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshotV1(
          backend.bytes!.slice(0, backend.bytes!.length - 5),
        ),
      "invalid_encoding",
    );
    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshotV1(
          Uint8Array.from([0xff, 0xfe, 0xfd]),
        ),
      "invalid_encoding",
    );
  });

  it("rejects a snapshot with the wrong schema version and an unsorted record set", async () => {
    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshotV1(
          new TextEncoder().encode(
            watcherCanonicalJsonV1({
              schemaVersion: "midgard-watcher-canonical-block-store-v0",
              revision: "0",
              deploymentMarker: { ...MARKER },
              records: [],
            }),
          ),
        ),
      "unsupported_schema",
    );

    const other = cloned(payloadRecord);
    const bytes = Buffer.alloc(8, 0x01);
    other.input.payload.cborHex = bytes.toString("hex");
    other.input.payload.sha256 = sha256Hex(bytes);
    other.input.inputId = sha256Hex(bytes);
    other.input.kind = "proof_input";
    other.metadata.inputId = sha256Hex(bytes);
    other.metadata.kind = "proof_input";
    other.metadata.contentKind = "proof_bundle";
    other.metadata.envelopeSha256 = sha256Hex(bytes);
    other.metadata.innerSha256 = null;
    other.metadata.byteLength = bytes.length;
    const pair = [cloned(payloadRecord), other].sort((left, right) =>
      left.input.inputId < right.input.inputId ? 1 : -1,
    );
    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshotV1(
          new TextEncoder().encode(
            watcherCanonicalJsonV1({
              schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_V1_SCHEMA_VERSION,
              revision: "2",
              deploymentMarker: { ...MARKER },
              records: pair,
            }),
          ),
        ),
      "unsorted_records",
    );
  });

  it("round-trips a snapshot through its canonical encoding", async () => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    const snapshot = decodeWatcherCanonicalBlockStoreSnapshotV1(backend.bytes!);
    expect(encodeWatcherCanonicalBlockStoreSnapshotV1(snapshot)).toEqual(
      backend.bytes,
    );
    expect(watcherDurableStoreBytesSha256(backend.bytes!)).toHaveLength(64);
  });
});

// ---------------------------------------------------------------------------
// 6. Fail-closed behaviour
// ---------------------------------------------------------------------------

describe("W21 canonical block store: fail-closed", () => {
  it("refuses operator-private provenance before anything is persisted", async () => {
    const backend = new MemoryAtomicBackend();
    const forged = cloned(payloadRecord);
    forged.metadata.provenance.trustClass = "operator_private_file";
    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identityOf(),
          record: forged as unknown as WatcherCanonicalBlockRecordV1,
        }),
      "provenance_not_public_da",
    );
    expect(backend.writes).toBe(0);
    expect(backend.bytes).toBeNull();
  });

  it("refuses an admitted trust class that is not public or permissionless DA", async () => {
    for (const trustClass of [
      "authenticated_cardano_l1",
      "signed_deployment_identity",
      "deterministic_local_computation",
    ]) {
      const backend = new MemoryAtomicBackend();
      const forged = cloned(payloadRecord);
      forged.metadata.provenance.trustClass = trustClass;
      await expectStoreError(
        async () =>
          persistWatcherCanonicalPublicBytesV1({
            backend,
            deploymentIdentity: identityOf(),
            record: forged as unknown as WatcherCanonicalBlockRecordV1,
          }),
        "provenance_not_public_da",
      );
      expect(backend.writes).toBe(0);
    }
  });

  it("refuses diagnostic-grade provenance", async () => {
    const forged = cloned(payloadRecord);
    forged.metadata.provenance.grade = "diagnostic";
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecordV1(forged),
      "invalid_field",
    );
  });

  it("refuses a record whose deployment marker is not this deployment", async () => {
    const backend = new MemoryAtomicBackend();
    const error = await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identityOf(OTHER_MANIFEST_ID),
          record: payloadRecord,
        }),
      "deployment_marker_mismatch",
    );
    // Refused up front against the caller's record, not later against a
    // snapshot the store would otherwise have had to construct first.
    expect(error.path).toBe("$.record.metadata.deploymentMarker");
    expect(backend.reads).toBe(0);
    expect(backend.writes).toBe(0);
  });

  it("refuses to load a snapshot written under another deployment", async () => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    await expectStoreError(
      async () =>
        loadWatcherCanonicalBlockStoreV1({
          backend,
          deploymentIdentity: identityOf(OTHER_MANIFEST_ID),
        }),
      "deployment_marker_mismatch",
    );
  });

  it("surfaces a backend read fault as persistence_failure, never as success", async () => {
    const backend = new MemoryAtomicBackend();
    backend.failRead = true;
    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identityOf(),
          record: payloadRecord,
        }),
      "persistence_failure",
    );
    await expectStoreError(
      async () =>
        loadWatcherCanonicalBlockStoreV1({
          backend,
          deploymentIdentity: identityOf(),
        }),
      "persistence_failure",
    );
    expect(backend.writes).toBe(0);
  });

  it("surfaces a backend commit fault as persistence_failure", async () => {
    const backend = new MemoryAtomicBackend();
    backend.failBeforeCommit = true;
    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identityOf(),
          record: payloadRecord,
        }),
      "persistence_failure",
    );
    expect(backend.writes).toBe(0);
    expect(backend.bytes).toBeNull();
  });

  it("gives up deterministically when compare-and-swap never wins", async () => {
    const backend = new MemoryAtomicBackend();
    backend.alwaysConflict = true;
    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identityOf(),
          record: payloadRecord,
        }),
      "cas_contention",
    );
    expect(backend.writes).toBe(0);
    expect(backend.bytes).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// 7. Restart safety
// ---------------------------------------------------------------------------

describe("W21 canonical block store: restart safety", () => {
  it("recovers the bytes after a crash between commit and caller verification", async () => {
    const backend = new MemoryAtomicBackend();
    backend.failAfterCommit = true;
    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identityOf(),
          record: payloadRecord,
        }),
      "persistence_failure",
    );

    // The process is gone; a fresh reader sees the committed snapshot.
    const restarted = new MemoryAtomicBackend(backend.bytes);
    const loaded = await loadWatcherCanonicalBlockStoreV1({
      backend: restarted,
      deploymentIdentity: identityOf(),
      retentionWindow: windowFor(),
    });
    expect(loaded!.snapshot.records).toHaveLength(1);
    expect(
      Buffer.from(loaded!.snapshot.records[0]!.input.payload.cborHex, "hex"),
    ).toEqual(envelope);

    // Re-driving the same persist is the idempotent no-op, not a duplicate.
    const replay = await persistWatcherCanonicalPublicBytesV1({
      backend: restarted,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    expect(replay.alreadyPresent).toBe(true);
    expect(restarted.writes).toBe(0);
  });

  it("leaves nothing observable when a crash precedes the commit", async () => {
    const backend = new MemoryAtomicBackend();
    backend.failBeforeCommit = true;
    await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytesV1({
          backend,
          deploymentIdentity: identityOf(),
          record: payloadRecord,
        }),
      "persistence_failure",
    );
    expect(
      await loadWatcherCanonicalBlockStoreV1({
        backend,
        deploymentIdentity: identityOf(),
      }),
    ).toBeNull();
  });

  it("retries a lost compare-and-swap race without a partial write", async () => {
    const backend = new MemoryAtomicBackend();
    backend.conflictOnce = true;
    const result = await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    expect(result.committed).toBe(true);
    expect(backend.writes).toBe(1);
    const loaded = await loadWatcherCanonicalBlockStoreV1({
      backend,
      deploymentIdentity: identityOf(),
    });
    expect(loaded!.snapshot.records).toHaveLength(1);
    expect(loaded!.snapshot.revision).toBe("1");
    expect(loaded!.snapshotSha256).toBe(result.snapshotSha256);
  });

  it("keeps a concurrent writer's record when this writer replays onto a newer snapshot", async () => {
    const backend = new MemoryAtomicBackend();
    const identity = identityOf();
    const otherBytes = Buffer.alloc(8, 0x2f);
    const other = parseWatcherCanonicalBlockRecordV1({
      input: {
        inputId: sha256Hex(otherBytes),
        kind: "proof_input",
        payload: {
          cborHex: otherBytes.toString("hex"),
          sha256: sha256Hex(otherBytes),
        },
      },
      metadata: {
        inputId: sha256Hex(otherBytes),
        kind: "proof_input",
        contentKind: "proof_bundle",
        headerHash: HEADER_HASH,
        envelopeSha256: sha256Hex(otherBytes),
        innerSha256: null,
        byteLength: otherBytes.length,
        sourcePeerIdentity: PEER,
        sourcePeerId: payloadRecord.metadata.sourcePeerId,
        provenance: { ...payloadRecord.metadata.provenance },
        deploymentMarker: { ...MARKER },
        observedAtSlot: OBSERVED_AT_SLOT,
        retainUntilSlot: payloadRecord.metadata.retainUntilSlot,
      },
    });
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identity,
      record: other,
    });
    backend.conflictOnce = true;
    await persistWatcherCanonicalPublicBytesV1({
      backend,
      deploymentIdentity: identity,
      record: payloadRecord,
    });

    const loaded = await loadWatcherCanonicalBlockStoreV1({
      backend,
      deploymentIdentity: identity,
    });
    expect(loaded!.snapshot.records).toHaveLength(2);
    expect(loaded!.snapshot.revision).toBe("2");
  });
});
