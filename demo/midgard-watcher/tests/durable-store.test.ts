import { describe, expect, it } from "vitest";

import { makeDeploymentMarkerV1 } from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import {
  decodeWatcherDurableStoreV1,
  encodeWatcherDurableStoreV1,
  journalWatcherProtocolUtxoTransitionV1,
  makeEmptyWatcherDurableStoreV1,
  makeWatcherDurablePayloadV1,
  makeWatcherDurableStoreV1,
  migrateWatcherDurableStoreV1,
  parseWatcherDurableStoreV1,
  rebuildWatcherDurableCachesV1,
  WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256,
  WATCHER_DURABLE_STORE_V1_SCHEMA_VERSION,
  type WatcherDurableAtomicBackend,
  type WatcherDurableRecordsV1,
  watcherDurableStoreBytesSha256,
  WatcherDurableStoreError,
  type WatcherDurableStoreErrorCode,
  type WatcherDurableStoreV1,
} from "../src/durable-store.js";

const hex32 = (byte: string): string => byte.repeat(32);
const marker = makeDeploymentMarkerV1(hex32("aa"));
const payload = (cborHex = "80") => makeWatcherDurablePayloadV1(cborHex);

const recordsFixture = (): WatcherDurableRecordsV1 => ({
  l1Observations: [
    {
      observationId: hex32("02"),
      providerId: "provider-a",
      chainPointId: hex32("01"),
      payload: payload("820001"),
    },
  ],
  chainPoints: [
    {
      chainPointId: hex32("01"),
      providerId: "provider-a",
      blockHash: hex32("10"),
      slot: "100",
      blockNo: "50",
      depth: "8",
    },
  ],
  protocolUtxos: [
    {
      outRef: `${hex32("11")}#0`,
      role: "state_queue",
      chainPointId: hex32("01"),
      output: payload("d87980"),
    },
  ],
  spentProtocolUtxos: [
    {
      outRef: `${hex32("12")}#0`,
      role: "deposit",
      chainPointId: hex32("01"),
      output: payload("d87981"),
      spentAtChainPointId: hex32("01"),
    },
  ],
  daProofInputs: [
    {
      inputId: hex32("03"),
      kind: "da_payload",
      payload: payload("4401020304"),
    },
  ],
  reconstructedStates: [
    {
      blockHash: hex32("10"),
      priorStateRoot: hex32("12"),
      postStateRoot: hex32("13"),
      inputIds: [hex32("03")],
      state: payload("82190100190101"),
    },
  ],
  decisions: [
    {
      blockHash: hex32("10"),
      decision: "fault_detected",
      reconstructionDigest: hex32("14"),
      evidenceDigest: hex32("15"),
    },
  ],
  faults: [
    {
      faultId: hex32("04"),
      blockHash: hex32("10"),
      familyId: "transition-trace",
      evidence: payload("a10001"),
    },
  ],
  submissions: [
    {
      submissionId: hex32("05"),
      faultId: hex32("04"),
      txBodyHash: hex32("16"),
      status: "submitted",
    },
  ],
  confirmations: [
    {
      confirmationId: hex32("06"),
      submissionId: hex32("05"),
      txHash: hex32("17"),
      chainPointId: hex32("01"),
      depth: "8",
      status: "confirmed",
    },
  ],
  retries: [
    {
      retryId: hex32("07"),
      submissionId: hex32("05"),
      attempt: "1",
      nextEligibleSlot: "101",
      reason: "submission_ambiguous",
    },
  ],
  deadlines: [
    {
      deadlineId: hex32("08"),
      subjectKind: "submission",
      subjectId: hex32("05"),
      kind: "confirmation",
      expiresAtSlot: "120",
    },
  ],
  correctionResults: [
    {
      correctionId: hex32("09"),
      faultId: hex32("04"),
      confirmationId: hex32("06"),
      outcome: "removed_slashed_and_rewarded",
      finalStateRoot: hex32("18"),
      slashLovelace: "5000000",
      rewardLovelace: "1000000",
    },
  ],
});

const populatedStore = (): WatcherDurableStoreV1 =>
  makeWatcherDurableStoreV1({
    deploymentMarker: marker,
    revision: "1",
    records: recordsFixture(),
  });

type MutableRecord = Record<string, any>;

const mutateStore = (
  store: WatcherDurableStoreV1,
  mutation: (mutable: MutableRecord) => void,
): MutableRecord => {
  const mutable = JSON.parse(JSON.stringify(store)) as MutableRecord;
  mutation(mutable);
  return mutable;
};

const expectStoreError = (
  operation: () => unknown,
  code: WatcherDurableStoreErrorCode,
): void => {
  try {
    operation();
    throw new Error("Expected watcher durable store rejection");
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherDurableStoreError);
    expect((error as WatcherDurableStoreError).code).toBe(code);
  }
};

class MemoryAtomicBackend implements WatcherDurableAtomicBackend {
  bytes: Uint8Array | null;
  writes = 0;
  failBeforeCommit = false;
  failAfterCommit = false;
  alwaysConflict = false;

  constructor(bytes: Uint8Array | null = null) {
    this.bytes = bytes;
  }

  async read(): Promise<Uint8Array | null> {
    return this.bytes === null ? null : Uint8Array.from(this.bytes);
  }

  async compareAndSwap(
    expectedSha256: string | null,
    next: Uint8Array,
  ): Promise<boolean> {
    if (this.alwaysConflict) {
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

describe("watcher durable store V1", () => {
  it("round-trips every W03 durable state class with exact content integrity", () => {
    const store = populatedStore();
    const encoded = encodeWatcherDurableStoreV1(store);
    const decoded = decodeWatcherDurableStoreV1(encoded);

    expect(decoded).toEqual(store);
    expect(decoded.deploymentMarker).toEqual(marker);
    expect(decoded.migrationManifestSha256).toBe(
      WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256,
    );
    expect(
      [
        decoded.l1Observations,
        decoded.chainPoints,
        decoded.protocolUtxos,
        decoded.spentProtocolUtxos,
        decoded.daProofInputs,
        decoded.reconstructedStates,
        decoded.decisions,
        decoded.faults,
        decoded.submissions,
        decoded.confirmations,
        decoded.retries,
        decoded.deadlines,
        decoded.correctionResults,
      ].every((records) => records.length === 1),
    ).toBe(true);
    expect(decoded.caches.entries).toHaveLength(13);
  });

  it("reproduces byte-identical caches and persisted bytes from reordered inputs", () => {
    const records = recordsFixture();
    const reversed: WatcherDurableRecordsV1 = {
      l1Observations: [...records.l1Observations].reverse(),
      chainPoints: [...records.chainPoints].reverse(),
      protocolUtxos: [...records.protocolUtxos].reverse(),
      spentProtocolUtxos: [...records.spentProtocolUtxos].reverse(),
      daProofInputs: [...records.daProofInputs].reverse(),
      reconstructedStates: [...records.reconstructedStates].reverse(),
      decisions: [...records.decisions].reverse(),
      faults: [...records.faults].reverse(),
      submissions: [...records.submissions].reverse(),
      confirmations: [...records.confirmations].reverse(),
      retries: [...records.retries].reverse(),
      deadlines: [...records.deadlines].reverse(),
      correctionResults: [...records.correctionResults].reverse(),
    };
    const first = populatedStore();
    const second = makeWatcherDurableStoreV1({
      deploymentMarker: marker,
      revision: "1",
      records: reversed,
    });

    expect(second.caches).toEqual(first.caches);
    expect(encodeWatcherDurableStoreV1(second)).toEqual(
      encodeWatcherDurableStoreV1(first),
    );
    expect(
      rebuildWatcherDurableCachesV1({
        deploymentMarker: first.deploymentMarker,
        l1Observations: first.l1Observations,
        chainPoints: first.chainPoints,
        protocolUtxos: first.protocolUtxos,
        spentProtocolUtxos: first.spentProtocolUtxos,
        daProofInputs: first.daProofInputs,
        reconstructedStates: first.reconstructedStates,
        decisions: first.decisions,
        faults: first.faults,
        submissions: first.submissions,
        confirmations: first.confirmations,
        retries: first.retries,
        deadlines: first.deadlines,
        correctionResults: first.correctionResults,
      }),
    ).toEqual(first.caches);
  });

  it("rejects unknown schemas, unknown fields, and noncanonical persisted bytes", () => {
    expectStoreError(
      () =>
        parseWatcherDurableStoreV1(
          mutateStore(populatedStore(), (value) => {
            value.schemaVersion = "midgard-watcher-durable-store-v0";
          }),
        ),
      "unsupported_schema",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStoreV1(
          mutateStore(populatedStore(), (value) => {
            value.legacyRecords = [];
          }),
        ),
      "unknown_field",
    );
    expectStoreError(
      () =>
        decodeWatcherDurableStoreV1(
          new TextEncoder().encode(JSON.stringify(populatedStore(), null, 2)),
        ),
      "noncanonical_encoding",
    );
  });

  it("rejects payload tampering, cache tampering, and broken references", () => {
    expectStoreError(
      () =>
        parseWatcherDurableStoreV1(
          mutateStore(populatedStore(), (value) => {
            value.daProofInputs[0].payload.cborHex = "81";
          }),
        ),
      "integrity_mismatch",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStoreV1(
          mutateStore(populatedStore(), (value) => {
            value.caches.sourceSha256 = hex32("ff");
          }),
        ),
      "cache_mismatch",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStoreV1(
          mutateStore(populatedStore(), (value) => {
            value.confirmations[0].submissionId = hex32("fe");
          }),
        ),
      "broken_reference",
    );
  });

  it("rejects duplicate keys and unsafe correction confirmation topology", () => {
    expectStoreError(
      () =>
        makeWatcherDurableStoreV1({
          deploymentMarker: marker,
          revision: "1",
          records: {
            ...recordsFixture(),
            faults: [recordsFixture().faults[0]!, recordsFixture().faults[0]!],
          },
        }),
      "duplicate_key",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStoreV1(
          mutateStore(populatedStore(), (value) => {
            value.confirmations[0].status = "rolled_back";
          }),
        ),
      "broken_reference",
    );
    expectStoreError(
      () =>
        makeWatcherDurableStoreV1({
          deploymentMarker: marker,
          revision: "1",
          records: {
            ...recordsFixture(),
            correctionResults: [
              recordsFixture().correctionResults[0]!,
              {
                ...recordsFixture().correctionResults[0]!,
                correctionId: hex32("19"),
              },
            ],
          },
        }),
      "duplicate_key",
    );
  });

  it("journals consumed protocol UTxOs exactly and rejects mutation or resurrection", () => {
    const source = populatedStore();
    const spentAtChainPointId = source.chainPoints[0]!.chainPointId;
    const journal = journalWatcherProtocolUtxoTransitionV1({
      sourceStore: source,
      nextChainPoints: source.chainPoints,
      nextProtocolUtxos: [],
      spentAtChainPointId,
    });
    expect(journal.protocolUtxos).toEqual([]);
    expect(journal.spentProtocolUtxos).toEqual([
      {
        ...source.protocolUtxos[0],
        spentAtChainPointId,
      },
      source.spentProtocolUtxos[0],
    ]);

    expectStoreError(
      () =>
        journalWatcherProtocolUtxoTransitionV1({
          sourceStore: source,
          nextChainPoints: source.chainPoints,
          nextProtocolUtxos: [
            {
              ...source.protocolUtxos[0]!,
              role: "reserve",
            },
          ],
          spentAtChainPointId,
        }),
      "integrity_mismatch",
    );
    expectStoreError(
      () =>
        journalWatcherProtocolUtxoTransitionV1({
          sourceStore: source,
          nextChainPoints: source.chainPoints,
          nextProtocolUtxos: [
            {
              outRef: source.spentProtocolUtxos[0]!.outRef,
              role: source.spentProtocolUtxos[0]!.role,
              chainPointId: source.spentProtocolUtxos[0]!.chainPointId,
              output: source.spentProtocolUtxos[0]!.output,
            },
          ],
          spentAtChainPointId,
        }),
      "duplicate_key",
    );
  });

  it("initializes once and makes repeat migration byte-idempotent", async () => {
    const backend = new MemoryAtomicBackend();
    const first = await migrateWatcherDurableStoreV1({
      backend,
      deploymentMarker: marker,
    });
    const second = await migrateWatcherDurableStoreV1({
      backend,
      deploymentMarker: marker,
    });

    expect(first.initialized).toBe(true);
    expect(second.initialized).toBe(false);
    expect(second.encodedSha256).toBe(first.encodedSha256);
    expect(second.snapshot).toEqual(first.snapshot);
    expect(backend.writes).toBe(1);
    expect(second.snapshot).toEqual(makeEmptyWatcherDurableStoreV1(marker));
  });

  it("recovers idempotently from a crash before atomic migration commit", async () => {
    const backend = new MemoryAtomicBackend();
    backend.failBeforeCommit = true;

    await expect(
      migrateWatcherDurableStoreV1({ backend, deploymentMarker: marker }),
    ).rejects.toMatchObject({ code: "persistence_failure" });
    expect(backend.bytes).toBeNull();

    const recovered = await migrateWatcherDurableStoreV1({
      backend,
      deploymentMarker: marker,
    });
    expect(recovered.initialized).toBe(true);
    expect(backend.writes).toBe(1);
  });

  it("reconciles an ambiguous crash after atomic migration commit without rewriting", async () => {
    const backend = new MemoryAtomicBackend();
    backend.failAfterCommit = true;

    await expect(
      migrateWatcherDurableStoreV1({ backend, deploymentMarker: marker }),
    ).rejects.toMatchObject({ code: "persistence_failure" });
    expect(backend.bytes).not.toBeNull();

    const recovered = await migrateWatcherDurableStoreV1({
      backend,
      deploymentMarker: marker,
    });
    expect(recovered.initialized).toBe(false);
    expect(backend.writes).toBe(1);
    expect(recovered.snapshot.schemaVersion).toBe(
      WATCHER_DURABLE_STORE_V1_SCHEMA_VERSION,
    );
  });

  it("converges concurrent migrations through atomic compare-and-swap", async () => {
    const backend = new MemoryAtomicBackend();
    const results = await Promise.all([
      migrateWatcherDurableStoreV1({ backend, deploymentMarker: marker }),
      migrateWatcherDurableStoreV1({ backend, deploymentMarker: marker }),
    ]);

    expect(results.filter((result) => result.initialized)).toHaveLength(1);
    expect(new Set(results.map((result) => result.encodedSha256)).size).toBe(1);
    expect(backend.writes).toBe(1);
  });

  it("fails closed on marker drift, partial bytes, and exhausted migration conflicts", async () => {
    const initialized = new MemoryAtomicBackend(
      encodeWatcherDurableStoreV1(makeEmptyWatcherDurableStoreV1(marker)),
    );
    await expect(
      migrateWatcherDurableStoreV1({
        backend: initialized,
        deploymentMarker: makeDeploymentMarkerV1(hex32("bb")),
      }),
    ).rejects.toMatchObject({ code: "deployment_marker_mismatch" });

    const partial = new MemoryAtomicBackend(
      new TextEncoder().encode('{"schemaVersion":'),
    );
    await expect(
      migrateWatcherDurableStoreV1({
        backend: partial,
        deploymentMarker: marker,
      }),
    ).rejects.toMatchObject({ code: "invalid_encoding" });

    const contended = new MemoryAtomicBackend();
    contended.alwaysConflict = true;
    await expect(
      migrateWatcherDurableStoreV1({
        backend: contended,
        deploymentMarker: marker,
        maxConflicts: 2,
      }),
    ).rejects.toMatchObject({ code: "migration_conflict" });
  });
});
