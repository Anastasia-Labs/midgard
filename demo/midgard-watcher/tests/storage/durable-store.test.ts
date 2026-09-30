import "@al-ft/midgard-core/deployment-manifest-identity";
import "vitest";
import "../../src/storage/durable-store.js";
import "./durable-store.records-fixture.js";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  compareAndSwapWatcherDurableAtomicSnapshot,
  decodeWatcherDurableStore,
  encodeWatcherDurableStore,
  journalWatcherProtocolUtxoTransition,
  makeEmptyWatcherDurableStore,
  makeWatcherDurableStore,
  migrateWatcherDurableStore,
  parseWatcherDurableStore,
  readWatcherDurableAtomicSnapshot,
  rebuildWatcherDurableCaches,
  WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256,
  WATCHER_DURABLE_STORE_SCHEMA_VERSION,
  type WatcherDurableRecords,
} from "../../src/storage/durable-store.js";
import {
  expectStoreError,
  hex32,
  marker,
  MemoryAtomicBackend,
  mutateStore,
  populatedStore,
  recordsFixture,
} from "./durable-store.records-fixture.js";

describe("watcher durable store V1", () => {
  it("round-trips every W03 durable state class with exact content integrity", () => {
    const store = populatedStore();
    const encoded = encodeWatcherDurableStore(store);
    const decoded = decodeWatcherDurableStore(encoded);

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
    const reversed: WatcherDurableRecords = {
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
    const second = makeWatcherDurableStore({
      deploymentMarker: marker,
      revision: "1",
      records: reversed,
    });

    expect(second.caches).toEqual(first.caches);
    expect(encodeWatcherDurableStore(second)).toEqual(
      encodeWatcherDurableStore(first),
    );
    expect(
      rebuildWatcherDurableCaches({
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
        parseWatcherDurableStore(
          mutateStore(populatedStore(), (value) => {
            value.schemaVersion = "midgard-watcher-durable-store-v0";
          }),
        ),
      "unsupported_schema",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStore(
          mutateStore(populatedStore(), (value) => {
            value.legacyRecords = [];
          }),
        ),
      "unknown_field",
    );
    expectStoreError(
      () =>
        decodeWatcherDurableStore(
          new TextEncoder().encode(JSON.stringify(populatedStore(), null, 2)),
        ),
      "noncanonical_encoding",
    );
  });

  it("rejects payload tampering, cache tampering, and broken references", () => {
    expectStoreError(
      () =>
        parseWatcherDurableStore(
          mutateStore(populatedStore(), (value) => {
            value.daProofInputs[0].payload.cborHex = "81";
          }),
        ),
      "integrity_mismatch",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStore(
          mutateStore(populatedStore(), (value) => {
            value.caches.sourceSha256 = hex32("ff");
          }),
        ),
      "cache_mismatch",
    );
    expectStoreError(
      () =>
        parseWatcherDurableStore(
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
        makeWatcherDurableStore({
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
        parseWatcherDurableStore(
          mutateStore(populatedStore(), (value) => {
            value.confirmations[0].status = "rolled_back";
          }),
        ),
      "broken_reference",
    );
    expectStoreError(
      () =>
        makeWatcherDurableStore({
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
    const journal = journalWatcherProtocolUtxoTransition({
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
        journalWatcherProtocolUtxoTransition({
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
        journalWatcherProtocolUtxoTransition({
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

    const laterPoint = {
      chainPointId: hex32("02"),
      providerId: "provider-a",
      blockHash: hex32("20"),
      slot: "101",
      blockNo: "51",
      depth: "8",
    };
    const forwardSource = makeWatcherDurableStore({
      deploymentMarker: source.deploymentMarker,
      revision: source.revision,
      records: {
        ...recordsFixture(),
        chainPoints: [...source.chainPoints, laterPoint],
        spentProtocolUtxos: [
          {
            ...source.spentProtocolUtxos[0]!,
            spentAtChainPointId: laterPoint.chainPointId,
          },
        ],
      },
    });
    const forwardJournal = journalWatcherProtocolUtxoTransition({
      sourceStore: forwardSource,
      nextChainPoints: forwardSource.chainPoints,
      nextProtocolUtxos: [],
      spentAtChainPointId: laterPoint.chainPointId,
    });
    expect(forwardJournal.spentProtocolUtxos).toContainEqual({
      ...forwardSource.protocolUtxos[0],
      spentAtChainPointId: laterPoint.chainPointId,
    });

    expectStoreError(
      () =>
        parseWatcherDurableStore(
          mutateStore(forwardSource, (value) => {
            value.spentProtocolUtxos[0].chainPointId = laterPoint.chainPointId;
            value.spentProtocolUtxos[0].spentAtChainPointId =
              spentAtChainPointId;
          }),
        ),
      "broken_reference",
    );

    const backwardJournalSource = makeWatcherDurableStore({
      deploymentMarker: source.deploymentMarker,
      revision: source.revision,
      records: {
        ...recordsFixture(),
        chainPoints: [...source.chainPoints, laterPoint],
        protocolUtxos: [
          {
            ...source.protocolUtxos[0]!,
            chainPointId: laterPoint.chainPointId,
          },
        ],
      },
    });
    expectStoreError(
      () =>
        journalWatcherProtocolUtxoTransition({
          sourceStore: backwardJournalSource,
          nextChainPoints: backwardJournalSource.chainPoints,
          nextProtocolUtxos: [],
          spentAtChainPointId,
        }),
      "broken_reference",
    );
  });

  it("initializes once and makes repeat migration byte-idempotent", async () => {
    const backend = new MemoryAtomicBackend();
    const first = await migrateWatcherDurableStore({
      backend,
      deploymentMarker: marker,
    });
    const second = await migrateWatcherDurableStore({
      backend,
      deploymentMarker: marker,
    });

    expect(first.initialized).toBe(true);
    expect(second.initialized).toBe(false);
    expect(second.encodedSha256).toBe(first.encodedSha256);
    expect(second.snapshot).toEqual(first.snapshot);
    expect(backend.writes).toBe(1);
    expect(second.snapshot).toEqual(makeEmptyWatcherDurableStore(marker));
  });

  it("recovers idempotently from a crash before atomic migration commit", async () => {
    const backend = new MemoryAtomicBackend();
    backend.failBeforeCommit = true;

    await expect(
      migrateWatcherDurableStore({ backend, deploymentMarker: marker }),
    ).rejects.toMatchObject({ code: "persistence_failure" });
    expect(backend.bytes).toBeNull();

    const recovered = await migrateWatcherDurableStore({
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
      migrateWatcherDurableStore({ backend, deploymentMarker: marker }),
    ).rejects.toMatchObject({ code: "persistence_failure" });
    expect(backend.bytes).not.toBeNull();

    const recovered = await migrateWatcherDurableStore({
      backend,
      deploymentMarker: marker,
    });
    expect(recovered.initialized).toBe(false);
    expect(backend.writes).toBe(1);
    expect(recovered.snapshot.schemaVersion).toBe(
      WATCHER_DURABLE_STORE_SCHEMA_VERSION,
    );
  });

  it("converges concurrent migrations through atomic compare-and-swap", async () => {
    const backend = new MemoryAtomicBackend();
    const results = await Promise.all([
      migrateWatcherDurableStore({ backend, deploymentMarker: marker }),
      migrateWatcherDurableStore({ backend, deploymentMarker: marker }),
    ]);

    expect(results.filter((result) => result.initialized)).toHaveLength(1);
    expect(new Set(results.map((result) => result.encodedSha256)).size).toBe(1);
    expect(backend.writes).toBe(1);
  });

  it("fails closed on marker drift, partial bytes, and exhausted migration conflicts", async () => {
    const initialized = new MemoryAtomicBackend(
      encodeWatcherDurableStore(makeEmptyWatcherDurableStore(marker)),
    );
    await expect(
      migrateWatcherDurableStore({
        backend: initialized,
        deploymentMarker: makeDeploymentMarker(hex32("bb")),
      }),
    ).rejects.toMatchObject({ code: "deployment_marker_mismatch" });

    const partial = new MemoryAtomicBackend(
      new TextEncoder().encode('{"schemaVersion":'),
    );
    await expect(
      migrateWatcherDurableStore({
        backend: partial,
        deploymentMarker: marker,
      }),
    ).rejects.toMatchObject({ code: "invalid_encoding" });

    const contended = new MemoryAtomicBackend();
    contended.alwaysConflict = true;
    await expect(
      migrateWatcherDurableStore({
        backend: contended,
        deploymentMarker: marker,
        maxConflicts: 2,
      }),
    ).rejects.toMatchObject({ code: "migration_conflict" });
  });

  it("exposes exact expected-prior atomic snapshots to higher-level journals", async () => {
    const backend = new MemoryAtomicBackend();
    const firstBytes = new TextEncoder().encode('{"revision":"0"}');
    const initialized = await compareAndSwapWatcherDurableAtomicSnapshot({
      backend,
      expectedSha256: null,
      next: firstBytes,
    });
    expect(initialized.committed).toBe(true);
    if (!initialized.committed) {
      throw new Error("expected initial atomic commit");
    }

    const snapshot = await readWatcherDurableAtomicSnapshot(backend);
    expect(snapshot).not.toBeNull();
    expect(snapshot?.sha256).toBe(initialized.sha256);
    firstBytes.fill(0);
    expect(new TextDecoder().decode(snapshot?.bytes)).toBe('{"revision":"0"}');

    const stale = await compareAndSwapWatcherDurableAtomicSnapshot({
      backend,
      expectedSha256: hex32("ff"),
      next: new TextEncoder().encode('{"revision":"1"}'),
    });
    expect(stale).toEqual({ committed: false });
    expect(stale).not.toHaveProperty("sha256");

    const contenders = await Promise.all([
      compareAndSwapWatcherDurableAtomicSnapshot({
        backend,
        expectedSha256: snapshot!.sha256,
        next: new TextEncoder().encode('{"writer":"a"}'),
      }),
      compareAndSwapWatcherDurableAtomicSnapshot({
        backend,
        expectedSha256: snapshot!.sha256,
        next: new TextEncoder().encode('{"writer":"b"}'),
      }),
    ]);
    expect(contenders.filter(({ committed }) => committed)).toHaveLength(1);
    expect(contenders.find(({ committed }) => !committed)).toEqual({
      committed: false,
    });
    expect(backend.writes).toBe(2);
  });
});
