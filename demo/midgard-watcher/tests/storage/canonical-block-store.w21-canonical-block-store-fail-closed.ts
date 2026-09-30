import "./canonical-block-store.w21-canonical-block-store-malformed-inputs.js";

import { describe, expect, it } from "vitest";

import {
  loadWatcherCanonicalBlockStore,
  parseWatcherCanonicalBlockRecord,
  persistWatcherCanonicalPublicBytes,
  type WatcherCanonicalBlockRecord,
} from "../../src/storage/canonical-block-store.js";
import {
  HEADER_HASH,
  identityOf,
  MARKER,
  OTHER_MANIFEST_ID,
  PEER,
  sha256Hex,
} from "./canonical-block-store.config-of.js";
import {
  cloned,
  envelope,
  expectStoreError,
  MemoryAtomicBackend,
  OBSERVED_AT_SLOT,
  payloadRecord,
  windowFor,
} from "./canonical-block-store.w21-canonical-block-store-hash-addressed-persistence.js";

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
        persistWatcherCanonicalPublicBytes({
          backend,
          deploymentIdentity: identityOf(),
          record: forged as unknown as WatcherCanonicalBlockRecord,
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
          persistWatcherCanonicalPublicBytes({
            backend,
            deploymentIdentity: identityOf(),
            record: forged as unknown as WatcherCanonicalBlockRecord,
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
      () => parseWatcherCanonicalBlockRecord(forged),
      "invalid_field",
    );
  });

  it("refuses a record whose deployment marker is not this deployment", async () => {
    const backend = new MemoryAtomicBackend();
    const error = await expectStoreError(
      async () =>
        persistWatcherCanonicalPublicBytes({
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
    await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    await expectStoreError(
      async () =>
        loadWatcherCanonicalBlockStore({
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
        persistWatcherCanonicalPublicBytes({
          backend,
          deploymentIdentity: identityOf(),
          record: payloadRecord,
        }),
      "persistence_failure",
    );
    await expectStoreError(
      async () =>
        loadWatcherCanonicalBlockStore({
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
        persistWatcherCanonicalPublicBytes({
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
        persistWatcherCanonicalPublicBytes({
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
        persistWatcherCanonicalPublicBytes({
          backend,
          deploymentIdentity: identityOf(),
          record: payloadRecord,
        }),
      "persistence_failure",
    );

    // The process is gone; a fresh reader sees the committed snapshot.
    const restarted = new MemoryAtomicBackend(backend.bytes);
    const loaded = await loadWatcherCanonicalBlockStore({
      backend: restarted,
      deploymentIdentity: identityOf(),
      retentionWindow: windowFor(),
    });
    expect(loaded!.snapshot.records).toHaveLength(1);
    expect(
      Buffer.from(loaded!.snapshot.records[0]!.input.payload.cborHex, "hex"),
    ).toEqual(envelope);

    // Re-driving the same persist is the idempotent no-op, not a duplicate.
    const replay = await persistWatcherCanonicalPublicBytes({
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
        persistWatcherCanonicalPublicBytes({
          backend,
          deploymentIdentity: identityOf(),
          record: payloadRecord,
        }),
      "persistence_failure",
    );
    expect(
      await loadWatcherCanonicalBlockStore({
        backend,
        deploymentIdentity: identityOf(),
      }),
    ).toBeNull();
  });

  it("retries a lost compare-and-swap race without a partial write", async () => {
    const backend = new MemoryAtomicBackend();
    backend.conflictOnce = true;
    const result = await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    expect(result.committed).toBe(true);
    expect(backend.writes).toBe(1);
    const loaded = await loadWatcherCanonicalBlockStore({
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
    const other = parseWatcherCanonicalBlockRecord({
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
    await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identity,
      record: other,
    });
    backend.conflictOnce = true;
    await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identity,
      record: payloadRecord,
    });

    const loaded = await loadWatcherCanonicalBlockStore({
      backend,
      deploymentIdentity: identity,
    });
    expect(loaded!.snapshot.records).toHaveLength(2);
    expect(loaded!.snapshot.revision).toBe("2");
  });
});
