import "./canonical-block-store.w21-canonical-block-store-retention-window.js";

import { describe, expect, it } from "vitest";

import {
  loadWatcherCanonicalBlockStore,
  makeWatcherCanonicalProofBundleRecord,
  parseWatcherCanonicalBlockRecord,
  persistWatcherCanonicalPublicBytes,
  pruneWatcherCanonicalBlockStore,
  type WatcherCanonicalBlockRecord,
} from "../../src/storage/canonical-block-store.js";
import {
  identityOf,
  publicDaProofBundle,
  repeatHex,
  sha256Hex,
} from "./canonical-block-store.config-of.js";
import {
  cloned,
  contextOf,
  envelope,
  expectStoreError,
  innerCbor,
  MemoryAtomicBackend,
  payloadRecord,
  windowFor,
} from "./canonical-block-store.w21-canonical-block-store-hash-addressed-persistence.js";

describe("W21 canonical block store: prune boundaries", () => {
  const persisted = async (): Promise<MemoryAtomicBackend> => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytes({
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
    pruneWatcherCanonicalBlockStore({
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
    const loaded = await loadWatcherCanonicalBlockStore({
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
    const loaded = await loadWatcherCanonicalBlockStore({
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
    const result = await pruneWatcherCanonicalBlockStore({
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
    await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    return backend;
  };

  it("rejects a proof_input record whose stored bytes were flipped underneath the digest", async () => {
    const backend = new MemoryAtomicBackend();
    const bundle = makeWatcherCanonicalProofBundleRecord({
      proofBundle: publicDaProofBundle(),
      context: contextOf(),
    });
    await persistWatcherCanonicalPublicBytes({
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
        loadWatcherCanonicalBlockStore({
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
        loadWatcherCanonicalBlockStore({
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
      () => parseWatcherCanonicalBlockRecord(forged),
      "integrity_mismatch",
    );
  });

  it("rejects a lie about envelopeSha256 while innerSha256 stays correct", async () => {
    const forged = cloned(payloadRecord);
    forged.metadata.envelopeSha256 = repeatHex(0xbb, 32);
    expect(forged.metadata.innerSha256).toBe(sha256Hex(innerCbor));
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(forged),
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
        persistWatcherCanonicalPublicBytes({
          backend,
          deploymentIdentity: identityOf(),
          record: forged as unknown as WatcherCanonicalBlockRecord,
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
      () => parseWatcherCanonicalBlockRecord(forged),
      "integrity_mismatch",
    );
  });

  it("rejects a byteLength that disagrees with the stored bytes", async () => {
    const forged = cloned(payloadRecord);
    forged.metadata.byteLength = envelope.length + 1;
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(forged),
      "integrity_mismatch",
    );
  });
});
