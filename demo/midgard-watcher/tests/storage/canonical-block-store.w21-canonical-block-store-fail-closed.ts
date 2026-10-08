import "./canonical-block-store.w21-canonical-block-store-malformed-inputs.js";

import { describe, expect, it } from "vitest";

import {
  parseWatcherCanonicalBlockRecord,
  pruneWatcherCanonicalBlockStore,
} from "../../src/storage/canonical-block-store.js";
import {
  identityOf,
  OTHER_MANIFEST_ID,
} from "./canonical-block-store.config-of.js";
import {
  cloned,
  expectStoreError,
  type MemoryAtomicBackend,
  payloadRecord,
  storeOf,
  windowFor,
} from "./canonical-block-store.w21-canonical-block-store-hash-addressed-persistence.js";

// ---------------------------------------------------------------------------
// 6. Fail-closed behaviour
// ---------------------------------------------------------------------------

describe("W21 canonical block store: fail-closed", () => {
  const pruneExpired = (
    backend: MemoryAtomicBackend,
    deploymentIdentity = identityOf(),
  ) =>
    pruneWatcherCanonicalBlockStore({
      backend,
      deploymentIdentity,
      atSlot: payloadRecord.metadata.retainUntilSlot + 1,
      stillChallengeableInputIds: [],
      retentionWindow: windowFor(),
    });

  it("refuses operator-private provenance", async () => {
    const forged = cloned(payloadRecord);
    forged.metadata.provenance.trustClass = "operator_private_file";
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(forged),
      "provenance_not_public_da",
    );
  });

  it("refuses an admitted trust class that is not public or permissionless DA", async () => {
    for (const trustClass of [
      "authenticated_cardano_l1",
      "signed_deployment_identity",
      "deterministic_local_computation",
    ]) {
      const forged = cloned(payloadRecord);
      forged.metadata.provenance.trustClass = trustClass;
      await expectStoreError(
        () => parseWatcherCanonicalBlockRecord(forged),
        "provenance_not_public_da",
      );
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

  it("refuses to prune a snapshot written under another deployment", async () => {
    const backend = storeOf(payloadRecord);
    await expectStoreError(
      async () => pruneExpired(backend, identityOf(OTHER_MANIFEST_ID)),
      "deployment_marker_mismatch",
    );
    expect(backend.writes).toBe(0);
  });

  it("surfaces a backend read fault as persistence_failure, never as success", async () => {
    const backend = storeOf(payloadRecord);
    backend.failRead = true;
    await expectStoreError(
      async () => pruneExpired(backend),
      "persistence_failure",
    );
    expect(backend.writes).toBe(0);
  });

  it("surfaces a backend commit fault as persistence_failure and keeps the snapshot", async () => {
    const backend = storeOf(payloadRecord);
    const before = Uint8Array.from(backend.bytes!);
    backend.failBeforeCommit = true;
    await expectStoreError(
      async () => pruneExpired(backend),
      "persistence_failure",
    );
    expect(backend.writes).toBe(0);
    expect(backend.bytes).toEqual(before);
  });

  it("gives up deterministically when compare-and-swap never wins", async () => {
    const backend = storeOf(payloadRecord);
    const before = Uint8Array.from(backend.bytes!);
    backend.alwaysConflict = true;
    await expectStoreError(async () => pruneExpired(backend), "cas_contention");
    expect(backend.writes).toBe(0);
    expect(backend.bytes).toEqual(before);
  });

  it("retries a lost compare-and-swap race without a partial write", async () => {
    const backend = storeOf(payloadRecord);
    backend.conflictOnce = true;
    const result = await pruneExpired(backend);
    expect(result.committed).toBe(true);
    expect(result.revision).toBe("2");
    expect(backend.writes).toBe(1);
  });
});
