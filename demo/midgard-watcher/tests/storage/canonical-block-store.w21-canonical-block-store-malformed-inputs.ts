import "./canonical-block-store.w21-canonical-block-store-mutation-rejection.js";

import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  decodeWatcherCanonicalBlockStoreSnapshot,
  encodeWatcherCanonicalBlockStoreSnapshot,
  parseWatcherCanonicalBlockRecord,
  persistWatcherCanonicalPublicBytes,
  WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
} from "../../src/storage/canonical-block-store.js";
import {
  watcherCanonicalJson,
  watcherDurableStoreBytesSha256,
} from "../../src/storage/durable-store.js";
import {
  identityOf,
  MARKER,
  sha256Hex,
} from "./canonical-block-store.config-of.js";
import {
  cloned,
  expectStoreError,
  MemoryAtomicBackend,
  payloadRecord,
} from "./canonical-block-store.w21-canonical-block-store-hash-addressed-persistence.js";

// ---------------------------------------------------------------------------
// 5. Malformed inputs
// ---------------------------------------------------------------------------

describe("W21 canonical block store: malformed inputs", () => {
  it("rejects non-hex, odd-length, and zero-length cborHex", async () => {
    for (const cborHex of ["zz", "abc", ""]) {
      const forged = cloned(payloadRecord);
      forged.input.payload.cborHex = cborHex;
      await expectStoreError(
        () => parseWatcherCanonicalBlockRecord(forged),
        "invalid_field",
      );
    }
  });

  it("rejects a payload above the DA transport payload ceiling", async () => {
    const oversize = Buffer.alloc(DA_TRANSPORT_LIMITS.maxPayloadBytes + 1);
    const forged = cloned(payloadRecord);
    forged.input.payload.cborHex = oversize.toString("hex");
    forged.input.payload.sha256 = sha256Hex(oversize);
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(forged),
      "invalid_field",
    );
  });

  it("rejects an unknown kind and an unknown contentKind", async () => {
    const badKind = cloned(payloadRecord);
    badKind.input.kind = "surprise";
    badKind.metadata.kind = "surprise";
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(badKind),
      "invalid_field",
    );

    const badContentKind = cloned(payloadRecord);
    badContentKind.metadata.contentKind = "surprise";
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(badContentKind),
      "invalid_field",
    );
  });

  it("rejects a contentKind that contradicts the reserved record kind", async () => {
    const forged = cloned(payloadRecord);
    forged.metadata.contentKind = "proof_bundle";
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(forged),
      "invalid_field",
    );
  });

  it("rejects missing and extra metadata keys", async () => {
    const missing = cloned(payloadRecord);
    delete missing.metadata.headerHash;
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(missing),
      "missing_field",
    );

    const extra = cloned(payloadRecord);
    extra.metadata.surprise = 1;
    await expectStoreError(
      () => parseWatcherCanonicalBlockRecord(extra),
      "unknown_field",
    );
  });

  it("rejects a non-canonical, a truncated, and a non-UTF8 snapshot encoding", async () => {
    const backend = new MemoryAtomicBackend();
    await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    const canonical = new TextDecoder().decode(backend.bytes!);

    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshot(
          new TextEncoder().encode(` ${canonical}`),
        ),
      "noncanonical_encoding",
    );
    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshot(
          backend.bytes!.slice(0, backend.bytes!.length - 5),
        ),
      "invalid_encoding",
    );
    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshot(
          Uint8Array.from([0xff, 0xfe, 0xfd]),
        ),
      "invalid_encoding",
    );
  });

  it("rejects a snapshot with the wrong schema version and an unsorted record set", async () => {
    await expectStoreError(
      () =>
        decodeWatcherCanonicalBlockStoreSnapshot(
          new TextEncoder().encode(
            watcherCanonicalJson({
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
        decodeWatcherCanonicalBlockStoreSnapshot(
          new TextEncoder().encode(
            watcherCanonicalJson({
              schemaVersion: WATCHER_CANONICAL_BLOCK_STORE_SCHEMA_VERSION,
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
    await persistWatcherCanonicalPublicBytes({
      backend,
      deploymentIdentity: identityOf(),
      record: payloadRecord,
    });
    const snapshot = decodeWatcherCanonicalBlockStoreSnapshot(backend.bytes!);
    expect(encodeWatcherCanonicalBlockStoreSnapshot(snapshot)).toEqual(
      backend.bytes,
    );
    expect(watcherDurableStoreBytesSha256(backend.bytes!)).toHaveLength(64);
  });
});
