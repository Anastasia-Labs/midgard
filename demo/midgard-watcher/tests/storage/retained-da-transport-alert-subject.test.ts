import { createHash } from "node:crypto";

import {
  DaRequestResponseProtocol,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadChunkRequestCbor,
  encodeDaTraceStepByIndexRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { watcherDaFetchAlertSubject } from "../../src/runtime/operations-observability.alert-book.js";
import { watcherDaFetchSubjectDigest } from "../../src/storage/retained-da-runtime.watcher-retained-da-libp2p-transport.js";

const HEADER = "ab".repeat(28);
const FINGERPRINT = Buffer.from("dd".repeat(32), "hex");
const headerHash = Buffer.from(HEADER, "hex");

describe("retained-DA fetch failure subject", () => {
  it("names the header for every request about it, so one outcome clears them all", () => {
    const requests = [
      {
        protocol: DaRequestResponseProtocol.payloadByHeader,
        payload: encodeDaPayloadByHeaderRequestCbor({
          deploymentFingerprint: FINGERPRINT,
          headerHash,
          acceptedPayloadHashes: null,
          maxInlineBytes: 1_024,
        }),
      },
      {
        protocol: DaRequestResponseProtocol.metadataByHeader,
        payload: encodeDaPayloadByHeaderRequestCbor({
          deploymentFingerprint: FINGERPRINT,
          headerHash,
          acceptedPayloadHashes: null,
          maxInlineBytes: 0,
        }),
      },
      {
        protocol: DaRequestResponseProtocol.payloadChunk,
        payload: encodeDaPayloadChunkRequestCbor({
          deploymentFingerprint: FINGERPRINT,
          headerHash,
          payloadHash: Buffer.from("ef".repeat(32), "hex"),
          chunkIndex: 3,
        }),
      },
      {
        protocol: DaRequestResponseProtocol.traceStepByIndex,
        payload: encodeDaTraceStepByIndexRequestCbor({
          deploymentFingerprint: FINGERPRINT,
          headerHash,
          stepIndex: 7,
        }),
      },
    ];
    for (const request of requests)
      expect(watcherDaFetchSubjectDigest(request)).toBe(
        watcherDaFetchAlertSubject(HEADER),
      );
  });

  it("keeps the payload digest for a request it cannot read as one about a header", () => {
    const payload = Buffer.from("not a request");
    const digest = createHash("sha256").update(payload).digest("hex");
    expect(
      watcherDaFetchSubjectDigest({
        protocol: DaRequestResponseProtocol.payloadByHeader,
        payload,
      }),
    ).toBe(digest);
    expect(
      watcherDaFetchSubjectDigest({
        protocol: DaRequestResponseProtocol.capabilities,
        payload,
      }),
    ).toBe(digest);
  });
});
