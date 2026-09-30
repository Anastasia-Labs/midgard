import "./public-da-client.watcher-public-da-client-v1-capability-negotiation.js";

import {
  DaPayloadContentEncoding,
  encodeDaPayloadEnvelope,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  decodeDaPayloadByHeaderRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  FINGERPRINT,
  HEADER_HASH,
  OTHER_HEADER_HASH,
  PEERS,
  repeatHex,
  ScriptedTransport,
} from "./public-da-client.raw-config.js";
import {
  chunksOf,
  clientWith,
  expectClientError,
  fixture,
  honestInlineScript,
  otherHeaderFixture,
  statuses,
} from "./public-da-client.watcher-public-da-client-v1-construction.js";

// ---------------------------------------------------------------------------
// 4. Inline payload: SHA-256 verification, both directions
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 inline payload verification", () => {
  it("accepts an inline payload whose SHA-256 matches the announced hash", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });

    expect(result.schemaVersion).toBe("midgard-watcher-public-da-client-v1");
    expect(result.deploymentFingerprint).toBe(FINGERPRINT);
    expect(result.headerHash).toBe(HEADER_HASH);
    expect(result.payloadHash).toBe(fixture.payloadHash.toString("hex"));
    expect(result.payloadEnvelopeCbor.equals(fixture.envelope)).toBe(true);
    expect(result.innerPayloadCbor.equals(fixture.innerCbor)).toBe(true);
    expect(result.sourcePeerIdentity).toBe(PEERS[0]);
    expect(result.durableInput.kind).toBe("da_payload");
    expect(result.durableInput.inputId).toBe(
      fixture.payloadHash.toString("hex"),
    );
    expect(result.durableInput.payload.cborHex).toBe(
      fixture.envelope.toString("hex"),
    );
    expect(statuses(result.attempts)).toEqual(["success"]);
  });

  it("REJECTS an inline payload whose bytes were corrupted after hashing", async () => {
    const corrupted = Buffer.from(fixture.envelope);
    corrupted[corrupted.length - 1] ^= 0xff;
    expect(corrupted.equals(fixture.envelope)).toBe(false);

    const transport = new ScriptedTransport(
      honestInlineScript({ payloadBytes: corrupted }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(error.code).toBe("all_peers_failed");
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
    expect(error.attempts[0]!.protocol).toBe("payload-by-header");
  });

  it("REJECTS an honest payload announced under a foreign payload hash", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        payloadHash: computeDaSha256Hash(Buffer.from("x")),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an envelope whose declared inner SHA-256 does not match its body", async () => {
    const tamperedInner = Buffer.from(fixture.innerCbor);
    tamperedInner[tamperedInner.length - 1] ^= 0x01;
    // Envelope is internally inconsistent: body is tampered but innerSha256
    // still commits to the original inner bytes.
    const envelope = encodeDaPayloadEnvelope({
      version: 1,
      contentEncoding: DaPayloadContentEncoding.identity,
      innerBytes: fixture.innerCbor.length,
      innerSha256: computeDaSha256Hash(fixture.innerCbor),
      body: tamperedInner,
    });

    const transport = new ScriptedTransport(
      honestInlineScript({
        payloadHash: computeDaSha256Hash(envelope),
        payloadBytes: envelope,
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an envelope carrying a payload for a different header", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        payloadHash: otherHeaderFixture.payloadHash,
        payloadBytes: otherHeaderFixture.envelope,
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a response echoing a different header hash", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        headerHash: Buffer.from(OTHER_HEADER_HASH, "hex"),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a payload not present in acceptedPayloadHashes", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({
        headerHash: HEADER_HASH,
        acceptedPayloadHashes: [repeatHex(0xee, 32)],
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("accepts a payload present in acceptedPayloadHashes", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
      acceptedPayloadHashes: [
        repeatHex(0xee, 32),
        fixture.payloadHash.toString("hex"),
      ],
    });
    expect(result.payloadHash).toBe(fixture.payloadHash.toString("hex"));
    const request = decodeDaPayloadByHeaderRequestCbor(
      transport.calls[1]!.requestCbor,
    );
    expect(request.acceptedPayloadHashes).toHaveLength(2);
  });

  it("REJECTS an inline response that also carries a chunk manifest", async () => {
    const { manifest } = chunksOf(fixture.envelope, 64);
    const transport = new ScriptedTransport(
      honestInlineScript({ chunkManifest: manifest }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a chunked response that also carries inline bytes", async () => {
    const { manifest } = chunksOf(fixture.envelope, 64);
    const transport = new ScriptedTransport(
      honestInlineScript({
        status: "found_chunked",
        chunkManifest: manifest,
        payloadBytes: fixture.envelope,
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a chunked response with no manifest at all", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        status: "found_chunked",
        payloadBytes: null,
        chunkManifest: null,
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });
});
