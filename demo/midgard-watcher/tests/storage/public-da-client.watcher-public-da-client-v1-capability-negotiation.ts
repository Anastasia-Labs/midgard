import {
  type DaCapabilitiesResponse,
  daDeploymentFingerprintFromHex,
  daRequestResponseProtocolId,
  decodeDaPayloadByHeaderRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { WatcherPublicDaClient } from "../../src/storage/public-da-client.js";
import {
  FINGERPRINT,
  HEADER_HASH,
  multiaddrFor,
  OTHER_FINGERPRINT,
  PEERS,
  repeatHex,
  ScriptedTransport,
} from "./public-da-client.raw-config.js";
import {
  capabilitiesBytes,
  clientWith,
  expectClientError,
  fixture,
  honestInlineScript,
  statuses,
} from "./public-da-client.watcher-public-da-client-v1-construction.js";

// ---------------------------------------------------------------------------
// 2. Request-argument validation
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 request validation", () => {
  const client = (): WatcherPublicDaClient =>
    clientWith(new ScriptedTransport({}));

  it.each([
    ["uppercase hex", HEADER_HASH.toUpperCase()],
    ["too short", repeatHex(0xab, 27)],
    ["too long", repeatHex(0xab, 29)],
    ["non-hex", "z".repeat(56)],
    ["empty", ""],
  ])(
    "rejects a %s header hash without dialing a peer",
    async (_label, hash) => {
      const transport = new ScriptedTransport({});
      const error = await expectClientError(
        clientWith(transport).fetchPayloadByHeader({ headerHash: hash }),
      );
      expect(error.code).toBe("invalid_request");
      expect(transport.calls).toHaveLength(0);
    },
  );

  it.each([
    ["empty", []],
    ["duplicated", [repeatHex(0x01, 32), repeatHex(0x01, 32)]],
    [
      "over the 64-entry cap",
      Array.from({ length: 65 }, (_, i) => repeatHex(i % 256, 32)),
    ],
    ["wrong length", [repeatHex(0x01, 31)]],
  ])(
    "rejects %s acceptedPayloadHashes",
    async (_label, acceptedPayloadHashes) => {
      const error = await expectClientError(
        client().fetchPayloadByHeader({
          headerHash: HEADER_HASH,
          acceptedPayloadHashes,
        }),
      );
      expect(error.code).toBe("invalid_request");
    },
  );

  it.each([-1, 1.5, Number.NaN, Number.MAX_SAFE_INTEGER + 2])(
    "rejects step index %s",
    async (stepIndex) => {
      const error = await expectClientError(
        client().fetchTraceStepByIndex({ headerHash: HEADER_HASH, stepIndex }),
      );
      expect(error.code).toBe("invalid_request");
    },
  );

  it.each([
    ["empty string", ""],
    ["odd-length hex", "abc"],
    ["uppercase hex", "ABCD"],
    ["empty bytes", new Uint8Array(0)],
    ["oversized bytes", new Uint8Array(4_097)],
  ])("rejects a %s event key", async (_label, eventKey) => {
    const error = await expectClientError(
      client().fetchEventToStepByEvent({ headerHash: HEADER_HASH, eventKey }),
    );
    expect(error.code).toBe("invalid_request");
  });
});

// ---------------------------------------------------------------------------
// 3. Capability negotiation
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 capability negotiation", () => {
  it("negotiates before requesting and clamps limits to the protocol ceiling", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });

    expect(result.payloadHash).toBe(fixture.payloadHash.toString("hex"));
    expect(transport.protocolsFor(PEERS[0]!)).toEqual([
      "capabilities",
      "payload-by-header",
    ]);
    const inlineRequest = decodeDaPayloadByHeaderRequestCbor(
      transport.calls[1]!.requestCbor,
    );
    // maxInlineBytes echoes the negotiated (clamped) inline ceiling.
    expect(inlineRequest.maxInlineBytes).toBe(500_000);
    expect(inlineRequest.headerHash.toString("hex")).toBe(HEADER_HASH);
  });

  it("dials the deployment-scoped protocol id", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    expect(transport.calls[0]!.protocolId).toBe(
      daRequestResponseProtocolId(FINGERPRINT, "capabilities"),
    );
    expect(transport.calls[0]!.multiaddr).toBe(multiaddrFor(0));
  });

  it("rejects capabilities announcing a foreign deployment fingerprint", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes({
            deploymentFingerprint:
              daDeploymentFingerprintFromHex(OTHER_FINGERPRINT),
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(error.code).toBe("all_peers_failed");
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
    expect(error.attempts[0]!.protocol).toBe("capabilities");
  });

  it.each([
    ["zero maxPayloadBytes", { maxPayloadBytes: 0 }],
    ["zero maxChunkBytes", { maxChunkBytes: 0 }],
    ["zero maxStreamsPerPeer", { maxStreamsPerPeer: 0 }],
    ["zero requestTimeoutMs", { requestTimeoutMs: 0 }],
    [
      "inline ceiling above payload ceiling",
      { maxPayloadBytes: 1_000, maxInlineResponseBytes: 2_000 },
    ],
    [
      "chunk ceiling above payload ceiling",
      {
        maxPayloadBytes: 1_000,
        maxInlineResponseBytes: 500,
        maxChunkBytes: 2_000,
      },
    ],
  ])("rejects capabilities with %s", async (_label, overrides) => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes(overrides as Partial<DaCapabilitiesResponse>),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(error.code).toBe("all_peers_failed");
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("rejects undecodable capability bytes", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => Buffer.from("not-cbor-at-all"),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });
});
