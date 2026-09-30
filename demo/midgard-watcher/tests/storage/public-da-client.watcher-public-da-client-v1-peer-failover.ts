import "./public-da-client.watcher-public-da-client-v1-chunked-payload-retrieval.js";

import { describe, expect, it } from "vitest";

import {
  hangUntilAborted,
  HEADER_HASH,
  PEERS,
  type PeerScript,
  ScriptedTransport,
} from "./public-da-client.raw-config.js";
import {
  capabilitiesBytes,
  clientWith,
  expectClientError,
  fixture,
  honestInlineScript,
  payloadByHeaderBytes,
  statuses,
} from "./public-da-client.watcher-public-da-client-v1-construction.js";

// ---------------------------------------------------------------------------
// 7. Peer failover
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 peer failover", () => {
  const honestPeer = (identity: string): Record<string, PeerScript> => ({
    [identity]: {
      capabilities: () => capabilitiesBytes(),
      "payload-by-header": () =>
        payloadByHeaderBytes({
          status: "found_inline",
          payloadHash: fixture.payloadHash,
          payloadBytes: fixture.envelope,
        }),
    },
  });

  it("fails over from a transport-level failure to the next peer", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => {
          throw new Error("dial refused");
        },
      },
      ...honestPeer(PEERS[1]!),
    });
    const result = await clientWith(transport, {
      peerCount: 2,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    expect(result.sourcePeerIdentity).toBe(PEERS[1]);
    expect(result.attempts).toHaveLength(2);
    expect(result.attempts[0]).toEqual({
      peerIdentity: PEERS[0],
      protocol: "capabilities",
      status: "transport_error",
    });
    expect(result.attempts[1]!.status).toBe("success");
  });

  it.each([
    ["not_found", "not_found"],
    ["conflict", "peer_conflict"],
    ["rejected", "peer_rejected"],
  ] as const)(
    "records a %s answer and fails over to an honest peer",
    async (status, expectedStatus) => {
      const transport = new ScriptedTransport({
        [PEERS[0]!]: {
          capabilities: () => capabilitiesBytes(),
          "payload-by-header": () => payloadByHeaderBytes({ status }),
        },
        ...honestPeer(PEERS[1]!),
      });
      const result = await clientWith(transport, {
        peerCount: 2,
      }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

      expect(result.sourcePeerIdentity).toBe(PEERS[1]);
      expect(statuses(result.attempts)).toEqual([expectedStatus, "success"]);
      expect(result.attempts[0]!.protocol).toBe("payload-by-header");
    },
  );

  it("fails over from a peer serving corrupted bytes to an honest peer", async () => {
    const corrupted = Buffer.from(fixture.envelope);
    corrupted[0] ^= 0xff;
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "payload-by-header": () =>
          payloadByHeaderBytes({
            status: "found_inline",
            payloadHash: fixture.payloadHash,
            payloadBytes: corrupted,
          }),
      },
      ...honestPeer(PEERS[1]!),
    });
    const result = await clientWith(transport, {
      peerCount: 2,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    expect(statuses(result.attempts)).toEqual(["invalid_content", "success"]);
    expect(result.payloadEnvelopeCbor.equals(fixture.envelope)).toBe(true);
  });

  it("stops at the first success and does not dial later peers", async () => {
    const transport = new ScriptedTransport({
      ...honestPeer(PEERS[0]!),
      [PEERS[1]!]: {
        capabilities: () => {
          throw new Error("should never be dialed");
        },
      },
    });
    const result = await clientWith(transport, {
      peerCount: 2,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    expect(result.attempts).toHaveLength(1);
    expect(transport.protocolsFor(PEERS[1]!)).toEqual([]);
  });

  it("reports all_peers_failed with one attempt per peer when every peer fails", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "payload-by-header": () =>
          payloadByHeaderBytes({ status: "not_found" }),
      },
      [PEERS[1]!]: {
        capabilities: () => capabilitiesBytes(),
        "payload-by-header": () => payloadByHeaderBytes({ status: "rejected" }),
      },
      [PEERS[2]!]: {
        capabilities: () => {
          throw new Error("dial refused");
        },
      },
    });
    const error = await expectClientError(
      clientWith(transport, { peerCount: 3 }).fetchPayloadByHeader({
        headerHash: HEADER_HASH,
      }),
    );
    expect(error.code).toBe("all_peers_failed");
    expect(statuses(error.attempts)).toEqual([
      "not_found",
      "peer_rejected",
      "transport_error",
    ]);
    expect(error.attempts.map((a) => a.peerIdentity)).toEqual([
      PEERS[0],
      PEERS[1],
      PEERS[2],
    ]);
  });

  it("records a per-peer timeout and continues to the next peer", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: { capabilities: hangUntilAborted },
      ...honestPeer(PEERS[1]!),
    });
    const result = await clientWith(transport, {
      peerCount: 2,
      requestTimeoutMs: 120,
      daFetchMs: 5_000,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    expect(statuses(result.attempts)).toEqual(["timeout", "success"]);
  });

  it("freezes the returned attempt ledger", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    expect(Object.isFrozen(result)).toBe(true);
    expect(Object.isFrozen(result.attempts)).toBe(true);
  });
});
