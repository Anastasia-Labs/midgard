import { describe, expect, it } from "vitest";

import {
  E2E_DA_GATE_SCHEMA_VERSION,
  probeDaGate,
  waitForDaGate,
} from "@/e2e/da-gates.js";

const headerHash = "ab".repeat(28);

describe("e2e DA publication gates", () => {
  it("classifies threshold libp2p publication as satisfied", async () => {
    const result = await probeDaGate({
      headerHash,
      publicationReport: {
        configured: true,
        headerHash,
        payloadHash: "cd".repeat(32),
        deploymentFingerprint: "ef".repeat(32),
        threshold: 2,
        acceptedPeers: 2,
        peerResults: [
          peerResult(0, "peer-a", "accepted"),
          peerResult(1, "peer-b", "duplicate"),
        ],
        announcement: {
          topic: `/midgard/${"ef".repeat(32)}/da/payload-announcements/1`,
          payloadHash: "cd".repeat(32),
          recipients: ["peer-a", "peer-b"],
        },
      },
    });

    expect(result.schemaVersion).toBe(E2E_DA_GATE_SCHEMA_VERSION);
    expect(result.status).toBe("satisfied");
    expect(result.nextSafeAction).toBe("continue");
    expect(result.acceptedPeers).toBe(2);
    expect(result.announcementRecipients).toEqual(["peer-a", "peer-b"]);
  });

  it("blocks when libp2p DA publication is not configured", async () => {
    const result = await probeDaGate({
      headerHash,
      publicationReport: {
        configured: false,
        headerHash,
        payloadHash: "cd".repeat(32),
        acceptedPeers: 0,
        peerResults: [],
        reason: "no libp2p DA manifest configured",
      },
    });

    expect(result.status).toBe("blocked");
    expect(result.nextSafeAction).toBe("configure_da_libp2p");
  });

  it("keeps below-threshold publication pending", async () => {
    const result = await probeDaGate({
      headerHash,
      publicationReport: {
        configured: true,
        headerHash,
        payloadHash: "cd".repeat(32),
        deploymentFingerprint: "ef".repeat(32),
        threshold: 2,
        acceptedPeers: 1,
        peerResults: [
          peerResult(0, "peer-a", "accepted"),
          peerResult(1, "peer-b", "transport_error"),
        ],
        announcement: {
          topic: `/midgard/${"ef".repeat(32)}/da/payload-announcements/1`,
          payloadHash: "cd".repeat(32),
          recipients: ["peer-a"],
        },
      },
    });

    expect(result.status).toBe("pending");
    expect(result.nextSafeAction).toBe("wait_for_da_payload_publication");
  });

  it("waits through bounded publication lag", async () => {
    let attempts = 0;
    const result = await waitForDaGate({
      headerHash,
      intervalMs: 1,
      sleep: async () => {},
      probePublication: async () => {
        attempts += 1;
        return {
          configured: true,
          headerHash,
          payloadHash: "cd".repeat(32),
          deploymentFingerprint: "ef".repeat(32),
          threshold: 2,
          acceptedPeers: attempts === 1 ? 1 : 2,
          peerResults:
            attempts === 1
              ? [
                  peerResult(0, "peer-a", "accepted"),
                  peerResult(1, "peer-b", "transport_error"),
                ]
              : [
                  peerResult(0, "peer-a", "accepted"),
                  peerResult(1, "peer-b", "accepted"),
                ],
          announcement: {
            topic: `/midgard/${"ef".repeat(32)}/da/payload-announcements/1`,
            payloadHash: "cd".repeat(32),
            recipients: attempts === 1 ? ["peer-a"] : ["peer-a", "peer-b"],
          },
        };
      },
    });

    expect(result.status).toBe("satisfied");
    expect(result.attempts).toBe(2);
    expect(result.timedOut).toBe(false);
  });

  it("rejects malformed header hashes", async () => {
    await expect(
      probeDaGate({
        headerHash: "not-a-header",
        publicationReport: {
          configured: false,
          headerHash: "not-a-header",
          payloadHash: "cd".repeat(32),
          acceptedPeers: 0,
          peerResults: [],
        },
      }),
    ).rejects.toThrow("56-character hex");
  });
});

const peerResult = (
  signerIndex: number,
  peerId: string,
  status: "accepted" | "duplicate" | "transport_error",
) => ({
  peerId,
  signerIndex,
  protocolId: `/midgard/${"ef".repeat(32)}/da/payload-submit/1`,
  status,
  payloadHash: "cd".repeat(32),
});
