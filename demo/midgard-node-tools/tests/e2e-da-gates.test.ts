import { describe, expect, it } from "vitest";

import {
  E2E_DA_GATE_SCHEMA_VERSION,
  parseDaGateProbeResultV1,
  parseDaGateResultV1,
  parseWaitForDaGateResultV1,
  probeDaGate,
  waitForDaGate,
} from "../src/e2e/da-gates.js";

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
    expect(result.kind).toBe("probe");
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
    expect(result.kind).toBe("wait");
    expect(result.attempts).toBe(2);
    expect(result.timedOut).toBe(false);
  });

  it("rejects missing, extra, wrong-version, and cross-kind artifacts", async () => {
    const probe = await probeDaGate({
      headerHash,
      publicationReport: {
        configured: true,
        headerHash,
        payloadHash: "cd".repeat(32),
        deploymentFingerprint: "ef".repeat(32),
        threshold: 1,
        acceptedPeers: 1,
        peerResults: [peerResult(0, "peer-a", "accepted")],
        announcement: {
          topic: `/midgard/${"ef".repeat(32)}/da/payload-announcements/1`,
          payloadHash: "cd".repeat(32),
          recipients: ["peer-a"],
        },
      },
      now: new Date("2026-01-01T00:00:00.000Z"),
    });
    expect(parseDaGateProbeResultV1(probe)).toEqual(probe);
    expect(parseDaGateResultV1(probe)).toEqual(probe);
    const { reason: _reason, ...missingReason } = probe;
    expect(() => parseDaGateProbeResultV1(missingReason)).toThrow(
      "missing required field",
    );
    expect(() =>
      parseDaGateProbeResultV1({ ...probe, unexpected: true }),
    ).toThrow("unknown field");
    expect(() =>
      parseDaGateProbeResultV1({
        ...probe,
        schemaVersion: "midgard-e2e-da-gate-v0",
      }),
    ).toThrow(E2E_DA_GATE_SCHEMA_VERSION);
    expect(() =>
      parseWaitForDaGateResultV1({
        ...probe,
        kind: "wait",
        attempts: 0,
        timedOut: false,
      }),
    ).toThrow("positive safe integer");
    expect(() =>
      parseWaitForDaGateResultV1({
        ...probe,
        attempts: 1,
        timedOut: false,
      }),
    ).toThrow(".kind must be wait");
    expect(() =>
      parseDaGateProbeResultV1({ ...probe, acceptedPeers: 0 }),
    ).toThrow("publication evidence is inconsistent");
    expect(() =>
      parseDaGateProbeResultV1({
        ...probe,
        status: "pending",
        nextSafeAction: "wait_for_da_payload_publication",
      }),
    ).toThrow("publication evidence is inconsistent");
    expect(() =>
      parseDaGateProbeResultV1({
        ...probe,
        peerResults: [
          probe.peerResults[0],
          { ...probe.peerResults[0], peerId: "peer-b" },
        ],
        acceptedPeers: 2,
      }),
    ).toThrow("publication evidence is inconsistent");
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
  ...(status === "transport_error" ? { error: "dial failed" } : {}),
});
