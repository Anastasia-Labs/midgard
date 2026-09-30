import "./store-retention.retention-deadline-report-v1.js";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { LIBP2P_DA_MIN_RETENTION_DAYS } from "../src/config.js";
import { assertLibp2pDaRetentionDays } from "../src/config.js";
import {
  hashBlockHeader,
  scanStateQueue,
} from "../src/l1/state-queue-scanner.js";
import { makeObservedNode, makePayloadFixture } from "./helpers.js";
import { FINGERPRINT } from "./store-retention.header-record.js";

describe("state-queue scanner L1 view", () => {
  it("hands the confirmed head and every live queue header to the poller", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({ header, headerHash });
    const views: unknown[] = [];
    await scanStateQueue(
      {
        fetchStateQueueNodes: async () => [node],
        fetchStateQueueSnapshot: async () => ({
          nodes: [node],
          confirmedHeaderHash: "AB".repeat(28),
          confirmedStateOutRef: `${"66".repeat(32)}#0`,
          observedChainPoint: { ...node.chainPoint, depth: 30 },
        }),
      },
      {
        deploymentFingerprint: FINGERPRINT,
        deploymentIdentityDigest: FINGERPRINT,
        stateQueuePolicyId: "22".repeat(28),
        daAttestationPolicyId: "33".repeat(28),
        finalityDepth: 30,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        recordL1View: (view) => views.push(view),
      },
    );
    expect(views).toEqual([
      {
        confirmedHeaderHash: "ab".repeat(28),
        liveQueueHeaderHashes: [hashBlockHeader(header)],
      },
    ]);
  });

  it("keeps a conflicted queue node in the live set", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({
      header,
      headerHash,
      assetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${"44".repeat(28)}`,
    });
    const views: unknown[] = [];
    const records = await scanStateQueue(
      {
        fetchStateQueueNodes: async () => [node],
        fetchStateQueueSnapshot: async () => ({
          nodes: [node],
          confirmedHeaderHash: "AB".repeat(28),
          confirmedStateOutRef: `${"66".repeat(32)}#0`,
          observedChainPoint: { ...node.chainPoint, depth: 30 },
        }),
      },
      {
        deploymentFingerprint: FINGERPRINT,
        deploymentIdentityDigest: FINGERPRINT,
        stateQueuePolicyId: "22".repeat(28),
        daAttestationPolicyId: "33".repeat(28),
        finalityDepth: 30,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        recordL1View: (view) => views.push(view),
      },
    );
    expect(records.map(({ status }) => status)).toEqual(["conflicted"]);
    expect(views).toEqual([
      {
        confirmedHeaderHash: "ab".repeat(28),
        liveQueueHeaderHashes: [hashBlockHeader(header)],
      },
    ]);
  });

  it("reports no view when the provider cannot supply a full snapshot", async () => {
    const views: unknown[] = [];
    await scanStateQueue(
      { fetchStateQueueNodes: async () => [] },
      {
        deploymentFingerprint: FINGERPRINT,
        deploymentIdentityDigest: FINGERPRINT,
        stateQueuePolicyId: "22".repeat(28),
        daAttestationPolicyId: "33".repeat(28),
        finalityDepth: 30,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        recordL1View: (view) => views.push(view),
      },
    );
    expect(views).toEqual([]);
  });
});

describe("assertLibp2pDaRetentionDaysV1", () => {
  it("accepts the canonical 15-day window matching the manifest", () => {
    expect(
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
        manifestRetentionDays: LIBP2P_DA_MIN_RETENTION_DAYS,
      }),
    ).toBe(15);
    expect(LIBP2P_DA_MIN_RETENTION_DAYS).toBe(
      MIDGARD_RETENTION_WINDOW.retentionDays,
    );
  });

  it("rejects 14 days and accepts 15 at the boundary", () => {
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 14,
        manifestRetentionDays: 14,
      }),
    ).toThrow(/must be at least 15 days/u);
    expect(
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 15,
        manifestRetentionDays: 15,
      }),
    ).toBe(15);
  });

  it("rejects a runtime window that differs from the manifest window", () => {
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays: 16,
        manifestRetentionDays: 15,
      }),
    ).toThrow(/must exactly equal the verified deployment manifest/u);
  });

  it("rejects malformed runtime retention days", () => {
    for (const bad of [Number.NaN, -1, 1.5, 2 ** 53]) {
      expect(() =>
        assertLibp2pDaRetentionDays({
          runtimeRetentionDays: bad,
          manifestRetentionDays: 15,
        }),
      ).toThrow(/da_transport\.retention_days/u);
    }
  });
});
