import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  scanStateQueue,
  type StateQueueProvider,
} from "../src/l1/state-queue-scanner.js";
import { makeObservedNode, makePayloadFixture } from "./helpers.js";

describe("state queue scanner", () => {
  it("records a deployment-bound durable queue anchor and refuses unproven advancement", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({
      header,
      headerHash,
      outRef: `${"44".repeat(32)}#1`,
    });
    const snapshot = {
      nodes: [{ ...node, chainPoint: { ...node.chainPoint, blockHeight: 90 } }],
      confirmedHeaderHash: "55".repeat(28),
      confirmedStateOutRef: `${"66".repeat(32)}#0`,
      observedChainPoint: {
        ...node.chainPoint,
        blockHeight: 89,
        depth: 30,
      },
    };
    const provider: StateQueueProvider = {
      fetchStateQueueNodes: async () => snapshot.nodes,
      fetchStateQueueSnapshot: async () => snapshot,
    };
    let anchor:
      | Parameters<
          NonNullable<
            Parameters<typeof scanStateQueue>[1]["recordReplayAnchor"]
          >
        >[0]
      | undefined;
    const common = {
      deploymentFingerprint: "11".repeat(32),
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 30,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    } as const;
    await scanStateQueue(provider, {
      ...common,
      recordReplayAnchor: (next) => {
        anchor = next;
      },
    });
    expect(anchor).toMatchObject({
      deploymentIdentityDigest: common.deploymentIdentityDigest,
      stateQueuePolicyId: common.stateQueuePolicyId,
      blockNo: "90",
      transactionIndex: "0",
    });

    const changed = {
      ...snapshot,
      nodes: [
        {
          ...snapshot.nodes[0]!,
          outRef: `${"77".repeat(32)}#0`,
        },
      ],
    };
    await expect(
      scanStateQueue(
        {
          fetchStateQueueNodes: async () => changed.nodes,
          fetchStateQueueSnapshot: async () => changed,
        },
        { ...common, terminalReplayAnchor: anchor! },
      ),
    ).rejects.toThrow(/without an authenticated replay checkpoint/u);

    await expect(
      scanStateQueue(provider, {
        ...common,
        terminalReplayAnchor: {
          ...anchor!,
          deploymentIdentityDigest: "99".repeat(32),
        },
      }),
    ).rejects.toThrow(/durable replay anchor release mismatch/u);
  });

  it("finds finalized unattested headers and ignores the root node", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const provider: StateQueueProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({ header, headerHash, linkedListKey: "Empty" }),
        makeObservedNode({ header, headerHash, depth: 3 }),
      ],
    };
    const records = await scanStateQueue(provider, {
      deploymentFingerprint: "dep",
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 2,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    });
    expect(records).toHaveLength(1);
    expect(records[0]!.status).toBe("unattested");
    expect(records[0]!.finalized).toBe(true);
  });

  it("marks attested and conflicted state-queue nodes", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const provider: StateQueueProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({
          header,
          headerHash,
          daAttestation: { Attested: { da_bond_asset_name: "33".repeat(32) } },
        }),
        makeObservedNode({
          header,
          headerHash,
          assetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${"44".repeat(28)}`,
        }),
        makeObservedNode({
          header,
          headerHash,
          daAttestation: { Attested: { da_bond_asset_name: "55".repeat(32) } },
        }),
      ],
    };
    const records = await scanStateQueue(provider, {
      deploymentFingerprint: "dep",
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 0,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    });
    // The third node carries an attestation marker, which is now a
    // legitimate StateQueueStatusV1 state rather than a conflict
    // (`unexpected_da_attestation_marker` was removed with the ByteArray
    // marker). Replacement adversarial coverage for the new status sum is
    // tracked in https://github.com/Anastasia-Labs/midgard/issues/645.
    expect(records.map((record) => record.status)).toEqual([
      "attested",
      "conflicted",
      "attested",
    ]);
    expect(records[1]!.validationErrors).toContain(
      "block_asset_suffix_mismatch",
    );
    expect(records[2]!.validationErrors).toEqual([]);
  });

  it("never marks finalized unattested headers out of scope", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const provider: StateQueueProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({ header, headerHash, depth: 10 }),
      ],
    };

    const records = await scanStateQueue(provider, {
      deploymentFingerprint: "dep",
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 0,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    });

    expect(records).toHaveLength(1);
    expect(records[0]!.status).toBe("unattested");
    expect(records[0]!.validationErrors).toEqual([]);
  });
});
