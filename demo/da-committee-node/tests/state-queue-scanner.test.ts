import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  scanStateQueue,
  type StateQueueProvider,
} from "../src/l1/state-queue-scanner.js";
import { makeObservedNode, makePayloadFixture } from "./helpers.js";

describe("state queue scanner", () => {
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
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 2,
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
          daAttestation: "33".repeat(28),
        }),
        makeObservedNode({
          header,
          headerHash,
          assetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${"44".repeat(28)}`,
        }),
        makeObservedNode({
          header,
          headerHash,
          daAttestation: "55".repeat(28),
        }),
      ],
    };
    const records = await scanStateQueue(provider, {
      deploymentFingerprint: "dep",
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 0,
    });
    expect(records.map((record) => record.status)).toEqual([
      "attested",
      "conflicted",
      "conflicted",
    ]);
    expect(records[1]!.validationErrors).toContain(
      "block_asset_suffix_mismatch",
    );
    expect(records[2]!.validationErrors).toContain(
      "unexpected_da_attestation_marker",
    );
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
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 0,
    });

    expect(records).toHaveLength(1);
    expect(records[0]!.status).toBe("unattested");
    expect(records[0]!.validationErrors).toEqual([]);
  });
});
