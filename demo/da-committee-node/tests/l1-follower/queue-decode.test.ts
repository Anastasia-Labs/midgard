import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  type QueueRow,
  walkLandedQueue,
} from "../../src/l1/follower/landed-queue.js";
import { decodeQueueOutput } from "../../src/l1/follower/queue-derivation.js";
import { makePayloadFixture } from "../helpers.js";
import { nodeDatum, rootDatum } from "./queue-sim-traffic.js";

const POLICY = "71".repeat(28);
const OTHER_HASH = "ee".repeat(28);
const MREG = Buffer.from("MREG").toString("hex");

const output = (assetName: string, datum: Buffer) => ({
  address: Buffer.alloc(29),
  paymentCredential: null,
  stakeCredential: null,
  lovelace: 5_000_000n,
  assets: new Map([[POLICY, new Map([[assetName, 1n]])]]),
  datumHash: null,
  datum,
  scriptRef: null,
});

const rowOf = (outRef: string, assetName: string, datum: Buffer): QueueRow => ({
  outRef,
  ...decodeQueueOutput(output(assetName, datum), POLICY)!,
  datumHex: datum.toString("hex"),
  createdSlot: 1,
  createdHeight: 1,
  createdTxIndex: 0,
  spentSlot: null,
});

/**
 * A node output whose NFT does not name its header: the decoder records
 * each mismatch as a problem, and the walk keeps the node, conflicted,
 * where its key links it.
 */
describe("a state-queue node output whose NFT does not name its header", () => {
  it.each([
    [
      "the block prefix with another header's hash",
      `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${OTHER_HASH}`,
      OTHER_HASH,
      ["linked_list_key_mismatch", "block_asset_suffix_mismatch"],
    ],
    [
      "another linked list's prefix with its own hash",
      `${MREG}{HEADER}`,
      "{HEADER}",
      ["block_asset_prefix_mismatch"],
    ],
    [
      "its own hash with no prefix",
      "{HEADER}",
      "{HEADER}",
      ["block_asset_prefix_mismatch"],
    ],
    [
      "the block prefix with its own hash",
      `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}{HEADER}`,
      "{HEADER}",
      [],
    ],
  ] as const)(
    "decodes %s with exactly its problems, conflicted in a healthy walk when it has any",
    async (_name, assetTemplate, keyTemplate, problems) => {
      const { header, headerHash } = await makePayloadFixture(2);
      const assetName = assetTemplate.replace("{HEADER}", headerHash);
      const key = keyTemplate.replace("{HEADER}", headerHash);
      const node = rowOf(
        "a#0",
        assetName,
        nodeDatum(header, "Unattested", null),
      );
      expect(node).toMatchObject({
        kind: "node",
        assetName,
        nodeKey: key,
        headerHash,
        problems,
      });
      const queue = walkLandedQueue([
        rowOf(
          "r#0",
          SDK.STATE_QUEUE_ROOT_ASSET_NAME,
          rootDatum("00".repeat(28), 0, key),
        ),
        node,
      ]);
      expect(queue).toMatchObject({ healthy: true, strays: [] });
      expect(queue.nodes).toMatchObject([
        {
          outRef: "a#0",
          headerHash,
          problems,
          status: problems.length === 0 ? "unattested" : "conflicted",
        },
      ]);
    },
  );
});
