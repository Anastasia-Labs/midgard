import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  kupoChainPointResolver,
  MultiStateQueueProvider,
  providerFromUrl,
  stateQueueUtxosToObservedNodes,
} from "../src/l1/provider.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";
import {
  makeObservedNode,
  makePayloadFixture,
  tempDir,
  writeJson,
} from "./helpers.js";

describe("L1 provider adapters", () => {
  it("keeps fixture providers for deterministic integration tests", async () => {
    const dir = await tempDir();
    const path = await writeJson(dir, "state-queue.json", []);
    const provider = await providerFromUrl(`fixture:${path}`, {
      network: "Preview",
      stateQueueAddress: "addr_test1statequeue",
      stateQueuePolicyId: "11".repeat(28),
    });
    await expect(provider.fetchStateQueueNodes()).resolves.toEqual([]);
  });

  it("normalizes SDK StateQueueUTxOs into scanner observations", async () => {
    const { header } = await makePayloadFixture();
    const headerHash = hashBlockHeader(header);
    const datum: SDK.LinkedListNodeView = {
      key: { Key: { key: headerHash } },
      next: "Empty",
      data: Data.castTo(
        { header, da_attestation: SDK.NO_DA_ATTESTATION },
        SDK.StateQueueNode,
      ) as SDK.LinkedListNodeView["data"],
    };
    const stateQueueUtxo: SDK.StateQueueUTxO = {
      utxo: {
        txHash: "aa".repeat(32),
        outputIndex: 1,
        address: "addr_test1statequeue",
        assets: {
          lovelace: 5_000_000n,
          ["11".repeat(28) +
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
          headerHash]: 1n,
        },
        datum: SDK.encodeLinkedListNodeView(datum),
      },
      datum,
      assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    };
    const observed = await stateQueueUtxosToObservedNodes(
      [stateQueueUtxo],
      "test-provider",
      async () => ({ depth: 7, blockHash: "bb".repeat(32) }),
    );
    expect(observed).toHaveLength(1);
    expect(observed[0]).toMatchObject({
      outRef: `${"aa".repeat(32)}#1`,
      assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
      linkedListKey: headerHash,
      daAttestation: SDK.NO_DA_ATTESTATION,
      chainPoint: { providerSource: "test-provider", depth: 7 },
    });
  });

  it("requires multiple L1 providers to agree before returning state-queue nodes", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const first = {
      fetchStateQueueNodes: async () => [
        {
          ...makeObservedNode({ header, headerHash, depth: 10 }),
          chainPoint: {
            ...makeObservedNode({ header, headerHash, depth: 10 }).chainPoint,
            providerSource: "provider-a",
          },
        },
      ],
    };
    const secondNode = makeObservedNode({ header, headerHash, depth: 3 });
    const second = {
      fetchStateQueueNodes: async () => [
        {
          ...secondNode,
          chainPoint: {
            ...secondNode.chainPoint,
            providerSource: "provider-b",
          },
        },
      ],
    };
    const provider = new MultiStateQueueProvider([first, second]);

    await expect(provider.fetchStateQueueNodes()).resolves.toMatchObject([
      {
        outRef: "ab".repeat(32) + "#0",
        chainPoint: {
          depth: 3,
          providerSource: "provider-a,provider-b",
        },
      },
    ]);
  });

  it("resolves Kupmios chain depth from Kupo created_at metadata", async () => {
    const calls: string[] = [];
    const fetchFn = async (url: string | URL | Request) => {
      const href = String(url);
      calls.push(href);
      if (href.endsWith("/health")) {
        return new Response("kupo_most_recent_node_tip  126197688\n");
      }
      return Response.json([
        {
          transaction_id: "aa".repeat(32),
          output_index: 1,
          created_at: {
            slot_no: 126197476,
            header_hash: "bb".repeat(32),
          },
        },
      ]);
    };
    const resolve = kupoChainPointResolver(
      "http://127.0.0.1:1442/",
      "addr_test1statequeue",
      fetchFn as typeof fetch,
    );

    await expect(
      resolve({
        txHash: "aa".repeat(32),
        outputIndex: 1,
      } as never),
    ).resolves.toMatchObject({
      slot: 126197476,
      blockHash: "bb".repeat(32),
      depth: 212,
    });
    expect(calls).toEqual([
      "http://127.0.0.1:1442/matches/addr_test1statequeue?unspent",
      "http://127.0.0.1:1442/health",
    ]);
  });

  it("refreshes Kupmios chain depth across scans", async () => {
    let tip = 126197476;
    const fetchFn = async (url: string | URL | Request) => {
      const href = String(url);
      if (href.endsWith("/health")) {
        return new Response(`kupo_most_recent_node_tip  ${tip}\n`);
      }
      return Response.json([
        {
          transaction_id: "aa".repeat(32),
          output_index: 1,
          created_at: {
            slot_no: 126197476,
            header_hash: "bb".repeat(32),
          },
        },
      ]);
    };
    const resolve = kupoChainPointResolver(
      "http://127.0.0.1:1442/",
      "addr_test1statequeue",
      fetchFn as typeof fetch,
    );

    await expect(
      resolve({
        txHash: "aa".repeat(32),
        outputIndex: 1,
      } as never),
    ).resolves.toMatchObject({ depth: 0 });

    tip = 126197479;

    await expect(
      resolve({
        txHash: "aa".repeat(32),
        outputIndex: 1,
      } as never),
    ).resolves.toMatchObject({ depth: 3 });
  });

  it("fails closed on L1 provider state-queue disagreement", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const first = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({ header, headerHash, depth: 10 }),
      ],
    };
    const second = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({
          header,
          headerHash,
          assetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${"99".repeat(28)}`,
          depth: 10,
        }),
      ],
    };
    const provider = new MultiStateQueueProvider([first, second]);

    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /provider disagreement/,
    );
  });

  it("rejects unsupported provider URL schemes", async () => {
    await expect(
      providerFromUrl("http://plain-url.example", {
        network: "Preview",
        stateQueueAddress: "addr_test1statequeue",
        stateQueuePolicyId: "11".repeat(28),
      }),
    ).rejects.toThrow(/unsupported CARDANO_PROVIDER_URLS/);
  });
});
