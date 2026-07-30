import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  assertOgmiosNetworkMagic,
  kupmiosChainPointResolver,
  l1AuthorityProviderSource,
  lucidChainPointResolver,
  MultiStateQueueProvider,
  providerFromUrl,
  stateQueueUtxosToObservedNodes,
} from "../src/l1/provider.js";
import { hashBlockHeaderV1 } from "../src/l1/state-queue-scanner.js";
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
      cardanoL1Source: localNodeSource,
      stateQueueAddress: "addr_test1statequeue",
      stateQueuePolicyId: "11".repeat(28),
    });
    await expect(provider.fetchStateQueueNodes()).resolves.toEqual([]);
  });

  it("normalizes SDK StateQueueUTxOs into scanner observations", async () => {
    const { header } = await makePayloadFixture();
    const headerHash = hashBlockHeaderV1(header);
    const datum: SDK.LinkedListNodeView = {
      key: { Key: { key: headerHash } },
      next: "Empty",
      data: Data.castTo(
        { header, da_attestation: SDK.NO_DA_ATTESTATION },
        SDK.StateQueueNodeV1,
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

  it("resolves chain points from provider-neutral transaction status", async () => {
    const txHash = "aa".repeat(32);
    const statusQuery = vi.fn(async () => ({
      status: "confirmed" as const,
      txHash,
      confirmation: {
        txHash,
        slot: 126197476,
        blockHash: "bb".repeat(32),
        blockHeight: 3_000_000,
        confirmations: 7,
      },
    }));
    const resolve = lucidChainPointResolver({
      transactionStatus: statusQuery,
    } as unknown as LucidEvolution);

    await expect(
      resolve({
        txHash,
        outputIndex: 1,
      } as never),
    ).resolves.toMatchObject({
      slot: 126197476,
      blockHash: "bb".repeat(32),
      blockHeight: 3_000_000,
      depth: 6,
    });
    expect(statusQuery).toHaveBeenCalledWith(txHash);
  });

  it("does not fabricate block depth when the provider omits confirmations", async () => {
    const txHash = "aa".repeat(32);
    const resolve = lucidChainPointResolver({
      transactionStatus: async () => ({
        status: "confirmed",
        txHash,
        confirmation: {
          txHash,
          slot: 126197476,
          blockHash: "bb".repeat(32),
        },
      }),
    } as unknown as LucidEvolution);

    const point = await resolve({
      txHash,
      outputIndex: 1,
    } as never);
    expect(point).toMatchObject({
      slot: 126197476,
      blockHash: "bb".repeat(32),
    });
    expect(point).not.toHaveProperty("depth");
  });

  it("retains Kupo tip depth when Kupmios omits confirmations", async () => {
    const txHash = "aa".repeat(32);
    const statusQuery = vi.fn(async () => ({
      status: "confirmed" as const,
      txHash,
      confirmation: {
        txHash,
        slot: 126197476,
        blockHash: "bb".repeat(32),
      },
    }));
    const fetchFn = vi.fn(
      async () => new Response("kupo_most_recent_node_tip  126197688\n"),
    );
    const resolve = kupmiosChainPointResolver(
      { transactionStatus: statusQuery } as unknown as LucidEvolution,
      "http://127.0.0.1:1442/",
      fetchFn as typeof fetch,
    );

    await expect(
      resolve({ txHash, outputIndex: 1 } as never),
    ).resolves.toMatchObject({
      slot: 126197476,
      blockHash: "bb".repeat(32),
      depth: 212,
    });
    expect(fetchFn).toHaveBeenCalledWith("http://127.0.0.1:1442/health");
  });

  it("checks Custom network magic against the Kupmios Ogmios authority", async () => {
    const fetchFn = vi.fn(
      async (..._args: Parameters<typeof fetch>): Promise<Response> =>
        Response.json({
          jsonrpc: "2.0",
          id: "midgard-network-magic-preflight",
          result: { networkMagic: 424242 },
        }),
    );
    await expect(
      assertOgmiosNetworkMagic(
        "ws://127.0.0.1:1337",
        424242,
        fetchFn as typeof fetch,
      ),
    ).resolves.toBeUndefined();
    expect(fetchFn).toHaveBeenCalledOnce();
    expect(fetchFn.mock.calls[0]?.[0]).toBe("http://127.0.0.1:1337/");
    expect(fetchFn.mock.calls[0]?.[1]).toMatchObject({
      method: "POST",
      headers: { "content-type": "application/json" },
    });
    expect(JSON.parse(String(fetchFn.mock.calls[0]?.[1]?.body))).toMatchObject({
      method: "queryNetwork/genesisConfiguration",
      params: { era: "shelley" },
    });
  });

  it("fails closed when Ogmios network magic is missing or mismatched", async () => {
    await expect(
      assertOgmiosNetworkMagic("http://127.0.0.1:1337", 424242, async () =>
        Response.json({
          jsonrpc: "2.0",
          id: "midgard-network-magic-preflight",
          result: { networkMagic: 42 },
        }),
      ),
    ).rejects.toThrow(/does not match configured Cardano network authority/);
    await expect(
      assertOgmiosNetworkMagic("http://127.0.0.1:1337", 424242, async () =>
        Response.json({
          jsonrpc: "2.0",
          id: "midgard-network-magic-preflight",
          result: {},
        }),
      ),
    ).rejects.toThrow(/missing an unsigned network magic/);
  });

  it("binds persisted provider provenance to the selected L1 authority", () => {
    expect(
      l1AuthorityProviderSource(
        { cardanoL1Source: localNodeSource },
        0,
        "kupmios:http://kupo|http://ogmios",
      ),
    ).toBe(
      `local_node:phase4-cardano-node:${"aa".repeat(32)}:${sha256("kupmios:http://kupo|http://ogmios")}`,
    );
    expect(
      l1AuthorityProviderSource(
        {
          cardanoL1Source: {
            sourceMode: "external_providers",
            providerAuthorityIds: ["11".repeat(32), "22".repeat(32)],
            authorityDigest: "bb".repeat(32),
            networkMagic: 2,
          },
        },
        1,
        "blockfrost:https://provider.example",
      ),
    ).toBe(
      `external_providers:${"22".repeat(32)}:${"bb".repeat(32)}:${sha256("blockfrost:https://provider.example")}`,
    );
  });

  it("fails closed when transaction status is not confirmed", async () => {
    const txHash = "aa".repeat(32);
    const resolve = lucidChainPointResolver({
      transactionStatus: async () => ({ status: "not_found", txHash }),
    } as unknown as LucidEvolution);

    await expect(
      resolve({
        txHash,
        outputIndex: 1,
      } as never),
    ).rejects.toThrow(/is not confirmed: not_found/);
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

  it("fails closed on provider chain-point disagreement", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({ header, headerHash, depth: 10 });
    const provider = new MultiStateQueueProvider([
      { fetchStateQueueNodes: async () => [node] },
      {
        fetchStateQueueNodes: async () => [
          {
            ...node,
            chainPoint: {
              ...node.chainPoint,
              blockHash: "ef".repeat(32),
              providerSource: "other-authority",
            },
          },
        ],
      },
    ]);

    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /chain-point disagreement/,
    );
  });

  it("rejects unsupported provider URL schemes", async () => {
    await expect(
      providerFromUrl("http://plain-url.example", {
        network: "Preview",
        cardanoL1Source: localNodeSource,
        stateQueueAddress: "addr_test1statequeue",
        stateQueuePolicyId: "11".repeat(28),
      }),
    ).rejects.toThrow(/unsupported CARDANO_PROVIDER_URLS/);
  });
});

const localNodeSource = {
  sourceMode: "local_node",
  authorityNodeId: "phase4-cardano-node",
  authorityDigest: "aa".repeat(32),
  networkMagic: 424242,
} as const;

const sha256 = (value: string): string =>
  createHash("sha256").update(value).digest("hex");
