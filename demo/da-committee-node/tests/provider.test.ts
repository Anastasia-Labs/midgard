import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  kupmiosChainPointResolver,
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

  it("accepts one local chain authority plus aligned query surfaces without treating them as independent providers", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({ header, headerHash, depth: 10 });
    let authorityNodes = [node];
    const provider = new MultiStateQueueProvider(
      [
        { fetchStateQueueNodes: async () => authorityNodes },
        {
          fetchStateQueueNodes: async () => [
            {
              ...node,
              chainPoint: {
                ...node.chainPoint,
                depth: 8,
                providerSource: "kupo",
              },
            },
          ],
        },
      ],
      {
        sourceMode: "local_node",
        identities: ["chain-sync:node-a", "query:kupo"],
      },
    );

    await expect(provider.fetchStateQueueNodes()).resolves.toMatchObject([
      {
        chainPoint: {
          depth: 8,
          providerSource: "chain-sync:node-a,query:kupo",
        },
      },
    ]);
    authorityNodes = [];
    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /local_node.*chain-sync:node-a.*query:kupo/u,
    );
  });

  it("rejects one external provider and incompatible provider chain points", async () => {
    const one = { fetchStateQueueNodes: async () => [] };
    expect(
      () =>
        new MultiStateQueueProvider([one], {
          sourceMode: "external_providers",
          identities: ["operator-a"],
        }),
    ).toThrow(/at least two/u);

    const { header, headerHash } = await makePayloadFixture();
    const canonical = makeObservedNode({ header, headerHash, depth: 10 });
    const forked = {
      ...canonical,
      chainPoint: { ...canonical.chainPoint, blockHash: "ef".repeat(32) },
    };
    const provider = new MultiStateQueueProvider(
      [
        { fetchStateQueueNodes: async () => [canonical] },
        { fetchStateQueueNodes: async () => [forked] },
      ],
      {
        sourceMode: "external_providers",
        identities: ["operator-a", "operator-b"],
      },
    );
    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /operator-a.*operator-b/u,
    );
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
