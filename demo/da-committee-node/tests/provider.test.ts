import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  kupoIndexedSlotFromHealth,
  kupmiosChainPointResolver,
  LocalNodeStateQueueProvider,
  lucidChainPointResolver,
  MultiStateQueueProvider,
  OgmiosChainSyncAuthority,
  ogmiosWebSocketEndpoint,
  providerFromUrl,
  stateQueueUtxosToObservedNodes,
  type OgmiosPoint,
  type TipAwareStateQueueProvider,
} from "../src/l1/provider.js";
import { hashBlockHeaderV1 } from "../src/l1/state-queue-scanner.js";
import {
  makeObservedNode,
  makePayloadFixture,
  tempDir,
  writeJson,
} from "./helpers.js";

describe("L1 provider adapters", () => {
  it("parses Kupo's JSON health contract and rejects stale authority states", () => {
    expect(
      kupoIndexedSlotFromHealth({
        connection_status: "connected",
        most_recent_checkpoint: 123,
        most_recent_node_tip: 123,
      }),
    ).toBe(123);
    expect(() =>
      kupoIndexedSlotFromHealth({
        connection_status: "disconnected",
        most_recent_checkpoint: 123,
        most_recent_node_tip: 123,
      }),
    ).toThrow(/not connected/u);
    expect(() =>
      kupoIndexedSlotFromHealth({
        connection_status: "connected",
        most_recent_checkpoint: null,
        most_recent_node_tip: 123,
      }),
    ).toThrow(/valid indexed checkpoint/u);
    expect(ogmiosWebSocketEndpoint("http://ogmios.local:1337")).toBe(
      "ws://ogmios.local:1337/",
    );
    expect(ogmiosWebSocketEndpoint("https://ogmios.example/rpc")).toBe(
      "wss://ogmios.example/rpc",
    );
  });

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
    const provider = new MultiStateQueueProvider([first, second], {
      sourceMode: "external_providers",
    });

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

  it("fails closed when a local query tip is stale or a rollback crosses the read", async () => {
    const point: OgmiosPoint = {
      slot: 100,
      id: "aa".repeat(32),
      height: 50,
    };
    const stalePoint: OgmiosPoint = {
      slot: 99,
      id: "bb".repeat(32),
      height: 49,
    };
    const query = (tip: OgmiosPoint): TipAwareStateQueueProvider => ({
      fetchStateQueueNodes: async () => [],
      fetchStateQueueObservation: async () => ({ nodes: [], tip }),
    });
    const stableAuthority = {
      synchronize: async () => ({ tip: point, rollbackSequence: 0 }),
    };
    const stale = new LocalNodeStateQueueProvider({
      authority: stableAuthority,
      authorityIdentity: "node-a",
      queryProviders: [query(stalePoint)],
      identities: ["query:node-a:0"],
    });
    await expect(stale.fetchStateQueueNodes()).rejects.toThrow(
      /stale or not aligned/u,
    );

    let calls = 0;
    const rolling = new LocalNodeStateQueueProvider({
      authority: {
        synchronize: async () => ({
          tip: point,
          rollbackSequence: calls++,
        }),
      },
      authorityIdentity: "node-a",
      queryProviders: [query(point)],
      identities: ["query:node-a:0"],
    });
    await expect(rolling.fetchStateQueueNodes()).rejects.toThrow(
      /rollback occurred/u,
    );
  });

  it("consumes Ogmios roll-backward events and advances onto the new tip", async () => {
    const firstTip = {
      slot: 100,
      id: "aa".repeat(32),
      height: 50,
    };
    const replacementTip = {
      slot: 101,
      id: "cc".repeat(32),
      height: 50,
    };
    const nextTip = {
      slot: 102,
      id: "dd".repeat(32),
      height: 51,
    };
    const nextTipWire = {
      slot: nextTip.slot,
      id: nextTip.id,
      blockNo: nextTip.height,
    };
    const originalWebSocket = (globalThis as unknown as { WebSocket?: unknown })
      .WebSocket;
    let session = 0;
    const methods: string[] = [];
    class FakeWebSocket {
      private readonly listeners = new Map<
        string,
        Set<(event: { data: unknown }) => void>
      >();
      private readonly session: number;
      private nextBlockCount = 0;

      constructor(_url: string) {
        session += 1;
        this.session = session;
        queueMicrotask(() => this.emit("open", { data: undefined }));
      }

      addEventListener(
        type: string,
        listener: (event: { data: unknown }) => void,
      ): void {
        const listeners = this.listeners.get(type) ?? new Set();
        listeners.add(listener);
        this.listeners.set(type, listeners);
      }

      removeEventListener(
        type: string,
        listener: (event: { data: unknown }) => void,
      ): void {
        this.listeners.get(type)?.delete(listener);
      }

      send(raw: string): void {
        const request = JSON.parse(raw) as {
          readonly method: string;
          readonly id: { readonly requestId: string };
        };
        methods.push(request.method);
        const result =
          request.method === "findIntersection"
            ? this.session === 1
              ? { intersection: "origin", tip: firstTip }
              : this.session === 2
                ? { intersection: firstTip, tip: replacementTip }
                : { intersection: replacementTip, tip: nextTipWire }
            : this.session === 2 && this.nextBlockCount++ === 0
              ? {
                  direction: "backward",
                  point: "origin",
                  tip: replacementTip,
                }
              : this.session === 3 && this.nextBlockCount++ === 0
                ? {
                    direction: "backward",
                    point: replacementTip,
                    tip: nextTipWire,
                  }
                : {
                    direction: "forward",
                    block: this.session === 2 ? replacementTip : nextTipWire,
                    tip: this.session === 2 ? replacementTip : nextTipWire,
                  };
        queueMicrotask(() =>
          this.emit("message", {
            data: JSON.stringify({ id: request.id, result }),
          }),
        );
      }

      close(): void {}

      private emit(type: string, event: { data: unknown }): void {
        for (const listener of this.listeners.get(type) ?? []) {
          listener(event);
        }
      }
    }
    (globalThis as unknown as { WebSocket?: unknown }).WebSocket =
      FakeWebSocket;
    try {
      const authority = new OgmiosChainSyncAuthority("ws://127.0.0.1:1337");
      await expect(authority.synchronize()).resolves.toMatchObject({
        tip: firstTip,
        rollbackSequence: 0,
      });
      await expect(authority.synchronize()).resolves.toMatchObject({
        tip: replacementTip,
        rollbackSequence: 1,
      });
      await expect(authority.synchronize()).resolves.toMatchObject({
        tip: nextTip,
        rollbackSequence: 1,
      });
      expect(methods).toEqual([
        "findIntersection",
        "findIntersection",
        "nextBlock",
        "nextBlock",
        "findIntersection",
        "nextBlock",
        "nextBlock",
      ]);
    } finally {
      (globalThis as unknown as { WebSocket?: unknown }).WebSocket =
        originalWebSocket;
    }
  });

  it("rejects an Ogmios intersection that was not submitted from bounded history", async () => {
    const firstTip: OgmiosPoint = {
      slot: 100,
      id: "aa".repeat(32),
      height: 50,
    };
    const inventedIntersection: OgmiosPoint = {
      slot: 99,
      id: "bb".repeat(32),
      height: 49,
    };
    const nextTip: OgmiosPoint = {
      slot: 101,
      id: "cc".repeat(32),
      height: 51,
    };
    const originalWebSocket = (globalThis as unknown as { WebSocket?: unknown })
      .WebSocket;
    let session = 0;
    const methods: string[] = [];
    class FakeWebSocket {
      private readonly listeners = new Map<
        string,
        Set<(event: { data: unknown }) => void>
      >();
      private readonly session: number;

      constructor(_url: string) {
        session += 1;
        this.session = session;
        queueMicrotask(() => this.emit("open", { data: undefined }));
      }

      addEventListener(
        type: string,
        listener: (event: { data: unknown }) => void,
      ): void {
        const listeners = this.listeners.get(type) ?? new Set();
        listeners.add(listener);
        this.listeners.set(type, listeners);
      }

      removeEventListener(
        type: string,
        listener: (event: { data: unknown }) => void,
      ): void {
        this.listeners.get(type)?.delete(listener);
      }

      send(raw: string): void {
        const request = JSON.parse(raw) as {
          readonly method: string;
          readonly id: { readonly requestId: string };
        };
        methods.push(request.method);
        const result =
          this.session === 1
            ? { intersection: "origin", tip: firstTip }
            : { intersection: inventedIntersection, tip: nextTip };
        queueMicrotask(() =>
          this.emit("message", {
            data: JSON.stringify({ id: request.id, result }),
          }),
        );
      }

      close(): void {}

      private emit(type: string, event: { data: unknown }): void {
        for (const listener of this.listeners.get(type) ?? []) {
          listener(event);
        }
      }
    }
    (globalThis as unknown as { WebSocket?: unknown }).WebSocket =
      FakeWebSocket;
    try {
      const authority = new OgmiosChainSyncAuthority("ws://127.0.0.1:1337");
      await expect(authority.synchronize()).resolves.toMatchObject({
        tip: firstTip,
        rollbackSequence: 0,
      });
      await expect(authority.synchronize()).rejects.toThrow(
        /not one of the submitted bounded-history candidates/u,
      );
      expect(methods).toEqual(["findIntersection", "findIntersection"]);
    } finally {
      (globalThis as unknown as { WebSocket?: unknown }).WebSocket =
        originalWebSocket;
    }
  });

  it("rejects one external provider and incompatible provider chain points", async () => {
    const one = { fetchStateQueueNodes: async () => [] };
    expect(() => new MultiStateQueueProvider([one, one], {} as never)).toThrow(
      /sourceMode must be local_node or external_providers/u,
    );
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

  it("uses descendant block heights when Kupmios omits confirmations", async () => {
    const txHash = "aa".repeat(32);
    const statusQuery = vi.fn(async () => ({
      status: "confirmed" as const,
      txHash,
      confirmation: {
        txHash,
        slot: 126197476,
        blockHash: "bb".repeat(32),
        blockHeight: 3_000_000,
      },
    }));
    const resolve = kupmiosChainPointResolver({
      transactionStatus: statusQuery,
    } as unknown as LucidEvolution);

    await expect(
      resolve({ txHash, outputIndex: 1 } as never, {
        slot: 126197688,
        id: "cc".repeat(32),
        height: 3_000_012,
      }),
    ).resolves.toMatchObject({
      slot: 126197476,
      blockHash: "bb".repeat(32),
      blockHeight: 3_000_000,
      depth: 12,
    });
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
    const provider = new MultiStateQueueProvider([first, second], {
      sourceMode: "external_providers",
    });

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
