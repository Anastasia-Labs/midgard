import { appendFile, writeFile } from "node:fs/promises";

import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  type CanonicalChainPoint,
  type ChainSyncEventBatch,
  fetchKupoCheckpoint,
  FileChainSyncConsumerCursorStore,
  FileChainSyncCursorStore,
  kupmiosChainPointResolver,
  LocalNodeChainAuthority,
  LocalNodeStateQueueProvider,
  lucidChainPointResolver,
  MultiStateQueueProvider,
  OgmiosChainSyncEventSource,
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
      currentChainPoint: async () => externalPoint("provider-a"),
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
      currentChainPoint: async () => externalPoint("provider-b"),
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

  it("durably replays roll-forward and rollback chain-sync events", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const point = (slot: number, byte: string): CanonicalChainPoint => ({
      network: "Preview",
      slot,
      blockHash: byte.repeat(64),
      providerSource: "chain-sync:node-a",
      observedAt: "2026-07-28T00:00:00.000Z",
    });
    const point1 = point(1, "a");
    const point2 = point(2, "b");
    const initialBatches: readonly ChainSyncEventBatch[] = [
      {
        event: { direction: "roll_forward", point: point1 },
        tip: point2,
      },
      {
        event: { direction: "roll_forward", point: point2 },
        tip: point2,
      },
    ];
    const initial = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      {
        next: async (cursor) => initialBatches[(cursor?.sequence ?? -1) + 1]!,
      },
      new FileChainSyncCursorStore(cursorPath, "11".repeat(32)),
    );
    await expect(initial.synchronizeToTip()).resolves.toEqual(point2);
    await expect(initial.replay(-1)).resolves.toMatchObject([
      { direction: "roll_forward", point: { slot: 1 } },
      { direction: "roll_forward", point: { slot: 2 } },
    ]);

    const point3 = point(3, "c");
    let resumedCalls = 0;
    const resumed = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      {
        next: async () => {
          const batch: ChainSyncEventBatch =
            resumedCalls === 0
              ? {
                  event: { direction: "roll_backward", point: point1 },
                  tip: point3,
                }
              : {
                  event: { direction: "roll_forward", point: point3 },
                  tip: point3,
                };
          resumedCalls += 1;
          return batch;
        },
      },
      new FileChainSyncCursorStore(cursorPath, "11".repeat(32)),
    );
    await expect(resumed.synchronizeToTip()).resolves.toEqual(point3);
    await expect(resumed.currentCursor()).resolves.toMatchObject({
      sequence: 3,
      rollbackGeneration: 1,
      point: { slot: 3 },
    });
    await expect(resumed.replay(1)).resolves.toMatchObject([
      { direction: "roll_backward", point: { slot: 1 } },
      { direction: "roll_forward", point: { slot: 3 } },
    ]);
  });

  it("recovers a valid journal append that reached disk before its cursor metadata", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const firstPoint = externalPoint("chain-sync:node-a", 1, "aa");
    const secondPoint = externalPoint("chain-sync:node-a", 2, "bb");
    const store = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    await store.append(
      { direction: "roll_forward", point: firstPoint },
      { sequence: 0, point: firstPoint, rollbackGeneration: 0 },
    );

    const recoveredCursor = {
      sequence: 1,
      point: secondPoint,
      rollbackGeneration: 0,
    };
    await appendFile(
      `${cursorPath}.events.jsonl`,
      `${JSON.stringify({
        sequence: 1,
        event: { direction: "roll_forward", point: secondPoint },
        cursor: recoveredCursor,
      })}\n`,
    );

    const restarted = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    await expect(restarted.load()).resolves.toEqual(recoveredCursor);
    await expect(restarted.replay(0)).resolves.toEqual([
      { direction: "roll_forward", point: secondPoint },
    ]);
  });

  it("fails closed when cursor metadata is ahead of a lost journal tail", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const point = externalPoint("chain-sync:node-a", 1, "aa");
    const store = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    await store.append(
      { direction: "roll_forward", point },
      { sequence: 0, point, rollbackGeneration: 0 },
    );
    await writeFile(`${cursorPath}.events.jsonl`, "");

    await expect(
      new FileChainSyncCursorStore(cursorPath, "11".repeat(32)).load(),
    ).rejects.toThrow(/cursor does not match its durable event journal/u);
  });

  it("persists an authority-bound monotonic rollback consumer cursor", async () => {
    const dir = await tempDir();
    const path = `${dir}/chain-sync-consumer.json`;
    const fingerprint = "11".repeat(32);
    const firstPoint = externalPoint("chain-sync:node-a", 10, "aa");
    const secondPoint = externalPoint("chain-sync:node-a", 12, "bb");
    const first = {
      sequence: 4,
      point: firstPoint,
      rollbackGeneration: 0,
    };
    const second = {
      sequence: 6,
      point: secondPoint,
      rollbackGeneration: 1,
    };
    const store = new FileChainSyncConsumerCursorStore(path, fingerprint);

    await store.save(first);
    await expect(
      new FileChainSyncConsumerCursorStore(path, fingerprint).load(),
    ).resolves.toEqual(first);
    await store.save(second);
    await expect(store.save(first)).rejects.toThrow(/cannot move backwards/u);
    await expect(
      new FileChainSyncConsumerCursorStore(path, "22".repeat(32)).load(),
    ).rejects.toThrow(/authority fingerprint/u);
  });

  it("bootstraps a mature Ogmios chain at a node-derived checkpoint without replaying from origin", async () => {
    const dir = await tempDir();
    const matureTip = { slot: 1_000_000, id: "bb".repeat(32) };
    let socketCount = 0;
    let nextBlockCount = 0;
    class FakeWebSocket {
      onopen: ((event: unknown) => void) | null = null;
      onmessage: ((event: { readonly data: unknown }) => void) | null = null;
      onerror: ((event: unknown) => void) | null = null;
      onclose: ((event: unknown) => void) | null = null;

      constructor(_url: string) {
        socketCount += 1;
        queueMicrotask(() => this.onopen?.({}));
      }

      send(raw: string): void {
        const request = JSON.parse(raw) as {
          readonly id: string;
          readonly method: string;
        };
        let result: unknown;
        if (request.method === "queryNetwork/genesisConfiguration") {
          result = { networkMagic: 2 };
        } else if (request.method === "queryNetwork/tip") {
          result = matureTip;
        } else if (request.method === "findIntersection") {
          result = { intersection: matureTip, tip: matureTip };
        } else {
          nextBlockCount += 1;
          result = {
            direction: "backward",
            point: matureTip,
            tip: matureTip,
          };
        }
        queueMicrotask(() =>
          this.onmessage?.({
            data: JSON.stringify({
              jsonrpc: "2.0",
              id: request.id,
              result,
            }),
          }),
        );
      }

      close(): void {}
    }
    vi.stubGlobal("WebSocket", FakeWebSocket);
    try {
      const source = new OgmiosChainSyncEventSource(
        "ws://ogmios.local",
        "Preview",
        "node-a",
      );
      const authority = new LocalNodeChainAuthority(
        "node-a",
        "Preview",
        source,
        new FileChainSyncCursorStore(
          `${dir}/chain-sync-cursor.json`,
          "11".repeat(32),
        ),
      );
      await expect(authority.synchronizeToTip(1)).resolves.toMatchObject({
        slot: 1_000_000,
        blockHash: "bb".repeat(32),
      });
      await expect(authority.currentCursor()).resolves.toMatchObject({
        sequence: 0,
        point: { slot: 1_000_000 },
      });
      await expect(authority.synchronizeToTip(1)).resolves.toMatchObject({
        slot: 1_000_000,
        blockHash: "bb".repeat(32),
      });
      expect(socketCount).toBe(1);
      expect(nextBlockCount).toBe(1);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("retries a fresh mature tip that rolls back before intersection without replaying origin", async () => {
    const dir = await tempDir();
    const matureTip = { slot: 1_000_000, id: "bb".repeat(32) };
    let socketCount = 0;
    let nextBlockCount = 0;
    class BootstrapRaceWebSocket {
      readonly socketIndex = socketCount++;
      onopen: ((event: unknown) => void) | null = null;
      onmessage: ((event: { readonly data: unknown }) => void) | null = null;
      onerror: ((event: unknown) => void) | null = null;
      onclose: ((event: unknown) => void) | null = null;

      constructor(_url: string) {
        queueMicrotask(() => this.onopen?.({}));
      }

      send(raw: string): void {
        const request = JSON.parse(raw) as {
          readonly id: string;
          readonly method: string;
        };
        let result: unknown;
        if (request.method === "queryNetwork/genesisConfiguration") {
          result = { networkMagic: 2 };
        } else if (request.method === "queryNetwork/tip") {
          result = matureTip;
        } else if (request.method === "findIntersection") {
          result = {
            intersection: this.socketIndex === 0 ? "origin" : matureTip,
            tip: matureTip,
          };
        } else {
          nextBlockCount += 1;
          result = {
            direction: "forward",
            block: { slot: 1, id: "aa".repeat(32) },
            tip: matureTip,
          };
        }
        queueMicrotask(() =>
          this.onmessage?.({
            data: JSON.stringify({
              jsonrpc: "2.0",
              id: request.id,
              result,
            }),
          }),
        );
      }

      close(): void {}
    }
    vi.stubGlobal("WebSocket", BootstrapRaceWebSocket);
    try {
      const authority = new LocalNodeChainAuthority(
        "node-a",
        "Preview",
        new OgmiosChainSyncEventSource(
          "ws://ogmios.local",
          "Preview",
          "node-a",
        ),
        new FileChainSyncCursorStore(
          `${dir}/chain-sync-cursor.json`,
          "11".repeat(32),
        ),
      );

      await expect(authority.synchronizeToTip(1)).resolves.toMatchObject({
        slot: matureTip.slot,
        blockHash: matureTip.id,
      });
      expect(socketCount).toBe(2);
      expect(nextBlockCount).toBe(0);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("reconnects Ogmios from the durable cursor without emitting its handshake rollback twice", async () => {
    const block1 = { slot: 1, id: "aa".repeat(32) };
    const block2 = { slot: 2, id: "bb".repeat(32) };
    let socketCount = 0;
    let recoveredNextCount = 0;
    class ReconnectingWebSocket {
      readonly socketIndex = socketCount++;
      onopen: ((event: unknown) => void) | null = null;
      onmessage: ((event: { readonly data: unknown }) => void) | null = null;
      onerror: ((event: unknown) => void) | null = null;
      onclose: ((event: unknown) => void) | null = null;

      constructor(_url: string) {
        queueMicrotask(() => this.onopen?.({}));
      }

      send(raw: string): void {
        const request = JSON.parse(raw) as {
          readonly id: string;
          readonly method: string;
        };
        if (this.socketIndex === 0 && request.method === "nextBlock") {
          queueMicrotask(() => this.onerror?.({}));
          return;
        }
        const result =
          request.method === "queryNetwork/genesisConfiguration"
            ? { networkMagic: 2 }
            : request.method === "findIntersection"
              ? { intersection: block1, tip: block2 }
              : recoveredNextCount++ === 0
                ? { direction: "backward", point: block1, tip: block2 }
                : { direction: "forward", block: block2, tip: block2 };
        queueMicrotask(() =>
          this.onmessage?.({
            data: JSON.stringify({
              jsonrpc: "2.0",
              id: request.id,
              result,
            }),
          }),
        );
      }

      close(): void {}
    }
    vi.stubGlobal("WebSocket", ReconnectingWebSocket);
    try {
      const point1 = externalPoint("chain-sync:node-a", 1, "aa");
      const source = new OgmiosChainSyncEventSource(
        "ws://ogmios.local",
        "Preview",
        "node-a",
      );
      await expect(
        source.next({
          sequence: 0,
          point: point1,
          rollbackGeneration: 0,
        }),
      ).resolves.toMatchObject({
        event: {
          direction: "roll_forward",
          point: { slot: 2, blockHash: "bb".repeat(32) },
        },
      });
      expect(socketCount).toBe(2);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("negotiates a missing durable tip from bounded journal points newest to oldest", async () => {
    const block1 = { slot: 1, id: "aa".repeat(32) };
    const block4 = { slot: 4, id: "dd".repeat(32) };
    let intersectionPoints: unknown;
    class CommonAncestorWebSocket {
      onopen: ((event: unknown) => void) | null = null;
      onmessage: ((event: { readonly data: unknown }) => void) | null = null;
      onerror: ((event: unknown) => void) | null = null;
      onclose: ((event: unknown) => void) | null = null;

      constructor(_url: string) {
        queueMicrotask(() => this.onopen?.({}));
      }

      send(raw: string): void {
        const request = JSON.parse(raw) as {
          readonly id: string;
          readonly method: string;
          readonly params: Record<string, unknown>;
        };
        if (request.method === "findIntersection") {
          intersectionPoints = request.params.points;
        }
        const result =
          request.method === "queryNetwork/genesisConfiguration"
            ? { networkMagic: 2 }
            : { intersection: block1, tip: block4 };
        queueMicrotask(() =>
          this.onmessage?.({
            data: JSON.stringify({
              jsonrpc: "2.0",
              id: request.id,
              result,
            }),
          }),
        );
      }

      close(): void {}
    }
    vi.stubGlobal("WebSocket", CommonAncestorWebSocket);
    try {
      const point1 = externalPoint("chain-sync:node-a", 1, "aa");
      const point2 = externalPoint("chain-sync:node-a", 2, "bb");
      const point3 = externalPoint("chain-sync:node-a", 3, "cc");
      const source = new OgmiosChainSyncEventSource(
        "ws://ogmios.local",
        "Preview",
        "node-a",
      );

      await expect(
        source.next({ sequence: 2, point: point3, rollbackGeneration: 0 }, [
          point3,
          point2,
          point1,
        ]),
      ).resolves.toMatchObject({
        event: {
          direction: "roll_backward",
          point: { slot: 1, blockHash: "aa".repeat(32) },
        },
        tip: { slot: 4, blockHash: "dd".repeat(32) },
      });
      expect(intersectionPoints).toEqual([
        { slot: 3, id: "cc".repeat(32) },
        { slot: 2, id: "bb".repeat(32) },
        { slot: 1, id: "aa".repeat(32) },
        "origin",
      ]);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("refuses to reuse a durable cursor for a different local authority", async () => {
    const dir = await tempDir();
    const path = `${dir}/cursor.json`;
    const point = externalPoint("chain-sync:node-a", 1, "ab");
    const original = new FileChainSyncCursorStore(path, "11".repeat(32));
    await original.append(
      { direction: "roll_forward", point },
      { sequence: 0, point, rollbackGeneration: 0 },
    );

    await expect(
      new FileChainSyncCursorStore(path, "22".repeat(32)).load(),
    ).rejects.toThrow(/authority fingerprint/u);
  });

  it("rejects local query snapshots that are stale against chain-sync authority", async () => {
    const dir = await tempDir();
    const canonical: CanonicalChainPoint = {
      network: "Preview",
      slot: 20,
      blockHash: "ab".repeat(32),
      providerSource: "chain-sync:node-a",
      observedAt: "2026-07-28T00:00:00.000Z",
    };
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      {
        next: async () => ({
          event: { direction: "roll_forward", point: canonical },
          tip: canonical,
        }),
      },
      new FileChainSyncCursorStore(`${dir}/cursor.json`, "11".repeat(32)),
    );
    const stalePoint: CanonicalChainPoint = {
      ...canonical,
      slot: 19,
      blockHash: "cd".repeat(32),
      providerSource: "query:node-a:0",
    };
    const provider = new LocalNodeStateQueueProvider(
      authority,
      [
        {
          fetchStateQueueNodes: async () => [],
          currentChainPoint: async () => stalePoint,
        },
      ],
      ["query:node-a:0"],
      new FileChainSyncConsumerCursorStore(
        `${dir}/consumer.json`,
        "11".repeat(32),
      ),
    );
    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /stale or on a mismatched chain point/u,
    );
  });

  it("merges aligned local query depth and finality conservatively", async () => {
    const dir = await tempDir();
    const canonical = externalPoint("chain-sync:node-a", 20, "ab");
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      {
        next: async () => ({
          event: { direction: "roll_forward", point: canonical },
          tip: canonical,
        }),
      },
      new FileChainSyncCursorStore(`${dir}/cursor.json`, "11".repeat(32)),
    );
    const { header, headerHash } = await makePayloadFixture();
    const first = makeObservedNode({ header, headerHash, depth: 10 });
    const second = makeObservedNode({ header, headerHash, depth: 3 });
    const queryPoint = (providerSource: string): CanonicalChainPoint => ({
      ...canonical,
      providerSource,
    });
    const provider = new LocalNodeStateQueueProvider(
      authority,
      [
        {
          fetchStateQueueNodes: async () => [
            {
              ...first,
              chainPoint: { ...first.chainPoint, finalized: true },
            },
          ],
          currentChainPoint: async () => queryPoint("query:kupo-a"),
        },
        {
          fetchStateQueueNodes: async () => [
            {
              ...second,
              chainPoint: { ...second.chainPoint, finalized: false },
            },
          ],
          currentChainPoint: async () => queryPoint("query:db-sync-a"),
        },
      ],
      ["query:kupo-a", "query:db-sync-a"],
      new FileChainSyncConsumerCursorStore(
        `${dir}/consumer.json`,
        "11".repeat(32),
      ),
    );

    await expect(provider.fetchStateQueueNodes()).resolves.toMatchObject([
      {
        chainPoint: {
          depth: 3,
          finalized: false,
          providerSource: "chain-sync:node-a,query:kupo-a,query:db-sync-a",
        },
      },
    ]);
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
        {
          fetchStateQueueNodes: async () => [canonical],
          currentChainPoint: async () => externalPoint("operator-a"),
        },
        {
          fetchStateQueueNodes: async () => [forked],
          currentChainPoint: async () => externalPoint("operator-b"),
        },
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

  it("does not convert Kupo slot distance into confirmation depth", async () => {
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

    const point = await resolve({ txHash, outputIndex: 1 } as never);
    expect(point).toMatchObject({
      slot: 126197476,
      blockHash: "bb".repeat(32),
    });
    expect(point).not.toHaveProperty("depth");
    expect(fetchFn).not.toHaveBeenCalled();
  });

  it("derives Kupmios confirmation depth from actual aligned node blocks", async () => {
    const txHash = "aa".repeat(32);
    const inclusion = { slot: 10, id: "11".repeat(32) };
    const tip = { slot: 100, id: "44".repeat(32), height: 4 };
    const descendants = [
      { slot: 20, id: "22".repeat(32) },
      { slot: 50, id: "33".repeat(32) },
      tip,
    ];
    let nextBlockIndex = 0;
    class ConfirmationDepthWebSocket {
      onopen: ((event: unknown) => void) | null = null;
      onmessage: ((event: { readonly data: unknown }) => void) | null = null;
      onerror: ((event: unknown) => void) | null = null;
      onclose: ((event: unknown) => void) | null = null;

      constructor(_url: string) {
        queueMicrotask(() => this.onopen?.({}));
      }

      send(raw: string): void {
        const request = JSON.parse(raw) as {
          readonly id: string;
          readonly method: string;
        };
        let result: unknown;
        if (request.method === "queryNetwork/tip") {
          result = tip;
        } else if (request.method === "queryNetwork/genesisConfiguration") {
          result = { networkMagic: 2 };
        } else if (request.method === "findIntersection") {
          result = { intersection: inclusion, tip };
        } else if (nextBlockIndex === 0) {
          nextBlockIndex += 1;
          result = { direction: "backward", point: inclusion, tip };
        } else {
          result = {
            direction: "forward",
            block: descendants[nextBlockIndex++ - 1],
            tip,
          };
        }
        queueMicrotask(() =>
          this.onmessage?.({
            data: JSON.stringify({
              jsonrpc: "2.0",
              id: request.id,
              result,
            }),
          }),
        );
      }

      close(): void {}
    }
    vi.stubGlobal("WebSocket", ConfirmationDepthWebSocket);
    const fetchFn = vi.fn(
      async () =>
        new Response("kupo_most_recent_checkpoint 100\n", {
          headers: { etag: `"${"44".repeat(32)}"` },
        }),
    );
    try {
      const resolve = kupmiosChainPointResolver(
        {
          transactionStatus: async () => ({
            status: "confirmed",
            txHash,
            confirmation: {
              txHash,
              slot: inclusion.slot,
              blockHash: inclusion.id,
            },
          }),
        } as unknown as LucidEvolution,
        "http://kupo.local",
        fetchFn as typeof fetch,
        "ws://ogmios.local",
        "Preview",
        3,
      );

      await expect(
        resolve({ txHash, outputIndex: 1 } as never),
      ).resolves.toMatchObject({
        slot: 10,
        blockHash: "11".repeat(32),
        depth: 3,
      });
      expect(nextBlockIndex).toBe(4);
      expect(fetchFn).toHaveBeenCalledTimes(2);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("binds a Kupo checkpoint slot to its ETag block hash", async () => {
    const fetchFn = vi.fn(
      async () =>
        new Response("kupo_most_recent_checkpoint 42\n", {
          headers: { etag: `"${"ab".repeat(32)}"` },
        }),
    );

    await expect(
      fetchKupoCheckpoint("http://kupo.local/", fetchFn as typeof fetch),
    ).resolves.toEqual({ slot: 42, blockHash: "ab".repeat(32) });
    expect(fetchFn).toHaveBeenCalledWith("http://kupo.local/health", {
      headers: { accept: "text/plain" },
    });

    await expect(
      fetchKupoCheckpoint(
        "http://kupo.local/",
        (async () =>
          new Response("kupo_most_recent_checkpoint 42\n")) as typeof fetch,
      ),
    ).rejects.toThrow(/checkpoint ETag/u);
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
      currentChainPoint: async () => externalPoint("provider-a"),
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
      currentChainPoint: async () => externalPoint("provider-b"),
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

  it("rejects external provider disagreement even when both result sets are empty", async () => {
    const provider = new MultiStateQueueProvider(
      [
        {
          fetchStateQueueNodes: async () => [],
          currentChainPoint: async () => externalPoint("provider-a", 100, "ab"),
        },
        {
          fetchStateQueueNodes: async () => [],
          currentChainPoint: async () => externalPoint("provider-b", 99, "cd"),
        },
      ],
      { identities: ["provider-a", "provider-b"] },
    );

    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /current chain-point disagreement/u,
    );
  });
});

const externalPoint = (
  providerSource: string,
  slot = 100,
  blockByte = "ab",
): CanonicalChainPoint => ({
  network: "Preview",
  slot,
  blockHash: blockByte.repeat(32),
  providerSource,
  observedAt: "2026-07-28T00:00:00.000Z",
});
