import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { ChainPoint } from "../src/domain.js";
import {
  ChainPointBatchDeadlineError,
  type ChainPointResolver,
  mapWithConcurrency,
} from "../src/l1/provider.chain-point-batch.js";
import {
  kupmiosChainPointResolver,
  LucidStateQueueProvider,
  stateQueueUtxosToObservedSnapshot,
} from "../src/l1/provider.js";
import { ChainMovedDuringSnapshotError } from "../src/l1/provider.local-node-chain-authority.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";
import { makePayloadFixture } from "./helpers.js";

// The lucid provider's state-queue read, replaced per test where a snapshot
// is read through the provider; everything else in the SDK is real.
vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof SDK>();
  return {
    ...actual,
    fetchSortedStateQueueUTxOs: vi.fn(actual.fetchSortedStateQueueUTxOs),
  };
});

/** Blocks at slots 10, 20, ..., 120; the chain's tip is `tipIndex`. */
const block = (index: number) => ({
  slot: (index + 1) * 10,
  id: (index + 1).toString(16).padStart(2, "0").repeat(32),
});

/**
 * A fake aligned Kupo and Ogmios over one chain. Counts aligned-tip reads
 * (one Kupo health read each) and the confirmation-depth walks open at once.
 */
const fakeKupmios = (
  options: {
    /** Moves the tip one block once this many walks have closed. */
    readonly moveTipAfterWalks?: number;
  } = {},
) => {
  let tipIndex = 9;
  let openWalks = 0;
  let maxOpenWalks = 0;
  let walksStarted = 0;
  let walksClosed = 0;
  const tip = () => ({ ...block(tipIndex), height: tipIndex + 1 });
  class FakeOgmiosSocket {
    onopen: ((event: unknown) => void) | null = null;
    onmessage: ((event: { readonly data: unknown }) => void) | null = null;
    onerror: ((event: unknown) => void) | null = null;
    onclose: ((event: unknown) => void) | null = null;
    private cursor: number | undefined;
    private walking = false;

    constructor(_url: string) {
      queueMicrotask(() => this.onopen?.({}));
    }

    send(raw: string): void {
      const request = JSON.parse(raw) as {
        readonly id: string;
        readonly method: string;
        readonly params?: { readonly points?: readonly unknown[] };
      };
      let result: unknown;
      if (request.method === "queryNetwork/tip") {
        result = tip();
      } else if (request.method === "queryNetwork/genesisConfiguration") {
        result = { networkMagic: 2 };
      } else if (request.method === "findIntersection") {
        const point = request.params?.points?.[0] as { readonly slot: number };
        this.cursor = point.slot / 10 - 1;
        this.walking = true;
        walksStarted += 1;
        openWalks += 1;
        maxOpenWalks = Math.max(maxOpenWalks, openWalks);
        result = { intersection: block(this.cursor), tip: tip() };
      } else if (this.cursor !== undefined && this.walking) {
        const index = this.cursor;
        this.walking = false;
        result = { direction: "backward", point: block(index), tip: tip() };
      } else {
        this.cursor = (this.cursor ?? 0) + 1;
        result = {
          direction: "forward",
          block: block(this.cursor),
          tip: tip(),
        };
      }
      // Answer on a later turn, so walks overlap the way real sessions do.
      setTimeout(() =>
        this.onmessage?.({
          data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
        }),
      );
    }

    close(): void {
      if (this.cursor === undefined) return;
      this.cursor = undefined;
      openWalks -= 1;
      walksClosed += 1;
      if (walksClosed === options.moveTipAfterWalks) tipIndex += 1;
    }
  }
  vi.stubGlobal("WebSocket", FakeOgmiosSocket);
  const fetchFn = vi.fn(
    async () =>
      new Response(`kupo_most_recent_checkpoint ${tip().slot.toString()}\n`, {
        headers: { etag: `"${tip().id}"` },
      }),
  );
  return {
    fetchFn,
    alignedTipReads: () => fetchFn.mock.calls.length,
    maxOpenWalks: () => maxOpenWalks,
    walksStarted: () => walksStarted,
  };
};

/** UTxO `index` was included in block `index`. */
const utxoAt = (index: number) =>
  ({ txHash: index.toString(16).padStart(64, "0"), outputIndex: 0 }) as UTxO;

const lucidAt = {
  transactionStatus: async (txHash: string) => {
    const index = Number.parseInt(txHash, 16);
    return {
      status: "confirmed",
      txHash,
      confirmation: {
        txHash,
        slot: block(index).slot,
        blockHash: block(index).id,
      },
    };
  },
} as unknown as LucidEvolution;

const resolverOver = (
  kupmios: ReturnType<typeof fakeKupmios>,
  batch: Parameters<typeof kupmiosChainPointResolver>[7] = {},
) =>
  kupmiosChainPointResolver(
    lucidAt,
    "http://kupo.local",
    kupmios.fetchFn as typeof fetch,
    "ws://ogmios.local",
    "Preview",
    3,
    undefined,
    batch,
  );

const UTXOS = [0, 1, 2, 3, 4, 5].map(utxoAt);

afterEach(() => {
  vi.unstubAllGlobals();
});

describe("one aligned Kupmios tip per state-queue snapshot", () => {
  it("resolves every UTxO against one tip read before and one after, at most four walks at a time, in order", async () => {
    const kupmios = fakeKupmios();
    const resolve = resolverOver(kupmios);

    const points = await resolve.resolveAll?.(UTXOS);

    expect(points?.map(({ slot, depth }) => ({ slot, depth }))).toEqual(
      UTXOS.map((_, index) => ({ slot: block(index).slot, depth: 3 })),
    );
    expect(kupmios.alignedTipReads()).toBe(2);
    expect(kupmios.walksStarted()).toBe(UTXOS.length);
    expect(kupmios.maxOpenWalks()).toBeGreaterThan(1);
    expect(kupmios.maxOpenWalks()).toBeLessThanOrEqual(4);
  });

  it("keeps a bracket per call when resolving one UTxO at a time", async () => {
    const kupmios = fakeKupmios();
    const resolve = resolverOver(kupmios);
    for (const utxo of UTXOS) await resolve(utxo);
    expect(kupmios.alignedTipReads()).toBe(2 * UTXOS.length);
  });

  it("still refuses the snapshot when the tip moved between the pinned read and the closing one", async () => {
    const kupmios = fakeKupmios({ moveTipAfterWalks: UTXOS.length });
    const resolve = resolverOver(kupmios);
    await expect(resolve.resolveAll?.(UTXOS)).rejects.toBeInstanceOf(
      ChainMovedDuringSnapshotError,
    );
    expect(kupmios.alignedTipReads()).toBe(2);
  });

  it("abandons a pass that outlasts its deadline before starting further walks, and the next pass resolves exactly once", async () => {
    const kupmios = fakeKupmios();
    let now = 0;
    const slow = resolverOver(kupmios, {
      batchDeadlineMs: 60_000,
      // Every clock read is 25 s later than the one before it.
      nowMs: () => (now += 25_000),
    });
    await expect(slow.resolveAll?.(UTXOS)).rejects.toBeInstanceOf(
      ChainPointBatchDeadlineError,
    );
    expect(kupmios.walksStarted()).toBeLessThan(UTXOS.length);

    const started = kupmios.walksStarted();
    const reads = kupmios.alignedTipReads();
    const points = await resolverOver(kupmios, {
      batchDeadlineMs: 60_000,
    }).resolveAll?.(UTXOS);
    expect(points).toHaveLength(UTXOS.length);
    expect(kupmios.walksStarted() - started).toBe(UTXOS.length);
    expect(kupmios.alignedTipReads() - reads).toBe(2);
  });
});

describe("snapshot chain-point resolution", () => {
  const snapshotUtxos = async (): Promise<SDK.StateQueueUTxO[]> => {
    const { header } = await makePayloadFixture();
    const headerHash = hashBlockHeader(header);
    const root: SDK.LinkedListNodeView = {
      key: "Empty",
      next: { Key: { key: headerHash } },
      data: SDK.castConfirmedStateToData(
        SDK.makeGenesisConfirmedState(0n),
      ) as SDK.LinkedListNodeView["data"],
    };
    const node: SDK.LinkedListNodeView = {
      key: { Key: { key: headerHash } },
      next: "Empty",
      data: Data.castTo(
        { proven_fraud: null, header, da_attestation: SDK.NO_DA_ATTESTATION },
        SDK.StateQueueNode,
      ) as SDK.LinkedListNodeView["data"],
    };
    return [root, node].map((datum, index) => ({
      utxo: {
        ...utxoAt(index),
        address: "addr_test1statequeue",
        assets: { lovelace: 5_000_000n },
        datum: SDK.encodeLinkedListNodeView(datum),
      },
      datum,
      assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    }));
  };

  it("resolves the root and every node in one resolveAll call, keeping each point with its UTxO", async () => {
    const utxos = await snapshotUtxos();
    const single = vi.fn(async (): Promise<ChainPoint> => ({ depth: 0 }));
    const resolveAll = vi.fn(
      async (batch: readonly UTxO[]): Promise<readonly ChainPoint[]> =>
        batch.map((utxo) => ({ depth: Number.parseInt(utxo.txHash, 16) + 7 })),
    );
    const resolver: ChainPointResolver = Object.assign(single, {
      resolveAll,
    });

    const snapshot = await stateQueueUtxosToObservedSnapshot(
      utxos,
      "test-provider",
      resolver,
    );

    expect(resolveAll).toHaveBeenCalledOnce();
    expect(resolveAll.mock.calls[0]?.[0]).toEqual(
      utxos.map(({ utxo }) => utxo),
    );
    expect(single).not.toHaveBeenCalled();
    expect(snapshot.observedChainPoint.depth).toBe(7);
    expect(snapshot.nodes.map(({ chainPoint }) => chainPoint.depth)).toEqual([
      8,
    ]);
  });

  it("keeps resolveAll on the resolver a lucid state-queue provider is given, so its snapshot takes the batch path", async () => {
    const utxos = await snapshotUtxos();
    vi.mocked(SDK.fetchSortedStateQueueUTxOs).mockResolvedValueOnce(utxos);
    const single = vi.fn(async (): Promise<ChainPoint> => ({ depth: 0 }));
    const resolveAll = vi.fn(
      async (batch: readonly UTxO[]): Promise<readonly ChainPoint[]> =>
        batch.map(() => ({ depth: 9 })),
    );
    const provider = new LucidStateQueueProvider({
      lucid: {} as LucidEvolution,
      stateQueueAddress: "addr_test1statequeue",
      stateQueuePolicyId: "00".repeat(28),
      providerSource: "test-provider",
      chainPointResolver: Object.assign(single, { resolveAll }),
      currentChainPointResolver: () =>
        Promise.reject(new Error("a snapshot does not read the chain point")),
      tipBlockNoResolver: async () => 10,
      replayCheckpoints: async () => [],
    });

    const snapshot = await provider.fetchStateQueueSnapshot();

    expect(resolveAll).toHaveBeenCalledOnce();
    expect(single).not.toHaveBeenCalled();
    expect(snapshot.nodes.map(({ chainPoint }) => chainPoint.depth)).toEqual([
      9,
    ]);
  });

  it("falls back to per-UTxO resolution for a resolver without resolveAll", async () => {
    const utxos = await snapshotUtxos();
    const single = vi.fn(
      async (utxo: UTxO): Promise<ChainPoint> => ({
        depth: Number.parseInt(utxo.txHash, 16) + 7,
      }),
    );
    const snapshot = await stateQueueUtxosToObservedSnapshot(
      utxos,
      "test-provider",
      single,
    );
    expect(single).toHaveBeenCalledTimes(2);
    expect(snapshot.observedChainPoint.depth).toBe(7);
    expect(snapshot.nodes[0]?.chainPoint.depth).toBe(8);
  });
});

describe("bounded fan-out", () => {
  it("starts nothing after the first failure and lets the running calls settle before rethrowing it", async () => {
    const started: number[] = [];
    const settled: number[] = [];
    const failure = new Error("walk failed");
    await expect(
      mapWithConcurrency([0, 1, 2, 3, 4, 5, 6, 7], 3, async (item) => {
        started.push(item);
        await new Promise((resolve) => setTimeout(resolve, item === 1 ? 1 : 5));
        settled.push(item);
        if (item === 1) throw failure;
        return item;
      }),
    ).rejects.toBe(failure);
    expect(started).toEqual([0, 1, 2]);
    expect([...settled].sort()).toEqual([0, 1, 2]);
  });
});
