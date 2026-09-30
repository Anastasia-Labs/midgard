import "./lucid-network.the-l1-factories-main-calls-build-custom-lucid-on-the-genesis-mapping.js";

import { join } from "node:path";

import { type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { availabilityResponderL1ReadersFromConfig } from "../src/availability/factory.js";
import type { LoadedCommitteeConfig } from "../src/config.js";
import { daAttestationReaderFromConfig } from "../src/l1/da-attestation-reader.js";
import { localNodeChainAuthorityFromConfig } from "../src/l1/provider.js";
import { tempDir } from "./helpers.js";
import {
  CUSTOM_MAGIC,
  kupmiosUrl,
  lucidOf,
  startFakeOgmios,
  stateQueueProvider,
} from "./lucid-network.start-fake-ogmios.js";

describe("the live L1 reads main() wires prove Custom identity against the configured magic", () => {
  // After the Lucid clients are built, every later live read (the aligned
  // Kupmios tip, the confirmation-depth query, the chain-sync session)
  // compares the chain's magic with the configured one; the configured magic
  // has to reach each of them.
  const tip = { slot: 100, id: "44".repeat(32) };
  class LiveCustomOgmiosWebSocket {
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
      const result =
        request.method === "queryNetwork/genesisConfiguration"
          ? { networkMagic: CUSTOM_MAGIC }
          : request.method === "findIntersection"
            ? { intersection: tip, tip }
            : tip;
      queueMicrotask(() =>
        this.onmessage?.({
          data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
        }),
      );
    }

    close(): void {}
  }
  const onLiveCustomChain = async <A>(run: () => Promise<A>): Promise<A> => {
    vi.stubGlobal("WebSocket", LiveCustomOgmiosWebSocket);
    vi.stubGlobal(
      "fetch",
      async () =>
        new Response(`kupo_most_recent_checkpoint ${tip.slot.toString()}\n`, {
          headers: { etag: `"${tip.id}"` },
        }),
    );
    try {
      return await run();
    } finally {
      vi.unstubAllGlobals();
    }
  };
  const atTip = { network: "Custom", slot: tip.slot, blockHash: tip.id };
  // A state-queue output confirmed in the tip block: its depth query runs the
  // full identity-checked path and counts zero descendants.
  const confirmedAtTip = async () => ({
    status: "confirmed",
    txHash: "aa".repeat(32),
    confirmation: {
      txHash: "aa".repeat(32),
      slot: tip.slot,
      blockHash: tip.id,
    },
  });
  const outputAtTip = { txHash: "aa".repeat(32), outputIndex: 0 } as never;

  it("the state-queue provider's current chain point", async () => {
    const ogmios = await startFakeOgmios({ networkMagic: CUSTOM_MAGIC });
    const provider = await stateQueueProvider({
      network: "Custom",
      ogmios,
      networkMagic: CUSTOM_MAGIC,
    });
    await expect(
      onLiveCustomChain(() =>
        (
          provider as unknown as {
            readonly currentChainPoint: () => Promise<unknown>;
          }
        ).currentChainPoint(),
      ),
    ).resolves.toMatchObject(atTip);
  });

  it("the state-queue provider's confirmation-depth query", async () => {
    const ogmios = await startFakeOgmios({ networkMagic: CUSTOM_MAGIC });
    // The depth resolver keeps the fetch it was built with for its Kupo reads;
    // the fake Ogmios answers the build-time magic and slot queries.
    const realFetch = globalThis.fetch;
    vi.stubGlobal("fetch", (async (input, init) =>
      String(input).startsWith("http://127.0.0.1:1442")
        ? new Response(`kupo_most_recent_checkpoint ${tip.slot.toString()}\n`, {
            headers: { etag: `"${tip.id}"` },
          })
        : realFetch(input, init)) as typeof fetch);
    const provider = await stateQueueProvider({
      network: "Custom",
      ogmios,
      networkMagic: CUSTOM_MAGIC,
    }).finally(() => vi.unstubAllGlobals());
    vi.spyOn(lucidOf(provider), "transactionStatus").mockImplementation(
      confirmedAtTip as never,
    );
    await expect(
      onLiveCustomChain(() =>
        (
          provider as unknown as {
            readonly chainPointResolver: (utxo: never) => Promise<unknown>;
          }
        ).chainPointResolver(outputAtTip),
      ),
    ).resolves.toMatchObject({ slot: tip.slot, blockHash: tip.id, depth: 0 });
  });

  it("the availability responder's current point and inclusion depth", async () => {
    const [currentPoint, inclusion] = await onLiveCustomChain(async () => {
      const readers = availabilityResponderL1ReadersFromConfig({
        config: {
          network: "Custom",
          finalityDepth: 1,
          cardanoL1Source: { networkMagic: CUSTOM_MAGIC },
        },
        lucid: {
          transactionStatus: confirmedAtTip,
        } as unknown as LucidEvolution,
        kupoUrl: "http://kupo.custom.local",
        ogmiosUrl: "ws://ogmios.custom.local",
        currentCursor: async () => {
          throw new Error("unread");
        },
      });
      return [
        await readers.currentPoint(),
        await readers.resolveInclusion(outputAtTip),
      ] as const;
    });
    expect(currentPoint).toMatchObject(atTip);
    expect(inclusion).toMatchObject({
      slot: tip.slot,
      blockHash: tip.id,
      depth: 0,
    });
  });

  it("the DA attestation reader's query point", async () => {
    const ogmios = await startFakeOgmios({ networkMagic: CUSTOM_MAGIC });
    const dir = await tempDir();
    const built = await daAttestationReaderFromConfig({
      network: "Custom",
      cardanoL1Source: {
        sourceMode: "local_node",
        authorityNodeId: "local-cardano-node",
        authorityDigest: "ab".repeat(32),
        networkMagic: CUSTOM_MAGIC,
      },
      l1Source: {
        sourceMode: "local_node",
        authorityNodeId: "local-cardano-node",
        chainSyncProviderUrl: `chain-sync:ogmios:${ogmios.url}`,
        chainSyncCursorPath: join(dir, "chain-sync-cursor.json"),
        queryProviderUrls: [kupmiosUrl(ogmios)],
      },
      localState: { kind: "file", path: join(dir, "state.json") },
    } as unknown as LoadedCommitteeConfig);
    const queryPoint = (
      built as unknown as {
        readonly queryPointResolver: () => Promise<unknown>;
      }
    ).queryPointResolver;
    await expect(onLiveCustomChain(queryPoint)).resolves.toMatchObject(atTip);
  });

  it.each([
    "chain-sync:ogmios:ws://ogmios.custom.local",
    "chain-sync:kupmios:http://kupo.custom.local|ws://ogmios.custom.local",
  ])("the local-node chain-sync authority on %s", async (chainSyncUrl) => {
    const dir = await tempDir();
    const authority = localNodeChainAuthorityFromConfig({
      network: "Custom",
      cardanoL1Source: { networkMagic: CUSTOM_MAGIC },
      l1Source: {
        sourceMode: "local_node",
        authorityNodeId: "local-cardano-node",
        chainSyncProviderUrl: chainSyncUrl,
        chainSyncCursorPath: join(dir, "chain-sync-cursor.json"),
        queryProviderUrls: [],
      },
      localState: { kind: "file", path: join(dir, "state.json") },
    } as unknown as LoadedCommitteeConfig);
    await expect(
      onLiveCustomChain(() => authority.synchronizeToTip(1)),
    ).resolves.toMatchObject(atTip);
  });
});
