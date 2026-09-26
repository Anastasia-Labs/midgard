import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { appendFile, open, readFile, writeFile } from "node:fs/promises";

import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import {
  kupoExchange,
  type L1Recording,
  loadL1Recording,
  ogmiosExchanges,
  ogmiosResult,
  recordedFetch,
  recordedOgmiosWebSocket,
  recordedText,
} from "@al-ft/midgard-test-support/l1-recordings";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  type DaSignatureRecordV1,
  parseDaSignatureRecord,
} from "../src/domain.js";
import {
  assertOgmiosNetworkMagic,
  type CanonicalChainPoint,
  CHAIN_SYNC_INTERSECTION_POINTS,
  CHAIN_SYNC_JOURNAL_PRUNE_SLACK,
  type ChainSyncCatchUpProgress,
  type ChainSyncCursor,
  type ChainSyncCursorStore,
  type ChainSyncEvent,
  type ChainSyncEventBatch,
  ChainSyncNoProgressError,
  fetchKupoCheckpoint,
  FileChainSyncConsumerCursorStore,
  FileChainSyncCursorStore,
  KUPMIOS_TIP_ALIGNMENT_ATTEMPTS,
  kupmiosChainPointResolver,
  kupmiosCurrentChainPointResolver,
  l1AuthorityProviderSource,
  LOCAL_NODE_SNAPSHOT_ATTEMPTS,
  LocalNodeChainAuthority,
  LocalNodeStateQueueProvider,
  lucidChainPointResolver,
  MultiStateQueueProvider,
  OgmiosChainSyncEventSource,
  providerFromUrl,
  requireStateQueueReplaySource,
  STATE_QUEUE_REPLAY_ATTEMPTS,
  stateQueueUtxosToObservedNodes,
} from "../src/l1/provider.js";
import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import {
  hashBlockHeader,
  type StateQueueProvider,
} from "../src/l1/state-queue-scanner.js";
import {
  makeObservedNode,
  makePayloadFixture,
  tempDir,
  writeJson,
} from "./helpers.js";

// Passthrough, so a test can make the store's own journal append fail part-way.
vi.mock("node:fs/promises", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs/promises")>();
  return { ...actual, appendFile: vi.fn(actual.appendFile) };
});

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

  it("refuses a Blockfrost state-queue provider at startup: it has no replay source", async () => {
    await expect(
      providerFromUrl("blockfrost:https://preview.example/api#project", {
        network: "Preview",
        cardanoL1Source: localNodeSource,
        stateQueueAddress: "addr_test1statequeue",
        stateQueuePolicyId: "11".repeat(28),
      }),
    ).rejects.toThrow(
      "blockfrost: cannot serve the state queue: it has no authenticated ordered history source; use kupmios:<kupo-url>|<ogmios-url>",
    );
  });

  it("normalizes SDK StateQueueUTxOs into scanner observations", async () => {
    const { header } = await makePayloadFixture();
    const headerHash = hashBlockHeader(header);
    const datum: SDK.LinkedListNodeView = {
      key: { Key: { key: headerHash } },
      next: "Empty",
      data: Data.castTo(
        { proven_fraud: null, header, da_attestation: SDK.NO_DA_ATTESTATION },
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

  it("discards a torn final journal line left by an interrupted append", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const journalPath = `${cursorPath}.events.jsonl`;
    const points = await seedChainSyncJournal(cursorPath, 3);
    const committed = await readFile(journalPath, "utf8");
    const nextPoint = externalPoint("chain-sync:node-a", 4, "04");
    await appendFile(journalPath, journalLine(3, nextPoint).slice(0, 57));

    const restarted = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    await expect(restarted.load()).resolves.toEqual({
      sequence: 2,
      point: points[2],
      rollbackGeneration: 0,
    });
    await expect(readFile(journalPath, "utf8")).resolves.toBe(committed);
    await restarted.append(
      { direction: "roll_forward", point: nextPoint },
      { sequence: 3, point: nextPoint, rollbackGeneration: 0 },
    );
    const replayed = [...points, nextPoint].map((point) => ({
      direction: "roll_forward",
      point,
    }));
    await expect(restarted.replay(-1)).resolves.toEqual(replayed);
    const reopened = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    await expect(reopened.load()).resolves.toMatchObject({ sequence: 3 });
    await expect(reopened.replay(-1)).resolves.toEqual(replayed);
  });

  it("discards an unterminated final journal line even when it is complete JSON", async () => {
    // The newline is the commit marker: the cursor metadata is written only
    // after the whole line, newline included, is durable, so an unterminated
    // line was never acknowledged. It is dropped, not adopted; the chain-sync
    // source re-delivers its event from the recovered cursor.
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const journalPath = `${cursorPath}.events.jsonl`;
    const points = await seedChainSyncJournal(cursorPath, 3);
    const committed = await readFile(journalPath, "utf8");
    const nextPoint = externalPoint("chain-sync:node-a", 4, "04");
    await appendFile(journalPath, journalLine(3, nextPoint));

    const restarted = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    await expect(restarted.load()).resolves.toMatchObject({
      sequence: 2,
      point: points[2],
    });
    await expect(readFile(journalPath, "utf8")).resolves.toBe(committed);
    await expect(restarted.replay(-1)).resolves.toHaveLength(3);
    await restarted.append(
      { direction: "roll_forward", point: nextPoint },
      { sequence: 3, point: nextPoint, rollbackGeneration: 0 },
    );
    await expect(
      new FileChainSyncCursorStore(cursorPath, "11".repeat(32)).replay(-1),
    ).resolves.toEqual(
      [...points, nextPoint].map((point) => ({
        direction: "roll_forward",
        point,
      })),
    );
  });

  it("repairs the journal after its own append fails part-way through a line", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const journalPath = `${cursorPath}.events.jsonl`;
    const points = await seedChainSyncJournal(cursorPath, 2);
    const committed = await readFile(journalPath, "utf8");
    const { appendFile: realAppendFile } =
      await vi.importActual<typeof import("node:fs/promises")>(
        "node:fs/promises",
      );
    vi.mocked(appendFile).mockImplementationOnce(async (path, data) => {
      await realAppendFile(path, String(data).slice(0, 61));
      throw Object.assign(new Error("ENOSPC: no space left on device, write"), {
        code: "ENOSPC",
      });
    });
    const store = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    const nextPoint = externalPoint("chain-sync:node-a", 3, "03");
    const nextEvent = { direction: "roll_forward", point: nextPoint } as const;
    const nextCursor = { sequence: 2, point: nextPoint, rollbackGeneration: 0 };
    await expect(store.append(nextEvent, nextCursor)).rejects.toThrow(
      /ENOSPC/u,
    );
    expect(await readFile(journalPath, "utf8")).not.toMatch(/\n$/u);

    await expect(store.load()).resolves.toMatchObject({
      sequence: 1,
      point: points[1],
    });
    await expect(readFile(journalPath, "utf8")).resolves.toBe(committed);
    await store.append(nextEvent, nextCursor);
    await expect(
      new FileChainSyncCursorStore(cursorPath, "11".repeat(32)).replay(-1),
    ).resolves.toEqual(
      [...points, nextPoint].map((point) => ({
        direction: "roll_forward",
        point,
      })),
    );
  });

  it("makes each journal line durable before the cursor metadata commits it, and each tail repair durable", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const journalPath = `${cursorPath}.events.jsonl`;
    const probe = await open(`${dir}/probe`, "w");
    const fileHandlePrototype = Object.getPrototypeOf(probe) as {
      datasync(): Promise<void>;
    };
    await probe.close();
    const datasync = fileHandlePrototype.datasync;
    const observed: { journal: string; metadataSequence?: number }[] = [];
    const spy = vi
      .spyOn(fileHandlePrototype, "datasync")
      .mockImplementation(async function (this: unknown) {
        let metadataSequence: number | undefined;
        try {
          metadataSequence = (
            JSON.parse(readFileSync(cursorPath, "utf8")) as {
              cursor?: { sequence: number };
            }
          ).cursor?.sequence;
        } catch {
          metadataSequence = undefined;
        }
        observed.push({
          journal: readFileSync(journalPath, "utf8"),
          metadataSequence,
        });
        return datasync.call(this);
      });
    try {
      const points = await seedChainSyncJournal(cursorPath, 2);
      // Each append synced the journal with its newline-terminated line while
      // the metadata still named the previous cursor.
      expect(observed).toEqual([
        { journal: `${journalLine(0, points[0]!)}\n` },
        {
          journal: `${journalLine(0, points[0]!)}\n${journalLine(1, points[1]!)}\n`,
          metadataSequence: 0,
        },
      ]);
      const committed = await readFile(journalPath, "utf8");
      await appendFile(
        journalPath,
        journalLine(2, externalPoint("chain-sync:node-a", 3, "03")).slice(
          0,
          40,
        ),
      );
      observed.length = 0;
      await new FileChainSyncCursorStore(cursorPath, "11".repeat(32)).load();
      expect(observed).toEqual([{ journal: committed, metadataSequence: 1 }]);
    } finally {
      spy.mockRestore();
    }
  });

  describe("chain-sync journal integrity beside a torn tail", () => {
    const seededWithJournal = async (
      rewrite: (lines: readonly string[]) => string,
    ): Promise<{ cursorPath: string; journal: string }> => {
      const dir = await tempDir();
      const cursorPath = `${dir}/chain-sync-cursor.json`;
      const points = await seedChainSyncJournal(cursorPath, 3);
      const journal = rewrite(
        points.map((point, sequence) => journalLine(sequence, point)),
      );
      await writeFile(`${cursorPath}.events.jsonl`, journal);
      return { cursorPath, journal };
    };
    const loadFails = async (
      { cursorPath, journal }: { cursorPath: string; journal: string },
      expected: (error: unknown) => void,
    ): Promise<void> => {
      const error = await new FileChainSyncCursorStore(
        cursorPath,
        "11".repeat(32),
      )
        .load()
        .then(
          () => undefined,
          (failure: unknown) => failure,
        );
      expected(error);
      // Nothing on the path to a refusal rewrites the journal.
      await expect(
        readFile(`${cursorPath}.events.jsonl`, "utf8"),
      ).resolves.toBe(journal);
    };

    it("refuses a newline-terminated unparseable final line", async () => {
      await loadFails(
        await seededWithJournal(
          (lines) => `${lines.join("\n")}\n${lines[2]!.slice(0, 40)}\n`,
        ),
        (error) => expect(error).toBeInstanceOf(SyntaxError),
      );
    });

    it("refuses a torn line followed by complete lines", async () => {
      // What appending after an unrepaired torn line produces.
      await loadFails(
        await seededWithJournal(
          ([line0, line1, line2]) =>
            `${line0!}\n${line1!.slice(0, 40)}${line1!}\n${line2!}\n${line2!.slice(0, 30)}`,
        ),
        (error) => expect(error).toBeInstanceOf(SyntaxError),
      );
    });

    it("refuses a duplicate sequence among the complete lines", async () => {
      await loadFails(
        await seededWithJournal(
          ([line0, line1, line2]) =>
            `${line0!}\n${line1!}\n${line1!}\n${line2!}\n${line2!.slice(0, 30)}`,
        ),
        (error) => {
          expect(error).toBeInstanceOf(L1SourceIntegrityError);
          expect(error).toHaveProperty(
            "message",
            expect.stringMatching(/sequences must be contiguous/u),
          );
        },
      );
    });

    it.each([
      ["torn", (line: string) => line.slice(0, 40)],
      ["unterminated complete-JSON", (line: string) => line],
    ])(
      "refuses cursor metadata that points at a %s final line",
      async (_label, tail) => {
        // Metadata is written only after its line is durable, so a cursor that
        // names an incomplete line was not written by this store.
        await loadFails(
          await seededWithJournal(
            ([line0, line1, line2]) => `${line0!}\n${line1!}\n${tail(line2!)}`,
          ),
          (error) => {
            expect(error).toBeInstanceOf(L1SourceIntegrityError);
            expect(error).toHaveProperty(
              "message",
              expect.stringMatching(
                /cursor does not match its durable event journal/u,
              ),
            );
          },
        );
      },
    );
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
      // Both syncs found the caller at the node tip: neither waited on a
      // nextBlock, which Ogmios answers there only once another block lands.
      expect(nextBlockCount).toBe(0);
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

  it("resumes after a journal append whose cursor metadata failed without re-recording the block", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const fingerprint = "11".repeat(32);
    const node = fakeOgmiosNode([
      { slot: 1, id: "aa".repeat(32) },
      { slot: 2, id: "bb".repeat(32) },
      { slot: 3, id: "cc".repeat(32) },
      { slot: 4, id: "dd".repeat(32) },
    ]);
    let inner = new FileChainSyncCursorStore(cursorPath, fingerprint);
    const point1 = externalPoint("chain-sync:node-a", 1, "aa");
    await inner.append(
      { direction: "roll_forward", point: point1 },
      { sequence: 0, point: point1, rollbackGeneration: 0 },
    );
    let failures = 0;
    const store: ChainSyncCursorStore = {
      load: () => inner.load(),
      replay: (afterSequence) => inner.replay(afterSequence),
      cursorAt: (sequence) => inner.cursorAt(sequence),
      intersectionPoints: (limit) => inner.intersectionPoints(limit),
      append: async (event, cursor) => {
        if (cursor.sequence !== 1 || failures > 0) {
          await inner.append(event, cursor);
          return;
        }
        failures += 1;
        // The journal line reaches disk, then the cursor metadata write fails
        // (ENOSPC): the metadata stays at the previous cursor and the store
        // forgets its cache, exactly as FileChainSyncCursorStore does.
        const previousMetadata = await readFile(cursorPath, "utf8");
        await inner.append(event, cursor);
        await writeFile(cursorPath, previousMetadata);
        inner = new FileChainSyncCursorStore(cursorPath, fingerprint);
        throw new Error("ENOSPC: no space left on device, write");
      },
    };
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const authority = new LocalNodeChainAuthority(
        "node-a",
        "Preview",
        new OgmiosChainSyncEventSource(
          "ws://ogmios.local",
          "Preview",
          "node-a",
        ),
        store,
      );
      await expect(authority.synchronizeToTip()).rejects.toThrow(/ENOSPC/u);
      await expect(authority.synchronizeToTip()).resolves.toMatchObject({
        slot: 4,
        blockHash: "dd".repeat(32),
      });
      await expect(authority.currentCursor()).resolves.toMatchObject({
        sequence: 3,
        point: { slot: 4 },
      });
      expect(
        (await authority.replay(-1)).map(({ direction, point }) => [
          direction,
          point.slot,
        ]),
      ).toEqual([
        ["roll_forward", 1],
        ["roll_forward", 2],
        ["roll_forward", 3],
        ["roll_forward", 4],
      ]);
      // The session already stood at the recovered cursor: no re-intersection.
      expect(node.sockets()).toBe(1);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("re-delivers a block whose journal append failed before reaching disk", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const node = fakeOgmiosNode([
      { slot: 1, id: "aa".repeat(32) },
      { slot: 2, id: "bb".repeat(32) },
      { slot: 3, id: "cc".repeat(32) },
      { slot: 4, id: "dd".repeat(32) },
    ]);
    const inner = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    const point1 = externalPoint("chain-sync:node-a", 1, "aa");
    await inner.append(
      { direction: "roll_forward", point: point1 },
      { sequence: 0, point: point1, rollbackGeneration: 0 },
    );
    let failures = 0;
    const store: ChainSyncCursorStore = {
      load: () => inner.load(),
      replay: (afterSequence) => inner.replay(afterSequence),
      cursorAt: (sequence) => inner.cursorAt(sequence),
      intersectionPoints: (limit) => inner.intersectionPoints(limit),
      append: async (event, cursor) => {
        if (cursor.sequence === 1 && failures === 0) {
          failures += 1;
          throw new Error("ENOSPC: no space left on device, write");
        }
        await inner.append(event, cursor);
      },
    };
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const authority = new LocalNodeChainAuthority(
        "node-a",
        "Preview",
        new OgmiosChainSyncEventSource(
          "ws://ogmios.local",
          "Preview",
          "node-a",
        ),
        store,
      );
      await expect(authority.synchronizeToTip()).rejects.toThrow(/ENOSPC/u);
      await expect(authority.synchronizeToTip()).resolves.toMatchObject({
        slot: 4,
        blockHash: "dd".repeat(32),
      });
      expect(
        (await authority.replay(-1)).map(({ direction, point }) => [
          direction,
          point.slot,
        ]),
      ).toEqual([
        ["roll_forward", 1],
        ["roll_forward", 2],
        ["roll_forward", 3],
        ["roll_forward", 4],
      ]);
      // The session had already delivered block 2, so it re-intersected at the
      // durable cursor instead of continuing past the lost block.
      expect(node.sockets()).toBe(2);
      expect(node.intersections()[1]![0]).toEqual({
        slot: 1,
        id: "aa".repeat(32),
      });
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("re-delivers a block whose journal append tore part-way through its line", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const journalPath = `${cursorPath}.events.jsonl`;
    const fingerprint = "11".repeat(32);
    const node = fakeOgmiosNode([
      { slot: 1, id: "aa".repeat(32) },
      { slot: 2, id: "bb".repeat(32) },
      { slot: 3, id: "cc".repeat(32) },
      { slot: 4, id: "dd".repeat(32) },
    ]);
    let inner = new FileChainSyncCursorStore(cursorPath, fingerprint);
    const point1 = externalPoint("chain-sync:node-a", 1, "aa");
    await inner.append(
      { direction: "roll_forward", point: point1 },
      { sequence: 0, point: point1, rollbackGeneration: 0 },
    );
    let failures = 0;
    const store: ChainSyncCursorStore = {
      load: () => inner.load(),
      replay: (afterSequence) => inner.replay(afterSequence),
      cursorAt: (sequence) => inner.cursorAt(sequence),
      intersectionPoints: (limit) => inner.intersectionPoints(limit),
      append: async (event, cursor) => {
        if (cursor.sequence !== 1 || failures > 0) {
          await inner.append(event, cursor);
          return;
        }
        failures += 1;
        // The disk fills part-way through the journal line: a torn,
        // unterminated line, no cursor metadata, and a store that forgets its
        // cache, exactly as FileChainSyncCursorStore does on a failed append.
        await appendFile(
          journalPath,
          JSON.stringify({ sequence: cursor.sequence, event, cursor }).slice(
            0,
            73,
          ),
        );
        inner = new FileChainSyncCursorStore(cursorPath, fingerprint);
        throw new Error("ENOSPC: no space left on device, write");
      },
    };
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const authority = new LocalNodeChainAuthority(
        "node-a",
        "Preview",
        new OgmiosChainSyncEventSource(
          "ws://ogmios.local",
          "Preview",
          "node-a",
        ),
        store,
      );
      await expect(authority.synchronizeToTip()).rejects.toThrow(/ENOSPC/u);
      await expect(authority.synchronizeToTip()).resolves.toMatchObject({
        slot: 4,
        blockHash: "dd".repeat(32),
      });
      await expect(authority.currentCursor()).resolves.toMatchObject({
        sequence: 3,
        point: { slot: 4 },
      });
      expect(
        (await authority.replay(-1)).map(({ direction, point }) => [
          direction,
          point.slot,
        ]),
      ).toEqual([
        ["roll_forward", 1],
        ["roll_forward", 2],
        ["roll_forward", 3],
        ["roll_forward", 4],
      ]);
      const journal = await readFile(journalPath, "utf8");
      expect(journal.endsWith("\n")).toBe(true);
      expect(
        journal
          .split("\n")
          .filter((line) => line.length > 0)
          .map((line) => (JSON.parse(line) as { sequence: number }).sequence),
      ).toEqual([0, 1, 2, 3]);
      // The session had delivered block 2, so it re-intersected at the
      // recovered cursor rather than continuing past the torn block.
      expect(node.sockets()).toBe(2);
      expect(node.intersections()[1]![0]).toEqual({
        slot: 1,
        id: "aa".repeat(32),
      });
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("re-intersects Ogmios at a caller cursor that diverges from its delivered point", async () => {
    const node = fakeOgmiosNode([
      { slot: 1, id: "aa".repeat(32) },
      { slot: 2, id: "bb".repeat(32) },
      { slot: 3, id: "cc".repeat(32) },
    ]);
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const point1 = externalPoint("chain-sync:node-a", 1, "aa");
      const point2 = externalPoint("chain-sync:node-a", 2, "bb");
      const source = new OgmiosChainSyncEventSource(
        "ws://ogmios.local",
        "Preview",
        "node-a",
      );
      const block2 = {
        event: { direction: "roll_forward", point: { slot: 2 } },
      };
      await expect(
        source.next({ sequence: 0, point: point1, rollbackGeneration: 0 }),
      ).resolves.toMatchObject(block2);
      // The caller did not record block 2: the source must hand it back, not
      // continue to block 3 and not surface the handshake rollback.
      await expect(
        source.next({ sequence: 0, point: point1, rollbackGeneration: 0 }),
      ).resolves.toMatchObject(block2);
      expect(node.sockets()).toBe(2);
      // A caller at the delivered point continues the same session.
      await expect(
        source.next({ sequence: 1, point: point2, rollbackGeneration: 0 }),
      ).resolves.toMatchObject({
        event: { direction: "roll_forward", point: { slot: 3 } },
      });
      expect(node.sockets()).toBe(2);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("re-delivers an unrecorded intersection rollback once across a forced reconnect", async () => {
    const node = fakeOgmiosNode([
      { slot: 1, id: "aa".repeat(32) },
      { slot: 2, id: "ee".repeat(32) },
      { slot: 3, id: "ff".repeat(32) },
    ]);
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const point1 = externalPoint("chain-sync:node-a", 1, "aa");
      const orphan2 = externalPoint("chain-sync:node-a", 2, "bb");
      const source = new OgmiosChainSyncEventSource(
        "ws://ogmios.local",
        "Preview",
        "node-a",
      );
      const orphanCursor = {
        sequence: 1,
        point: orphan2,
        rollbackGeneration: 0,
      };
      const rollback = {
        event: { direction: "roll_backward", point: { slot: 1 } },
      };
      await expect(
        source.next(orphanCursor, [orphan2, point1]),
      ).resolves.toMatchObject(rollback);
      // The rollback was not recorded: the orphaned cursor is handed back.
      await expect(
        source.next(orphanCursor, [orphan2, point1]),
      ).resolves.toMatchObject(rollback);
      expect(node.sockets()).toBe(2);
      // Once recorded, the handshake echo of the intersection is suppressed.
      await expect(
        source.next({ sequence: 2, point: point1, rollbackGeneration: 1 }, [
          point1,
          orphan2,
        ]),
      ).resolves.toMatchObject({
        event: {
          direction: "roll_forward",
          point: { slot: 2, blockHash: "ee".repeat(32) },
        },
      });
      expect(node.sockets()).toBe(2);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("keeps one Ogmios session while its caller idles at the tip", async () => {
    const node = fakeOgmiosNode([
      { slot: 1, id: "aa".repeat(32) },
      { slot: 2, id: "bb".repeat(32) },
    ]);
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const atTip = {
        sequence: 1,
        point: externalPoint("chain-sync:node-a", 2, "bb"),
        rollbackGeneration: 0,
      };
      const source = new OgmiosChainSyncEventSource(
        "ws://ogmios.local",
        "Preview",
        "node-a",
      );
      await expect(source.next(atTip)).resolves.toEqual({
        tip: expect.objectContaining({ slot: 2 }),
      });
      await expect(source.next(atTip)).resolves.toEqual({
        tip: expect.objectContaining({ slot: 2 }),
      });
      expect(node.sockets()).toBe(1);
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
    const currentStalePoint = vi.fn(async () => stalePoint);
    const provider = new LocalNodeStateQueueProvider(
      authority,
      [
        {
          fetchStateQueueNodes: async () => [],
          fetchStateQueueSnapshot: async () => ({
            nodes: [],
            confirmedHeaderHash: "00".repeat(28),
            confirmedStateOutRef: `${"00".repeat(32)}#0`,
            observedChainPoint: stalePoint,
          }),
          currentChainPoint: currentStalePoint,
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
    expect(currentStalePoint).toHaveBeenCalledTimes(
      LOCAL_NODE_SNAPSHOT_ATTEMPTS,
    );
  });

  it("retakes a local query snapshot when a block lands during the read", async () => {
    const dir = await tempDir();
    const canonical = externalPoint("chain-sync:node-a", 20, "ab");
    let synchronized = false;
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      {
        next: async () => {
          if (synchronized) return { tip: canonical };
          synchronized = true;
          return {
            event: { direction: "roll_forward", point: canonical },
            tip: canonical,
          };
        },
      },
      new FileChainSyncCursorStore(`${dir}/cursor.json`, "11".repeat(32)),
    );
    const queryPoint = { ...canonical, providerSource: "query:node-a:0" };
    const moved = { ...queryPoint, slot: 21, blockHash: "cd".repeat(32) };
    // The first read sees the next block arrive before its closing check.
    const points = [queryPoint, moved];
    const currentChainPoint = vi.fn(async () => points.shift() ?? queryPoint);
    const fetchStateQueueSnapshot = vi.fn(async () => ({
      nodes: [],
      confirmedHeaderHash: "00".repeat(28),
      confirmedStateOutRef: `${"00".repeat(32)}#0`,
      observedChainPoint: queryPoint,
    }));
    const provider = new LocalNodeStateQueueProvider(
      authority,
      [
        {
          fetchStateQueueNodes: async () => [],
          fetchStateQueueSnapshot,
          currentChainPoint,
        },
      ],
      ["query:node-a:0"],
      new FileChainSyncConsumerCursorStore(
        `${dir}/consumer.json`,
        "11".repeat(32),
      ),
    );

    await expect(provider.fetchStateQueueSnapshot()).resolves.toMatchObject({
      confirmedHeaderHash: "00".repeat(28),
    });
    expect(fetchStateQueueSnapshot).toHaveBeenCalledTimes(2);
  });

  it("reads a local query snapshot at one chain-sync cursor, retaking it when the authority rolled back and re-adopted the same point", async () => {
    const dir = await tempDir();
    const blocks = [1, 2, 3, 4, 5].map((slot) => blockAt(slot));
    const chain = { blocks: [...blocks] };
    await writeFollowedJournal(`${dir}/cursor.json`, blocks);
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      followingSource(() => chain.blocks),
      new FileChainSyncCursorStore(`${dir}/cursor.json`, "11".repeat(32)),
    );
    let reads = 0;
    const fetchStateQueueSnapshot = vi.fn(async () => {
      reads += 1;
      if (reads === 1) {
        // While the first read runs, the tip block is rolled back and then
        // adopted again: the chain point is unchanged, the cursor is not.
        chain.blocks = blocks.slice(0, 4);
        await authority.synchronizeToTip();
        chain.blocks = [...blocks];
        await authority.synchronizeToTip();
      }
      return {
        nodes: [],
        confirmedHeaderHash: "00".repeat(28),
        confirmedStateOutRef: `${"00".repeat(32)}#0`,
        observedChainPoint: {
          ...nodePoint(blocks[4]!),
          providerSource: "query:node-a:0",
        },
      };
    });
    const provider = new LocalNodeStateQueueProvider(
      authority,
      [
        {
          fetchStateQueueNodes: async () => [],
          fetchStateQueueSnapshot,
          currentChainPoint: async () => ({
            ...(await authority.currentPoint()),
            providerSource: "query:node-a:0",
          }),
        },
      ],
      ["query:node-a:0"],
      new FileChainSyncConsumerCursorStore(
        `${dir}/consumer.json`,
        "11".repeat(32),
      ),
    );

    const snapshot = await provider.fetchStateQueueSnapshot();
    expect(fetchStateQueueSnapshot).toHaveBeenCalledTimes(2);
    expect(snapshot.chainSyncCursor).toEqual(await authority.currentCursor());
    expect(snapshot.chainSyncCursor).toMatchObject({
      sequence: 6,
      point: nodePoint(blocks[4]!),
      rollbackGeneration: 1,
    });
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
          fetchStateQueueSnapshot: async () => ({
            nodes: [
              {
                ...first,
                chainPoint: { ...first.chainPoint, finalized: true },
              },
            ],
            confirmedHeaderHash: "00".repeat(28),
            confirmedStateOutRef: `${"00".repeat(32)}#0`,
            observedChainPoint: queryPoint("query:kupo-a"),
          }),
          currentChainPoint: async () => queryPoint("query:kupo-a"),
        },
        {
          fetchStateQueueNodes: async () => [
            {
              ...second,
              chainPoint: { ...second.chainPoint, finalized: false },
            },
          ],
          fetchStateQueueSnapshot: async () => ({
            nodes: [
              {
                ...second,
                chainPoint: { ...second.chainPoint, finalized: false },
              },
            ],
            confirmedHeaderHash: "00".repeat(28),
            confirmedStateOutRef: `${"00".repeat(32)}#0`,
            observedChainPoint: queryPoint("query:db-sync-a"),
          }),
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

  it("hands a signature record a chain point from a recorded preprod chain-sync session that the strict parser accepts", async () => {
    const dir = await tempDir();
    // The authority's point is what `parseOgmiosPoint` made of a live Ogmios
    // v7.0.0 bootstrap — a `queryNetwork/tip` with no height, then the
    // `findIntersection` answer whose tip carries one — not a hand-built point.
    const replay = recordedOgmiosWebSocket(loadL1Recording(FOLLOW_TIP));
    vi.stubGlobal("WebSocket", replay.WebSocket);
    try {
      const authority = recordedPreprodAuthority(`${dir}/cursor.json`);
      const { header, headerHash } = await makePayloadFixture();
      const node = makeObservedNode({ header, headerHash });
      const queryPoint = async (): Promise<CanonicalChainPoint> => ({
        ...(await authority.currentPoint()),
        providerSource: "query:node-a:0",
      });
      const provider = new LocalNodeStateQueueProvider(
        authority,
        [
          {
            fetchStateQueueNodes: () => {
              throw new Error("the snapshot is read, not the node list");
            },
            // A query surface at the authority's point hands back points of
            // the authority's own, wider type: fields no record declares.
            fetchStateQueueSnapshot: async () => ({
              nodes: [
                {
                  ...node,
                  chainPoint: {
                    ...(await authority.currentPoint()),
                    depth: 10,
                  },
                },
              ],
              confirmedHeaderHash: "00".repeat(28),
              confirmedStateOutRef: `${"00".repeat(32)}#0`,
              observedChainPoint: await queryPoint(),
            }),
            currentChainPoint: queryPoint,
          },
        ],
        ["query:node-a:0"],
        new FileChainSyncConsumerCursorStore(
          `${dir}/consumer.json`,
          "11".repeat(32),
        ),
      );

      const snapshot = await provider.fetchStateQueueSnapshot();
      // The scanner hands `node.chainPoint` to the header record unchanged as
      // `observedChainPoint`, and signing copies that into `l1ChainPoint`.
      const l1ChainPoint = snapshot.nodes[0]!.chainPoint;
      const signature = daSignatureRecordAt({
        l1ChainPoint,
        headerHash,
        stateQueueOutRef: node.outRef,
      });
      const [bootstrapTip] = recordedTips(loadL1Recording(FOLLOW_TIP));
      expect(parseDaSignatureRecord(signature).l1ChainPoint).toEqual({
        slot: bootstrapTip!.slot,
        blockHash: bootstrapTip!.id,
        depth: 10,
        providerSource: "chain-sync:node-a,query:node-a:0",
        observedAt: l1ChainPoint.observedAt,
      });
      // The snapshot's own observed point is a chain point too.
      expect(() =>
        parseDaSignatureRecord({
          ...signature,
          l1ChainPoint: snapshot.observedChainPoint,
        }),
      ).not.toThrow();
      // Handed over as the authority holds it, the point is refused.
      const canonical = await authority.currentPoint();
      expect(() =>
        parseDaSignatureRecord({ ...signature, l1ChainPoint: canonical }),
      ).toThrow("chain point contains unknown field network");
      // As is the height Ogmios put on the tip, had it been copied through.
      expect(() =>
        parseDaSignatureRecord({
          ...signature,
          l1ChainPoint: { ...l1ChainPoint, height: 5_220_971 },
        }),
      ).toThrow("chain point contains unknown field height");
    } finally {
      vi.unstubAllGlobals();
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

  describe("Kupmios current tip", () => {
    // No height: a live Ogmios v7.0.0 answers `queryNetwork/tip` with the slot
    // and id alone (see "recorded preprod Ogmios and Kupo"). The re-reads Kupo
    // needs to catch up cannot be recorded, so the answers here stay doubles.
    const ogmiosTip = { slot: 101, id: "44".repeat(32) };
    class TipWebSocket {
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
          request.method === "queryNetwork/tip"
            ? ogmiosTip
            : { networkMagic: 2 };
        queueMicrotask(() =>
          this.onmessage?.({
            data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
          }),
        );
      }

      close(): void {}
    }
    /** Kupo answers the given checkpoints in turn, then repeats the last. */
    const kupoCheckpoints = (...slots: readonly number[]) => {
      let call = 0;
      return vi.fn(async () => {
        const slot = slots[Math.min(call++, slots.length - 1)]!;
        const hash = slot === ogmiosTip.slot ? ogmiosTip.id : "33".repeat(32);
        return new Response(`kupo_most_recent_checkpoint ${slot}\n`, {
          headers: { etag: `"${hash}"` },
        });
      });
    };
    const readTip = async (fetchFn: ReturnType<typeof kupoCheckpoints>) => {
      vi.stubGlobal("WebSocket", TipWebSocket);
      vi.stubGlobal("fetch", fetchFn);
      try {
        return await kupmiosCurrentChainPointResolver(
          "Preview",
          "http://kupo.local",
          "ws://ogmios.local",
        )();
      } finally {
        vi.unstubAllGlobals();
      }
    };

    it("re-reads while Kupo catches up to the Ogmios tip", async () => {
      const fetchFn = kupoCheckpoints(100, 100, 101);
      const point = await readTip(fetchFn);
      expect(point).toMatchObject({ slot: 101, blockHash: "44".repeat(32) });
      expect(point).not.toHaveProperty("blockHeight");
      expect(fetchFn).toHaveBeenCalledTimes(3);
    });

    it("refuses surfaces that stay on different chain points, as an observation failure", async () => {
      const fetchFn = kupoCheckpoints(100);
      const failure = await readTip(fetchFn).then(
        () => undefined,
        (error: unknown) => error,
      );
      expect((failure as Error).message).toMatch(
        /not aligned after \d+ reads: Kupo=100:3{64}, Ogmios=101:4{64}/u,
      );
      expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
      expect(fetchFn).toHaveBeenCalledTimes(KUPMIOS_TIP_ALIGNMENT_ATTEMPTS);
    });
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
    const provider = new MultiStateQueueProvider([first, second], {
      sourceMode: "external_providers",
    });

    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /provider disagreement/,
    );
  });

  it("fails closed on provider chain-point disagreement", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({ header, headerHash, depth: 10 });
    const provider = new MultiStateQueueProvider(
      [
        {
          fetchStateQueueNodes: async () => [node],
          currentChainPoint: async () => externalPoint("provider-a"),
        },
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
          currentChainPoint: async () => externalPoint("provider-b"),
        },
      ],
      { sourceMode: "external_providers" },
    );

    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /disagreement/,
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
      {
        sourceMode: "external_providers",
        identities: ["provider-a", "provider-b"],
      },
    );

    await expect(provider.fetchStateQueueNodes()).rejects.toThrow(
      /current chain-point disagreement/u,
    );
  });

  describe("L1 observation versus integrity failures", () => {
    /** A local-node authority whose chain-sync source replays `pending`. */
    const scriptedAuthority = (dir: string, first: CanonicalChainPoint) => {
      const pending: {
        direction: "roll_forward" | "roll_backward";
        point: CanonicalChainPoint;
      }[] = [{ direction: "roll_forward", point: first }];
      let tip = first;
      const authority = new LocalNodeChainAuthority(
        "node-a",
        "Preview",
        {
          next: async () => {
            const event = pending.shift();
            if (event === undefined) return { tip };
            tip = event.point;
            return { event, tip: event.point };
          },
        },
        new FileChainSyncCursorStore(`${dir}/cursor.json`, "11".repeat(32)),
      );
      return {
        authority,
        advance: (point: CanonicalChainPoint) =>
          pending.push({ direction: "roll_forward", point }),
        rollBack: (point: CanonicalChainPoint) =>
          pending.push({ direction: "roll_backward", point }),
      };
    };
    const emptySnapshot = (point: CanonicalChainPoint) => ({
      nodes: [],
      confirmedHeaderHash: "00".repeat(28),
      confirmedStateOutRef: `${"00".repeat(32)}#0`,
      observedChainPoint: point,
    });
    const consumerStore = (dir: string) =>
      new FileChainSyncConsumerCursorStore(
        `${dir}/consumer.json`,
        "11".repeat(32),
      );
    const disagreeingHistories = [
      async () => [],
      async () => [{ checkpointKind: "merge" } as never],
    ];

    it("treats an exhausted local snapshot retake as an observation failure", async () => {
      const dir = await tempDir();
      const canonical = externalPoint("chain-sync:node-a", 20, "ab");
      const { authority } = scriptedAuthority(dir, canonical);
      const queryPoint = { ...canonical, providerSource: "query:node-a:0" };
      const moved = { ...queryPoint, slot: 21, blockHash: "cd".repeat(32) };
      // Every read sees a block land before its closing check.
      let reads = 0;
      const currentChainPoint = vi.fn(async () =>
        reads++ % 2 === 0 ? queryPoint : moved,
      );
      const provider = new LocalNodeStateQueueProvider(
        authority,
        [
          {
            fetchStateQueueNodes: async () => [],
            fetchStateQueueSnapshot: async () => emptySnapshot(queryPoint),
            currentChainPoint,
          },
        ],
        ["query:node-a:0"],
        consumerStore(dir),
      );

      const failure = await provider.fetchStateQueueSnapshot().then(
        () => undefined,
        (error: unknown) => error,
      );
      expect(failure).toBeInstanceOf(Error);
      expect((failure as Error).message).toMatch(
        /changed chain point while its snapshot was read/u,
      );
      expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
      expect(currentChainPoint).toHaveBeenCalledTimes(
        2 * LOCAL_NODE_SNAPSHOT_ATTEMPTS,
      );
    });

    it("retakes the snapshot when the Kupmios chain moves during block-confirmation derivation", async () => {
      const txHash = "aa".repeat(32);
      const inclusion = { slot: 10, id: "11".repeat(32) };
      const tip = { slot: 100, id: "44".repeat(32), height: 4 };
      const nextTip = { slot: 101, id: "55".repeat(32), height: 5 };
      const descendants = [
        { slot: 20, id: "22".repeat(32) },
        { slot: 50, id: "33".repeat(32) },
        tip,
      ];
      let tipQueries = 0;
      let nextBlockIndex = 0;
      class MovingTipWebSocket {
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
            // A block arrives after the opening tip read.
            result = tipQueries++ === 0 ? tip : nextTip;
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
              data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
            }),
          );
        }

        close(): void {}
      }
      let kupoReads = 0;
      const fetchFn = vi.fn(async () => {
        const at = kupoReads++ === 0 ? tip : nextTip;
        return new Response(`kupo_most_recent_checkpoint ${at.slot}\n`, {
          headers: { etag: `"${at.id}"` },
        });
      });
      vi.stubGlobal("WebSocket", MovingTipWebSocket);
      let moved: unknown;
      try {
        moved = await kupmiosChainPointResolver(
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
        )({ txHash, outputIndex: 1 } as never).then(
          () => undefined,
          (error: unknown) => error,
        );
      } finally {
        vi.unstubAllGlobals();
      }
      expect(moved).toBeInstanceOf(Error);
      expect((moved as Error).message).toBe(
        "Kupmios chain point changed while deriving block confirmations",
      );
      expect(moved).not.toBeInstanceOf(L1SourceIntegrityError);

      // Raised from a local-node query surface, the snapshot is retaken.
      const dir = await tempDir();
      const canonical = externalPoint("chain-sync:node-a", 20, "ab");
      const { authority } = scriptedAuthority(dir, canonical);
      const queryPoint = { ...canonical, providerSource: "query:node-a:0" };
      let snapshotReads = 0;
      const fetchStateQueueSnapshot = vi.fn(async () => {
        if (snapshotReads++ === 0) throw moved;
        return emptySnapshot(queryPoint);
      });
      const provider = new LocalNodeStateQueueProvider(
        authority,
        [
          {
            fetchStateQueueNodes: async () => [],
            fetchStateQueueSnapshot,
            currentChainPoint: async () => queryPoint,
          },
        ],
        ["query:node-a:0"],
        consumerStore(dir),
      );
      await expect(provider.fetchStateQueueSnapshot()).resolves.toMatchObject({
        confirmedHeaderHash: "00".repeat(28),
      });
      expect(fetchStateQueueSnapshot).toHaveBeenCalledTimes(2);
    });

    // Per replay attempt: whether the chain moves under it (a block, or a
    // rollback), and whether the two surfaces' histories disagree.
    type ReplayAttempt = {
      readonly move?: "forward" | "rollback";
      readonly disagree: boolean;
    };
    it.each<
      readonly [
        string,
        readonly ReplayAttempt[],
        "integrity" | "observation" | "replayed",
      ]
    >([
      ["the chain held", [{ disagree: true }], "integrity"],
      [
        "a block landed during the first attempt only",
        [{ move: "forward", disagree: true }, { disagree: true }],
        "integrity",
      ],
      [
        "a block landed during the first attempt, whose retake agreed",
        [{ move: "forward", disagree: true }, { disagree: false }],
        "replayed",
      ],
      [
        "a block landed during every attempt",
        [
          { move: "forward", disagree: true },
          { move: "forward", disagree: true },
          { move: "forward", disagree: true },
        ],
        "observation",
      ],
      [
        "a rollback landed during the first attempt",
        [
          { move: "rollback", disagree: true },
          { disagree: true },
          { disagree: true },
        ],
        "observation",
      ],
    ])(
      "classifies a local-node replay disagreement when %s",
      async (_label, attempts, outcome) => {
        const dir = await tempDir();
        const canonical = externalPoint("chain-sync:node-a", 20, "ab");
        const { authority, advance, rollBack } = scriptedAuthority(
          dir,
          canonical,
        );
        let attempt = -1;
        let slot = 20;
        const surface = (identity: string, index: number) => ({
          fetchStateQueueNodes: async () => [],
          fetchStateQueueSnapshot: async () =>
            emptySnapshot({ ...canonical, providerSource: identity }),
          currentChainPoint: async () => ({
            ...canonical,
            providerSource: identity,
          }),
          fetchStateQueueReplayCheckpoints: async (
            _anchor: unknown,
            _current: unknown,
            tipBlockNo: number,
          ) => {
            expect(tipBlockNo).toBe(7);
            if (index === 0) {
              attempt += 1;
              const move = attempts[attempt]?.move;
              if (move === "forward") {
                slot += 1;
                advance(externalPoint("chain-sync:node-a", slot, "cd"));
              } else if (move === "rollback") {
                rollBack(externalPoint("chain-sync:node-a", 19, "ef"));
              }
            }
            return attempts[attempt]?.disagree === true
              ? disagreeingHistories[index]!()
              : disagreeingHistories[0]!();
          },
        });
        const provider = new LocalNodeStateQueueProvider(
          authority,
          [surface("query:node-a:0", 0), surface("query:node-a:1", 1)],
          ["query:node-a:0", "query:node-a:1"],
          consumerStore(dir),
        );
        await provider.fetchStateQueueSnapshot();

        const failure = await provider
          .fetchStateQueueReplayCheckpoints([], [], 7, 64)
          .then(
            () => undefined,
            (error: unknown) => error,
          );
        expect(attempt + 1).toBe(attempts.length);
        if (outcome === "replayed") {
          expect(failure).toBeUndefined();
          return;
        }
        expect((failure as Error).message).toMatch(
          /local-node state-queue replay disagreement/u,
        );
        if (outcome === "observation") {
          expect((failure as Error).message).toMatch(
            new RegExp(
              `^chain moved while state-queue history was replayed, ${STATE_QUEUE_REPLAY_ATTEMPTS.toString()} times`,
              "u",
            ),
          );
          expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
        } else {
          expect(failure).toBeInstanceOf(L1SourceIntegrityError);
        }
      },
    );

    it("asks every local-node query surface for at most the caller's limit", async () => {
      const dir = await tempDir();
      const canonical = externalPoint("chain-sync:node-a", 20, "ab");
      const { authority } = scriptedAuthority(dir, canonical);
      const limits: number[] = [];
      const surface = (identity: string) => ({
        fetchStateQueueNodes: async () => [],
        fetchStateQueueSnapshot: async () =>
          emptySnapshot({ ...canonical, providerSource: identity }),
        currentChainPoint: async () => ({
          ...canonical,
          providerSource: identity,
        }),
        fetchStateQueueReplayCheckpoints: async (
          _anchor: unknown,
          _current: unknown,
          _tipBlockNo: number,
          limit: number,
        ) => {
          limits.push(limit);
          return [];
        },
      });
      const provider = new LocalNodeStateQueueProvider(
        authority,
        [surface("query:node-a:0"), surface("query:node-a:1")],
        ["query:node-a:0", "query:node-a:1"],
        consumerStore(dir),
      );
      await provider.fetchStateQueueSnapshot();
      await expect(
        provider.fetchStateQueueReplayCheckpoints([], [], 7, 13),
      ).resolves.toEqual([]);
      expect(limits).toEqual([13, 13]);
    });

    it("treats a local-node query surface without a replay source as an observation failure", async () => {
      const dir = await tempDir();
      const canonical = externalPoint("chain-sync:node-a", 20, "ab");
      const { authority } = scriptedAuthority(dir, canonical);
      const provider = new LocalNodeStateQueueProvider(
        authority,
        [
          {
            fetchStateQueueNodes: async () => [],
            fetchStateQueueSnapshot: async () =>
              emptySnapshot({ ...canonical, providerSource: "query:node-a:0" }),
            currentChainPoint: async () => ({
              ...canonical,
              providerSource: "query:node-a:0",
            }),
          },
        ],
        ["query:node-a:0"],
        consumerStore(dir),
      );
      await provider.fetchStateQueueSnapshot();
      const failure = await provider
        .fetchStateQueueReplayCheckpoints([], [], 7, 64)
        .then(
          () => undefined,
          (error: unknown) => error,
        );
      expect((failure as Error).message).toMatch(
        /has no authenticated ordered history source/u,
      );
      expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
    });

    it.each([
      ["still holds", true, "integrity", 2],
      ["no longer holds", false, "observation", STATE_QUEUE_REPLAY_ATTEMPTS],
      [
        "cannot tell whether it holds",
        undefined,
        "observation",
        STATE_QUEUE_REPLAY_ATTEMPTS,
      ],
    ] as const)(
      "retakes an external replay from the new points only when the chain %s the snapshot's",
      async (_label, holds, outcome, expectedReplays) => {
        // A block lands during the first attempt only; every attempt
        // disagrees.
        let slot = 100;
        let replays = 0;
        const held: CanonicalChainPoint[] = [];
        const provider = new MultiStateQueueProvider(
          (["provider-a", "provider-b"] as const).map((identity, index) => ({
            fetchStateQueueNodes: async () => [],
            fetchStateQueueSnapshot: async () =>
              emptySnapshot(externalPoint(identity)),
            currentChainPoint: async () =>
              externalPoint(identity, slot, slot === 100 ? "ab" : "cd"),
            ...(holds === undefined
              ? {}
              : {
                  holdsChainPoint: async (point: CanonicalChainPoint) => {
                    held.push(point);
                    return holds;
                  },
                }),
            fetchStateQueueReplayCheckpoints: async (
              _anchor: unknown,
              _current: unknown,
              _tipBlockNo: number,
              limit: number,
            ) => {
              expect(limit).toBe(64);
              if (index === 0) {
                replays += 1;
                if (replays === 1) slot = 101;
              }
              return disagreeingHistories[index]!();
            },
          })),
          {
            sourceMode: "external_providers",
            identities: ["provider-a", "provider-b"],
          },
        );
        await provider.fetchStateQueueSnapshot();

        const failure = await provider
          .fetchStateQueueReplayCheckpoints([], [], 7, 64)
          .then(
            () => undefined,
            (error: unknown) => error,
          );
        expect((failure as Error).message).toMatch(
          /state-queue replay disagreement between provider-a and provider-b/u,
        );
        expect(replays).toBe(expectedReplays);
        if (holds !== undefined) {
          // Each provider was asked about its own snapshot point.
          expect(
            held
              .slice(0, 2)
              .map(({ providerSource, slot: at }) => [providerSource, at]),
          ).toEqual([
            ["provider-a", 100],
            ["provider-b", 100],
          ]);
        }
        if (outcome === "integrity") {
          expect(failure).toBeInstanceOf(L1SourceIntegrityError);
        } else {
          expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
        }
      },
    );

    it("retakes an external replay from the snapshot's points when any provider no longer holds its snapshot point", async () => {
      // A block lands during the first attempt only; every attempt
      // disagrees. Provider-b no longer holds its snapshot's point, so no
      // retake may be watched from the new points.
      let slot = 100;
      let replays = 0;
      const provider = new MultiStateQueueProvider(
        (["provider-a", "provider-b"] as const).map((identity, index) => ({
          fetchStateQueueNodes: async () => [],
          fetchStateQueueSnapshot: async () =>
            emptySnapshot(externalPoint(identity)),
          currentChainPoint: async () =>
            externalPoint(identity, slot, slot === 100 ? "ab" : "cd"),
          holdsChainPoint: async () => index === 0,
          fetchStateQueueReplayCheckpoints: async () => {
            if (index === 0) {
              replays += 1;
              if (replays === 1) slot = 101;
            }
            return disagreeingHistories[index]!();
          },
        })),
        {
          sourceMode: "external_providers",
          identities: ["provider-a", "provider-b"],
        },
      );
      await provider.fetchStateQueueSnapshot();
      const failure = await provider
        .fetchStateQueueReplayCheckpoints([], [], 7, 64)
        .then(
          () => undefined,
          (error: unknown) => error,
        );
      expect((failure as Error).message).toMatch(
        /state-queue replay disagreement between provider-a and provider-b/u,
      );
      expect(replays).toBe(STATE_QUEUE_REPLAY_ATTEMPTS);
      expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
    });

    it("refuses at startup a live provider without a snapshot and replay source, but not a fixture", () => {
      const snapshotOnly: StateQueueProvider = {
        fetchStateQueueNodes: async () => [],
        fetchStateQueueSnapshot: async () =>
          emptySnapshot(externalPoint("provider-a")),
      };
      const nodesOnly: StateQueueProvider = {
        fetchStateQueueNodes: async () => [],
        fetchStateQueueReplayCheckpoints: async () => [],
      };
      for (const provider of [snapshotOnly, nodesOnly]) {
        expect(() =>
          requireStateQueueReplaySource(
            "kupmios:http://kupo.test|ws://ogmios.test#secret",
            provider,
          ),
        ).toThrow(
          "state-queue provider kupmios:http://kupo.test has no authenticated ordered history source",
        );
      }
      const capable = { ...snapshotOnly, ...nodesOnly };
      expect(
        requireStateQueueReplaySource("kupmios:http://kupo.test", capable),
      ).toBe(capable);
      for (const url of ["fixture:/tmp/queue.json", "file:///tmp/queue.json"]) {
        expect(requireStateQueueReplaySource(url, snapshotOnly)).toBe(
          snapshotOnly,
        );
      }
    });

    it("treats an external provider without a replay source as an observation failure", async () => {
      const provider = new MultiStateQueueProvider(
        (["provider-a", "provider-b"] as const).map((identity, index) => ({
          fetchStateQueueNodes: async () => [],
          fetchStateQueueSnapshot: async () =>
            emptySnapshot(externalPoint(identity)),
          currentChainPoint: async () => externalPoint(identity),
          ...(index === 0
            ? { fetchStateQueueReplayCheckpoints: async () => [] }
            : {}),
        })),
        {
          sourceMode: "external_providers",
          identities: ["provider-a", "provider-b"],
        },
      );
      await provider.fetchStateQueueSnapshot();
      const failure = await provider
        .fetchStateQueueReplayCheckpoints([], [], 7, 64)
        .then(
          () => undefined,
          (error: unknown) => error,
        );
      expect((failure as Error).message).toBe(
        "state-queue provider provider-b has no authenticated ordered history source",
      );
      expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
    });

    it.each([
      ["held", false],
      ["moved", true],
    ] as const)(
      "keeps an external replay disagreement as integrity only while the chain %s",
      async (_label, chainMoves) => {
        let slot = 100;
        let replays = 0;
        const provider = new MultiStateQueueProvider(
          (["provider-a", "provider-b"] as const).map((identity, index) => ({
            fetchStateQueueNodes: async () => [],
            fetchStateQueueSnapshot: async () =>
              emptySnapshot(externalPoint(identity)),
            currentChainPoint: async () =>
              externalPoint(identity, slot, slot === 100 ? "ab" : "cd"),
            fetchStateQueueReplayCheckpoints: async (
              _anchor: unknown,
              _current: unknown,
              tipBlockNo: number,
              limit: number,
            ) => {
              expect(tipBlockNo).toBe(7);
              expect(limit).toBe(64);
              if (index === 0) replays += 1;
              if (chainMoves) slot = 101;
              return disagreeingHistories[index]!();
            },
          })),
          {
            sourceMode: "external_providers",
            identities: ["provider-a", "provider-b"],
          },
        );
        await provider.fetchStateQueueSnapshot();

        const failure = await provider
          .fetchStateQueueReplayCheckpoints([], [], 7, 64)
          .then(
            () => undefined,
            (error: unknown) => error,
          );
        expect((failure as Error).message).toMatch(
          /state-queue replay disagreement between provider-a and provider-b/u,
        );
        if (chainMoves) {
          // These providers cannot show the snapshot survived, so every
          // retake is watched from it and the moved chain excuses them all.
          expect(replays).toBe(STATE_QUEUE_REPLAY_ATTEMPTS);
          expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
        } else {
          expect(replays).toBe(1);
          expect(failure).toBeInstanceOf(L1SourceIntegrityError);
        }
      },
    );

    it.each([
      ["one tip height", [42, 42], 42],
      ["different tip heights", [42, 43], undefined],
    ] as const)(
      "merges external snapshots read at %s",
      async (_label, heights, merged) => {
        const provider = new MultiStateQueueProvider(
          (["provider-a", "provider-b"] as const).map((identity, index) => ({
            fetchStateQueueNodes: async () => [],
            fetchStateQueueSnapshot: async () => ({
              ...emptySnapshot(externalPoint(identity)),
              tipBlockNo: heights[index]!,
            }),
            currentChainPoint: async () => externalPoint(identity),
          })),
          {
            sourceMode: "external_providers",
            identities: ["provider-a", "provider-b"],
          },
        );
        const snapshot = provider.fetchStateQueueSnapshot();
        if (merged === undefined) {
          await expect(snapshot).rejects.toThrow(
            /read their state-queue snapshots at different tip heights/u,
          );
          await expect(snapshot).rejects.not.toBeInstanceOf(
            L1SourceIntegrityError,
          );
        } else {
          await expect(snapshot).resolves.toMatchObject({ tipBlockNo: merged });
        }
      },
    );

    it("classifies surface disagreement at one proven chain point as integrity", async () => {
      const { header, headerHash } = await makePayloadFixture();
      const provider = new MultiStateQueueProvider(
        [
          {
            fetchStateQueueNodes: async () => [
              makeObservedNode({ header, headerHash, depth: 10 }),
            ],
            currentChainPoint: async () => externalPoint("provider-a"),
          },
          {
            fetchStateQueueNodes: async () => [],
            currentChainPoint: async () => externalPoint("provider-b"),
          },
        ],
        { sourceMode: "external_providers" },
      );
      await expect(provider.fetchStateQueueNodes()).rejects.toBeInstanceOf(
        L1SourceIntegrityError,
      );
    });
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

const journalLine = (sequence: number, point: CanonicalChainPoint): string =>
  JSON.stringify({
    sequence,
    event: { direction: "roll_forward", point },
    cursor: { sequence, point, rollbackGeneration: 0 },
  });

/** Appends `count` roll-forward events through the real store. */
const seedChainSyncJournal = async (
  cursorPath: string,
  count: number,
): Promise<readonly CanonicalChainPoint[]> => {
  const store = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
  const points: CanonicalChainPoint[] = [];
  for (let sequence = 0; sequence < count; sequence += 1) {
    const point = externalPoint(
      "chain-sync:node-a",
      sequence + 1,
      (sequence + 1).toString(16).padStart(2, "0"),
    );
    await store.append(
      { direction: "roll_forward", point },
      { sequence, point, rollbackGeneration: 0 },
    );
    points.push(point);
  }
  return points;
};

type FakeOgmiosBlock = { readonly slot: number; readonly id: string };

/**
 * A stateful Ogmios chain-sync double over a fixed chain: each socket keeps
 * its own read pointer, set only by findIntersection (which also queues the
 * handshake rollback echo) and advanced by every nextBlock it answers.
 */
const fakeOgmiosNode = (chain: readonly FakeOgmiosBlock[]) => {
  let sockets = 0;
  const intersections: unknown[][] = [];
  const tip = chain.at(-1)!;
  class FakeOgmiosWebSocket {
    onopen: ((event: unknown) => void) | null = null;
    onmessage: ((event: { readonly data: unknown }) => void) | null = null;
    onerror: ((event: unknown) => void) | null = null;
    onclose: ((event: unknown) => void) | null = null;
    private readPointer = -1;
    private handshakeEcho: FakeOgmiosBlock | "origin" | undefined;

    constructor(_url: string) {
      sockets += 1;
      queueMicrotask(() => this.onopen?.({}));
    }

    send(raw: string): void {
      const request = JSON.parse(raw) as {
        readonly id: string;
        readonly method: string;
        readonly params: Record<string, unknown>;
      };
      let result: unknown;
      if (request.method === "queryNetwork/genesisConfiguration") {
        result = { networkMagic: 2 };
      } else if (request.method === "queryNetwork/tip") {
        result = tip;
      } else if (request.method === "findIntersection") {
        const points = request.params.points as readonly (
          | FakeOgmiosBlock
          | "origin"
        )[];
        intersections.push([...points]);
        this.readPointer = -1;
        this.handshakeEcho = "origin";
        for (const point of points) {
          if (point === "origin") {
            break;
          }
          const index = chain.findIndex(
            ({ slot, id }) => slot === point.slot && id === point.id,
          );
          if (index >= 0) {
            this.readPointer = index;
            this.handshakeEcho = chain[index]!;
            break;
          }
        }
        result = { intersection: this.handshakeEcho, tip };
      } else if (this.handshakeEcho !== undefined) {
        result = { direction: "backward", point: this.handshakeEcho, tip };
        this.handshakeEcho = undefined;
      } else {
        this.readPointer += 1;
        const block = chain[this.readPointer];
        if (block === undefined) {
          throw new Error("fake Ogmios node has no block past its tip");
        }
        result = { direction: "forward", block, tip };
      }
      queueMicrotask(() =>
        this.onmessage?.({
          data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
        }),
      );
    }

    close(): void {}
  }
  return {
    WebSocket: FakeOgmiosWebSocket,
    sockets: () => sockets,
    intersections: () => intersections,
  };
};

const localNodeSource = {
  sourceMode: "local_node",
  authorityNodeId: "phase4-cardano-node",
  authorityDigest: "aa".repeat(32),
  networkMagic: 424242,
} as const;

const sha256 = (value: string): string =>
  createHash("sha256").update(value).digest("hex");

const blockAt = (slot: number, fork = 0): FakeOgmiosBlock => ({
  slot,
  id: `${fork.toString(16).padStart(2, "0")}${slot.toString(16).padStart(62, "0")}`,
});

const nodePoint = ({ slot, id }: FakeOgmiosBlock): CanonicalChainPoint => ({
  network: "Preview",
  slot,
  blockHash: id,
  providerSource: "chain-sync:node-a",
  observedAt: "2026-07-28T00:00:00.000Z",
});

/**
 * An Ogmios chain-sync double that behaves like a live node: a nextBlock at
 * the tip is held until the node adopts another block, and a rollback is
 * delivered to each follower as a backward response before the new blocks.
 */
const liveOgmiosNode = (initial: readonly FakeOgmiosBlock[]) => {
  const chain = [...initial];
  const followers = new Set<LiveOgmiosWebSocket>();
  let nextBlocks = 0;
  let heldAtTip = 0;
  const intersections: unknown[][] = [];
  const tip = () => chain.at(-1)!;
  class LiveOgmiosWebSocket {
    onopen: ((event: unknown) => void) | null = null;
    onmessage: ((event: { readonly data: unknown }) => void) | null = null;
    onerror: ((event: unknown) => void) | null = null;
    onclose: ((event: unknown) => void) | null = null;
    readPointer = -1;
    handshakeEcho: FakeOgmiosBlock | undefined;
    rollbackTo: number | undefined;
    held: string | undefined;

    constructor(_url: string) {
      followers.add(this);
      queueMicrotask(() => this.onopen?.({}));
    }

    respond(id: string, result: unknown): void {
      queueMicrotask(() =>
        this.onmessage?.({
          data: JSON.stringify({ jsonrpc: "2.0", id, result }),
        }),
      );
    }

    /** Answers a nextBlock, or holds it at the tip as a live node does. */
    answerNextBlock(id: string): void {
      if (this.handshakeEcho !== undefined) {
        const point = this.handshakeEcho;
        this.handshakeEcho = undefined;
        this.respond(id, { direction: "backward", point, tip: tip() });
        return;
      }
      if (this.rollbackTo !== undefined) {
        this.readPointer = this.rollbackTo;
        this.rollbackTo = undefined;
        this.respond(id, {
          direction: "backward",
          point: chain[this.readPointer],
          tip: tip(),
        });
        return;
      }
      const block = chain[this.readPointer + 1];
      if (block === undefined) {
        heldAtTip += 1;
        this.held = id;
        return;
      }
      this.readPointer += 1;
      this.respond(id, { direction: "forward", block, tip: tip() });
    }

    send(raw: string): void {
      const request = JSON.parse(raw) as {
        readonly id: string;
        readonly method: string;
        readonly params: Record<string, unknown>;
      };
      if (request.method === "queryNetwork/genesisConfiguration") {
        this.respond(request.id, { networkMagic: 2 });
      } else if (request.method === "queryNetwork/tip") {
        this.respond(request.id, tip());
      } else if (request.method === "findIntersection") {
        const points = request.params.points as readonly (
          | FakeOgmiosBlock
          | "origin"
        )[];
        intersections.push([...points]);
        const index = points
          .filter((point) => point !== "origin")
          .map((point) =>
            chain.findIndex(
              ({ slot, id }) => slot === point.slot && id === point.id,
            ),
          )
          .find((found) => found >= 0);
        if (index === undefined) {
          throw new Error("live Ogmios double only intersects on its chain");
        }
        this.readPointer = index;
        this.rollbackTo = undefined;
        this.handshakeEcho = chain[index]!;
        this.respond(request.id, { intersection: chain[index], tip: tip() });
      } else if (request.method === "nextBlock") {
        nextBlocks += 1;
        this.answerNextBlock(request.id);
      } else {
        throw new Error(`live Ogmios double has no ${request.method}`);
      }
    }

    close(): void {
      followers.delete(this);
    }
  }
  const releaseHeld = () => {
    for (const follower of followers) {
      const held = follower.held;
      if (held !== undefined) {
        follower.held = undefined;
        follower.answerNextBlock(held);
      }
    }
  };
  return {
    WebSocket: LiveOgmiosWebSocket,
    nextBlocks: () => nextBlocks,
    heldAtTip: () => heldAtTip,
    intersections: () => intersections,
    sockets: () => followers.size,
    extend: (block: FakeOgmiosBlock) => {
      chain.push(block);
      releaseHeld();
    },
    rollBackTo: (slot: number) => {
      const index = chain.findIndex((block) => block.slot === slot);
      chain.length = index + 1;
      for (const follower of followers) {
        if (follower.readPointer > index) {
          follower.rollbackTo = Math.min(follower.rollbackTo ?? index, index);
        }
      }
    },
  };
};

/**
 * Writes a chain-sync journal of roll-forward events over `blocks`, with its
 * cursor metadata, as a member that followed them would have left it.
 */
const writeFollowedJournal = async (
  cursorPath: string,
  blocks: readonly FakeOgmiosBlock[],
): Promise<void> => {
  await writeFile(
    `${cursorPath}.events.jsonl`,
    blocks
      .map((block, sequence) => `${journalLine(sequence, nodePoint(block))}\n`)
      .join(""),
  );
  await writeFile(
    cursorPath,
    `${JSON.stringify({
      schemaVersion: 2,
      authorityFingerprint: "11".repeat(32),
      cursor: {
        sequence: blocks.length - 1,
        point: nodePoint(blocks.at(-1)!),
        rollbackGeneration: 0,
      },
    })}\n`,
  );
};

const journalSequences = async (cursorPath: string): Promise<number[]> =>
  (await readFile(`${cursorPath}.events.jsonl`, "utf8"))
    .split("\n")
    .filter((line) => line.length > 0)
    .map((line) => (JSON.parse(line) as { sequence: number }).sequence);

/** Resolves to "blocked" when `work` has not settled within `ms`. */
const settlesWithin = async <T>(
  work: Promise<T>,
  ms: number,
): Promise<T | "blocked"> => {
  work.catch(() => undefined);
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    return await Promise.race([
      work,
      new Promise<"blocked">((resolve) => {
        timer = setTimeout(() => resolve("blocked"), ms);
      }),
    ]);
  } finally {
    clearTimeout(timer);
  }
};

const followingAuthority = (cursorPath: string) =>
  new LocalNodeChainAuthority(
    "node-a",
    "Preview",
    new OgmiosChainSyncEventSource("ws://ogmios.local", "Preview", "node-a"),
    new FileChainSyncCursorStore(cursorPath, "11".repeat(32)),
  );

const consumerOf = (
  authority: LocalNodeChainAuthority,
  cursorPath: string,
  consumerPath = `${cursorPath}.watcher-consumer-v1`,
): LocalNodeStateQueueProvider =>
  new LocalNodeStateQueueProvider(
    authority,
    [
      {
        fetchStateQueueNodes: () => {
          throw new Error("no query surface in a chain-sync test");
        },
        currentChainPoint: () => authority.currentPoint(),
      },
    ],
    ["query:node-a:0"],
    new FileChainSyncConsumerCursorStore(consumerPath, "11".repeat(32)),
  );

describe("restarted local-node chain sync", () => {
  it("answers a sync at the node tip at once instead of holding a nextBlock until the next block", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const blocks = [1, 2, 3, 4, 5].map((slot) => blockAt(slot));
    await writeFollowedJournal(cursorPath, blocks.slice(0, 3));
    const node = liveOgmiosNode(blocks);
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const authority = followingAuthority(cursorPath);
      await expect(authority.synchronizeToTip()).resolves.toMatchObject({
        slot: 5,
      });
      // The next sync (the next tick) finds nothing new: it must not wait for
      // the node's next block, which a live node holds the nextBlock for.
      await expect(
        settlesWithin(authority.synchronizeToTip(), 1_000),
      ).resolves.toMatchObject({ slot: 5, blockHash: blocks[4]!.id });
      expect(node.heldAtTip()).toBe(0);

      // A block landing later is delivered, not skipped.
      node.extend(blockAt(6));
      await expect(
        settlesWithin(authority.synchronizeToTip(), 1_000),
      ).resolves.toMatchObject({ slot: 6 });

      // A rollback landing while the member stood at the tip is delivered in
      // order before the blocks that replace it.
      node.rollBackTo(4);
      node.extend(blockAt(5, 1));
      node.extend(blockAt(6, 1));
      await expect(
        settlesWithin(authority.synchronizeToTip(), 1_000),
      ).resolves.toMatchObject({ slot: 6, blockHash: blockAt(6, 1).id });
      expect(
        (await authority.replay(2)).map(({ direction, point }) => [
          direction,
          point.slot,
          point.blockHash.slice(0, 2),
        ]),
      ).toEqual([
        ["roll_forward", 4, "00"],
        ["roll_forward", 5, "00"],
        ["roll_forward", 6, "00"],
        ["roll_backward", 4, "00"],
        ["roll_forward", 5, "01"],
        ["roll_forward", 6, "01"],
      ]);
      await expect(authority.currentCursor()).resolves.toMatchObject({
        sequence: 8,
        rollbackGeneration: 1,
      });
      expect(node.heldAtTip()).toBe(0);
      expect(node.intersections()).toHaveLength(1);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("resumes a restarted member from its durable cursor, walks only the blocks it missed, and keeps the journal bounded", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const history = 5_000;
    const missed = 12;
    const blocks = Array.from({ length: history + missed }, (_, index) =>
      blockAt(index + 1),
    );
    await writeFollowedJournal(cursorPath, blocks.slice(0, history));
    const node = liveOgmiosNode(blocks);
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const authority = followingAuthority(cursorPath);
      const consumer = consumerOf(authority, cursorPath);
      await expect(
        settlesWithin(authority.synchronizeToTip(), 2_000),
      ).resolves.toMatchObject({ slot: history + missed });
      // One intersection at the durable cursor, then one nextBlock for the
      // handshake echo and one per missed block: never a walk of history.
      expect(node.intersections()).toHaveLength(1);
      expect(node.intersections()[0]![0]).toEqual(blocks[history - 1]);
      expect(node.nextBlocks()).toBe(missed + 1);
      // The next tick's sync finds the member at the tip and returns at once.
      await expect(
        settlesWithin(authority.synchronizeToTip(), 1_000),
      ).resolves.toMatchObject({ slot: history + missed });
      expect(node.nextBlocks()).toBe(missed + 1);
      expect(node.heldAtTip()).toBe(0);

      // Once the consumer has replayed through the cursor, the journal keeps
      // only the points a resumption intersects with.
      const cursor = await authority.currentCursor();
      await consumer.acknowledgeChainSyncCursor(cursor);
      const kept = await journalSequences(cursorPath);
      expect(kept).toHaveLength(2_160);
      expect(kept[0]).toBe(cursor.sequence - 2_159);
      expect(kept.at(-1)).toBe(cursor.sequence);
      await expect(authority.replay(cursor.sequence)).resolves.toEqual([]);

      // A second restart loads the pruned journal and again walks only what
      // it missed.
      node.extend(blockAt(history + missed + 1));
      node.extend(blockAt(history + missed + 2));
      const restarted = followingAuthority(cursorPath);
      await expect(
        settlesWithin(restarted.synchronizeToTip(), 1_000),
      ).resolves.toMatchObject({ slot: history + missed + 2 });
      const candidates = node.intersections().at(-1)!;
      expect(candidates[0]).toEqual(blocks.at(-1));
      expect(candidates).toHaveLength(2_160 + 1);
      await expect(restarted.currentCursor()).resolves.toMatchObject({
        sequence: cursor.sequence + 2,
      });
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("never prunes an event the durable consumer has not replayed", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const blocks = Array.from({ length: 5_000 }, (_, index) =>
      blockAt(index + 1),
    );
    await writeFollowedJournal(cursorPath, blocks);
    const store = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));

    // Too few prunable entries to be worth a rewrite yet.
    await store.prune(999);
    expect(await journalSequences(cursorPath)).toHaveLength(5_000);

    // The consumer lags far behind the intersection window: everything after
    // it survives, so its rollback replay stays contiguous.
    await store.prune(2_000);
    const kept = await journalSequences(cursorPath);
    expect(kept[0]).toBe(2_001);
    expect(kept).toHaveLength(2_999);
    const replayed = await store.replay(2_000);
    expect(replayed).toHaveLength(2_999);
    expect(replayed[0]!.point.slot).toBe(2_002);

    // Reloaded from disk, the pruned journal is accepted and still appends.
    const reloaded = new FileChainSyncCursorStore(cursorPath, "11".repeat(32));
    await expect(reloaded.load()).resolves.toMatchObject({ sequence: 4_999 });
    const next = nodePoint(blockAt(5_001));
    await reloaded.append(
      { direction: "roll_forward", point: next },
      { sequence: 5_000, point: next, rollbackGeneration: 0 },
    );
    expect((await journalSequences(cursorPath)).at(-1)).toBe(5_000);
  });

  it("refuses a pruned journal with a gap", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const blocks = Array.from({ length: 10 }, (_, index) => blockAt(index + 1));
    await writeFollowedJournal(cursorPath, blocks);
    const lines = (await readFile(`${cursorPath}.events.jsonl`, "utf8"))
      .split("\n")
      .filter((line) => line.length > 0);
    await writeFile(
      `${cursorPath}.events.jsonl`,
      [...lines.slice(4, 6), ...lines.slice(7)]
        .map((line) => `${line}\n`)
        .join(""),
    );
    await expect(
      new FileChainSyncCursorStore(cursorPath, "11".repeat(32)).load(),
    ).rejects.toThrow(L1SourceIntegrityError);
  });
});

/** A durable cursor store held in memory, for catch-ups too long to fsync. */
const memoryCursorStore = (
  initial: readonly FakeOgmiosBlock[],
): ChainSyncCursorStore & {
  readonly entries: { event: ChainSyncEvent; cursor: ChainSyncCursor }[];
} => {
  const entries: { event: ChainSyncEvent; cursor: ChainSyncCursor }[] =
    initial.map((block, sequence) => ({
      event: { direction: "roll_forward" as const, point: nodePoint(block) },
      cursor: {
        sequence,
        point: nodePoint(block),
        rollbackGeneration: 0,
      },
    }));
  return {
    entries,
    load: async () => entries.at(-1)?.cursor,
    append: async (event, cursor) => {
      entries.push({ event, cursor });
    },
    replay: async (afterSequence) =>
      entries
        .filter(({ cursor }) => cursor.sequence > afterSequence)
        .map(({ event }) => event),
    cursorAt: async (sequence) =>
      entries.find(({ cursor }) => cursor.sequence === sequence)?.cursor,
  };
};

/**
 * A chain-sync source that follows `chain()` from whatever cursor it is given,
 * rolling a cursor that left the chain back to the last point they share.
 */
const followingSource = (chain: () => readonly FakeOgmiosBlock[]) => {
  let calls = 0;
  return {
    calls: () => calls,
    next: async (
      cursor: ChainSyncCursor | undefined,
    ): Promise<ChainSyncEventBatch> => {
      calls += 1;
      const blocks = chain();
      const tip = nodePoint(blocks.at(-1)!);
      const at = blocks.findIndex(
        (block) =>
          block.slot === cursor?.point.slot &&
          block.id === cursor.point.blockHash,
      );
      if (at < 0) {
        const shared = blocks.filter(
          (block) => block.slot < (cursor?.point.slot ?? 0),
        );
        return {
          event: {
            direction: "roll_backward",
            point: nodePoint(shared.at(-1)!),
          },
          tip,
        };
      }
      const block = blocks[at + 1];
      return block === undefined
        ? { tip }
        : {
            event: { direction: "roll_forward", point: nodePoint(block) },
            tip,
          };
    },
  };
};

describe("long-outage chain-sync catch-up", () => {
  it("starts after an outage longer than one chunk, delivering exactly the missed blocks in order through a mid-catch-up rollback", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const blocks = Array.from({ length: 40 }, (_, index) => blockAt(index + 1));
    await writeFollowedJournal(cursorPath, blocks.slice(0, 10));
    const node = liveOgmiosNode(blocks);
    vi.stubGlobal("WebSocket", node.WebSocket);
    try {
      const progress: (ChainSyncCatchUpProgress & { caughtUp: boolean })[] = [];
      const authority = new LocalNodeChainAuthority(
        "node-a",
        "Preview",
        new OgmiosChainSyncEventSource(
          "ws://ogmios.local",
          "Preview",
          "node-a",
        ),
        new FileChainSyncCursorStore(cursorPath, "11".repeat(32)),
        (report) => {
          progress.push(report);
          if (progress.length === 1) {
            // The chain forks while the member is still catching up.
            node.rollBackTo(12);
            for (let slot = 13; slot <= 40; slot += 1) {
              node.extend(blockAt(slot, 1));
            }
          }
        },
      );
      const consumer = consumerOf(authority, cursorPath);
      await expect(
        settlesWithin(authority.synchronizeToTip(4), 5_000),
      ).resolves.toMatchObject({ slot: 40, blockHash: blockAt(40, 1).id });

      expect(
        (await authority.replay(9)).map(({ direction, point }) => [
          direction,
          point.slot,
          point.blockHash.slice(0, 2),
        ]),
      ).toEqual([
        ...[11, 12, 13, 14].map((slot) => ["roll_forward", slot, "00"]),
        ["roll_backward", 12, "00"],
        ...Array.from({ length: 28 }, (_, index) => [
          "roll_forward",
          index + 13,
          "01",
        ]),
      ]);
      await expect(authority.currentCursor()).resolves.toMatchObject({
        sequence: 9 + 4 + 1 + 28,
        rollbackGeneration: 1,
      });
      expect(node.intersections()).toHaveLength(1);
      // One progress line per full chunk, then one when the tip is reached.
      expect(
        progress.map(({ events, caughtUp }) => [events, caughtUp]),
      ).toEqual([
        [4, false],
        [8, false],
        [12, false],
        [16, false],
        [20, false],
        [24, false],
        [28, false],
        [32, false],
        [33, true],
      ]);
      expect(progress[0]).toMatchObject({ cursorSlot: 14, tipSlot: 40 });
      expect(progress.at(-1)).toMatchObject({ cursorSlot: 40, tipSlot: 40 });
      expect(consumer.chainSyncCatchUpProgress()).toBeUndefined();
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("syncs past the default chunk size in order instead of failing startup", async () => {
    const history = 10;
    const outage = 2 * 4_096 + 100;
    const blocks = Array.from({ length: history + outage }, (_, index) =>
      blockAt(index + 1),
    );
    const store = memoryCursorStore(blocks.slice(0, history));
    const progress: number[] = [];
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      followingSource(() => blocks),
      store,
      ({ events, caughtUp }) => {
        if (!caughtUp) progress.push(events);
      },
    );
    await expect(authority.synchronizeToTip()).resolves.toMatchObject({
      slot: history + outage,
    });
    expect(progress).toEqual([4_096, 8_192]);
    const delivered = store.entries.slice(history);
    expect(delivered).toHaveLength(outage);
    expect(
      delivered.every(
        ({ event, cursor }, index) =>
          event.direction === "roll_forward" &&
          event.point.slot === history + index + 1 &&
          cursor.sequence === history + index,
      ),
    ).toBe(true);
  });

  it("fails a chunk that makes no progress with a retryable error, then resumes", async () => {
    const blocks = Array.from({ length: 20 }, (_, index) => blockAt(index + 1));
    let oscillating = true;
    const honest = followingSource(() => blocks);
    const source = {
      calls: 0,
      next: async (cursor: ChainSyncCursor | undefined) => {
        source.calls += 1;
        if (source.calls > 1_000) {
          throw new Error("chain-sync never gave up on a source going nowhere");
        }
        if (!oscillating) return honest.next(cursor);
        const tip = nodePoint(blocks.at(-1)!);
        // Forward to slot 11 and back to slot 10, forever.
        return cursor!.point.slot === 10
          ? {
              event: {
                direction: "roll_forward" as const,
                point: nodePoint(blocks[10]!),
              },
              tip,
            }
          : {
              event: {
                direction: "roll_backward" as const,
                point: nodePoint(blocks[9]!),
              },
              tip,
            };
      },
    };
    const store = memoryCursorStore(blocks.slice(0, 10));
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      source,
      store,
    );
    const failure = authority.synchronizeToTip(4);
    await expect(failure).rejects.toThrow(ChainSyncNoProgressError);
    await expect(failure).rejects.not.toThrow(L1SourceIntegrityError);
    expect(source.calls).toBe(4);
    expect(authority.catchUpProgress()).toBeUndefined();

    // Nothing durable is wrong: once the source delivers the chain, the next
    // attempt resumes from the durable cursor and reaches the tip.
    oscillating = false;
    await expect(authority.synchronizeToTip(4)).resolves.toMatchObject({
      slot: 20,
    });
    expect(store.entries.map(({ cursor }) => cursor.sequence)).toEqual(
      Array.from({ length: store.entries.length }, (_, index) => index),
    );
  });

  it("reports catch-up until the tip and reads no query surface before it", async () => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    // Longer than one default chunk, which is what the snapshot's sync uses.
    const blocks = Array.from({ length: 5 + 4_096 + 20 }, (_, index) =>
      blockAt(index + 1),
    );
    const followed = followingSource(() => blocks);
    let release: (() => void) | undefined;
    const held = new Promise<void>((resolve) => {
      release = resolve;
    });
    const store = memoryCursorStore(blocks.slice(0, 5));
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      {
        next: async (cursor) => {
          // Hold the catch-up after its first chunk.
          if (cursor!.point.slot === 5 + 4_096) await held;
          return followed.next(cursor);
        },
      },
      store,
    );
    let snapshotReads = 0;
    const provider = new LocalNodeStateQueueProvider(
      authority,
      [
        {
          fetchStateQueueNodes: async () => [],
          fetchStateQueueSnapshot: async () => {
            snapshotReads += 1;
            throw new Error("query surface read");
          },
          fetchStateQueueReplayCheckpoints: async () => [],
          currentChainPoint: () => authority.currentPoint(),
        },
      ],
      ["query:node-a:0"],
      new FileChainSyncConsumerCursorStore(
        `${cursorPath}.watcher-consumer-v1`,
        "11".repeat(32),
      ),
    );
    const snapshot = provider.fetchStateQueueSnapshot();
    snapshot.catch(() => undefined);
    await expect
      .poll(() => provider.chainSyncCatchUpProgress())
      .toEqual({
        events: 4_096,
        cursorSlot: 5 + 4_096,
        tipSlot: 5 + 4_096 + 20,
      });
    expect(snapshotReads).toBe(0);
    release!();
    await expect(snapshot).rejects.toThrow("query surface read");
    expect(snapshotReads).toBe(1);
    expect(provider.chainSyncCatchUpProgress()).toBeUndefined();
    await expect(authority.currentPoint()).resolves.toMatchObject({
      slot: 5 + 4_096 + 20,
    });
  });
});

/**
 * The chain-sync reader against what a live preprod Ogmios v7.0.0 and Kupo
 * v2.11.0 answered, recorded by
 * `@al-ft/midgard-test-support/scripts/capture-l1-recordings.mjs`:
 *
 * - `preprod-ogmios-follow-tip` is a session in the order a member's syncs
 *   issue it: bootstrap at the tip, the tip re-read until the node adopts a
 *   block, and only then the handshake `nextBlock` and the one that delivers
 *   the block.
 * - `preprod-ogmios-network` holds the rest of what the reader leans on: a
 *   `nextBlock` sent while the tip is unchanged, which Ogmios holds until the
 *   next block, and Kupo's text/plain health with its ETag.
 *
 * Every refusal mutates one field of a recording rather than inventing an
 * answer.
 */
describe("recorded preprod Ogmios and Kupo", () => {
  it("records the Ogmios v7.0.0 shapes the chain-sync reader is written against", () => {
    const network = loadL1Recording(NETWORK);
    // No `queryNetwork/tip` carries a height, over WebSocket or HTTP.
    const tips = [
      ...recordedTips(network),
      ...recordedTips(loadL1Recording(FOLLOW_TIP)),
      ...network.exchanges
        .filter(
          (exchange) =>
            exchange.surface === "ogmios-http" &&
            exchange.request.method === "queryNetwork/tip",
        )
        .map(
          (exchange) =>
            (JSON.parse(recordedText(exchange.response)) as { result: object })
              .result,
        ),
    ];
    expect(tips.length).toBeGreaterThan(0);
    for (const tip of tips) {
      expect(Object.keys(tip).sort()).toEqual(["id", "slot"]);
    }
    // The height rides chain-sync's own tips, and blockHeight answers alone.
    const [found] = ogmiosExchanges(network, "findIntersection").map(
      ogmiosResult,
    ) as { intersection: object; tip: { height: number } }[];
    const [height] = ogmiosExchanges(network, "queryNetwork/blockHeight").map(
      ogmiosResult,
    );
    expect(found!.tip.height).toBe(height);
    const [echo, forward] = ogmiosExchanges(network, "nextBlock");
    // The first nextBlock echoes the intersection back as a rollback.
    expect(ogmiosResult(echo!)).toMatchObject({
      direction: "backward",
      point: found!.intersection,
      tip: found!.tip,
    });
    expect(ogmiosResult(forward!)).toMatchObject({
      direction: "forward",
      tip: { height: found!.tip.height + 1 },
    });
    // Sent while the tip had not moved, the next one was held until the node
    // adopted a block: longer than a session's 15 s request timeout, which is
    // why a sync at the tip asks for the tip before it sends a nextBlock.
    expect(forward!.elapsedMs).toBeGreaterThan(15_000);
  });

  it("follows the recorded tip: bootstrap, re-reads at the tip, the handshake echo, the adopted block", async () => {
    const dir = await tempDir();
    const recording = loadL1Recording(FOLLOW_TIP);
    const tips = recordedTips(recording);
    const bootstrapTip = tips[0]!;
    const adoptedTip = tips.at(-1)!;
    expect(adoptedTip).not.toEqual(bootstrapTip);
    const replay = recordedOgmiosWebSocket(recording);
    vi.stubGlobal("WebSocket", replay.WebSocket);
    try {
      const authority = recordedPreprodAuthority(`${dir}/cursor.json`);
      // One sync per recorded tip read: the bootstrap, then each sync at the
      // tip, the last of which sees the tip move.
      const points: CanonicalChainPoint[] = [];
      for (let sync = 0; sync < tips.length; sync += 1) {
        points.push(await authority.synchronizeToTip());
      }
      expect(points.map(({ slot, blockHash }) => [slot, blockHash])).toEqual([
        ...tips.slice(0, -1).map(() => [bootstrapTip.slot, bootstrapTip.id]),
        [adoptedTip.slot, adoptedTip.id],
      ]);
      // Chain-sync's tips carried a height; the points it made carry none.
      for (const point of points) {
        expect(point).not.toHaveProperty("blockHeight");
      }
      // Every recorded answer was asked for, in the order it was recorded,
      // on one connection.
      expect(replay.requests().map(({ method }) => method)).toEqual(
        recording.exchanges.map(({ request }) => request.method),
      );
      expect(replay.sockets()).toBe(1);
      // The handshake echo was no rollback.
      expect(
        (await authority.replay(-1)).map(({ direction, point }) => [
          direction,
          point.slot,
        ]),
      ).toEqual([
        ["roll_forward", bootstrapTip.slot],
        ["roll_forward", adoptedTip.slot],
      ]);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("journals a backward answer that is not the intersection as a rollback", async () => {
    const dir = await tempDir();
    const recording = loadL1Recording(FOLLOW_TIP);
    const tips = recordedTips(recording);
    const bootstrapTip = tips[0]!;
    const echo = (
      ogmiosExchanges(recording, "nextBlock")[0]!.response.body as {
        result: { direction: string; point: { slot: number; id: string } };
      }
    ).result;
    expect(echo.direction).toBe("backward");
    echo.point = { slot: bootstrapTip.slot - 1, id: "ab".repeat(32) };
    vi.stubGlobal("WebSocket", recordedOgmiosWebSocket(recording).WebSocket);
    try {
      const authority = recordedPreprodAuthority(`${dir}/cursor.json`);
      for (let sync = 0; sync < tips.length; sync += 1) {
        await authority.synchronizeToTip();
      }
      expect(
        (await authority.replay(-1)).map(({ direction, point }) => [
          direction,
          point.slot,
          point.blockHash,
        ]),
      ).toEqual([
        ["roll_forward", bootstrapTip.slot, bootstrapTip.id],
        ["roll_backward", bootstrapTip.slot - 1, "ab".repeat(32)],
        ["roll_forward", tips.at(-1)!.slot, tips.at(-1)!.id],
      ]);
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("refuses a recorded session once its genesis names another network", async () => {
    const dir = await tempDir();
    const recording = loadL1Recording(FOLLOW_TIP);
    (
      ogmiosExchanges(recording, "queryNetwork/genesisConfiguration")[0]!
        .response.body as { result: { networkMagic: number } }
    ).result.networkMagic = 2;
    vi.stubGlobal("WebSocket", recordedOgmiosWebSocket(recording).WebSocket);
    try {
      const failure = await recordedPreprodAuthority(`${dir}/cursor.json`)
        .synchronizeToTip()
        .then(
          () => undefined,
          (error: unknown) => error,
        );
      expect(failure).toBeInstanceOf(L1SourceIntegrityError);
      expect((failure as Error).message).toBe(
        "Ogmios network magic 2 does not match configured Preprod magic 1",
      );
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("binds Kupo's recorded checkpoint to the block Ogmios adopted", async () => {
    const network = loadL1Recording(NETWORK);
    const adopted = (
      ogmiosResult(ogmiosExchanges(network, "nextBlock")[1]!) as {
        block: { slot: number; id: string };
      }
    ).block;
    await expect(
      fetchKupoCheckpoint("http://kupo.recorded/", recordedFetch(network)),
    ).resolves.toEqual({ slot: adopted.slot, blockHash: adopted.id });
    expect(recordedTips(network).at(-1)).toEqual({
      slot: adopted.slot,
      id: adopted.id,
    });
  });

  it("refuses Kupo's recorded health once it loses its ETag", async () => {
    const network = loadL1Recording(NETWORK);
    const headers = kupoExchange(network, "/health").response.headers as Record<
      string,
      string
    >;
    delete headers.etag;
    await expect(
      fetchKupoCheckpoint("http://kupo.recorded/", recordedFetch(network)),
    ).rejects.toThrow(/checkpoint ETag/u);
  });
});

const NETWORK = "preprod-ogmios-network";
const FOLLOW_TIP = "preprod-ogmios-follow-tip";

/** The `queryNetwork/tip` answers a recording's session holds, in order. */
const recordedTips = (
  recording: L1Recording,
): { readonly slot: number; readonly id: string }[] =>
  ogmiosExchanges(recording, "queryNetwork/tip").map(
    (exchange) => ogmiosResult(exchange) as { slot: number; id: string },
  );

/** A preprod authority following the stubbed global WebSocket. */
const recordedPreprodAuthority = (cursorPath: string) =>
  new LocalNodeChainAuthority(
    "node-a",
    "Preprod",
    new OgmiosChainSyncEventSource("ws://ogmios.recorded", "Preprod", "node-a"),
    new FileChainSyncCursorStore(cursorPath, "11".repeat(32)),
  );

/** A signature record carrying `l1ChainPoint`, otherwise fixed. */
const daSignatureRecordAt = ({
  l1ChainPoint,
  headerHash,
  stateQueueOutRef,
}: {
  readonly l1ChainPoint: unknown;
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
}): DaSignatureRecordV1 => {
  const availabilityCommitmentCbor = SDK.encodeDaAvailabilityCommitment(
    SDK.buildDaAvailabilityCommitment({
      deploymentIdentity: "99".repeat(28),
      headerHash,
      payload: Buffer.from("public retained DA"),
      bondOwner: "76".repeat(28),
      responseGeometry: SDK.availabilityResponseGeometry({
        chunkByteLength: 14_020,
        trancheByteLength: 4 * 1_024 * 1_024,
        maxTrancheCount: 16,
      }),
    }),
  );
  return {
    deploymentFingerprint: "dep",
    headerHash,
    signerIndex: 0,
    signatureWitness: "00" + "11".repeat(64),
    availabilityCommitmentCbor,
    availabilityCommitmentDigest: computeDaSha256Hash(
      Buffer.from(availabilityCommitmentCbor, "hex"),
    ).toString("hex"),
    payloadHash: "03".repeat(32),
    committeeSignersHash: "02".repeat(32),
    signedAt: "2026-01-01T00:00:00.000Z",
    broadcastStatus: "local",
    source: "local",
    l1ChainPoint: l1ChainPoint as DaSignatureRecordV1["l1ChainPoint"],
    validation: {
      payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
      rootsMatch: true,
      stateQueueOutRef,
      headerHash,
      rootSummary: {
        utxosRoot: "00".repeat(32),
        transactionsRoot: "00".repeat(32),
        depositsRoot: "00".repeat(32),
        withdrawalsRoot: "00".repeat(32),
        forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
      countSummary: {
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: 0n,
        depositCount: 0n,
        totalEventCount: 0n,
        transitionStepCount: 0n,
        validationTraceCount: 0n,
      },
      l1Header: {
        startTime: "1",
        endTime: "2",
        operatorVkey: "04".repeat(28),
        prevHeaderHash: "05".repeat(28),
        protocolVersion: "1",
      },
    },
  };
};

describe("chain-sync consumer acknowledgement", () => {
  /**
   * A member whose journal already holds `history` followed blocks, with an
   * authority following a chain the test extends or forks mid-tick, and the
   * durable consumer the committee acknowledges through.
   */
  const followingMember = async (history: number) => {
    const dir = await tempDir();
    const cursorPath = `${dir}/chain-sync-cursor.json`;
    const chain = {
      blocks: Array.from({ length: history }, (_, index) => blockAt(index + 1)),
    };
    await writeFollowedJournal(cursorPath, chain.blocks);
    const authority = new LocalNodeChainAuthority(
      "node-a",
      "Preview",
      followingSource(() => chain.blocks),
      new FileChainSyncCursorStore(cursorPath, "11".repeat(32)),
    );
    const consumer = consumerOf(authority, cursorPath);
    const extend = async (count: number) => {
      const from = chain.blocks.at(-1)!.slot + 1;
      for (let slot = from; slot < from + count; slot += 1) {
        chain.blocks.push(blockAt(slot));
      }
      await authority.synchronizeToTip();
    };
    const journal = () => readFile(`${cursorPath}.events.jsonl`, "utf8");
    await authority.synchronizeToTip();
    return { dir, cursorPath, chain, authority, consumer, extend, journal };
  };

  /** The cursor a follower journals at `sequence` on the unforked chain. */
  const followedCursor = (sequence: number): ChainSyncCursor => ({
    sequence,
    point: nodePoint(blockAt(sequence + 1)),
    rollbackGeneration: 0,
  });

  it("acknowledges the cursor a slow tick captured after the authority moved on, keeps every newer event, and leaves exactly those to replay", async () => {
    const { cursorPath, authority, consumer, extend } =
      await followingMember(5_000);
    // The tick captures the authority cursor for its rollback check...
    const captured = await consumer.currentChainSyncCursor();
    expect(captured).toEqual(followedCursor(4_999));
    // ...and outlives a few blocks: its own queries synchronize the
    // authority past the captured cursor.
    const newer = 12;
    await extend(newer);
    const current = await authority.currentCursor();
    expect(current.sequence).toBe(captured.sequence + newer);

    await expect(
      consumer.acknowledgeChainSyncCursor(captured),
    ).resolves.toEqual({ rollbackSinceCapture: false });
    await expect(consumer.loadConsumedChainSyncCursor()).resolves.toEqual(
      captured,
    );
    // Pruned down to the intersection window, which still holds every
    // event after the captured cursor.
    const kept = await journalSequences(cursorPath);
    expect(kept).toHaveLength(CHAIN_SYNC_INTERSECTION_POINTS);
    expect(kept[0]).toBeLessThanOrEqual(captured.sequence + 1);
    expect(kept.at(-1)).toBe(current.sequence);
    // The next tick replays from the consumed cursor: exactly the newer
    // events, in order.
    const replayed = await consumer.replayChainSyncEvents(
      (await consumer.loadConsumedChainSyncCursor())!.sequence,
    );
    expect(
      replayed.map(({ direction, point }) => [direction, point.slot]),
    ).toEqual(
      Array.from({ length: newer }, (_, index) => [
        "roll_forward",
        captured.point.slot + 1 + index,
      ]),
    );
    // Acknowledging the newer cursor afterwards moves the consumer on.
    await expect(consumer.acknowledgeChainSyncCursor(current)).resolves.toEqual(
      {
        rollbackSinceCapture: false,
      },
    );
    await expect(consumer.loadConsumedChainSyncCursor()).resolves.toEqual(
      current,
    );
  });

  it("acknowledges a first captured cursor though the chain rolled back after it, keeping the rollback to replay", async () => {
    const { cursorPath, chain, authority, consumer } =
      await followingMember(5_000);
    const captured = await consumer.currentChainSyncCursor();
    // Mid-tick, the node forks at the captured block.
    chain.blocks.length = 4_999;
    for (let slot = 5_000; slot <= 5_003; slot += 1) {
      chain.blocks.push(blockAt(slot, 1));
    }
    await authority.synchronizeToTip();
    const current = await authority.currentCursor();
    expect(current).toMatchObject({ sequence: 5_004, rollbackGeneration: 1 });

    await expect(
      consumer.acknowledgeChainSyncCursor(captured),
    ).resolves.toEqual({ rollbackSinceCapture: true });
    await expect(consumer.loadConsumedChainSyncCursor()).resolves.toEqual(
      captured,
    );
    // Pruned, but never past the acknowledged cursor: the rollback after it
    // is still journaled, and the next replay delivers it.
    const kept = await journalSequences(cursorPath);
    expect(kept.length).toBeLessThan(current.sequence + 1);
    expect(kept[0]).toBeLessThanOrEqual(captured.sequence + 1);
    expect(kept.at(-1)).toBe(current.sequence);
    await expect(
      consumer.replayChainSyncEvents(
        (await consumer.loadConsumedChainSyncCursor())!.sequence,
      ),
    ).resolves.toEqual([
      { direction: "roll_backward", point: nodePoint(blockAt(4_999)) },
      ...[5_000, 5_001, 5_002, 5_003].map((slot) => ({
        direction: "roll_forward",
        point: nodePoint(blockAt(slot, 1)),
      })),
    ]);
  });

  it("moves an existing consumed cursor to a later capture the chain rolled back after, keeping the rollback to replay", async () => {
    const { chain, authority, consumer, extend } = await followingMember(20);
    const consumed = await consumer.currentChainSyncCursor();
    await expect(
      consumer.acknowledgeChainSyncCursor(consumed),
    ).resolves.toEqual({ rollbackSinceCapture: false });
    await extend(3);
    const captured = await consumer.currentChainSyncCursor();
    expect(captured).toEqual(followedCursor(22));
    chain.blocks.length = 22;
    chain.blocks.push(blockAt(23, 1), blockAt(24, 1));
    await authority.synchronizeToTip();

    await expect(
      consumer.acknowledgeChainSyncCursor(captured),
    ).resolves.toEqual({ rollbackSinceCapture: true });
    await expect(consumer.loadConsumedChainSyncCursor()).resolves.toEqual(
      captured,
    );
    // The next tick replays from the acknowledged cursor: the rollback first.
    await expect(
      consumer.replayChainSyncEvents(captured.sequence),
    ).resolves.toEqual([
      { direction: "roll_backward", point: nodePoint(blockAt(22)) },
      ...[23, 24].map((slot) => ({
        direction: "roll_forward",
        point: nodePoint(blockAt(slot, 1)),
      })),
    ]);
  });

  it("refuses a cursor ahead of the authority, a forged one, one behind the consumer, and one whose event was pruned, recording nothing", async () => {
    const { dir, authority, consumer, extend, journal } =
      await followingMember(5_000);
    const consumed = await consumer.currentChainSyncCursor();
    await expect(
      consumer.acknowledgeChainSyncCursor(consumed),
    ).resolves.toEqual({ rollbackSinceCapture: false });
    await extend(5);
    const current = await authority.currentCursor();
    expect(current).toEqual(followedCursor(5_004));
    const before = await journal();
    const laterObservation = "2026-07-29T00:00:00.000Z";

    const refused: readonly ChainSyncCursor[] = [
      // Ahead of the authority cursor.
      followedCursor(current.sequence + 1),
      { ...current, rollbackGeneration: 1 },
      // A journaled sequence carrying a point the journal does not hold.
      {
        ...followedCursor(5_002),
        point: { ...followedCursor(5_002).point, observedAt: laterObservation },
      },
      { ...followedCursor(5_002), point: nodePoint(blockAt(5_003, 1)) },
      { ...followedCursor(5_002), sequence: 5_003 },
      // Behind the durable consumer cursor, though still journaled.
      followedCursor(consumed.sequence - 1),
      // At the consumer's sequence, but not the cursor it recorded.
      {
        ...consumed,
        point: { ...consumed.point, observedAt: laterObservation },
      },
    ];
    for (const cursor of refused) {
      await expect(consumer.acknowledgeChainSyncCursor(cursor)).rejects.toThrow(
        L1SourceIntegrityError,
      );
      expect(await journal()).toBe(before);
      await expect(consumer.loadConsumedChainSyncCursor()).resolves.toEqual(
        consumed,
      );
    }

    // A consumer without a durable cursor cannot acknowledge an event the
    // journal no longer holds, even one the follower genuinely journaled.
    const fresh = consumerOf(authority, dir, `${dir}/fresh-consumer.json`);
    await expect(
      fresh.acknowledgeChainSyncCursor(followedCursor(100)),
    ).rejects.toThrow(/journal does not hold/u);
    expect(await journal()).toBe(before);
    await expect(fresh.loadConsumedChainSyncCursor()).resolves.toBeUndefined();

    // The recorded cursor is still accepted again, though its own event may
    // be pruned, and so is the current one.
    await expect(
      consumer.acknowledgeChainSyncCursor(consumed),
    ).resolves.toEqual({ rollbackSinceCapture: false });
    await expect(consumer.acknowledgeChainSyncCursor(current)).resolves.toEqual(
      {
        rollbackSinceCapture: false,
      },
    );
    await expect(consumer.loadConsumedChainSyncCursor()).resolves.toEqual(
      current,
    );
  });

  it(
    "keeps the journal bounded across many ticks that each outlive new blocks",
    { timeout: 30_000 },
    async () => {
      const perTick = 30;
      const { cursorPath, authority, consumer, extend } = await followingMember(
        CHAIN_SYNC_INTERSECTION_POINTS + CHAIN_SYNC_JOURNAL_PRUNE_SLACK,
      );
      const lengths: number[] = [];
      let captured = await consumer.currentChainSyncCursor();
      for (let tick = 0; tick < 12; tick += 1) {
        await extend(perTick);
        await expect(
          consumer.acknowledgeChainSyncCursor(captured),
        ).resolves.toEqual({ rollbackSinceCapture: false });
        await expect(consumer.loadConsumedChainSyncCursor()).resolves.toEqual(
          captured,
        );
        lengths.push((await journalSequences(cursorPath)).length);
        captured = await authority.currentCursor();
      }
      // The first acknowledgement rewrites the journal down to the intersection
      // window, and it then grows only by what later ticks append, never past
      // the rewrite threshold.
      expect(lengths).toEqual(
        lengths.map(
          (_, tick) => CHAIN_SYNC_INTERSECTION_POINTS + tick * perTick,
        ),
      );
      expect(Math.max(...lengths)).toBeLessThan(
        CHAIN_SYNC_INTERSECTION_POINTS + CHAIN_SYNC_JOURNAL_PRUNE_SLACK,
      );
    },
  );
});
