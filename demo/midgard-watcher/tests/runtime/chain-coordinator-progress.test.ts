import "../support/chain-coordinator-unit-fixture.js";

import { DatabaseSync } from "node:sqlite";

import { describe, expect, it, vi } from "vitest";

import type { WatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import type { WatcherNativeChainSyncEvent } from "../../src/l1/native-chain-sync.js";
import type { WatcherBlockRelevance } from "../../src/runtime/block-relevance.js";
import { WATCHER_QUIET_RECOVERY_RUN_BLOCKS } from "../../src/runtime/chain-coordinator.advance-canonical.js";
import { unsafeCreateWatcherChainCoordinatorForTest } from "../../src/runtime/chain-coordinator.js";
import { createWatcherSqliteBlockProgressStore } from "../../src/storage/block-progress-store.js";
import type { WatcherDurableRuntime } from "../../src/storage/durable-runtime.js";

const hashOf = (blockNo: number): string =>
  blockNo.toString(16).padStart(64, "0");
const block = (
  blockNo: number,
  relevance: WatcherBlockRelevance = "quiet",
  hashByte?: string,
): WatcherNativeBlockAdmission =>
  Object.freeze({
    schemaVersion: "midgard-watcher-native-block-admission-v1",
    blockType: "7",
    protocolMajor: "10",
    blockHash: hashByte === undefined ? hashOf(blockNo) : hashByte.repeat(64),
    prevHash: hashOf(blockNo - 1),
    slot: (blockNo * 20).toString(),
    blockNo: blockNo.toString(),
    rawBlockCbor: "80",
    rawHeaderCbor: "80",
    transactionIds: Object.freeze(
      relevance === "touched" ? ["cc".repeat(32)] : [],
    ),
    transactionCbors: Object.freeze(relevance === "touched" ? ["80"] : []),
  });
const forward = (
  admitted: WatcherNativeBlockAdmission,
  tipBlockNo: number,
): WatcherNativeChainSyncEvent =>
  Object.freeze({
    schemaVersion: "midgard-watcher-native-chain-sync-v1",
    kind: "roll_forward",
    blockHash: admitted.blockHash,
    blockType: admitted.blockType,
    prevHash: admitted.prevHash,
    slot: admitted.slot,
    blockNo: admitted.blockNo,
    rawBlockCbor: admitted.rawBlockCbor,
    tip: Object.freeze({
      kind: "point",
      blockHash: hashOf(tipBlockNo),
      slot: (tipBlockNo * 20).toString(),
      blockNo: tipBlockNo.toString(),
    }),
  });
const backward = (
  point: WatcherNativeBlockAdmission,
  tipBlockNo: number,
): WatcherNativeChainSyncEvent =>
  Object.freeze({
    schemaVersion: "midgard-watcher-native-chain-sync-v1",
    kind: "roll_backward",
    point: {
      kind: "point" as const,
      blockHash: point.blockHash,
      slot: point.slot,
    },
    tip: Object.freeze({
      kind: "point",
      blockHash: hashOf(tipBlockNo),
      slot: (tipBlockNo * 20).toString(),
      blockNo: tipBlockNo.toString(),
    }),
  });
const finalizedState = (admitted: WatcherNativeBlockAdmission) => ({
  phase: "finalized" as const,
  pending: null,
  finalized: {
    blockHash: admitted.blockHash,
    slot: admitted.slot,
    blockNo: admitted.blockNo,
  },
});
const policy = Object.freeze({
  confirmationDepth: "30",
}) as WatcherFinalityPolicy;
const relevance = (
  candidate: WatcherNativeBlockAdmission,
): WatcherBlockRelevance =>
  candidate.transactionCbors.length === 0 ? "quiet" : "touched";
const key = Uint8Array.from({ length: 32 }, (_, index) => index + 7);

const harness = (
  options?: Readonly<{
    database?: DatabaseSync;
    restart?: WatcherNativeBlockAdmission;
  }>,
) => {
  const included: string[] = [];
  const authority = block(10, "touched");
  let state = finalizedState(authority);
  const observed: string[] = [];
  const persisted: {
    blockNo: string;
    ancestry: readonly string[] | undefined;
  }[] = [];
  const finalized: {
    blockNo: string;
    relevance: string;
    observation: unknown;
  }[] = [];
  const rollbacks: unknown[] = [];
  const persistRollback = vi.fn();
  // Each journaled run, with how many blocks were delivered before it.
  const journaled: { blockNos: string[]; deliveredBefore: number }[] = [];
  const persistObservations = vi.fn(
    async (input: {
      readonly entries: readonly {
        readonly block: { readonly chainPoint: { readonly blockNo: string } };
      }[];
    }) => {
      journaled.push({
        blockNos: input.entries.map(
          ({ block: entry }) => entry.chainPoint.blockNo,
        ),
        deliveredBefore: finalized.length,
      });
      return { persistence: "committed" };
    },
  );
  const observation = {
    observe: async ({
      block: candidate,
      depth,
    }: {
      block: WatcherNativeBlockAdmission;
      depth: string;
    }) => {
      observed.push(`${candidate.blockNo}:${depth}`);
      return {
        block: {
          chainPoint: {
            blockNo: candidate.blockNo,
            blockHash: candidate.blockHash,
            slot: candidate.slot,
          },
        },
        consistency: {},
        transportAttestations: [],
      };
    },
    close: () => undefined,
  } as unknown as WatcherLocalKupmiosNativeObservationRuntime;
  const durable = {
    readFinality: () => state,
    read: () => ({ currentFinalityState: state, currentStore: {} }),
    persistCanonicalProgress: async (input: {
      readonly block: {
        readonly chainPoint: {
          readonly blockNo: string;
          readonly blockHash: string;
          readonly slot: string;
        };
      };
      readonly ancestry?: readonly { blockNo: string }[];
    }) => {
      persisted.push({
        blockNo: input.block.chainPoint.blockNo,
        ancestry: input.ancestry?.map(({ blockNo }) => blockNo),
      });
      state = {
        phase: "finalized",
        pending: null,
        finalized: input.block.chainPoint,
      };
      return {
        persistence: "committed",
        finalityResult: { action: "finalize" },
      };
    },
    persistRollback,
    persistObservations,
  } as unknown as WatcherDurableRuntime;
  const database = options?.database ?? new DatabaseSync(":memory:");
  const progress = createWatcherSqliteBlockProgressStore({
    database,
    authenticationKey: key,
  });
  const restart = options?.restart ?? authority;
  const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
    {
      policy,
      durable,
      observation,
      restartIntersection: {
        kind: "point" as const,
        blockHash: restart.blockHash,
        slot: restart.slot,
      },
      hooks: {
        onIncluded: async ({ nativeBlock }) => {
          included.push(nativeBlock.blockNo);
        },
        onRollback: async (point) => {
          rollbacks.push(point);
        },
        onFinalized: async ({
          nativeBlock,
          localObservation,
          relevance: seen,
        }) => {
          finalized.push({
            blockNo: nativeBlock.blockNo,
            relevance: seen,
            observation: localObservation,
          });
        },
      },
    },
    {
      admitRollForward: (event) => blocks.get(event.blockHash)!,
      relevance,
      progress,
    },
  );
  const blocks = new Map<string, WatcherNativeBlockAdmission>();
  const feed = async (
    candidate: WatcherNativeBlockAdmission,
    tipBlockNo: number,
  ) => {
    blocks.set(candidate.blockHash, candidate);
    await coordinator.handle(forward(candidate, tipBlockNo));
  };
  const start = async (tipBlockNo: number) => {
    await coordinator.handle(backward(restart, tipBlockNo));
  };
  return {
    authority,
    coordinator,
    database,
    progress,
    feed,
    start,
    observed,
    persisted,
    finalized,
    rollbacks,
    persistRollback,
    persistObservations,
    journaled,
    included,
  };
};

describe("coordinator progress ring over quiet blocks", () => {
  it("retains authenticated quiet history without advancing sparse finality", async () => {
    const h = harness();
    await h.start(11);
    await h.feed(block(11), 11);
    await h.feed(block(12), 40);
    expect(h.finalized).toEqual([
      { blockNo: "11", relevance: "quiet", observation: null },
    ]);
    expect(h.observed).toEqual(["11:30"]);
    expect(h.persistObservations).toHaveBeenCalledTimes(1);
    expect(h.persisted).toEqual([]);
    expect(h.progress.readHead()).toMatchObject({
      blockNo: "11",
      relevance: "quiet",
    });
    expect(h.coordinator.status()).toMatchObject({
      processedThrough: { blockNo: "11", blockHash: hashOf(11) },
      bufferedBlockCount: 1,
    });
    await h.feed(block(13), 41);
    expect(h.finalized.map(({ blockNo }) => blockNo)).toEqual(["11", "12"]);
  });

  it("persists a touched block with the quiet ancestry back to the finalized authority", async () => {
    const h = harness();
    await h.start(45);
    await h.feed(block(11), 45);
    await h.feed(block(12), 45);
    await h.feed(block(13, "touched"), 45);
    expect(
      h.finalized.map(({ blockNo, relevance: seen }) => `${blockNo}:${seen}`),
    ).toEqual(["11:quiet", "12:quiet", "13:touched"]);
    // The node already advertised 13, so 11 and 12 wait for it and journal
    // as one run once the touched block ends that run.
    expect(h.observed).toEqual(["13:33", "11:35", "12:34"]);
    expect(h.journaled).toEqual([
      { blockNos: ["11", "12"], deliveredBefore: 0 },
    ]);
    expect(h.persisted).toEqual([{ blockNo: "13", ancestry: ["11", "12"] }]);
    expect(h.finalized[2]!.observation).not.toBeNull();
    expect(h.progress.readHead()).toMatchObject({
      blockNo: "13",
      relevance: "touched",
    });
  });

  it("journals a catch-up's quiet blocks in one revision per run before delivering them", async () => {
    const run = WATCHER_QUIET_RECOVERY_RUN_BLOCKS;
    const blockNos = (from: number, count: number) =>
      Array.from({ length: count }, (_, index) => `${from + index}`);
    const h = harness();
    await h.start(1_000);
    for (let blockNo = 11; blockNo < 10 + run; blockNo += 1)
      await h.feed(block(blockNo), 1_000);
    // The node advertises far more blocks: the run keeps growing undelivered.
    expect(h.persistObservations).not.toHaveBeenCalled();
    expect(h.finalized).toEqual([]);
    await h.feed(block(10 + run), 1_000);
    expect(h.journaled).toEqual([
      { blockNos: blockNos(11, run), deliveredBefore: 0 },
    ]);
    expect(h.finalized.map(({ blockNo }) => blockNo)).toEqual(
      blockNos(11, run),
    );
    // A touched block ends a run early; its quiet predecessors never wait.
    await h.feed(block(11 + run), 1_000);
    await h.feed(block(12 + run), 1_000);
    await h.feed(block(13 + run, "touched"), 1_000);
    expect(h.journaled.slice(1)).toEqual([
      { blockNos: blockNos(11 + run, 2), deliveredBefore: run },
    ]);
    expect(h.persisted.map(({ blockNo }) => blockNo)).toEqual([`${13 + run}`]);
    expect(h.finalized.at(-1)).toMatchObject({
      blockNo: `${13 + run}`,
      relevance: "touched",
    });
  });

  it("stops a quiet run at the forced authority checkpoint", async () => {
    const h = harness();
    await h.start(10_000);
    for (let blockNo = 11; blockNo <= 2_400; blockNo += 1)
      await h.feed(block(blockNo), 10_000);
    // 2170 is 2,160 blocks past the authority at 10: it must advance the
    // sparse authority itself, never ride in a quiet run.
    expect(h.persisted.map(({ blockNo }) => blockNo)).toContain("2170");
    expect(h.journaled.flatMap(({ blockNos }) => blockNos)).not.toContain(
      "2170",
    );
  }, 120_000);

  it("fires onIncluded only for blocks below the confirmation depth", async () => {
    const h = harness();
    await h.start(430);
    for (let blockNo = 11; blockNo <= 430; blockNo += 1)
      await h.feed(block(blockNo), 430);
    // 11..401 arrive finalizable and wait in runs; 402..430 arrive shallow.
    expect(h.finalized.map(({ blockNo }) => Number(blockNo)).at(-1)).toBe(401);
    expect(h.included).toEqual(
      Array.from({ length: 29 }, (_, index) => `${402 + index}`),
    );
  });

  it("journals a run once its newest block is the last one the node has finalizable", async () => {
    const h = harness();
    await h.start(45);
    // Depths 35..31: the node already has the next finalizable block.
    for (let blockNo = 11; blockNo <= 15; blockNo += 1)
      await h.feed(block(blockNo), 45);
    expect(h.persistObservations).not.toHaveBeenCalled();
    // Depth 30: the node's next block is still shallow, so nothing is coming.
    await h.feed(block(16), 45);
    expect(h.journaled).toEqual([
      { blockNos: ["11", "12", "13", "14", "15", "16"], deliveredBefore: 0 },
    ]);
    expect(h.progress.readHead()).toMatchObject({ blockNo: "16" });
  });

  it("rolls the ring back on the quiet side without touching the durable authority", async () => {
    const h = harness();
    await h.start(40);
    // Each block arrives exactly at the confirmation depth, so it finalizes.
    await h.feed(block(11), 40);
    await h.feed(block(12), 41);
    await h.feed(block(13), 42);
    await h.coordinator.handle(backward(block(11), 42));
    expect(h.rollbacks).toHaveLength(1);
    expect(h.persistRollback).not.toHaveBeenCalled();
    expect(h.coordinator.status()).toMatchObject({
      rollbackPoint: null,
      processedThrough: { blockNo: "11" },
    });
    expect(h.progress.readHead()).toMatchObject({ blockNo: "11" });
    const replacement = { ...block(12), blockHash: "ab".repeat(32) };
    await h.feed(replacement, 41);
    expect(h.finalized.map(({ blockNo }) => blockNo)).toEqual([
      "11",
      "12",
      "13",
      "12",
    ]);
    expect(h.progress.readHead()).toMatchObject({
      blockNo: "12",
      blockHash: "ab".repeat(32),
    });
  });

  it("resumes from the recorded head and treats a lower intersection as a rewind", async () => {
    const database = new DatabaseSync(":memory:");
    const first = harness({ database });
    await first.start(40);
    await first.feed(block(11), 40);
    await first.feed(block(12), 41);
    await first.feed(block(13), 42);
    expect(first.progress.readHead()).toMatchObject({ blockNo: "13" });

    const resumed = harness({ database, restart: block(13) });
    await resumed.coordinator.handle(backward(block(13), 42));
    expect(resumed.rollbacks).toEqual([]);
    expect(resumed.coordinator.status()).toMatchObject({
      processedThrough: { blockNo: "13" },
    });
    await resumed.feed(block(14), 43);
    expect(resumed.finalized.map(({ blockNo }) => blockNo)).toEqual(["14"]);

    const rewound = harness({ database, restart: block(12) });
    await rewound.coordinator.handle(backward(block(12), 60));
    expect(rewound.rollbacks).toHaveLength(1);
    expect(rewound.persistRollback).not.toHaveBeenCalled();
    expect(rewound.progress.readHead()).toMatchObject({ blockNo: "12" });
    expect(rewound.coordinator.status()).toMatchObject({
      rollbackPoint: null,
      processedThrough: { blockNo: "12" },
    });
  });
});
