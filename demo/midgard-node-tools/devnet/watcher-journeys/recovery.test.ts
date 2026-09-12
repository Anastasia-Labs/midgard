import { appendFile, mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  admitWatcherNativeRollForwardBlock,
  startWatcherNativeChainSync,
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncRollForward,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { journeyNativeNodeQuery } from "./native-node.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
type Point = Readonly<{ blockHash: string; blockNo: string; slot: string }>;
const pointOf = (block: Point): Point => ({
  blockHash: block.blockHash,
  blockNo: block.blockNo,
  slot: block.slot,
});

it.skipIf(runDirectory === undefined)(
  "reconnects a native follower after three produced blocks on the continuing chain",
  async () => {
    if (runDirectory === undefined) throw new Error("Run directory required");
    const nativeQuery = await journeyNativeNodeQuery(runDirectory);
    const evidenceDirectory = join(
      runDirectory,
      "work/native-follower-recovery",
      new Date().toISOString().replaceAll(":", "-"),
    );
    await mkdir(evidenceDirectory, { recursive: true });
    // Only the native follower starts. The other configuration sections satisfy
    // its shared type; no wallet, DA service, or watcher database is opened.
    const watcherConfig: WatcherConfig = {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      ...nativeQuery.watcherConfig,
      l1: {
        ...nativeQuery.watcherConfig.l1,
        requestTimeoutMs: 30_000,
        maxConcurrency: 1,
        finality: {
          depth: 30,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: 30,
            postFinalityRecoveryMaxDepth: 2160,
          },
        },
      },
      da: { peers: [], requestTimeoutMs: 30_000, maxConcurrency: 1 },
      storage: {
        driver: "sqlite",
        path: join(evidenceDirectory, "unused.sqlite"),
        rollbackAuthorityKeySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
        },
      },
      proverWallet: {
        keySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_PROVER_KEY",
        },
      },
      deadlines: {
        daFetchMs: 60_000,
        daPublishMs: 60_000,
        proofConstructMs: 300_000,
        proofSubmitMs: 120_000,
      },
    };
    const runEnv = await readFile(join(runDirectory, "run.env"), "utf8");
    const port = /^MIDGARD_PHASE4_OGMIOS_PORT=([0-9]+)$/mu.exec(runEnv)?.[1];
    if (port === undefined) throw new Error("Run omitted its Ogmios port");
    const response = await fetch(`http://127.0.0.1:${port}/health`, {
      signal: AbortSignal.timeout(10_000),
    });
    expect(response.ok).toBe(true);
    const health = await response.json();
    expect(health.connectionStatus).toBe("connected");
    const discovered: Point = {
      blockHash: health.lastKnownTip.id,
      blockNo: String(health.lastKnownTip.height),
      slot: String(health.lastKnownTip.slot),
    };
    const startedAt = new Date().toISOString();
    const deadline = performance.now() + 600_000;

    const collect = async (
      name: string,
      intersection: Point,
      count: number,
      expectedPrefix: readonly WatcherNativeChainSyncRollForward[] = [],
    ): Promise<WatcherNativeChainSyncRollForward[]> => {
      const blocks: WatcherNativeChainSyncRollForward[] = [];
      let previous = intersection;
      let stopping = false;
      let resolveComplete!: () => void;
      const complete = new Promise<void>((resolve) => {
        resolveComplete = resolve;
      });
      const follower = await startWatcherNativeChainSync({
        watcherConfig,
        binaryPath: nativeQuery.binaryPath,
        startupTimeoutMs: 30_000,
        intersection: {
          kind: "point",
          blockHash: intersection.blockHash,
          slot: intersection.slot,
        },
        onEvent: async (event) => {
          if (stopping || blocks.length >= count) return;
          if (
            blocks.length === 0 &&
            event.kind === "roll_backward" &&
            event.point.kind === "point" &&
            event.point.blockHash === intersection.blockHash &&
            event.point.slot === intersection.slot
          )
            return;
          if (event.kind !== "roll_forward")
            throw new Error("Native recovery encountered a canonical rollback");
          admitWatcherNativeRollForwardBlock(event);
          expect(event.prevHash).toBe(previous.blockHash);
          expect(BigInt(event.blockNo)).toBe(BigInt(previous.blockNo) + 1n);
          expect(BigInt(event.slot)).toBeGreaterThan(BigInt(previous.slot));
          const expected = expectedPrefix[blocks.length];
          if (expected !== undefined) {
            expect(pointOf(event)).toEqual(pointOf(expected));
            expect(event.rawBlockCbor).toBe(expected.rawBlockCbor);
          }
          await appendFile(
            join(evidenceDirectory, `${name}.ndjson`),
            `${JSON.stringify(event)}\n`,
          );
          blocks.push(event);
          previous = pointOf(event);
          console.info("Native recovery observed canonical block", {
            phase: name,
            ...previous,
          });
          if (blocks.length === count) resolveComplete();
        },
      });
      void follower.done.catch(() => undefined);
      let timer: ReturnType<typeof setTimeout> | undefined;
      try {
        const identity = watcherNativeChainSyncAuthorityDetails(
          follower.authority,
        );
        expect(identity?.genesisIdentitySha256).toBe(
          watcherConfig.l1.source.sourceMode === "local_node"
            ? watcherConfig.l1.source.chainSync.genesisIdentitySha256
            : undefined,
        );
        await writeFile(
          join(evidenceDirectory, `${name}-authority.json`),
          JSON.stringify(identity, null, 2),
        );
        await Promise.race([
          complete,
          follower.done.then(() => {
            throw new Error(
              "Native follower exited before completing recovery",
            );
          }),
          new Promise<never>((_resolve, reject) => {
            timer = setTimeout(
              () => reject(new Error("Native recovery exceeded ten minutes")),
              Math.max(1, deadline - performance.now()),
            );
          }),
        ]);
        return blocks;
      } finally {
        stopping = true;
        clearTimeout(timer);
        await follower.close();
      }
    };

    // Ogmios only discovers the initial intersection. Every recorded block and
    // replay comparison below comes from the admitted native chain protocol.
    const initial = await collect("before-outage", discovered, 1);
    const checkpoint = pointOf(initial[0]!);
    expect(initial[0]!.tip).toEqual({ kind: "point", ...checkpoint });
    const outageStartedAt = new Date().toISOString();
    // This independent witness observes the producer while the tested follower
    // is closed. It provides the exact prefix the restarted follower must replay.
    const missed = await collect("outage-witness", checkpoint, 3);
    const resumedAt = new Date().toISOString();
    const resumed = await collect("resumed", checkpoint, 4, missed);
    const last = pointOf(resumed[3]!);
    expect(BigInt(last.blockNo)).toBe(BigInt(checkpoint.blockNo) + 4n);
    const result = {
      evidenceLayer: "native_follower",
      startedAt,
      outageStartedAt,
      resumedAt,
      completedAt: new Date().toISOString(),
      checkpoint,
      missedBlocks: missed.map(pointOf),
      replayedBlocks: resumed.slice(0, 3).map(pointOf),
      subsequentLiveBlock: last,
      missedBlockCount: 3,
      missedSlotDistance: (
        BigInt(missed[2]!.slot) - BigInt(checkpoint.slot)
      ).toString(),
      status: "passed",
    };
    await writeFile(
      join(evidenceDirectory, "result.json"),
      JSON.stringify(result, null, 2),
    );
    console.info(
      "Native follower recovery evidence",
      evidenceDirectory,
      result,
    );
  },
  660_000,
);
