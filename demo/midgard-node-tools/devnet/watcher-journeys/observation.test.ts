import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  admitWatcherNativeRollForwardBlock,
  assertWatcherLocalKupmiosNativeObservation,
  createWatcherLocalKupmiosNativeObservationRuntime,
  loadWatcherVerifiedDeploymentAuthority,
  parseWatcherProcessConfig,
  startWatcherNativeChainSync,
  type WatcherLocalKupmiosNativeObservationRuntime,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncRollForward,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { journeyNativeNodeQuery } from "./native-node.js";
import { readJourneyNativeEvidencePath } from "./readiness-evidence.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined)(
  "observes backfill through the native startup tip and subsequent live blocks",
  async () => {
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const processConfig = parseWatcherProcessConfig(
      JSON.parse(
        await readFile(join(directory, "watcher-process.json"), "utf8"),
      ),
    );
    const watcherConfig = processConfig.watcherConfig;
    const authority = await loadWatcherVerifiedDeploymentAuthority({
      path: processConfig.deploymentAuthorityPath,
      ruleBundlePath: processConfig.ruleBundlePath,
    });
    const recorded: WatcherNativeChainSyncRollForward[] = (
      await readFile(await readJourneyNativeEvidencePath(directory), "utf8")
    )
      .trim()
      .split("\n")
      .map((line) => JSON.parse(line))
      .filter((event) => event.kind === "roll_forward");
    if (recorded.length < 2)
      throw new Error("Observation check requires a retained native chain");
    const target = recorded.at(-Math.min(64, recorded.length - 1))!;
    const predecessor = recorded.find(
      (block) => block.blockHash === target.prevHash,
    );
    if (predecessor === undefined)
      throw new Error("Observation check omitted the retained predecessor");
    let resolveObservation!: (
      runtime: WatcherLocalKupmiosNativeObservationRuntime,
    ) => void;
    let rejectObservation!: (error: unknown) => void;
    const observationReady =
      new Promise<WatcherLocalKupmiosNativeObservationRuntime>(
        (resolve, reject) => {
          resolveObservation = resolve;
          rejectObservation = reject;
        },
      );
    void observationReady.catch(() => undefined);
    let resolveLive!: () => void;
    const live = new Promise<void>((resolve) => {
      resolveLive = resolve;
    });
    let startupTipBlockNo = 0n;
    let lastHash = predecessor.blockHash;
    let lastBlockNo = BigInt(predecessor.blockNo);
    let observedBlocks = 0;
    let observedLiveBlocks = 0;
    let crossedStartupTip = false;
    let stopping = false;
    const native = await startWatcherNativeChainSync({
      watcherConfig,
      binaryPath: (await journeyNativeNodeQuery(runDirectory!)).binaryPath,
      intersection: {
        kind: "point",
        blockHash: predecessor.blockHash,
        slot: predecessor.slot,
      },
      startupTimeoutMs: 30_000,
      onEvent: async (event) => {
        const runtime = await observationReady;
        if (stopping) return;
        if (
          observedBlocks === 0 &&
          event.kind === "roll_backward" &&
          event.point.kind === "point" &&
          event.point.blockHash === predecessor.blockHash &&
          event.point.slot === predecessor.slot
        )
          return;
        if (event.kind !== "roll_forward")
          throw new Error("Observation handover encountered a native rollback");
        expect(event.prevHash).toBe(lastHash);
        expect(BigInt(event.blockNo)).toBe(lastBlockNo + 1n);
        if (event.tip.kind !== "point")
          throw new Error("Native roll-forward omitted its observed tip");
        const block = admitWatcherNativeRollForwardBlock(event);
        const depth = (
          BigInt(event.tip.blockNo) -
          BigInt(event.blockNo) +
          1n
        ).toString();
        const result = await runtime.observe({ block, depth });
        assertWatcherLocalKupmiosNativeObservation(result, block);
        expect(result.consistency.status).toBe("agreed");
        lastHash = event.blockHash;
        lastBlockNo = BigInt(event.blockNo);
        observedBlocks += 1;
        if (lastBlockNo === startupTipBlockNo) {
          crossedStartupTip = true;
          console.info("Actual observation reached native startup tip", {
            blockNo: event.blockNo,
            observedBlocks,
          });
        }
        if (lastBlockNo > startupTipBlockNo) {
          observedLiveBlocks += 1;
          console.info("Subsequent actual block observation passed", {
            blockNo: event.blockNo,
            slot: event.slot,
            observedLiveBlocks,
          });
          if (observedLiveBlocks === 2) resolveLive();
        }
      },
    });
    void native.done.catch(() => undefined);
    let observation: WatcherLocalKupmiosNativeObservationRuntime | undefined;
    let timeout: ReturnType<typeof setTimeout> | undefined;
    try {
      const details = watcherNativeChainSyncAuthorityDetails(native.authority);
      if (details?.currentTip.kind !== "point")
        throw new Error("Observation check requires a nonempty native tip");
      startupTipBlockNo = BigInt(details.currentTip.blockNo);
      observation = await createWatcherLocalKupmiosNativeObservationRuntime({
        watcherConfig,
        deploymentIdentity: authority.deploymentIdentity,
        nativeAuthority: native.authority,
      });
      resolveObservation(observation);
      await Promise.race([
        live,
        native.done.then(() => {
          throw new Error(
            "Native stream ended before the live observation gate",
          );
        }),
        new Promise<never>((_resolve, reject) => {
          timeout = setTimeout(
            () =>
              reject(
                new Error("Native observation handover exceeded ten minutes"),
              ),
            600_000,
          );
        }),
      ]);
      expect(crossedStartupTip).toBe(true);
      expect(observedLiveBlocks).toBe(2);
      expect(observedBlocks).toBeGreaterThan(2);
    } finally {
      stopping = true;
      clearTimeout(timeout);
      rejectObservation(
        new Error("Observation check ended before initialization"),
      );
      await native.close();
      observation?.close();
    }
  },
  660_000,
);
