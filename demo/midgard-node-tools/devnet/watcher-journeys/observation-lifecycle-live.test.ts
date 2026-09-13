import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  readAdmittedLocalKupmiosBoundary,
  withLocalKupmiosSourceCapture,
} from "@al-ft/midgard-fault-proofs";
import {
  admitWatcherNativeRollForwardBlock,
  createWatcherLocalKupmiosNativeObservationRuntime,
  createWatcherResolvedBlockObservationSource,
  loadWatcherVerifiedDeploymentAuthority,
  parseWatcherConfig,
  startWatcherNativeChainSync,
  watcherL1TransportAttestationDetails,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncRollForward,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { writeJourneyArtifact } from "./artifacts.js";
import { journeyNativeNodeQuery } from "./native-node.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined)(
  "preserves owned query authority across a real 330-second consumer delay",
  async () => {
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const watcherConfig = parseWatcherConfig(
      JSON.parse(await readFile(join(directory, "watcher.json"), "utf8")),
    );
    const authority = await loadWatcherVerifiedDeploymentAuthority({
      path: join(directory, "deployment-authority.json"),
      ruleBundlePath: join(directory, "rules.json"),
    });
    const recorded: WatcherNativeChainSyncRollForward[] = (
      await readFile(join(directory, "native-chain.ndjson"), "utf8")
    )
      .trim()
      .split("\n")
      .map((line) => JSON.parse(line))
      .filter((event) => event.kind === "roll_forward");
    const target = recorded.at(-64);
    if (target === undefined)
      throw new Error("Lifecycle probe requires 64 retained native blocks");
    const predecessor = recorded.find(
      (event) => event.blockHash === target.prevHash,
    );
    if (predecessor === undefined)
      throw new Error("Lifecycle probe lacks the exact native predecessor");
    const native = await startWatcherNativeChainSync({
      watcherConfig,
      binaryPath: (await journeyNativeNodeQuery(runDirectory!)).binaryPath,
      intersection: {
        kind: "point",
        blockHash: predecessor.blockHash,
        slot: predecessor.slot,
      },
      startupTimeoutMs: 30_000,
      onEvent: async () => undefined,
    });
    let nativeFailure: unknown;
    void native.done.catch((error: unknown) => {
      nativeFailure = error;
    });
    const started = performance.now();
    const samples: {
      elapsedMs: number;
      live: readonly { surface: string; active: boolean }[];
    }[] = [];
    let outcome: unknown;
    let closeObservation: (() => void) | undefined;
    try {
      const runtime = await createWatcherLocalKupmiosNativeObservationRuntime({
        watcherConfig,
        deploymentIdentity: authority.deploymentIdentity,
        nativeAuthority: native.authority,
      });
      closeObservation = runtime.close;
      const tip = watcherNativeChainSyncAuthorityDetails(
        native.authority,
      )?.currentTip;
      if (tip?.kind !== "point") throw new Error("Native tip unavailable");
      const nativeBlock = admitWatcherNativeRollForwardBlock(target);
      const depth = BigInt(tip.blockNo) - BigInt(nativeBlock.blockNo) + 1n;
      expect(depth).toBeGreaterThanOrEqual(30n);
      const localObservation = await runtime.observe({
        block: nativeBlock,
        depth: depth.toString(),
      });
      const surfaces = localObservation.transportAttestations.map((context) => {
        const details = watcherL1TransportAttestationDetails(context);
        if (details?.provider.source.sourceMode !== "local_node")
          throw new Error("Initial local transport is not admitted");
        return details.provider.source.surface;
      });
      console.info("Real transport lifetime probe started", {
        surfaces,
        depth: depth.toString(),
      });
      for (;;) {
        if (nativeFailure !== undefined) throw nativeFailure;
        samples.push({
          elapsedMs: performance.now() - started,
          live: localObservation.transportAttestations.map(
            (context, index) => ({
              surface: surfaces[index]!,
              active: watcherL1TransportAttestationDetails(context) !== null,
            }),
          ),
        });
        const previous = samples.at(-2);
        const current = samples.at(-1)!;
        if (
          previous === undefined ||
          current.live.some(
            (transport, index) =>
              transport.active !== previous.live[index]?.active,
          )
        )
          console.info("Real query authority lifetime", current);
        if (performance.now() - started >= 330_000) break;
        await pause(Math.min(10_000, 330_000 - (performance.now() - started)));
      }
      const source = createWatcherResolvedBlockObservationSource({
        deploymentIdentity: authority.deploymentIdentity,
        rawSource: runtime.rawSource,
      });
      await withLocalKupmiosSourceCapture(runtime.rawSource, async () => {
        await readAdmittedLocalKupmiosBoundary({ source: runtime.rawSource });
        await source.observe({ nativeBlock, localObservation });
      });
      expect(
        samples.every(({ live }) => live.every(({ active }) => active)),
      ).toBe(true);
      outcome = { status: "passed" };
    } catch (error) {
      outcome = {
        status: "failed",
        error: error instanceof Error ? error.message : String(error),
      };
      throw error;
    } finally {
      closeObservation?.();
      await native.close();
      await writeJourneyArtifact(
        join(runDirectory!, "work/observation-lifecycle-evidence.json"),
        {
          deploymentFingerprint: authority.deploymentIdentity.manifestId,
          observedAt: new Date().toISOString(),
          durationMs: performance.now() - started,
          outcome,
          samples,
        },
      );
    }
  },
  390_000,
);
