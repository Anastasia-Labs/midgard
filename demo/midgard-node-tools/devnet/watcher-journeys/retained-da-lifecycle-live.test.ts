import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  createWatcherRetainedDaRuntimeOwner,
  loadWatcherVerifiedDeploymentAuthority,
  parseWatcherConfig,
  parseWatcherProcessConfig,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { loadJourneyContext } from "./live-context.js";
import { startJourneyRetainedDa } from "./retained-da.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const concurrentLeases = Number(
  process.env.MIDGARD_WATCHER_DA_CONCURRENT_LEASES ?? "2",
);
if (
  !Number.isSafeInteger(concurrentLeases) ||
  concurrentLeases < 1 ||
  concurrentLeases > 8
)
  throw new Error("Concurrent DA leases must be an integer from 1 to 8");
const continuousSeconds = Number(
  process.env.MIDGARD_WATCHER_DA_CONTINUOUS_SECONDS ?? "0",
);
if (
  !Number.isSafeInteger(continuousSeconds) ||
  continuousSeconds < 0 ||
  continuousSeconds > 1800
)
  throw new Error("Continuous DA observation must be 0–1800 whole seconds");

it.skipIf(runDirectory === undefined)(
  "preserves actual public DA retrieval through client turnover and idle time",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const processConfig = parseWatcherProcessConfig(
      JSON.parse(
        await readFile(join(directory, "watcher-process.json"), "utf8"),
      ),
    );
    const authority = await loadWatcherVerifiedDeploymentAuthority({
      path: processConfig.deploymentAuthorityPath,
      ruleBundlePath: processConfig.ruleBundlePath,
    });
    const configured = processConfig.watcherConfig;
    type Block = { headerHash: string; payloadEnvelopeCbor: Uint8Array };
    const staged = await readJourneyArtifact<{
      predecessor: Block;
      current: Block;
    }>(join(directory, "staged.json"));
    const server = await startJourneyRetainedDa({
      runDirectory: runDirectory!,
      runEnv: context.runEnv,
      deploymentFingerprint: context.deployment.manifest.manifestId,
    });
    const watcherConfig = parseWatcherConfig({
      ...configured,
      da: { ...configured.da, peers: [server.peer] },
    });
    const owner = createWatcherRetainedDaRuntimeOwner({
      deploymentIdentity: authority.deploymentIdentity,
    });
    const observations: unknown[] = [];
    const started = performance.now();
    const fetchBoth = async (label: string) => {
      const runtime = await owner.createRuntime(watcherConfig);
      try {
        for (const block of [staged.predecessor, staged.current]) {
          const result = await runtime.sources[0]!.fetchPayloadByHeaderHash(
            block.headerHash,
          );
          observations.push({
            label,
            elapsedMs: performance.now() - started,
            headerHash: block.headerHash,
            ok: result.ok,
            attempts: result.attempts,
          });
          if (!result.ok)
            throw new Error(
              `Public DA lifecycle failure: ${JSON.stringify(result)}`,
            );
          expect(Buffer.from(result.payloadEnvelopeCbor)).toEqual(
            Buffer.from(block.payloadEnvelopeCbor),
          );
          expect(result.sourcePeerId).toBe(watcherConfig.da.peers[0]!.peerId);
        }
      } finally {
        await runtime.close();
      }
    };
    let outcome = "failed";
    let failure: string | undefined;
    const fetchRound = async (label: string) => {
      const results = await Promise.allSettled(
        Array.from({ length: concurrentLeases }, (_, index) =>
          fetchBoth(`${label}-${index}`),
        ),
      );
      for (const result of results)
        if (result.status === "rejected") throw result.reason;
    };
    try {
      if (continuousSeconds > 0) {
        const deadline = performance.now() + continuousSeconds * 1000;
        let round = 0;
        let nextProgressAt = performance.now();
        while (performance.now() < deadline) {
          await fetchRound(`continuous-${round++}`);
          if (performance.now() >= nextProgressAt) {
            console.info("Public DA continuous retrieval", {
              elapsedMs: performance.now() - started,
              completedRequests: observations.length,
            });
            nextProgressAt = performance.now() + 30_000;
          }
          await pause(
            Math.min(1000, Math.max(0, deadline - performance.now())),
          );
        }
      } else {
        for (let round = 0; round < 32; round++) {
          await fetchRound(`before-${round}`);
        }
        console.info(
          "Public DA turnover probe passed; observing 330 seconds of idle time",
        );
        await pause(330_000);
        for (let round = 0; round < 32; round++) {
          await fetchRound(`after-${round}`);
        }
      }
      outcome = "passed";
    } catch (error) {
      failure = error instanceof Error ? error.message : String(error);
      throw error;
    } finally {
      await writeJourneyArtifact(
        join(
          runDirectory!,
          continuousSeconds > 0
            ? "work/retained-da-continuous-evidence.json"
            : "work/retained-da-lifecycle-evidence.json",
        ),
        {
          deploymentFingerprint: context.deployment.manifest.manifestId,
          continuousSeconds,
          concurrentLeases,
          durationMs: performance.now() - started,
          outcome,
          failure,
          observations,
        },
      );
      try {
        await owner.close();
      } finally {
        await server.close();
      }
    }
  },
  Math.max(480_000, continuousSeconds * 1000 + 120_000),
);
