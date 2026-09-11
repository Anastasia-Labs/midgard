import { mkdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  loadWatcherVerifiedDeploymentAuthority,
  parseWatcherConfig,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { loadJourneyContext } from "./live-context.js";
import { verifyJourneyPublicDa } from "./public-da-preflight.js";
import { startJourneyRetainedDa } from "./retained-da.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
type RetainedBlock = {
  headerHash: string;
  payloadEnvelopeCbor: Uint8Array;
};

it.skipIf(runDirectory === undefined)(
  "retrieves the committed journey payloads through production public DA",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const authority = await loadWatcherVerifiedDeploymentAuthority({
      path: join(directory, "deployment-authority.json"),
      ruleBundlePath: join(directory, "rules.json"),
    });
    const config = JSON.parse(
      await readFile(join(directory, "watcher.json"), "utf8"),
    );
    const staged = await readJourneyArtifact<{
      predecessor: RetainedBlock;
      current: RetainedBlock;
    }>(join(directory, "staged.json"));
    const server = await startJourneyRetainedDa({
      runDirectory: runDirectory!,
      runEnv: context.runEnv,
      deploymentFingerprint: context.deployment.manifest.manifestId,
    });
    try {
      const probeDirectory = join(runDirectory!, "work/public-da-probe");
      await mkdir(probeDirectory, { recursive: true });
      const evidence = await verifyJourneyPublicDa({
        directory: probeDirectory,
        watcherConfig: parseWatcherConfig({
          ...config,
          da: { ...config.da, peers: [server.peer] },
        }),
        deploymentIdentity: authority.deploymentIdentity,
        predecessor: staged.predecessor,
        current: staged.current,
      });
      expect(evidence.outcome).toBe("passed");
      expect(evidence.receipts).toHaveLength(2);
    } finally {
      await server.close();
    }
  },
  120_000,
);
