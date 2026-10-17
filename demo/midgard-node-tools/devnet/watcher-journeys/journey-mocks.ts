import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { vi } from "vitest";

const state = vi.hoisted(() => ({
  runDirectory: "",
  closed: [] as string[],
  launches: [] as { command: string; configPath: string; directory: string }[],
  watcherState: { state: "running", exitCode: null as number | null },
  bindingCalls: 0,
  failBinding: false,
  failNativeClose: false,
  sessionSources: [] as string[],
  replayCheckpointDirectories: [] as string[],
}));
vi.mock("../../../../scripts/lib/source-facets.mjs", () => ({
  sourceFacetPaths: () => state.sessionSources,
}));
vi.mock("./journey-timing.js", () => ({
  readJourneyTiming: async () => ({ allowances: {} }),
}));
vi.mock("./live-context.js", async (original) => ({
  ...(await original<typeof import("./live-context.js")>()),
  loadJourneyContext: async () => ({
    runDirectory: state.runDirectory,
    deployment: {
      manifest: { manifestId: "ab".repeat(32), hubOracleOneShot: {} },
      contracts: { computationThread: { policyId: "cd".repeat(28) } },
    },
    provider: {},
    accounts: {
      publisher: { seedPhrase: "publisher" },
      availability: { seedPhrase: "availability" },
    },
    runEnv: {},
    customNetwork: {},
    ogmiosUrl: "http://127.0.0.1:1337",
    kupoUrl: "http://127.0.0.1:1442",
  }),
}));
vi.mock("midgard-watcher", async (original) => ({
  ...(await original<typeof import("midgard-watcher")>()),
  loadWatcherVerifiedDeploymentAuthority: async () => ({
    deploymentIdentity: { manifestId: "ab".repeat(32) },
  }),
  parseWatcherConfig: (value: unknown) => value,
  parseWatcherProcessConfig: (value: unknown) => value,
  watcherDeploymentReleaseFinalityAuthority: () => ({
    verifyForWorkflow: async () => ({
      policy: { confirmationDepth: 30 },
      policyDigest: "ef".repeat(32),
    }),
  }),
}));
vi.mock("midgard-watcher/tests/support/published-deployment-authority", () => ({
  createPublishedWatcherDeploymentAuthority: async () => {
    const path = join(state.runDirectory, "binding.json");
    return {
      deploymentAuthority: {
        deploymentIdentity: { manifestId: "ab".repeat(32) },
      },
      authorityPath: path,
      ruleBundlePath: path,
      fundingBundlePath: path,
      fundingProfileBundlePath: path,
      manifestPath: path,
      blueprintPath: path,
      deploymentInfoPath: path,
    };
  },
}));
vi.mock("./retained-da.js", () => ({
  startJourneyRetainedDa: async () => ({
    peer: { identity: "peer", multiaddr: "address" },
    retain: vi.fn(),
    close: async () => {
      state.closed.push("da");
    },
  }),
}));
vi.mock("./native-node.js", () => ({
  journeyNativeNodeQuery: async () => ({
    watcherConfig: {
      l1: { source: { sourceMode: "local_node", chainSync: {} } },
    },
    binaryPath: join(state.runDirectory, "binding.json"),
  }),
}));
vi.mock("../../src/l1-origin.js", () => ({
  deriveL1Origin: async () => ({ origin: { slot: 7, blockHash: "34" } }),
}));
vi.mock("./native-recorder.js", () => ({
  startJourneyNativeRecorder: async ({ directory }: { directory: string }) => ({
    nativeEvidencePath: join(directory, "native-chain.ndjson"),
    assertHealthy: vi.fn(),
    transaction: vi.fn(),
    includedThrough: vi.fn(),
    observedBlockNo: () => undefined,
    close: async () => {
      state.closed.push("native");
      if (state.failNativeClose) throw new Error("native close failed");
    },
  }),
}));
vi.mock("./process.js", () => ({
  journeyPorts: async () => [1235, 1236],
  launchJourneyWatcherProcess: (input: {
    command: string;
    configPath: string;
    directory: string;
  }) => {
    state.launches.push(input);
    const pid = state.launches.length;
    if (input.command === "start")
      state.watcherState = { state: "running", exitCode: null };
    return {
      observe: () => ({
        pid,
        attempt: `attempt-${pid}`,
        startedAt: "2026-09-14T00:00:00.000Z",
        tail: [],
        ...(input.command === "start"
          ? state.watcherState
          : { state: "running", exitCode: null }),
      }),
      assertHealthy: vi.fn(),
      close: async () => {
        state.closed.push(`${input.command}-${pid}`);
      },
    };
  },
}));
vi.mock("./workflow-binding-preflight.js", () => ({
  verifyJourneyWorkflowBindings: async () => {
    state.bindingCalls++;
    if (state.failBinding) throw new Error("binding rejected");
    const path = join(state.runDirectory, "binding.json");
    return {
      configurationFiles: [
        {
          path,
          sha256: createHash("sha256")
            .update(await readFile(path))
            .digest("hex"),
        },
      ],
      sourceIdentity: { files: [] },
    };
  },
}));

vi.mock("./replayed-ledger.js", () => ({
  openJourneyReplayedLedger: async (input: {
    checkpointDirectory: string;
    authority: { deploymentIdentity: { manifestId: string } };
  }) => {
    state.replayCheckpointDirectories.push(input.checkpointDirectory);
    const { fileLedgerCheckpointStore } = await import(
      "@al-ft/midgard-fault-proofs"
    );
    const checkpoints = fileLedgerCheckpointStore(
      join(
        input.checkpointDirectory,
        `replayed-ledger-${input.authority.deploymentIdentity.manifestId}.json`,
      ),
    );
    return {
      ledgerAt: async (headerHash: string) => {
        await checkpoints.write({ headerHash, entries: [] });
        return [];
      },
      close: async () => {},
    };
  },
}));

export { state };
