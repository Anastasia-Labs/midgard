/**
 * The operations server binds before the L1-dependent startup stages, so an
 * unanswering node at startup leaves the watcher live and unready with the
 * waiting stage named, never exited.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { createServer } from "node:net";

import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import { generatePrivateKey } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.js";
import { writeWatcherRuntimeProcessConfig } from "../support/watcher-runtime-process-config.js";

/** The node: down for every read until the test brings it up. */
const node = vi.hoisted(() => ({
  readinessDown: true,
  queriesDown: true,
  readinessReads: 0,
  queries: 0,
}));

const nodeDown = () =>
  new L1ProviderTransientError("transport", "the node socket did not accept");

// Everything past the operations server that needs a deployed chain is
// replaced; the startup stages, their retries and the server are real.
vi.mock("../../src/runtime/deployment-identity.js", async (load) => ({
  ...(await load<typeof import("../../src/runtime/deployment-identity.js")>()),
  verifyWatcherUserEventScriptBinding: () => ({}),
  readWatcherUserEventScriptBinding: () => ({
    eventProjection: {},
    deposit: { policyId: "" },
    withdrawal: { policyId: "" },
    forcedOrder: { policyId: "", addressHex: "" },
  }),
}));
vi.mock(
  "../../src/funding/workflow-funding-profile-overlay.js",
  async (load) => ({
    ...(await load<
      typeof import("../../src/funding/workflow-funding-profile-overlay.js")
    >()),
    loadWatcherWorkflowFundingProfileOverlay: async () => ({}),
  }),
);
vi.mock(
  "../../src/l1/native-chain-sync.derive-watcher-native-genesis-identity.js",
  () => ({
    deriveWatcherNativeGenesisIdentity: async () => ({ networkMagic: 1 }),
  }),
);
vi.mock("../../src/l1-follower/deployment-follower.js", () => ({
  watcherFollowedScripts: () => [],
  openWatcherDeploymentFollower: () => ({
    store: {},
    rawReads: {},
    provider: {},
    proofRetention: {},
    faultProofL1: {},
    fundingInputFacts: undefined,
    transport: {
      query: async () => {
        node.queries += 1;
        if (node.queriesDown) throw nodeDown();
        throw new Error("protocol parameters are malformed");
      },
    },
    close: async () => undefined,
  }),
}));
vi.mock("../../src/l1-follower/user-events.js", async (load) => ({
  ...(await load<typeof import("../../src/l1-follower/user-events.js")>()),
  createWatcherFollowerUserEvents: () => ({ close: async () => undefined }),
}));
// The workflow readiness stage, reduced to its L1 read.
vi.mock("../../src/runtime/watcher-runtime.prepare-services.js", () => ({
  prepareWatcherRuntimeWorkflows: async (
    _input: unknown,
    options: {
      startup: (
        stage: string,
        action: (context: {
          retryL1Read: <U>(read: () => Promise<U>) => Promise<U>;
        }) => Promise<unknown>,
      ) => Promise<unknown>;
    },
  ) =>
    await options.startup("workflow_readiness", ({ retryL1Read }) =>
      retryL1Read(async () => {
        node.readinessReads += 1;
        if (node.readinessDown) throw nodeDown();
        return {
          faultProofApplication: {
            installedCategories: [],
            close: async () => undefined,
          },
          faultProofReadiness: [],
        };
      }),
    ),
}));

const directories: string[] = [];
afterEach(async () => {
  vi.unstubAllEnvs();
  vi.restoreAllMocks();
  Object.assign(node, {
    readinessDown: true,
    queriesDown: true,
    readinessReads: 0,
    queries: 0,
  });
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => rm(path, { recursive: true, force: true })),
  );
});

const freePort = async (): Promise<number> =>
  await new Promise((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      server.close(() =>
        typeof address === "object" && address !== null
          ? resolve(address.port)
          : reject(new Error("no port")),
      );
    });
  });

const processConfig = async (): Promise<WatcherProcessConfig> => {
  const directory = await mkdtemp("/var/tmp/midgard-watcher-startup-ready-");
  directories.push(directory);
  vi.stubEnv("MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY", "11".repeat(32));
  vi.stubEnv("MIDGARD_WATCHER_PROVER_KEY", generatePrivateKey());
  vi.stubEnv("WATCHER_AVAILABILITY_KEY", generatePrivateKey());
  return {
    ...(await writeWatcherRuntimeProcessConfig(directory)),
    operationsEndpoint: `http://127.0.0.1:${(await freePort()).toString()}`,
  };
};

type Probe = Readonly<{
  status: number;
  body: {
    reasons?: string[];
    readinessReasons?: string[];
    startup?: { stage: string; outcome: string; error?: string };
  };
}>;

const probe = async (
  config: WatcherProcessConfig,
  path: string,
): Promise<Probe | undefined> => {
  try {
    const response = await fetch(`${config.operationsEndpoint}${path}`);
    return {
      status: response.status,
      body: (await response.json()) as Probe["body"],
    };
  } catch {
    return undefined;
  }
};

describe("watcher startup readiness", () => {
  it("answers /readyz 503 naming the stage while the node is down at startup, and does not exit", async () => {
    const config = await processConfig();
    const exit = vi.spyOn(process, "exit");
    let settled: unknown;
    const starting = createWatcherRuntime({ config }).then(
      () => (settled = "started"),
      (error: unknown) => (settled = error),
    );

    // The protocol parameters read reports the node's outage as its own.
    for (const [stage, reads, error] of [
      ["workflow_readiness", () => node.readinessReads, nodeDown().message],
      [
        "protocol_parameters",
        () => node.queries,
        "Current local funding parameters are temporarily unavailable",
      ],
    ] as const) {
      await vi.waitFor(
        async () => {
          expect(reads()).toBeGreaterThanOrEqual(2);
          expect(await probe(config, "/readyz")).toEqual({
            status: 503,
            body: {
              ready: false,
              reasons: [`startup:${stage}`],
              l1: [],
              startup: {
                stage,
                outcome: "pending",
                error,
                retryAfterMs: expect.any(Number),
              },
            },
          });
        },
        { timeout: 20_000, interval: 50 },
      );
      // The liveness probe answers while the stage waits.
      expect(await probe(config, "/v1/status")).toMatchObject({
        status: 200,
        body: { readinessReasons: [`startup:${stage}`] },
      });
      expect(settled).toBeUndefined();
      expect(exit).not.toHaveBeenCalled();
      node.readinessDown = false;
    }

    // A failure that is not an L1 transient ends startup, and the server
    // bound for it closes with the rest.
    node.queriesDown = false;
    await starting;
    expect(settled).toBeInstanceOf(Error);
    expect((settled as Error).message).toBe(
      "protocol parameters are malformed",
    );
    expect(await probe(config, "/readyz")).toBeUndefined();
  }, 60_000);
});
