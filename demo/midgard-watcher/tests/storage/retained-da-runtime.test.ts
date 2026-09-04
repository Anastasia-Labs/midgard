import { mkdtemp, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  createManifestBoundWorkflowRunner,
  WORKFLOW_RUNTIME_CONFIG,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import type { WatcherOperationsSink } from "../../src/runtime/operations-observability.js";
import { WatcherPublicDaLibp2pTransport } from "../../src/storage/public-da-libp2p-transport.js";
import {
  bindWatcherRetainedDaOperations,
  createWatcherRetainedDaRuntime,
  createWatcherWorkflowRuntimeLoader,
  WATCHER_RETAINED_DA_RUNTIME,
} from "../../src/storage/retained-da-runtime.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const AUTHORITY = makeWatcherDeploymentAuthorityFixture();
const DEPLOYMENT = AUTHORITY.result.manifestId;
const PEER_ID = "12D3KooWAbcdefghijkmnopqrstuvwxyz12345";

type TestDoubleSpendWorkflow = Readonly<{
  binding: Readonly<{
    deploymentFingerprint: string;
    definition: Readonly<{
      category: "doubleSpend";
      headerHash: string;
    }>;
  }>;
}>;

const rawConfig = (multiaddr = `/dns4/da-a.example/tcp/443/p2p/${PEER_ID}`) =>
  ({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: {
      source: {
        sourceMode: "external_providers",
        providers: [
          {
            identity: "provider-a",
            operatorIdentitySha256: "11".repeat(32),
            endpoint: "https://cardano-a.example",
          },
          {
            identity: "provider-b",
            operatorIdentitySha256: "22".repeat(32),
            endpoint: "https://cardano-b.example",
          },
        ],
      },
      requestTimeoutMs: 10_000,
      maxConcurrency: 8,
      finality: {
        depth: 30,
        rollback: {
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: 30,
        },
      },
    },
    da: {
      peers: [{ identity: "da-peer-a", multiaddr }],
      requestTimeoutMs: 10_000,
      maxConcurrency: 8,
    },
    storage: {
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
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
  }) as const;

const deploymentIdentity = (): VerifiedWatcherDeploymentIdentity =>
  AUTHORITY.result;

const transportFactory = () => {
  const request = vi.fn(async () => new Uint8Array([0xf6]));
  const stop = vi.fn(async () => undefined);
  const transport = Object.create(
    WatcherPublicDaLibp2pTransport.prototype,
  ) as WatcherPublicDaLibp2pTransport;
  Object.defineProperties(transport, {
    request: { value: request },
    stop: { value: stop },
  });
  return { request, stop, factory: vi.fn(async () => transport) };
};

const invocation = (
  overrides: Partial<WorkflowAdapterRunnerInput> = {},
): WorkflowAdapterRunnerInput => ({
  mode: "run",
  category: "doubleSpend",
  deploymentFingerprint: DEPLOYMENT,
  headerHash: "ab".repeat(28),
  decisionDigest: "cd".repeat(32),
  actuationPermit: Object.freeze({
    permitVersion: "midgard-production-workflow-actuation-permit-v1",
  }),
  fundingReservationPermit: Object.freeze({
    permitVersion: "midgard-production-workflow-funding-reservation-permit-v1",
  }),
  journalDirectory: "/var/lib/midgard-watcher/fraud-proof-journals",
  runtimeConfigPath: "/etc/midgard/watcher-v1.json",
  ...overrides,
});

describe("production retained-DA runtime V1", () => {
  it("constructs one concrete deployment-bound source per admitted public peer and closes once", async () => {
    const fake = transportFactory();
    const recordDaFetch = vi.fn();
    const setAlert = vi.fn();
    const operationsBinding = bindWatcherRetainedDaOperations({
      deploymentIdentity: deploymentIdentity(),
      sink: Object.freeze({
        recordDaFetch,
        setAlert,
      }) as unknown as WatcherOperationsSink,
    });
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig: rawConfig(),
      deploymentIdentity: deploymentIdentity(),
      unsafeTransportFactoryForTest: fake.factory,
    });
    expect(runtime.schemaVersion).toBe(WATCHER_RETAINED_DA_RUNTIME);
    expect(runtime.deploymentFingerprint).toBe(DEPLOYMENT);
    expect(runtime.sources).toHaveLength(1);
    expect(runtime.sources[0]?.sourceId).toBe("watcher-public-da/da-peer-a");

    const source = runtime.sources[0]!;
    await source.fetchPayloadByHeaderHash("ab".repeat(28));
    expect(recordDaFetch).toHaveBeenCalledWith(
      expect.objectContaining({ outcome: "succeeded" }),
    );
    expect(setAlert).toHaveBeenCalledWith(
      expect.objectContaining({ code: "da_fetch_failure", active: false }),
    );
    expect(fake.request).toHaveBeenCalledWith(
      expect.objectContaining({
        peerId: PEER_ID,
        multiaddr: `/dns4/da-a.example/tcp/443/p2p/${PEER_ID}`,
        protocol: DaRequestResponseProtocol.payloadByHeader,
        protocolId: `/midgard/${DEPLOYMENT}/da/payload-by-header/1`,
        timeoutMs: 10_000,
      }),
    );
    await runtime.close();
    await runtime.close();
    operationsBinding.close();
    expect(fake.stop).toHaveBeenCalledTimes(1);
  });

  it.each([
    `/dns4/localhost/tcp/443/p2p/${PEER_ID}`,
    `/dns4/da-a.local/tcp/443/p2p/${PEER_ID}`,
    `/dns4/10.0.0.1/tcp/443/p2p/${PEER_ID}`,
    `/ip4/203.0.113.1/tcp/443/p2p/${PEER_ID}`,
  ])(
    "rejects local/private/non-public source substitution: %s",
    async (multiaddr) => {
      const fake = transportFactory();
      await expect(
        createWatcherRetainedDaRuntime({
          watcherConfig: rawConfig(multiaddr),
          deploymentIdentity: deploymentIdentity(),
          unsafeTransportFactoryForTest: fake.factory,
        }),
      ).rejects.toThrow();
      expect(fake.factory).not.toHaveBeenCalled();
    },
  );

  it("rejects a foreign network or structurally forged deployment identity before transport startup", async () => {
    const wrongNetwork = transportFactory();
    await expect(
      createWatcherRetainedDaRuntime({
        watcherConfig: { ...rawConfig(), targetNetwork: "Mainnet" },
        deploymentIdentity: deploymentIdentity(),
        unsafeTransportFactoryForTest: wrongNetwork.factory,
      }),
    ).rejects.toThrow("network differs");
    expect(wrongNetwork.factory).not.toHaveBeenCalled();

    const wrongMarker = transportFactory();
    await expect(
      createWatcherRetainedDaRuntime({
        watcherConfig: rawConfig(),
        deploymentIdentity: {
          ...deploymentIdentity(),
          durableMarker: makeDeploymentMarker("ff".repeat(32)),
        },
        unsafeTransportFactoryForTest: wrongMarker.factory,
      }),
    ).rejects.toThrow("invalid_field at $.verifiedDeploymentIdentity");
    expect(wrongMarker.factory).not.toHaveBeenCalled();
  });

  it("rejects a symlinked runtime config path before transport startup", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-public-da-runtime-path-"),
    );
    const canonicalPath = join(directory, "watcher.json");
    const symlinkPath = join(directory, "watcher-link.json");
    const fake = transportFactory();
    try {
      await writeFile(canonicalPath, JSON.stringify(rawConfig()));
      await symlink(canonicalPath, symlinkPath);
      const loader = createWatcherWorkflowRuntimeLoader({
        deploymentIdentity: deploymentIdentity(),
        unsafeTransportFactoryForTest: fake.factory,
        buildInfrastructureConfig: async () => ({ ok: true }),
      });
      await expect(
        loader({
          runtimeConfigPath: symlinkPath,
          invocation: invocation({ runtimeConfigPath: symlinkPath }),
        }),
      ).rejects.toThrow("must not traverse a symlink");
      expect(fake.factory).not.toHaveBeenCalled();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("rejects development mode before transport startup", async () => {
    const fake = transportFactory();
    await expect(
      createWatcherRetainedDaRuntime({
        watcherConfig: { ...rawConfig(), mode: "development" },
        deploymentIdentity: deploymentIdentity(),
        unsafeTransportFactoryForTest: fake.factory,
      }),
    ).rejects.toThrow("requires an admitted acceptance-mode");
    expect(fake.factory).not.toHaveBeenCalled();
  });

  it("loads the shared runner shape from strict config and transfers close ownership", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-public-da-runtime-"),
    );
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const fake = transportFactory();
    const buildInfrastructureConfig = vi.fn(async ({ invocation: call }) => ({
      category: call.category,
      headerHash: call.headerHash,
    }));
    try {
      const loader = createWatcherWorkflowRuntimeLoader({
        deploymentIdentity: deploymentIdentity(),
        unsafeTransportFactoryForTest: fake.factory,
        buildInfrastructureConfig,
      });
      const call = invocation({ runtimeConfigPath: configPath });
      const loaded = await loader({
        runtimeConfigPath: configPath,
        invocation: call,
      });
      expect(loaded.schemaVersion).toBe(WORKFLOW_RUNTIME_CONFIG);
      expect(loaded.retainedDaSources).toHaveLength(1);
      expect(loaded.config).toEqual({
        category: "doubleSpend",
        headerHash: call.headerHash,
      });
      expect(buildInfrastructureConfig).toHaveBeenCalledWith(
        expect.objectContaining({ invocation: call }),
      );
      await loaded.close();
      expect(fake.stop).toHaveBeenCalledTimes(1);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("snapshots verified authority and builders before asynchronous config I/O", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-public-da-runtime-authority-snapshot-"),
    );
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const originalTransport = transportFactory();
    const substitutedTransport = transportFactory();
    const originalBuilder = vi.fn(async () => ({ authority: "original" }));
    const substitutedBuilder = vi.fn(async () => ({
      authority: "substituted",
    }));
    const substituteAuthority = makeWatcherDeploymentAuthorityFixture({
      releaseDigest: "33".repeat(32),
    });
    const mutableOptions = {
      deploymentIdentity: deploymentIdentity(),
      unsafeTransportFactoryForTest: originalTransport.factory,
      buildInfrastructureConfig: originalBuilder,
    };
    try {
      const loader = createWatcherWorkflowRuntimeLoader(mutableOptions);
      const pending = loader({
        runtimeConfigPath: configPath,
        invocation: invocation({ runtimeConfigPath: configPath }),
      });
      mutableOptions.deploymentIdentity = substituteAuthority.result;
      mutableOptions.unsafeTransportFactoryForTest =
        substitutedTransport.factory;
      mutableOptions.buildInfrastructureConfig = substitutedBuilder;

      const loaded = await pending;
      expect(loaded.config).toEqual({ authority: "original" });
      expect(originalBuilder).toHaveBeenCalledTimes(1);
      expect(substitutedBuilder).not.toHaveBeenCalled();
      expect(originalTransport.factory).toHaveBeenCalledTimes(1);
      expect(substitutedTransport.factory).not.toHaveBeenCalled();
      await loaded.close();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("rejects invocation substitution before I/O and closes transport when infrastructure construction fails", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-public-da-runtime-"),
    );
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    try {
      const substituted = transportFactory();
      const loader = createWatcherWorkflowRuntimeLoader({
        deploymentIdentity: deploymentIdentity(),
        unsafeTransportFactoryForTest: substituted.factory,
        buildInfrastructureConfig: async () => ({ ok: true }),
      });
      await expect(
        loader({
          runtimeConfigPath: configPath,
          invocation: invocation({ deploymentFingerprint: "ff".repeat(32) }),
        }),
      ).rejects.toThrow("invocation deployment differs");
      expect(substituted.factory).not.toHaveBeenCalled();

      const failed = transportFactory();
      const failingLoader = createWatcherWorkflowRuntimeLoader({
        deploymentIdentity: deploymentIdentity(),
        unsafeTransportFactoryForTest: failed.factory,
        buildInfrastructureConfig: async () => {
          throw new Error("infrastructure refused");
        },
      });
      await expect(
        failingLoader({
          runtimeConfigPath: configPath,
          invocation: invocation({ runtimeConfigPath: configPath }),
        }),
      ).rejects.toThrow("infrastructure refused");
      expect(failed.stop).toHaveBeenCalledTimes(1);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("rejects a structural actuation permit before the watcher loader allocates transport", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-public-da-runtime-runner-"),
    );
    const configPath = join(directory, "watcher.json");
    await writeFile(configPath, JSON.stringify(rawConfig()));
    const call = invocation({ runtimeConfigPath: configPath });
    try {
      const transport = transportFactory();
      const builder = vi.fn(
        async ({
          invocation: exactInvocation,
        }): Promise<TestDoubleSpendWorkflow> => ({
          binding: {
            deploymentFingerprint: exactInvocation.deploymentFingerprint,
            definition: {
              category: "doubleSpend",
              headerHash: exactInvocation.headerHash,
            },
          },
        }),
      );
      const runner = createManifestBoundWorkflowRunner({
        category: "doubleSpend",
        loadRuntimeConfig:
          createWatcherWorkflowRuntimeLoader<TestDoubleSpendWorkflow>({
            deploymentIdentity: deploymentIdentity(),
            unsafeTransportFactoryForTest: transport.factory,
            buildInfrastructureConfig: builder,
          }),
        constructWorkflow: async (config) => config,
        execute: async () => {
          throw new Error("structural permit reached execution");
        },
      });
      await expect(runner.runOrResume(call)).rejects.toThrow(
        "actuation permit was not admitted",
      );
      expect(builder).not.toHaveBeenCalled();
      expect(transport.factory).not.toHaveBeenCalled();
      expect(transport.stop).not.toHaveBeenCalled();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
