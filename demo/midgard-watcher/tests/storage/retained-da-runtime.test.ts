import { mkdtemp, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  DaRequestResponseProtocol,
  encodeDaPayloadByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  createManifestBoundWorkflowRunner,
  DaLibp2pRetainedDaSource,
  type RetainedDaFetchAttempt,
  type RetainedDaPayloadSourceResult,
  WORKFLOW_RUNTIME_CONFIG,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import type { WatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
} from "../../src/runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import {
  createWatcherOperationsObservability,
  type WatcherOperationsSink,
} from "../../src/runtime/operations-observability.js";
import {
  WatcherPublicDaClient,
  type WatcherPublicDaRequest,
} from "../../src/storage/public-da-client.js";
import {
  encodeWatcherPublicDaFrame,
  WatcherPublicDaLibp2pTransport,
} from "../../src/storage/public-da-libp2p-transport.js";
import {
  bindWatcherRetainedDaOperations,
  createWatcherRetainedDaRuntime,
  createWatcherWorkflowRuntimeLoader,
  WATCHER_RETAINED_DA_RUNTIME,
  WatcherRetainedDaSourceWithL1Fallback,
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

describe("retained-DA fallback attempt history", () => {
  const headerHash = "ab".repeat(28);
  const publicAttempt: RetainedDaFetchAttempt = {
    sourceId: "public-da",
    sourcePeerId: PEER_ID,
    protocol: DaRequestResponseProtocol.payloadByHeader,
    status: "transport_error",
    detail: "Public peer connection failed",
  };
  const fallbackAttempt: RetainedDaFetchAttempt = {
    ...publicAttempt,
    sourceId: "l1-availability",
    sourcePeerId: "cardano-l1",
    status: "not_found",
    detail: "No published payload in canonical history",
  };
  const success: RetainedDaPayloadSourceResult = {
    ok: true,
    sourceId: "l1-availability",
    sourcePeerId: "cardano-l1",
    payloadEnvelopeCbor: Buffer.from("public payload"),
    provenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "l1-availability",
      grade: "security",
    },
    metadata: { publication: "canonical" },
    attempts: [],
  };

  const compose = (fallbackResult: RetainedDaPayloadSourceResult) => {
    const fetch = vi.fn(async () => fallbackResult);
    const source = new WatcherRetainedDaSourceWithL1Fallback(
      {
        sourceId: "public-da",
        deploymentFingerprint: DEPLOYMENT,
        peers: [{ peerId: PEER_ID }],
        transport: {
          request: async () => {
            throw new Error("Public peer connection failed");
          },
        },
      },
      { sourceId: "l1-availability", fetchPayloadByHeaderHash: fetch },
    );
    return { source, fetch };
  };

  it.each([{ attempts: [] }, { attempts: [fallbackAttempt] }])(
    "preserves failed public attempts with fallback attempts %j",
    async ({ attempts }) => {
      const fallback = Object.freeze({
        ok: false as const,
        sourceId: "l1-availability",
        attempts: Object.freeze(attempts),
      });
      const { source, fetch } = compose(fallback);
      const expectedPublic = await new DaLibp2pRetainedDaSource({
        sourceId: "public-da",
        deploymentFingerprint: DEPLOYMENT,
        peers: [{ peerId: PEER_ID }],
        transport: {
          request: async () => {
            throw new Error("Public peer connection failed");
          },
        },
      }).fetchPayloadByHeaderHash(headerHash);
      expect(expectedPublic.ok).toBe(false);
      expect(expectedPublic.attempts.length).toBeGreaterThan(0);
      const result = await source.fetchPayloadByHeaderHash(headerHash);
      expect(result).toEqual({
        ...fallback,
        attempts: [...expectedPublic.attempts, ...attempts],
      });
      expect(fetch).toHaveBeenCalledExactlyOnceWith(headerHash);
      expect(fallback.attempts).toEqual(attempts);
    },
  );

  it("retains failed public attempts without relabeling successful L1 provenance or payload", async () => {
    const { source } = compose(success);
    const result = await source.fetchPayloadByHeaderHash(headerHash);
    expect({ ...result, attempts: [] }).toEqual(success);
    expect(result.attempts).not.toHaveLength(0);
    if (!result.ok) throw new Error("Expected successful fallback");
    expect(result.provenance).toBe(success.provenance);
    expect(result.payloadEnvelopeCbor).toBe(success.payloadEnvelopeCbor);
    expect(result.metadata).toBe(success.metadata);
    expect(success.attempts).toEqual([]);
  });

  it("returns public success unchanged without calling L1", async () => {
    const publicSuccess = {
      ...success,
      sourceId: "public-da",
      attempts: [publicAttempt],
    };
    const original = vi
      .spyOn(DaLibp2pRetainedDaSource.prototype, "fetchPayloadByHeaderHash")
      .mockResolvedValue(publicSuccess);
    try {
      const { source, fetch } = compose(success);
      expect(await source.fetchPayloadByHeaderHash(headerHash)).toBe(
        publicSuccess,
      );
      expect(fetch).not.toHaveBeenCalled();
    } finally {
      original.mockRestore();
    }
  });
});

describe("explicit Custom DA transport", () => {
  const peerId = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
  const address = `/ip4/127.0.0.1/tcp/4141/p2p/${peerId}`;
  const config = () => {
    const common = rawConfig(address);
    return parseWatcherConfig({
      ...common,
      targetNetwork: "Custom",
      customNetwork: {
        networkMagic: 424242,
        slotConfig: { zeroTime: 1789056000000, zeroSlot: 0, slotLength: 1000 },
      },
      l1: {
        ...common.l1,
        source: {
          sourceMode: "local_node",
          authorityNodeId: "watcher-node",
          chainSync: {
            kind: "cardano_node_socket",
            socketPath: "/run/cardano/node.socket",
            nodeConfigPath: "/etc/cardano/node-config.json",
            genesisConfigPath: "/etc/cardano/shelley-genesis.json",
            genesisIdentitySha256: "33".repeat(32),
          },
          queryServices: [
            {
              kind: "ogmios",
              identity: "local-ogmios",
              endpoint: "ws://127.0.0.1:1337",
            },
            {
              kind: "kupo",
              identity: "local-kupo",
              endpoint: "http://127.0.0.1:1442",
            },
          ],
        },
      },
    });
  };
  const transportFixture = (response = Buffer.from([0xf6])) => {
    let authenticatedPeer = peerId;
    const abort = vi.fn();
    const dialProtocol = vi.fn(async () => ({
      send: () => true,
      close: async () => undefined,
      abort,
      async *[Symbol.asyncIterator]() {
        yield encodeWatcherPublicDaFrame(response);
      },
    }));
    const transport = new WatcherPublicDaLibp2pTransport({
      libp2pFactory: async () => ({
        start: async () => undefined,
        stop: async () => undefined,
        dialProtocol,
        getConnections: () => [
          { remotePeer: { toString: () => authenticatedPeer } },
        ],
      }),
    });
    return {
      transport,
      dialProtocol,
      abort,
      changeRemotePeer: () => {
        authenticatedPeer = PEER_ID;
      },
    };
  };

  it("requires exact admitted Custom config, identity, address and protocol before dialing ip4", async () => {
    const identity = makeWatcherDeploymentAuthorityFixture({
      network: "Custom",
    }).result;
    const customNetwork = {
      watcherConfig: config(),
      deploymentIdentity: identity,
    };
    const request: WatcherPublicDaRequest = {
      peerIdentity: "da-peer-a",
      peerId,
      multiaddr: address,
      protocol: DaRequestResponseProtocol.payloadByHeader,
      protocolId: `/midgard/${identity.manifestId}/da/payload-by-header/1`,
      requestCbor: Buffer.from([0xa0]),
      timeoutMs: 1000,
      signal: new AbortController().signal,
      customNetwork,
    };
    const f = transportFixture();
    await f.transport.start();
    try {
      await expect(f.transport.request(request)).resolves.toEqual(
        Buffer.from([0xf6]),
      );
      expect(f.dialProtocol).toHaveBeenCalledTimes(1);
      for (const rejected of [
        { ...request, customNetwork: undefined },
        { ...request, peerIdentity: "unconfigured-peer" },
        { ...request, multiaddr: address.replace("4141", "4142") },
        {
          ...request,
          protocolId: `/midgard/${"99".repeat(32)}/da/payload-by-header/1`,
        },
        {
          ...request,
          customNetwork: {
            ...customNetwork,
            deploymentIdentity: deploymentIdentity(),
          },
        },
        {
          ...request,
          customNetwork: {
            ...customNetwork,
            deploymentIdentity: { ...identity },
          },
        },
        {
          ...request,
          customNetwork: {
            ...customNetwork,
            watcherConfig: parseWatcherConfig(rawConfig()),
          },
        },
      ])
        await expect(f.transport.request(rejected)).rejects.toThrow();
      expect(f.dialProtocol).toHaveBeenCalledTimes(1);
      f.changeRemotePeer();
      await expect(f.transport.request(request)).rejects.toThrow(
        "Noise-authenticated remote peer",
      );
      expect(f.abort).toHaveBeenCalledOnce();
    } finally {
      await f.transport.stop();
    }
  });

  it("carries the same Custom admission through retained runtime and public client requests", async () => {
    const identity = makeWatcherDeploymentAuthorityFixture({
      network: "Custom",
    }).result;
    const watcherConfig = config();
    const f = transportFixture(
      encodeDaPayloadByHeaderResponseCbor({
        status: "not_found",
        headerHash: Buffer.from("ab".repeat(28), "hex"),
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: null,
      }),
    );
    await f.transport.start();
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig,
      deploymentIdentity: identity,
      unsafeTransportFactoryForTest: async () => f.transport,
    });
    try {
      const result = await runtime.sources[0]!.fetchPayloadByHeaderHash(
        "ab".repeat(28),
      );
      expect(result.ok).toBe(false);
      // The real transport delivers the peer's not-found response. The public
      // client also reaches transport, then refuses this non-capabilities reply.
      expect(
        result.attempts.every((attempt) => attempt.status === "not_found"),
      ).toBe(true);
      const retainedDials = f.dialProtocol.mock.calls.length;
      expect(retainedDials).toBeGreaterThan(0);
      const client = new WatcherPublicDaClient({
        config: watcherConfig,
        deploymentIdentity: identity,
        transport: f.transport,
      });
      await expect(
        client.fetchPayloadByHeader({ headerHash: "ab".repeat(28) }),
      ).rejects.toMatchObject({ attempts: [{ status: "invalid_content" }] });
      expect(f.dialProtocol.mock.calls.length).toBeGreaterThan(retainedDials);
    } finally {
      await runtime.close();
    }
  });
});

describe("production retained-DA runtime V1", () => {
  it.each([
    { wallAfter: 90_000, fails: false },
    { wallAfter: 900_000, fails: false },
    { wallAfter: 90_000, fails: true },
    { wallAfter: 900_000, fails: true },
  ])(
    "measures DA elapsed time with wall movement to $wallAfter, failure=$fails",
    async ({ wallAfter, fails }) => {
      let wall = 100_000;
      let monotonic = 100;
      const fake = transportFactory();
      const originalError = new Error("original public transport failure");
      fake.request.mockImplementation(async () => {
        wall = wallAfter;
        monotonic += 2128.43;
        if (fails) throw originalError;
        return new Uint8Array([0xf6]);
      });
      const observability = createWatcherOperationsObservability({
        deploymentFingerprint: DEPLOYMENT,
        supervisor: {
          status: () => ({
            phase: "accepting",
            recovered: true,
            queuedJobCount: 0,
            activeJob: null,
            blockedJob: null,
            deadlineHealth: "safe",
            earliestDeadlineJob: null,
            remainingSafeStartMs: "1000",
          }),
        } as unknown as WatcherFaultProofSupervisor,
        launchScopeStatus: () => ({
          installedCategoryCount: 54,
          requiredCategoryCount: 54,
        }),
        durableProofQueueStatus: () => ({
          queuedJobCount: 0,
          oldestQueuedAtMs: null,
        }),
        nowMs: () => BigInt(wall),
        monotonicNowMs: () => monotonic,
      });
      const binding = bindWatcherRetainedDaOperations({
        deploymentIdentity: deploymentIdentity(),
        sink: observability.sink,
      });
      const runtime = await createWatcherRetainedDaRuntime({
        watcherConfig: rawConfig(),
        deploymentIdentity: deploymentIdentity(),
        unsafeTransportFactoryForTest: fake.factory,
      });
      const wallSpy = vi.spyOn(Date, "now").mockImplementation(() => wall);
      const monotonicSpy = vi
        .spyOn(performance, "now")
        .mockImplementation(() => monotonic);
      try {
        const result = await runtime.sources[0]!.fetchPayloadByHeaderHash(
          "ab".repeat(28),
        );
        if (fails)
          expect(result.attempts).toEqual([
            expect.objectContaining({ detail: originalError.message }),
          ]);
        const records = observability.api.diagnostics({
          kind: "da_fetch",
        }).records;
        expect(records).toEqual([
          expect.objectContaining({
            startedAtMs: "100000",
            completedAtMs: wallAfter.toString(),
            elapsedMs: "2129",
            outcome: fails ? "failed" : "succeeded",
          }),
        ]);
        expect(observability.api.metrics().daLatencyMs.maximum).toBe("2129");
      } finally {
        wallSpy.mockRestore();
        monotonicSpy.mockRestore();
        await runtime.close();
        binding.close();
      }
    },
  );

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
      blueprintHash: "33".repeat(32),
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
