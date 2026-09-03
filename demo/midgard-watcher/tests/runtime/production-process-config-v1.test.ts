import { mkdtemp, rm } from "node:fs/promises";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { afterEach, describe, expect, it } from "vitest";

import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
} from "../../src/runtime/config.js";
import {
  parseWatcherProcessConfig,
  parseWatcherTrustedHeadAuthorityProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
  type WatcherProcessConfig,
  type WatcherTrustedHeadAuthorityProcessConfig,
} from "../../src/runtime/production-process-config-v1.js";
import {
  createWatcherTrustedHeadClientRuntime,
  startWatcherTrustedHeadAuthorityProcess,
} from "../../src/runtime/production-trusted-head-runtime-v1.js";

const directories: string[] = [];
const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);

const watcherConfigValue = () => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "local_node",
      authorityNodeId: "watcher-node",
      chainSync: {
        kind: "cardano_node_socket",
        socketPath: "/run/cardano/node.socket",
        nodeConfigPath: "/etc/cardano/node-config.json",
        genesisConfigPath: "/etc/cardano/shelley-genesis.json",
        genesisIdentitySha256: h32("66"),
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
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
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
    peers: [
      {
        identity: "da-peer-a",
        multiaddr:
          "/dns4/da.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      },
    ],
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
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
});

const watcherConfig = (): WatcherConfig =>
  parseWatcherConfig(watcherConfigValue());

const policy = (): WatcherFinalityPolicy => {
  const value = makeWatcherFinalityPolicy(watcherConfig(), {
    manifestId: h32("11"),
    network: "Preprod",
    trustRootId: h32("33"),
    releaseEvidenceDigest: h32("22"),
    ruleBundleCommitment: h32("44"),
    programCommitments: { validation: h32("55") },
    durableMarker: makeDeploymentMarker(h32("11")),
  });
  if (value === null) throw new Error("test finality policy is invalid");
  return value;
};

const productionConfig = (): WatcherProcessConfig =>
  parseWatcherProcessConfig({
    schemaVersion: WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
    watcherConfig: watcherConfig(),
    watcherRuntimeConfigPath: "/etc/midgard/watcher.json",
    deploymentAuthorityPath: "/etc/midgard/deployment-authority.json",
    fundingProfileBundlePath: "/etc/midgard/funding-profiles.json",
    nativeChainSyncBinaryPath: "/usr/local/bin/midgard-chain-sync",
    trustedHeadAuthorityEndpoint: "http://127.0.0.1:43123",
    operationsEndpoint: "http://127.0.0.1:43124",
    httpBearerSecretSource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_TRUSTED_HEAD_BEARER",
    },
    workflowJournalDirectory: "/var/lib/midgard-watcher/workflows",
    readinessHeaderHash: h28("77"),
    faultProofInfrastructure: {
      manifestPath: "/etc/midgard/deployment-manifest.json",
      blueprintPath: "/etc/midgard/plutus.json",
      deploymentInfoPath: "/etc/midgard/contract-deployment-info.json",
      midgardNodeUrl: "http://127.0.0.1:3000",
      midgardNodeAdminKeySource: {
        kind: "environment",
        variable: "MIDGARD_NODE_ADMIN_KEY",
      },
      historicalNativeScriptHistory: {
        sourceMode: "external_provider_quorum",
        consistencyPolicy: "exact_bytes_all_providers_v1",
        providers: [
          {
            sourceId: "history-a",
            operatorIdentitySha256: h32("a1"),
            authorityEndpoint: "https://history-a.example.test",
          },
          {
            sourceId: "history-b",
            operatorIdentitySha256: h32("b2"),
            authorityEndpoint: "https://history-b.example.test",
          },
        ],
      },
    },
  });

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (directory) =>
        rm(directory, { force: true, recursive: true }),
      ),
  );
});

describe("production process authority separation", () => {
  it("admits only acceptance Preprod local-node watcher topology", () => {
    expect(productionConfig().watcherConfig.l1.source.sourceMode).toBe(
      "local_node",
    );
    const base = productionConfig();
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        watcherConfig: { ...watcherConfigValue(), mode: "development" },
      }),
    ).toThrow("requires acceptance Preprod local_node authority");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        watcherRollbackKeySource:
          base.watcherConfig.storage.rollbackAuthorityKeySource,
      }),
    ).toThrow("unknown or missing fields");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        readinessHeaderHash: h32("77"),
      }),
    ).toThrow("readiness header hash is invalid");
    expect(() => {
      const { fundingProfileBundlePath: _omitted, ...withoutBundle } = base;
      return parseWatcherProcessConfig(withoutBundle);
    }).toThrow("unknown or missing fields");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        fundingProfileBundlePath: "etc/midgard/funding-profiles.json",
      }),
    ).toThrow(
      "watcher funding profile bundle is not a canonical production path",
    );
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        faultProofInfrastructure: {
          ...base.faultProofInfrastructure,
          historicalNativeScriptHistory: {
            ...base.faultProofInfrastructure.historicalNativeScriptHistory,
            providers: [
              base.faultProofInfrastructure.historicalNativeScriptHistory
                .providers[0]!,
              {
                ...base.faultProofInfrastructure.historicalNativeScriptHistory
                  .providers[1],
                operatorIdentitySha256:
                  base.faultProofInfrastructure.historicalNativeScriptHistory
                    .providers[0]!.operatorIdentitySha256,
              },
            ],
          },
        },
      }),
    ).toThrow("not independent");
  });

  it("keeps authority config structurally unable to receive watcher rollback or proof signer sources", () => {
    const input = {
      schemaVersion:
        WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
      policy: policy(),
      directory: "/var/lib/midgard-trusted-head",
      endpoint: "http://127.0.0.1:43123",
      recordAuthenticationKeySource: {
        kind: "environment",
        variable: "MIDGARD_SIDECAR_RECORD_KEY",
      },
      httpBearerSecretSource: {
        kind: "environment",
        variable: "MIDGARD_SIDECAR_BEARER",
      },
    };
    expect(parseWatcherTrustedHeadAuthorityProcessConfig(input)).toEqual(input);
    expect(() =>
      parseWatcherTrustedHeadAuthorityProcessConfig({
        ...input,
        proofSignerKeySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_PROVER_KEY",
        },
      }),
    ).toThrow("unknown or missing fields");
  });

  it("rejects equal authority record and HTTP bearer values before opening the server", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-process-secrets-");
    directories.push(directory);
    const authorityConfig: WatcherTrustedHeadAuthorityProcessConfig = {
      schemaVersion:
        WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
      policy: policy(),
      directory,
      endpoint: "http://127.0.0.1:0",
      recordAuthenticationKeySource: {
        kind: "environment",
        variable: "RECORD_KEY",
      },
      httpBearerSecretSource: {
        kind: "environment",
        variable: "BEARER",
      },
    };
    await expect(
      startWatcherTrustedHeadAuthorityProcess({
        config: authorityConfig,
        unsafeEnvironmentForTest: {
          RECORD_KEY: "11".repeat(32),
          BEARER: "11".repeat(32),
        },
        unsafeAllowEphemeralPortForTest: true,
      }),
    ).rejects.toThrow("pairwise distinct");
  });

  it("rejects a sidecar record-key identity collision from the watcher without receiving the record key", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-process-identity-");
    directories.push(directory);
    const rollbackAndRecordKey = "12".repeat(32);
    const bearer = "watcher-sidecar-bearer-secret-0001";
    const authority = await startWatcherTrustedHeadAuthorityProcess({
      config: {
        schemaVersion:
          WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
        policy: policy(),
        directory,
        endpoint: "http://127.0.0.1:0",
        recordAuthenticationKeySource: {
          kind: "environment",
          variable: "RECORD_KEY",
        },
        httpBearerSecretSource: {
          kind: "environment",
          variable: "BEARER",
        },
      },
      unsafeEnvironmentForTest: {
        RECORD_KEY: rollbackAndRecordKey,
        BEARER: bearer,
      },
      unsafeAllowEphemeralPortForTest: true,
    });
    try {
      const base = productionConfig();
      await expect(
        createWatcherTrustedHeadClientRuntime({
          config: {
            ...base,
            trustedHeadAuthorityEndpoint: authority.server.endpoint,
          },
          policy: policy(),
          unsafeEnvironmentForTest: {
            MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY: rollbackAndRecordKey,
            MIDGARD_WATCHER_PROVER_KEY:
              "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
            MIDGARD_WATCHER_TRUSTED_HEAD_BEARER: bearer,
          },
        }),
      ).rejects.toThrow("pairwise distinct");
    } finally {
      await authority.close();
    }
  });
});
