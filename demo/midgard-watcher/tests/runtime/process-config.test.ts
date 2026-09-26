import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it } from "vitest";

import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  parseWatcherConfig,
  parseWatcherStrictJsonValue,
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
} from "../../src/runtime/config.js";
import {
  loadWatcherProcessConfigFile,
  loadWatcherTrustedHeadAuthorityProcessConfigFile,
  parseWatcherProcessConfig,
  parseWatcherTrustedHeadAuthorityProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
  type WatcherProcessConfig,
  type WatcherTrustedHeadAuthorityProcessConfig,
} from "../../src/runtime/process-config.js";
import {
  createWatcherTrustedHeadClientRuntime,
  startWatcherTrustedHeadAuthorityProcess,
} from "../../src/runtime/trusted-head-runtime.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";

const directories: string[] = [];

/** The compiled deployment profile's release depth (3 testing, 30 public). */
const RELEASE_DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

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
        genesisIdentitySha256: h32(0x66),
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
      depth: RELEASE_DEPTH,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: RELEASE_DEPTH,
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

type JsonObject = Record<string, unknown>;

const jsonObject = (value: unknown, path: string): JsonObject => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${path} is not a JSON object`);
  }
  return value as JsonObject;
};

/**
 * Reads a shipped template and rebinds its release depths to the compiled
 * profile. The templates ship the public depth 30 (mainnet, preprod-public);
 * a testing-profile build (depth 3) must refuse that value, so the depth
 * fields (and the authority policy digest that commits to them) are pinned
 * here and every other field is parsed as shipped.
 */
const shippedTemplate = async (name: string): Promise<unknown> => {
  const value = jsonObject(
    structuredClone(
      parseWatcherStrictJsonValue(
        await readFile(new URL(`../../${name}`, import.meta.url), "utf8"),
      ),
    ),
    name,
  );
  if (name === "watcher-process.example.json") {
    const l1 = jsonObject(
      jsonObject(value.watcherConfig, "watcherConfig").l1,
      "l1",
    );
    const finality = jsonObject(l1.finality, "l1.finality");
    const rollback = jsonObject(finality.rollback, "l1.finality.rollback");
    expect([finality.depth, rollback.maxDepth]).toEqual([30, 30]);
    finality.depth = RELEASE_DEPTH;
    rollback.maxDepth = RELEASE_DEPTH;
  } else if (name === "authority.example.json") {
    const templatePolicy = jsonObject(value.policy, "policy");
    expect([
      templatePolicy.confirmationDepth,
      templatePolicy.maximumPreFinalityRollbackDepth,
    ]).toEqual(["30", "30"]);
    const { policyDigest, ...committed } = templatePolicy;
    expect(policyDigest).toBe(watcherSha256CanonicalJson(committed));
    templatePolicy.confirmationDepth = RELEASE_DEPTH.toString();
    templatePolicy.maximumPreFinalityRollbackDepth = RELEASE_DEPTH.toString();
    const { policyDigest: _stale, ...rebound } = templatePolicy;
    templatePolicy.policyDigest = watcherSha256CanonicalJson(rebound);
  }
  return value;
};

const watcherConfig = (): WatcherConfig =>
  parseWatcherConfig(watcherConfigValue());

const policy = (): WatcherFinalityPolicy => {
  const value = makeWatcherFinalityPolicy(watcherConfig(), {
    manifestId: h32(0x11),
    network: "Preprod",
    trustRootId: h32(0x33),
    fundingProfileBundleDigest: "ab".repeat(32),
    blueprintHash: h32(0x22),
    ruleBundleCommitment: h32(0x44),
    programCommitments: { validation: h32(0x55) },
    durableMarker: makeDeploymentMarker(h32(0x11)),
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
    ruleBundlePath: "/etc/midgard/rule-bundle.json",
    fundingProfileBundlePath: "/etc/midgard/funding-profiles.json",
    nativeChainSyncBinaryPath: "/usr/local/bin/midgard-chain-sync",
    trustedHeadAuthorityEndpoint: "http://127.0.0.1:43123",
    operationsEndpoint: "http://127.0.0.1:43124",
    httpBearerSecretSource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_TRUSTED_HEAD_BEARER",
    },
    workflowJournalDirectory: "/var/lib/midgard-watcher/workflows",
    availability: {
      keySource: { kind: "environment", variable: "WATCHER_AVAILABILITY_KEY" },
      journalPath: "/var/lib/midgard-watcher/availability.sqlite",
      minimumFundingLovelace: "100000000",
    },
    faultProofInfrastructure: {
      manifestPath: "/etc/midgard/deployment-manifest.json",
      blueprintPath: "/etc/midgard/plutus.json",
      deploymentInfoPath: "/etc/midgard/contract-deployment-info.json",
      historicalNativeScriptHistory: {
        sourceMode: "external_provider_quorum",
        consistencyPolicy: "exact_bytes_all_providers_v1",
        providers: [
          {
            sourceId: "history-a",
            operatorIdentitySha256: h32(0xa1),
            authorityEndpoint: "https://history-a.example.test",
          },
          {
            sourceId: "history-b",
            operatorIdentitySha256: h32(0xb2),
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
  it("loads the strict JSON process configuration with its required release artifact path", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-process-config-");
    directories.push(directory);
    const path = join(directory, "process.json");
    const config = productionConfig();
    await writeFile(
      path,
      JSON.stringify({ ...config, watcherConfig: watcherConfigValue() }),
    );
    await expect(loadWatcherProcessConfigFile(path)).resolves.toEqual(config);
  });

  it("admits only acceptance Preprod or Custom local-node watcher topology", () => {
    expect(productionConfig().watcherConfig.l1.source.sourceMode).toBe(
      "local_node",
    );
    const base = productionConfig();
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        watcherConfig: { ...watcherConfigValue(), mode: "development" },
      }),
    ).toThrow("requires acceptance Preprod or Custom local_node authority");
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
        readinessHeaderHash: h28(0x77),
      }),
    ).toThrow("unknown or missing fields");
    expect(() => {
      const { ruleBundlePath: _omitted, ...withoutBundle } = base;
      return parseWatcherProcessConfig(withoutBundle);
    }).toThrow("unknown or missing fields");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        ruleBundlePath: "etc/midgard/rule-bundle.json",
      }),
    ).toThrow("watcher release rule bundle is not a canonical production path");
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

  it("rejects the deleted Midgard node lease coordination keys as unknown fields", () => {
    const base = productionConfig();
    for (const deleted of [
      { midgardNodeUrl: "http://127.0.0.1:3000" },
      {
        midgardNodeAdminKeySource: {
          kind: "environment",
          variable: "MIDGARD_NODE_ADMIN_KEY",
        },
      },
      { stateQueueLeaseTtlMs: 30_000 },
    ]) {
      expect(() =>
        parseWatcherProcessConfig({
          ...base,
          faultProofInfrastructure: {
            ...base.faultProofInfrastructure,
            ...deleted,
          },
        }),
      ).toThrow(
        "watcher fault-proof infrastructure has unknown or missing fields",
      );
    }
  });

  it("parses the shipped watcher-process.example.json template", async () => {
    // Parsed from its bytes rather than loaded by path: the loader refuses a
    // checkout under /tmp, and the template's location is not what is tested.
    const config = parseWatcherProcessConfig(
      await shippedTemplate("watcher-process.example.json"),
    );
    expect(config.schemaVersion).toBe(WATCHER_PROCESS_CONFIG_SCHEMA_VERSION);
    expect(Object.keys(config.faultProofInfrastructure).sort()).toEqual([
      "blueprintPath",
      "deploymentInfoPath",
      "historicalNativeScriptHistory",
      "manifestPath",
    ]);
  });

  it("parses the shipped authority.example.json template as the sidecar of the start template", async () => {
    const template = shippedTemplate;
    const authority = parseWatcherTrustedHeadAuthorityProcessConfig(
      await template("authority.example.json"),
    );
    const start = parseWatcherProcessConfig(
      await template("watcher-process.example.json"),
    );
    expect(authority.schemaVersion).toBe(
      WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
    );
    expect(authority.endpoint).toBe(start.trustedHeadAuthorityEndpoint);
    expect(authority.httpBearerSecretSource).toEqual(
      start.httpBearerSecretSource,
    );
    const source = start.watcherConfig.l1.source;
    if (source.sourceMode !== "local_node") {
      throw new Error("start template is not a local_node watcher");
    }
    expect(authority.policy.network).toBe(start.watcherConfig.targetNetwork);
    expect(authority.policy.authorityNodeId).toBe(source.authorityNodeId);
    expect(authority.policy.authorityChainSyncSocketPath).toBe(
      source.chainSync.socketPath,
    );
    expect(authority.policy.authorityGenesisIdentitySha256).toBe(
      source.chainSync.genesisIdentitySha256,
    );
    expect(
      authority.policy.localQueryServices
        .map((service) => `${service.providerId} ${service.endpoint}`)
        .sort(),
    ).toEqual(
      source.queryServices
        .map((service) => `${service.identity} ${service.endpoint}`)
        .sort(),
    );
  });

  it("requires a durable availability journal, positive capital, and a distinct challenger key", () => {
    const base = productionConfig();
    const { availability: _omitted, ...withoutAvailability } = base;
    expect(() => parseWatcherProcessConfig(withoutAvailability)).toThrow(
      "unknown or missing fields",
    );
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        availability: {
          ...base.availability,
          keySource: base.watcherConfig.proverWallet.keySource,
        },
      }),
    ).toThrow("pairwise distinct");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        availability: {
          ...base.availability,
          journalPath: "/tmp/availability.sqlite",
        },
      }),
    ).toThrow("canonical production path");
    for (const minimumFundingLovelace of ["0", "-1", "01", "1.5"]) {
      expect(() =>
        parseWatcherProcessConfig({
          ...base,
          availability: {
            ...base.availability,
            minimumFundingLovelace,
          },
        }),
      ).toThrow("positive lovelace");
    }
  });

  it("binds finality and pre-finality rollback depth to the compiled profile depth", () => {
    // Acceptance is tied to the compiled selection: only the selected profile's
    // depth parses, and the other profile family's depth is always refused.
    const base = productionConfig();
    expect(base.watcherConfig.l1.finality.depth).toBe(RELEASE_DEPTH);
    expect(base.watcherConfig.l1.finality.rollback.maxDepth).toBe(
      RELEASE_DEPTH,
    );
    const otherDepth = RELEASE_DEPTH === 3 ? 30 : 3;
    const withDepths = (depth: number, maxDepth: number) => {
      const value = watcherConfigValue();
      return {
        ...base,
        watcherConfig: {
          ...value,
          l1: {
            ...value.l1,
            finality: {
              ...value.l1.finality,
              depth,
              rollback: { ...value.l1.finality.rollback, maxDepth },
            },
          },
        },
      };
    };
    expect(
      parseWatcherProcessConfig(withDepths(RELEASE_DEPTH, RELEASE_DEPTH))
        .watcherConfig.l1.finality.depth,
    ).toBe(RELEASE_DEPTH);
    const refusal = `requires finality depth and pre-finality rollback depth ${RELEASE_DEPTH} from the deployment profile`;
    expect(() =>
      parseWatcherProcessConfig(withDepths(otherDepth, otherDepth)),
    ).toThrow(refusal);
    expect(() =>
      parseWatcherProcessConfig(withDepths(RELEASE_DEPTH, RELEASE_DEPTH - 1)),
    ).toThrow(refusal);
  });

  it("refuses startup without the signed release rule-bundle artifact", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-release-rule-bundle-");
    directories.push(directory);
    const config = parseWatcherProcessConfig({
      ...productionConfig(),
      watcherRuntimeConfigPath: join(directory, "watcher.json"),
      deploymentAuthorityPath: join(directory, "deployment-authority.json"),
      ruleBundlePath: join(directory, "rule-bundle.json"),
      workflowJournalDirectory: join(directory, "workflows"),
    });
    await writeFile(
      config.watcherRuntimeConfigPath,
      JSON.stringify(watcherConfigValue()),
    );
    await writeFile(config.deploymentAuthorityPath, "{}");
    await expect(createWatcherRuntime({ config })).rejects.toMatchObject({
      code: "ENOENT",
      path: config.ruleBundlePath,
    });
  });

  it("loads authority config from JSON and keeps signer sources separate", async () => {
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
    const directory = await mkdtemp("/var/tmp/midgard-authority-config-");
    directories.push(directory);
    const path = join(directory, "authority.json");
    await writeFile(path, JSON.stringify(input));
    expect(
      await loadWatcherTrustedHeadAuthorityProcessConfigFile(path),
    ).toEqual(input);
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
