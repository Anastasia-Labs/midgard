import { readFile } from "node:fs/promises";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { expect } from "vitest";

import {
  parseWatcherConfig,
  parseWatcherStrictJsonValue,
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
} from "../../src/runtime/config.js";
import {
  parseWatcherProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  type WatcherProcessConfig,
} from "../../src/runtime/process-config.js";

export const directories: string[] = [];

/** The compiled deployment profile's release depth (10 live testing, 30 public). */
export const RELEASE_DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

export const watcherConfigValue = () => ({
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
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
    finality: {
      depth: RELEASE_DEPTH,
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
 * Reads a shipped template and rebinds its release depth to the compiled
 * profile. The template ships the public depth 30 (mainnet, preprod-public);
 * a testing-profile build must refuse that value, so the depth is pinned here
 * and every other field is parsed as shipped.
 */
export const shippedTemplate = async (name: string): Promise<unknown> => {
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
    expect(finality.depth).toBe(30);
    finality.depth = RELEASE_DEPTH;
  }
  return value;
};

const watcherConfig = (): WatcherConfig =>
  parseWatcherConfig(watcherConfigValue());

export const productionConfig = (): WatcherProcessConfig =>
  parseWatcherProcessConfig({
    schemaVersion: WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
    watcherConfig: watcherConfig(),
    watcherRuntimeConfigPath: "/etc/midgard/watcher.json",
    deploymentAuthorityPath: "/etc/midgard/deployment-authority.json",
    ruleBundlePath: "/etc/midgard/rule-bundle.json",
    fundingProfileBundlePath: "/etc/midgard/funding-profiles.json",
    l1NodeTransportBinaryPath: "/usr/local/bin/midgard-l1-node-transport",
    operationsEndpoint: "http://127.0.0.1:43124",
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
    },
  });
