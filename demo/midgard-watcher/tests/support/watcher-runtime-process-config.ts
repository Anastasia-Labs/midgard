import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { h32 } from "@al-ft/midgard-test-support/hex";

import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  parseWatcherProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  type WatcherProcessConfig,
} from "../../src/runtime/process-config.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import {
  makeWatcherDeploymentAuthorityFixture,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
} from "./deployment-authority-fixture.js";

/**
 * Writes a parseable production process config under `directory`: the signed
 * unit deployment authority, its rule bundle, the runtime config and an empty
 * blueprint. These are unit release bytes, not a genuine deployment receipt.
 */
export const writeWatcherRuntimeProcessConfig = async (
  directory: string,
): Promise<WatcherProcessConfig> => {
  const construction = makeWatcherDeploymentAuthorityFixture();
  const ruleBundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: construction.result.manifestId,
      network: construction.result.network,
      blueprintHash: construction.result.blueprintHash,
      programCommitments: construction.result.programCommitments,
    },
    targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  });
  const deployment = makeWatcherDeploymentAuthorityFixture({
    ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
  });
  const environment = (variable: string) =>
    Object.freeze({ kind: "environment" as const, variable });
  const configInput = {
    schemaVersion: WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
    watcherConfig: {
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
          depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
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
        path: join(directory, "watcher.sqlite"),
        rollbackAuthorityKeySource: environment(
          "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
        ),
      },
      proverWallet: { keySource: environment("MIDGARD_WATCHER_PROVER_KEY") },
      deadlines: {
        daFetchMs: 60_000,
        daPublishMs: 60_000,
        proofConstructMs: 300_000,
        proofSubmitMs: 120_000,
      },
    },
    watcherRuntimeConfigPath: join(directory, "watcher.json"),
    deploymentAuthorityPath: join(directory, "deployment-authority.json"),
    ruleBundlePath: join(directory, "rule-bundle.json"),
    fundingProfileBundlePath: join(directory, "funding-profiles.json"),
    nativeChainSyncBinaryPath: "/usr/local/bin/midgard-chain-sync",
    trustedHeadAuthorityEndpoint: "http://127.0.0.1:43123",
    operationsEndpoint: "http://127.0.0.1:43124",
    httpBearerSecretSource: environment("MIDGARD_WATCHER_TRUSTED_HEAD_BEARER"),
    workflowJournalDirectory: join(directory, "workflows"),
    availability: {
      keySource: environment("WATCHER_AVAILABILITY_KEY"),
      journalPath: join(directory, "availability.sqlite"),
      minimumFundingLovelace: "100000000",
    },
    faultProofInfrastructure: {
      manifestPath: join(directory, "deployment-manifest.json"),
      blueprintPath: join(directory, "plutus.json"),
      deploymentInfoPath: join(directory, "contract-deployment-info.json"),
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
  };
  const config = parseWatcherProcessConfig(configInput);
  await Promise.all([
    writeFile(
      config.watcherRuntimeConfigPath,
      JSON.stringify(configInput.watcherConfig),
    ),
    writeFile(
      config.deploymentAuthorityPath,
      JSON.stringify({
        signedIdentity: deployment.signedIdentity,
        policy: deployment.policy,
        trustRoots: deployment.trustRoots,
        durableMarker: deployment.marker,
      }),
    ),
    writeFile(config.ruleBundlePath, JSON.stringify(ruleBundle)),
    writeFile(config.faultProofInfrastructure.blueprintPath, "{}"),
  ]);
  return config;
};
