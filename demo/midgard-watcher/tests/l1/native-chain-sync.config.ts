import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { fileURLToPath } from "node:url";

import { startWatcherNativeChainSync } from "../../src/l1/native-chain-sync.js";
import {
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
} from "../../src/runtime/config.js";

const fixturePath = fileURLToPath(
  new URL("../support/native-chain-sync-fixture.mjs", import.meta.url),
);

export const NODE_CONFIG_PATH = "/etc/cardano/node-config.json";

export const GENESIS_CONFIG_PATH = "/etc/cardano/shelley-genesis.json";

export const NODE_CONFIG_BYTES = new TextEncoder().encode(
  JSON.stringify({ ShelleyGenesisFile: GENESIS_CONFIG_PATH }),
);

export const GENESIS_CONFIG_BYTES = new TextEncoder().encode(
  JSON.stringify({ networkMagic: 1 }),
);

export const GENESIS = createHash("sha256")
  .update(GENESIS_CONFIG_BYTES)
  .digest("hex");

export const INTERSECTION = Object.freeze({
  blockHash: "aa".repeat(32),
  kind: "point" as const,
  slot: "100",
});

export const config = (): WatcherConfig =>
  Object.freeze({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Preprod",
    l1: Object.freeze({
      source: Object.freeze({
        sourceMode: "local_node",
        authorityNodeId: "watcher-node",
        chainSync: Object.freeze({
          kind: "cardano_node_socket",
          socketPath: "/run/cardano/node.socket",
          nodeConfigPath: NODE_CONFIG_PATH,
          genesisConfigPath: GENESIS_CONFIG_PATH,
          genesisIdentitySha256: GENESIS,
        }),
        queryServices: Object.freeze([
          Object.freeze({
            kind: "ogmios",
            identity: "local-ogmios",
            endpoint: "ws://127.0.0.1:1337",
          }),
          Object.freeze({
            kind: "kupo",
            identity: "local-kupo",
            endpoint: "http://127.0.0.1:1442",
          }),
        ]),
      }),
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
      finality: Object.freeze({
        depth: 30,
        rollback: Object.freeze({
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: 30,
          postFinalityRecoveryMaxDepth: 2_160,
        }),
      }),
    }),
    da: Object.freeze({
      peers: Object.freeze([]),
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
    }),
    storage: Object.freeze({
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
      rollbackAuthorityKeySource: Object.freeze({
        kind: "environment",
        variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
      }),
    }),
    proverWallet: Object.freeze({
      keySource: Object.freeze({
        kind: "environment",
        variable: "MIDGARD_WATCHER_PROVER_KEY",
      }),
    }),
    deadlines: Object.freeze({
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    }),
  });

export const spawnFixture = (mode: string) => () =>
  spawn(process.execPath, [fixturePath, mode], {
    stdio: ["pipe", "pipe", "pipe"],
  });

export const readIdentityFixture = async (
  path: string,
): Promise<Uint8Array> => {
  if (path === NODE_CONFIG_PATH) return NODE_CONFIG_BYTES;
  if (path === GENESIS_CONFIG_PATH) return GENESIS_CONFIG_BYTES;
  throw new Error("unexpected native identity fixture path");
};

export const start = async (
  mode: string,
  onEvent: Parameters<typeof startWatcherNativeChainSync>[0]["onEvent"],
  onSpawn?: (child: ChildProcessWithoutNullStreams) => void,
) =>
  await startWatcherNativeChainSync({
    binaryPath: "/test/native-chain-sync",
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2_000,
    onEvent,
    unsafeSpawnForTest: () => {
      const child = spawnFixture(mode)();
      onSpawn?.(child);
      return child;
    },
    unsafeReadIdentityFileForTest: readIdentityFixture,
  });

export const waitFor = async (predicate: () => boolean): Promise<void> => {
  const deadline = Date.now() + 2_000;
  while (!predicate()) {
    if (Date.now() >= deadline) throw new Error("native fixture timed out");
    await new Promise<void>((resolve) => setTimeout(resolve, 5));
  }
};
