import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherLocalNodeAuthorityTransport,
  watcherL1TransportAttestationDetails,
} from "../../src/l1/l1-adapter.js";
import {
  parseWatcherNativeChainSyncEvent,
  startWatcherNativeChainSync,
  startWatcherNativeChainSyncWithRetry,
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  watcherNativeChainSyncAuthorityDetails,
} from "../../src/l1/native-chain-sync.js";
import {
  WATCHER_CONFIG_SCHEMA_VERSION,
  type WatcherConfig,
} from "../../src/runtime/config.js";

const fixturePath = fileURLToPath(
  new URL("../support/native-chain-sync-fixture.mjs", import.meta.url),
);
const NODE_CONFIG_PATH = "/etc/cardano/node-config.json";
const GENESIS_CONFIG_PATH = "/etc/cardano/shelley-genesis.json";
const NODE_CONFIG_BYTES = new TextEncoder().encode(
  JSON.stringify({ ShelleyGenesisFile: GENESIS_CONFIG_PATH }),
);
const GENESIS_CONFIG_BYTES = new TextEncoder().encode(
  JSON.stringify({ networkMagic: 1 }),
);
const GENESIS = createHash("sha256").update(GENESIS_CONFIG_BYTES).digest("hex");
const INTERSECTION = Object.freeze({
  blockHash: "aa".repeat(32),
  kind: "point" as const,
  slot: "100",
});

const config = (): WatcherConfig =>
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

const spawnFixture = (mode: string) => () =>
  spawn(process.execPath, [fixturePath, mode], {
    stdio: ["pipe", "pipe", "pipe"],
  });

const readIdentityFixture = async (path: string): Promise<Uint8Array> => {
  if (path === NODE_CONFIG_PATH) return NODE_CONFIG_BYTES;
  if (path === GENESIS_CONFIG_PATH) return GENESIS_CONFIG_BYTES;
  throw new Error("unexpected native identity fixture path");
};

const start = async (
  mode: string,
  onEvent: Parameters<typeof startWatcherNativeChainSync>[0]["onEvent"],
) =>
  await startWatcherNativeChainSync({
    binaryPath: "/test/native-chain-sync",
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2_000,
    onEvent,
    unsafeSpawnForTest: spawnFixture(mode),
    unsafeReadIdentityFileForTest: readIdentityFixture,
  });

const waitFor = async (predicate: () => boolean): Promise<void> => {
  const deadline = Date.now() + 2_000;
  while (!predicate()) {
    if (Date.now() >= deadline) throw new Error("native fixture timed out");
    await new Promise<void>((resolve) => setTimeout(resolve, 5));
  }
};

describe("native Cardano node-to-client chain-sync supervisor", () => {
  it("seals the exact startup identity and admits ordered roll-forward/rollback", async () => {
    const events: unknown[] = [];
    const runtime = await start("honest", async (event) => {
      events.push(event);
    });
    try {
      await waitFor(() => events.length === 2);
      expect(events).toMatchObject([
        {
          kind: "roll_forward",
          blockType: "6",
          prevHash: INTERSECTION.blockHash,
        },
        { kind: "roll_backward", point: INTERSECTION },
      ]);
      expect(watcherNativeChainSyncAuthorityDetails(runtime.authority)).toEqual(
        {
          network: "Preprod",
          authorityNodeId: "watcher-node",
          genesisIdentitySha256: GENESIS,
          socketPath: "/run/cardano/node.socket",
          startupDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
          selectedIntersection: INTERSECTION,
          currentTip: {
            blockHash: "44".repeat(32),
            blockNo: "12",
            kind: "point",
            slot: "103",
          },
        },
      );
      const context = establishWatcherLocalNodeAuthorityTransport(
        runtime.authority,
      );
      try {
        expect(watcherL1TransportAttestationDetails(context)).toMatchObject({
          provider: {
            network: "Preprod",
            providerId: "watcher-node",
            source: {
              sourceMode: "local_node",
              authorityNodeId: "watcher-node",
              surface: "chain_sync",
            },
            authentication: {
              kind: "cardano_node_genesis_v1",
              publicIdentitySha256: GENESIS,
            },
          },
          transportEndpoint: "/run/cardano/node.socket",
        });
      } finally {
        closeWatcherL1TransportAttestationContext(context);
      }
    } finally {
      await runtime.close();
    }
  });

  it("rejects substituted startup identity before minting authority", async () => {
    await expect(start("forged_ready", async () => undefined)).rejects.toThrow(
      "ready identity differs",
    );
  });

  it("derives genesis identity from the exact node config before spawning", async () => {
    let spawnCount = 0;
    const invoke = async (
      readIdentityFile: (path: string) => Promise<Uint8Array>,
    ) =>
      await startWatcherNativeChainSync({
        binaryPath: "/test/native-chain-sync",
        watcherConfig: config(),
        intersection: INTERSECTION,
        startupTimeoutMs: 2_000,
        onEvent: async () => undefined,
        unsafeSpawnForTest: () => {
          spawnCount += 1;
          return spawnFixture("honest")();
        },
        unsafeReadIdentityFileForTest: readIdentityFile,
      });

    await expect(
      invoke(async (path) =>
        path === NODE_CONFIG_PATH
          ? new TextEncoder().encode(
              `{"ShelleyGenesisFile":"${GENESIS_CONFIG_PATH}","ShelleyGenesisFile":"${GENESIS_CONFIG_PATH}"}`,
            )
          : GENESIS_CONFIG_BYTES,
      ),
    ).rejects.toThrow(/duplicate_field/u);
    await expect(
      invoke(async (path) =>
        path === NODE_CONFIG_PATH
          ? NODE_CONFIG_BYTES
          : new TextEncoder().encode(JSON.stringify({ networkMagic: 2 })),
      ),
    ).rejects.toThrow("network magic differs");
    expect(spawnCount).toBe(0);
  });

  it.each(["reordered", "first_slot_regression", "unknown_rollback"])(
    "terminates on hostile %s output",
    async (mode) => {
      const runtime = await start(mode, async () => undefined);
      await expect(runtime.done).rejects.toThrow(
        /out of order|not durable history/u,
      );
      await runtime.close();
    },
  );

  it("surfaces helper process crash after authenticated startup", async () => {
    const runtime = await start("crash", async () => undefined);
    await expect(runtime.done).rejects.toThrow("exited unexpectedly");
    await runtime.close();
  });

  it("retries durable ancestors one process at a time and binds explicit Origin", async () => {
    const runtime = await startWatcherNativeChainSyncWithRetry({
      binaryPath: "/test/native-chain-sync",
      watcherConfig: config(),
      intersectionCandidates: [INTERSECTION, { kind: "origin" }],
      startupTimeoutMs: 2_000,
      onEvent: async () => undefined,
      unsafeSpawnForTest: spawnFixture("retry_intersection"),
      unsafeReadIdentityFileForTest: readIdentityFixture,
    });
    try {
      expect(
        watcherNativeChainSyncAuthorityDetails(runtime.authority)
          ?.selectedIntersection,
      ).toEqual({ kind: "origin" });
    } finally {
      await runtime.close();
    }
  });

  it("strictly parses canonical bounded event shapes", () => {
    const event = {
      blockHash: "bb".repeat(32),
      blockNo: "10",
      blockType: "6",
      kind: "roll_forward",
      prevHash: "aa".repeat(32),
      rawBlockCbor: "80",
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      slot: "101",
      tip: {
        blockHash: "cc".repeat(32),
        blockNo: "11",
        kind: "point",
        slot: "102",
      },
    };
    expect(parseWatcherNativeChainSyncEvent(event)).toEqual(event);
    expect(() =>
      parseWatcherNativeChainSyncEvent({ ...event, trusted: true }),
    ).toThrow("unknown or missing");
    expect(() =>
      parseWatcherNativeChainSyncEvent({ ...event, slot: "0101" }),
    ).toThrow("slot is invalid");
    expect(() =>
      parseWatcherNativeChainSyncEvent({ ...event, rawBlockCbor: "0" }),
    ).toThrow("CBOR is invalid");
    expect(
      parseWatcherNativeChainSyncEvent({
        kind: "roll_backward",
        point: { kind: "origin" },
        schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
        tip: { kind: "origin" },
      }),
    ).toMatchObject({ kind: "roll_backward", point: { kind: "origin" } });
  });
});
