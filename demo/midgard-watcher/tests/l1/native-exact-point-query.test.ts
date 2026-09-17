import { createHash } from "node:crypto";
import { chmod, mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import {
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
  watcherNativeChainSyncAuthorityDetails,
} from "../../src/l1/native-chain-sync.js";
import {
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
} from "../../src/runtime/config.js";
const GENESIS_BYTES = JSON.stringify({ networkMagic: 1 });
const GENESIS = createHash("sha256").update(GENESIS_BYTES).digest("hex");
const config = (NODE_CONFIG_PATH: string, GENESIS_CONFIG_PATH: string) =>
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
        }),
      }),
    }),
    da: Object.freeze({
      peers: Object.freeze([
        {
          identity: "da-peer-a",
          multiaddr:
            "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
        },
      ]),
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

const predecessor = { blockHash: "aa".repeat(32), blockNo: "9", slot: "100" };
const target = { blockHash: "bb".repeat(32), blockNo: "10", slot: "101" };
const cleanup: (() => Promise<void>)[] = [];
afterEach(async () => {
  vi.restoreAllMocks();
  for (const close of cleanup.splice(0).reverse()) await close();
});
const input = async (mode = "query_idle", timeoutMs = 2000) => {
  const dir = await mkdtemp(join("/var/tmp", "native-exact-query-"));
  cleanup.push(() => rm(dir, { recursive: true, force: true }));
  const nodeConfig = join(dir, "node.json");
  const genesisConfig = join(dir, "genesis.json");
  const binaryPath = join(dir, "helper");
  await writeFile(genesisConfig, GENESIS_BYTES);
  await writeFile(
    nodeConfig,
    JSON.stringify({ ShelleyGenesisFile: genesisConfig }),
  );
  await writeFile(
    binaryPath,
    `#!${process.execPath}
process.argv[2] = ${JSON.stringify(mode)};
await import(${JSON.stringify(new URL("../support/native-chain-sync-fixture.mjs", import.meta.url).href)});
`,
  );
  await chmod(binaryPath, 0o700);
  return {
    binaryPath,
    watcherConfig: config(nodeConfig, genesisConfig),
    predecessor,
    target,
    timeoutMs,
  };
};
const open = async (mode = "query_idle", timeoutMs = 2000) => {
  const args = await input(mode, timeoutMs);
  const query = await openWatcherNativeExactPointQuery({
    ...args,
    watcherConfig:
      mode === "query_ack"
        ? parseWatcherConfig(args.watcherConfig)
        : args.watcherConfig,
  });
  cleanup.push(query.close);
  return query;
};
const delay = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));
describe("bounded native exact-point query", () => {
  it.each(["query_idle", "query_ack"])(
    "captures %s through the configured executable and owns receipt liveness",
    async (mode) => {
      const query = await open(mode);
      const value = readWatcherNativeExactPointQuery(query.receipt);
      expect(value.event.blockHash).toBe(target.blockHash);
      expect(value.depthAtObservedTip).toBe("3");
      expect(
        watcherNativeChainSyncAuthorityDetails(value.authority)?.operation,
      ).toEqual({
        kind: "exact_point",
        predecessorBlockNo: "9",
        target,
        timeoutMs: 2000,
      });
      expect(() =>
        readWatcherNativeExactPointQuery({ ...query.receipt }),
      ).toThrow(/absent or stale/);
      await query.close();
      await query.close();
      expect(() => readWatcherNativeExactPointQuery(query.receipt)).toThrow(
        /absent or stale/,
      );
    },
  );
  it("derives old-history depth from the target event tip", async () => {
    const query = await open("query_old");
    const value = readWatcherNativeExactPointQuery(query.receipt);
    expect(value.depthAtObservedTip).toBe("4991");
    expect(
      watcherNativeChainSyncAuthorityDetails(value.authority)?.currentTip,
    ).toMatchObject({ blockNo: "12" });
  });
  it.each([
    [
      "wrong_operation",
      "native chain-sync ready identity differs from startup authority",
    ],
    [
      "missing_operation",
      "native chain-sync ready event has unknown or missing fields",
    ],
    [
      "query_wrong_target",
      "native exact-point query returned a different target",
    ],
    ["query_bad_tip", "native exact-point query tip cannot contain the target"],
  ])("refuses %s", async (mode, expected) => {
    await expect(open(mode)).rejects.toThrow(expected);
  });
  it.each(["query_exit", "query_extra"])("revokes on %s", async (mode) => {
    const query = await open(mode);
    await delay(200);
    expect(() => readWatcherNativeExactPointQuery(query.receipt)).toThrow();
  });
  it("expires monotonically even when wall time moves backward", async () => {
    const query = await open("query_idle", 400);
    vi.spyOn(Date, "now").mockReturnValue(0);
    await delay(450);
    expect(() => readWatcherNativeExactPointQuery(query.receipt)).toThrow();
  });
  it.each(["no_ready", "query_wait"])("cancels while %s", async (mode) => {
    const controller = new AbortController();
    const pending = openWatcherNativeExactPointQuery({
      ...(await input(mode)),
      signal: controller.signal,
    });
    const refusal = expect(pending).rejects.toThrow(/cancel/);
    setTimeout(() => controller.abort(), 50);
    await refusal;
  });
  it.each(["no_ready", "query_wait"])(
    "expires pending acquisition %s",
    async (mode) => {
      await expect(open(mode, 150)).rejects.toThrow(/expired|timed out|cancel/);
    },
  );
  it("refuses an already cancelled acquisition and structural signals", async () => {
    const args = await input();
    const controller = new AbortController();
    controller.abort();
    await expect(
      openWatcherNativeExactPointQuery({ ...args, signal: controller.signal }),
    ).rejects.toThrow();
    await expect(
      openWatcherNativeExactPointQuery({
        ...args,
        signal: Object.create(AbortSignal.prototype) as AbortSignal,
      }),
    ).rejects.toThrow(/not an AbortSignal/);
  });
  it("revokes on cancellation after capture and requires a new acquisition", async () => {
    const controller = new AbortController();
    const first = await openWatcherNativeExactPointQuery({
      ...(await input()),
      signal: controller.signal,
    });
    cleanup.push(first.close);
    controller.abort();
    expect(() => readWatcherNativeExactPointQuery(first.receipt)).toThrow();
    await first.close();
    const second = await open();
    expect(
      readWatcherNativeExactPointQuery(second.receipt).event.blockHash,
    ).toBe(target.blockHash);
    expect(() => readWatcherNativeExactPointQuery(first.receipt)).toThrow();
  });
  it.each([
    { predecessor: { ...predecessor, blockNo: "8" } },
    { target: { ...target, slot: "100" } },
    { target: { ...target, blockNo: "18446744073709551616" } },
    { timeoutMs: 99 },
    { timeoutMs: 120001 },
  ])("refuses invalid query bounds %j", async (changes) => {
    await expect(
      openWatcherNativeExactPointQuery({ ...(await input()), ...changes }),
    ).rejects.toThrow();
  });
});
