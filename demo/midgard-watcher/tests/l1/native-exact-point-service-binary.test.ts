import { existsSync } from "node:fs";
import { chmod, mkdtemp, rm, writeFile } from "node:fs/promises";
import { createServer, type Socket } from "node:net";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  closeWatcherNativeExactPointServices,
  openWatcherNativeExactPointQuery,
  watcherNativeExactPointServicePids,
} from "../../src/l1/native-chain-sync.js";
import {
  exactPointWatcherConfig,
  GENESIS_BYTES,
} from "../support/native-exact-point-query-config.js";

// The compiled helper itself, as `pnpm native:build` leaves it.
const BINARY = fileURLToPath(
  new URL("../../dist/native/midgard-chain-sync", import.meta.url),
);
const predecessor = { blockHash: "aa".repeat(32), blockNo: "9", slot: "100" };
const target = { blockHash: "bb".repeat(32), blockNo: "10", slot: "101" };

const cleanup: (() => Promise<void>)[] = [];
afterEach(async () => {
  await closeWatcherNativeExactPointServices();
  for (const close of cleanup.splice(0).reverse()) await close();
});

beforeAll(() => {
  if (!existsSync(BINARY))
    throw new Error(
      `compiled native helper is missing at ${BINARY}; run pnpm --dir demo/midgard-watcher native:build`,
    );
});

const alive = (pid: number): boolean => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ESRCH") return false;
    throw error;
  }
};

const workspace = async () => {
  const dir = await mkdtemp(join("/var/tmp", "native-exact-binary-"));
  cleanup.push(() => rm(dir, { recursive: true, force: true }));
  const nodeConfig = join(dir, "node.json");
  const genesisConfig = join(dir, "genesis.json");
  await writeFile(genesisConfig, GENESIS_BYTES);
  await writeFile(
    nodeConfig,
    JSON.stringify({ ShelleyGenesisFile: genesisConfig }),
  );
  const base = exactPointWatcherConfig(nodeConfig, genesisConfig);
  const watcherConfig = (socketPath: string) => ({
    ...base,
    l1: {
      ...base.l1,
      source: {
        ...base.l1.source,
        chainSync: { ...base.l1.source.chainSync, socketPath },
      },
    },
  });
  return { dir, watcherConfig };
};

// A node socket that accepts connections and never answers them. It drains
// what the helper sends, so the helper's own close is seen as end of stream.
const silentNode = async (socketPath: string) => {
  const connections: Socket[] = [];
  const closed: Promise<void>[] = [];
  const server = createServer((socket) => {
    connections.push(socket);
    closed.push(new Promise((resolve) => socket.once("close", resolve)));
    socket.resume();
  });
  await new Promise<void>((resolve) => server.listen(socketPath, resolve));
  cleanup.push(async () => {
    for (const socket of connections) socket.destroy();
    await new Promise((resolve) => server.close(resolve));
  });
  return { connections, closed };
};

describe("compiled native exact-point helper", () => {
  it("refuses a startup without a node socket and keeps serving the next session", async () => {
    const { dir, watcherConfig } = await workspace();
    const open = () =>
      openWatcherNativeExactPointQuery({
        binaryPath: BINARY,
        watcherConfig: watcherConfig(join(dir, "absent.socket")),
        predecessor,
        target,
        timeoutMs: 10_000,
      });
    await expect(open()).rejects.toMatchObject({
      name: "NativeChainSyncStartupFailure",
      code: "invalid_startup",
    });
    const [pid] = watcherNativeExactPointServicePids();
    expect(pid).toBeDefined();
    await expect(open()).rejects.toMatchObject({ code: "invalid_startup" });
    expect(watcherNativeExactPointServicePids()).toEqual([pid]);
    await closeWatcherNativeExactPointServices();
    expect(alive(pid!)).toBe(false);
    expect(watcherNativeExactPointServicePids()).toEqual([]);
  });

  it("releases the node connection of a cancelled session and keeps serving", async () => {
    const { dir, watcherConfig } = await workspace();
    const socketPath = join(dir, "node.socket");
    const node = await silentNode(socketPath);
    const controller = new AbortController();
    const pending = openWatcherNativeExactPointQuery({
      binaryPath: BINARY,
      watcherConfig: watcherConfig(socketPath),
      predecessor,
      target,
      timeoutMs: 60_000,
      signal: controller.signal,
    });
    const refusal = expect(pending).rejects.toThrow(/cancelled or expired/);
    await expect.poll(() => node.connections.length).toBe(1);
    const [pid] = watcherNativeExactPointServicePids();
    controller.abort();
    await refusal;
    // The helper ends the session and drops its node connection.
    await node.closed[0];
    await expect(
      openWatcherNativeExactPointQuery({
        binaryPath: BINARY,
        watcherConfig: watcherConfig(join(dir, "absent.socket")),
        predecessor,
        target,
        timeoutMs: 10_000,
      }),
    ).rejects.toMatchObject({ code: "invalid_startup" });
    expect(watcherNativeExactPointServicePids()).toEqual([pid]);
  });

  it("fails a query closed against a helper built before service mode", async () => {
    const { dir, watcherConfig } = await workspace();
    // The current binary without its service flag answers exactly as a
    // pre-service build answers an open frame: one invalid_startup line on
    // stdout, then exit 64.
    const stale = join(dir, "stale-helper");
    await writeFile(stale, `#!/bin/sh\nexec '${BINARY}'\n`);
    await chmod(stale, 0o700);
    await expect(
      openWatcherNativeExactPointQuery({
        binaryPath: stale,
        watcherConfig: watcherConfig(join(dir, "absent.socket")),
        predecessor,
        target,
        timeoutMs: 10_000,
      }),
    ).rejects.toThrow("native chain-sync process exited unexpectedly");
    const pids = watcherNativeExactPointServicePids();
    expect(pids).toEqual([]);
  });
});
