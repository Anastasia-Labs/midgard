import { EventEmitter } from "node:events";
import { createServer, type Server, type Socket } from "node:net";
import { setImmediate as immediate } from "node:timers/promises";

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherLocalNodeAuthorityTransport,
  establishWatcherLocalNodeQueryTransport,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
} from "../../src/l1/l1-adapter.js";
import type { WatcherNativeChainSyncAuthority } from "../../src/l1/native-chain-sync.js";

const fixture = vi.hoisted(() => ({
  native: Object.freeze({}),
  nativeLive: true,
  mode: "real" as "real" | "timeout" | "foreign",
  created: [] as Socket[],
}));
// Native admission is covered by its own suite; these lifecycle tests exercise
// actual query sockets and opaque transport contexts with one fixed upstream.
vi.mock("../../src/l1/native-chain-sync.js", async (importOriginal) => ({
  ...(await importOriginal<
    typeof import("../../src/l1/native-chain-sync.js")
  >()),
  watcherNativeChainSyncAuthorityDetails: (authority: unknown) =>
    authority === fixture.native && fixture.nativeLive
      ? {
          network: "Preprod",
          authorityNodeId: "local-node",
          genesisIdentitySha256: "11".repeat(32),
          startupDigest: "22".repeat(32),
          socketPath: "/run/cardano/node.socket",
        }
      : null,
}));
vi.mock("node:net", async (importOriginal) => {
  const original = await importOriginal<typeof import("node:net")>();
  return {
    ...original,
    createConnection: (
      ...args: Parameters<typeof original.createConnection>
    ) => {
      const socket =
        fixture.mode === "timeout"
          ? new original.Socket()
          : original.createConnection(...args);
      if (fixture.mode === "foreign")
        socket.once("connect", () =>
          Object.defineProperty(socket, "remoteAddress", {
            value: "127.0.0.2",
          }),
        );
      fixture.created.push(socket);
      return socket;
    },
  };
});

const contexts: WatcherL1TransportAttestationContext[] = [];
const servers: Server[] = [];
const peers = new Set<Socket>();
let accepted = 0;
let peak = 0;
const spinUntil = async (ready: () => boolean) => {
  for (let index = 0; index < 1000; index += 1) {
    if (ready()) return;
    await immediate();
  }
  throw new Error("loopback transport did not reach expected state");
};
const open = async () => {
  const server = createServer((socket) => {
    peers.add(socket);
    accepted += 1;
    peak = Math.max(peak, peers.size);
    socket.once("close", () => peers.delete(socket));
    socket.on("error", () => socket.destroy());
  });
  servers.push(server);
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (address === null || typeof address === "string")
    throw new Error("missing loopback port");
  const native = establishWatcherLocalNodeAuthorityTransport(
    fixture.native as WatcherNativeChainSyncAuthority,
  );
  contexts.push(native);
  const query = await establishWatcherLocalNodeQueryTransport(native, {
    transportKind: "tcp",
    providerId: "local-ogmios",
    surface: "ogmios",
    endpoint: `ws://127.0.0.1:${address.port}`,
    connectTimeoutMs: 100,
  });
  contexts.push(query);
  return { server, native, query };
};
beforeEach(() => {
  vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
  fixture.nativeLive = true;
  fixture.mode = "real";
  fixture.created = [];
  accepted = 0;
  peak = 0;
});
afterEach(async () => {
  for (const context of contexts.splice(0))
    closeWatcherL1TransportAttestationContext(context);
  for (const socket of fixture.created) socket.destroy();
  for (const socket of peers) socket.destroy();
  await Promise.all(
    servers
      .splice(0)
      .map(
        (server) =>
          new Promise<void>((resolve, reject) =>
            server.close((error) =>
              error &&
              "code" in error &&
              error.code !== "ERR_SERVER_NOT_RUNNING"
                ? reject(error)
                : resolve(),
            ),
          ),
      ),
  );
  peers.clear();
  vi.useRealTimers();
});

describe("continuous local query transport ownership", () => {
  it("rolls over live loopback sockets without changing authority or exceeding two connections", async () => {
    const { query } = await open();
    const identity = watcherL1TransportAttestationDetails(query);
    expect(identity).not.toBeNull();
    for (let cycle = 0; cycle < 3; cycle += 1) {
      await vi.advanceTimersByTimeAsync(30_000);
      await spinUntil(() => accepted === cycle + 2 && peers.size === 1);
      expect(watcherL1TransportAttestationDetails(query)).toEqual(identity);
    }
    expect(peak).toBe(2);
    closeWatcherL1TransportAttestationContext(query);
    await spinUntil(() => peers.size === 0);
    await vi.advanceTimersByTimeAsync(90_000);
    expect(accepted).toBe(4);
    expect(watcherL1TransportAttestationDetails(query)).toBeNull();
  });

  it("waits for retired socket close acknowledgement before another rollover", async () => {
    const { query } = await open();
    const original = fixture.created[0]!;
    const emit = (event: string | symbol, ...args: unknown[]) =>
      EventEmitter.prototype.emit.call(original, event, ...args);
    let acknowledgeClose: (() => void) | undefined;
    vi.spyOn(original, "emit").mockImplementation(
      (event: string | symbol, ...args: unknown[]) => {
        if (event === "close") {
          acknowledgeClose = () => {
            emit(event, ...args);
          };
          return true;
        }
        return emit(event, ...args);
      },
    );
    await vi.advanceTimersByTimeAsync(30_000);
    await spinUntil(() => acknowledgeClose !== undefined);
    expect(watcherL1TransportAttestationDetails(query)).not.toBeNull();
    await vi.advanceTimersByTimeAsync(90_000);
    expect(fixture.created).toHaveLength(2);
    acknowledgeClose!();
    await vi.advanceTimersByTimeAsync(30_000);
    await spinUntil(() => fixture.created.length === 3 && peers.size === 1);
    expect(watcherL1TransportAttestationDetails(query)).not.toBeNull();
  });

  it("destroys a connecting candidate on owner close and never reactivates the context", async () => {
    const { query } = await open();
    fixture.mode = "timeout";
    await vi.advanceTimersByTimeAsync(30_000);
    expect(fixture.created).toHaveLength(2);
    closeWatcherL1TransportAttestationContext(query);
    await immediate();
    expect(fixture.created.every((socket) => socket.destroyed)).toBe(true);
    await vi.advanceTimersByTimeAsync(90_000);
    expect(fixture.created).toHaveLength(2);
    expect(watcherL1TransportAttestationDetails(query)).toBeNull();
  });

  it("fails closed when a rollover candidate times out", async () => {
    const { query } = await open();
    fixture.mode = "timeout";
    await vi.advanceTimersByTimeAsync(30_100);
    expect(watcherL1TransportAttestationDetails(query)).toBeNull();
    expect(fixture.created.every((socket) => socket.destroyed)).toBe(true);
    await vi.advanceTimersByTimeAsync(90_000);
    expect(fixture.created).toHaveLength(2);
  });

  it("fails closed on connection refusal instead of retaining a renewable authority", async () => {
    const { server, query } = await open();
    server.close();
    await vi.advanceTimersByTimeAsync(30_000);
    await spinUntil(() => watcherL1TransportAttestationDetails(query) === null);
    expect(fixture.created).toHaveLength(2);
    await vi.advanceTimersByTimeAsync(90_000);
    expect(fixture.created).toHaveLength(2);
  });

  it.each(["query", "native"] as const)(
    "never renews after loss of the %s transport authority",
    async (lost) => {
      const { query } = await open();
      if (lost === "query") {
        [...peers][0]!.destroy();
        await spinUntil(() => fixture.created[0]!.destroyed);
      } else fixture.nativeLive = false;
      expect(watcherL1TransportAttestationDetails(query)).toBeNull();
      await vi.advanceTimersByTimeAsync(90_000);
      expect(fixture.created).toHaveLength(1);
      expect(watcherL1TransportAttestationDetails(query)).toBeNull();
    },
  );

  it("rejects a candidate with a different remote identity", async () => {
    const { query } = await open();
    fixture.mode = "foreign";
    await vi.advanceTimersByTimeAsync(30_000);
    await spinUntil(
      () =>
        fixture.created.length === 2 &&
        fixture.created.every((socket) => socket.destroyed),
    );
    expect(watcherL1TransportAttestationDetails(query)).toBeNull();
    await vi.advanceTimersByTimeAsync(90_000);
    expect(fixture.created).toHaveLength(2);
  });
});
