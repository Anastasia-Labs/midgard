import { createHash } from "node:crypto";
import { createServer } from "node:http";
import type { Duplex } from "node:stream";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect, it } from "vitest";

import { captureWatcherStateQueueRead } from "../../src/indexers/authenticated-state-queue-observation.capture-read.js";
import {
  createWatcherStateQueueReadScopes,
  WatcherStateQueueReadRetired,
} from "../../src/indexers/authenticated-state-queue-observation.read-scopes.js";
import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import {
  readWatcherNativeChainSyncEventReceipt,
  startWatcherNativeChainSync,
  startWatcherNativeChainSyncWithRetry,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncEventReceipt,
  watcherNativeChainSyncEventReceipt,
} from "../../src/l1/native-chain-sync.js";
import { parseWatcherConfig } from "../../src/runtime/config.js";
import { createWatcherNativeEventHandler } from "../../src/runtime/watcher-runtime.create-watcher-native-event-handler.js";
import { makeDeploymentAuthority } from "../support/deployment-authority-fixture.js";
import {
  config,
  INTERSECTION,
  readIdentityFixture,
  waitFor,
} from "./native-chain-sync.config.js";
import { fakeNodeTransport } from "./native-chain-sync.fake-transport.js";

const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((yes) => {
    resolve = yes;
  });
  return { promise, resolve };
};
// An actual loopback WS handshake and stalled Ogmios request. Echo close so the
// production physical-session owner observes drain rather than a fake result.
const stalledOgmios = async () => {
  let requests = 0;
  let closed = 0;
  const sockets = new Set<Duplex>();
  const server = createServer();
  server.on("upgrade", (request, socket) => {
    const key = request.headers["sec-websocket-key"]!;
    const accept = createHash("sha1")
      .update(key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
      .digest("base64");
    socket.write(
      "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: " +
        accept +
        "\r\n\r\n",
    );
    sockets.add(socket);
    socket.on("data", (chunk) => {
      if ((chunk[0]! & 15) === 8) socket.end(Buffer.from([0x88, 0]));
      else requests += 1;
    });
    socket.on("error", () => undefined);
    socket.on("close", () => {
      sockets.delete(socket);
      closed += 1;
    });
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const address = server.address();
  if (address === null || typeof address === "string")
    throw new Error("missing WS test port");
  return {
    endpoint: `ws://127.0.0.1:${address.port}`,
    requests: () => requests,
    closed: () => closed,
    close: async () => {
      for (const socket of sockets) socket.destroy();
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
    },
  };
};

it.each(["exit", "stream_failure", "abort", "rollback"] as const)(
  "aborts the actual native read transport on %s after native provenance revocation",
  async (lifecycle) => {
    const ogmios = await stalledOgmios();
    const original = config();
    if (original.l1.source.sourceMode !== "local_node")
      throw new Error("local fixture required");
    const watcherConfig = parseWatcherConfig({
      ...original,
      da: {
        ...original.da,
        peers: [
          {
            identity: "da-peer-a",
            multiaddr:
              "/dns4/da.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
          },
        ],
      },
      l1: {
        ...original.l1,
        requestTimeoutMs: 5000,
        source: {
          ...original.l1.source,
          queryServices: original.l1.source.queryServices.map((service) =>
            service.kind === "ogmios"
              ? { ...service, endpoint: ogmios.endpoint }
              : service,
          ),
        },
        finality: {
          depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
          },
        },
      },
    });
    const deploymentIdentity = makeDeploymentAuthority().result;
    const scopes = createWatcherStateQueueReadScopes({
      watcherConfig,
      deploymentIdentity,
    });
    const queueSource = createWatcherLocalKupmiosRawSource({
      watcherConfig,
      deploymentIdentity,
    });
    const nativeSignal = new AbortController();
    const gate = deferred();
    const transport = await fakeNodeTransport();
    let receipt: WatcherNativeChainSyncEventReceipt | undefined;
    let event: WatcherNativeChainSyncEvent | undefined;
    let retiredReceiptAtHook = false;
    let read: Promise<unknown> | undefined;
    const handler = createWatcherNativeEventHandler({
      coordinator: Promise.resolve({
        handle: async (received) => {
          if (received.kind !== "roll_forward") return;
          event = received;
          receipt = watcherNativeChainSyncEventReceipt(received)!;
          read = captureWatcherStateQueueRead({
            queueSource,
            readScopes: scopes,
            observationDepth: "release_finality",
            read: ({ rawSource }) => rawSource.readBoundary(),
          }).catch((error) => error as unknown);
          await waitFor(() => ogmios.requests() > 0);
          if (lifecycle !== "rollback") await gate.promise;
        },
      }),
      onCaughtUp: () => undefined,
      onRollbackArrived: scopes.invalidate,
    });
    const common = {
      binaryPath: transport.binaryPath,
      watcherConfig,
      startupTimeoutMs: 2000,
      onEvent: handler,
      onAuthorityRevoked: () => {
        retiredReceiptAtHook =
          event !== undefined &&
          watcherNativeChainSyncEventReceipt(event) === null;
        scopes.invalidate();
      },
      unsafeReadIdentityFileForTest: readIdentityFixture,
    };
    const runtime =
      lifecycle === "abort"
        ? await startWatcherNativeChainSync({
            ...common,
            intersection: INTERSECTION,
            signal: nativeSignal.signal,
          })
        : await startWatcherNativeChainSyncWithRetry({
            ...common,
            intersectionCandidates: [INTERSECTION],
          });
    try {
      await waitFor(() => ogmios.requests() === 1);
      if (lifecycle === "exit") process.kill(transport.pid(), "SIGKILL");
      else if (lifecycle === "stream_failure") transport.failStreams();
      else if (lifecycle === "abort")
        nativeSignal.abort(new Error("native lifetime aborted"));
      expect(await read).toBeInstanceOf(WatcherStateQueueReadRetired);
      await waitFor(() => ogmios.closed() === 1);
      expect(ogmios.requests()).toBe(1);
      expect(() => readWatcherNativeChainSyncEventReceipt(receipt!)).toThrow(
        "absent or stale",
      );
      if (lifecycle !== "rollback") expect(retiredReceiptAtHook).toBe(true);
    } finally {
      gate.resolve();
      scopes.close();
      await runtime.close();
      await read;
      await ogmios.close();
    }
  },
);

it("runs native cleanup and reports an unexpected lifetime-hook fault", async () => {
  const gate = deferred();
  const transport = await fakeNodeTransport();
  let reached = false;
  const fault = new Error("infallible lifetime hook failed");
  const runtime = await startWatcherNativeChainSync({
    binaryPath: transport.binaryPath,
    watcherConfig: config(),
    intersection: INTERSECTION,
    startupTimeoutMs: 2000,
    onEvent: async () => {
      reached = true;
      await gate.promise;
    },
    onAuthorityRevoked: () => {
      throw fault;
    },
    unsafeReadIdentityFileForTest: readIdentityFixture,
  });
  try {
    await waitFor(() => reached);
    transport.failStreams();
    const close = runtime.close();
    const rejected = expect(close).rejects.toMatchObject({
      message: "native read lifetime revocation failed",
      cause: fault,
    });
    gate.resolve();
    await rejected;
    await expect(runtime.done).rejects.toThrow(
      "native read lifetime revocation failed",
    );
  } finally {
    gate.resolve();
    await runtime.close().catch(() => undefined);
  }
});
