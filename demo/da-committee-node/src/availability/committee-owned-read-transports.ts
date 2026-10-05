import { connect as connectTcp, isIP, type Socket } from "node:net";
import { Readable } from "node:stream";
import { connect as connectTls } from "node:tls";

import type {
  DaAvailabilityOperationContext,
  DaAvailabilityReadScope,
} from "@al-ft/midgard-sdk";
import { fetch as ownedFetch, Pool } from "undici";
import WebSocket from "ws";

import type { StateQueueReplayWebSocketFactory } from "../l1/state-queue-replay-provider.open-rpc.js";

/** A private configured attempt owns these transports. Destroy/terminate join
 * local resources; remote TCP/TLS acknowledgements are not a completion proof. */
export const committeeOwnedReadTransports = () => {
  const pools = new Map<
    string,
    { pool: Pool; requests: Map<DaAvailabilityReadScope, number> }
  >();
  const sockets = new Set<WebSocket>();
  const closing = new Set<Promise<void>>();
  const connections = new Set<Socket>();
  const connectOwned = (
    endpoint: URL,
    scope: DaAvailabilityReadScope,
    openingOnly = false,
  ): Socket => {
    assertOpen(scope);
    if (connections.size >= 16)
      throw new Error("Owned committee connection limit exceeded");
    const host = endpoint.hostname.replace(/^\[|\]$/gu, "");
    const secure =
      endpoint.protocol === "https:" || endpoint.protocol === "wss:";
    const port = Number(endpoint.port) || (secure ? 443 : 80);
    const socket = secure
      ? connectTls({
          host,
          port,
          servername: isIP(host) ? undefined : host,
          ALPNProtocols: ["http/1.1"],
        })
      : connectTcp({ host, port });
    connections.add(socket);
    let resolveClosed!: () => void;
    const closed = new Promise<void>((resolve) => {
      resolveClosed = resolve;
    });
    closing.add(closed);
    const stop = () => socket.destroy();
    const timer = setTimeout(stop, Math.max(1, Math.ceil(scope.remainingMs())));
    timer.unref();
    const detachOpeningScope = () => {
      clearTimeout(timer);
      scope.signal.removeEventListener("abort", stop);
    };
    if (openingOnly)
      socket.once(secure ? "secureConnect" : "connect", detachOpeningScope);
    socket.on("error", () => undefined);
    socket.once("close", () => {
      detachOpeningScope();
      connections.delete(socket);
      closing.delete(closed);
      resolveClosed();
    });
    scope.signal.addEventListener("abort", stop, { once: true });
    if (scope.signal.aborted) stop();
    return socket;
  };
  let draining: Promise<void> | undefined;
  const assertOpen = (scope: DaAvailabilityReadScope) => {
    scope.assertCurrent();
    if (draining) throw new Error("Committee read resources are draining");
  };
  const fetchFor =
    (scope: DaAvailabilityReadScope): typeof fetch =>
    async (input, init) => {
      assertOpen(scope);
      const request = new Request(input, init);
      if (request.keepalive && request.body !== null)
        throw new Error(
          "Owned HTTP does not support streaming keepalive bodies",
        );
      const endpoint = new URL(request.url);
      if (!["http:", "https:"].includes(endpoint.protocol))
        throw new Error("Owned committee HTTP endpoint is invalid");
      let entry = pools.get(endpoint.origin);
      if (!entry) {
        if (pools.size >= 2)
          throw new Error("Owned committee HTTP origin limit exceeded");
        const requests = new Map<DaAvailabilityReadScope, number>();
        const pool = new Pool(endpoint.origin, {
          connections: 4,
          pipelining: 1,
          connect: (_options, callback) => {
            let socket: Socket;
            try {
              const active = [...requests.keys()].find(
                (requestScope) => !requestScope.signal.aborted,
              );
              if (!active)
                throw new Error("HTTP connector has no active request");
              socket = connectOwned(new URL(endpoint.origin), active, true);
            } catch (error) {
              callback(
                error instanceof Error
                  ? error
                  : new Error("Owned connector failed", { cause: error }),
                null,
              );
              return;
            }
            let reported = false;
            const failed = (error: Error) => {
              if (!reported) {
                reported = true;
                callback(error, null);
              }
            };
            socket.once(
              endpoint.protocol === "https:" ? "secureConnect" : "connect",
              () => {
                if (!reported) {
                  reported = true;
                  callback(null, socket);
                }
              },
            );
            socket.once("error", failed);
            socket.once("close", () =>
              failed(new Error("Owned HTTP connection closed before ready")),
            );
          },
        });
        entry = { pool, requests };
        pools.set(endpoint.origin, entry);
      }
      // Normalize the native Request before crossing Undici's Request realm.
      // Native construction applies init overrides and rejects consumed bodies.
      entry.requests.set(scope, (entry.requests.get(scope) ?? 0) + 1);
      try {
        return (await ownedFetch(request.url, {
          method: request.method,
          headers: [...request.headers.entries()],
          body:
            request.body === null
              ? undefined
              : Readable.fromWeb(
                  request.body as import("node:stream/web").ReadableStream<Uint8Array>,
                ),
          duplex: "half",
          redirect: "error",
          signal: AbortSignal.any([scope.signal, request.signal]),
          cache: request.cache,
          credentials: request.credentials,
          integrity: request.integrity,
          keepalive: request.keepalive,
          mode: request.mode,
          referrer: request.referrer,
          referrerPolicy: request.referrerPolicy,
          dispatcher: entry.pool,
        })) as unknown as Response;
      } finally {
        const remaining = (entry.requests.get(scope) ?? 1) - 1;
        if (remaining === 0) entry.requests.delete(scope);
        else entry.requests.set(scope, remaining);
      }
    };
  const webSocketFor =
    (
      scope: DaAvailabilityReadScope,
      maxPayload: number,
    ): StateQueueReplayWebSocketFactory =>
    (url) => {
      assertOpen(scope);
      if (sockets.size >= 8)
        throw new Error("Owned committee WebSocket limit exceeded");
      const socket = new WebSocket(url, {
        maxPayload,
        perMessageDeflate: false,
        createConnection: () => connectOwned(new URL(url), scope),
        handshakeTimeout: Math.max(1, Math.ceil(scope.remainingMs())),
      });
      sockets.add(socket);
      let resolveClosed!: () => void;
      const closed = new Promise<void>((resolve) => {
        resolveClosed = resolve;
      });
      closing.add(closed);
      const terminate = () => socket.terminate();
      // Opening termination emits an error before close. Consumers get their
      // own error event; this listener also covers drain before their setup.
      socket.on("error", () => undefined);
      socket.once("close", () => {
        scope.signal.removeEventListener("abort", terminate);
        sockets.delete(socket);
        closing.delete(closed);
        resolveClosed();
      });
      scope.signal.addEventListener("abort", terminate, { once: true });
      if (scope.signal.aborted) terminate();
      return {
        send: (data) => socket.send(data),
        close: terminate,
        addEventListener: (type, listener, options) => {
          if (
            type !== "open" &&
            type !== "close" &&
            type !== "error" &&
            type !== "message"
          )
            throw new Error("Unsupported committee WebSocket event");
          socket.addEventListener(
            type,
            (event) => listener(event as never),
            options,
          );
        },
      };
    };
  const drain = (): Promise<void> => {
    if (draining) return draining;
    for (const socket of sockets) socket.terminate();
    for (const socket of connections) socket.destroy();
    const ownedPools = [...pools.values()];
    pools.clear();
    draining = Promise.all([
      ...ownedPools.map(({ pool }) => pool.destroy()),
      ...closing,
    ])
      .then(() => undefined)
      .finally(() => {
        draining = undefined;
      });
    return draining;
  };
  return {
    fetchFor,
    webSocketFor,
    drain,
    assertDrained: () => {
      if (
        draining ||
        sockets.size ||
        pools.size ||
        closing.size ||
        connections.size
      )
        throw new Error("Committee read resources have not drained");
    },
  };
};

type Owner = ReturnType<typeof committeeOwnedReadTransports>;
const owners = new WeakMap<DaAvailabilityReadScope, Owner>();
export const registerCommitteeReadOwner = (
  scope: DaAvailabilityReadScope,
  owner: Owner,
): void => {
  owners.set(scope, owner);
};
export const inheritCommitteeReadOwner = (
  parent: DaAvailabilityReadScope,
  child?: DaAvailabilityReadScope,
): void => {
  const owner = owners.get(parent);
  if (owner && child) owners.set(child, owner);
};
export const committeeReadOwner = (
  scope: DaAvailabilityReadScope,
): Owner | undefined => owners.get(scope);
export const drainCommitteeReadResources = async (
  scope: DaAvailabilityReadScope,
): Promise<void> => {
  await owners.get(scope)?.drain();
};

/** SDK observation creates child scopes. Carry ownership explicitly alongside
 * its existing absolute deadline/signal; no global mutable transport binding. */
export const committeeBoundReadContext = (
  context: DaAvailabilityOperationContext,
  parent: DaAvailabilityReadScope,
): DaAvailabilityOperationContext => ({
  ...context,
  assertActuationCurrent: async (child) => {
    inheritCommitteeReadOwner(parent, child);
    await context.assertActuationCurrent(child ?? parent);
  },
  ...(context.readBoundary
    ? {
        readBoundary: async (child?: DaAvailabilityReadScope) => {
          inheritCommitteeReadOwner(parent, child);
          return context.readBoundary!(child ?? parent);
        },
      }
    : {}),
  ...(context.observe
    ? {
        observe: async (intent, child) => {
          inheritCommitteeReadOwner(parent, child);
          return context.observe!(intent, child ?? parent);
        },
      }
    : {}),
});
