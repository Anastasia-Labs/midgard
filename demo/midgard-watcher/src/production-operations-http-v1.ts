import { createServer, type Server } from "node:http";
import type { Socket } from "node:net";

import type { WatcherProductionOperationsObservabilityV1 } from "./production-operations-observability-v1.js";

export const WATCHER_PRODUCTION_OPERATIONS_HTTP_V1 =
  "midgard-watcher-production-operations-http-v1" as const;

const LOOPBACK_HOSTS = new Set(["127.0.0.1", "::1"]);
const MAXIMUM_REQUEST_TARGET_BYTES = 2_048;
const MAXIMUM_RESPONSE_BYTES = 1024 * 1024;
const FORCE_CLOSE_AFTER_MS = 5_000;

const endpoint = (
  value: string,
  unsafeAllowEphemeralPortForTest: boolean,
): URL => {
  let parsed: URL;
  try {
    parsed = new URL(value);
  } catch {
    throw new Error("watcher operations HTTP endpoint is invalid");
  }
  if (
    parsed.protocol !== "http:" ||
    !LOOPBACK_HOSTS.has(parsed.hostname.toLowerCase()) ||
    parsed.username.length !== 0 ||
    parsed.password.length !== 0 ||
    parsed.search.length !== 0 ||
    parsed.hash.length !== 0 ||
    !["", "/"].includes(parsed.pathname) ||
    parsed.port.length === 0 ||
    (!unsafeAllowEphemeralPortForTest && parsed.port === "0")
  ) {
    throw new Error(
      "watcher operations HTTP endpoint must be fixed loopback HTTP",
    );
  }
  return parsed;
};

const closeServer = async (
  server: Server,
  sockets: Set<Socket>,
): Promise<void> =>
  await new Promise<void>((resolve, reject) => {
    let settled = false;
    const finish = (error?: Error) => {
      if (settled) return;
      settled = true;
      clearTimeout(forceClose);
      if (error === undefined) resolve();
      else reject(error);
    };
    const forceClose = setTimeout(() => {
      for (const socket of sockets) socket.destroy();
    }, FORCE_CLOSE_AFTER_MS);
    forceClose.unref();
    server.close((error) => finish(error));
    server.closeIdleConnections();
  });

export type WatcherProductionOperationsHttpServerV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_OPERATIONS_HTTP_V1;
  endpoint: string;
  done: Promise<void>;
  close(): Promise<void>;
}>;

/**
 * Loopback-only, read-only operations surface. It exposes only the bounded,
 * secret-safe response objects produced by the admitted observability module.
 */
export const startWatcherProductionOperationsHttpServerV1 = async (input: {
  readonly endpoint: string;
  readonly observability: WatcherProductionOperationsObservabilityV1;
  readonly unsafeAllowEphemeralPortForTest?: boolean;
}): Promise<WatcherProductionOperationsHttpServerV1> => {
  const configured = endpoint(
    input.endpoint,
    input.unsafeAllowEphemeralPortForTest === true,
  );
  const sockets = new Set<Socket>();
  let closing = false;
  let closePromise: Promise<void> | undefined;
  let resolveDone!: () => void;
  let rejectDone!: (error: Error) => void;
  const done = new Promise<void>((resolve, reject) => {
    resolveDone = resolve;
    rejectDone = reject;
  });
  void done.catch(() => undefined);
  const server = createServer(async (request, response) => {
    try {
      const target = request.url ?? "";
      if (
        target.length === 0 ||
        Buffer.byteLength(target, "utf8") > MAXIMUM_REQUEST_TARGET_BYTES ||
        !target.startsWith("/") ||
        request.headers["transfer-encoding"] !== undefined ||
        (request.headers["content-length"] !== undefined &&
          request.headers["content-length"] !== "0")
      ) {
        response.writeHead(400, {
          "cache-control": "no-store",
          "content-type": "application/json; charset=utf-8",
          "x-content-type-options": "nosniff",
        });
        response.end('{"error":"invalid_request"}');
        return;
      }
      const result = await input.observability.handleHttpRequest(
        new Request(new URL(target, configured), {
          method: request.method,
        }),
      );
      const body = new Uint8Array(await result.arrayBuffer());
      if (body.byteLength > MAXIMUM_RESPONSE_BYTES) {
        throw new Error("watcher operations HTTP response exceeded its bound");
      }
      response.statusCode = result.status;
      result.headers.forEach((value, name) => response.setHeader(name, value));
      response.end(body);
    } catch {
      if (response.headersSent) {
        response.destroy();
        return;
      }
      response.writeHead(500, {
        "cache-control": "no-store",
        "content-type": "application/json; charset=utf-8",
        "x-content-type-options": "nosniff",
      });
      response.end('{"error":"internal_error"}');
    }
  });
  server.on("connection", (socket) => {
    sockets.add(socket);
    socket.once("close", () => sockets.delete(socket));
  });
  server.on("clientError", (_error, socket) => {
    if (!socket.destroyed) {
      socket.end(
        "HTTP/1.1 400 Bad Request\r\nConnection: close\r\nContent-Length: 0\r\n\r\n",
      );
    }
  });
  await new Promise<void>((resolve, reject) => {
    const onError = (error: Error) => {
      server.off("listening", onListening);
      reject(error);
    };
    const onListening = () => {
      server.off("error", onError);
      resolve();
    };
    server.once("error", onError);
    server.once("listening", onListening);
    server.listen(Number(configured.port), configured.hostname);
  });
  server.once("error", (error) => rejectDone(error));
  server.once("close", () => {
    if (closing) resolveDone();
    else rejectDone(new Error("watcher operations HTTP server stopped"));
  });
  const address = server.address();
  if (address === null || typeof address === "string") {
    await closeServer(server, sockets);
    throw new Error("watcher operations HTTP server address is unavailable");
  }
  const advertisedHostname =
    address.family === "IPv6" ? `[${address.address}]` : address.address;
  const advertisedEndpoint = `http://${advertisedHostname}:${address.port.toString()}`;
  return Object.freeze({
    schemaVersion: WATCHER_PRODUCTION_OPERATIONS_HTTP_V1,
    endpoint: advertisedEndpoint,
    done,
    close: () => {
      if (closePromise !== undefined) return closePromise;
      closing = true;
      closePromise = closeServer(server, sockets);
      return closePromise;
    },
  });
};
