/**
 * Where the node's HTTP server listens: plain HTTP on TCP, or on a Unix socket
 * behind the TLS ingress.
 */
import { chmodSync, lstatSync, statSync, unlinkSync } from "node:fs";
import { createServer as createHttpServer, type Server } from "node:http";
import type { ListenOptions } from "node:net";
import { dirname } from "node:path";

/** A refused HTTP setting; its reason never carries a secret. */
export class PublicHttpConfigError extends Error {
  constructor(readonly reason: string) {
    super(`E_PUBLIC_HTTP_CONFIG: ${reason}`);
  }
}
export const failHttpConfig = (reason: string): never => {
  throw new PublicHttpConfigError(reason);
};

export type HttpListenerTransport = {
  readonly createServer: () => Server;
  /** Where the server listens, given the configured TCP port. */
  readonly listenOptions: (port: number) => ListenOptions;
};

/** Plain HTTP on every interface, on PORT. */
export const tcpTransport: HttpListenerTransport = {
  createServer: createHttpServer,
  listenOptions: (port) => ({ port, host: "0.0.0.0" }),
};

/**
 * Plain HTTP on a Unix socket, for the TLS ingress in front of the node. A
 * socket has no network address, and its directory must admit no other
 * users: whoever reaches the socket names its own client
 * (x-midgard-client-ip). A socket a killed node left behind is replaced, so a
 * restart needs no manual cleanup; anything else at the path is refused.
 */
export const unixSocketTransport = (path: string): HttpListenerTransport => {
  let directory;
  try {
    directory = statSync(dirname(path));
  } catch {
    return failHttpConfig(
      "the HTTP_SOCKET_PATH directory is missing or unreadable",
    );
  }
  if (!directory.isDirectory())
    return failHttpConfig("the HTTP_SOCKET_PATH directory is not a directory");
  if ((directory.mode & 0o007) !== 0)
    return failHttpConfig(
      "the HTTP_SOCKET_PATH directory grants other users access; make it mode 0700 or 0750",
    );
  const existing = () => lstatSync(path, { throwIfNoEntry: false });
  const found = existing();
  if (found !== undefined && !found.isSocket())
    return failHttpConfig(
      "HTTP_SOCKET_PATH names an existing file that is not a socket",
    );
  return {
    listenOptions: () => ({ path }),
    createServer: () => {
      // A closing server removes its socket; one still there is stale.
      if (existing()?.isSocket() === true) unlinkSync(path);
      const server = createHttpServer({
        requestTimeout: 30_000,
        headersTimeout: 10_000,
        maxHeaderSize: 8_192,
      });
      server.maxConnections = 512;
      // The ingress keeps its connections open and ends them itself: its
      // upstream keepalive_timeout (60 s) is shorter than this, and it caps
      // requests per connection, so the node never closes one under it.
      server.keepAliveTimeout = 65_000;
      server.maxRequestsPerSocket = 0;
      server.once("listening", () => chmodSync(path, 0o660));
      return server;
    },
  };
};
