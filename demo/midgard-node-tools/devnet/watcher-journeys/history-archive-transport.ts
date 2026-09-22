import { execFile } from "node:child_process";
import { createServer } from "node:http";
import { connect, type Socket } from "node:net";
import { promisify } from "node:util";

export type JourneyHistoryTransportEnvironment = {
  NODE_USE_ENV_PROXY: "1";
  HTTPS_PROXY: string;
  NO_PROXY: string;
};

export const readJourneyHistoryPublishedPort = (
  bindings: readonly { HostIp: string; HostPort: string }[] | null | undefined,
): number => {
  const loopback = bindings?.filter(({ HostIp }) => HostIp === "127.0.0.1");
  const port = loopback?.[0]?.HostPort;
  if (
    loopback?.length !== 1 ||
    port === undefined ||
    !/^[1-9][0-9]*$/.test(port) ||
    Number(port) > 65535
  )
    throw new Error(
      "Archive requires one Docker-published 127.0.0.1 host port for 8443/tcp",
    );
  return Number(port);
};

/** Tunnel only these archive authorities; TLS still terminates at each archive. */
export const startJourneyHistoryTransport = async (
  targets: ReadonlyMap<string, number>,
) => {
  const routes = new Map(targets);
  const sockets = new Set<Socket>();
  const server = createServer((_request, response) => {
    response.writeHead(403).end();
  });
  server.on("connection", (socket) => {
    sockets.add(socket);
    socket.on("error", () => socket.destroy());
    socket.once("close", () => sockets.delete(socket));
  });
  server.on("connect", (request, client, head) => {
    const port = routes.get(request.url ?? "");
    if (port === undefined) {
      client.end("HTTP/1.1 403 Forbidden\r\nConnection: close\r\n\r\n");
      return;
    }
    const upstream = connect({ host: "127.0.0.1", port });
    sockets.add(upstream);
    upstream.setTimeout(2_000, () => upstream.destroy());
    upstream.once("close", () => {
      sockets.delete(upstream);
      client.destroy();
    });
    client.once("close", () => upstream.destroy());
    upstream.on("error", () => client.destroy());
    upstream.once("connect", () => {
      upstream.setTimeout(0);
      client.write("HTTP/1.1 200 Connection Established\r\n\r\n");
      if (head.length > 0) upstream.write(head);
      client.pipe(upstream).pipe(client);
    });
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", resolve);
  });
  const address = server.address();
  if (address === null || typeof address === "string")
    throw new Error("Archive tunnel did not bind a loopback port");
  const environment: JourneyHistoryTransportEnvironment = {
    NODE_USE_ENV_PROXY: "1",
    HTTPS_PROXY: `http://127.0.0.1:${address.port}`,
    NO_PROXY: "localhost,127.0.0.1,[::1]",
  };
  return {
    environment,
    close: async () => {
      for (const socket of sockets) socket.destroy();
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
    },
  };
};

/** Use the watcher's actual Node fetch/env path, including CA verification. */
export const verifyJourneyHistoryTransport = async (input: {
  endpoint: string;
  caPath: string;
  environment: JourneyHistoryTransportEnvironment;
  timeoutMs?: number;
}): Promise<void> => {
  await promisify(execFile)(
    process.execPath,
    [
      "--input-type=module",
      "-e",
      `
    import { setTimeout as pause } from "node:timers/promises";
    const deadline = performance.now() + Number(process.argv[2]);
    for (;;) {
      try {
        const response = await fetch(new URL("/__watcher_archive_readiness__", process.argv[1]), {
          signal: AbortSignal.timeout(Math.max(1, Math.ceil(Math.min(2000, deadline - performance.now())))),
        });
        await response.arrayBuffer();
        if (response.status !== 404) throw new Error("Archive HTTPS readiness expected 404, received " + response.status);
        break;
      } catch (error) {
        if (!["ECONNREFUSED", "ECONNRESET", "UND_ERR_SOCKET"].includes(error.cause?.code) && error.name !== "TimeoutError") throw error;
        if (performance.now() >= deadline) throw new Error("Archive HTTPS listener did not become ready", { cause: error });
        await pause(100);
      }
    }
  `,
      input.endpoint,
      String(input.timeoutMs ?? 10_000),
    ],
    {
      env: { ...input.environment, NODE_EXTRA_CA_CERTS: input.caPath },
      timeout: (input.timeoutMs ?? 10_000) + 5_000,
    },
  );
};
