import { execFileSync } from "node:child_process";
import { createHash, createPrivateKey, X509Certificate } from "node:crypto";
import {
  appendFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  rmSync,
} from "node:fs";
import { createServer as createHttpServer } from "node:http";
import { createServer as createHttpsServer } from "node:https";
import { connect, type Socket } from "node:net";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

import { writeDurableFile, writeOnceFile } from "./durable.js";
import { type Layout, type RunEnv, servicePorts } from "./layout.js";

/**
 * Two independent historical native-script history providers. The watcher
 * admits only external HTTPS providers, and it pins its provider roster
 * (endpoints included) into its store on first use, so each provider gets a
 * stable DNS name that resolves nowhere: the watcher reaches it through a
 * CONNECT tunnel that maps exactly these names to the providers' loopback
 * listeners, and TLS still terminates at each provider under its own key.
 */
export const HISTORY_ROLES = ["a", "b"] as const;
export type HistoryRole = (typeof HISTORY_ROLES)[number];

const HISTORY_PORT = 443;

export type HistoryProvider = {
  readonly sourceId: string;
  readonly operatorIdentitySha256: string;
  readonly authorityEndpoint: string;
};

export type HistoricalNativeScriptHistory = {
  readonly sourceMode: "external_provider_quorum";
  readonly consistencyPolicy: "exact_bytes_all_providers_v1";
  readonly providers: readonly HistoryProvider[];
};

const dnsLabel = (value: string) =>
  value
    .toLowerCase()
    .replace(/[^a-z0-9-]/gu, "-")
    .replace(/^-+|-+$/gu, "") || "run";

export const historyHost = (run: RunEnv, role: HistoryRole) =>
  `history-${role}.${dnsLabel(run.runId)}.midgard-devnet.internal`;

const archiveFile = (layout: Layout, role: HistoryRole, name: string) =>
  join(layout.watcherHistoryArchive(role), name);

const readableKey = (path: string) => {
  try {
    return createPrivateKey(readFileSync(path)).asymmetricKeyType === "ed25519";
  } catch {
    return false;
  }
};

/**
 * Creates the provider's TLS key and certificate once (the operator identity
 * is the key's SPKI digest, so the key is never replaced), then derives the
 * roster entry from the certificate on disk.
 */
const ensureProviderIdentity = (
  layout: Layout,
  run: RunEnv,
  role: HistoryRole,
): HistoryProvider => {
  const host = historyHost(run, role);
  const directory = layout.watcherHistoryArchive(role);
  for (const sub of ["records", "canonical", "native-scripts"])
    mkdirSync(join(directory, sub), { recursive: true, mode: 0o700 });
  const keyPath = archiveFile(layout, role, "key.pem");
  const certificatePath = archiveFile(layout, role, "certificate.pem");
  if (!existsSync(certificatePath)) {
    // No certificate pins the key yet, so an unreadable key (openssl or this
    // controller killed mid-write) is replaced. The key is made durable before
    // any certificate names it.
    if (!readableKey(keyPath)) {
      const pendingKey = `${keyPath}.pending`;
      execFileSync(
        "openssl",
        ["genpkey", "-algorithm", "ed25519", "-out", pendingKey],
        { stdio: "pipe", timeout: 30_000 },
      );
      writeDurableFile(keyPath, readFileSync(pendingKey), 0o600);
      rmSync(pendingKey);
    }
    const pending = `${certificatePath}.pending`;
    execFileSync(
      "openssl",
      [
        "req",
        "-x509",
        "-key",
        keyPath,
        "-out",
        pending,
        "-days",
        "3650",
        "-subj",
        `/CN=${host}`,
        "-addext",
        `subjectAltName=DNS:${host}`,
      ],
      { stdio: "pipe", timeout: 30_000 },
    );
    writeDurableFile(certificatePath, readFileSync(pending), 0o644);
    rmSync(pending);
  }
  const certificate = new X509Certificate(readFileSync(certificatePath));
  if (!certificate.checkHost(host))
    throw new Error(`${certificatePath} does not name ${host}`);
  return {
    sourceId: `devnet-history-${role}`,
    operatorIdentitySha256: createHash("sha256")
      .update(certificate.publicKey.export({ type: "spki", format: "der" }))
      .digest("hex"),
    authorityEndpoint: `https://${host}`,
  };
};

/**
 * Provider identities, their authority records and the CA bundle the watcher
 * trusts, all written once. `releaseFinality` is the watcher release's
 * verified finality authority; each provider answers only for it.
 */
export const ensureHistoryProviders = (
  layout: Layout,
  run: RunEnv,
  releaseFinality: { readonly deploymentIdentityDigest: string },
  deploymentFingerprint: string,
): HistoricalNativeScriptHistory => {
  if (releaseFinality.deploymentIdentityDigest !== deploymentFingerprint)
    throw new Error(
      "the history providers' release differs from the deployment",
    );
  const providers = HISTORY_ROLES.map((role) =>
    ensureProviderIdentity(layout, run, role),
  );
  HISTORY_ROLES.forEach((role, index) => {
    const provider = providers[index]!;
    writeOnceFile(
      archiveFile(layout, role, "authority.json"),
      JSON.stringify({
        releaseFinality,
        sourceId: provider.sourceId,
        operatorIdentitySha256: provider.operatorIdentitySha256,
      }),
    );
  });
  writeOnceFile(
    layout.watcherHistoryCa,
    HISTORY_ROLES.map((role) =>
      readFileSync(archiveFile(layout, role, "certificate.pem"), "utf8"),
    ).join("\n"),
    0o644,
  );
  const configuration: HistoricalNativeScriptHistory = {
    sourceMode: "external_provider_quorum",
    consistencyPolicy: "exact_bytes_all_providers_v1",
    providers,
  };
  writeOnceFile(
    layout.watcherHistoryProviders,
    `${JSON.stringify(configuration, null, 2)}\n`,
    0o644,
  );
  return configuration;
};

/** What a process needs in its environment to reach the providers. */
export const historyTransportEnvironment = (layout: Layout, run: RunEnv) => ({
  NODE_USE_ENV_PROXY: "1",
  HTTPS_PROXY: `http://127.0.0.1:${servicePorts(run).historyTunnel}`,
  NO_PROXY: "localhost,127.0.0.1,[::1]",
  NODE_EXTRA_CA_CERTS: layout.watcherHistoryCa,
});

type ArchiveDispatch = (
  method: string | undefined,
  url: string | undefined,
  body: unknown,
) => { status: number; value?: unknown };

export type Listening = { readonly close: () => Promise<void> };

/** Serves until SIGTERM or SIGINT, then closes. */
export const untilSignalled = async (listening: Listening) => {
  await new Promise<void>((resolve) => {
    for (const signal of ["SIGTERM", "SIGINT"] as const)
      process.once(signal, () => resolve());
  });
  await listening.close();
};

/**
 * One provider: the watcher journeys' archive dispatch (the only
 * implementation of the provider API) behind this provider's own HTTPS
 * identity, on its loopback port.
 */
export const startHistoryArchive = async (
  layout: Layout,
  run: RunEnv,
  role: HistoryRole,
): Promise<Listening> => {
  const directory = layout.watcherHistoryArchive(role);
  const { createHistoryArchiveDispatch } = (await import(
    pathToFileURL(
      join(
        layout.toolsRoot,
        "devnet/watcher-journeys/history-archive-server.mjs",
      ),
    ).href
  )) as {
    createHistoryArchiveDispatch: (
      directory: string,
      authority: unknown,
    ) => ArchiveDispatch;
  };
  const dispatch = createHistoryArchiveDispatch(
    directory,
    JSON.parse(readFileSync(join(directory, "authority.json"), "utf8")),
  );
  const server = createHttpsServer(
    {
      key: readFileSync(join(directory, "key.pem")),
      cert: readFileSync(join(directory, "certificate.pem")),
    },
    (request, response) =>
      void (async () => {
        try {
          const chunks: Buffer[] = [];
          let size = 0;
          for await (const chunk of request as AsyncIterable<Buffer>) {
            size += chunk.length;
            if (size > 16_384) {
              response.writeHead(413).end();
              return;
            }
            chunks.push(chunk);
          }
          const body =
            size === 0
              ? undefined
              : JSON.parse(Buffer.concat(chunks).toString("utf8"));
          const result = dispatch(request.method, request.url, body);
          appendFileSync(
            join(directory, "requests.ndjson"),
            `${JSON.stringify({ at: new Date().toISOString(), method: request.method, url: request.url, status: result.status })}\n`,
          );
          response
            .writeHead(result.status, { "content-type": "application/json" })
            .end(
              result.value === undefined ? "" : JSON.stringify(result.value),
            );
        } catch {
          response.writeHead(400).end();
        }
      })(),
  );
  const port = servicePorts(run).historyArchive(HISTORY_ROLES.indexOf(role));
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(port, "127.0.0.1", resolve);
  });
  console.log(
    JSON.stringify({
      service: `history-archive-${role}`,
      state: "ready",
      port,
    }),
  );
  return {
    close: () =>
      new Promise<void>((resolve) => {
        server.closeAllConnections();
        server.close(() => resolve());
      }),
  };
};

/**
 * The CONNECT tunnel: only the providers' names are routed, each to its own
 * loopback listener; everything else is refused. `GET /healthz` answers the
 * supervisor.
 */
export const startHistoryTunnel = async (run: RunEnv): Promise<Listening> => {
  const ports = servicePorts(run);
  const routes = new Map(
    HISTORY_ROLES.map((role, index) => [
      `${historyHost(run, role)}:${HISTORY_PORT}`,
      ports.historyArchive(index),
    ]),
  );
  const sockets = new Set<Socket>();
  const server = createHttpServer((request, response) => {
    if (request.method === "GET" && request.url === "/healthz")
      response.writeHead(200).end("ok");
    else response.writeHead(403).end();
  });
  server.on("connection", (socket) => {
    sockets.add(socket);
    socket.on("error", () => socket.destroy());
    socket.once("close", () => sockets.delete(socket));
  });
  server.on("connect", (request, client: Socket, head: Buffer) => {
    const port = routes.get(request.url ?? "");
    if (port === undefined) {
      client.end("HTTP/1.1 403 Forbidden\r\nConnection: close\r\n\r\n");
      return;
    }
    const upstream = connect({ host: "127.0.0.1", port });
    sockets.add(upstream);
    upstream.setTimeout(5_000, () => upstream.destroy());
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
    server.listen(ports.historyTunnel, "127.0.0.1", resolve);
  });
  console.log(
    JSON.stringify({
      service: "history-tunnel",
      state: "ready",
      routes: [...routes.keys()],
    }),
  );
  return {
    close: () =>
      new Promise<void>((resolve) => {
        for (const socket of sockets) socket.destroy();
        server.close(() => resolve());
      }),
  };
};
