import { execFileSync } from "node:child_process";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { request } from "node:http";
import { createServer } from "node:https";
import { createServer as createTcpServer } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import type { Duplex } from "node:stream";
import { setTimeout as pause } from "node:timers/promises";

import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  readJourneyHistoryPublishedPort,
  startJourneyHistoryTransport,
  verifyJourneyHistoryTransport,
} from "./history-archive-transport.js";

const authority = "192.0.2.10:8443";
const endpoint = `https://${authority}`;
let directory: string;
let caPath: string;
let key: string;
let cert: string;
const cleanup: (() => Promise<void>)[] = [];

beforeAll(async () => {
  directory = await mkdtemp(join(tmpdir(), "watcher-history-transport-"));
  caPath = join(directory, "certificate.pem");
  const keyPath = join(directory, "key.pem");
  execFileSync(
    "openssl",
    [
      "req",
      "-x509",
      "-newkey",
      "ed25519",
      "-noenc",
      "-keyout",
      keyPath,
      "-out",
      caPath,
      "-days",
      "1",
      "-subj",
      "/CN=journey-history-test",
      "-addext",
      "subjectAltName=IP:192.0.2.10",
    ],
    { stdio: "pipe" },
  );
  [key, cert] = await Promise.all([
    readFile(keyPath, "utf8"),
    readFile(caPath, "utf8"),
  ]);
});
afterEach(async () => {
  for (const close of cleanup.splice(0).reverse()) await close();
});
afterAll(async () => rm(directory, { recursive: true, force: true }));

const archive = async (status = 404, port = 0) => {
  const requests: { host: string | undefined; path: string | undefined }[] = [];
  const sockets = new Set<Duplex>();
  const server = createServer({ key, cert }, (request, response) => {
    requests.push({ host: request.headers.host, path: request.url });
    response.writeHead(status).end();
  });
  server.on("connection", (socket) => {
    sockets.add(socket);
    socket.once("close", () => sockets.delete(socket));
  });
  await new Promise<void>((resolve) =>
    server.listen(port, "127.0.0.1", resolve),
  );
  const address = server.address();
  if (address === null || typeof address === "string")
    throw new Error("No archive test port");
  cleanup.push(async () => {
    for (const socket of sockets) socket.destroy();
    await new Promise<void>((resolve, reject) =>
      server.close((error) => (error ? reject(error) : resolve())),
    );
  });
  return { port: address.port, requests };
};

const tunnel = async (port: number, target = authority) => {
  const transport = await startJourneyHistoryTransport(
    new Map([[target, port]]),
  );
  cleanup.push(transport.close);
  return transport;
};

describe("archive transport identity", () => {
  it("retries a starting listener and bounds an unavailable listener", async () => {
    const reservation = createTcpServer();
    await new Promise<void>((resolve) =>
      reservation.listen(0, "127.0.0.1", resolve),
    );
    const address = reservation.address();
    if (address === null || typeof address === "string")
      throw new Error("No reserved test port");
    await new Promise<void>((resolve, reject) =>
      reservation.close((error) => (error ? reject(error) : resolve())),
    );
    const transport = await tunnel(address.port);
    await expect(
      verifyJourneyHistoryTransport({
        endpoint,
        caPath,
        environment: transport.environment,
        timeoutMs: 150,
      }),
    ).rejects.toThrow("did not become ready");
    const readiness = verifyJourneyHistoryTransport({
      endpoint,
      caPath,
      environment: transport.environment,
      timeoutMs: 2_000,
    });
    await pause(150);
    const target = await archive(404, address.port);
    await readiness;
    expect(target.requests).toHaveLength(1);
  });
  it("uses the actual loopback mapping and rejects absent or unsafe bindings", () => {
    expect(
      readJourneyHistoryPublishedPort([
        { HostIp: "127.0.0.1", HostPort: "50379" },
      ]),
    ).toBe(50379);
    for (const bindings of [
      undefined,
      null,
      [],
      [{ HostIp: "0.0.0.0", HostPort: "50379" }],
      [{ HostIp: "127.0.0.1", HostPort: "" }],
      [{ HostIp: "127.0.0.1", HostPort: "65536" }],
      [{ HostIp: "127.0.0.1", HostPort: "0" }],
    ])
      expect(() => readJourneyHistoryPublishedPort(bindings)).toThrow(
        "Docker-published",
      );
  });

  it("preserves the original HTTPS authority and verified certificate across published-port changes", async () => {
    const first = await archive();
    const second = await archive();
    expect(first.port).not.toBe(second.port);
    for (const target of [first, second]) {
      const transport = await tunnel(target.port);
      await verifyJourneyHistoryTransport({
        endpoint,
        caPath,
        environment: transport.environment,
      });
      expect(target.requests).toEqual([
        { host: authority, path: "/__watcher_archive_readiness__" },
      ]);
    }
  });

  it("rejects a certificate for a different authority without sending an API request", async () => {
    const target = await archive();
    const other = "192.0.2.11:8443";
    const transport = await tunnel(target.port, other);
    await expect(
      verifyJourneyHistoryTransport({
        endpoint: `https://${other}`,
        caPath,
        environment: transport.environment,
      }),
    ).rejects.toThrow("ERR_TLS_CERT_ALTNAME_INVALID");
    expect(target.requests).toEqual([]);
  });

  it("rejects an untrusted certificate and an unexpected HTTP response", async () => {
    const target = await archive(200);
    const transport = await tunnel(target.port);
    const emptyCa = join(directory, "empty-ca.pem");
    await writeFile(emptyCa, "");
    await expect(
      verifyJourneyHistoryTransport({
        endpoint,
        caPath: emptyCa,
        environment: transport.environment,
      }),
    ).rejects.toThrow("DEPTH_ZERO_SELF_SIGNED_CERT");
    expect(target.requests).toEqual([]);
    await expect(
      verifyJourneyHistoryTransport({
        endpoint,
        caPath,
        environment: transport.environment,
      }),
    ).rejects.toThrow("expected 404, received 200");
    expect(target.requests).toHaveLength(1);
  });

  it("refuses unlisted CONNECT authorities and ordinary HTTP proxy requests", async () => {
    const target = await archive();
    const transport = await tunnel(target.port);
    const proxy = new URL(transport.environment.HTTPS_PROXY);
    const connectStatus = await new Promise<number | undefined>(
      (resolve, reject) => {
        const client = request({
          hostname: proxy.hostname,
          port: proxy.port,
          method: "CONNECT",
          path: "192.0.2.11:8443",
        });
        client.once("connect", (response, socket) => {
          socket.destroy();
          resolve(response.statusCode);
        });
        client.once("error", reject);
        client.end();
      },
    );
    expect(connectStatus).toBe(403);
    const response = await fetch(transport.environment.HTTPS_PROXY);
    expect(response.status).toBe(403);
    await response.arrayBuffer();
    expect(target.requests).toEqual([]);
  });
});
