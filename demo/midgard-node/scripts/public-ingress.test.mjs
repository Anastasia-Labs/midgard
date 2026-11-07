// The public TLS ingress as nginx runs it: TLS from the client, plain HTTP to
// a stand-in node on a Unix socket. Needs an nginx binary (MIDGARD_NGINX, or
// nginx on PATH; GitHub's Ubuntu runners ship one) and openssl.
import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createServer } from "node:http";
import { Agent, request } from "node:https";
import { createServer as createNetServer } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { after, before, describe, test } from "node:test";

import { publicApiHostname, renderPublicIngress } from "./public-ingress.mjs";

const HOST = "api.example.org";
const ENV = { MIDGARD_PUBLIC_API_ORIGIN: `https://${HOST}` };
const BODY_BYTES = 295_048;
const SOCKET = "/run/midgard-http/node.sock";

describe("public ingress inputs", () => {
  test("refuses an origin that is not a bare HTTPS hostname", () => {
    for (const origin of [
      undefined,
      "http://api.example.org",
      "https://api.example.org:8443",
      "https://api.example.org/v1",
      "https://user@api.example.org",
      "https://api.example.org?x=1",
      "https://api_example.org",
    ])
      assert.throws(
        () => publicApiHostname({ MIDGARD_PUBLIC_API_ORIGIN: origin }),
        /E_PUBLIC_INGRESS_ORIGIN/u,
        String(origin),
      );
    assert.equal(publicApiHostname(ENV), HOST);
  });

  test("refuses a socket path or body cap that could change the configuration", () => {
    for (const socketPath of [
      undefined,
      "node.sock",
      "/run/midgard-http/../node.sock",
      "/run/midgard http/node.sock",
      "/run/node.sock; include /etc/passwd",
      "/run/node.sock'",
      `/run/${"a".repeat(100)}`,
    ])
      assert.throws(
        () =>
          renderPublicIngress(ENV, { socketPath, submitBodyBytes: BODY_BYTES }),
        /E_PUBLIC_INGRESS_SOCKET/u,
        String(socketPath),
      );
    for (const submitBodyBytes of [undefined, 0, -1, 1.5, "295048", 2 ** 53])
      assert.throws(
        () => renderPublicIngress(ENV, { socketPath: SOCKET, submitBodyBytes }),
        /E_PUBLIC_INGRESS_BODY/u,
        String(submitBodyBytes),
      );
  });
});

const nginxBinary = () => {
  for (const candidate of [
    process.env.MIDGARD_NGINX,
    "nginx",
    "/usr/sbin/nginx",
  ])
    if (
      candidate &&
      spawnSync(candidate, ["-v"], { encoding: "utf8" }).status === 0
    )
      return candidate;
  return undefined;
};
const nginx = nginxBinary();
// CI must run it; a local run without nginx reports it skipped.
const skip =
  nginx === undefined && process.env.CI === undefined
    ? "no nginx binary (set MIDGARD_NGINX)"
    : false;

const freePort = () =>
  new Promise((resolve, reject) => {
    const server = createNetServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const { port } = server.address();
      server.close(() => resolve(port));
    });
  });

describe("public ingress served by nginx", { skip }, () => {
  /** What the stand-in node saw. */
  const seen = { requests: [], connections: 0 };
  let directory;
  let socketPath;
  let port;
  let ca;
  let node;
  const run = (...args) =>
    spawnSync(
      nginx,
      ["-p", directory, "-c", join(directory, "nginx.conf"), ...args],
      { encoding: "utf8" },
    );

  const startNode = async () => {
    node = createServer({ keepAliveTimeout: 65_000 }, (req, res) => {
      seen.requests.push({
        method: req.method,
        url: req.url,
        clientIp: req.headers["x-midgard-client-ip"],
        contentLength: req.headers["content-length"],
      });
      req.resume();
      req.once("end", () => {
        if (req.url === "/slow") setTimeout(() => res.end("{}"), 400);
        else if (req.url === "/limited") {
          res.writeHead(429, {
            "content-type": "application/json",
            "retry-after": "7",
          });
          res.end('{"error":"Request rate exceeded"}');
        } else {
          res.writeHead(200, { "content-type": "application/json" });
          res.end('{"ok":true}');
        }
      });
    });
    node.maxRequestsPerSocket = 0;
    node.on("connection", () => (seen.connections += 1));
    await new Promise((resolve) => node.listen(socketPath, resolve));
    chmodSync(socketPath, 0o660);
  };

  before(async () => {
    assert.ok(nginx, "CI runs the ingress under nginx; none was found");
    directory = mkdtempSync(join(tmpdir(), "midgard-ingress-"));
    mkdirSync(join(directory, "tmp"));
    socketPath = join(directory, "node.sock");
    port = await freePort();
    const openssl = spawnSync(
      "openssl",
      [
        ...["req", "-x509", "-newkey", "ec"],
        ...["-pkeyopt", "ec_paramgen_curve:P-256", "-nodes", "-days", "1"],
        ...["-keyout", join(directory, "key.pem")],
        ...["-out", join(directory, "cert.pem")],
        ...["-subj", `/CN=${HOST}`, "-addext", `subjectAltName=DNS:${HOST}`],
      ],
      { encoding: "utf8" },
    );
    assert.equal(openssl.status, 0, openssl.stderr);
    ca = readFileSync(join(directory, "cert.pem"));
    // The rendered configuration, moved into this sandbox: its files, port
    // and logs. Everything that decides how requests are served is kept.
    const config = renderPublicIngress(ENV, {
      socketPath,
      submitBodyBytes: BODY_BYTES,
    })
      .replace("/run/public/tls-cert.pem", join(directory, "cert.pem"))
      .replace("/run/secrets/tls-key.pem", join(directory, "key.pem"))
      .replace("/tmp/nginx.pid", join(directory, "nginx.pid"))
      .replaceAll(/(_temp_path) \/tmp\//gu, `$1 ${directory}/tmp/`)
      .replace("error_log /dev/null", `error_log ${directory}/error.log`)
      .replace("access_log /dev/stdout", `access_log ${directory}/access.log`)
      .replace("listen 8443 ssl", `listen 127.0.0.1:${port} ssl`);
    writeFileSync(join(directory, "nginx.conf"), config);
    const checked = run("-e", join(directory, "error.log"), "-t");
    assert.equal(checked.status, 0, checked.stderr);
    await startNode();
    const started = run("-e", join(directory, "error.log"));
    assert.equal(started.status, 0, started.stderr);
    // nginx accepts once its workers listen.
    for (let attempt = 0; attempt < 50; attempt += 1) {
      const ready = await send("/healthz").catch(() => undefined);
      if (ready?.status === 200) break;
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
    seen.requests.length = 0;
    seen.connections = 0;
  });

  after(async () => {
    if (directory === undefined) return;
    agent.destroy();
    let master;
    try {
      master = Number(readFileSync(join(directory, "nginx.pid"), "utf8"));
    } catch {
      // nginx never started.
    }
    run("-e", join(directory, "error.log"), "-s", "stop");
    // nginx stops asynchronously; wait for its master before removing its files.
    for (let attempt = 0; master !== undefined && attempt < 100; attempt += 1)
      try {
        process.kill(master, 0);
        await new Promise((resolve) => setTimeout(resolve, 50));
      } catch {
        break;
      }
    node?.close();
    node?.closeAllConnections();
    rmSync(directory, { recursive: true, force: true });
  });

  const agent = new Agent({ keepAlive: true, maxSockets: 4 });
  const send = (
    path,
    { method = "GET", body, headers = {}, via = agent } = {},
  ) =>
    new Promise((resolve, reject) => {
      const outgoing = request(
        {
          host: "127.0.0.1",
          port,
          servername: HOST,
          ca,
          agent: via,
          path,
          method,
          headers: { host: HOST, ...headers },
        },
        (response) => {
          const chunks = [];
          response.on("data", (chunk) => chunks.push(chunk));
          response.once("end", () =>
            resolve({
              status: response.statusCode,
              rawHeaders: response.rawHeaders,
              retryAfter: response.headers["retry-after"],
              body: Buffer.concat(chunks).toString(),
            }),
          );
        },
      );
      outgoing.once("error", reject);
      outgoing.end(body);
    });
  const count = (results) =>
    results.reduce(
      (tally, { status }) => ({ ...tally, [status]: (tally[status] ?? 0) + 1 }),
      {},
    );

  test("passes 1,000 requests a second from one client to the node, over reused socket connections", async () => {
    const start = Date.now();
    const pending = [];
    for (let index = 0; index < 1_000; index += 1) {
      const wait = start + index - Date.now();
      if (wait > 0) await new Promise((resolve) => setTimeout(resolve, wait));
      pending.push(
        index % 2 === 0
          ? send("/utxos?address=addr_test1")
          : send("/submit", {
              method: "POST",
              body: Buffer.alloc(200),
              headers: {
                "content-type": "application/vnd.midgard.v1+cbor",
                // A client cannot name itself to the node.
                "x-midgard-client-ip": "203.0.113.9",
              },
            }),
      );
    }
    const results = await Promise.all(pending);
    // No refusal of nginx's own: the node's limiter decides.
    assert.deepEqual(count(results), { 200: 1_000 });
    assert.equal(seen.requests.length, 1_000);
    assert.ok(
      seen.requests.every(({ clientIp }) => clientIp === "127.0.0.1"),
      "the node is told the connecting address, never the client's header",
    );
    assert.ok(
      seen.requests
        .filter(({ method }) => method === "POST")
        .every(({ contentLength }) => contentLength === "200"),
    );
    assert.ok(
      seen.connections <= 8,
      `${seen.connections} socket connections for 1,000 requests`,
    );
  });

  test("passes the node's own refusals through unchanged", async () => {
    const limited = await send("/limited");
    assert.equal(limited.status, 429);
    assert.equal(limited.body, '{"error":"Request rate exceeded"}');
    // One Retry-After, the node's.
    const retryAfter = limited.rawHeaders.filter(
      (name, index) => index % 2 === 0 && name.toLowerCase() === "retry-after",
    );
    assert.equal(retryAfter.length, 1);
    assert.equal(limited.retryAfter, "7");
  });

  test("caps a submission at one envelope", async () => {
    const at = await send("/submit", {
      method: "POST",
      body: Buffer.alloc(BODY_BYTES),
    });
    const over = await send("/submit", {
      method: "POST",
      body: Buffer.alloc(BODY_BYTES + 1),
      via: new Agent({ keepAlive: false }),
    });
    assert.equal(at.status, 200);
    assert.equal(over.status, 413);
  });

  test("refuses a fifth concurrent request from one address, and floods of health checks, in the node's JSON shape", async () => {
    const fresh = new Agent({ keepAlive: false });
    const slow = await Promise.all(
      Array.from({ length: 6 }, () => send("/slow", { via: fresh })),
    );
    assert.deepEqual(count(slow), { 200: 4, 503: 2 });
    const busy = slow.find(({ status }) => status === 503);
    assert.equal(busy.retryAfter, "5");
    assert.equal(JSON.parse(busy.body).errorCode, "E_HTTP_INGRESS_CAPACITY");
    const health = await Promise.all(
      Array.from({ length: 60 }, () => send("/healthz")),
    );
    const limited = health.find(({ status }) => status === 429);
    assert.ok(limited, JSON.stringify(count(health)));
    assert.equal(limited.retryAfter, "1");
    assert.equal(JSON.parse(limited.body).errorCode, "E_HTTP_RATE_LIMITED");
  });

  test("answers 503 with Retry-After while the node's socket is gone, and serves again once it is back", async () => {
    await new Promise((resolve) => {
      node.close(resolve);
      node.closeAllConnections();
    });
    rmSync(socketPath, { force: true });
    const down = await send("/utxos");
    assert.equal(down.status, 503);
    assert.equal(down.retryAfter, "5");
    assert.equal(JSON.parse(down.body).errorCode, "E_HTTP_INGRESS_CAPACITY");
    await startNode();
    assert.equal((await send("/utxos")).status, 200);
  });
});
