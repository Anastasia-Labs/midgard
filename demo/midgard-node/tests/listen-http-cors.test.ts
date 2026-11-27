import { once } from "node:events";
import { createServer, request as httpRequest } from "node:http";
import type { AddressInfo } from "node:net";

import {
  HttpServer,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import {
  createHttpIngress,
  withHttpIngress,
} from "../src/commands/listen-http-ingress.js";
import { NodeConfig } from "../src/services/config.make-config.js";
import {
  DEFAULT_HTTP_RATE_LIMITS,
  type HttpRateLimitPolicy,
} from "../src/services/http-rate-limits.js";
import {
  makeNativeSubmitTx,
  wrapNativeSubmitTx,
} from "./database.test/fixtures.submission.js";
import {
  closeSharedPool,
  runCleanups,
  serveSubmit,
} from "./helpers/submit-request-server.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";

type RawReply = {
  readonly status: number;
  readonly headers: Readonly<Record<string, string | string[] | undefined>>;
  readonly body: string;
};

const servers: { close: () => Promise<void> }[] = [];

afterEach(async () => {
  for (const server of servers.splice(0)) await server.close();
  await runCleanups();
});
afterAll(closeSharedPool);

const sendRaw = (
  port: number,
  path: string,
  {
    method = "GET",
    headers = {},
    body,
  }: {
    readonly method?: string;
    readonly headers?: Record<string, string>;
    readonly body?: Buffer;
  } = {},
): Promise<RawReply> =>
  new Promise((resolve, reject) => {
    const req = httpRequest(
      { hostname: "127.0.0.1", port, path, method, headers },
      (res) => {
        const chunks: Buffer[] = [];
        res.on("data", (chunk: Buffer) => chunks.push(chunk));
        res.once("end", () =>
          resolve({
            status: res.statusCode!,
            headers: res.headers,
            body: Buffer.concat(chunks).toString("utf8"),
          }),
        );
      },
    );
    req.once("error", reject);
    req.end(body);
  });

const startIngressServer = async (policy?: HttpRateLimitPolicy) => {
  let invoked = 0;
  const ingress = createHttpIngress({
    now: () => 0,
    policy: policy ?? DEFAULT_HTTP_RATE_LIMITS,
  });
  const handler = Effect.gen(function* () {
    const req = yield* HttpServerRequest.HttpServerRequest;
    if (req.method === "POST") yield* req.arrayBuffer;
    invoked += 1;
    return yield* HttpServerResponse.json({ ok: true });
  });
  const server = createServer(
    Effect.runSync(
      NodeHttpServer.makeHandler(
        withHttpIngress(handler, ingress).pipe(
          Effect.provideService(
            NodeConfig,
            makeTestNodeConfig({ HTTP_TRUSTED_PROXY_PEERS: ["127.0.0.1"] }),
          ),
        ),
      ),
    ),
  );
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const port = (server.address() as AddressInfo).port;
  servers.push({
    close: () =>
      new Promise<void>((resolve, reject) => {
        server.closeAllConnections();
        server.close((error) => (error ? reject(error) : resolve()));
      }),
  });
  return { port, invoked: () => invoked };
};

describe("HTTP CORS and OPTIONS preflight", () => {
  it("answers OPTIONS on public routes with 204 and exact CORS headers without invoking the handler", async () => {
    const { port, invoked } = await startIngressServer();
    const client = { "x-midgard-client-ip": "198.51.100.10" };

    const utxos = await sendRaw(port, "/utxos", {
      method: "OPTIONS",
      headers: client,
    });
    expect(utxos.status).toBe(204);
    expect(utxos.body).toBe("");
    expect(utxos.headers["access-control-allow-origin"]).toBe("*");
    expect(utxos.headers["access-control-allow-methods"]).toBe(
      "GET, POST, OPTIONS",
    );
    expect(utxos.headers["access-control-allow-headers"]).toBe("Content-Type");
    expect(utxos.headers["access-control-max-age"]).toBe("86400");
    expect(utxos.headers["access-control-allow-credentials"]).toBeUndefined();

    const submit = await sendRaw(port, "/submit", {
      method: "OPTIONS",
      headers: { ...client, "content-type": "text/plain" },
    });
    expect(submit.status).toBe(204);
    expect(submit.headers["access-control-allow-methods"]).toBe(
      "POST, OPTIONS",
    );

    const blocks = await sendRaw(port, "/blocks", {
      method: "OPTIONS",
      headers: client,
    });
    expect(blocks.status).toBe(204);
    expect(blocks.headers["access-control-allow-methods"]).toBe("GET, OPTIONS");
    expect(invoked()).toBe(0);
  });

  it("enforces identity, target length, and read rate limit on public OPTIONS preflight", async () => {
    const { port } = await startIngressServer({
      enabled: true,
      classes: {
        ...DEFAULT_HTTP_RATE_LIMITS.classes,
        read: {
          peer: { burst: 1, perSecond: 0.1 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      },
    });

    const noIdentity = await sendRaw(port, "/blocks", { method: "OPTIONS" });
    expect(noIdentity.status).toBe(503);
    expect(noIdentity.headers["access-control-allow-origin"]).toBe("*");
    expect(noIdentity.headers["access-control-expose-headers"]).toBe(
      "Retry-After",
    );

    const tooLong = await sendRaw(port, `/blocks?${"a".repeat(2_049)}`, {
      method: "OPTIONS",
      headers: { "x-midgard-client-ip": "198.51.100.11" },
    });
    expect(tooLong.status).toBe(414);
    expect(tooLong.headers["access-control-allow-origin"]).toBe("*");

    const client = { "x-midgard-client-ip": "198.51.100.12" };
    expect(
      (await sendRaw(port, "/blocks", { method: "OPTIONS", headers: client }))
        .status,
    ).toBe(204);
    const limited = await sendRaw(port, "/blocks", {
      method: "OPTIONS",
      headers: client,
    });
    expect(limited.status).toBe(429);
    expect(limited.headers["access-control-allow-origin"]).toBe("*");
    expect(limited.headers["access-control-expose-headers"]).toBe(
      "Retry-After",
    );
    expect(limited.headers["retry-after"]).toBeDefined();
  });

  it("attaches CORS to public handled responses and ingress refusals, and never to admin, peer, or unknown routes", async () => {
    const { port } = await startIngressServer();
    const client = { "x-midgard-client-ip": "198.51.100.20" };

    const ok = await sendRaw(port, "/blocks", { headers: client });
    expect(ok.status).toBe(200);
    expect(ok.headers["access-control-allow-origin"]).toBe("*");
    expect(ok.headers["access-control-expose-headers"]).toBe("Retry-After");
    expect(ok.headers["access-control-allow-credentials"]).toBeUndefined();

    const badLength = await sendRaw(port, "/utxos", {
      method: "POST",
      headers: { ...client, "content-length": "01" },
    });
    expect(badLength.status).toBe(400);
    expect(badLength.headers["access-control-allow-origin"]).toBe("*");
    expect(badLength.headers["access-control-expose-headers"]).toBe(
      "Retry-After",
    );

    const tooLarge = await sendRaw(port, "/tx-status", {
      method: "POST",
      headers: {
        ...client,
        "content-type": "application/json",
        "content-length": "65537",
      },
    });
    expect(tooLarge.status).toBe(413);
    expect(tooLarge.headers["access-control-allow-origin"]).toBe("*");

    for (const privPath of [
      "/init",
      "/commit",
      "/operator/status",
      "/admin/session",
      "/peer/txs",
      "/peer/inventory",
    ]) {
      const opt = await sendRaw(port, privPath, {
        method: "OPTIONS",
        headers: client,
      });
      expect(opt.status).toBe(405);
      expect(opt.headers["access-control-allow-origin"]).toBeUndefined();
      expect(opt.headers["access-control-allow-methods"]).toBeUndefined();
      expect(opt.headers["access-control-expose-headers"]).toBeUndefined();
    }

    const adminOk = await sendRaw(port, "/admin/session", { headers: client });
    expect(adminOk.status).toBe(200);
    expect(adminOk.headers["access-control-allow-origin"]).toBeUndefined();
    expect(adminOk.headers["access-control-expose-headers"]).toBeUndefined();

    const peerOk = await sendRaw(port, "/peer/txs", {
      method: "POST",
      headers: { ...client, "content-length": "0" },
    });
    expect(peerOk.status).toBe(200);
    expect(peerOk.headers["access-control-allow-origin"]).toBeUndefined();

    const unknown = await sendRaw(port, "/not-a-route", {
      method: "OPTIONS",
      headers: client,
    });
    expect(unknown.status).toBe(404);
    expect(unknown.headers["access-control-allow-origin"]).toBeUndefined();
  });

  it("attaches CORS headers on raw POST /submit answers across success and error status codes", async () => {
    const node = await serveSubmit();
    const tx = makeNativeSubmitTx();
    const accepted = await node.call({
      body: wrapNativeSubmitTx(tx.txCanonicalCbor),
    });
    expect(accepted.status).toBe(202);
    expect(accepted.headers["access-control-allow-origin"]).toBe("*");
    expect(accepted.headers["access-control-expose-headers"]).toBe(
      "Retry-After",
    );
    expect(
      accepted.headers["access-control-allow-credentials"],
    ).toBeUndefined();

    const malformed = await node.call({ body: Buffer.from("ff00ff", "hex") });
    expect(malformed.status).toBe(400);
    expect(malformed.headers["access-control-allow-origin"]).toBe("*");
    expect(malformed.headers["access-control-expose-headers"]).toBe(
      "Retry-After",
    );

    const noClient = await node.call({ client: null });
    expect(noClient.status).toBe(503);
    expect(noClient.headers["access-control-allow-origin"]).toBe("*");
    expect(noClient.headers["access-control-expose-headers"]).toBe(
      "Retry-After",
    );
  });

  it("emits CORS and 204 preflight for public routes only while startup HTTP is booting", async () => {
    await Effect.runPromise(
      withStartupHttpServer(0, () =>
        Effect.gen(function* () {
          const port = yield* HttpServer.addressWith((address) =>
            address._tag === "TcpAddress"
              ? Effect.succeed(address.port)
              : Effect.die("expected TCP"),
          );

          const preflight = yield* Effect.promise(() =>
            sendRaw(port, "/utxos", { method: "OPTIONS" }),
          );
          expect(preflight.status).toBe(204);
          expect(preflight.headers["access-control-allow-origin"]).toBe("*");
          expect(preflight.headers["access-control-allow-methods"]).toBe(
            "GET, POST, OPTIONS",
          );
          expect(preflight.headers["access-control-allow-headers"]).toBe(
            "Content-Type",
          );
          expect(preflight.headers["access-control-max-age"]).toBe("86400");

          const health = yield* Effect.promise(() => sendRaw(port, "/healthz"));
          expect(health.status).toBe(200);
          expect(health.headers["access-control-allow-origin"]).toBe("*");
          expect(health.headers["access-control-expose-headers"]).toBe(
            "Retry-After",
          );

          const booting = yield* Effect.promise(() => sendRaw(port, "/blocks"));
          expect(booting.status).toBe(503);
          expect(booting.headers["access-control-allow-origin"]).toBe("*");
          expect(booting.headers["access-control-expose-headers"]).toBe(
            "Retry-After",
          );

          const adminPreflight = yield* Effect.promise(() =>
            sendRaw(port, "/commit", { method: "OPTIONS" }),
          );
          expect(adminPreflight.status).toBe(503);
          expect(
            adminPreflight.headers["access-control-allow-origin"],
          ).toBeUndefined();

          const peerBoot = yield* Effect.promise(() =>
            sendRaw(port, "/peer/txs", { method: "POST" }),
          );
          expect(peerBoot.status).toBe(503);
          expect(
            peerBoot.headers["access-control-allow-origin"],
          ).toBeUndefined();
        }),
      ),
    );
  });
});
