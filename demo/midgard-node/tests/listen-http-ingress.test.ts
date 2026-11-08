import { once } from "node:events";
import { createServer, request as httpRequest } from "node:http";
import type { AddressInfo } from "node:net";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  Headers,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import { HTTP_ROUTE_ACTIONS } from "../src/commands/http/routes.js";
import {
  claimAdminMutation,
  getAdminSessionHandler,
  signAdminMutation,
  withAdminAccess,
} from "../src/commands/listen-admin-access.js";
import {
  createHttpIngress,
  httpClientIdentity,
  httpPeerIdentity,
  withHttpIngress,
} from "../src/commands/listen-http-ingress.js";
import { HTTP_LIVENESS_MAX_ENTRIES } from "../src/commands/listen-http-rate-limit.js";
import { encodeReadPage } from "../src/commands/listen-read-page.js";
import { stateQueueHeaderPage } from "../src/commands/listen-state-queue-page.js";
import { NodeConfig } from "../src/services/config.js";

const config = { ADMIN_API_KEY: "01".repeat(32) } as never;
const session = async (configuration = config) => {
  const result = await Effect.runPromise(
    getAdminSessionHandler.pipe(
      Effect.provideService(NodeConfig, configuration),
    ),
  );
  return (await HttpServerResponse.toWeb(result).json()).session as string;
};
const signed = (
  id: string,
  target = "/commit",
  nonce = "ab".repeat(32),
  timestamp = Date.now().toString(),
) => ({
  "x-midgard-admin-session": id,
  "x-midgard-admin-nonce": nonce,
  "x-midgard-admin-timestamp": timestamp,
  "x-midgard-admin-signature": signAdminMutation(
    "01".repeat(32),
    id,
    target,
    timestamp,
    nonce,
  ),
});

describe("HTTP admission quotas and admin replay", () => {
  it("uses the socket peer and normalizes IPv4 mapped addresses", () => {
    expect(httpPeerIdentity(Option.some("::ffff:192.0.2.1"))).toBe("192.0.2.1");
    expect(httpPeerIdentity(Option.none())).toBeUndefined();
    expect(httpPeerIdentity(Option.some("forged.example"))).toBeUndefined();
  });
  it("accepts a client header only from a socket peer in a trusted proxy range", () => {
    const headers = Headers.fromInput({
      "x-midgard-client-ip": "198.51.100.1",
      "x-forwarded-for": "203.0.113.9",
    });
    const identity = (
      remote: string,
      peers: readonly string[],
      requestHeaders = headers,
    ) =>
      httpClientIdentity(
        { remoteAddress: Option.some(remote), headers: requestHeaders },
        { HTTP_TRUSTED_PROXY_PEERS: peers, HTTP_SOCKET_PATH: undefined },
      );
    expect(identity("192.0.2.1", ["10.0.0.2"])).toBe("192.0.2.1");
    expect(identity("10.0.0.2", ["10.0.0.2"])).toBe("198.51.100.1");
    // A load balancer's published IPv4 and IPv6 ranges.
    expect(identity("35.191.7.9", ["35.191.0.0/16"])).toBe("198.51.100.1");
    expect(identity("35.192.0.1", ["35.191.0.0/16"])).toBe("35.192.0.1");
    expect(identity("2a06:98c7::9", ["2a06:98c0::/29"])).toBe("198.51.100.1");
    expect(identity("2a06:98c8::9", ["2a06:98c0::/29"])).toBe("2a06:98c8::9");
    expect(identity("::ffff:10.1.2.3", ["10.0.0.0/8"])).toBe("198.51.100.1");
    expect(identity("10.0.0.2", ["10.0.0.2"], Headers.empty)).toBeUndefined();
    expect(
      identity(
        "10.0.0.2",
        ["10.0.0.2"],
        Headers.fromInput({
          "x-midgard-client-ip": "198.51.100.1, 203.0.113.1",
        }),
      ),
    ).toBeUndefined();
  });
  it("names the client from the ingress header on the Unix socket, and only there", () => {
    const socket = (
      remoteAddress: Option.Option<string>,
      headers: Headers.Headers,
    ) =>
      httpClientIdentity(
        { remoteAddress, headers },
        {
          HTTP_TRUSTED_PROXY_PEERS: ["10.0.0.2"],
          HTTP_SOCKET_PATH: "/run/midgard-http/node.sock",
        },
      );
    const named = Headers.fromInput({ "x-midgard-client-ip": "2001:db8::7" });
    expect(socket(Option.none(), named)).toBe("2001:db8::7");
    expect(socket(Option.none(), Headers.empty)).toBeUndefined();
    // No TCP peer can reach a node serving on its socket.
    expect(socket(Option.some("10.0.0.2"), named)).toBeUndefined();
  });
  it("bounds active work and bytes without queuing; releases are idempotent", () => {
    const ingress = createHttpIngress({ now: () => 0 });
    const first = ingress.reserve(
      "192.0.2.1",
      "write",
      MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    );
    const second = ingress.reserve(
      "192.0.2.2",
      "write",
      MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    );
    expect(first.accepted).toBe(true);
    expect(second.accepted).toBe(true);
    expect(ingress.reserve("192.0.2.3", "write", 1)).toMatchObject({
      accepted: false,
      status: 503,
    });
    if (first.accepted) {
      first.release();
      first.release();
    }
    expect(ingress.snapshot()).toMatchObject({
      submitActive: 1,
      inFlightBytes: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    });
    if (second.accepted) second.release();
    expect(ingress.snapshot()).toMatchObject({
      submitActive: 0,
      inFlightBytes: 0,
    });
  });
  it("reserves a full protocol body for submission even while advisory bytes are full", () => {
    const ingress = createHttpIngress({ now: () => 0 });
    const advisory = ingress.reserve(
      "192.0.2.1",
      "evaluate",
      MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    );
    expect(advisory.accepted).toBe(true);
    expect(ingress.reserve("192.0.2.2", "evaluate", 1)).toMatchObject({
      accepted: false,
      status: 503,
    });
    const admission = ingress.reserve(
      "192.0.2.3",
      "write",
      MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    );
    expect(admission.accepted).toBe(true);
    if (advisory.accepted) advisory.release();
    if (admission.accepted) admission.release();
    expect(ingress.snapshot()).toMatchObject({
      active: 0,
      inFlightBytes: 0,
      advisoryBytes: 0,
    });
  });
  it("keeps operator-peer clients, which no global bucket bounds, in a bounded table of their own", () => {
    let now = 0;
    const ingress = createHttpIngress({ now: () => now });
    for (let index = 0; index < HTTP_LIVENESS_MAX_ENTRIES + 100; index += 1) {
      now += 1;
      const result = ingress.reserve(
        `10.${(index >> 16) & 255}.${(index >> 8) & 255}.${index & 255}`,
        "peer",
        0,
      );
      expect(result.accepted).toBe(true);
      if (result.accepted) result.release();
    }
    expect(ingress.snapshot().livenessPeers).toBe(HTTP_LIVENESS_MAX_ENTRIES);
    const reader = ingress.reserve("192.0.2.1", "read", 0);
    expect(reader.accepted).toBe(true);
    if (reader.accepted) reader.release();
  });
  it("binds mutation signatures to method/target/session and claims even failed execution", async () => {
    const id = await session();
    const headers = signed(id);
    expect(claimAdminMutation(config, "01".repeat(32), "/merge", headers)).toBe(
      401,
    );
    expect(
      claimAdminMutation(config, "01".repeat(32), "/commit", headers),
    ).toBeUndefined();
    expect(
      claimAdminMutation(config, "01".repeat(32), "/commit", headers),
    ).toBe(409);
    const restarted = {};
    expect(
      claimAdminMutation(
        restarted,
        "01".repeat(32),
        "/commit",
        signed(id, "/commit", "cd".repeat(32)),
      ),
    ).toBe(401);
    expect(
      claimAdminMutation(
        config,
        "01".repeat(32),
        "/commit",
        signed(
          id,
          "/commit",
          "ef".repeat(32),
          (Date.now() - 31_000).toString(),
        ),
      ),
    ).toBe(401);
  });
  it("declares every protected mutation as POST", () => {
    for (const path of [
      "/init",
      "/commit",
      "/merge",
      "/logBlocksDB",
      "/logGlobals",
    ])
      expect(HTTP_ROUTE_ACTIONS.filter((action) => action[1] === path)).toEqual(
        [["POST", path, "admin"]],
      );
  });
  it("bounds queue pages in chain order and refuses cursors from a changed view", () => {
    const headers = ["ff", "00", "ab"].map((byte) => byte.repeat(28));
    const view = Buffer.alloc(32, 1);
    const first = stateQueueHeaderPage(
      headers,
      { limit: 2, cursor: undefined },
      view,
    );
    expect(first.kind).toBe("page");
    if (first.kind !== "page") throw new Error("Expected queue page");
    expect(first.entries).toEqual(headers.slice(0, 2));
    expect(first.nextCursor).not.toBeNull();
    const cursor = Buffer.from(first.nextCursor!, "hex");
    expect(stateQueueHeaderPage(headers, { limit: 2, cursor }, view)).toEqual({
      kind: "page",
      entries: headers.slice(2),
      nextCursor: null,
    });
    expect(
      stateQueueHeaderPage(headers, { limit: 2, cursor }, Buffer.alloc(32, 2)),
    ).toEqual({ kind: "stale" });
    expect(
      stateQueueHeaderPage(headers.slice(2), { limit: 2, cursor }, view),
    ).toEqual({ kind: "stale" });
  });
  it("does not skip a row omitted to satisfy the response byte cap", () => {
    const rows = [
      { key: Buffer.alloc(32, 1), body: "x".repeat(600_000) },
      { key: Buffer.alloc(32, 2), body: "y".repeat(600_000) },
    ];
    const page = encodeReadPage(
      rows,
      { limit: 2, cursor: undefined },
      (row) => row.key,
      (row) => row.body,
    );
    expect(page.entries).toHaveLength(1);
    expect(page.nextCursor).toBe(rows[0]!.key.toString("hex"));
  });
  it("enforces real socket rate/target/body limits and ignores forged forwarded identities", async () => {
    let invoked = 0;
    const handler = Effect.gen(function* () {
      const req = yield* HttpServerRequest.HttpServerRequest;
      if (req.method === "POST") yield* req.arrayBuffer;
      invoked += 1;
      return yield* HttpServerResponse.json({ ok: true });
    });
    const handle = Effect.runSync(
      NodeHttpServer.makeHandler(
        withHttpIngress(handler, createHttpIngress({ now: () => 0 })).pipe(
          Effect.provideService(NodeConfig, {} as never),
        ),
      ),
    );
    const server = createServer(handle);
    server.listen(0, "127.0.0.1");
    await once(server, "listening");
    const port = (server.address() as AddressInfo).port;
    const call = (
      path: string,
      method = "GET",
      headers: Record<string, string> = {},
    ) =>
      new Promise<number>((resolve, reject) => {
        const req = httpRequest(
          { hostname: "127.0.0.1", port, path, method, headers },
          (res) => {
            res.resume();
            res.once("end", () => resolve(res.statusCode!));
          },
        );
        req.once("error", reject);
        req.end();
      });
    try {
      expect(await call("/commit")).toBe(405);
      expect(invoked).toBe(0);
      expect(await call("/tx?" + "a".repeat(2_049))).toBe(414);
      expect(invoked).toBe(0);
      expect(
        await call("/tx-status", "POST", {
          "content-type": "application/json",
          "content-length": "65537",
        }),
      ).toBe(413);
      expect(invoked).toBe(0);
      for (let index = 0; index < 30; index += 1)
        expect(
          await call("/tx", "GET", {
            "x-forwarded-for": `192.0.2.${index + 1}`,
          }),
        ).toBe(200);
      expect(
        await call("/tx", "GET", { "x-forwarded-for": "198.51.100.1" }),
      ).toBe(429);
      expect(invoked).toBe(30);
    } finally {
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
    }
  });
  it("refuses unsigned mutation before the effect and consumes a signed claim before failure", async () => {
    const id = await session({ ADMIN_API_KEY: "01".repeat(32) } as never);
    let invoked = 0;
    const configuration = { ADMIN_API_KEY: "01".repeat(32) } as never;
    const current = await session(configuration);
    const request = (headers: Record<string, string>, method = "POST") =>
      HttpServerRequest.fromWeb(
        new Request("http://node.test/commit", { method, headers }),
      );
    const handler = withAdminAccess(
      "commit",
      Effect.sync(() => {
        invoked += 1;
        throw new Error("operation fails");
      }),
    );
    const unsigned = await Effect.runPromise(
      handler.pipe(
        Effect.provideService(NodeConfig, configuration),
        Effect.provideService(HttpServerRequest.HttpServerRequest, request({})),
      ),
    );
    expect((unsigned as HttpServerResponse.HttpServerResponse).status).toBe(
      401,
    );
    expect(invoked).toBe(0);
    expect(id).not.toBe(current);
    const headers = signed(current, "/commit", "aa".repeat(32));
    await expect(
      Effect.runPromise(
        handler.pipe(
          Effect.provideService(NodeConfig, configuration),
          Effect.provideService(
            HttpServerRequest.HttpServerRequest,
            request(headers),
          ),
        ),
      ),
    ).rejects.toThrow("operation fails");
    expect(invoked).toBe(1);
    const replay = await Effect.runPromise(
      handler.pipe(
        Effect.provideService(NodeConfig, configuration),
        Effect.provideService(
          HttpServerRequest.HttpServerRequest,
          request(headers),
        ),
      ),
    );
    expect((replay as HttpServerResponse.HttpServerResponse).status).toBe(409);
    expect(invoked).toBe(1);
  });
});
