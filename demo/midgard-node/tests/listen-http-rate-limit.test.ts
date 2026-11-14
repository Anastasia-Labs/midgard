import { ConfigProvider, Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { httpIngressConfig } from "../src/services/config.http-proxy-peers.js";
import { DEFAULT_HTTP_RATE_LIMITS } from "../src/services/http-rate-limits.js";
import {
  closeServers,
  policyWith,
  proxied,
  serve,
} from "./listen-http-ingress.serve-fixture.js";

afterEach(closeServers);

describe("HTTP request rate limits", () => {
  it("refuses a client over its rate with 429, Retry-After and the structured error, then recovers after refill", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        read: {
          peer: { burst: 3, perSecond: 0.5 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      }),
    });
    expect(await node.statuses(3, "/tx", { client: "198.51.100.1" })).toEqual([
      200, 200, 200,
    ]);
    const refused = await node.call("/tx", { client: "198.51.100.1" });
    expect(refused).toEqual({
      status: 429,
      retryAfter: "2",
      body: {
        error: "Request rate exceeded",
        error_code: "E_HTTP_RATE_LIMITED",
      },
    });
    expect(node.invoked()).toBe(3);
    node.advance(1_000);
    expect((await node.call("/tx", { client: "198.51.100.1" })).status).toBe(
      429,
    );
    node.advance(1_000);
    expect((await node.call("/tx", { client: "198.51.100.1" })).status).toBe(
      200,
    );
  });

  it("does not refuse one client because another floods", async () => {
    const node = await serve(proxied);
    const flood = await node.statuses(60, "/tx", { client: "198.51.100.1" });
    expect(flood.filter((status) => status === 429).length).toBeGreaterThan(0);
    expect(await node.statuses(5, "/tx", { client: "198.51.100.2" })).toEqual([
      200, 200, 200, 200, 200,
    ]);
  });

  it("caps the sum of all clients with the global bucket", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        read: {
          peer: { burst: 10, perSecond: 1 },
          global: { burst: 5, perSecond: 1 },
        },
      }),
    });
    for (let index = 1; index <= 5; index += 1)
      expect(
        (await node.call("/tx", { client: `198.51.100.${index}` })).status,
      ).toBe(200);
    const refused = await node.call("/tx", { client: "198.51.100.6" });
    expect(refused.status).toBe(429);
    expect(refused.body.error_code).toBe("E_HTTP_RATE_LIMITED");
  });

  it("gives each class its default per-client burst: 100 submissions, 30 reads, 4 batch reads and 2 evaluations", async () => {
    const node = await serve(proxied);
    const accepted = async (
      count: number,
      path: string,
      method: string,
      client: string,
    ) =>
      (await node.statuses(count, path, { method, client })).filter(
        (status) => status === 200,
      ).length;
    expect(await accepted(120, "/submit", "POST", "198.51.100.3")).toBe(100);
    expect(await accepted(40, "/tx-status", "GET", "198.51.100.1")).toBe(30);
    expect(await accepted(40, "/tx-status", "POST", "198.51.100.2")).toBe(4);
    expect(await accepted(40, "/tx/evaluate", "POST", "198.51.100.4")).toBe(2);
    // Submissions refill at 20 a second.
    node.advance(1_000);
    expect(await accepted(30, "/submit", "POST", "198.51.100.3")).toBe(20);
  });

  it("ignores client-supplied forwarding headers without a trusted proxy", async () => {
    const node = await serve({
      policy: policyWith({
        read: {
          peer: { burst: 2, perSecond: 1 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      }),
    });
    const forged = (index: number) => ({
      "x-midgard-client-ip": `203.0.113.${index}`,
      "x-forwarded-for": `192.0.2.${index}`,
    });
    expect((await node.call("/tx", { headers: forged(1) })).status).toBe(200);
    expect((await node.call("/tx", { headers: forged(2) })).status).toBe(200);
    expect((await node.call("/tx", { headers: forged(3) })).status).toBe(429);
  });

  it("honours the client address set by a trusted proxy and only that header", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        read: {
          peer: { burst: 1, perSecond: 1 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      }),
    });
    for (let index = 1; index <= 3; index += 1)
      expect(
        (await node.call("/tx", { client: `198.51.100.${index}` })).status,
      ).toBe(200);
    // A trusted proxy that omits its client header supplies no identity.
    expect(
      await node.call("/tx", { headers: { "x-forwarded-for": "192.0.2.9" } }),
    ).toEqual({
      status: 503,
      retryAfter: "5",
      body: {
        error: "Socket peer identity is unavailable",
        error_code: "E_HTTP_CLIENT_IDENTITY_UNAVAILABLE",
      },
    });
  });

  it("groups IPv6 clients by /56 and keeps mapped IPv4 clients on their IPv4 bucket", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        read: {
          peer: { burst: 2, perSecond: 1 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      }),
    });
    const status = async (client: string) =>
      (await node.call("/tx", { client })).status;
    // 2001:db8:1:200::/56 holds 2001:db8:1:2ff:ffff::9.
    expect(await status("2001:db8:1:200::1")).toBe(200);
    expect(await status("2001:db8:1:2ff:ffff::9")).toBe(200);
    expect(await status("2001:0db8:0001:02ab::abcd")).toBe(429);
    expect(await status("2001:db8:1:300::1")).toBe(200);
    expect(await status("198.51.100.7")).toBe(200);
    expect(await status("::ffff:198.51.100.7")).toBe(200);
    expect(await status("::ffff:c633:6407")).toBe(429);
  });

  it("answers health, readiness and admin routes while public classes are flooded", async () => {
    const node = await serve(proxied);
    for (let index = 0; index < 400; index += 1)
      await node.call("/tx", { client: `198.51.100.${index % 250}` });
    for (let index = 0; index < 100; index += 1)
      await node.call("/submit", { method: "POST", client: "192.0.2.1" });
    expect((await node.call("/tx", { client: "192.0.2.1" })).status).toBe(429);
    expect(
      (await node.call("/submit", { method: "POST", client: "192.0.2.1" }))
        .status,
    ).toBe(429);
    expect((await node.call("/healthz", { client: "192.0.2.1" })).status).toBe(
      200,
    );
    expect((await node.call("/readyz", { client: "192.0.2.1" })).status).toBe(
      200,
    );
    expect(
      (await node.call("/admin/session", { client: "192.0.2.1" })).status,
    ).toBe(200);
    expect(
      (
        await node.call("/commit", {
          method: "POST",
          client: "192.0.2.1",
          headers: { "content-type": "text/plain" },
        })
      ).status,
    ).toBe(200);
    // A flooding client's own health checks still answer.
    expect(
      (await node.call("/healthz", { client: "198.51.100.1" })).status,
    ).toBe(200);
  });

  it("applies no rate refusals when a testing profile disables the limiter", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({}, false),
    });
    const results = await node.statuses(100, "/tx/evaluate", {
      method: "POST",
      client: "198.51.100.1",
    });
    expect(results.every((status) => status === 200)).toBe(true);
  });
});

describe("HTTP rate-limit configuration", () => {
  const load = (entries: Record<string, string>) =>
    Effect.runSync(
      Effect.withConfigProvider(
        httpIngressConfig,
        ConfigProvider.fromMap(new Map(Object.entries(entries))),
      ),
    );

  it("defaults to the enabled conservative policy", () => {
    expect(load({}).HTTP_RATE_LIMITS).toEqual(DEFAULT_HTTP_RATE_LIMITS);
  });

  it("reads per-class overrides and the testing switch", () => {
    const policy = load({
      HTTP_RATE_LIMIT_ENABLED: "false",
      HTTP_RATE_LIMIT_EVALUATE: "1,0.1,4,0.5",
      HTTP_RATE_LIMIT_HEALTH: "50,25",
    }).HTTP_RATE_LIMITS;
    expect(policy.enabled).toBe(false);
    expect(policy.classes.evaluate).toEqual({
      peer: { burst: 1, perSecond: 0.1 },
      global: { burst: 4, perSecond: 0.5 },
    });
    expect(policy.classes.health).toEqual({
      peer: { burst: 50, perSecond: 25 },
    });
    expect(policy.classes.read).toEqual(DEFAULT_HTTP_RATE_LIMITS.classes.read);
  });

  it("reads a submission override as client and prefix rates", () => {
    expect(
      load({ HTTP_RATE_LIMIT_WRITE: "50,10,500,100" }).HTTP_RATE_LIMITS.classes
        .write,
    ).toEqual({
      peer: { burst: 50, perSecond: 10 },
      prefix: { burst: 500, perSecond: 100 },
    });
  });

  it.each([
    ["HTTP_RATE_LIMIT_ENABLED", "maybe"],
    ["HTTP_RATE_LIMIT_WRITE", "100,20"],
    ["HTTP_RATE_LIMIT_WRITE", "100,20,1000,200,16,4"],
    ["HTTP_RATE_LIMIT_READ", "30,10"],
    ["HTTP_RATE_LIMIT_READ", "0,10,200,100"],
    ["HTTP_RATE_LIMIT_WRITE", "4,1,16,-4"],
    ["HTTP_RATE_LIMIT_BATCH", "4,1,16,x"],
    ["HTTP_RATE_LIMIT_ADMIN", "30,10,100,10"],
    ["HTTP_RATE_LIMIT_WRITE", "4,0,16,4"],
    ["HTTP_RATE_LIMIT_WRITE", "4,1,16,0"],
    ["HTTP_RATE_LIMIT_HEALTH", "20,0"],
    ["HTTP_RATE_LIMIT_READ", "30,.5,200,100"],
    ["HTTP_RATE_LIMIT_READ", "30,10,1e3,100"],
  ])("refuses %s=%s", (name, value) => {
    expect(() => load({ [name]: value })).toThrow();
  });

  it("reads trusted proxy addresses and IPv4 or IPv6 ranges, and the HTTP socket path", () => {
    expect(load({})).toMatchObject({
      HTTP_TRUSTED_PROXY_PEERS: [],
      HTTP_SOCKET_PATH: undefined,
    });
    expect(
      load({
        HTTP_TRUSTED_PROXY_PEERS:
          "10.91.7.2, 35.191.0.0/16,2600:2D00:1:B029::/64 ,2a06:98c0::/29",
      }).HTTP_TRUSTED_PROXY_PEERS,
    ).toEqual([
      "10.91.7.2",
      "35.191.0.0/16",
      "2600:2d00:1:b029::/64",
      "2a06:98c0::/29",
    ]);
    expect(
      load({ HTTP_SOCKET_PATH: "/run/midgard-http/node.sock" }),
    ).toMatchObject({
      HTTP_TRUSTED_PROXY_PEERS: [],
      HTTP_SOCKET_PATH: "/run/midgard-http/node.sock",
    });
  });

  it("refuses trusted proxies on a socket, where the ingress names each client", () => {
    expect(() =>
      load({
        HTTP_TRUSTED_PROXY_PEERS: "10.91.7.2",
        HTTP_SOCKET_PATH: "/run/midgard-http/node.sock",
      }),
    ).toThrow("HTTP_TRUSTED_PROXY_PEERS applies only to a TCP listener");
  });

  it.each([
    ["HTTP_TRUSTED_PROXY_PEERS", "0.0.0.0/0"],
    ["HTTP_TRUSTED_PROXY_PEERS", "10.0.0.0/7"],
    ["HTTP_TRUSTED_PROXY_PEERS", "::/0"],
    ["HTTP_TRUSTED_PROXY_PEERS", "2001:db8::/23"],
    ["HTTP_TRUSTED_PROXY_PEERS", "10.0.0.1/8"],
    ["HTTP_TRUSTED_PROXY_PEERS", "10.0.0.0/33"],
    ["HTTP_TRUSTED_PROXY_PEERS", "10.0.0.0/+8"],
    ["HTTP_TRUSTED_PROXY_PEERS", "::ffff:10.0.0.1"],
    ["HTTP_TRUSTED_PROXY_PEERS", "fe80::1%eth0"],
    ["HTTP_TRUSTED_PROXY_PEERS", "proxy.example"],
    ["HTTP_TRUSTED_PROXY_PEERS", "10.0.0.2,10.0.0.2"],
    [
      "HTTP_TRUSTED_PROXY_PEERS",
      Array.from({ length: 65 }, (_, index) => `10.0.${index}.0/24`).join(","),
    ],
    ["HTTP_SOCKET_PATH", "node.sock"],
    ["HTTP_SOCKET_PATH", "/run/midgard-http/"],
    ["HTTP_SOCKET_PATH", `/run/${"a".repeat(100)}.sock`],
  ])("refuses %s=%s", (name, value) => {
    expect(() => load({ [name]: value })).toThrow();
  });

  it("accepts global rates of any size, since no client table can fill", () => {
    expect(
      load({ HTTP_RATE_LIMIT_READ: "30,10,20000,10000" }).HTTP_RATE_LIMITS
        .classes.read.global,
    ).toEqual({ burst: 20_000, perSecond: 10_000 });
  });
});
