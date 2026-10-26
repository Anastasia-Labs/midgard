import { ConfigProvider, Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { HTTP_PEER_MAX_ENTRIES } from "../src/commands/listen-http-ingress.js";
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
    // Global refusals do not allocate a client entry.
    expect(node.ingress.snapshot().peers).toBe(5);
  });

  it("holds evaluation, submission and batch status to stricter default rates than single reads", async () => {
    const node = await serve(proxied);
    const accepted = async (path: string, method: string, client: string) =>
      (await node.statuses(40, path, { method, client })).filter(
        (status) => status === 200,
      ).length;
    const reads = await accepted("/tx-status", "GET", "198.51.100.1");
    const batches = await accepted("/tx-status", "POST", "198.51.100.2");
    const submits = await accepted("/submit", "POST", "198.51.100.3");
    const evaluations = await accepted("/tx/evaluate", "POST", "198.51.100.4");
    expect(reads).toBe(30);
    expect(batches).toBe(4);
    expect(submits).toBe(4);
    expect(evaluations).toBe(2);
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

  it("groups IPv6 clients by /64", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        read: {
          peer: { burst: 2, perSecond: 1 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      }),
    });
    expect((await node.call("/tx", { client: "2001:db8:1:2::1" })).status).toBe(
      200,
    );
    expect(
      (await node.call("/tx", { client: "2001:db8:1:2:ffff:0:0:7" })).status,
    ).toBe(200);
    expect(
      (await node.call("/tx", { client: "2001:0db8:0001:0002::abcd" })).status,
    ).toBe(429);
    expect((await node.call("/tx", { client: "2001:db8:1:3::1" })).status).toBe(
      200,
    );
  });

  it("keeps the client table bounded under 10,000 distinct addresses and frees idle entries", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        read: {
          peer: { burst: 1, perSecond: 1 },
          global: { burst: 1_000_000, perSecond: 1_000_000 },
        },
      }),
    });
    let refused = 0;
    for (let index = 0; index < 10_000; index += 1) {
      node.advance(1);
      const client = `10.${(index >> 16) & 255}.${(index >> 8) & 255}.${index & 255}`;
      if ((await node.call("/tx", { client })).status === 429) refused += 1;
      expect(node.ingress.snapshot().peers).toBeLessThanOrEqual(
        HTTP_PEER_MAX_ENTRIES,
      );
    }
    expect(refused).toBe(10_000 - HTTP_PEER_MAX_ENTRIES);
    // Health and admin clients keep their own table.
    expect((await node.call("/healthz", { client: "192.0.2.1" })).status).toBe(
      200,
    );
    expect(
      (await node.call("/admin/session", { client: "192.0.2.1" })).status,
    ).toBe(200);
    node.advance(60_000);
    expect((await node.call("/tx", { client: "198.51.100.1" })).status).toBe(
      200,
    );
    // 10,000 addresses inside one /64 occupy one entry.
    const before = node.ingress.snapshot().peers;
    for (let index = 0; index < 10_000; index += 1)
      await node.call("/tx", { client: `2001:db8::${index.toString(16)}` });
    expect(node.ingress.snapshot().peers).toBe(before + 1);
  }, 120_000);

  it("answers health, readiness and admin routes while public classes are flooded", async () => {
    const node = await serve(proxied);
    for (let index = 0; index < 400; index += 1)
      await node.call("/tx", { client: `198.51.100.${index % 250}` });
    for (let index = 0; index < 40; index += 1)
      await node.call("/submit", {
        method: "POST",
        client: `203.0.113.${index}`,
      });
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

  it.each([
    ["HTTP_RATE_LIMIT_ENABLED", "maybe"],
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

  it("refuses global rates that could fill the client table within one idle window", () => {
    expect(() => load({ HTTP_RATE_LIMIT_READ: "30,10,2000,1000" })).toThrow(
      /8192/u,
    );
  });

  it("accepts any global rates when the limiter is disabled", () => {
    expect(
      load({
        HTTP_RATE_LIMIT_ENABLED: "false",
        HTTP_RATE_LIMIT_READ: "30,10,2000,1000",
      }).HTTP_RATE_LIMITS.classes.read.global,
    ).toEqual({ burst: 2_000, perSecond: 1_000 });
  });
});
