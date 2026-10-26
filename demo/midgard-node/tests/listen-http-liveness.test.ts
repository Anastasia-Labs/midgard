import { afterEach, describe, expect, it } from "vitest";

import { HTTP_PEER_MAX_ENTRIES } from "../src/commands/listen-http-ingress.js";
import {
  closeServers,
  policyWith,
  proxied,
  serve,
} from "./listen-http-ingress.serve-fixture.js";

afterEach(closeServers);

const address = (prefix: number, index: number) =>
  `${prefix}.${(index >> 16) & 255}.${(index >> 8) & 255}.${index & 255}`;

/** Every liveness route, from a client that sent nothing before. */
const livenessStatuses = async (
  node: Awaited<ReturnType<typeof serve>>,
  client: string,
) => [
  (await node.call("/healthz", { client })).status,
  (await node.call("/readyz", { client })).status,
  (await node.call("/admin/session", { client })).status,
  (
    await node.call("/commit", {
      method: "POST",
      client,
      headers: { "content-type": "text/plain" },
    })
  ).status,
];

describe("HTTP liveness routes under public load", () => {
  it("answers health, readiness and admin routes while slow public requests hold every public slot", async () => {
    const node = await serve(proxied);
    for (let client = 1; client <= 16; client += 1)
      for (let slot = 0; slot < 4; slot += 1)
        node.hold("/utxos", { client: `198.51.100.${client}` });
    await node.waitFor(() => node.ingress.snapshot().active === 64);
    const refused = await node.call("/tx", { client: "192.0.2.1" });
    expect(refused.status).toBe(503);
    expect(refused.body.error_code).toBe("E_HTTP_INGRESS_CAPACITY");
    expect(await livenessStatuses(node, "192.0.2.1")).toEqual([
      200, 200, 200, 200,
    ]);
    // A client holding public slots still reaches its own health checks.
    expect(await livenessStatuses(node, "198.51.100.1")).toEqual([
      200, 200, 200, 200,
    ]);
  });

  it("bounds slow health and admin requests separately from public requests", async () => {
    const node = await serve(proxied);
    for (let client = 1; client <= 2; client += 1)
      for (let slot = 0; slot < 4; slot += 1)
        node.hold("/commit", { client: `198.51.100.${client}` });
    await node.waitFor(() => node.ingress.snapshot().livenessActive === 8);
    const refused = await node.call("/healthz", { client: "192.0.2.1" });
    expect(refused.status).toBe(503);
    expect(refused.body.error_code).toBe("E_HTTP_INGRESS_CAPACITY");
    expect((await node.call("/tx", { client: "192.0.2.1" })).status).toBe(200);
    node.releaseHeld();
    await node.waitFor(() => node.ingress.snapshot().livenessActive === 0);
    expect((await node.call("/healthz", { client: "192.0.2.1" })).status).toBe(
      200,
    );
  });

  it("answers new health and admin clients after an address spray fills their table", async () => {
    const node = await serve(proxied);
    for (let index = 0; index < HTTP_PEER_MAX_ENTRIES; index += 1) {
      node.advance(1);
      expect(
        (await node.call("/healthz", { client: address(10, index) })).status,
      ).toBe(200);
    }
    expect(node.ingress.snapshot().livenessPeers).toBe(HTTP_PEER_MAX_ENTRIES);
    expect(
      (await node.call("/admin/session", { client: "192.0.2.1" })).status,
    ).toBe(200);
    expect((await node.call("/healthz", { client: "192.0.2.2" })).status).toBe(
      200,
    );
    expect(node.ingress.snapshot().livenessPeers).toBe(HTTP_PEER_MAX_ENTRIES);
    // The public table is separate and stays empty.
    expect(node.ingress.snapshot().peers).toBe(0);
  }, 120_000);
});

describe("HTTP rate limiter disabled for testing", () => {
  it("never refuses for client-table capacity", async () => {
    const node = await serve({ ...proxied, policy: policyWith({}, false) });
    let refused = 0;
    for (let index = 0; index < HTTP_PEER_MAX_ENTRIES + 10; index += 1) {
      node.advance(1);
      if (
        (await node.call("/tx", { client: address(10, index) })).status !== 200
      )
        refused += 1;
    }
    expect(refused).toBe(0);
    expect(node.ingress.snapshot().peers).toBeLessThanOrEqual(
      HTTP_PEER_MAX_ENTRIES,
    );
  }, 120_000);
});

describe("HTTP Retry-After", () => {
  it("reports the longest wait of every refusing bucket", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        evaluate: {
          peer: { burst: 1, perSecond: 0.2 },
          global: { burst: 1, perSecond: 0.5 },
        },
      }),
    });
    const evaluate = (client: string) =>
      node.call("/tx/evaluate", { method: "POST", client });
    expect((await evaluate("198.51.100.1")).status).toBe(200);
    // Both buckets are empty: the global one refills in 2 s, the client's in 5 s.
    const both = await evaluate("198.51.100.1");
    expect([both.status, both.retryAfter]).toEqual([429, "5"]);
    // Only the global bucket refuses a fresh client.
    const global = await evaluate("198.51.100.2");
    expect([global.status, global.retryAfter]).toEqual([429, "2"]);
  });

  it("rounds a fractional wait up to whole seconds", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        read: {
          peer: { burst: 1, perSecond: 0.3 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      }),
    });
    expect((await node.call("/tx", { client: "198.51.100.1" })).status).toBe(
      200,
    );
    expect(
      (await node.call("/tx", { client: "198.51.100.1" })).retryAfter,
    ).toBe("4");
    node.advance(1_000);
    expect(
      (await node.call("/tx", { client: "198.51.100.1" })).retryAfter,
    ).toBe("3");
  });
});
