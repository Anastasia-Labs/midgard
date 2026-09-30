import "./trusted-head-authority.independent-monotonic-watcher-trusted-head-authority.js";

import { createServer, type RequestListener } from "node:http";

import { describe, expect, it } from "vitest";

import { createWatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import {
  authenticationKey,
  head,
  hex32,
  policy,
} from "./trusted-head-authority.policy.js";

describe("trusted-head idempotent read transport recovery", () => {
  const withServer = async (
    listener: RequestListener,
    run: (endpoint: string) => Promise<void>,
  ) => {
    const server = createServer(listener);
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const address = server.address();
    if (address === null || typeof address === "string")
      throw new Error("test authority requires TCP");
    try {
      await run(`http://127.0.0.1:${address.port}`);
    } finally {
      server.closeAllConnections();
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
    }
  };
  const client = (endpoint: string, requestTimeoutMs = 2_000) =>
    createWatcherTrustedHeadAuthorityClient({
      endpoint,
      httpSecret: "authority-http-secret-with-sufficient-entropy",
      policy: policy(),
      authenticationKey,
      requestTimeoutMs,
    });

  it("retries a closed socket for each GET and still authenticates the recovered head", async () => {
    const requests = new Map<string, number>();
    const finality = policy();
    const expected = head(finality, 0, "10");
    await withServer(
      (request, response) => {
        const path = request.url!;
        const count = (requests.get(path) ?? 0) + 1;
        requests.set(path, count);
        if (count === 1) {
          request.socket.destroy();
          return;
        }
        response.setHeader("content-type", "application/json");
        response.end(
          JSON.stringify(
            path === "/v1/identity"
              ? { recordAuthenticationKeyId: hex32("12") }
              : { head: expected },
          ),
        );
      },
      async (endpoint) => {
        const authority = client(endpoint);
        expect(await authority.readRecordAuthenticationKeyId()).toBe(
          hex32("12"),
        );
        expect(await authority.readCurrent()).toEqual(expected);
        expect([...requests.values()]).toEqual([2, 2]);
      },
    );
  });

  it("retries transport loss while reading a successful GET response body", async () => {
    let requests = 0;
    await withServer(
      (request, response) => {
        requests += 1;
        if (requests === 1) {
          response.writeHead(200, {
            "content-type": "application/json",
            "content-length": "100",
          });
          response.write('{"head":');
          setImmediate(() => request.socket.destroy());
          return;
        }
        response.end(JSON.stringify({ head: null }));
      },
      async (endpoint) => {
        expect(await client(endpoint).readCurrent()).toBeNull();
        expect(requests).toBe(2);
      },
    );
  });

  it("bounds persistent socket loss to three attempts under the original read deadline", async () => {
    let requests = 0;
    await withServer(
      (request) => {
        requests += 1;
        request.socket.destroy();
      },
      async (endpoint) => {
        await expect(client(endpoint).readCurrent()).rejects.toThrow(
          "fetch failed",
        );
        expect(requests).toBe(3);
        requests = 0;
        const started = performance.now();
        await expect(client(endpoint, 60).readCurrent()).rejects.toThrow();
        expect(requests).toBe(1);
        expect(performance.now() - started).toBeLessThan(500);
      },
    );
  });

  it.each<[number, string]>([
    [401, JSON.stringify({ error: "unauthorized" })],
    [500, JSON.stringify({ error: "persistence_failure" })],
    [200, "not json"],
    [200, JSON.stringify({ head: null, extra: true })],
    [
      200,
      JSON.stringify({
        head: { ...head(policy(), 0, "10"), headMac: hex32("00") },
      }),
    ],
  ])(
    "does not retry HTTP or invalid authority responses (%s, %s)",
    async (status, body) => {
      let requests = 0;
      await withServer(
        (_request, response) => {
          requests += 1;
          response.writeHead(status, { "content-type": "application/json" });
          response.end(body);
        },
        async (endpoint) => {
          await expect(client(endpoint).readCurrent()).rejects.toThrow();
          expect(requests).toBe(1);
        },
      );
    },
  );

  it("preserves the existing 409 response validation without retrying it", async () => {
    let requests = 0;
    await withServer(
      (_request, response) => {
        requests += 1;
        response.writeHead(409, { "content-type": "application/json" });
        response.end(JSON.stringify({ head: null }));
      },
      async (endpoint) => {
        expect(await client(endpoint).readCurrent()).toBeNull();
        expect(requests).toBe(1);
      },
    );
  });

  it("never retries an ambiguous CAS whose response connection was lost", async () => {
    let writes = 0;
    await withServer(
      (request) => {
        if (request.method === "POST") writes += 1;
        request.socket.destroy();
      },
      async (endpoint) => {
        await expect(
          client(endpoint).compareAndSwap({
            expectedTrustedHead: null,
            nextTrustedHead: head(policy(), 0, "10"),
          }),
        ).rejects.toThrow("fetch failed");
        expect(writes).toBe(1);
      },
    );
  });
});
