import { describe, expect, it } from "vitest";

import { startWatcherOperationsHttpServer } from "../../src/runtime/production-operations-http-v1.js";
import type { WatcherOperationsObservability } from "../../src/runtime/production-operations-observability-v1.js";

describe("production operations HTTP V1", () => {
  it("mounts only the bounded read-only loopback handler and closes cleanly", async () => {
    const requests: Request[] = [];
    const observability = Object.freeze({
      handleHttpRequest: async (request: Request) => {
        requests.push(request);
        return new Response('{"readiness":"ready"}', {
          status: 200,
          headers: {
            "cache-control": "no-store",
            "content-type": "application/json; charset=utf-8",
          },
        });
      },
    }) as unknown as WatcherOperationsObservability;
    const server = await startWatcherOperationsHttpServer({
      endpoint: "http://127.0.0.1:0",
      observability,
      unsafeAllowEphemeralPortForTest: true,
    });
    try {
      const response = await fetch(`${server.endpoint}/v1/status`);
      expect(response.status).toBe(200);
      await expect(response.json()).resolves.toEqual({ readiness: "ready" });
      expect(response.headers.get("cache-control")).toBe("no-store");
      expect(requests).toHaveLength(1);
      expect(requests[0]!.method).toBe("GET");
      expect(new URL(requests[0]!.url).pathname).toBe("/v1/status");

      const rejected = await fetch(`${server.endpoint}/v1/status`, {
        method: "POST",
        body: "not admitted",
      });
      expect(rejected.status).toBe(400);
      expect(requests).toHaveLength(1);
    } finally {
      await server.close();
    }
    await expect(server.done).resolves.toBeUndefined();
  });

  it("rejects non-loopback and production ephemeral endpoints", async () => {
    const observability = Object.freeze({
      handleHttpRequest: async () => new Response(null, { status: 204 }),
    }) as unknown as WatcherOperationsObservability;
    await expect(
      startWatcherOperationsHttpServer({
        endpoint: "http://0.0.0.0:3000",
        observability,
      }),
    ).rejects.toThrow("fixed loopback HTTP");
    await expect(
      startWatcherOperationsHttpServer({
        endpoint: "http://127.0.0.1:0",
        observability,
      }),
    ).rejects.toThrow("fixed loopback HTTP");
  });
});
