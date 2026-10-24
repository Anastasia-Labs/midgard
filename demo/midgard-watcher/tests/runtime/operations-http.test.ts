import { describe, expect, it } from "vitest";

import { startWatcherOperationsHttpServer } from "../../src/runtime/operations-http.js";
import {
  createWatcherOperationsObservability,
  type WatcherOperationsObservability,
} from "../../src/runtime/operations-observability.js";
import { supervisor } from "./operations-observability.supervisor.js";

describe("production operations HTTP V1", () => {
  it("serves one live readiness snapshot as held503 or ready200 without changing status health", async () => {
    const proofSupervisor = supervisor();
    proofSupervisor.setStatus({ phase: "blocked", recovered: false });
    let statusReads = 0;
    let monotonic = 100_000;
    const observability = createWatcherOperationsObservability({
      deploymentFingerprint: "11".repeat(32),
      supervisor: proofSupervisor.runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 54,
        requiredCategoryCount: 54,
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 1,
        oldestQueuedAtMs: "99000",
      }),
      nowMs: () => {
        statusReads += 1;
        return 100_000n;
      },
      monotonicNowMs: () => monotonic,
      l1FreshnessMaximumAgeMs: 10_000,
    });
    observability.sink.recordL1Source({
      sourceIdentityDigest: "22".repeat(32),
      sourceMode: "local_node",
      status: "consistent",
      blockHash: "33".repeat(32),
      blockNo: "50",
      slot: "500",
      observedAtMs: "99000",
    });
    const server = await startWatcherOperationsHttpServer({
      endpoint: "http://127.0.0.1:0",
      observability,
      unsafeAllowEphemeralPortForTest: true,
    });
    const readiness = async (status: number, reasons: readonly string[]) => {
      statusReads = 0;
      const response = await fetch(`${server.endpoint}/readyz`);
      expect(response.status).toBe(status);
      await expect(response.json()).resolves.toEqual({
        ready: status === 200,
        reasons,
        l1: [],
      });
      expect(response.headers.get("cache-control")).toBe("no-store");
      expect(statusReads).toBe(1);
    };
    try {
      await readiness(503, ["supervisor_not_accepting", "recovery_incomplete"]);
      const health = await fetch(`${server.endpoint}/v1/status`);
      expect(health.status).toBe(200);
      await expect(health.json()).resolves.toMatchObject({
        readiness: "not_ready",
        readinessReasons: ["supervisor_not_accepting", "recovery_incomplete"],
      });
      proofSupervisor.setStatus({ phase: "accepting", recovered: true });
      await readiness(200, []);
      monotonic += 10_000;
      await readiness(503, ["l1_source_stale"]);
      statusReads = 0;
      expect((await fetch(`${server.endpoint}/readyz?probe=true`)).status).toBe(
        404,
      );
      const method = await fetch(`${server.endpoint}/readyz`, {
        method: "POST",
      });
      expect(method.status).toBe(405);
      expect(method.headers.get("allow")).toBe("GET");
      expect(statusReads).toBe(0);
    } finally {
      await server.close();
    }
    await expect(server.done).resolves.toBeUndefined();
  });
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
