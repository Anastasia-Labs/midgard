import type { AddressInfo } from "node:net";

import { describe, expect, it } from "vitest";

import { createCommitteeApiServer } from "../src/api/server.js";
import type { CommitteeReadinessSnapshot } from "../src/committee-service.js";
import { listenStartingServer, retryStartup } from "../src/startup.js";

const port = (address: AddressInfo | string | null): number => {
  if (address === null || typeof address === "string")
    throw new Error("expected a TCP address");
  return address.port;
};

const get = async (listening: number, path: string) => {
  const response = await fetch(
    `http://127.0.0.1:${listening.toString()}${path}`,
  );
  return { status: response.status, body: (await response.json()) as unknown };
};

describe("committee node startup retry", () => {
  it("retries a dependency that is not up yet with a doubling, capped backoff and starts exactly once", async () => {
    const sleeps: number[] = [];
    const reasons: string[] = [];
    let attempts = 0;
    const started = await retryStartup({
      attempt: async () => {
        attempts += 1;
        if (attempts <= 5)
          throw new Error("connect ENOENT /run/cardano/node.socket");
        return "runtime";
      },
      onFailure: (reason) => reasons.push(reason),
      write: () => undefined,
      sleep: async (ms) => {
        sleeps.push(ms);
      },
      initialMs: 10,
      maxMs: 50,
    });

    expect(started).toBe("runtime");
    expect(attempts).toBe(6);
    expect(sleeps).toEqual([10, 20, 40, 50, 50]);
    expect(new Set(reasons)).toEqual(
      new Set(["starting:connect ENOENT /run/cardano/node.socket"]),
    );
  });

  it("holds unready under the named reason on a stale deployment or a point before L1_ORIGIN, retrying with capped backoff", async () => {
    const intervention = [
      "stale_deployment_state_requires_fresh_redeploy: stored_manifest_id=aa",
      "committee_store_point_before_l1_origin: the header record names slot 5, before L1_ORIGIN slot 9",
    ];
    for (const message of intervention) {
      const sleeps: number[] = [];
      const reasons: string[] = [];
      let attempts = 0;
      // The store is reset (or the node restarted on corrected
      // configuration) after four refusals: the same loop then starts.
      const started = await retryStartup({
        attempt: () => {
          attempts += 1;
          return attempts <= 4
            ? Promise.reject(new Error(message))
            : Promise.resolve("runtime");
        },
        onFailure: (reason) => reasons.push(reason),
        write: () => undefined,
        sleep: async (ms) => {
          sleeps.push(ms);
        },
        initialMs: 10,
        maxMs: 30,
      });
      expect(started).toBe("runtime");
      expect(attempts).toBe(5);
      expect(sleeps).toEqual([10, 20, 30, 30]);
      expect(new Set(reasons)).toEqual(new Set([`starting:${message}`]));
    }
  });

  it("rethrows any failure at once on a one-shot run", async () => {
    const error = new Error(
      "stale_deployment_state_requires_fresh_redeploy: stored_manifest_id=aa",
    );
    let attempts = 0;
    await expect(
      retryStartup({
        attempt: () => {
          attempts += 1;
          return Promise.reject(error);
        },
        onFailure: () => undefined,
        write: () => undefined,
        isFatal: () => true,
        sleep: async () => undefined,
      }),
    ).rejects.toBe(error);
    expect(attempts).toBe(1);
  });
});

describe("committee node API while starting", () => {
  it("answers health and reports the last startup failure as not ready", async () => {
    const starting = await listenStartingServer(0, "127.0.0.1");
    try {
      const listening = port(starting.address());
      expect(await get(listening, "/healthz")).toEqual({
        status: 200,
        body: { ok: true, status: "starting" },
      });
      starting.setReason("starting:store_instance_lock_held");
      expect(await get(listening, "/readyz")).toEqual({
        status: 503,
        body: { ready: false, reasons: ["starting:store_instance_lock_held"] },
      });
    } finally {
      await starting.close();
    }
  });

  it("frees the port for the committee API once closed", async () => {
    const starting = await listenStartingServer(0, "127.0.0.1");
    const listening = port(starting.address());
    await starting.close();
    const api = createCommitteeApiServer({
      deploymentFingerprint: "dep",
      store: undefined,
      readiness: () => ({ ready: true }) as CommitteeReadinessSnapshot,
    });
    await api.listen(listening, "127.0.0.1");
    try {
      expect((await get(listening, "/healthz")).status).toBe(200);
    } finally {
      await api.close();
    }
  });
});

describe("committee node process health", () => {
  it("fails /healthz only while the health callback reports the process wedged", async () => {
    let hung: number | undefined;
    const api = createCommitteeApiServer({
      deploymentFingerprint: "dep",
      store: undefined,
      readiness: () => ({ ready: false }) as CommitteeReadinessSnapshot,
      health: () =>
        hung === undefined
          ? { ok: true }
          : { ok: false, reason: `committee_tick_hung:${hung.toString()}` },
    });
    await api.listen(0, "127.0.0.1");
    try {
      const listening = port(api.address());
      expect(await get(listening, "/healthz")).toEqual({
        status: 200,
        body: { ok: true },
      });
      hung = 600_001;
      expect(await get(listening, "/healthz")).toEqual({
        status: 503,
        body: { ok: false, reason: "committee_tick_hung:600001" },
      });
      // Not ready is not unhealthy.
      expect((await get(listening, "/readyz")).status).toBe(503);
    } finally {
      await api.close();
    }
  });
});
