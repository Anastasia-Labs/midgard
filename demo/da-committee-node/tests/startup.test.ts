import type { AddressInfo } from "node:net";

import { describe, expect, it } from "vitest";

import { createCommitteeApiServer } from "../src/api/server.js";
import type { CommitteeReadinessSnapshot } from "../src/committee-service.js";
import {
  classifyStartupFailure,
  CommitteeStartupFailedError,
  listenStartingServer,
  retryStartup,
} from "../src/startup.js";
import { isInstanceLockHeldElsewhere } from "../src/store/postgres.instance-lock.js";

const coded = (message: string, code: string, syscall?: string) =>
  Object.assign(new Error(message), {
    code,
    ...(syscall === undefined ? {} : { syscall }),
  });

/** Ends a startup that retries past any budget a test sets. */
const RETRY_GUARD = 1_000;

/** The rejection `run` ends with. */
const failureOf = async (run: Promise<unknown>) => {
  try {
    await run;
  } catch (error) {
    return error;
  }
  throw new Error("expected the startup to fail");
};

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
          throw coded(
            "connect ENOENT /run/cardano/node.socket",
            "ENOENT",
            "connect",
          );
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

  it("fails a transient failure that outlasts the budget under committee_startup_dependency_unavailable", async () => {
    let now = 0;
    let attempts = 0;
    const cause = coded("connect ECONNREFUSED 127.0.0.1:5432", "ECONNREFUSED");
    const error = await failureOf(
      retryStartup({
        attempt: () => {
          attempts += 1;
          return Promise.reject(cause);
        },
        onFailure: () => undefined,
        write: () => undefined,
        budgetMs: 60_000,
        now: () => now,
        sleep: async (ms) => {
          now += ms;
          if (attempts > RETRY_GUARD) throw new Error("retried without bound");
        },
        initialMs: 10_000,
        maxMs: 30_000,
      }),
    );
    // Failures at 0, 10, 30, 60 s: the fourth is past the budget.
    expect(attempts).toBe(4);
    expect(error).toBeInstanceOf(CommitteeStartupFailedError);
    expect(error).toMatchObject({
      reason: "committee_startup_dependency_unavailable",
      attempts: 4,
      cause,
    });
  });

  it("fails at once, with no retry, on a stale deployment, a point before L1_ORIGIN or any failure it does not recognise", async () => {
    for (const message of [
      "stale_deployment_state_requires_fresh_redeploy: stored_manifest_id=aa",
      "committee_store_point_before_l1_origin: the header record names slot 5, before L1_ORIGIN slot 9",
      "something nobody classified",
    ]) {
      const cause = new Error(message);
      let attempts = 0;
      const sleeps: number[] = [];
      const error = await failureOf(
        retryStartup({
          attempt: () => {
            attempts += 1;
            return Promise.reject(cause);
          },
          onFailure: () => undefined,
          write: () => undefined,
          sleep: async (ms) => {
            sleeps.push(ms);
            if (attempts > RETRY_GUARD)
              throw new Error("retried without bound");
          },
        }),
      );
      expect(attempts).toBe(1);
      expect(sleeps).toEqual([]);
      expect(error).toMatchObject({
        reason: "committee_startup_failed",
        attempts: 1,
        cause,
      });
      expect((error as Error).message).toContain(message);
    }
  });

  it("waits on another live holder of the store's lock without a deadline", async () => {
    let now = 0;
    let attempts = 0;
    const started = await retryStartup({
      attempt: () => {
        attempts += 1;
        return attempts <= 40
          ? Promise.reject(new Error("held"))
          : Promise.resolve("runtime");
      },
      classify: (error) =>
        isInstanceLockHeldElsewhere(error) ||
        (error as Error).message === "held"
          ? "waiting"
          : "fatal",
      onFailure: () => undefined,
      write: () => undefined,
      budgetMs: 60_000,
      now: () => now,
      sleep: async (ms) => {
        now += ms;
      },
    });
    expect(started).toBe("runtime");
    expect(now).toBeGreaterThan(60_000 * 10);
  });

  it("classifies dependency failures by their cause", () => {
    expect(
      classifyStartupFailure(
        new TypeError("fetch failed", {
          cause: coded("connect ECONNREFUSED", "ECONNREFUSED"),
        }),
      ),
    ).toBe("transient");
    expect(
      classifyStartupFailure(coded("Connection terminated", "57P01")),
    ).toBe("transient");
    expect(
      classifyStartupFailure(coded("no such file", "ENOENT", "open")),
    ).toBe("fatal");
    expect(
      classifyStartupFailure(coded("password authentication failed", "28P01")),
    ).toBe("fatal");
    expect(classifyStartupFailure("a string")).toBe("fatal");
  });

  it("fails any failure at once on a one-shot run", async () => {
    const cause = coded("connect ECONNREFUSED", "ECONNREFUSED");
    let attempts = 0;
    const error = await failureOf(
      retryStartup({
        attempt: () => {
          attempts += 1;
          return Promise.reject(cause);
        },
        onFailure: () => undefined,
        write: () => undefined,
        classify: () => "fatal",
        sleep: async () => {
          if (attempts > RETRY_GUARD) throw new Error("retried without bound");
        },
      }),
    );
    expect(error).toMatchObject({ reason: "committee_startup_failed", cause });
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
