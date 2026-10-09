/**
 * What the committee process does on a failure (owner ruling 2026-10-09):
 * a startup failure no restart is known to repair holds the process up,
 * unready with `committee_startup_failed`, its `/healthz` live; transient
 * failures that outlasted the startup budget exit non-zero. After startup,
 * a transient failure that outlived its bound (the store's instance lock,
 * the L1 follower's store) logs one named line, shuts down and exits
 * non-zero, once.
 */
import type { AddressInfo } from "node:net";

import { describe, expect, it } from "vitest";

import {
  COMMITTEE_STARTUP_FAILED,
  CommitteeStartupFailedError,
  listenStartingServer,
  startOrHold,
  startupFailureOutcome,
} from "../src/startup.js";
import {
  COMMITTEE_TRANSIENT_BUDGET_EXHAUSTED,
  transientExhaustionExit,
} from "../src/transient-exhaustion.js";

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

const exhaustedStartup = () =>
  new CommitteeStartupFailedError(
    "committee_startup_dependency_unavailable",
    9,
    new Error("connect ECONNREFUSED"),
  );
const fatalStartup = () =>
  new CommitteeStartupFailedError(
    "committee_startup_failed",
    1,
    new Error("stored L1 origin differs from L1_ORIGIN"),
  );

describe("committee startup failure outcome", () => {
  it("exits only on transient failures that outlasted the startup budget", () => {
    expect(startupFailureOutcome(exhaustedStartup())).toBe("exit");
    expect(startupFailureOutcome(fatalStartup())).toBe("hold");
    // A setup failure (configuration, key material) and anything unknown.
    expect(startupFailureOutcome(new Error("bad key file"))).toBe("hold");
    expect(startupFailureOutcome("unknown")).toBe("hold");
  });

  it("holds a deterministic startup failure up and unready, its health live", async () => {
    const starting = await listenStartingServer(0, "127.0.0.1");
    const lines: string[] = [];
    try {
      const listening = port(starting.address());
      const started = await startOrHold(
        starting,
        (line) => lines.push(line),
        () => Promise.reject(fatalStartup()),
      );
      expect(started).toBeUndefined();
      expect(await get(listening, "/readyz")).toEqual({
        status: 503,
        body: {
          ready: false,
          reasons: [COMMITTEE_STARTUP_FAILED],
          detail: fatalStartup().message,
        },
      });
      expect(await get(listening, "/healthz")).toEqual({
        status: 200,
        body: { ok: true, status: "held" },
      });
      expect(lines.map((line) => JSON.parse(line) as unknown)).toEqual([
        {
          event: "committee_startup_held",
          reason: COMMITTEE_STARTUP_FAILED,
          detail: fatalStartup().message,
        },
      ]);
    } finally {
      await starting.close();
    }
  });

  it("exits on an exhausted startup budget, closing the starting server", async () => {
    const starting = await listenStartingServer(0, "127.0.0.1");
    const listening = port(starting.address());
    const failure = exhaustedStartup();
    await expect(
      startOrHold(
        starting,
        () => undefined,
        () => Promise.reject(failure),
      ),
    ).rejects.toBe(failure);
    await expect(get(listening, "/healthz")).rejects.toThrow();
  });

  it("exits a one-shot run on any startup failure", async () => {
    const failure = fatalStartup();
    await expect(
      startOrHold(
        undefined,
        () => undefined,
        () => Promise.reject(failure),
      ),
    ).rejects.toBe(failure);
  });

  it("returns the started runtime when the startup succeeds", async () => {
    expect(
      await startOrHold(
        undefined,
        () => undefined,
        () => Promise.resolve(7),
      ),
    ).toBe(7);
  });
});

describe("committee transient exhaustion after startup", () => {
  it("logs one named line, shuts down and exits non-zero, once", async () => {
    const lines: string[] = [];
    const codes: number[] = [];
    let shutdowns = 0;
    const exited = new Promise<void>((resolve) => {
      const report = transientExhaustionExit({
        write: (line) => lines.push(line),
        shutdown: () => async () => {
          shutdowns += 1;
        },
        exit: (code) => {
          codes.push(code);
          resolve();
        },
      });
      report({
        source: "store_instance_lock",
        reason: "store_instance_lock_failed",
        detail: "Postgres unreachable past the reacquire budget",
      });
      report({
        source: "l1_follower",
        reason: "l1_follower_transient_exhausted",
        detail: "second",
      });
    });
    await exited;
    expect(codes).toEqual([1]);
    expect(shutdowns).toBe(1);
    expect(lines.map((line) => JSON.parse(line) as unknown)).toEqual([
      {
        event: COMMITTEE_TRANSIENT_BUDGET_EXHAUSTED,
        source: "store_instance_lock",
        reason: "store_instance_lock_failed",
        detail: "Postgres unreachable past the reacquire budget",
        outcome:
          "a transient failure outlived its bound; the process exits non-zero",
      },
    ]);
  });

  it("exits on its deadline when the shutdown does not settle", async () => {
    const codes: number[] = [];
    await new Promise<void>((resolve) => {
      transientExhaustionExit({
        write: () => undefined,
        shutdown: () => () => new Promise<void>(() => undefined),
        exit: (code) => {
          codes.push(code);
          resolve();
        },
        deadlineMs: 10,
      })({ source: "l1_follower", reason: "r", detail: "d" });
    });
    expect(codes).toEqual([1]);
  });
});
