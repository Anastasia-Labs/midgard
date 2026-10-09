/**
 * The follow loop's failure classes (owner ruling 2026-10-09: retry only
 * what is transient): how `classifyFailure` and `classifyStreamFailure` sort
 * a failure, and that the loop reopens a stream or restarts the store on a
 * transient failure however often it repeats, and stops under
 * `l1_follower_apply_stuck` once one that is not transient repeats
 * `stuckAfter` times in a row. Transient store failures are bounded
 * (orchestrator ruling R2): past `transientBudgetMs` with no event settled
 * the loop stops `exhausted`; stream drops and a held writer lease are not.
 */
import {
  type ChainSyncEvent,
  StreamInterruptedError,
  TransportFailedError,
  TransportProtocolError,
  TransportRequestError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";
import { describe, expect, it } from "vitest";

import { stuckRecorder } from "../src/follow/stuck.js";
import {
  classifyFailure,
  classifyStreamFailure,
  type FactStore,
  FOLLOWER_APPLY_STUCK,
  FOLLOWER_TRANSIENT_EXHAUSTED,
  type FollowStatus,
  openSqliteFactStore,
} from "../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  simStoreOptions,
} from "../src/testing/index.js";
import { appliedAll, follow, script } from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";

const corpus = forkCorpus(SIM_K);
const short: readonly ChainSyncEvent[] = buildForkSteps(corpus[0]!.scenario, [
  FIXTURE_PROJECTION,
]).steps.map((step) => step.event);

const openStore = (): FactStore =>
  openSqliteFactStore({
    ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
    path: ":memory:",
  });

const reasons = (status: FollowStatus): string[] =>
  status.readiness.map((r) => r.reason);

const coded = (message: string, code: string): Error =>
  Object.assign(new Error(message), { code });

describe("classifyFailure", () => {
  const coded = (message: string, code: string) =>
    Object.assign(new Error(message), { code });

  it.each([
    ["pg's connect timeout", new Error("timeout expired"), "transient"],
    ["a reset socket", coded("read ECONNRESET", "ECONNRESET"), "transient"],
    [
      "a connection-class SQLSTATE",
      coded("admin shutdown", "57P01"),
      "transient",
    ],
    [
      "a constraint violation",
      coded("duplicate key", "23505"),
      "deterministic",
    ],
    [
      "a missing database",
      coded("database does not exist", "3D000"),
      "unknown",
    ],
    ["an unrecognised error", new Error("boom"), "unknown"],
    [
      "a message that only mentions a timeout",
      new Error("statement timeout expired soon"),
      "unknown",
    ],
    ["a non-Error value", "timeout expired", "unknown"],
  ] as const)("classifies %s", (_name, error, expected) => {
    expect(classifyFailure(error)).toBe(expected);
  });
});

describe("classifyStreamFailure", () => {
  it.each([
    [
      "the transport unavailable",
      new TransportUnavailableError("sidecar_restarting", "restarting"),
      "transient",
    ],
    [
      "an interrupted stream",
      new StreamInterruptedError("node_connection_lost"),
      "transient",
    ],
    [
      "a reopen refused while the node is away",
      new TransportRequestError("node_unavailable", "down"),
      "transient",
    ],
    [
      "a reset socket",
      Object.assign(new Error("read ECONNRESET"), { code: "ECONNRESET" }),
      "transient",
    ],
    [
      "an undecodable block",
      new TransportRequestError("block_header_undecodable", "bad header"),
      "unknown",
    ],
    [
      "a protocol error in the sidecar's frames",
      new TransportProtocolError("sequence 3 does not follow 1"),
      "unknown",
    ],
    ["an unrecognised error", new Error("boom"), "unknown"],
    [
      "a transport that failed on a refused handshake",
      new TransportFailedError("node_handshake_failed", "network magic"),
      "deterministic",
    ],
  ] as const)("classifies %s", (_name, error, expected) => {
    expect(classifyStreamFailure(error)).toBe(expected);
  });
});

describe("followChain: stream failures by class", () => {
  const failingStream = async (failWith: () => Error) => {
    const store = openStore();
    try {
      const s = script(short, { failAt: 4, failTimes: 6, failWith });
      const run = await follow({
        store,
        script: s,
        stuckAfter: 3,
        until: appliedAll(s),
      });
      return { ...run, opens: s.opens };
    } finally {
      await store.close();
    }
  };

  it("reopens a stream that fails transiently, however often, and catches up", async () => {
    const run = await failingStream(
      () => new TransportUnavailableError("sidecar_restarting", "restarting"),
    );
    expect(run.final.stuck).toBeNull();
    expect(run.opens).toBe(7);
    expect(run.final.events).toBe(short.length);
  });

  it("stops under l1_follower_apply_stuck once a stream failure that is not transient repeats N times", async () => {
    const run = await failingStream(
      () => new TransportRequestError("block_header_undecodable", "bad header"),
    );
    expect(run.opens).toBe(3);
    expect(run.final.state).toBe("intervention");
    expect(run.final.stuck).toMatchObject({ at: "stream", failures: 3 });
    expect(reasons(run.final)).toContain(FOLLOWER_APPLY_STUCK);
  });
});

describe("followChain: store failures by class", () => {
  it("stops once an unknown store failure with no point repeats N times in a row, and waits out a transient one", async () => {
    for (const [error, stops] of [
      [() => new Error("boom"), true],
      [() => coded("read ECONNRESET", "ECONNRESET"), false],
    ] as const) {
      const store = openStore();
      let failures = 6;
      let starts = 0;
      const proxy: FactStore = {
        ...store,
        start: async () => {
          starts += 1;
          if (failures-- > 0) throw error();
          return store.start();
        },
      };
      try {
        const s = script(short);
        const { final } = await follow({
          store: proxy,
          script: s,
          stuckAfter: 3,
          until: appliedAll(s),
        });
        if (stops) {
          expect(starts).toBe(3);
          expect(final.state).toBe("intervention");
          expect(final.stuck).toMatchObject({ at: "store", failures: 3 });
          expect(reasons(final)).toContain(FOLLOWER_APPLY_STUCK);
        } else {
          expect(starts).toBe(7);
          expect(final.stuck).toBeNull();
          expect(final.events).toBe(short.length);
        }
      } finally {
        await store.close();
      }
    }
  });
});

/** A clock one minute further on at every read. */
const minuteClock = () => {
  let at = 0;
  return () => (at += 60_000);
};

describe("followChain: the transient store budget", () => {
  it("stops exhausted once transient store failures outlast the budget with no event settled", async () => {
    const store = openStore();
    let starts = 0;
    const proxy: FactStore = {
      ...store,
      start: () => {
        starts += 1;
        return Promise.reject(coded("read ECONNRESET", "ECONNRESET"));
      },
    };
    try {
      const s = script(short);
      const { final, log } = await follow({
        store: proxy,
        script: s,
        transientBudgetMs: 5 * 60_000,
        now: minuteClock(),
        until: appliedAll(s),
        timeoutMs: 3_000,
      });
      // Failing for 0, 1, ... 5 minutes: the sixth start reaches the budget.
      expect(starts).toBe(6);
      expect(final.state).toBe("exhausted");
      expect(final.stuck).toBeNull();
      expect(reasons(final)).toContain(FOLLOWER_TRANSIENT_EXHAUSTED);
      expect(log.join("\n")).toContain(
        "follower exhausted its transient budget",
      );
    } finally {
      await store.close();
    }
  });

  it("reopens a transiently failing stream past the budget: waiting on the L1 node has no bound", async () => {
    const store = openStore();
    try {
      const s = script(short, {
        failAt: 4,
        failTimes: 6,
        failWith: () =>
          new TransportUnavailableError("node_unreachable", "down"),
      });
      const { final } = await follow({
        store,
        script: s,
        transientBudgetMs: 1,
        now: minuteClock(),
        until: appliedAll(s),
      });
      expect(s.opens).toBe(7);
      expect(final.state).not.toBe("exhausted");
      expect(final.events).toBe(short.length);
    } finally {
      await store.close();
    }
  });
});

describe("stuckRecorder: the transient budget", () => {
  const recorder = (budgetMs: number) => {
    let at = 0;
    const published: string[] = [];
    const recorded = stuckRecorder({
      stuckAfter: 3,
      transientBudgetMs: budgetMs,
      now: () => at,
      log: () => undefined,
      publish: (change) => {
        if (change.state !== undefined) published.push(change.state);
        return Promise.resolve();
      },
    });
    return {
      ...recorded,
      published,
      advance: (ms: number) => (at += ms),
    };
  };

  it("stops on the store failure that reaches the budget, not before", async () => {
    const r = recorder(10_000);
    expect(await r.failed("store", "down", "transient")).toBe(false);
    r.advance(9_999);
    expect(await r.failed("apply", "down", "transient")).toBe(false);
    r.advance(1);
    expect(await r.failed("store", "down", "transient")).toBe(true);
    expect(r.published.at(-1)).toBe("exhausted");
  });

  it("restarts the clock once an event settles", async () => {
    const r = recorder(10_000);
    expect(await r.failed("store", "down", "transient")).toBe(false);
    r.advance(9_000);
    r.settled();
    expect(await r.failed("store", "down", "transient")).toBe(false);
    r.advance(9_000);
    expect(await r.failed("store", "down", "transient")).toBe(false);
  });

  it("never stops on a stream failure or a held writer lease", async () => {
    const r = recorder(10_000);
    for (const cause of ["stream", "store_locked"] as const) {
      expect(await r.failed(cause, "away", "transient")).toBe(false);
      r.advance(60_000);
      expect(await r.failed(cause, "away", "transient")).toBe(false);
    }
    expect(r.published).not.toContain("exhausted");
  });
});
