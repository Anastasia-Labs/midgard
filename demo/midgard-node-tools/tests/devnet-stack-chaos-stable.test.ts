import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  type ChaosDeps,
  type DrillRecord,
  runDrills,
} from "../src/devnet-stack/chaos.js";
import { awaitStable } from "../src/devnet-stack/chaos-stable.js";
import { SERVICE_MARKER_ENV } from "../src/devnet-stack/supervisor.js";

const dirs: string[] = [];
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const T0 = Date.parse("2026-09-30T00:00:00Z");
const SERVICES = ["node", "watcher"] as const;
const s = (seconds: number) => seconds * 1000;
const iso = (ms: number) => new Date(ms).toISOString();

type Down = { service: string; from: number; to: number; reasons: unknown };
type Event = Record<string, unknown> & { at: number };

/**
 * A supervised stack on a virtual clock. A service is unready while inside
 * one of its `down` intervals; supervisor events appear once the clock
 * passes their time. `waitReady` behaves like waitForServices: it returns
 * once every service is ready, or throws at its bound.
 */
const world = () => {
  let clock = T0;
  const downs: Down[] = [];
  const timeline: Event[] = [];
  const seen: Record<string, unknown>[] = [];
  const pids = new Map<string, number>(
    SERVICES.map((name, index) => [name, 1000 + index]),
  );
  /** PID changes the supervisor never recorded. */
  const repids: { service: string; at: number; pid: number }[] = [];
  const waits: { timeoutMs: number; honourStartGrace?: boolean }[] = [];
  const signals: { pid: number; at: number }[] = [];
  const onKill: ((at: number) => void)[] = [];
  const downAt = (service: string, at: number) =>
    downs.find((d) => d.service === service && d.from <= at && at < d.to);
  const flush = () => {
    timeline.sort((a, b) => a.at - b.at);
    while (timeline.length > 0 && timeline[0]!.at <= clock) {
      const { at, ...event } = timeline.shift()!;
      if (event.event === "start")
        pids.set(event.service as string, event.pid as number);
      seen.push({ at: iso(at), ...event });
    }
  };
  /** The service exits (code 70) at `at` and is back `downMs` later. */
  const crash = (
    service: string,
    at: number,
    downMs: number,
    reasons: unknown = "TypeError: fetch failed",
  ) => {
    const pid = 5000 + timeline.length;
    downs.push({ service, from: at, to: at + downMs, reasons });
    timeline.push(
      { at, event: "exit", service, code: 70, signal: null },
      { at, event: "restart-scheduled", service, delayMs: downMs },
      { at: at + downMs, event: "start", service, pid },
    );
  };
  /** A crash loop from `from`: up `upMs`, then down for its backoff, again and again. */
  const crashLoop = (
    service: string,
    from: number,
    upMs = s(100),
    downMs = s(50),
  ) => {
    for (let at = from + upMs; at < from + s(3_600); at += upMs + downMs)
      crash(service, at, downMs);
  };
  const pending = () =>
    SERVICES.filter((name) => downAt(name, clock) !== undefined);
  const deps: ChaosDeps = {
    compose: async () => ({ code: 0, stderr: "" }),
    sleep: async (ms) => {
      clock += ms;
    },
    signalGroup: (pid) => {
      signals.push({ pid, at: clock });
      // The supervisor restarts the killed node; it is back 60 s later.
      crash("node", clock, s(60), ["starting"]);
      for (const hook of onKill) hook(clock);
      flush();
    },
    environ: () => undefined,
    fetchNode: async () => ({}),
    now: () => clock,
    waitReady: async (timeoutMs, options) => {
      waits.push({ timeoutMs, honourStartGrace: options?.honourStartGrace });
      const deadline = clock + timeoutMs;
      while (pending().length > 0) {
        const back = Math.max(
          ...pending().map((name) => downAt(name, clock)!.to),
        );
        if (back > deadline) {
          clock = deadline;
          throw new Error(
            `services not ready after ${timeoutMs / 1000} s: ${pending().join(", ")}`,
          );
        }
        clock = back;
      }
      return { graced: false };
    },
    supervisorEvents: () => {
      flush();
      return seen;
    },
    readiness: async () =>
      SERVICES.map((name) => {
        const down = downAt(name, clock);
        const moved = repids.filter((r) => r.service === name && r.at <= clock);
        return {
          name,
          pid: moved.at(-1)?.pid ?? pids.get(name),
          alive: true,
          ready: down === undefined,
          ...(down === undefined ? {} : { reasons: down.reasons }),
        };
      }),
  };
  return {
    deps,
    crash,
    crashLoop,
    downs,
    repids,
    waits,
    signals,
    /** Runs `hook` with the kill's time, once the node is killed. */
    afterKill: (hook: (at: number) => void) => onKill.push(hook),
    /** When the kill fired (after the preflight's own stable window). */
    injectedAt: () => signals[0]!.at,
  };
};

/** A run whose node PID file names a process carrying this run's marker. */
const paths = () => {
  const runDir = mkdtempSync(join(tmpdir(), "devnet-chaos-stable-"));
  dirs.push(runDir);
  const pidDir = join(runDir, "services");
  mkdirSync(pidDir);
  writeFileSync(join(pidDir, "node.json"), JSON.stringify({ pid: 4242 }));
  return { runDir, pidDir, drillsLog: join(runDir, "drills.ndjson") };
};

const killNode = async (
  w: ReturnType<typeof world>,
  extra: { stableMs?: number } = {},
) => {
  const run = paths();
  const summary = await runDrills({
    drills: [{ kind: "kill", name: "kill-node", service: "node" }],
    ...run,
    deps: {
      ...w.deps,
      environ: () => `${SERVICE_MARKER_ENV}=${run.runDir}#node`,
    },
    recoveryMs: s(900),
    ...extra,
  });
  const log = readFileSync(run.drillsLog, "utf8")
    .trim()
    .split("\n")
    .map((line) => JSON.parse(line) as DrillRecord & { at: string });
  return { summary, log };
};

describe("runDrills counts only a stable stack as recovered", () => {
  it("fails a kill whose stack answers ready between a service's restarts", async () => {
    // Drill 9 on lc1: the watcher crash-loops (exit 70 every ~150 s) while
    // the node comes back, and one snapshot finds everything ready.
    const w = world();
    w.afterKill((at) => w.crashLoop("watcher", at));
    const { summary, log } = await killNode(w);
    expect(w.signals.map((signal) => signal.pid)).toEqual([4242]);
    expect(summary.counts).toEqual({ recovered: 0, skipped: 0, failed: 1 });
    // The preflight held its own 300 s window first.
    expect(w.injectedAt()).toBe(T0 + s(300));
    const [record] = log;
    expect(record).toMatchObject({
      drill: "kill-node",
      ok: false,
      recoveredAt: null,
    });
    expect(record!.injectedAt).toBe(iso(w.injectedAt()));
    expect(record!.detail).toMatch(/not stably ready within 900 s/);
    expect(record!.detail).toMatch(/last because watcher exited \(code 70\)/);
    expect(record!.detail).toMatch(
      /watcher: \d+ exits \(code 70 x\d+\), \d+ restarts, last not ready: TypeError: fetch failed/,
    );
    // It gave up at the bound, not at the first disruption.
    expect(Date.parse(record!.at)).toBeGreaterThanOrEqual(
      w.injectedAt() + s(900),
    );
  });

  it("dates a recovery to the start of a window the stack held with no restart", async () => {
    const w = world();
    const { summary, log } = await killNode(w);
    expect(summary.counts).toEqual({ recovered: 1, skipped: 0, failed: 0 });
    const [record] = log;
    // The node is back 60 s after the kill; the window then runs 300 s.
    expect(record!.recoveredAt).toBe(iso(w.injectedAt() + s(60)));
    expect(record!.detail).toMatch(
      /recovered in 60 s; stayed up with no restart for 300 s/,
    );
    expect(Date.parse(record!.at)).toBe(w.injectedAt() + s(60 + 300));
  });

  it("restarts the window when a service restarts inside it", async () => {
    const w = world();
    // The watcher exits 140 s into the window after the node's return.
    w.afterKill((at) => w.crash("watcher", at + s(200), s(30)));
    const { summary, log } = await killNode(w);
    expect(summary.counts).toEqual({ recovered: 1, skipped: 0, failed: 0 });
    expect(log[0]!.recoveredAt).toBe(iso(w.injectedAt() + s(230)));
    expect(Date.parse(log[0]!.at)).toBe(w.injectedAt() + s(230 + 300));
    // After a disruption the stack must settle inside the bound: no start grace.
    expect(w.waits.map((wait) => wait.honourStartGrace)).toEqual([
      true,
      true,
      false,
    ]);
  });

  it("restarts the window on an exit and start that fall between two probes", async () => {
    const w = world();
    // Back 3 s later, before the next 10 s probe sees it unready.
    w.afterKill((at) => w.crash("watcher", at + s(203), s(3)));
    const { log } = await killNode(w);
    expect(log[0]!.recoveredAt).toBe(iso(w.injectedAt() + s(210)));
  });

  it("restarts the window when a service's PID changes with no event", async () => {
    const w = world();
    w.afterKill((at) =>
      w.repids.push({ service: "watcher", at: at + s(150), pid: 7777 }),
    );
    const { log } = await killNode(w);
    expect(log[0]!.recoveredAt).toBe(iso(w.injectedAt() + s(150)));
    expect(Date.parse(log[0]!.at)).toBe(w.injectedAt() + s(150 + 300));
  });

  it("keeps the window through a running service's unready spell, with no exit", async () => {
    // A busy node answers unready (local finalization pending) and is not
    // crashing: once it is ready at the window's end the stack has recovered.
    const w = world();
    const busy = (from: number, to: number) => (at: number) =>
      w.downs.push({
        service: "node",
        from: at + s(from),
        to: at + s(to),
        reasons: ["local_finalization_pending"],
      });
    w.afterKill(busy(120, 140));
    // Unready across the window's end, too: waited for within the bound.
    w.afterKill(busy(350, 390));
    const { summary, log } = await killNode(w);
    expect(summary.counts.recovered).toBe(1);
    expect(log[0]!.recoveredAt).toBe(iso(w.injectedAt() + s(60)));
    expect(Date.parse(log[0]!.at)).toBe(w.injectedAt() + s(390));
  });

  it("fails a running service that is still unready at the window's end when the bound runs out", async () => {
    const w = world();
    w.afterKill((at) =>
      w.downs.push({
        service: "node",
        from: at + s(100),
        to: at + s(5_000),
        reasons: ["local_finalization_pending"],
      }),
    );
    const { summary, log } = await killNode(w);
    expect(summary.counts.failed).toBe(1);
    expect(log[0]!.detail).toMatch(
      /not stably ready within 900 s.*last because not ready at the window's end: services not ready after 540 s: node/,
    );
    expect(log[0]!.detail).toMatch(
      /node: last not ready: \["local_finalization_pending"\]/,
    );
  });

  it("stops at an abort between two windows without waiting again", async () => {
    const w = world();
    w.crashLoop("watcher", T0);
    const controller = new AbortController();
    const result = await awaitStable(
      {
        ...w.deps,
        readiness: async () => {
          const reports = await w.deps.readiness();
          if (w.deps.now() >= T0 + s(100)) controller.abort();
          return reports;
        },
      },
      {
        injectedAt: T0,
        deadline: T0 + s(900),
        boundMs: s(900),
        windowMs: s(300),
        signal: controller.signal,
      },
    );
    expect(result).toMatchObject({ ok: false });
    expect(!result.ok && result.detail).toMatch(/^interrupted before/);
    expect(w.waits).toHaveLength(1);
  });

  it("honours --stable, and refuses a stable window of 0", async () => {
    const w = world();
    const { log } = await killNode(w, { stableMs: s(180) });
    expect(w.injectedAt()).toBe(T0 + s(180));
    expect(Date.parse(log[0]!.at)).toBe(w.injectedAt() + s(60 + 180));
    await expect(killNode(world(), { stableMs: 0 })).rejects.toThrow(/above 0/);
  });

  it("refuses to inject into a stack already in a restart loop", async () => {
    const w = world();
    w.crashLoop("watcher", T0 - s(30));
    const { summary, log } = await killNode(w);
    expect(w.signals).toEqual([]);
    expect(summary.counts).toEqual({ recovered: 0, skipped: 0, failed: 1 });
    expect(log).toHaveLength(1);
    expect(log[0]).toMatchObject({
      drill: "kill-node",
      ok: false,
      injectedAt: null,
      recoveredAt: null,
    });
    expect(log[0]!.skipped).toBeUndefined();
    expect(log[0]!.detail).toMatch(
      /^not injected: preflight; not stably ready within 900 s/,
    );
    expect(log[0]!.detail).toMatch(/watcher: \d+ exits \(code 70 x\d+\)/);
  });
});
