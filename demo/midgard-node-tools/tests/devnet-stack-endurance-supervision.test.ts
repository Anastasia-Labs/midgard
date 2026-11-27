import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { abortableSleep } from "../src/devnet-stack/reserve-float-chain.js";
import {
  DEFAULT_POLICY,
  type InProcessMaintainer,
  keepMaintaining,
  type ServiceSpec,
  superviseWithMaintainers,
  type SupervisorPaths,
} from "../src/devnet-stack/supervisor.js";

const dirs: string[] = [];
const tempDir = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-endurance-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const pathsAt = (dir: string): SupervisorPaths => ({
  runDir: dir,
  pidDir: join(dir, "services"),
  events: join(dir, "supervisor.ndjson"),
  serviceLog: (name) => join(dir, `${name}.log`),
});

const events = (paths: SupervisorPaths) =>
  readFileSync(paths.events, "utf8")
    .trim()
    .split("\n")
    .map((line) => JSON.parse(line) as Record<string, unknown>);

const alive = (pid: number) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

/** Polls `condition` every 20 ms; fails the test after `ms`, never hangs it. */
const until = async (condition: () => boolean, what: string, ms = 10_000) => {
  const deadline = Date.now() + ms;
  while (!condition()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
};

/** Resolves "late" unless `promise` settles within `ms`. */
const settlesWithin = (promise: Promise<unknown>, ms: number) =>
  Promise.race([
    promise.then(
      () => "resolved",
      (error: unknown) => (error instanceof Error ? error.message : "rejected"),
    ),
    new Promise((resolve) => setTimeout(() => resolve("late"), ms)),
  ]);

const service = (dir: string): ServiceSpec => ({
  name: "idle",
  command: process.execPath,
  args: ["-e", "setInterval(() => {}, 1000)"],
  cwd: dir,
  env: {},
});

const servicePid = (paths: SupervisorPaths) =>
  (
    JSON.parse(readFileSync(join(paths.pidDir, "idle.json"), "utf8")) as {
      pid: number;
    }
  ).pid;

describe("superviseWithMaintainers", () => {
  it("keeps the services running through a maintainer that throws, and restarts it", async () => {
    const dir = tempDir();
    const paths = pathsAt(dir);
    const abort = new AbortController();
    const runs: AbortSignal[] = [];
    const endurance: InProcessMaintainer = {
      name: "endurance",
      run: async (signal) => {
        runs.push(signal);
        if (runs.length === 1) throw new Error("Kupo answered 503");
        // The real maintainer: sleeps on the signal until it aborts.
        for (;;) await abortableSleep(signal)(60_000);
      },
    };
    let settled = false;
    const supervising = superviseWithMaintainers(
      [service(dir)],
      paths,
      abort.signal,
      [endurance],
      { restartMs: 50 },
    ).finally(() => (settled = true));
    let pid: number | undefined;
    try {
      await until(() => runs.length === 2, "the maintainer's second run");
      pid = servicePid(paths);
      expect(settled).toBe(false);
      expect(alive(pid)).toBe(true);
      expect(
        events(paths).filter((e) => e.event !== "supervisor-start"),
      ).toEqual([
        expect.objectContaining({ event: "start", service: "idle", pid }),
        expect.objectContaining({
          event: "maintainer-error",
          maintainer: "endurance",
          error: "Kupo answered 503",
        }),
      ]);
    } finally {
      abort.abort();
      await supervising.catch(() => undefined);
      if (pid !== undefined && alive(pid)) process.kill(pid, "SIGKILL");
    }
    await expect(supervising).resolves.toBeUndefined();
    expect(runs[1]?.aborted).toBe(true);
    // The "stopped" rejection of the shutdown is not a fault.
    expect(
      events(paths).filter((e) => e.event === "maintainer-error"),
    ).toHaveLength(1);
    expect(events(paths).at(-1)).toEqual(
      expect.objectContaining({ event: "supervisor-stop" }),
    );
    expect(alive(pid)).toBe(false);
  });

  it("stops its maintainers on the stop signal, before the services have stopped, and does not wait out a restart", async () => {
    const dir = tempDir();
    const paths = pathsAt(dir);
    const armed = join(dir, "armed");
    // A service that outlives SIGTERM: stopping it takes the stop grace.
    const slow: ServiceSpec = {
      ...service(dir),
      args: [
        "-e",
        "process.on('SIGTERM', () => {}); require('fs').writeFileSync(process.argv[1], ''); setInterval(() => {}, 1000)",
        armed,
      ],
    };
    const abort = new AbortController();
    let watching: AbortSignal | undefined;
    let failures = 0;
    let settled = false;
    const supervising = superviseWithMaintainers(
      [slow],
      paths,
      abort.signal,
      [
        {
          name: "watching",
          run: async (signal) => {
            watching = signal;
            for (;;) await abortableSleep(signal)(60_000);
          },
        },
        // Failed, and waiting out a long restart when the stop comes.
        {
          name: "failed",
          run: () => {
            failures += 1;
            return Promise.reject(new Error("Kupo answered 503"));
          },
        },
      ],
      { policy: { ...DEFAULT_POLICY, stopGraceMs: 1_500 }, restartMs: 60_000 },
    ).finally(() => (settled = true));
    let pid: number | undefined;
    try {
      await until(
        () => existsSync(armed) && watching !== undefined && failures === 1,
        "the service and both maintainers",
      );
      pid = servicePid(paths);
      abort.abort();
      await until(
        () => watching?.aborted === true,
        "the maintainers' stop",
        300,
      );
      expect(settled).toBe(false);
      expect(alive(pid)).toBe(true);
      expect(await settlesWithin(supervising, 5_000)).toBe("resolved");
    } finally {
      abort.abort();
      await supervising.catch(() => undefined);
      if (pid !== undefined && alive(pid)) process.kill(pid, "SIGKILL");
    }
    expect(failures).toBe(1);
    expect(
      events(paths)
        .filter((e) => e.event === "kill")
        .map((e) => e.service),
    ).toEqual(["idle"]);
  });

  it("stops every maintainer and rethrows at once when the supervisor itself fails", async () => {
    const dir = tempDir();
    // The pid directory cannot be made: superviseServices fails at its start.
    writeFileSync(join(dir, "blocked"), "");
    const paths = { ...pathsAt(dir), pidDir: join(dir, "blocked", "services") };
    const seen: AbortSignal[] = [];
    // Work that never watches the signal, like a cardano-cli call in flight.
    const stuck: InProcessMaintainer = {
      name: "stuck",
      run: (signal) => {
        seen.push(signal);
        return new Promise(() => {});
      },
    };
    const supervising = superviseWithMaintainers(
      [service(dir)],
      paths,
      new AbortController().signal,
      [stuck],
    );
    expect(await settlesWithin(supervising, 2_000)).toMatch(/ENOTDIR/);
    expect(seen).toHaveLength(1);
    expect(seen[0]?.aborted).toBe(true);
  });
});

describe("keepMaintaining", () => {
  it("stops at once on abort while it waits to restart a failed maintainer", async () => {
    const abort = new AbortController();
    const recorded: Record<string, unknown>[] = [];
    const keeping = keepMaintaining(
      { name: "m", run: () => Promise.reject(new Error("Kupo answered 503")) },
      abort.signal,
      { restartMs: 60_000, record: (event) => recorded.push(event) },
    );
    await until(() => recorded.length === 1, "the maintainer's failure");
    abort.abort();
    expect(await settlesWithin(keeping, 1_000)).toBe("resolved");
    expect(recorded).toHaveLength(1);
  });

  it("records a standing failure once and a new one again, then stops on abort", async () => {
    const abort = new AbortController();
    const recorded: Record<string, unknown>[] = [];
    const failures = ["a", "a", "a", "b"];
    let runs = 0;
    await keepMaintaining(
      {
        name: "m",
        run: () => {
          runs += 1;
          const failure = failures.shift();
          if (failure === undefined) abort.abort();
          return Promise.reject(new Error(failure ?? "stopped"));
        },
      },
      abort.signal,
      { restartMs: 1, record: (event) => recorded.push(event) },
    );
    expect(runs).toBe(5);
    expect(recorded.map((event) => event.error)).toEqual(["a", "b"]);
  });

  it("survives a maintainer that throws synchronously and an event log that cannot be written", async () => {
    const abort = new AbortController();
    let runs = 0;
    await keepMaintaining(
      {
        name: "m",
        run: () => {
          runs += 1;
          if (runs === 3) abort.abort();
          throw new Error("no run records");
        },
      },
      abort.signal,
      {
        restartMs: 1,
        record: () => {
          throw new Error("EROFS");
        },
      },
    );
    expect(runs).toBe(3);
  });
});

describe("abortableSleep", () => {
  it("rejects at once on a signal that has already aborted", async () => {
    const abort = new AbortController();
    abort.abort();
    expect(
      await settlesWithin(abortableSleep(abort.signal)(60_000), 1_000),
    ).toBe("stopped");
  });

  it("rejects at once when the signal aborts mid-sleep", async () => {
    const abort = new AbortController();
    const sleeping = abortableSleep(abort.signal)(60_000);
    setTimeout(() => abort.abort(), 20);
    expect(await settlesWithin(sleeping, 1_000)).toBe("stopped");
  });

  it("resolves after its delay while the signal stays live", async () => {
    const abort = new AbortController();
    expect(await settlesWithin(abortableSleep(abort.signal)(10), 1_000)).toBe(
      "resolved",
    );
  });
});
