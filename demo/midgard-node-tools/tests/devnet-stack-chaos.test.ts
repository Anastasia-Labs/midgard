import { spawn } from "node:child_process";
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
  type Drill,
  drillCatalogue,
  type DrillRecord,
  runDrills,
  selectDrills,
  type Trigger,
  TRIGGERS,
} from "../src/devnet-stack/chaos.js";
import { SERVICE_MARKER_ENV } from "../src/devnet-stack/supervisor.js";

const dirs: string[] = [];
const pids: number[] = [];
const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-chaos-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const pid of pids.splice(0))
    try {
      process.kill(pid, "SIGKILL");
    } catch {
      // Already gone.
    }
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const alive = (pid: number) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

const waitFor = async (
  what: string,
  check: () => boolean,
  timeoutMs = 10_000,
) => {
  const deadline = Date.now() + timeoutMs;
  while (!check()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
};

/** A detached idle process, like a supervised service, with `env` added. */
const spawnIdle = async (env: Record<string, string>) => {
  const child = spawn(process.execPath, ["-e", "setInterval(() => {}, 1000)"], {
    env: { ...process.env, ...env },
    detached: true,
    stdio: "ignore",
  });
  child.unref();
  const pid = child.pid!;
  pids.push(pid);
  await waitFor("the child to exec", () => {
    try {
      return readFileSync(`/proc/${pid}/environ`, "utf8").includes("PATH=");
    } catch {
      return false;
    }
  });
  return pid;
};

/** A run directory with a PID file for `service` naming `pid`. */
const runWith = (service: string, pid: number) => {
  const runDir = scratch();
  const pidDir = join(runDir, "stack/services");
  mkdirSync(pidDir, { recursive: true });
  writeFileSync(join(pidDir, `${service}.json`), JSON.stringify({ pid }));
  return {
    runDir,
    pidDir,
    drillsLog: join(runDir, "stack/logs/drills.ndjson"),
  };
};

/**
 * Fakes with a virtual clock: sleep advances it instantly. Signals and compose
 * calls are recorded, never sent unless `signalGroup` is overridden.
 */
const fakes = (overrides: Partial<ChaosDeps> = {}) => {
  let clock = Date.parse("2026-09-30T00:00:00Z");
  const signals: { pid: number; signal: string }[] = [];
  const composeCalls: string[] = [];
  const events: Record<string, unknown>[] = [];
  const deps: ChaosDeps = {
    signalGroup: (pid, signal) => {
      signals.push({ pid, signal });
    },
    environ: () => undefined,
    compose: async (args) => {
      composeCalls.push(args.join(" "));
      return { code: 0, stderr: "" };
    },
    fetchNode: async () => ({}),
    now: () => clock,
    sleep: async (ms) => {
      clock += ms;
    },
    waitReady: async () => ({ graced: false }),
    supervisorEvents: () => events,
    readiness: async () => [],
    ...overrides,
  };
  return {
    deps,
    signals,
    composeCalls,
    events,
    advance: (ms: number) => {
      clock += ms;
    },
  };
};

const logLines = (path: string) =>
  readFileSync(path, "utf8")
    .trim()
    .split("\n")
    .map((line) => JSON.parse(line) as DrillRecord & { at: string });

const nodeKill = (trigger?: Trigger): Drill => ({
  kind: "kill",
  name: trigger === undefined ? "kill-node" : "kill-node-on-admission",
  service: "node",
  ...(trigger === undefined ? {} : { trigger }),
});

const idlePipeline = {
  durableAdmission: { backlog: "0" },
  stateQueue: {
    unconfirmedSubmittedBlockTxHash: null,
    localFinalizationPending: false,
  },
};

describe("drillCatalogue", () => {
  it("offers a kill only for services the supervisor runs", () => {
    const names = (services: string[]) =>
      drillCatalogue(services).map((d) => d.name);
    const base = [
      "da-committee-0",
      "da-committee-1",
      "public-retained-da",
      "node",
    ];
    expect(names(base)).toEqual([
      "kill-node",
      "kill-node-on-admission",
      "kill-node-on-block-submitted",
      "kill-node-on-settlement",
      "kill-da-member-0",
      "kill-da-member-1",
      "kill-public-retained-da",
      "stop-kupo",
      "stop-ogmios",
      "pause-postgres",
      "restart-cardano-node",
    ]);
    expect(names([...base, "watcher"])).toContain("kill-watcher");
    expect(() => selectDrills(drillCatalogue(base), ["kill-watcher"])).toThrow(
      /unknown drill/,
    );
  });

  it("recognises each targeted moment from the node's status bodies", () => {
    expect(TRIGGERS.admission.holds(idlePipeline)).toBe(false);
    expect(
      TRIGGERS.admission.holds({ durableAdmission: { backlog: "3" } }),
    ).toBe(true);
    expect(TRIGGERS.blockSubmitted.holds(idlePipeline)).toBe(false);
    expect(
      TRIGGERS.blockSubmitted.holds({
        stateQueue: { unconfirmedSubmittedBlockTxHash: "ab" },
      }),
    ).toBe(true);
    const settlement = (state: string, detail: string) => ({
      settlement: { state, detail },
    });
    expect(
      TRIGGERS.settlement.holds(
        settlement("running", "settlement queue drained"),
      ),
    ).toBe(false);
    expect(
      TRIGGERS.settlement.holds(
        settlement("waiting", "settlement eligibility wait until x"),
      ),
    ).toBe(false);
    expect(
      TRIGGERS.settlement.holds(
        settlement(
          "waiting",
          "submitted exact journaled settlement transaction",
        ),
      ),
    ).toBe(true);
  });
});

describe("runDrills kills", () => {
  it("fires a targeted kill only once its trigger holds, then waits for the restart", async () => {
    const pid = 4242;
    const paths = runWith("node", pid);
    const marker = `${SERVICE_MARKER_ENV}=${paths.runDir}#node`;
    let polls = 0;
    const fake = fakes({
      environ: () => `PATH=/bin\0${marker}\0`,
      fetchNode: async (path) => {
        expect(path).toBe("/pipeline-status");
        polls += 1;
        // The kill must not have fired while the trigger did not hold.
        expect(fake.signals).toEqual([]);
        return polls < 5
          ? idlePipeline
          : { ...idlePipeline, durableAdmission: { backlog: "2" } };
      },
    });
    fake.deps = {
      ...fake.deps,
      signalGroup: (target, signal) => {
        fake.signals.push({ pid: target, signal });
        fake.events.push({ event: "exit", service: "node", pid: target });
        fake.events.push({ event: "start", service: "node", pid: target + 1 });
      },
    };
    fake.events.push({ event: "start", service: "node", pid });
    const summary = await runDrills({
      drills: [nodeKill(TRIGGERS.admission)],
      ...paths,
      deps: fake.deps,
    });
    expect(polls).toBe(5);
    expect(fake.signals).toEqual([{ pid, signal: "SIGKILL" }]);
    expect(summary.failures).toEqual([]);
    const [line] = logLines(paths.drillsLog);
    expect(line).toMatchObject({
      drill: "kill-node-on-admission",
      target: "node",
      ok: true,
    });
    expect(line!.injectedAt).not.toBeNull();
    expect(line!.recoveredAt).not.toBeNull();
    expect(line!.detail).toMatch(/durable admission backlog/);
  });

  it("skips a targeted kill cleanly when its trigger never holds", async () => {
    const paths = runWith("node", 1);
    const fake = fakes({ fetchNode: async () => idlePipeline });
    const summary = await runDrills({
      drills: [nodeKill(TRIGGERS.admission)],
      ...paths,
      deps: fake.deps,
      triggerTimeoutMs: 60_000,
    });
    expect(fake.signals).toEqual([]);
    expect(summary.counts).toEqual({ recovered: 0, skipped: 1, failed: 0 });
    expect(logLines(paths.drillsLog)).toMatchObject([
      {
        drill: "kill-node-on-admission",
        ok: true,
        skipped: true,
        injectedAt: null,
      },
    ]);
  });

  it("kills a process carrying this run's marker, and refuses one that does not", async () => {
    const dir = scratch();
    const ours = await spawnIdle({ [SERVICE_MARKER_ENV]: `${dir}#node` });
    const foreign = await spawnIdle({
      [SERVICE_MARKER_ENV]: `/elsewhere#node`,
    });
    const pidDir = join(dir, "services");
    mkdirSync(pidDir);
    const drillsLog = join(dir, "drills.ndjson");
    const run = async (pid: number) => {
      writeFileSync(join(pidDir, "node.json"), JSON.stringify({ pid }));
      const fake = fakes();
      // A real group signal and the real /proc environment.
      return runDrills({
        drills: [nodeKill()],
        runDir: dir,
        pidDir,
        drillsLog,
        deps: {
          ...fake.deps,
          signalGroup: (target, signal) => process.kill(-target, signal),
          environ: (target) => readFileSync(`/proc/${target}/environ`, "utf8"),
        },
        recoveryMs: 5_000,
      });
    };
    const refused = await run(foreign);
    expect(refused.failures[0]?.detail).toMatch(
      /refused: PID \d+ is not this run's node/,
    );
    expect(alive(foreign)).toBe(true);
    // Nothing restarts it here, so the recovery bound is missed.
    const killed = await run(ours);
    await waitFor("our process to die", () => !alive(ours));
    expect(killed.failures[0]?.detail).toMatch(
      /no new start of node within 5 s/,
    );
    expect(alive(foreign)).toBe(true);
  });

  it("reports a recovery that misses its bound as a failure", async () => {
    const paths = runWith("da-committee-0", 4242);
    const marker = `${SERVICE_MARKER_ENV}=${paths.runDir}#da-committee-0`;
    let calls = 0;
    const fake = fakes({
      environ: () => marker,
      waitReady: async (timeoutMs) => {
        // The preflight passes; after the kill the stack stays down.
        calls += 1;
        if (calls > 1) {
          fake.advance(timeoutMs);
          throw new Error("services not ready");
        }
        return { graced: false };
      },
    });
    fake.deps = {
      ...fake.deps,
      signalGroup: () => {
        fake.events.push({
          event: "start",
          service: "da-committee-0",
          pid: 4243,
        });
      },
    };
    const summary = await runDrills({
      drills: selectDrills(drillCatalogue(["da-committee-0"]), [
        "kill-da-member-0",
      ]),
      ...paths,
      deps: fake.deps,
    });
    expect(summary.failures).toHaveLength(1);
    expect(logLines(paths.drillsLog)).toMatchObject([
      {
        drill: "kill-da-member-0",
        target: "da-committee-0",
        ok: false,
        recoveredAt: null,
      },
    ]);
    expect(summary.failures[0]!.detail).toMatch(/not recovered within 900 s/);
  });

  it("counts a recovery that lands after its bound as a failure", async () => {
    const dir = scratch();
    let calls = 0;
    const fake = fakes({
      waitReady: async (timeoutMs) => {
        calls += 1;
        if (calls > 1) fake.advance(timeoutMs + 1);
        return { graced: false };
      },
    });
    const summary = await runDrills({
      drills: selectDrills(drillCatalogue([]), ["restart-cardano-node"]),
      runDir: dir,
      pidDir: dir,
      drillsLog: join(dir, "drills.ndjson"),
      deps: fake.deps,
      recoveryMs: 60_000,
    });
    expect(summary.failures[0]?.detail).toMatch(/over the 60 s bound/);
  });
});
