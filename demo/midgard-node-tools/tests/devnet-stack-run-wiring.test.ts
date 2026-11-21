import { spawn } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  codeStamp,
  runtimeDistTargets,
} from "../src/devnet-stack/dist-freshness.js";
import {
  holdRunForResume,
  runLockPaths,
  startRunUser,
  upDeps,
} from "../src/devnet-stack/fresh-controller.js";
import { type Layout, makeLayout } from "../src/devnet-stack/layout.js";
import { lockOwner, processStartTime } from "../src/devnet-stack/lock.js";
import {
  recordSupervisorSpecs,
  supervisorRuns,
} from "../src/devnet-stack/stack.js";
import {
  SERVICE_MARKER_ENV,
  type ServiceSpec,
} from "../src/devnet-stack/supervisor.js";

const dirs: string[] = [];
const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-run-wiring-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const writeAt = (path: string, content: string, ageMs: number) => {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, content);
  const at = new Date(Date.now() - ageMs);
  utimesSync(path, at, at);
};

/**
 * An existing run on a scratch checkout whose every runtime dist was built
 * after its sources.
 */
const existingRun = (): Layout => {
  const repo = scratch();
  const demo = join(repo, "demo");
  const layout = {
    ...makeLayout(join(scratch(), "lc1")),
    repoRoot: repo,
    nodeRoot: join(demo, "midgard-node"),
    daRoot: join(demo, "da-committee-node"),
    watcherRoot: join(demo, "midgard-watcher"),
    toolsRoot: join(demo, "midgard-node-tools"),
  };
  for (const target of runtimeDistTargets(layout)) {
    for (const source of target.sources)
      writeAt(join(source, "a.ts"), "a", 120_000);
    writeAt(target.dist, `${target.packageName}();`, 60_000);
  }
  mkdirSync(layout.state, { recursive: true });
  return layout;
};

/** Makes the node's dist older than a source. */
const staleNode = (layout: Layout) =>
  writeAt(join(layout.nodeRoot, "src/a.ts"), "a2", 0);

/** A lock file naming a live process other than this one. */
const heldByOther = (path: string) => {
  writeFileSync(path, `${process.ppid} ${processStartTime(process.ppid)}`);
  return process.ppid;
};

const noResume = async () => {};

describe("the production steps of up", () => {
  it("keeps the build lock beside the run and the controller lock in its state", () => {
    const layout = existingRun();
    const deps = upDeps(layout, noResume);
    expect(deps.buildLockPath).toBe(`${layout.runDir}.build.lock`);
    expect(deps.lockPath).toBe(layout.lock);
  });

  it("refuses dists older than their sources, and passes current ones", () => {
    const layout = existingRun();
    const deps = upDeps(layout, noResume);
    expect(() => deps.requireFreshDists()).not.toThrow();
    staleNode(layout);
    expect(() => deps.requireFreshDists()).toThrow(
      "up refuses to run stale builds",
    );
  });

  it("names the run's live journey, and nobody when none runs", () => {
    const layout = existingRun();
    const deps = upDeps(layout, noResume);
    expect(deps.runUsers()).toEqual([]);
    const owner = heldByOther(runLockPaths(layout).journey);
    expect(deps.runUsers()).toEqual([{ name: "journey", pid: owner }]);
  });

  it("reads the run's live supervisor", () => {
    const layout = existingRun();
    const deps = upDeps(layout, noResume);
    expect(deps.runningSupervisor()).toBeUndefined();
    expect(heldByOther(layout.supervisorPid)).toBe(deps.runningSupervisor());
  });

  it("stamps the run's runtime dists", () => {
    const layout = existingRun();
    const deps = upDeps(layout, noResume);
    const before = deps.codeStamp();
    expect(before).toBe(codeStamp(runtimeDistTargets(layout)));
    writeAt(join(layout.nodeRoot, "dist/index.js"), "patched();", 60_000);
    expect(deps.codeStamp()).not.toBe(before);
  });

  it("stops a service a dead supervisor left running, and records it", async () => {
    const layout = existingRun();
    const child = spawn(
      process.execPath,
      ["-e", "setInterval(() => {}, 1000)"],
      {
        detached: true,
        stdio: "ignore",
        env: { ...process.env, [SERVICE_MARKER_ENV]: `${layout.runDir}#node` },
      },
    );
    const exited = new Promise((resolve) => child.once("exit", resolve));
    const sleep = (ms: number) =>
      new Promise((resolve) => setTimeout(resolve, ms));
    try {
      const pidFile = join(layout.state, "services/node.json");
      // The child has exec'd once its environment is readable.
      for (let tries = 0; tries < 100; tries += 1) {
        const environ = readFileSync(`/proc/${child.pid}/environ`, "utf8");
        if (environ.includes(SERVICE_MARKER_ENV)) break;
        await sleep(20);
      }
      writeAt(pidFile, JSON.stringify({ pid: child.pid }), 0);
      await upDeps(layout, noResume).sweepOrphans();
      await Promise.race([exited, sleep(5_000)]);
      expect(child.signalCode).toBe("SIGTERM");
      expect(existsSync(pidFile)).toBe(false);
      expect(readFileSync(layout.supervisorEvents, "utf8")).toContain(
        `"by":"up","event":"orphan-stop","service":"node","pid":${child.pid}`,
      );
    } finally {
      if (child.exitCode === null && child.signalCode === null)
        child.kill("SIGKILL");
    }
  }, 20_000);
});

describe("the start of a journey or drill", () => {
  it("takes its own lock on current dists with no build running", () => {
    const layout = existingRun();
    startRunUser(layout, "journey");
    expect(lockOwner(runLockPaths(layout).journey)).toBe(process.pid);
  });

  it("refuses while an up holds the build lock, holding its own lock first", () => {
    const layout = existingRun();
    const locks = runLockPaths(layout);
    const owner = heldByOther(locks.build);
    expect(() => startRunUser(layout, "drill")).toThrow(
      `drill refuses to start while up (pid ${owner}) may rebuild`,
    );
    // Taken before the check: an up deciding now sees this drill.
    expect(lockOwner(locks.drill)).toBe(process.pid);
  });

  it("refuses while another of the same command holds its lock", () => {
    const layout = existingRun();
    const owner = heldByOther(runLockPaths(layout).journey);
    expect(() => startRunUser(layout, "journey")).toThrow(
      `another controller (pid ${owner}) holds`,
    );
  });

  it("refuses dists older than their sources", () => {
    const layout = existingRun();
    staleNode(layout);
    expect(() => startRunUser(layout, "drill")).toThrow(
      "drill refuses to run stale builds",
    );
  });

  it("refuses when the dists changed between its first stamp and its lock", () => {
    const layout = existingRun();
    const stamps = ["loaded", "rebuilt"];
    const calls: (number | undefined)[] = [];
    expect(() =>
      startRunUser(layout, "journey", () => {
        calls.push(lockOwner(runLockPaths(layout).journey));
        return stamps.shift()!;
      }),
    ).toThrow("journey: the runtime dists changed while it started");
    // Stamped once before its lock and once holding it.
    expect(calls).toEqual([undefined, process.pid]);
  });
});

describe("the resume head of up", () => {
  it("checks the loaded code while holding the controller lock", () => {
    const layout = existingRun();
    const seen: (number | undefined)[] = [];
    holdRunForResume(layout, {
      stamp: "code",
      assertUnchanged: () => seen.push(lockOwner(layout.lock)),
    });
    expect(seen).toEqual([process.pid]);
  });

  it("refuses when a build rewrote the dists, and when another controller holds the run", () => {
    const layout = existingRun();
    const rebuilt = () => {
      throw new Error("the runtime dists changed");
    };
    expect(() =>
      holdRunForResume(layout, { stamp: "code", assertUnchanged: rebuilt }),
    ).toThrow("the runtime dists changed");
    const other = existingRun();
    const owner = heldByOther(other.lock);
    let checked = false;
    expect(() =>
      holdRunForResume(other, {
        stamp: "code",
        assertUnchanged: () => {
          checked = true;
        },
      }),
    ).toThrow(`another controller (pid ${owner}) holds`);
    expect(checked).toBe(false);
  });
});

describe("the supervisor's record", () => {
  const spec: ServiceSpec = {
    name: "node",
    command: "/usr/bin/node",
    args: ["dist/index.js", "listen"],
    cwd: "/repo/demo/midgard-node",
    env: { A: "1" },
  };

  it("names the code on disk when it starts, so a later rebuild reads as other code", () => {
    const layout = existingRun();
    recordSupervisorSpecs(layout, [spec]);
    const stamp = () => codeStamp(runtimeDistTargets(layout));
    expect(supervisorRuns(layout, [spec], stamp())).toBe(true);
    writeAt(join(layout.watcherRoot, "dist/cli.js"), "rebuilt();", 0);
    expect(supervisorRuns(layout, [spec], stamp())).toBe(false);
  });
});
