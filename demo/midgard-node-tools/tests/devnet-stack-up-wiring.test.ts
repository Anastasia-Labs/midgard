import { EventEmitter } from "node:events";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { Command } from "commander";
import { afterEach, describe, expect, it } from "vitest";

import {
  type ControllerChild,
  liveRunUsers,
  refuseWhileBuilding,
  registerUp,
  runLockPaths,
  type Spawner,
  type UpDeps,
  type UpOptions,
} from "../src/devnet-stack/fresh-controller.js";
import { processStartTime } from "../src/devnet-stack/lock.js";

const dirs: string[] = [];
const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-up-wiring-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

/** A lock file naming a live process other than this one. */
const heldByOther = (path: string) => {
  writeFileSync(path, `${process.ppid} ${processStartTime(process.ppid)}`);
  return process.ppid;
};

/**
 * Parses `args` through the real `up` declaration. The tree is stale and a
 * supervisor is live, so a build would decide, stop, build and re-execute.
 */
const parseUp = async (args: readonly string[]) => {
  const dir = scratch();
  const order: string[] = [];
  const spawned: (readonly string[])[] = [];
  const wired: UpOptions[] = [];
  const exits: number[] = [];
  const spawner: Spawner = (_command, spawnArgs) => {
    spawned.push(spawnArgs);
    const child = new EventEmitter();
    setImmediate(() => child.emit("exit", 5, null));
    return Object.assign(child, {
      kill: () => true,
    }) as unknown as ControllerChild;
  };
  const program = new Command().exitOverride();
  registerUp(
    program,
    (options) => {
      wired.push(options);
      const deps: UpDeps = {
        codeStamp: () => "code",
        validateEnvironment: async () => {},
        pendingBuild: async () => {
          order.push("decide");
          return ["midgard-node: dist/index.js is older than src/a.ts"];
        },
        buildEverything: async () => {
          order.push("build");
        },
        buildLockPath: join(dir, "lc1.build.lock"),
        lockPath: join(dir, "lc1", "stack", "controller.lock"),
        runUsers: () => [],
        runningSupervisor: () => 4242,
        stopSupervisor: async () => {
          order.push("stop");
        },
        sweepOrphans: async () => {
          order.push("sweep");
        },
        requireFreshDists: () => {
          order.push("fresh-check");
        },
        resume: async () => {
          order.push("resume");
        },
      };
      return {
        deps,
        launch: {
          execPath: "/usr/bin/node",
          execArgv: [],
          argv: ["/usr/bin/node", "/repo/dist/devnet-stack.js", ...args],
          env: {},
          spawner,
          signals: new EventEmitter(),
        },
      };
    },
    (code) => exits.push(code),
  );
  await program.parseAsync(args, { from: "user" });
  return { order, spawned, wired, exits };
};

describe("the up command", () => {
  it("passes --no-build through as no build: it checks the dists and resumes in place, never re-executing", async () => {
    const parsed = await parseUp([
      "up",
      "--run-dir",
      "/runs/lc1",
      "--no-build",
    ]);
    expect(parsed.wired).toEqual([
      { runDir: "/runs/lc1", build: false, readyTimeoutMs: 1_200_000 },
    ]);
    expect(parsed.order).toEqual(["fresh-check", "resume"]);
    expect(parsed.spawned).toEqual([]);
    expect(parsed.exits).toEqual([0]);
  });

  it("builds a stale tree by default, then re-executes once with --no-build and reports its status", async () => {
    const parsed = await parseUp([
      "up",
      "--run-dir",
      "/runs/lc1",
      "--ready-timeout",
      "60",
    ]);
    expect(parsed.wired).toEqual([
      { runDir: "/runs/lc1", build: true, readyTimeoutMs: 60_000 },
    ]);
    expect(parsed.order).toEqual(["decide", "stop", "build"]);
    expect(parsed.spawned).toEqual([
      [
        "/repo/dist/devnet-stack.js",
        "up",
        "--run-dir",
        "/runs/lc1",
        "--ready-timeout",
        "60",
        "--no-build",
      ],
    ]);
    expect(parsed.exits).toEqual([5]);
  });
});

describe("run locks of journey and drill", () => {
  it("keeps the build lock beside the run directory and the others in its state", () => {
    expect(
      runLockPaths({ runDir: "/runs/lc1", state: "/runs/lc1/stack" }),
    ).toEqual({
      build: "/runs/lc1.build.lock",
      journey: "/runs/lc1/stack/journey.lock",
      drill: "/runs/lc1/stack/drill.lock",
    });
  });

  it("names only the live journey and drill processes", () => {
    const dir = scratch();
    const locks = runLockPaths({ runDir: join(dir, "lc1"), state: dir });
    expect(liveRunUsers(locks)).toEqual([]);
    const owner = heldByOther(locks.drill);
    // A lock left by a process that has exited names nobody.
    writeFileSync(locks.journey, "999999999 1");
    expect(liveRunUsers(locks)).toEqual([{ name: "drill", pid: owner }]);
  });

  it("refuses to start a journey or drill while an up holds the build lock, and starts it otherwise", () => {
    const dir = scratch();
    const buildLock = join(dir, "lc1.build.lock");
    expect(() => refuseWhileBuilding(buildLock, "journey")).not.toThrow();
    const owner = heldByOther(buildLock);
    expect(() => refuseWhileBuilding(buildLock, "drill")).toThrow(
      `drill refuses to start while up (pid ${owner}) may rebuild this run's dists (${buildLock})`,
    );
  });
});
