import { EventEmitter } from "node:events";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { constants, tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  type ControllerChild,
  type ControllerLaunch,
  type LiveProcess,
  runFreshController,
  runUp,
  type Spawner,
  type UpDeps,
} from "../src/devnet-stack/fresh-controller.js";
import { lockOwner, processStartTime } from "../src/devnet-stack/lock.js";

const dirs: string[] = [];
const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-fresh-controller-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const ARGV = [
  "/usr/bin/node",
  "/repo/demo/midgard-node-tools/dist/devnet-stack.js",
  "up",
  "--run-dir",
  "/runs/lc1",
  "--ready-timeout",
  "600",
];

type Spawned = {
  command: string;
  args: readonly string[];
  stdio: unknown;
  env: unknown;
  lockHeld: boolean;
};

/** A spawner whose child exits as `outcome` says, one tick after it starts. */
const fakeSpawner = (
  outcome: { code: number | null; signal: NodeJS.Signals | null },
  lockPath?: string,
) => {
  const spawned: Spawned[] = [];
  const kills: (NodeJS.Signals | number | undefined)[] = [];
  let child: EventEmitter | undefined;
  const spawner: Spawner = (command, args, options) => {
    spawned.push({
      command,
      args,
      stdio: options.stdio,
      env: options.env,
      lockHeld: lockPath !== undefined && lockOwner(lockPath) !== undefined,
    });
    const emitter = new EventEmitter();
    child = emitter;
    setImmediate(() => emitter.emit("exit", outcome.code, outcome.signal));
    return Object.assign(emitter, {
      kill: (signal?: NodeJS.Signals | number) => {
        kills.push(signal);
        return true;
      },
    }) as unknown as ControllerChild;
  };
  return { spawner, spawned, kills, child: () => child };
};

const launch = (
  spawner: Spawner,
  signals = new EventEmitter(),
): ControllerLaunch => ({
  execPath: "/usr/bin/node",
  execArgv: ["--enable-source-maps"],
  argv: ARGV,
  env: { MARKER: "kept" },
  spawner,
  signals,
});

/** A run whose state directory exists (or not), and its two lock paths. */
const run = (withState = true) => {
  const dir = scratch();
  const state = join(dir, "lc1", "stack");
  if (withState) mkdirSync(state, { recursive: true });
  return {
    state,
    buildLockPath: join(dir, "lc1.build.lock"),
    lockPath: join(state, "controller.lock"),
  };
};

/** Steps that record what ran, in order, into `order`. */
const fakeDeps = (
  paths: { buildLockPath: string; lockPath: string },
  options: {
    pending?: readonly string[];
    supervisor?: number;
    users?: readonly LiveProcess[];
    buildError?: Error;
    stopError?: Error;
    stamps?: string[];
  } = {},
) => {
  const order: string[] = [];
  const stamps = options.stamps ?? ["code"];
  const held = (path: string) => lockOwner(path) === process.pid;
  const deps: UpDeps = {
    ...paths,
    codeStamp: () => (stamps.length > 1 ? stamps.shift()! : stamps[0]!),
    validateEnvironment: async () => {
      order.push("validate");
    },
    pendingBuild: async () => {
      order.push(`decide build-lock=${held(paths.buildLockPath)}`);
      return options.pending ?? [];
    },
    buildEverything: async () => {
      order.push(
        `build build-lock=${held(paths.buildLockPath)} controller-lock=${held(paths.lockPath)}`,
      );
      if (options.buildError !== undefined) throw options.buildError;
    },
    runUsers: () => options.users ?? [],
    runningSupervisor: () => options.supervisor,
    stopSupervisor: async () => {
      order.push(`stop controller-lock=${held(paths.lockPath)}`);
      if (options.stopError !== undefined) throw options.stopError;
    },
    sweepOrphans: async () => {
      order.push(`sweep controller-lock=${held(paths.lockPath)}`);
    },
    requireFreshDists: () => {
      order.push("fresh-check");
    },
    resume: async (code) => {
      order.push(`resume ${code.stamp}`);
      code.assertUnchanged();
    },
  };
  return { deps, order };
};

/** A lock file naming a live process other than this one. */
const heldByOther = (path: string) => {
  writeFileSync(path, `${process.ppid} ${processStartTime(process.ppid)}`);
  return process.ppid;
};

describe("runUp", () => {
  it("builds a stale tree under the build and controller locks, then hands the rest to exactly one --no-build controller", async () => {
    const paths = run();
    const fake = fakeSpawner({ code: 0, signal: null }, paths.lockPath);
    const { deps, order } = fakeDeps(paths, {
      pending: ["midgard-node: dist/index.js is older than src/a.ts"],
    });
    expect(await runUp(true, deps, launch(fake.spawner))).toBe(0);
    // The rest never runs on the controller that built. With no supervisor
    // live, services a dead one left are stopped before the build.
    expect(order).toEqual([
      "validate",
      "decide build-lock=true",
      "sweep controller-lock=true",
      "build build-lock=true controller-lock=true",
    ]);
    expect(fake.spawned).toEqual([
      {
        command: "/usr/bin/node",
        args: [
          "--enable-source-maps",
          "/repo/demo/midgard-node-tools/dist/devnet-stack.js",
          "up",
          "--run-dir",
          "/runs/lc1",
          "--ready-timeout",
          "600",
          "--no-build",
        ],
        stdio: "inherit",
        env: { MARKER: "kept" },
        // The child takes the controller lock itself.
        lockHeld: false,
      },
    ]);
    expect(existsSync(paths.lockPath)).toBe(false);
    expect(existsSync(paths.buildLockPath)).toBe(false);
  });

  it("returns the fresh controller's exit status", async () => {
    const fake = fakeSpawner({ code: 3, signal: null });
    const { deps } = fakeDeps(run(), { pending: ["stale"] });
    expect(await runUp(true, deps, launch(fake.spawner))).toBe(3);
  });

  it("never re-executes without a build: it checks the dists and the rest runs here", async () => {
    const paths = run();
    const fake = fakeSpawner({ code: 0, signal: null });
    // Stale and with a live supervisor: --no-build still neither decides nor stops.
    const { deps, order } = fakeDeps(paths, {
      pending: ["stale"],
      supervisor: 4242,
    });
    expect(await runUp(false, deps, launch(fake.spawner))).toBe(0);
    expect(order).toEqual(["validate", "fresh-check", "resume code"]);
    expect(fake.spawned).toEqual([]);
  });

  it("builds nothing, stops nothing and continues itself when every build output is current", async () => {
    const paths = run();
    const fake = fakeSpawner({ code: 0, signal: null });
    const { deps, order } = fakeDeps(paths, { pending: [], supervisor: 4242 });
    expect(await runUp(true, deps, launch(fake.spawner))).toBe(0);
    expect(order).toEqual([
      "validate",
      "decide build-lock=true",
      "fresh-check",
      "resume code",
    ]);
    expect(fake.spawned).toEqual([]);
    expect(existsSync(paths.buildLockPath)).toBe(false);
  });

  it("stops a live supervisor, holding the controller lock, before it builds", async () => {
    const paths = run();
    const fake = fakeSpawner({ code: 0, signal: null });
    const { deps, order } = fakeDeps(paths, {
      pending: ["stale"],
      supervisor: 4242,
    });
    expect(await runUp(true, deps, launch(fake.spawner))).toBe(0);
    expect(order).toEqual([
      "validate",
      "decide build-lock=true",
      "stop controller-lock=true",
      "build build-lock=true controller-lock=true",
    ]);
    expect(fake.spawned).toHaveLength(1);
  });

  it("never builds when the supervisor does not stop, and releases both locks", async () => {
    const paths = run();
    const fake = fakeSpawner({ code: 0, signal: null });
    const { deps, order } = fakeDeps(paths, {
      pending: ["stale"],
      supervisor: 4242,
      stopError: new Error("supervisor 4242 did not stop within 90 s"),
    });
    await expect(runUp(true, deps, launch(fake.spawner))).rejects.toThrow(
      "supervisor 4242 did not stop within 90 s",
    );
    expect(order).toEqual([
      "validate",
      "decide build-lock=true",
      "stop controller-lock=true",
    ]);
    expect(fake.spawned).toEqual([]);
    expect(existsSync(paths.lockPath)).toBe(false);
    expect(existsSync(paths.buildLockPath)).toBe(false);
  });

  it("leaves the services stopped, says so and starts no controller when the build fails", async () => {
    const paths = run();
    const fake = fakeSpawner({ code: 0, signal: null });
    const { deps, order } = fakeDeps(paths, {
      pending: ["stale"],
      supervisor: 4242,
      buildError: new Error("workspace-build failed"),
    });
    await expect(runUp(true, deps, launch(fake.spawner))).rejects.toThrow(
      "build failed; the services stay stopped (supervisor 4242 was stopped before the build); run up again to retry: workspace-build failed",
    );
    expect(order).toContain("stop controller-lock=true");
    expect(fake.spawned).toEqual([]);
    expect(existsSync(paths.lockPath)).toBe(false);
    expect(existsSync(paths.buildLockPath)).toBe(false);
  });

  it("refuses to build while a journey or drill of the run is live, naming each, and stops nothing", async () => {
    const fake = fakeSpawner({ code: 0, signal: null });
    const { deps, order } = fakeDeps(run(), {
      pending: ["stale"],
      supervisor: 4242,
      users: [
        { name: "journey", pid: 111 },
        { name: "drill", pid: 222 },
      ],
    });
    await expect(runUp(true, deps, launch(fake.spawner))).rejects.toThrow(
      "up refuses to build while journey (pid 111) and drill (pid 222) runs on this run",
    );
    expect(order).toEqual(["validate", "decide build-lock=true"]);
    expect(fake.spawned).toEqual([]);
  });

  it("refuses to build while another up holds the controller lock, before stopping anything", async () => {
    const paths = run();
    const owner = heldByOther(paths.lockPath);
    const fake = fakeSpawner({ code: 0, signal: null });
    const { deps, order } = fakeDeps(paths, {
      pending: ["stale"],
      supervisor: 4242,
    });
    await expect(runUp(true, deps, launch(fake.spawner))).rejects.toThrow(
      `another controller (pid ${owner}) holds ${paths.lockPath}`,
    );
    expect(order).toEqual(["validate", "decide build-lock=true"]);
  });

  it("refuses while another up holds the build lock, before deciding", async () => {
    const paths = run();
    const owner = heldByOther(paths.buildLockPath);
    const { deps, order } = fakeDeps(paths, { pending: ["stale"] });
    await expect(
      runUp(true, deps, launch(fakeSpawner({ code: 0, signal: null }).spawner)),
    ).rejects.toThrow(
      `another controller (pid ${owner}) holds ${paths.buildLockPath}`,
    );
    expect(order).toEqual(["validate"]);
  });

  it("writes nothing into a fresh run's directory: chain generation would move it aside", async () => {
    const paths = run(false);
    const { deps, order } = fakeDeps(paths, { pending: ["stale"] });
    await runUp(
      true,
      deps,
      launch(fakeSpawner({ code: 0, signal: null }).spawner),
    );
    expect(order).toContain("build build-lock=true controller-lock=false");
    expect(existsSync(paths.state)).toBe(false);
    expect(existsSync(join(paths.state, ".."))).toBe(false);
  });

  it("fails the resume when the dists changed after this controller loaded them", async () => {
    const { deps, order } = fakeDeps(run(), { stamps: ["loaded", "rebuilt"] });
    await expect(
      runUp(
        false,
        deps,
        launch(fakeSpawner({ code: 0, signal: null }).spawner),
      ),
    ).rejects.toThrow(
      "the runtime dists changed after this controller loaded them",
    );
    // The stamp is the one taken at start, before anything else ran.
    expect(order).toEqual(["validate", "fresh-check", "resume loaded"]);
  });
});

describe("runFreshController", () => {
  it("fails when the child is killed by a signal", async () => {
    const fake = fakeSpawner({ code: null, signal: "SIGKILL" });
    const code = await runFreshController(launch(fake.spawner));
    expect(code).toBe(128 + constants.signals.SIGKILL);
  });

  it("passes SIGINT, SIGTERM and SIGHUP on to the child and stops listening once it exits", async () => {
    const signals = new EventEmitter();
    const fake = fakeSpawner({ code: 0, signal: null });
    const done = runFreshController(launch(fake.spawner, signals));
    signals.emit("SIGTERM");
    signals.emit("SIGINT");
    signals.emit("SIGHUP");
    expect(fake.kills).toEqual(["SIGTERM", "SIGINT", "SIGHUP"]);
    await done;
    for (const signal of ["SIGINT", "SIGTERM", "SIGHUP"])
      expect(signals.listenerCount(signal)).toBe(0);
  });

  it("runs a real child with this stdio and environment and reports its exit or signal", async () => {
    const dir = scratch();
    const entry = join(dir, "controller.mjs");
    const record = join(dir, "argv.json");
    writeFileSync(
      entry,
      [
        "import { writeFileSync } from 'node:fs';",
        `writeFileSync(${JSON.stringify(record)}, JSON.stringify({ argv: process.argv.slice(2), marker: process.env.MARKER }));`,
        "if (process.argv.includes('die')) process.kill(process.pid, 'SIGKILL');",
        "process.exit(7);",
      ].join("\n"),
    );
    const real = (command: string): ControllerLaunch => ({
      execPath: process.execPath,
      execArgv: [],
      argv: [process.execPath, entry, command, "--run-dir", "/runs/x"],
      env: { ...process.env, MARKER: "inherited" },
      signals: new EventEmitter(),
    });
    expect(await runFreshController(real("up"))).toBe(7);
    expect(JSON.parse(readFileSync(record, "utf8"))).toEqual({
      argv: ["up", "--run-dir", "/runs/x", "--no-build"],
      marker: "inherited",
    });
    expect(await runFreshController(real("die"))).toBe(
      128 + constants.signals.SIGKILL,
    );
  }, 20_000);
});
