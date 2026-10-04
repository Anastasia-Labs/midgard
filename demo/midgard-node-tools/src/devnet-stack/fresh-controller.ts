import {
  type ChildProcess,
  spawn,
  type SpawnOptions,
} from "node:child_process";
import { appendFileSync, existsSync, mkdirSync } from "node:fs";
import { constants } from "node:os";
import { dirname, join } from "node:path";

import type { Command } from "commander";

import {
  buildEverything,
  buildInputs,
  pendingBuild,
  validateEnvironment,
} from "./build.js";
import {
  codeStamp,
  requireFreshDists,
  runtimeDistTargets,
} from "./dist-freshness.js";
import type { Layout } from "./layout.js";
import { acquireLock, lockOwner } from "./lock.js";
import { supervisorPaths } from "./services.js";
import {
  runningSupervisor,
  stopSupervisor,
  SUPERVISOR_STOP_MS,
} from "./stack.js";
import { DEFAULT_POLICY, sweepOrphans } from "./supervisor.js";

export type ControllerChild = Pick<ChildProcess, "kill" | "once">;

export type Spawner = (
  command: string,
  args: readonly string[],
  options: SpawnOptions,
) => ControllerChild;

type SignalSource = {
  on(signal: NodeJS.Signals, listener: () => void): unknown;
  off(signal: NodeJS.Signals, listener: () => void): unknown;
};

/** How this controller was started, and how to start another like it. */
export type ControllerLaunch = {
  readonly execPath: string;
  readonly execArgv: readonly string[];
  /** `process.argv`: the Node binary, the controller entry, then the command line. */
  readonly argv: readonly string[];
  readonly env: NodeJS.ProcessEnv;
  readonly spawner?: Spawner;
  /** Where SIGINT, SIGTERM and SIGHUP arrive; `process` in production. */
  readonly signals: SignalSource;
};

const FORWARDED_SIGNALS = ["SIGINT", "SIGTERM", "SIGHUP"] as const;

/** The same entry and command line, with the build skipped. */
export const noBuildArgs = (
  launch: Pick<ControllerLaunch, "execArgv" | "argv">,
): string[] => [...launch.execArgv, ...launch.argv.slice(1), "--no-build"];

/**
 * Runs the rest of the command in a new controller process started from the
 * entry on disk, with this process's stdio and environment, and resolves to
 * its exit status. SIGINT, SIGTERM and SIGHUP are passed on to it; a child
 * killed by a signal resolves to 128 + the signal number, never to success.
 */
export const runFreshController = (launch: ControllerLaunch): Promise<number> =>
  new Promise((resolve, reject) => {
    const child = (launch.spawner ?? spawn)(
      launch.execPath,
      noBuildArgs(launch),
      {
        stdio: "inherit",
        env: launch.env,
      },
    );
    const forwarders = FORWARDED_SIGNALS.map((signal) => {
      const forward = () => {
        child.kill(signal);
      };
      launch.signals.on(signal, forward);
      return () => launch.signals.off(signal, forward);
    });
    const detach = () => {
      for (const stop of forwarders) stop();
    };
    child.once("error", (error) => {
      detach();
      reject(error);
    });
    child.once("exit", (code, signal) => {
      detach();
      if (signal === null) {
        resolve(code ?? 1);
        return;
      }
      console.error(`up: the rebuilt controller was killed by ${signal}`);
      resolve(128 + (constants.signals[signal] ?? 0));
    });
  });

/** The run's locks besides the controller lock and the supervisor's. */
export const runLockPaths = (layout: { runDir: string; state: string }) => ({
  /**
   * Held by a building `up` from its build decision to the end of the build.
   * Beside the run directory, not in it: a fresh run's directory does not
   * exist until chain generation, which moves any earlier content aside.
   */
  build: `${layout.runDir}.build.lock`,
  journey: join(layout.state, "journey.lock"),
  drill: join(layout.state, "drill.lock"),
});

export type LiveProcess = { readonly name: string; readonly pid: number };

/** The live `journey` and `drill` processes of a run. */
export const liveRunUsers = (
  locks: Pick<ReturnType<typeof runLockPaths>, "journey" | "drill">,
): LiveProcess[] =>
  (["journey", "drill"] as const).flatMap((name) => {
    const pid = lockOwner(locks[name]);
    return pid === undefined ? [] : [{ name, pid }];
  });

/**
 * `journey` and `drill` load the dists once and run for hours, so neither
 * starts while an `up` may rebuild them. Called holding the command's own
 * lock: a building `up` checks those locks holding the build lock, so one of
 * the two always sees the other.
 */
export const refuseWhileBuilding = (buildLock: string, command: string) => {
  const owner = lockOwner(buildLock);
  if (owner !== undefined)
    throw new Error(
      `${command} refuses to start while up (pid ${owner}) may rebuild this run's dists (${buildLock}); start it once that up has built`,
    );
};

/**
 * The start of a `journey` or `drill`: takes the command's own lock, then
 * refuses while a building `up` holds the build lock, or when the dists
 * changed since the command first stamped them (a build ended in between;
 * this process may have loaded the code it replaced), or are older than
 * their sources.
 */
export const startRunUser = (
  layout: Layout,
  name: "journey" | "drill",
  stamp: () => string = () => codeStamp(runtimeDistTargets(layout)),
): void => {
  const locks = runLockPaths(layout);
  const loaded = stamp();
  acquireLock(locks[name]);
  refuseWhileBuilding(locks.build, name);
  if (stamp() !== loaded)
    throw new Error(
      `${name}: the runtime dists changed while it started (a build ended); start it again`,
    );
  requireFreshDists(layout, name);
};

/** The code a controller loaded when it started. */
export type LoadedCode = {
  /** The dists' code stamp then; the supervisor's spec digest compares it. */
  readonly stamp: string;
  /**
   * Throws when the dists have changed since. Called once the controller
   * lock is held: from then on no building `up` can rewrite them, and before
   * it another one could have.
   */
  readonly assertUnchanged: () => void;
};

export type UpDeps = {
  /** The code stamp of the runtime dists on disk (dist-freshness.ts). */
  readonly codeStamp: () => string;
  readonly validateEnvironment: () => Promise<void>;
  /** Why a build is needed, one line per reason; empty when none is. */
  readonly pendingBuild: () => Promise<readonly string[]>;
  readonly buildEverything: () => Promise<void>;
  readonly buildLockPath: string;
  /** The run's controller lock, in its state directory. */
  readonly lockPath: string;
  /** The run's live `journey` and `drill` processes. */
  readonly runUsers: () => readonly LiveProcess[];
  readonly runningSupervisor: () => number | undefined;
  /** Stops the supervisor and every service, gracefully. */
  readonly stopSupervisor: () => Promise<unknown>;
  /** Stops the services a supervisor that died uncleanly left running. */
  readonly sweepOrphans: () => Promise<void>;
  /** Refuses dists older than their sources. */
  readonly requireFreshDists: () => void;
  /** Everything `up` does after the build, on the code this process loaded. */
  readonly resume: (code: LoadedCode) => Promise<void>;
};

const errorMessage = (error: unknown) =>
  error instanceof Error ? error.message : String(error);

/**
 * Builds if any build output is stale, and resolves to whether it did. The
 * decision and the build hold the build lock. A build rewrites the dists
 * under every process that loaded them, so it refuses while a journey or
 * drill of the run is live, holds the controller lock (refusing while
 * another `up` holds it) and stops a live supervisor, and with it every
 * service, before it starts; with none live, it stops any service a dead
 * supervisor left behind. A fresh run has no state directory, so nothing of
 * the run can be live and nothing is written into it.
 */
const buildIfStale = async (deps: UpDeps): Promise<boolean> => {
  mkdirSync(dirname(deps.buildLockPath), { recursive: true, mode: 0o700 });
  const releaseBuild = acquireLock(deps.buildLockPath);
  try {
    const pending = await deps.pendingBuild();
    if (pending.length === 0) {
      console.log("build: every build output is current; nothing to build");
      return false;
    }
    console.log(`build: needed:\n  ${pending.join("\n  ")}`);
    const users = deps.runUsers();
    if (users.length > 0)
      throw new Error(
        `up refuses to build while ${users.map((user) => `${user.name} (pid ${user.pid})`).join(" and ")} runs on this run: the build rewrites the dists it runs. Let it finish or stop it, or run up --no-build`,
      );
    const releaseController = existsSync(dirname(deps.lockPath))
      ? acquireLock(deps.lockPath)
      : () => {};
    try {
      const supervisor = deps.runningSupervisor();
      if (supervisor !== undefined) {
        console.log(
          `services: stopping supervisor ${supervisor} before the build rewrites the dists its services run`,
        );
        await deps.stopSupervisor();
      } else await deps.sweepOrphans();
      try {
        await deps.buildEverything();
      } catch (error) {
        const stopped =
          supervisor === undefined
            ? ""
            : `; the services stay stopped (supervisor ${supervisor} was stopped before the build)`;
        throw new Error(
          `build failed${stopped}; run up again to retry: ${errorMessage(error)}`,
          { cause: error },
        );
      }
    } finally {
      releaseController();
    }
    return true;
  } finally {
    releaseBuild();
  }
};

/**
 * `up`. A build rewrites the dists this process has already loaded into its
 * module cache, including the controller's own bundle, and a module imported
 * later (the watcher dist) would resolve its dependencies to the stale cached
 * copies. So a building `up` never continues itself once it has built: it
 * hands the rest to a fresh controller with --no-build. When nothing is
 * stale it builds nothing, stops nothing and continues itself, on code that
 * matches the disk. Resolves to the exit status to report.
 */
export const runUp = async (
  build: boolean,
  deps: UpDeps,
  launch: ControllerLaunch,
): Promise<number> => {
  // First: the stamp of the code this process loaded at start.
  const stamp = deps.codeStamp();
  await deps.validateEnvironment();
  if (build && (await buildIfStale(deps))) return runFreshController(launch);
  deps.requireFreshDists();
  await deps.resume({
    stamp,
    assertUnchanged: () => {
      if (deps.codeStamp() !== stamp)
        throw new Error(
          "up: the runtime dists changed after this controller loaded them (another build ran); run up again",
        );
    },
  });
  return 0;
};

/**
 * The head of `up` after the build, in the controller that resumes: takes
 * the controller lock, then refuses if a build rewrote the dists after this
 * process loaded them (`code`), before anything else runs on them.
 */
export const holdRunForResume = (
  layout: Pick<Layout, "state" | "lock">,
  code: LoadedCode,
): void => {
  mkdirSync(layout.state, { recursive: true, mode: 0o700 });
  acquireLock(layout.lock);
  code.assertUnchanged();
};

/** The production steps of `up` on `layout`; `resume` runs after the build. */
export const upDeps = (layout: Layout, resume: UpDeps["resume"]): UpDeps => {
  // Outside the run directory: a fresh run's does not exist yet.
  const controllerLogs = `${layout.runDir}.controller-logs`;
  const locks = runLockPaths(layout);
  return {
    codeStamp: () => codeStamp(runtimeDistTargets(layout)),
    validateEnvironment: () => validateEnvironment(controllerLogs),
    pendingBuild: () => pendingBuild(buildInputs(layout, controllerLogs)),
    buildEverything: () => buildEverything(layout, controllerLogs),
    buildLockPath: locks.build,
    lockPath: layout.lock,
    runUsers: () => liveRunUsers(locks),
    runningSupervisor: () => runningSupervisor(layout),
    stopSupervisor: () => stopSupervisor(layout, SUPERVISOR_STOP_MS),
    sweepOrphans: () =>
      sweepOrphans(supervisorPaths({ layout }), DEFAULT_POLICY, (event) => {
        mkdirSync(dirname(layout.supervisorEvents), { recursive: true });
        appendFileSync(
          layout.supervisorEvents,
          `${JSON.stringify({ at: new Date().toISOString(), by: "up", ...event })}\n`,
        );
      }),
    requireFreshDists: () => requireFreshDists(layout, "up"),
    resume,
  };
};

export type UpOptions = {
  readonly runDir: string;
  readonly build: boolean;
  readonly readyTimeoutMs: number;
  readonly initializeWatcherAuthority?: boolean;
};

/**
 * Declares `up` on `program`. `wire` builds the steps for the parsed
 * options; `exit` receives the exit status.
 */
export const registerUp = (
  program: Command,
  wire: (options: UpOptions) => { deps: UpDeps; launch: ControllerLaunch },
  exit: (code: number) => void = (code) => {
    process.exitCode = code;
  },
) =>
  program
    .command("up")
    .description(
      "Build what is stale, generate, fund, deploy and start everything; resumes a partial run and never replaces its identity",
    )
    .requiredOption(
      "--run-dir <path>",
      "Absolute run directory (created on first use)",
    )
    .option(
      "--initialize-watcher-authority",
      "Explicitly initialize only a new owned watcher authority, or retry its retained pending attempt",
    )
    .option("--no-build", "Skip the dependency, contract and runtime build")
    .option(
      "--ready-timeout <seconds>",
      "How long to wait for the services to become ready",
      "1200",
    )
    .action(
      async (options: {
        runDir: string;
        build: boolean;
        readyTimeout: string;
        initializeWatcherAuthority?: boolean;
      }) => {
        const up: UpOptions = {
          runDir: options.runDir,
          build: options.build,
          readyTimeoutMs: Number(options.readyTimeout) * 1000,
          ...(options.initializeWatcherAuthority
            ? { initializeWatcherAuthority: true }
            : {}),
        };
        const { deps, launch } = wire(up);
        exit(await runUp(up.build, deps, launch));
      },
    );
