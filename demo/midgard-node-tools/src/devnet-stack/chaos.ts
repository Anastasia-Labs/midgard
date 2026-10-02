import { appendFileSync, mkdirSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";

import { waitL1Ready } from "./chain.js";
import {
  type Drill,
  drillCatalogue,
  type Json,
  section,
  selectDrills,
  type Trigger,
  type TriggerEndpoint,
  TRIGGERS,
  WATCHER_SERVICE,
} from "./chaos.drill-catalogue.js";
import {
  assertComposeAction,
  composeAction,
  type ComposeDeps,
  composeDeps,
  finishOwedRestore,
  type OwedRestore,
  owedRestorePath,
  recordOwedRestore,
  restore,
} from "./chaos-restore.js";
import {
  awaitStable,
  parseJsonLine,
  readSupervisorEvents,
  serviceReadiness,
  type StabilityDeps,
  STABLE_WINDOW_MS,
} from "./chaos-stable.js";
import type { DeployContext } from "./deploy.js";
import { readJsonIfPresent } from "./durable.js";
import { servicePorts } from "./layout.js";
import type { HubOracleOneShot } from "./node-env.js";
import { waitForServices } from "./stack.js";
import { SERVICE_MARKER_ENV } from "./supervisor.js";

export {
  type Drill,
  drillCatalogue,
  type Json,
  selectDrills,
  type Trigger,
  type TriggerEndpoint,
  TRIGGERS,
  WATCHER_SERVICE,
};

const message = (error: unknown) =>
  error instanceof Error ? error.message : String(error);

export type { L1Service } from "./chaos-restore.js";

/**
 * Everything a drill does to the outside world. `waitReady` waits for L1 too;
 * a start grace is a startup catch-up the supervisor counts healthy.
 */
export type ChaosDeps = ComposeDeps &
  StabilityDeps & {
    /** Signals a process group (the service is its leader). */
    readonly signalGroup: (pid: number, signal: NodeJS.Signals) => void;
    /** The process's NUL-separated environment, or undefined when unreadable. */
    readonly environ: (pid: number) => string | undefined;
    /** GET on the node's HTTP API: the JSON body whatever the status, or {}. */
    readonly fetchNode: (path: TriggerEndpoint) => Promise<Json>;
  };

const signalGroup = (pid: number, signal: NodeJS.Signals) => {
  try {
    process.kill(-pid, signal);
  } catch {
    process.kill(pid, signal);
  }
};

const readEnviron = (pid: number) => {
  try {
    return readFileSync(`/proc/${pid}/environ`, "utf8");
  } catch {
    return undefined;
  }
};

/** Production dependencies for a deployed, supervised run. */
export const productionChaosDeps = (
  context: DeployContext,
  oneShot: HubOracleOneShot,
): ChaosDeps => {
  const { layout, run } = context;
  const node = `http://127.0.0.1:${servicePorts(run).nodeHttp}`;
  return {
    ...composeDeps(layout, run),
    signalGroup,
    environ: readEnviron,
    fetchNode: async (path) => {
      const response = await fetch(`${node}${path}`, {
        signal: AbortSignal.timeout(5_000),
      });
      return parseJsonLine(await response.text())[0] ?? {};
    },
    now: Date.now,
    waitReady: async (timeoutMs, { honourStartGrace = true } = {}) => {
      const deadline = Date.now() + timeoutMs;
      await waitL1Ready(run, timeoutMs);
      let graced = false;
      // A killed watcher answers its readiness URL only after its startup
      // catch-up, which may outlast the recovery bound.
      await waitForServices(
        context,
        oneShot,
        Math.max(1, deadline - Date.now()),
        {
          honourStartGrace,
          onGrace: () => (graced = true),
        },
      );
      return { graced };
    },
    supervisorEvents: readSupervisorEvents(context),
    readiness: serviceReadiness(context, oneShot),
    progress: console.log,
  };
};

export type DrillRecord = {
  readonly drill: string;
  readonly target: string;
  readonly injectedAt: string | null;
  readonly recoveredAt: string | null;
  readonly ok: boolean;
  /** Not injected: its trigger never held, or the run was told to stop. */
  readonly skipped?: boolean;
  readonly detail: string;
};

const summarise = (records: readonly DrillRecord[]) => {
  const failures = records.filter((r) => !r.ok);
  const skipped = records.filter((r) => r.skipped === true);
  return {
    records,
    failures,
    skipped,
    /** Every record is exactly one of these. */
    counts: {
      recovered: records.length - failures.length - skipped.length,
      skipped: skipped.length,
      failed: failures.length,
    },
  };
};

export type DrillSummary = ReturnType<typeof summarise>;

export type RunDrillsOptions = {
  readonly drills: readonly Drill[];
  /** The run directory; with the service name it forms the child's marker. */
  readonly runDir: string;
  /** The supervisor's PID directory (`<state>/services`). */
  readonly pidDir: string;
  /** One JSON line per drill is appended here. */
  readonly drillsLog: string;
  readonly deps: ChaosDeps;
  /** Pause between two drills. */
  readonly gapMs?: number;
  /** Passes over `drills`; Infinity cycles until `shouldStop`. */
  readonly rounds?: number;
  /** Bound on every service being alive and ready again. */
  readonly recoveryMs?: number;
  /** How long the stack must then stay ready with no restart to count as recovered. */
  readonly stableMs?: number;
  /** Bound on a targeted kill's trigger holding. */
  readonly triggerTimeoutMs?: number;
  readonly pollMs?: number;
  readonly shouldStop?: () => boolean | Promise<boolean>;
  readonly signal?: AbortSignal;
};

/**
 * Kills only a process that carries this run's marker for `service`, so a
 * stale PID file or a recycled PID is never signalled.
 */
export const ownedPid = (
  deps: Pick<ChaosDeps, "environ">,
  { runDir, pidDir }: { runDir: string; pidDir: string },
  service: string,
): { pid: number } | { refused: string } => {
  const pid = readJsonIfPresent<{ pid?: number }>(
    join(pidDir, `${service}.json`),
  )?.pid;
  if (pid === undefined || !Number.isSafeInteger(pid) || pid <= 0)
    return { refused: `no PID file for ${service}` };
  const marker = `${SERVICE_MARKER_ENV}=${runDir}#${service}`;
  if (deps.environ(pid)?.split("\0").includes(marker) !== true)
    return {
      refused: `PID ${pid} is not this run's ${service}; not signalled`,
    };
  return { pid };
};

type Outcome = Omit<DrillRecord, "drill" | "target">;

/**
 * Runs the drills one after another while the journey runs, each followed by
 * a bounded recovery check. Returns every record; the caller fails the run
 * when `failures` is non-empty. A stopped or paused L1 service is restored
 * before this returns, even when a wait throws or `signal` aborts, and a
 * restore a killed previous run left owed is done first.
 */
export const runDrills = async (
  options: RunDrillsOptions,
): Promise<DrillSummary> => {
  const { deps, signal, gapMs = 120_000, rounds = 1, pollMs = 250 } = options;
  const {
    recoveryMs = 15 * 60_000,
    triggerTimeoutMs = 20 * 60_000,
    stableMs = STABLE_WINDOW_MS,
  } = options;
  // A NaN or zero bound (a mistyped CLI number) would pass a run that injected nothing.
  const bounds = [
    rounds,
    recoveryMs,
    triggerTimeoutMs,
    stableMs,
    ...options.drills.map((d) => (d.kind === "outage" ? d.outageMs : 1)),
  ];
  if (!bounds.every((n) => n > 0))
    throw new Error(
      "runDrills needs rounds, recovery, stable, trigger and outage bounds above 0",
    );
  const iso = (ms: number) => new Date(ms).toISOString();
  const records: DrillRecord[] = [];
  let halted = false;
  const record = (drill: string, target: string, result: Outcome) => {
    const line: DrillRecord = { drill, target, ...result };
    records.push(line);
    mkdirSync(dirname(options.drillsLog), { recursive: true, mode: 0o700 });
    appendFileSync(
      options.drillsLog,
      `${JSON.stringify({ at: iso(deps.now()), ...line })}\n`,
    );
  };
  const stopping = async () =>
    signal?.aborted === true || (await options.shouldStop?.()) === true;
  const outcome = (
    ok: boolean,
    detail: string,
    injectedAt?: number,
    recoveredAt?: number,
  ) => ({
    injectedAt: injectedAt === undefined ? null : iso(injectedAt),
    recoveredAt: recoveredAt === undefined ? null : iso(recoveredAt),
    ok,
    detail,
  });
  const failed = (injectedAt: number | null, detail: string) =>
    outcome(false, detail, injectedAt ?? undefined);

  /**
   * Everything alive and ready within what is left of the bound, then still
   * running with no restart for `stableMs` and ready at its end: a crash loop
   * answers ready between its restarts. The recovery is dated to the start
   * of that window.
   */
  const recover = async (
    injectedAt: number,
    deadline: number,
    what: string,
  ): Promise<Outcome> => {
    const result = await awaitStable(deps, {
      injectedAt,
      deadline,
      boundMs: recoveryMs,
      windowMs: stableMs,
      signal,
    });
    if (!result.ok)
      return outcome(
        false,
        `${what}; ${result.detail}`,
        injectedAt,
        result.backAt,
      );
    const at = result.stableFrom;
    const took = `${Math.round((at - injectedAt) / 1000)} s`;
    const held = `; stayed up with no restart for ${stableMs / 1000} s`;
    return at <= deadline
      ? outcome(true, `${what}; recovered in ${took}${held}`, injectedAt, at)
      : outcome(
          true,
          `${what}; recovered in ${took}, past the ${recoveryMs / 1000} s bound inside a service's start grace${held}`,
          injectedAt,
          at,
        );
  };

  const awaitTrigger = async (trigger: Trigger): Promise<Json | string> => {
    const deadline = deps.now() + triggerTimeoutMs;
    while (deps.now() < deadline) {
      if (await stopping()) return "stopped before the trigger held";
      try {
        const body = await deps.fetchNode(trigger.endpoint);
        if (trigger.holds(body)) return body;
      } catch {
        // The node is not answering; keep polling.
      }
      await deps.sleep(pollMs, signal);
    }
    return `trigger "${trigger.description}" did not hold within ${triggerTimeoutMs / 1000} s`;
  };

  const kill = async (
    drill: Extract<Drill, { kind: "kill" }>,
  ): Promise<Outcome> => {
    let observed = "";
    if (drill.trigger !== undefined) {
      const seen = await awaitTrigger(drill.trigger);
      if (typeof seen === "string")
        return { ...outcome(true, seen), skipped: true };
      const queue = section(seen, "stateQueue");
      observed = ` at "${drill.trigger.description}"${typeof queue.unconfirmedSubmittedBlockTxHash === "string" ? ` (block tx ${queue.unconfirmedSubmittedBlockTxHash})` : ""}`;
    }
    const owned = ownedPid(deps, options, drill.service);
    if ("refused" in owned) return failed(null, `refused: ${owned.refused}`);
    const eventsBefore = deps.supervisorEvents().length;
    const injectedAt = deps.now();
    deps.signalGroup(owned.pid, "SIGKILL");
    const deadline = injectedAt + recoveryMs;
    const what = `SIGKILL process group ${owned.pid}${observed}`;
    // The supervisor must restart it: readiness alone could be a survivor.
    for (;;) {
      const restarted = deps
        .supervisorEvents()
        .slice(eventsBefore)
        .find(
          (e) =>
            e.event === "start" &&
            e.service === drill.service &&
            e.pid !== owned.pid,
        );
      if (restarted !== undefined) break;
      if (deps.now() >= deadline)
        return failed(
          injectedAt,
          `${what}; the supervisor recorded no new start of ${drill.service} within ${recoveryMs / 1000} s`,
        );
      if (signal?.aborted)
        return failed(injectedAt, `${what}; interrupted before the restart`);
      await deps.sleep(Math.max(pollMs, 1_000), signal);
    }
    return recover(injectedAt, deadline, what);
  };

  /**
   * A stop, pause or restart of an L1 container. A restart owes a `start` too:
   * a run killed mid-restart must not leave the container stopped.
   */
  const outage = async (
    drill: Exclude<Drill, { kind: "kill" }>,
  ): Promise<Outcome> => {
    const verb = drill.kind === "restart" ? "restart" : drill.mode;
    // Refused before anything is recorded as owed.
    assertComposeAction(verb, drill.service);
    const owed: OwedRestore = {
      drill: drill.name,
      service: drill.service,
      undo: verb === "pause" ? "unpause" : "start",
    };
    recordOwedRestore(options.drillsLog, owed);
    const injectedAt = deps.now();
    let injectError: unknown;
    let restoreError: unknown;
    try {
      await composeAction(deps, verb, drill.service);
      if (drill.kind === "outage") await deps.sleep(drill.outageMs, signal);
    } catch (error) {
      injectError = error;
    } finally {
      try {
        await restore(deps, options.drillsLog, owed);
      } catch (error) {
        restoreError = error;
      }
    }
    const what =
      drill.kind === "restart"
        ? `restart ${drill.service}`
        : `${drill.mode} ${drill.service} for ${Math.round((deps.now() - injectedAt) / 1000)} s`;
    // Chaos stops while a dependency this run took down may still be down.
    if (restoreError !== undefined) halted = true;
    const problems = [
      injectError === undefined
        ? ""
        : `injection failed: ${message(injectError)}`,
      restoreError === undefined
        ? ""
        : `RESTORE FAILED, ${owedRestorePath(options.drillsLog)} records it: ${message(restoreError)}`,
    ].filter((problem) => problem !== "");
    if (problems.length > 0)
      return failed(injectedAt, `${what}; ${problems.join("; ")}`);
    return recover(injectedAt, injectedAt + recoveryMs, what);
  };

  const run = (drill: Drill): Promise<Outcome> =>
    drill.kind === "kill" ? kill(drill) : outage(drill);

  const leftover = readJsonIfPresent<OwedRestore>(
    owedRestorePath(options.drillsLog),
  );
  if (leftover !== undefined) {
    const what = `a previous drill run stopped mid-outage; ${leftover.undo} ${leftover.service}`;
    try {
      await finishOwedRestore(deps, options.drillsLog);
      record(leftover.drill, leftover.service, {
        ...failed(null, what),
        ok: true,
      });
    } catch (error) {
      record(
        leftover.drill,
        leftover.service,
        failed(null, `${what} failed: ${message(error)}`),
      );
      return summarise(records);
    }
  }

  for (let round = 0; round < rounds; round += 1)
    for (const [index, drill] of options.drills.entries()) {
      if (round + index > 0) {
        const gapEnd = deps.now() + gapMs;
        while (deps.now() < gapEnd && !(await stopping()))
          await deps.sleep(Math.min(5_000, gapEnd - deps.now()), signal);
      }
      if (halted || (await stopping())) return summarise(records);
      // An injection into a stack that is not healthy proves nothing.
      const preflight = await recover(
        deps.now(),
        deps.now() + recoveryMs,
        "preflight",
      );
      if (!preflight.ok) {
        record(
          drill.name,
          drill.service,
          failed(null, `not injected: ${preflight.detail}`),
        );
        return summarise(records);
      }
      try {
        record(drill.name, drill.service, await run(drill));
      } catch (error) {
        record(
          drill.name,
          drill.service,
          failed(null, message(error).slice(0, 1_000)),
        );
      }
    }
  return summarise(records);
};

/** True once the journey's last phase is journaled done. */
export const journeyFinished = (journeyDir: string) =>
  readJsonIfPresent<{ entries?: Record<string, unknown> }>(
    join(journeyDir, "journey.json"),
  )?.entries?.["phase:holdings"] === "done";
