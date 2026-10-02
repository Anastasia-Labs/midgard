import { existsSync, readFileSync } from "node:fs";

import { abortableSleep } from "./chaos-restore.js";
import type { DeployContext } from "./deploy.js";
import type { HubOracleOneShot } from "./node-env.js";
import { serviceSpecs, supervisorPaths } from "./services.js";
import {
  runningSupervisor,
  type ServiceReport,
  serviceReport,
  waitForServices,
} from "./stack.js";
import { DEFAULT_POLICY } from "./supervisor.js";

type Json = Record<string, unknown>;

const message = (error: unknown) =>
  error instanceof Error ? error.message : String(error);

/**
 * How long every service must stay running, with no supervisor event, before
 * a stack counts as recovered: the supervisor's own line for a stable
 * service. A service that exits sooner than `stableMs` after its start keeps
 * doubling its restart backoff (1 s up to 60 s), so each such loop exits, or
 * is down and then restarts, inside any window this long. A single readiness
 * snapshot is not enough: a crash-looping watcher answers ready for a while
 * after each restart's catch-up.
 */
export const STABLE_WINDOW_MS = DEFAULT_POLICY.stableMs;

/** One service's liveness and readiness, as `serviceReport` reads them. */
export type ServiceReadiness = Pick<
  ServiceReport,
  "name" | "pid" | "alive" | "ready" | "reasons"
>;

export type StabilityDeps = {
  readonly now: () => number;
  readonly sleep: (ms: number, signal?: AbortSignal) => Promise<void>;
  /**
   * Resolves once every service is alive and ready; throws after `timeoutMs`
   * unless `honourStartGrace` (the default) and every service still pending
   * is inside the start grace its spec grants. `graced` says the wait was
   * held open past `timeoutMs` that way.
   */
  readonly waitReady: (
    timeoutMs: number,
    options?: { readonly honourStartGrace?: boolean },
  ) => Promise<{ readonly graced: boolean }>;
  /** The supervisor's events so far, oldest first. */
  readonly supervisorEvents: () => readonly Json[];
  /** One probe of every supervised service, and of the supervisor. */
  readonly readiness: () => Promise<readonly ServiceReadiness[]>;
  /** Hears a progress line about once a minute while a window holds. */
  readonly progress?: (line: string) => void;
};

export type StableResult =
  | { readonly ok: true; readonly stableFrom: number; readonly graced: boolean }
  | { readonly ok: false; readonly detail: string; readonly backAt?: number };

const exitOf = (event: Json) =>
  typeof event.signal === "string"
    ? `signal ${event.signal}`
    : `code ${String(event.code)}`;

const describeEvent = (event: Json) => {
  const who =
    typeof event.service === "string" ? event.service : "the supervisor";
  if (event.event === "exit") return `${who} exited (${exitOf(event)})`;
  if (event.event === "start")
    return `${who} restarted (pid ${String(event.pid)})`;
  return `${who}: supervisor event ${String(event.event)}`;
};

const reasonsText = (reasons: unknown) =>
  (typeof reasons === "string"
    ? reasons
    : JSON.stringify(reasons ?? "not ready")
  ).slice(0, 200);

/**
 * Per service, the exits and restarts in `events` and the last readiness
 * reasons seen for it: what a failed recovery reports.
 */
const churn = (
  events: readonly Json[],
  lastReasons: ReadonlyMap<string, unknown>,
) => {
  const services = new Map<
    string,
    { exits: Map<string, number>; starts: number }
  >();
  const entry = (name: string) => {
    const found = services.get(name) ?? {
      exits: new Map<string, number>(),
      starts: 0,
    };
    services.set(name, found);
    return found;
  };
  for (const event of events) {
    if (typeof event.service !== "string") continue;
    const service = entry(event.service);
    if (event.event === "exit")
      service.exits.set(
        exitOf(event),
        (service.exits.get(exitOf(event)) ?? 0) + 1,
      );
    if (event.event === "start") service.starts += 1;
  }
  for (const name of lastReasons.keys()) entry(name);
  return [...services]
    .map(([name, { exits, starts }]) => {
      const exitCount = [...exits.values()].reduce((sum, n) => sum + n, 0);
      const parts = [
        exitCount === 0
          ? ""
          : `${exitCount} exits (${[...exits].map(([how, n]) => `${how} x${n}`).join(", ")})`,
        starts === 0 ? "" : `${starts} restarts`,
        lastReasons.has(name)
          ? `last not ready: ${reasonsText(lastReasons.get(name))}`
          : "",
      ].filter((part) => part !== "");
      return parts.length === 0 ? "" : `${name}: ${parts.join(", ")}`;
    })
    .filter((line) => line !== "")
    .join("; ");
};

const INTERRUPTED = Symbol("interrupted");

/** How often a holding window reports its progress. */
const PROGRESS_MS = 60_000;

const names = (reports: readonly ServiceReadiness[]) =>
  reports.map((report) => report.name).join(", ");

/**
 * Waits for every service to be alive and ready, then probes every `pollMs`
 * through a window of `windowMs`. Only crash-loop evidence restarts the
 * window: a supervisor event (an exit, a start, a hang), a PID change, or a
 * service (or the supervisor) that is not running. A running service that
 * answers unready inside the window does not, since the node's /readyz goes
 * unready on a healthy, busy stack (local finalization pending, a slow L1
 * provider); every service must be ready again at the window's end, waited
 * for within the bound. A restart keeps the bound (`deadline`) running; once
 * it has run out the recovery fails, naming each service's exits, restarts
 * and last readiness reasons. Only the first wait honours a service's start
 * grace (a long first catch-up): after a disruption the stack has to settle
 * inside the bound.
 */
export const awaitStable = async (
  deps: StabilityDeps,
  options: {
    readonly injectedAt: number;
    readonly deadline: number;
    readonly boundMs: number;
    readonly windowMs: number;
    readonly pollMs?: number;
    readonly signal?: AbortSignal;
  },
): Promise<StableResult> => {
  const {
    deadline,
    windowMs,
    signal,
    pollMs = DEFAULT_POLICY.probeIntervalMs,
  } = options;
  const bound = `${options.boundMs / 1000} s`;
  const window = `${windowMs / 1000} s`;
  const firstEvent = deps.supervisorEvents().length;
  const lastReasons = new Map<string, unknown>();
  let disruptions = 0;
  let lastDisruption = "";
  let honourStartGrace = true;
  const summary = () => {
    const lines = churn(deps.supervisorEvents().slice(firstEvent), lastReasons);
    const restarted =
      disruptions === 0
        ? ""
        : `the stable window restarted ${disruptions} times, last because ${lastDisruption}`;
    return [restarted, lines].filter((part) => part !== "").join("; ");
  };
  const withSummary = (head: string) =>
    [head, summary()].filter((part) => part !== "").join("; ");
  const interrupted = (): StableResult => ({
    ok: false,
    detail: withSummary(`interrupted before the stack stayed up for ${window}`),
  });

  /** Undefined once the window holds; otherwise what broke it. */
  const hold = async (
    from: number,
  ): Promise<string | typeof INTERRUPTED | undefined> => {
    const mark = deps.supervisorEvents().length;
    let pids: ReadonlyMap<string, number | undefined> | undefined;
    let rewaited = false;
    let reported = from;
    for (;;) {
      const reports = await deps.readiness();
      for (const report of reports)
        if (!report.alive || report.ready === false)
          lastReasons.set(
            report.name,
            report.alive ? report.reasons : "not running",
          );
      const event = deps.supervisorEvents().slice(mark)[0];
      if (event !== undefined) return describeEvent(event);
      const gone = reports.filter((report) => !report.alive);
      if (gone.length > 0)
        return gone.map((r) => `${r.name} is not running`).join("; ");
      const moved = reports.find(
        (r) => pids !== undefined && pids.get(r.name) !== r.pid,
      );
      if (moved !== undefined)
        return `${moved.name} changed PID ${pids?.get(moved.name)} -> ${moved.pid}`;
      pids = new Map(reports.map((report) => [report.name, report.pid]));
      const unready = reports.filter((report) => report.ready === false);
      const held = deps.now() - from;
      if (held >= windowMs) {
        if (unready.length === 0) return undefined;
        if (rewaited) return `${names(unready)} not ready at the window's end`;
        // Busy, not crashed: wait (within the bound) for ready, then
        // re-probe for anything that restarted meanwhile.
        rewaited = true;
        try {
          await deps.waitReady(Math.max(1, deadline - deps.now()), {
            honourStartGrace,
          });
        } catch (error) {
          return `not ready at the window's end: ${message(error).slice(0, 300)}`;
        }
        continue;
      }
      if (signal?.aborted === true) return INTERRUPTED;
      if (deps.now() - reported >= PROGRESS_MS) {
        reported = deps.now();
        deps.progress?.(
          `services: up with no restart for ${Math.round(held / 1000)} of ${window}${unready.length === 0 ? "" : `; running but unready: ${unready.map((r) => `${r.name} ${reasonsText(r.reasons)}`).join("; ")}`}`,
        );
      }
      await deps.sleep(Math.min(pollMs, windowMs - held), signal);
    }
  };

  for (;;) {
    if (signal?.aborted === true) return interrupted();
    let graced: boolean;
    try {
      ({ graced } = await deps.waitReady(Math.max(1, deadline - deps.now()), {
        honourStartGrace,
      }));
    } catch (error) {
      return {
        ok: false,
        detail: withSummary(
          `not recovered within ${bound}: ${message(error).slice(0, 500)}`,
        ),
      };
    }
    const readyAt = deps.now();
    if (readyAt > deadline && !graced)
      return {
        ok: false,
        backAt: readyAt,
        detail: withSummary(
          `back after ${Math.round((readyAt - options.injectedAt) / 1000)} s, over the ${bound} bound`,
        ),
      };
    const broke = await hold(readyAt);
    if (broke === undefined)
      return { ok: true, stableFrom: readyAt, graced: readyAt > deadline };
    if (broke === INTERRUPTED) return interrupted();
    disruptions += 1;
    lastDisruption = broke;
    if (deps.now() >= deadline)
      return {
        ok: false,
        detail: withSummary(
          `not stably ready within ${bound} (ready, then running with no exit, restart or PID change for ${window}, and ready again at its end)`,
        ),
      };
    honourStartGrace = false;
  }
};

/** Parses one JSON line; a torn or empty line yields nothing. */
export const parseJsonLine = (text: string): Json[] => {
  try {
    return text === "" ? [] : [JSON.parse(text) as Json];
  } catch {
    return [];
  }
};

/** The supervisor's event log, oldest first; a torn last line is skipped. */
export const readSupervisorEvents =
  (context: DeployContext) => (): readonly Json[] => {
    const { events } = supervisorPaths(context);
    return existsSync(events)
      ? readFileSync(events, "utf8").split("\n").flatMap(parseJsonLine)
      : [];
  };

const alive = (pid: number) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

/**
 * Probes every supervised service once. A dead supervisor reads as a service
 * that is not running: nothing would restart the others.
 */
export const serviceReadiness = (
  context: DeployContext,
  oneShot: HubOracleOneShot,
  supervisorPid?: number,
) => {
  let specs: ReturnType<typeof serviceSpecs> | undefined;
  return async (): Promise<readonly ServiceReadiness[]> => {
    specs ??= serviceSpecs(context, oneShot);
    const supervisor = supervisorPid ?? runningSupervisor(context.layout);
    const reports = await Promise.all(
      specs.map((spec) => serviceReport(context.layout, spec)),
    );
    return supervisor !== undefined && alive(supervisor)
      ? reports
      : [...reports, { name: "supervisor", alive: false }];
  };
};

/**
 * `up`'s readiness: every service ready and then stable for `windowMs`, so
 * `up` never reports a stack in a restart loop as ready.
 */
export const waitStablyReady = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
  timeoutMs: number,
  options: { readonly supervisorPid?: number; readonly windowMs?: number } = {},
) => {
  const deps: StabilityDeps = {
    now: Date.now,
    sleep: abortableSleep,
    progress: console.log,
    waitReady: async (ms, { honourStartGrace = true } = {}) => {
      let graced = false;
      await waitForServices(context, oneShot, ms, {
        honourStartGrace,
        ...(options.supervisorPid === undefined
          ? {}
          : { supervisorPid: options.supervisorPid }),
        onGrace: () => (graced = true),
      });
      return { graced };
    },
    supervisorEvents: readSupervisorEvents(context),
    readiness: serviceReadiness(context, oneShot, options.supervisorPid),
  };
  const startedAt = Date.now();
  const result = await awaitStable(deps, {
    injectedAt: startedAt,
    deadline: startedAt + timeoutMs,
    boundMs: timeoutMs,
    windowMs: options.windowMs ?? STABLE_WINDOW_MS,
  });
  if (!result.ok) throw new Error(`services: ${result.detail}`);
};
