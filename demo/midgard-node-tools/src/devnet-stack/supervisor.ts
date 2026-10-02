import { type ChildProcess, spawn } from "node:child_process";
import {
  appendFileSync,
  closeSync,
  existsSync,
  mkdirSync,
  openSync,
  readdirSync,
  readFileSync,
  unlinkSync,
} from "node:fs";
import { join } from "node:path";

import { writeDurableJson } from "./durable.js";

/** One long-running process the supervisor keeps alive. */
export type ServiceSpec = {
  readonly name: string;
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
  readonly env: Readonly<Record<string, string>>;
  /** Liveness URL; a service that stops answering it is restarted. */
  readonly healthUrl?: string;
  /** Readiness URL, reported by status and waited on by `up`. */
  readonly readyUrl?: string;
  /** Overrides the policy's start grace for a service with a long startup. */
  readonly startGraceMs?: number;
  /** Must resolve true before each start; retried until it does. */
  readonly prestart?: () => Promise<boolean>;
};

export type SupervisorPolicy = {
  /** How long a service may take to answer its liveness URL after a start. */
  readonly startGraceMs: number;
  readonly probeIntervalMs: number;
  readonly probeTimeoutMs: number;
  /** Continuous liveness failure after which a service counts as hung. */
  readonly hangMs: number;
  /** Grace between SIGTERM and SIGKILL. */
  readonly stopGraceMs: number;
  readonly initialBackoffMs: number;
  readonly maxBackoffMs: number;
  /** Uptime after which the restart backoff resets. */
  readonly stableMs: number;
  /** Delay between prestart checks that are not yet satisfied. */
  readonly prestartRetryMs: number;
};

export const DEFAULT_POLICY: SupervisorPolicy = {
  startGraceMs: 10 * 60_000,
  probeIntervalMs: 10_000,
  probeTimeoutMs: 5_000,
  hangMs: 3 * 60_000,
  stopGraceMs: 60_000,
  initialBackoffMs: 1_000,
  maxBackoffMs: 60_000,
  stableMs: 5 * 60_000,
  prestartRetryMs: 5_000,
};

/** Marks a child as this run's service, so a later supervisor can find it. */
export const SERVICE_MARKER_ENV = "MIDGARD_DEVNET_SERVICE";

/**
 * Resolves after `ms`, or as soon as `signal` aborts. The abort listener is
 * removed when the timer fires: the supervisor's signal lives as long as it
 * does, and a crash-looping service sleeps on it every few seconds.
 */
export const sleep = (ms: number, signal?: AbortSignal) =>
  new Promise<void>((resolve) => {
    const done = () => {
      clearTimeout(timer);
      signal?.removeEventListener("abort", done);
      resolve();
    };
    const timer = setTimeout(done, ms);
    signal?.addEventListener("abort", done, { once: true });
  });

const alive = (pid: number) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

/** Signals the child's whole process group (it is spawned as a leader). */
const signalGroup = (pid: number, signal: NodeJS.Signals) => {
  try {
    process.kill(-pid, signal);
  } catch {
    try {
      process.kill(pid, signal);
    } catch {
      // Already gone.
    }
  }
};

export const probe = async (url: string, timeoutMs: number) => {
  try {
    const response = await fetch(url, {
      signal: AbortSignal.timeout(timeoutMs),
    });
    const body = await response.text();
    return { ok: response.ok, status: response.status, body };
  } catch (error) {
    return { ok: false, status: 0, body: String(error) };
  }
};

export type SupervisorPaths = {
  readonly runDir: string;
  readonly pidDir: string;
  readonly events: string;
  readonly serviceLog: (name: string) => string;
};

const markerFor = (paths: SupervisorPaths, name: string) =>
  `${paths.runDir}#${name}`;

/**
 * Stops children a previous supervisor left running (it was killed before it
 * could stop them). A child is recognised by its marker variable, never by
 * PID alone, so a recycled PID is never signalled.
 */
export const sweepOrphans = async (
  paths: SupervisorPaths,
  policy: SupervisorPolicy,
  record: (event: Record<string, unknown>) => void,
) => {
  if (!existsSync(paths.pidDir)) return;
  for (const file of readdirSync(paths.pidDir)) {
    if (!file.endsWith(".json")) continue;
    const name = file.slice(0, -".json".length);
    const path = join(paths.pidDir, file);
    const { pid } = JSON.parse(readFileSync(path, "utf8")) as { pid: number };
    let environ = "";
    try {
      environ = readFileSync(`/proc/${pid}/environ`, "utf8");
    } catch {
      // Gone, or not ours.
    }
    if (
      environ
        .split("\0")
        .includes(`${SERVICE_MARKER_ENV}=${markerFor(paths, name)}`)
    ) {
      record({ event: "orphan-stop", service: name, pid });
      signalGroup(pid, "SIGTERM");
      const deadline = Date.now() + policy.stopGraceMs;
      while (alive(pid) && Date.now() < deadline) await sleep(500);
      if (alive(pid)) signalGroup(pid, "SIGKILL");
    }
    unlinkSync(path);
  }
};

/** Appends one decision of the supervisor to its event log, as a JSON line. */
const eventRecorder =
  (paths: SupervisorPaths) => (event: Record<string, unknown>) =>
    appendFileSync(
      paths.events,
      `${JSON.stringify({ at: new Date().toISOString(), ...event })}\n`,
    );

/**
 * Keeps every service running until `signal` aborts: restarts a service that
 * exits (with backoff) or stops answering its liveness URL, and records each
 * decision as one JSON line.
 */
export const superviseServices = async (
  services: readonly ServiceSpec[],
  paths: SupervisorPaths,
  signal: AbortSignal,
  policy: SupervisorPolicy = DEFAULT_POLICY,
) => {
  mkdirSync(paths.pidDir, { recursive: true, mode: 0o700 });
  const record = eventRecorder(paths);
  await sweepOrphans(paths, policy, record);
  record({
    event: "supervisor-start",
    pid: process.pid,
    services: services.map((s) => s.name),
  });

  const running = new Map<string, ChildProcess>();
  const stop = async (name: string, child: ChildProcess) => {
    if (child.exitCode !== null || child.signalCode !== null) return;
    const exited = new Promise<void>((resolve) =>
      child.once("exit", () => resolve()),
    );
    signalGroup(child.pid!, "SIGTERM");
    const timer = setTimeout(() => {
      record({ event: "kill", service: name, pid: child.pid });
      signalGroup(child.pid!, "SIGKILL");
    }, policy.stopGraceMs);
    await exited;
    clearTimeout(timer);
  };

  const keepAlive = async (service: ServiceSpec) => {
    let backoff = policy.initialBackoffMs;
    while (!signal.aborted) {
      if (service.prestart !== undefined) {
        let ready = false;
        try {
          ready = await service.prestart();
        } catch (error) {
          record({
            event: "prestart-error",
            service: service.name,
            error: String(error),
          });
        }
        if (!ready) {
          await sleep(policy.prestartRetryMs, signal);
          continue;
        }
      }
      if (signal.aborted) break;
      const log = openSync(paths.serviceLog(service.name), "a", 0o600);
      const child = spawn(service.command, [...service.args], {
        cwd: service.cwd,
        env: {
          PATH: process.env.PATH,
          HOME: process.env.HOME,
          ...service.env,
          [SERVICE_MARKER_ENV]: markerFor(paths, service.name),
        },
        stdio: ["ignore", log, log],
        detached: true,
      });
      closeSync(log);
      const startedAt = Date.now();
      const exited = new Promise<{
        code: number | null;
        signal: string | null;
      }>((resolve) =>
        child.once("exit", (code, exitSignal) =>
          resolve({ code, signal: exitSignal }),
        ),
      );
      child.once("error", (error) =>
        record({
          event: "spawn-error",
          service: service.name,
          error: String(error),
        }),
      );
      running.set(service.name, child);
      if (child.pid !== undefined)
        writeDurableJson(join(paths.pidDir, `${service.name}.json`), {
          pid: child.pid,
          startedAt: new Date(startedAt).toISOString(),
        });
      record({ event: "start", service: service.name, pid: child.pid });

      // Liveness: after the first answer, or once the start grace is spent,
      // a continuous failure longer than hangMs means the process is stuck.
      const watchdog = new AbortController();
      const watching = (async () => {
        if (service.healthUrl === undefined) return;
        let answered = false;
        let failingSince: number | undefined;
        while (!watchdog.signal.aborted) {
          await sleep(policy.probeIntervalMs, watchdog.signal);
          if (watchdog.signal.aborted) return;
          const result = await probe(service.healthUrl, policy.probeTimeoutMs);
          const now = Date.now();
          if (result.ok) {
            answered = true;
            failingSince = undefined;
            continue;
          }
          if (
            !answered &&
            now - startedAt < (service.startGraceMs ?? policy.startGraceMs)
          )
            continue;
          failingSince ??= now;
          if (now - failingSince >= policy.hangMs) {
            record({
              event: "hung",
              service: service.name,
              pid: child.pid,
              lastProbe: result.body.slice(0, 200),
            });
            await stop(service.name, child);
            return;
          }
        }
      })();

      let onAbort!: () => void;
      const aborted = new Promise<"abort">((resolve) => {
        onAbort = () => resolve("abort");
        signal.addEventListener("abort", onAbort, { once: true });
      });
      const outcome = await Promise.race([exited, aborted]);
      signal.removeEventListener("abort", onAbort);
      if (outcome === "abort") {
        await stop(service.name, child);
      }
      watchdog.abort();
      await watching;
      const result = await exited;
      running.delete(service.name);
      const uptimeMs = Date.now() - startedAt;
      record({
        event: "exit",
        service: service.name,
        pid: child.pid,
        ...result,
        uptimeMs,
      });
      if (signal.aborted) break;
      backoff = uptimeMs >= policy.stableMs ? policy.initialBackoffMs : backoff;
      record({
        event: "restart-scheduled",
        service: service.name,
        delayMs: backoff,
      });
      await sleep(backoff, signal);
      backoff = Math.min(backoff * 2, policy.maxBackoffMs);
    }
    const pidFile = join(paths.pidDir, `${service.name}.json`);
    if (existsSync(pidFile)) unlinkSync(pidFile);
  };

  await Promise.all(services.map((service) => keepAlive(service)));
  record({ event: "supervisor-stop", pid: process.pid });
};

/**
 * Work the supervisor runs in its own process, for its whole life, beside its
 * services: not a child process, so it has no command, liveness URL or
 * restart policy of its own.
 */
export type InProcessMaintainer = {
  readonly name: string;
  /** Runs until `signal` aborts; may reject, which only restarts it. */
  readonly run: (signal: AbortSignal) => Promise<void>;
};

/** How long a maintainer that failed waits before it runs again. */
export const MAINTAINER_RESTART_MS = 60_000;

/** Resolves once `signal` aborts, leaving no listener behind if it already has. */
const abortedOf = (signal: AbortSignal) =>
  new Promise<void>((resolve) => {
    if (signal.aborted) return resolve();
    signal.addEventListener("abort", () => resolve(), { once: true });
  });

/**
 * Runs `maintainer` until `signal` aborts and never rejects. A failure before
 * the abort (an L1 read that timed out, a run record it could not read) is
 * logged when it differs from the last one and the maintainer runs again
 * after `restartMs`; whatever it throws once aborted (its sleeps reject with
 * "stopped") is the shutdown, not a fault. It returns as soon as `signal`
 * aborts, even while the maintainer is inside work that does not watch the
 * signal.
 */
export const keepMaintaining = async (
  maintainer: InProcessMaintainer,
  signal: AbortSignal,
  options: {
    readonly restartMs: number;
    readonly record: (event: Record<string, unknown>) => void;
  },
): Promise<void> => {
  const aborted = abortedOf(signal);
  let last: string | undefined;
  while (!signal.aborted) {
    const attempt = Promise.resolve()
      .then(() => maintainer.run(signal))
      .then(
        () => "returned before the supervisor stopped",
        (error: unknown) =>
          error instanceof Error ? error.message : String(error),
      );
    // `aborted` settles first on the abort (its listener is the oldest), so
    // a message is a failure from before it.
    const message = await Promise.race([attempt, aborted]);
    if (message === undefined) break;
    if (message !== last)
      try {
        options.record({
          event: "maintainer-error",
          maintainer: maintainer.name,
          error: message,
          restartMs: options.restartMs,
        });
      } catch {
        // The event log is evidence; a write that fails never stops the work.
      }
    last = message;
    await sleep(options.restartMs, signal);
  }
};

/**
 * superviseServices with in-process maintainers beside it. A maintainer can
 * never take the services down: each runs under keepMaintaining, which never
 * rejects, so only the services decide how this ends. When they end (on
 * `signal`, or a fault of the supervisor itself) every maintainer is stopped
 * and this returns or rethrows at once, so a maintainer never keeps a
 * supervisor whose services are gone.
 */
export const superviseWithMaintainers = async (
  services: readonly ServiceSpec[],
  paths: SupervisorPaths,
  signal: AbortSignal,
  maintainers: readonly InProcessMaintainer[],
  options: {
    readonly policy?: SupervisorPolicy;
    readonly restartMs?: number;
  } = {},
): Promise<void> => {
  const stop = new AbortController();
  const forward = () => stop.abort();
  if (signal.aborted) stop.abort();
  else signal.addEventListener("abort", forward, { once: true });
  const record = eventRecorder(paths);
  const kept = maintainers.map((maintainer) =>
    keepMaintaining(maintainer, stop.signal, {
      restartMs: options.restartMs ?? MAINTAINER_RESTART_MS,
      record,
    }),
  );
  try {
    await superviseServices(services, paths, signal, options.policy);
  } finally {
    signal.removeEventListener("abort", forward);
    stop.abort();
    await Promise.all(kept);
  }
};
