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

import {
  hasReadinessProbe,
  probe,
  probeServiceReadiness,
  type ValidatedReadinessProbe,
} from "./service-readiness.js";
export { probe } from "./service-readiness.js";
import { writeDurableJson } from "./durable.js";
import type { HistoryReadinessSpecification } from "./history-role-context.js";
import { createHistoryRoleRegistry } from "./history-role-registry.js";
import {
  recoveryScope,
  recoveryScopeMatches,
} from "./service-recovery-scope.js";
import {
  clearServiceRefusal,
  CONFIG_REFUSAL_EXIT_CODE,
  consumeServiceRecovery,
  readinessAnswered,
  type RecoveryRequest,
  refuseService,
  type ServiceRefusal,
  serviceRefusal,
} from "./service-refusal.js";
/** One long-running process the supervisor keeps alive. */
export type ServiceSpec = {
  readonly historyReadiness?: HistoryReadinessSpecification;
  readonly name: string;
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
  readonly env: Readonly<Record<string, string>>;
  readonly healthUrl?: string; // Liveness failure uses the restart policy.
  readonly readyUrl?: string; // Readiness is reported and waited on by up.
  readonly readyProbe?: ValidatedReadinessProbe;
  readonly startGraceMs?: number;
  readonly prestart?: () => Promise<boolean>; // Required before each start.
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
/** Abortable delay; each completed sleep removes its abort listener. */
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
export type SupervisorPaths = {
  readonly runtimeCodeStamp?: () => string;
  serviceSpecs?: readonly ServiceSpec[];
  readonly runDir: string;
  readonly deploymentBinding?: string;
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
export const eventRecorder =
  (paths: SupervisorPaths) => (event: Record<string, unknown>) =>
    appendFileSync(
      paths.events,
      `${JSON.stringify({ at: new Date().toISOString(), ...event })}\n`,
    );
/** Keeps services until abort; restarts transient exits/hangs with backoff.
 * Exit 78 persists a refusal until one explicit retry answers readiness.
 * Each decision is recorded as one JSON line. */
export const superviseServices = async (
  services: readonly ServiceSpec[],
  paths: SupervisorPaths,
  signal: AbortSignal,
  policy: SupervisorPolicy = DEFAULT_POLICY,
) => {
  const recoveryPaths = { ...paths, serviceSpecs: services };
  const startedScope = recoveryScope(recoveryPaths);
  const history = createHistoryRoleRegistry(recoveryPaths);
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
    let authorized: ServiceRefusal | undefined;
    let permission: RecoveryRequest | undefined;
    while (!signal.aborted) {
      const refusal = serviceRefusal(paths, service.name);
      if (
        refusal !== undefined &&
        authorized?.refusalId !== refusal.refusalId
      ) {
        const request = !hasReadinessProbe(service)
          ? undefined
          : consumeServiceRecovery(recoveryPaths, refusal);
        if (
          request === undefined ||
          !recoveryScopeMatches(startedScope, request)
        ) {
          await sleep(policy.prestartRetryMs, signal);
          continue;
        }
        authorized = refusal;
        permission = request;
        record({
          event: "refusal-retry-authorized",
          note: request.note,
          requestedAt: request.requestedAt,
          service: service.name,
          refusalId: refusal.refusalId,
        });
      }
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
      if (
        authorized !== undefined &&
        (!recoveryScopeMatches(permission, recoveryScope(recoveryPaths)) ||
          !recoveryScopeMatches(startedScope, permission))
      ) {
        record({
          event: "refusal-retry-invalidated",
          service: service.name,
          reason: "runtime_code_or_service_set_changed",
        });
        authorized = undefined;
        permission = undefined;
        continue;
      }
      const attemptScope = permission;
      authorized = undefined;
      permission = undefined;
      const log = openSync(paths.serviceLog(service.name), "a", 0o600);
      const historyAttempt = history.prepare(service);
      const child = spawn(service.command, [...service.args], {
        cwd: service.cwd,
        env: {
          PATH: process.env.PATH,
          HOME: process.env.HOME,
          ...service.env,
          ...historyAttempt?.env,
          [SERVICE_MARKER_ENV]: markerFor(paths, service.name),
        },
        stdio:
          historyAttempt === null
            ? ["ignore", log, log]
            : ["ignore", log, log, "pipe"],
        detached: true,
      });
      closeSync(log);
      const closeHistory =
        historyAttempt === null
          ? () => undefined
          : history.register(historyAttempt, child);
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
      const watchdog = new AbortController();
      const watching = (async () => {
        if (service.healthUrl === undefined && refusal === undefined) return;
        let answered = false;
        let failingSince: number | undefined;
        while (!watchdog.signal.aborted) {
          await sleep(policy.probeIntervalMs, watchdog.signal);
          if (watchdog.signal.aborted) return;
          if (refusal !== undefined && hasReadinessProbe(service)) {
            const historyProof =
              service.historyReadiness === undefined
                ? undefined
                : await history.prove(service, policy.probeTimeoutMs);
            const ready =
              service.historyReadiness === undefined
                ? await probeServiceReadiness(service, policy.probeTimeoutMs)
                : {
                    ok: historyProof !== undefined,
                    body: '{"ready":true}',
                  };
            if (!ready.ok && service.historyReadiness !== undefined)
              record({
                event: "refusal-readiness-held",
                service: service.name,
                ...history.diagnostic(),
              });
            if (
              ready.ok &&
              readinessAnswered(ready.body) &&
              child.exitCode === null &&
              child.signalCode === null &&
              recoveryScopeMatches(
                attemptScope,
                recoveryScope(recoveryPaths),
              ) &&
              recoveryScopeMatches(startedScope, attemptScope) &&
              (service.historyReadiness === undefined ||
                historyProof?.current() === true) &&
              clearServiceRefusal(paths, refusal)
            )
              record({
                event: "refusal-recovered",
                service: service.name,
                refusalId: refusal.refusalId,
              });
          }
          if (service.healthUrl === undefined) continue;
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
      closeHistory();
      running.delete(service.name);
      const uptimeMs = Date.now() - startedAt;
      record({
        event: "exit",
        service: service.name,
        pid: child.pid,
        ...result,
        uptimeMs,
      });
      if (result.code === CONFIG_REFUSAL_EXIT_CODE) {
        const refused = refuseService(paths, service.name);
        record({
          event: "service-refused",
          ...refused,
          reason: "configuration_or_deployment_refused",
        });
      }
      if (signal.aborted) break;
      if (serviceRefusal(paths, service.name) !== undefined) continue;
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
  try {
    await Promise.all(services.map((service) => keepAlive(service)));
  } finally {
    history.close();
  }
  record({ event: "supervisor-stop", pid: process.pid });
};
export {
  type InProcessMaintainer,
  keepMaintaining,
  MAINTAINER_RESTART_MS,
  superviseWithMaintainers,
} from "./service-maintenance.js";
