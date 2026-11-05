import { spawn } from "node:child_process";
import { closeSync, existsSync, openSync, readFileSync } from "node:fs";

import { onlyAwaitingActivation } from "../full-stack/readiness.js";
import { type DeployContext, nodeCli } from "./deploy.js";
import { codeStamp, runtimeDistTargets } from "./dist-freshness.js";
import { readJsonIfPresent, writeDurableFile } from "./durable.js";
import { lastJsonValue, requireSuccess } from "./exec.js";
import type { Journal } from "./journal.js";
import type { Layout } from "./layout.js";
import { lockOwner } from "./lock.js";
import type { HubOracleOneShot } from "./node-env.js";
import {
  hasReadinessProbe,
  probeServiceReadiness,
} from "./service-readiness.js";
import { type ServiceRefusal, serviceRefusal } from "./service-refusal.js";
import { serviceSpecs, specsDigest } from "./services.js";
import { DEFAULT_POLICY, probe, type ServiceSpec } from "./supervisor.js";

// Each service gets its own stop grace; they stop in parallel.
export const SUPERVISOR_STOP_MS = DEFAULT_POLICY.stopGraceMs + 30_000;

const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

const alive = (pid: number) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

/** The hub-oracle nonce the run state records, once it is complete. */
export const recordedOneShot = (
  layout: Layout,
): HubOracleOneShot | undefined => {
  const state = readJsonIfPresent<{
    identity?: { hubOracleOneShot?: HubOracleOneShot };
    steps?: { hubOracleNonce?: { status?: string } };
  }>(layout.deploymentRunState);
  return state?.steps?.hubOracleNonce?.status === "complete"
    ? state.identity?.hubOracleOneShot
    : undefined;
};

/** The live supervisor's PID, or undefined when none holds the run. */
export const runningSupervisor = (layout: Layout): number | undefined =>
  lockOwner(layout.supervisorPid);

/**
 * Whether the running supervisor was started with exactly `specs` on the code
 * stamped `code`. It fixes its service set when it starts, and its services
 * keep the code they loaded, so one started by an older controller, before a
 * change to any service's command or environment, or before a rebuild of the
 * dists, never runs what this controller would.
 */
export const supervisorRuns = (
  layout: Layout,
  specs: readonly ServiceSpec[],
  code: string,
) =>
  existsSync(layout.supervisorSpecs) &&
  readFileSync(layout.supervisorSpecs, "utf8") === specsDigest(specs, code);

/**
 * Records, for a supervisor about to start `specs`, what supervisorRuns
 * compares: the service set and the stamp of the dists on disk, the code its
 * services will load.
 */
export const recordSupervisorSpecs = (
  layout: Layout,
  specs: readonly ServiceSpec[],
) =>
  writeDurableFile(
    layout.supervisorSpecs,
    specsDigest(specs, codeStamp(runtimeDistTargets(layout))),
  );

export type SupervisorControl = {
  readonly running: () => number | undefined;
  /** Whether the running one runs this service set on this code (supervisorRuns). */
  readonly runs: () => boolean;
  readonly stop: () => Promise<unknown>;
  /** Starts a new supervisor (and does what must precede it); resolves to its PID. */
  readonly start: () => Promise<number>;
};

/**
 * The supervisor of this service set on this code: the running one when it
 * is, otherwise a new one, after stopping a running one that is not.
 */
export const ensureSupervisor = async (
  control: SupervisorControl,
): Promise<number> => {
  const running = control.running();
  if (running !== undefined && control.runs()) {
    console.log(`services: supervisor ${running} already running`);
    return running;
  }
  if (running !== undefined) {
    // Only a restart adopts another service set or rebuilt code.
    console.log(
      `services: supervisor ${running} runs another service set or older code; restarting it`,
    );
    await control.stop();
  }
  const started = await control.start();
  console.log(`services: started supervisor ${started}`);
  return started;
};

/** Starts `supervise` detached from this controller; it outlives `up`. */
export const startSupervisor = (layout: Layout): number => {
  const log = openSync(layout.serviceLog("supervisor"), "a", 0o600);
  const child = spawn(
    process.execPath,
    // This controller's own entry script, not a bundled chunk's URL.
    [process.argv[1]!, "supervise", "--run-dir", layout.runDir],
    { detached: true, stdio: ["ignore", log, log], env: process.env },
  );
  closeSync(log);
  child.unref();
  if (child.pid === undefined) throw new Error("the supervisor did not start");
  return child.pid;
};

export type BondStatus = {
  belowBond: boolean;
  backing: string;
  requiredBacking: string;
};

export const bondStatus = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
) =>
  lastJsonValue(
    requireSuccess(
      await nodeCli(
        context,
        [
          "--l1",
          "kupmios",
          "da-bond",
          "status",
          "--manifest",
          context.layout.contractManifest,
        ],
        "da-bond-status",
        oneShot,
      ),
      "da-bond status",
    ).stdout,
  ) as BondStatus;

/** The committee's DA bond pool on L1, and a top-up paid by the committee. */
export type BondPool = {
  readonly status: () => Promise<BondStatus>;
  readonly topUp: (lovelace: bigint) => Promise<void>;
};

/** The run's pool, topped up from the cosigner: a committee member's wallet
 * that no running service spends, and not one of the operator's wallets. */
export const devnetBondPool = (
  context: DeployContext,
  oneShot: HubOracleOneShot,
): BondPool => ({
  status: () => bondStatus(context, oneShot),
  topUp: async (lovelace) => {
    requireSuccess(
      await nodeCli(
        context,
        [
          "--l1",
          "kupmios",
          "da-bond",
          "top-up",
          "--manifest",
          context.layout.contractManifest,
          "--amount",
          String(lovelace),
          "--wallet-seed-env",
          "DA_COSIGNER_SEED_PHRASE",
        ],
        "da-bond-top-up",
        oneShot,
      ),
      "da-bond top-up",
    );
  },
});

const BOND_POOL_BACKED = "da-bond-pool-backed";

/**
 * Backs the committee's DA bond pool once, at the deployment's first start.
 * A pool below its bond after that was slashed, and topping it up is a DA
 * operator's decision (`da-bond top-up`), so later starts only return its
 * status for the caller to report.
 */
export const backBondPoolOnce = async (pool: BondPool, journal: Journal) => {
  const status = await pool.status();
  if (journal.get(BOND_POOL_BACKED) !== undefined) return status;
  if (!status.belowBond) {
    journal.set(BOND_POOL_BACKED, { backing: status.backing });
    return status;
  }
  await pool.topUp(BigInt(status.requiredBacking) - BigInt(status.backing));
  const after = await pool.status();
  if (after.belowBond)
    throw new Error(
      "the DA bond pool is still below its bond after its first backing",
    );
  journal.set(BOND_POOL_BACKED, { backing: after.backing });
  return after;
};

export type ServiceReport = {
  readonly name: string;
  readonly pid?: number;
  readonly startedAt?: string;
  readonly alive: boolean;
  readonly live?: boolean;
  readonly ready?: boolean;
  readonly reasons?: unknown;
  readonly refusal?: ServiceRefusal;
};

/**
 * The node and committee answer `{ ready, reasons }`; the watcher's status
 * answers `{ readiness: "ready" | "not_ready", readinessReasons }`.
 */
const readyBody = (body: string): { ready?: boolean; reasons?: unknown } => {
  try {
    const parsed = JSON.parse(body) as {
      ready?: boolean;
      reasons?: unknown;
      readiness?: string;
      readinessReasons?: unknown;
    };
    return parsed.readiness === undefined
      ? parsed
      : {
          ready: parsed.readiness === "ready",
          reasons: parsed.readinessReasons,
        };
  } catch {
    return {};
  }
};

export const serviceReport = async (
  layout: Layout,
  service: ServiceSpec,
): Promise<ServiceReport> => {
  const pidFile = readJsonIfPresent<{ pid: number; startedAt?: string }>(
    `${layout.state}/services/${service.name}.json`,
  );
  const pid = pidFile?.pid;
  const report: ServiceReport = {
    name: service.name,
    pid,
    startedAt: pidFile?.startedAt,
    alive: pid !== undefined && alive(pid),
  };
  const refusal = serviceRefusal(
    {
      runDir: layout.runDir,
      pidDir: `${layout.state}/services`,
      events: layout.supervisorEvents,
      serviceLog: layout.serviceLog,
    },
    service.name,
  );
  if (refusal !== undefined)
    return {
      ...report,
      live: false,
      ready: false,
      reasons: ["configuration_or_deployment_refused"],
      refusal,
    };
  const live =
    service.healthUrl === undefined
      ? undefined
      : (await probe(service.healthUrl, 5_000)).ok;
  if (!hasReadinessProbe(service)) return { ...report, live };
  const ready = await probeServiceReadiness(service, 10_000);
  const body = readyBody(ready.body);
  return {
    ...report,
    live,
    // `up` activates an operator awaiting activation once the services are up.
    ready:
      (ready.ok && body.ready !== false) ||
      onlyAwaitingActivation(body.reasons),
    ...(ready.ok && body.ready !== false
      ? {}
      : { reasons: body.reasons ?? ready.body.slice(0, 300) }),
  };
};

export const serviceReports = (
  layout: Layout,
  services: readonly ServiceSpec[],
): Promise<readonly ServiceReport[]> =>
  Promise.all(services.map((service) => serviceReport(layout, service)));

/**
 * Whether `report` is a running service still inside the start grace its spec
 * grants (a long startup catch-up), which the supervisor too counts healthy.
 */
export const insideStartGrace = (
  spec: ServiceSpec,
  report: ServiceReport,
  now: number,
) =>
  spec.startGraceMs !== undefined &&
  report.alive &&
  report.startedAt !== undefined &&
  now - Date.parse(report.startedAt) < spec.startGraceMs;

/**
 * Waits until every service with a readiness URL is ready and every other
 * one is running. Fails fast if the supervisor exits. With `honourStartGrace`,
 * a pending service still inside its start grace extends the wait, by at most
 * the longest grace; `onGrace` hears each time it does.
 */
export const waitForServices = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
  timeoutMs: number,
  options: {
    readonly honourStartGrace?: boolean;
    readonly supervisorPid?: number;
    readonly onGrace?: () => void;
  } = {},
): Promise<readonly ServiceReport[]> => {
  const services = serviceSpecs(context, oneShot);
  const deadline = Date.now() + timeoutMs;
  const graceDeadline =
    deadline + Math.max(0, ...services.map((spec) => spec.startGraceMs ?? 0));
  // A supervisor started a moment ago has not written its PID file yet; the
  // PID it was started with is the one that must stay alive.
  const supervisorPid =
    options.supervisorPid ?? runningSupervisor(context.layout);
  let lastPrinted = 0;
  for (;;) {
    if (supervisorPid === undefined || !alive(supervisorPid))
      throw new Error(
        `the supervisor is not running; see ${context.layout.serviceLog("supervisor")}`,
      );
    const reports = await serviceReports(context.layout, services);
    const pending = reports.filter((r) => !r.alive || r.ready === false);
    if (pending.length === 0) return reports;
    const now = Date.now();
    const graced =
      options.honourStartGrace === true &&
      now < graceDeadline &&
      pending.every((r) =>
        insideStartGrace(services.find((s) => s.name === r.name)!, r, now),
      );
    if (now >= deadline && !graced)
      throw new Error(
        `services not ready after ${timeoutMs / 1000} s: ${JSON.stringify(pending)}`,
      );
    if (now >= deadline) options.onGrace?.();
    if (Date.now() - lastPrinted > 60_000) {
      lastPrinted = Date.now();
      console.log(
        `services: waiting on ${pending.map((r) => `${r.name}${r.reasons === undefined ? "" : ` ${JSON.stringify(r.reasons).slice(0, 200)}`}`).join("; ")}`,
      );
    }
    await sleep(5_000);
  }
};

/** Stops the supervisor, which stops every service it runs. */
export const stopSupervisor = async (layout: Layout, timeoutMs: number) => {
  const pid = runningSupervisor(layout);
  if (pid === undefined) return false;
  process.kill(pid, "SIGTERM");
  const deadline = Date.now() + timeoutMs;
  while (alive(pid)) {
    if (Date.now() >= deadline)
      throw new Error(
        `supervisor ${pid} did not stop within ${timeoutMs / 1000} s`,
      );
    await sleep(500);
  }
  return true;
};
