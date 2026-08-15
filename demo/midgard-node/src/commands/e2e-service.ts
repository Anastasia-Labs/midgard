import { spawn } from "node:child_process";
import { closeSync, openSync, writeSync } from "node:fs";
import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import {
  buildE2EProcessEnv,
  type E2EEnvInheritance,
  type E2EEnvProvenance,
} from "@/e2e/env.js";
import {
  exactRecord,
  nonEmptyString,
  positiveInteger,
} from "@/e2e/exact-artifact.js";
import {
  parseRedactedCommandV1,
  redactArg,
  type RedactedCommand,
  redactEnvKeys,
} from "@/e2e/runner.js";
import {
  type HttpProbeSample,
  inspectPidFile,
  parseHttpProbeSampleV1,
  parsePidFileObservationV1,
  type PidFileObservation,
  probeHttpEndpoint,
} from "@/e2e/service-supervisor.js";
import { sleep } from "@/sleep.js";

export const E2E_MANAGED_SERVICE_SCHEMA_VERSION =
  "midgard-e2e-managed-service-v1";

export type StartServiceOptions = {
  readonly service: string;
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
  readonly env?: Readonly<Record<string, string | undefined>>;
  readonly envFiles?: readonly string[];
  readonly envInheritance?: E2EEnvInheritance;
  readonly rawLogPath: string;
  readonly pidFilePath: string;
  readonly readyUrl: string;
  readonly healthUrl?: string;
  readonly readyTimeoutMs: number;
  readonly pollIntervalMs: number;
};

export type StartServiceSummary = {
  readonly schemaVersion: typeof E2E_MANAGED_SERVICE_SCHEMA_VERSION;
  readonly service: string;
  readonly pid: number;
  readonly rawLogPath: string;
  readonly pidFile: PidFileObservation;
  readonly ready: HttpProbeSample;
  readonly health?: HttpProbeSample;
  readonly command: RedactedCommand;
};

export const parseManagedServiceSummaryV1 = (
  value: unknown,
): StartServiceSummary => {
  const label = "managed service summary";
  const input = exactRecord(
    value,
    label,
    [
      "schemaVersion",
      "service",
      "pid",
      "rawLogPath",
      "pidFile",
      "ready",
      "command",
    ],
    ["health"],
  );
  if (input.schemaVersion !== E2E_MANAGED_SERVICE_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_MANAGED_SERVICE_SCHEMA_VERSION}`,
    );
  }
  const parsed: StartServiceSummary = {
    schemaVersion: E2E_MANAGED_SERVICE_SCHEMA_VERSION,
    service: nonEmptyString(input.service, `${label}.service`),
    pid: positiveInteger(input.pid, `${label}.pid`),
    rawLogPath: nonEmptyString(input.rawLogPath, `${label}.rawLogPath`),
    pidFile: parsePidFileObservationV1(input.pidFile, `${label}.pidFile`),
    ready: parseHttpProbeSampleV1(input.ready, `${label}.ready`),
    ...(input.health === undefined
      ? {}
      : {
          health: parseHttpProbeSampleV1(input.health, `${label}.health`),
        }),
    command: parseRedactedCommandV1(input.command, `${label}.command`),
  };
  if (
    parsed.pidFile.status !== "runner_owned" ||
    parsed.pidFile.pid !== parsed.pid ||
    parsed.ready.status !== "healthy"
  ) {
    throw new Error(
      `${label} pid ownership or readiness evidence is inconsistent`,
    );
  }
  return parsed;
};

const processAlive = (pid: number): boolean => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

const redactedServiceCommand = (
  options: Pick<StartServiceOptions, "command" | "args" | "cwd">,
  provenance: E2EEnvProvenance,
): StartServiceSummary["command"] => ({
  command: options.command,
  args: options.args.map(redactArg),
  cwd: options.cwd,
  envKeys: provenance.explicitEnvKeys,
  envFiles: provenance.envFiles,
  envInheritance: provenance.inheritance,
});

export const startManagedService = async (
  options: StartServiceOptions,
): Promise<StartServiceSummary> => {
  const priorPid = await inspectPidFile({ path: options.pidFilePath });
  if (priorPid.status === "foreign" || priorPid.status === "runner_owned") {
    throw new Error(
      `Refusing to start ${options.service}; pid file ${options.pidFilePath} points at live pid ${priorPid.pid?.toString()}`,
    );
  }
  await mkdir(dirname(options.rawLogPath), { recursive: true });
  await mkdir(dirname(options.pidFilePath), { recursive: true });
  const { env, provenance } = await buildE2EProcessEnv({
    cwd: options.cwd,
    envFiles: options.envFiles,
    overrides: options.env,
    inherit: options.envInheritance,
  });
  const command = redactedServiceCommand(options, provenance);
  const logFd = openSync(options.rawLogPath, "a");
  const child = spawn(options.command, [...options.args], {
    cwd: options.cwd,
    env,
    shell: false,
    detached: true,
    stdio: ["ignore", logFd, logFd],
  });
  const pid = child.pid;
  if (pid === undefined) {
    closeSync(logFd);
    throw new Error(`Failed to start ${options.service}: child pid missing`);
  }
  child.unref();
  writeSync(
    logFd,
    JSON.stringify({
      event: "e2e_managed_service_started",
      service: options.service,
      pid,
      at: new Date().toISOString(),
      command: {
        ...command,
        inheritedEnvKeyCount:
          provenance.inheritance === "process"
            ? redactEnvKeys(process.env).length
            : 0,
      },
    }) + "\n",
  );
  await writeFile(options.pidFilePath, `${pid.toString()}\n`, "utf8");

  try {
    const deadline = Date.now() + options.readyTimeoutMs;
    let ready = await probeHttpEndpoint({
      label: `${options.service}:ready`,
      url: options.readyUrl,
    });
    while (ready.status !== "healthy" && Date.now() < deadline) {
      if (!processAlive(pid)) {
        throw new Error(
          `${options.service} exited before readiness; inspect ${options.rawLogPath}`,
        );
      }
      await sleep(options.pollIntervalMs);
      ready = await probeHttpEndpoint({
        label: `${options.service}:ready`,
        url: options.readyUrl,
      });
    }
    if (ready.status !== "healthy") {
      throw new Error(
        `${options.service} did not become ready before timeout; status=${ready.status}; inspect ${options.rawLogPath}`,
      );
    }
    const health =
      options.healthUrl === undefined
        ? undefined
        : await probeHttpEndpoint({
            label: `${options.service}:health`,
            url: options.healthUrl,
          });
    return parseManagedServiceSummaryV1({
      schemaVersion: E2E_MANAGED_SERVICE_SCHEMA_VERSION,
      service: options.service,
      pid,
      rawLogPath: options.rawLogPath,
      pidFile: await inspectPidFile({
        path: options.pidFilePath,
        runnerOwnedPids: new Set([pid]),
      }),
      ready,
      ...(health === undefined ? {} : { health }),
      command,
    });
  } finally {
    closeSync(logFd);
  }
};
