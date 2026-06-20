import { spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import { redactArg, redactEnvKeys } from "@/e2e/runner.js";
import {
  type HttpProbeSample,
  inspectPidFile,
  type PidFileObservation,
  probeHttpEndpoint,
} from "@/e2e/service-supervisor.js";

export type StartServiceOptions = {
  readonly service: string;
  readonly command: string;
  readonly args: readonly string[];
  readonly cwd: string;
  readonly rawLogPath: string;
  readonly pidFilePath: string;
  readonly readyUrl: string;
  readonly healthUrl?: string;
  readonly readyTimeoutMs: number;
  readonly pollIntervalMs: number;
};

export type StartServiceSummary = {
  readonly schemaVersion: "midgard-e2e-managed-service-v1";
  readonly service: string;
  readonly pid: number;
  readonly rawLogPath: string;
  readonly pidFile: PidFileObservation;
  readonly ready: HttpProbeSample;
  readonly health?: HttpProbeSample;
  readonly command: {
    readonly command: string;
    readonly args: readonly string[];
    readonly cwd: string;
    readonly envKeys: readonly string[];
  };
};

const sleep = (milliseconds: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, milliseconds));

const processAlive = (pid: number): boolean => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

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
  const log = createWriteStream(options.rawLogPath, { flags: "a" });
  const child = spawn(options.command, [...options.args], {
    cwd: options.cwd,
    env: process.env,
    shell: false,
    detached: true,
    stdio: ["ignore", "pipe", "pipe"],
  });
  const pid = child.pid;
  if (pid === undefined) {
    throw new Error(`Failed to start ${options.service}: child pid missing`);
  }
  child.unref();
  child.stdout.pipe(log, { end: false });
  child.stderr.pipe(log, { end: false });
  log.write(
    JSON.stringify({
      event: "e2e_managed_service_started",
      service: options.service,
      pid,
      at: new Date().toISOString(),
      command: {
        command: options.command,
        args: options.args.map(redactArg),
        cwd: options.cwd,
        envKeys: redactEnvKeys(process.env),
      },
    }) + "\n",
  );
  await writeFile(options.pidFilePath, `${pid.toString()}\n`, "utf8");

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
  return {
    schemaVersion: "midgard-e2e-managed-service-v1",
    service: options.service,
    pid,
    rawLogPath: options.rawLogPath,
    pidFile: await inspectPidFile({
      path: options.pidFilePath,
      runnerOwnedPids: new Set([pid]),
    }),
    ready,
    ...(health === undefined ? {} : { health }),
    command: {
      command: options.command,
      args: options.args.map(redactArg),
      cwd: options.cwd,
      envKeys: redactEnvKeys(process.env),
    },
  };
};
