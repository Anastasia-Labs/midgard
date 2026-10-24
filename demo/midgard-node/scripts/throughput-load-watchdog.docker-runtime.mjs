import { spawnSync } from "node:child_process";
import { closeSync, mkdirSync, openSync, writeSync } from "node:fs";
import { dirname, resolve } from "node:path";

import {
  canonicalWatchdogEvidenceRecordV1,
  DOCKER_COMMAND_TIMEOUT_MS,
  MAX_EVIDENCE_STRING_CHARS,
  WATCHDOG_SCHEMA_VERSION,
} from "./throughput-load-watchdog.canonical-watchdog-evidence-record-v1.mjs";

const runProcess = (command, args, { env, timeoutMs } = {}) => {
  const result = spawnSync(command, args, {
    encoding: "utf8",
    env,
    maxBuffer: 1024 * 1024,
    timeout: timeoutMs,
  });
  return {
    status: result.status,
    signal: result.signal,
    stdout: result.stdout ?? "",
    stderr: result.stderr ?? "",
    error: result.error,
  };
};

const requireSuccessfulProcess = (result, description) => {
  if (result.status !== 0 || result.error !== undefined) {
    const detail = (result.stderr || result.error?.message || "").trim();
    throw new Error(
      `${description} failed${detail.length === 0 ? "" : `: ${detail}`}`,
    );
  }
  return result.stdout.trim();
};

export const dockerRuntime = ({ probeCommand, probeTimeoutMs }) => ({
  now: () => new Date(),
  inspect: (container) => {
    const output = requireSuccessfulProcess(
      runProcess("docker", ["inspect", container], {
        timeoutMs: DOCKER_COMMAND_TIMEOUT_MS,
      }),
      `docker inspect ${container}`,
    );
    const parsed = JSON.parse(output);
    if (!Array.isArray(parsed) || parsed.length !== 1) {
      throw new Error(`docker inspect ${container} returned no unique target`);
    }
    const [inspection] = parsed;
    return {
      id: inspection.Id,
      name: String(inspection.Name ?? "").replace(/^\//u, ""),
      status: inspection.State?.Status,
      running: inspection.State?.Running === true,
      exitCode: inspection.State?.ExitCode,
      labels: inspection.Config?.Labels ?? {},
    };
  },
  start: (containerId) => {
    requireSuccessfulProcess(
      runProcess("docker", ["start", containerId], {
        timeoutMs: DOCKER_COMMAND_TIMEOUT_MS,
      }),
      `docker start ${containerId}`,
    );
  },
  stop: (containerId, timeoutSeconds) => {
    requireSuccessfulProcess(
      runProcess(
        "docker",
        ["stop", "--time", timeoutSeconds.toString(), containerId],
        { timeoutMs: (timeoutSeconds + 10) * 1_000 },
      ),
      `docker stop ${containerId}`,
    );
  },
  kill: (containerId) => {
    const killed = runProcess("docker", ["kill", containerId], {
      timeoutMs: DOCKER_COMMAND_TIMEOUT_MS,
    });
    if (killed.status === 0 && killed.error === undefined) return;
    const inspected = runProcess("docker", ["inspect", containerId], {
      timeoutMs: DOCKER_COMMAND_TIMEOUT_MS,
    });
    if (inspected.status === 0 && inspected.error === undefined) {
      const parsed = JSON.parse(inspected.stdout);
      if (Array.isArray(parsed) && parsed[0]?.State?.Running === false) return;
    }
    requireSuccessfulProcess(killed, `docker kill ${containerId}`);
  },
  probe: (phase, target) => {
    const [command, ...args] = probeCommand;
    const result = runProcess(command, args, {
      env: {
        ...process.env,
        WATCHDOG_PHASE: phase,
        WATCHDOG_CONTAINER_ID: target.id,
        WATCHDOG_CONTAINER_NAME: target.name,
      },
      timeoutMs: probeTimeoutMs,
    });
    return {
      status: result.status,
      signal: result.signal,
      stdout: result.stdout.trim(),
      stderr: result.stderr.trim(),
      error: result.error?.message,
    };
  },
  sleep: (milliseconds, signal) =>
    new Promise((resolveSleep, rejectSleep) => {
      const complete = () => {
        signal?.removeEventListener("abort", abort);
        resolveSleep();
      };
      const timeout = setTimeout(complete, milliseconds);
      const abort = () => {
        clearTimeout(timeout);
        signal?.removeEventListener("abort", abort);
        rejectSleep(signal.reason ?? new Error("watchdog interrupted"));
      };
      if (signal?.aborted === true) {
        abort();
        return;
      }
      signal?.addEventListener("abort", abort, { once: true });
    }),
});

export const createEvidenceWriter = (path) => {
  const absolutePath = resolve(path);
  mkdirSync(dirname(absolutePath), { recursive: true });
  const descriptor = openSync(absolutePath, "wx", 0o600);
  let sequence = 0;
  return {
    path: absolutePath,
    record: (event) => {
      if (
        event === null ||
        typeof event !== "object" ||
        Array.isArray(event) ||
        Object.hasOwn(event, "schemaVersion") ||
        Object.hasOwn(event, "sequence")
      ) {
        throw new Error(
          "watchdog evidence event must not override its V1 identity",
        );
      }
      const nextSequence = sequence + 1;
      const canonical = canonicalWatchdogEvidenceRecordV1(
        {
          ...event,
          schemaVersion: WATCHDOG_SCHEMA_VERSION,
          sequence: nextSequence,
        },
        nextSequence,
      );
      writeSync(descriptor, `${JSON.stringify(canonical)}\n`);
      sequence = nextSequence;
    },
    close: () => closeSync(descriptor),
  };
};

export const PROBE_TRUNCATION_SUFFIX = "<truncated>";

const boundedProbeOutput = (value) =>
  typeof value === "string" && value.length > MAX_EVIDENCE_STRING_CHARS
    ? `${value.slice(
        0,
        MAX_EVIDENCE_STRING_CHARS - PROBE_TRUNCATION_SUFFIX.length,
      )}${PROBE_TRUNCATION_SUFFIX}`
    : value;

export const probeEvent = (probe) => ({
  probeStatus: probe.status ?? null,
  probeSignal: probe.signal ?? null,
  probeStdout: boundedProbeOutput(probe.stdout) ?? null,
  probeStderr: boundedProbeOutput(probe.stderr) ?? null,
  probeError: probe.error ?? null,
});
