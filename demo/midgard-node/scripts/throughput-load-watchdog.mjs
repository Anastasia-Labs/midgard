#!/usr/bin/env node

import { closeSync, mkdirSync, openSync, writeSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { pathToFileURL } from "node:url";

export const WATCHDOG_SCHEMA_VERSION = "midgard-throughput-watchdog-v1";
export const DEFAULT_REQUIRED_LABEL = "midgard.benchmark.load=true";
const DOCKER_COMMAND_TIMEOUT_MS = 15_000;
const MAX_EVIDENCE_LINE_BYTES = 64 * 1024;
const MAX_EVIDENCE_STRING_CHARS = 4_096;
const WATCHDOG_EVENT_FIELDS = Object.freeze({
  target_verified: [],
  preflight_probe: [
    "probeStatus",
    "probeSignal",
    "probeStdout",
    "probeStderr",
    "probeError",
  ],
  preflight_failed: [],
  start_started: [],
  start_finished: [],
  sample_probe: [
    "probeStatus",
    "probeSignal",
    "probeStdout",
    "probeStderr",
    "probeError",
  ],
  completed: ["exitCode"],
  load_failed: ["exitCode"],
  stop_started: ["reason", "stopTimeoutSeconds"],
  stop_failed: ["reason", "error"],
  stop_verification_failed: ["reason", "error"],
  kill_started: ["reason"],
  kill_failed: ["reason", "error"],
  kill_finished: ["reason", "running", "exitCode"],
  kill_verification_failed: ["reason", "error"],
  stop_finished: ["reason", "stopMode", "running", "exitCode"],
  watchdog_error: ["error"],
});

const boundedString = (value, fieldName, { nullable = false } = {}) => {
  if (nullable && value === null) return null;
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.length > MAX_EVIDENCE_STRING_CHARS
  ) {
    throw new Error(
      `${fieldName} must be a nonempty string of at most ${MAX_EVIDENCE_STRING_CHARS.toString()} characters`,
    );
  }
  return value;
};

const canonicalTimestamp = (value) => {
  if (value === null) return null;
  boundedString(value, "watchdog evidence at");
  const timestamp = new Date(value);
  if (
    !Number.isFinite(timestamp.getTime()) ||
    timestamp.toISOString() !== value
  ) {
    throw new Error("watchdog evidence at must be canonical ISO-8601");
  }
  return value;
};

const exactInteger = (value, fieldName, minimum = Number.MIN_SAFE_INTEGER) => {
  if (!Number.isSafeInteger(value) || value < minimum) {
    throw new Error(`${fieldName} is outside its canonical integer bound`);
  }
  return value;
};

const canonicalProbeField = (fieldName, value) => {
  if (fieldName === "probeStatus") {
    return value === null
      ? null
      : exactInteger(value, "watchdog evidence probeStatus");
  }
  if (
    value !== null &&
    (typeof value !== "string" || value.length > MAX_EVIDENCE_STRING_CHARS)
  ) {
    throw new Error(
      `watchdog evidence ${fieldName} must be null or at most ${MAX_EVIDENCE_STRING_CHARS.toString()} characters`,
    );
  }
  return value;
};

const canonicalWatchdogEvidenceRecordV1 = (record, expectedSequence) => {
  if (record === null || typeof record !== "object" || Array.isArray(record)) {
    throw new Error("watchdog evidence record must be an object");
  }
  if (record.schemaVersion !== WATCHDOG_SCHEMA_VERSION) {
    throw new Error("watchdog evidence schemaVersion must be exact V1");
  }
  const sequence = exactInteger(
    record.sequence,
    "watchdog evidence sequence",
    1,
  );
  if (
    expectedSequence !== undefined &&
    sequence !== exactInteger(expectedSequence, "expected sequence", 1)
  ) {
    throw new Error("watchdog evidence sequence is not contiguous");
  }
  const event = boundedString(record.event, "watchdog evidence event");
  const eventFields = WATCHDOG_EVENT_FIELDS[event];
  if (eventFields === undefined) {
    throw new Error(`unknown watchdog evidence event ${event}`);
  }
  const expectedFields = [
    "schemaVersion",
    "sequence",
    "at",
    "event",
    "containerId",
    "containerName",
    ...eventFields,
  ];
  const actualFields = Object.keys(record);
  if (
    actualFields.length !== expectedFields.length ||
    actualFields.some((field) => !expectedFields.includes(field))
  ) {
    throw new Error(
      `watchdog evidence ${event} fields must be exact: ${expectedFields.join(",")}`,
    );
  }

  const canonical = {
    schemaVersion: WATCHDOG_SCHEMA_VERSION,
    sequence,
    at: canonicalTimestamp(record.at),
    event,
    containerId: boundedString(
      record.containerId,
      "watchdog evidence containerId",
    ),
    containerName: boundedString(
      record.containerName,
      "watchdog evidence containerName",
    ),
  };
  for (const fieldName of eventFields) {
    const value = record[fieldName];
    if (fieldName.startsWith("probe")) {
      canonical[fieldName] = canonicalProbeField(fieldName, value);
    } else if (fieldName === "exitCode" || fieldName === "stopTimeoutSeconds") {
      canonical[fieldName] = exactInteger(
        value,
        `watchdog evidence ${fieldName}`,
        fieldName === "stopTimeoutSeconds" ? 0 : Number.MIN_SAFE_INTEGER,
      );
    } else if (fieldName === "running") {
      if (typeof value !== "boolean") {
        throw new Error("watchdog evidence running must be boolean");
      }
      canonical[fieldName] = value;
    } else if (fieldName === "stopMode") {
      if (value !== "graceful" && value !== "kill") {
        throw new Error("watchdog evidence stopMode must be graceful or kill");
      }
      canonical[fieldName] = value;
    } else {
      canonical[fieldName] = boundedString(
        value,
        `watchdog evidence ${fieldName}`,
      );
    }
  }
  return Object.freeze(canonical);
};

export const parseThroughputWatchdogEvidenceLineV1 = (
  line,
  expectedSequence,
) => {
  if (
    typeof line !== "string" ||
    line.length === 0 ||
    Buffer.byteLength(line, "utf8") > MAX_EVIDENCE_LINE_BYTES ||
    line.includes("\n") ||
    line.includes("\r")
  ) {
    throw new Error("watchdog evidence line is empty, oversized, or multiline");
  }
  let parsed;
  try {
    parsed = JSON.parse(line);
  } catch {
    throw new Error("watchdog evidence line must be valid JSON");
  }
  const canonical = canonicalWatchdogEvidenceRecordV1(parsed, expectedSequence);
  if (JSON.stringify(canonical) !== line) {
    throw new Error("watchdog evidence line is not canonical JSON");
  }
  return canonical;
};

const requireInteger = (value, name, minimum) => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < minimum) {
    throw new Error(`${name} must be an integer >= ${minimum.toString()}`);
  }
  return parsed;
};

const requireValue = (args, index, name) => {
  const value = args[index + 1];
  if (value === undefined || value.startsWith("--")) {
    throw new Error(`${name} requires a value`);
  }
  return value;
};

export const parseWatchdogArgs = (argv) => {
  const separator = argv.indexOf("--");
  if (separator < 0 || separator === argv.length - 1) {
    throw new Error("watchdog requires a probe command after --");
  }
  const optionArgs = argv.slice(0, separator);
  const probeCommand = argv.slice(separator + 1);
  const options = {
    requiredLabel: DEFAULT_REQUIRED_LABEL,
    intervalMs: 5_000,
    stopTimeoutSeconds: 5,
    probeTimeoutMs: 15_000,
  };

  for (let index = 0; index < optionArgs.length; index += 1) {
    const argument = optionArgs[index];
    if (argument === "--container") {
      options.container = requireValue(optionArgs, index, argument);
      index += 1;
    } else if (argument === "--evidence") {
      options.evidencePath = requireValue(optionArgs, index, argument);
      index += 1;
    } else if (argument === "--required-label") {
      options.requiredLabel = requireValue(optionArgs, index, argument);
      index += 1;
    } else if (argument === "--interval-ms") {
      options.intervalMs = requireInteger(
        requireValue(optionArgs, index, argument),
        argument,
        1,
      );
      index += 1;
    } else if (argument === "--stop-timeout-seconds") {
      options.stopTimeoutSeconds = requireInteger(
        requireValue(optionArgs, index, argument),
        argument,
        0,
      );
      index += 1;
    } else if (argument === "--probe-timeout-ms") {
      options.probeTimeoutMs = requireInteger(
        requireValue(optionArgs, index, argument),
        argument,
        1,
      );
      index += 1;
    } else {
      throw new Error(`unknown watchdog option: ${argument}`);
    }
  }

  if (typeof options.container !== "string" || options.container.length === 0) {
    throw new Error("--container is required");
  }
  if (
    typeof options.evidencePath !== "string" ||
    options.evidencePath.length === 0
  ) {
    throw new Error("--evidence is required");
  }
  const labelSeparator = options.requiredLabel.indexOf("=");
  if (labelSeparator <= 0) {
    throw new Error("--required-label must use key=value syntax");
  }
  options.requiredLabelKey = options.requiredLabel.slice(0, labelSeparator);
  options.requiredLabelValue = options.requiredLabel.slice(labelSeparator + 1);
  options.probeCommand = probeCommand;
  return options;
};

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

const dockerRuntime = ({ probeCommand, probeTimeoutMs }) => ({
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
      sequence += 1;
      const canonical = canonicalWatchdogEvidenceRecordV1(
        {
          ...event,
          schemaVersion: WATCHDOG_SCHEMA_VERSION,
          sequence,
        },
        sequence,
      );
      writeSync(descriptor, `${JSON.stringify(canonical)}\n`);
    },
    close: () => closeSync(descriptor),
  };
};

const PROBE_TRUNCATION_SUFFIX = "<truncated>";
const boundedProbeOutput = (value) =>
  typeof value === "string" && value.length > MAX_EVIDENCE_STRING_CHARS
    ? `${value.slice(
        0,
        MAX_EVIDENCE_STRING_CHARS - PROBE_TRUNCATION_SUFFIX.length,
      )}${PROBE_TRUNCATION_SUFFIX}`
    : value;

const probeEvent = (probe) => ({
  probeStatus: probe.status ?? null,
  probeSignal: probe.signal ?? null,
  probeStdout: boundedProbeOutput(probe.stdout) ?? null,
  probeStderr: boundedProbeOutput(probe.stderr) ?? null,
  probeError: probe.error ?? null,
});

export const runThroughputLoadWatchdog = async ({
  container,
  requiredLabelKey,
  requiredLabelValue,
  intervalMs,
  stopTimeoutSeconds,
  runtime,
  record,
  signal,
}) => {
  const at = () => runtime.now().toISOString();
  const target = await runtime.inspect(container);
  if (target.status !== "created" || target.running) {
    throw new Error(
      `watchdog target must be a stopped, newly created container; ${target.name} is ${String(target.status)}`,
    );
  }
  if (target.labels?.[requiredLabelKey] !== requiredLabelValue) {
    throw new Error(
      `watchdog target ${target.name} is missing required label ${requiredLabelKey}=${requiredLabelValue}`,
    );
  }
  const targetIdentity = { containerId: target.id, containerName: target.name };
  record({ at: at(), event: "target_verified", ...targetIdentity });

  const preflight = await runtime.probe("preflight", target);
  record({
    at: at(),
    event: "preflight_probe",
    ...targetIdentity,
    ...probeEvent(preflight),
  });
  if (preflight.status !== 0) {
    record({ at: at(), event: "preflight_failed", ...targetIdentity });
    return { status: "preflight_failed", target };
  }

  const beforeStart = await runtime.inspect(target.id);
  if (
    beforeStart.id !== target.id ||
    beforeStart.status !== "created" ||
    beforeStart.running ||
    beforeStart.labels?.[requiredLabelKey] !== requiredLabelValue
  ) {
    throw new Error(
      `watchdog target ${target.id} changed state or identity during preflight`,
    );
  }

  let startAttempted = false;
  let stopAttempted = false;
  const safeAt = () => {
    try {
      return at();
    } catch {
      return null;
    }
  };
  const safeRecord = (event) => {
    try {
      record(event);
      return true;
    } catch {
      return false;
    }
  };
  const stopTarget = async (reason) => {
    if (stopAttempted) return;
    stopAttempted = true;
    safeRecord({
      at: safeAt(),
      event: "stop_started",
      reason,
      ...targetIdentity,
      stopTimeoutSeconds,
    });
    let stopMode = "graceful";
    try {
      await runtime.stop(target.id, stopTimeoutSeconds);
    } catch (error) {
      stopMode = "kill";
      safeRecord({
        at: safeAt(),
        event: "stop_failed",
        reason,
        ...targetIdentity,
        error: error instanceof Error ? error.message : String(error),
      });
    }
    let stopped;
    let terminationConfirmed = false;
    try {
      stopped = await runtime.inspect(target.id);
      terminationConfirmed = !stopped.running;
      if (stopped.running) {
        stopMode = "kill";
      }
    } catch (error) {
      stopMode = "kill";
      safeRecord({
        at: safeAt(),
        event: "stop_verification_failed",
        reason,
        ...targetIdentity,
        error: error instanceof Error ? error.message : String(error),
      });
    }
    if (stopMode === "kill") {
      safeRecord({
        at: safeAt(),
        event: "kill_started",
        reason,
        ...targetIdentity,
      });
      try {
        await runtime.kill(target.id);
      } catch (error) {
        safeRecord({
          at: safeAt(),
          event: "kill_failed",
          reason,
          ...targetIdentity,
          error: error instanceof Error ? error.message : String(error),
        });
        throw error;
      }
      try {
        stopped = await runtime.inspect(target.id);
        terminationConfirmed = !stopped.running;
        safeRecord({
          at: safeAt(),
          event: "kill_finished",
          reason,
          ...targetIdentity,
          running: stopped.running,
          exitCode: stopped.exitCode,
        });
      } catch (error) {
        safeRecord({
          at: safeAt(),
          event: "kill_verification_failed",
          reason,
          ...targetIdentity,
          error: error instanceof Error ? error.message : String(error),
        });
      }
    }
    if (stopped !== undefined) {
      safeRecord({
        at: safeAt(),
        event: "stop_finished",
        reason,
        ...targetIdentity,
        stopMode,
        running: stopped.running,
        exitCode: stopped.exitCode,
      });
    }
    if (stopped?.running === true) {
      throw new Error(
        `watchdog target ${target.id} remained running after stop`,
      );
    }
    if (!terminationConfirmed) {
      throw new Error(
        `watchdog could not verify termination of target ${target.id}`,
      );
    }
  };

  try {
    if (signal?.aborted === true) {
      throw signal.reason ?? new Error("watchdog interrupted before start");
    }
    record({ at: at(), event: "start_started", ...targetIdentity });
    startAttempted = true;
    await runtime.start(target.id);
    record({ at: at(), event: "start_finished", ...targetIdentity });

    while (true) {
      if (signal?.aborted === true) {
        throw signal.reason ?? new Error("watchdog interrupted");
      }
      const current = await runtime.inspect(target.id);
      if (!current.running) {
        const status = current.exitCode === 0 ? "completed" : "load_failed";
        record({
          at: at(),
          event: status,
          ...targetIdentity,
          exitCode: current.exitCode,
        });
        return { status, target, exitCode: current.exitCode };
      }
      const probe = await runtime.probe("sample", target);
      record({
        at: at(),
        event: "sample_probe",
        ...targetIdentity,
        ...probeEvent(probe),
      });
      if (probe.status !== 0) {
        await stopTarget(`probe_exit_${String(probe.status ?? "unknown")}`);
        return { status: "tripped", target, probe };
      }
      await runtime.sleep(intervalMs, signal);
    }
  } catch (error) {
    safeRecord({
      at: safeAt(),
      event: "watchdog_error",
      ...targetIdentity,
      error: error instanceof Error ? error.message : String(error),
    });
    if (startAttempted) {
      try {
        await stopTarget("watchdog_error");
      } catch (stopError) {
        throw new AggregateError(
          [error, stopError],
          `watchdog failed and could not confirm termination of ${target.id}`,
        );
      }
    }
    throw error;
  }
};

const main = async () => {
  const options = parseWatchdogArgs(process.argv.slice(2));
  const evidence = createEvidenceWriter(options.evidencePath);
  const controller = new AbortController();
  const interrupt = (signalName) =>
    controller.abort(new Error(`watchdog received ${signalName}`));
  const handleSigint = () => interrupt("SIGINT");
  const handleSigterm = () => interrupt("SIGTERM");
  process.on("SIGINT", handleSigint);
  process.on("SIGTERM", handleSigterm);
  try {
    const result = await runThroughputLoadWatchdog({
      ...options,
      runtime: dockerRuntime(options),
      record: evidence.record,
      signal: controller.signal,
    });
    process.exitCode = result.status === "completed" ? 0 : 2;
  } finally {
    process.off("SIGINT", handleSigint);
    process.off("SIGTERM", handleSigterm);
    evidence.close();
  }
};

if (import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch((error) => {
    process.stderr.write(
      `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
    );
    process.exitCode = 1;
  });
}
