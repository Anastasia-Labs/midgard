import { parseWatchdogArgs } from "./throughput-load-watchdog.canonical-watchdog-evidence-record-v1.mjs";
import {
  createEvidenceWriter,
  dockerRuntime,
  probeEvent,
} from "./throughput-load-watchdog.docker-runtime.mjs";

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

export const main = async () => {
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
