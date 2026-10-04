import { spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
import { setTimeout as delay } from "node:timers/promises";
import { registerOwnedLaunch } from "./resources.mjs";
import { pinnedPnpmEnvironment } from "./pnpm.mjs";

// Each invocation owns a process group; the close event joins stdio as well
// as the direct child. Grandchildren cannot keep a cancelled run alive.
export const runProcess = async ({
  argv,
  cwd,
  env = process.env,
  logPath,
  signal,
  timeoutMs = 1_800_000,
  maxBytes = 16 * 1024 * 1024,
  echo = false,
}) => {
  if (
    !Array.isArray(argv) ||
    argv.length === 0 ||
    argv.some((part) => typeof part !== "string" || part.includes("\0"))
  )
    throw new Error("command must be a nonempty argv array");
  signal?.throwIfAborted();
  const startedAt = new Date().toISOString();
  const started = performance.now();
  const log = createWriteStream(logPath, { flags: "wx", mode: 0o600 });
  let logError;
  log.on("error", (error) => {
    logError = error;
  });
  const launch = registerOwnedLaunch(env);
  const child = spawn(argv[0], argv.slice(1), {
    cwd,
    env: pinnedPnpmEnvironment(env),
    detached: true,
    stdio: ["ignore", "pipe", "pipe"],
  });
  let registrationError;
  try {
    if (child.pid) launch.attach(child.pid);
  } catch (error) {
    registrationError = error.message;
  }
  let bytes = 0;
  let reason;
  let escalation;
  const kill = (kind) => {
    try {
      if (child.pid) process.kill(-child.pid, kind);
    } catch (error) {
      if (error.code !== "ESRCH") throw error;
    }
  };
  const stop = (why) => {
    if (reason) return;
    reason = why;
    kill("SIGTERM");
    escalation = setTimeout(() => kill("SIGKILL"), 1500);
  };
  const collect = (chunk) => {
    const remaining = Math.max(0, maxBytes - bytes);
    bytes += chunk.length;
    if (remaining > 0) {
      const kept = chunk.subarray(0, remaining);
      log.write(kept);
      if (echo) process.stderr.write(kept);
    }
    if (bytes > maxBytes) stop(`output exceeded ${maxBytes} bytes`);
  };
  child.stdout.on("data", collect);
  child.stderr.on("data", collect);
  const abort = () => stop("cancelled");
  signal?.addEventListener("abort", abort, { once: true });
  if (registrationError) stop(registrationError);
  const timeout = setTimeout(
    () => stop(`deadline exceeded ${timeoutMs} ms`),
    timeoutMs,
  );
  let spawnError;
  const result = await new Promise((done) => {
    child.on("error", (error) => {
      spawnError = error.message;
    });
    child.on("exit", () => {
      // Also close any inherited pipes held by descendants after parent exit.
      if (child.pid) kill("SIGTERM");
      escalation ??= setTimeout(() => kill("SIGKILL"), 1500);
    });
    child.on("close", (exitCode, exitSignal) =>
      done({ exitCode, signal: exitSignal }),
    );
  });
  clearTimeout(timeout);
  // Ensure surviving group members have received SIGKILL before releasing
  // their build/database lease. Usually there are none and ESRCH is immediate.
  kill("SIGKILL");
  clearTimeout(escalation);
  signal?.removeEventListener("abort", abort);
  launch.joined();
  log.end();
  if (!log.closed)
    await new Promise((done) => {
      log.once("close", done);
    });
  await delay(0);
  return {
    argv,
    cwd,
    startedAt,
    endedAt: new Date().toISOString(),
    durationMs: performance.now() - started,
    ...result,
    bytes,
    reason: reason ?? spawnError ?? logError?.message,
    logPath,
  };
};
