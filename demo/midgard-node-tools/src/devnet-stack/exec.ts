import { spawn } from "node:child_process";
import { createWriteStream, mkdirSync } from "node:fs";
import { join } from "node:path";

export type ExecResult = {
  readonly code: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly stdout: string;
  readonly stderr: string;
  readonly log: string;
};

export type ExecOptions = {
  readonly cwd?: string;
  readonly env?: NodeJS.ProcessEnv;
  readonly timeoutMs?: number;
  /** Grace between the timeout's SIGTERM and SIGKILL. */
  readonly killGraceMs?: number;
  /** Directory for the full transcript; one file per invocation. */
  readonly logDir: string;
  readonly label: string;
  readonly input?: string;
};

let sequence = 0;

const KILL_GRACE_MS = 30_000;

/**
 * Runs one command to completion, keeping the full transcript on disk. The
 * environment is exactly `env` (plus PATH/HOME): nothing from the calling
 * shell leaks into a deployment command. Past `timeoutMs` the child gets
 * SIGTERM, then SIGKILL after `killGraceMs`; the result settles even when a
 * grandchild still holds its output open.
 */
export const execLogged = (
  command: string,
  args: readonly string[],
  options: ExecOptions,
): Promise<ExecResult> => {
  mkdirSync(options.logDir, { recursive: true, mode: 0o700 });
  const stamp = new Date().toISOString().replace(/[:.]/gu, "-");
  const log = join(
    options.logDir,
    `${stamp}-${(sequence += 1).toString().padStart(3, "0")}-${options.label}.log`,
  );
  const transcript = createWriteStream(log, { mode: 0o600 });
  transcript.write(`$ ${command} ${args.join(" ")}\n`);
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      cwd: options.cwd,
      env: {
        PATH: process.env.PATH,
        HOME: process.env.HOME,
        ...options.env,
      },
      stdio: ["pipe", "pipe", "pipe"],
    });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk: Buffer) => {
      stdout += chunk.toString();
      transcript.write(chunk);
    });
    child.stderr.on("data", (chunk: Buffer) => {
      stderr += chunk.toString();
      transcript.write(chunk);
    });
    child.stdin.end(options.input ?? "");
    let timedOut = false;
    let killTimer: NodeJS.Timeout | undefined;
    // The child is not a group leader, so only it is signalled: a grandchild
    // it leaves behind may keep the pipes, and so `close`, open.
    const timer =
      options.timeoutMs === undefined
        ? undefined
        : setTimeout(() => {
            timedOut = true;
            child.kill("SIGTERM");
            killTimer = setTimeout(() => {
              child.kill("SIGKILL");
              child.stdout.destroy();
              child.stderr.destroy();
            }, options.killGraceMs ?? KILL_GRACE_MS);
          }, options.timeoutMs);
    const clearTimers = () => {
      clearTimeout(timer);
      clearTimeout(killTimer);
    };
    child.once("error", (error) => {
      clearTimers();
      transcript.end();
      reject(error);
    });
    child.once("close", (code, signal) => {
      clearTimers();
      const timeout = timedOut
        ? ` timed out after ${options.timeoutMs! / 1000} s`
        : "";
      // Settle once the exit line is on disk, so a caller reading the
      // transcript sees the whole of it.
      transcript.end(`\n[exit code=${code} signal=${signal}${timeout}]\n`, () =>
        resolve({ code, signal, stdout, stderr, log }),
      );
    });
  });
};

export const requireSuccess = (
  result: ExecResult,
  what: string,
): ExecResult => {
  if (result.code === 0) return result;
  const tail = `${result.stdout}\n${result.stderr}`
    .trim()
    .split("\n")
    .slice(-25)
    .join("\n");
  throw new Error(
    `${what} failed (exit ${result.code ?? result.signal}); transcript ${result.log}\n${tail}`,
  );
};

/**
 * The node CLI mixes log lines into stdout even with --json. The machine
 * result is the last top-level JSON value, which always starts at column 0.
 */
export const lastJsonValue = (stdout: string): unknown => {
  const lines = stdout.split("\n");
  for (let start = lines.length - 1; start >= 0; start -= 1) {
    const first = lines[start]!;
    if (!first.startsWith("{") && !first.startsWith("[")) continue;
    for (let end = lines.length; end > start; end -= 1) {
      const last = lines[end - 1]!.trimEnd();
      if (!last.endsWith("}") && !last.endsWith("]")) continue;
      try {
        return JSON.parse(lines.slice(start, end).join("\n"));
      } catch {
        // Not this span; keep narrowing, then scan upwards.
      }
    }
  }
  throw new Error("command printed no JSON result");
};
