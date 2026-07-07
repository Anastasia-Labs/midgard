import { spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
import { mkdir } from "node:fs/promises";
import { dirname } from "node:path";

import {
  type ChildProcessCleanupResult,
  shouldSpawnDetachedProcessGroup,
  terminateChildProcessGroup,
} from "@/e2e/process-cleanup.js";

export type LoggedChildProcessResult = {
  readonly pid: number | null;
  readonly startedAt: string;
  readonly finishedAt: string;
  readonly durationMs: number;
  readonly exitCode: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly timedOut: boolean;
  readonly cleanup: ChildProcessCleanupResult | null;
  readonly stdout: string;
  readonly combinedOutput: string;
  readonly error: Error | null;
};

export type LoggedChildProcessInput = {
  readonly command: string;
  readonly args?: readonly string[];
  readonly cwd: string;
  readonly env: NodeJS.ProcessEnv;
  readonly rawLogPath: string;
  readonly timeoutMs?: number;
  readonly startedAtDate?: Date;
  readonly startEvent: (input: {
    readonly pid: number | null;
    readonly startedAt: string;
  }) => Readonly<Record<string, unknown>>;
  readonly cleanupEvent: (input: {
    readonly cleanup: ChildProcessCleanupResult;
    readonly at: string;
  }) => Readonly<Record<string, unknown>>;
};

export const runLoggedChildProcessAttempt = async ({
  command,
  args = [],
  cwd,
  env,
  rawLogPath,
  timeoutMs,
  startedAtDate = new Date(),
  startEvent,
  cleanupEvent,
}: LoggedChildProcessInput): Promise<LoggedChildProcessResult> => {
  const startedAt = startedAtDate.toISOString();
  await mkdir(dirname(rawLogPath), { recursive: true });
  const log = createWriteStream(rawLogPath, { flags: "a" });
  let pid: number | null = null;
  let stdout = "";
  let combinedOutput = "";
  let timedOut = false;
  let cleanup: ChildProcessCleanupResult | null = null;

  return await new Promise<LoggedChildProcessResult>((resolve) => {
    let settled = false;
    const settle = (
      exitCode: number | null,
      signal: NodeJS.Signals | null,
      error: Error | null,
    ): void => {
      if (settled) {
        return;
      }
      settled = true;
      const finishedAtDate = new Date();
      const result: LoggedChildProcessResult = {
        pid,
        startedAt,
        finishedAt: finishedAtDate.toISOString(),
        durationMs: Math.max(
          0,
          finishedAtDate.getTime() - startedAtDate.getTime(),
        ),
        exitCode,
        signal,
        timedOut,
        cleanup,
        stdout,
        combinedOutput,
        error,
      };
      log.end(() => resolve(result));
    };

    const child = spawn(command, [...args], {
      cwd,
      env,
      shell: false,
      detached: shouldSpawnDetachedProcessGroup(),
      stdio: ["ignore", "pipe", "pipe"],
    });
    pid = child.pid ?? null;
    log.write(JSON.stringify(startEvent({ pid, startedAt })) + "\n");

    const timeout =
      timeoutMs === undefined
        ? undefined
        : setTimeout(
            () => {
              timedOut = true;
              cleanup = terminateChildProcessGroup({ pid, signal: "SIGTERM" });
              log.write(
                JSON.stringify(
                  cleanupEvent({
                    cleanup,
                    at: new Date().toISOString(),
                  }),
                ) + "\n",
              );
            },
            Math.max(1, timeoutMs),
          );

    child.stdout.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      stdout += text;
      combinedOutput += text;
      log.write(text);
    });
    child.stderr.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      combinedOutput += text;
      log.write(text);
    });
    child.on("error", (error) => {
      if (timeout !== undefined) {
        clearTimeout(timeout);
      }
      settle(null, null, error);
    });
    child.on("close", (exitCode, signal) => {
      if (timeout !== undefined) {
        clearTimeout(timeout);
      }
      settle(exitCode, signal, null);
    });
  });
};
