import { spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
import { mkdir } from "node:fs/promises";
import { dirname } from "node:path";

import {
  type ChildProcessCleanupResult,
  shouldSpawnDetachedProcessGroup,
  terminateChildProcessGroup,
} from "./process-cleanup.js";
import {
  type OwnedProcessGroupSpec,
  removeOwnedProcessGroupRecord,
  writeOwnedProcessGroupRecord,
} from "./process-ownership.js";

export type LoggedChildProcessResult = {
  readonly pid: number | null;
  readonly startedAt: string;
  readonly finishedAt: string;
  readonly durationMs: number;
  readonly exitCode: number | null;
  readonly signal: NodeJS.Signals | null;
  readonly timedOut: boolean;
  readonly cleanup: ChildProcessCleanupResult | null;
  readonly outputTermination: OutputTerminationObservation | null;
  readonly stdout: string;
  readonly combinedOutput: string;
  readonly error: Error | null;
};

export type OutputTerminationSpec = {
  readonly marker: string;
  readonly signal: NodeJS.Signals;
};

export type OutputTerminationObservation = {
  readonly marker: string;
  readonly signal: NodeJS.Signals;
  readonly at: string;
};

export type LoggedChildProcessInput = {
  readonly command: string;
  readonly args?: readonly string[];
  readonly cwd: string;
  readonly env: NodeJS.ProcessEnv;
  readonly rawLogPath: string;
  readonly timeoutMs?: number;
  readonly terminateOnOutput?: OutputTerminationSpec;
  readonly ownership?: OwnedProcessGroupSpec;
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
  terminateOnOutput,
  ownership,
}: LoggedChildProcessInput): Promise<LoggedChildProcessResult> => {
  const startedAt = startedAtDate.toISOString();
  const monotonicStartedAt = process.hrtime.bigint();
  const initialElapsedMs = Math.max(0, Date.now() - startedAtDate.getTime());
  const coherentNow = (): {
    readonly at: string;
    readonly durationMs: number;
  } => {
    const monotonicElapsedMs = Number(
      (process.hrtime.bigint() - monotonicStartedAt) / 1_000_000n,
    );
    const durationMs = initialElapsedMs + monotonicElapsedMs;
    return {
      at: new Date(startedAtDate.getTime() + durationMs).toISOString(),
      durationMs,
    };
  };
  await mkdir(dirname(rawLogPath), { recursive: true });
  const log = createWriteStream(rawLogPath, { flags: "a" });
  let pid: number | null = null;
  let stdout = "";
  let combinedOutput = "";
  let timedOut = false;
  let cleanup: ChildProcessCleanupResult | null = null;
  let outputTermination: OutputTerminationObservation | null = null;
  let outputScanTail = "";
  if (terminateOnOutput?.marker.length === 0) {
    throw new Error("terminateOnOutput.marker must not be empty");
  }

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
      // Derive the persisted wall-clock tuple from monotonic elapsed time so a
      // host clock correction cannot make an observation precede its attempt.
      const finished = coherentNow();
      const result: LoggedChildProcessResult = {
        pid,
        startedAt,
        finishedAt: finished.at,
        durationMs: finished.durationMs,
        exitCode,
        signal,
        timedOut,
        cleanup,
        outputTermination,
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
    if (ownership !== undefined) {
      if (pid === null || !shouldSpawnDetachedProcessGroup()) {
        child.kill("SIGKILL");
        settle(
          null,
          null,
          new Error(
            "owned process-group supervision requires a detached child pid on this platform",
          ),
        );
        return;
      }
      try {
        writeOwnedProcessGroupRecord({
          spec: ownership,
          pid,
          command,
          args,
          cwd,
        });
      } catch (error) {
        try {
          process.kill(-pid, "SIGKILL");
        } catch {
          child.kill("SIGKILL");
        }
        settle(
          null,
          null,
          error instanceof Error ? error : new Error(String(error)),
        );
        return;
      }
    }
    log.write(JSON.stringify(startEvent({ pid, startedAt })) + "\n");

    const timeout =
      timeoutMs === undefined
        ? undefined
        : setTimeout(
            () => {
              timedOut = true;
              cleanup = terminateChildProcessGroup({
                pid,
                signal: "SIGTERM",
                ownership,
              });
              log.write(
                JSON.stringify(
                  cleanupEvent({
                    cleanup,
                    at: coherentNow().at,
                  }),
                ) + "\n",
              );
            },
            Math.max(1, timeoutMs),
          );

    const appendOutput = (text: string): void => {
      combinedOutput += text;
      log.write(text);
      if (terminateOnOutput === undefined || outputTermination !== null) {
        return;
      }
      const { marker, signal } = terminateOnOutput;
      const searchable = outputScanTail + text;
      if (searchable.includes(marker)) {
        outputTermination = { marker, signal, at: coherentNow().at };
        cleanup = terminateChildProcessGroup({ pid, signal, ownership });
        log.write(
          JSON.stringify(cleanupEvent({ cleanup, at: outputTermination.at })) +
            "\n",
        );
        return;
      }
      // Keep enough to find a marker split across two chunks.
      outputScanTail = searchable.slice(
        Math.max(0, searchable.length - (marker.length - 1)),
      );
    };

    child.stdout.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      stdout += text;
      appendOutput(text);
    });
    child.stderr.on("data", (chunk: Buffer) => {
      const text = chunk.toString("utf8");
      appendOutput(text);
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
      if (ownership !== undefined) {
        removeOwnedProcessGroupRecord(ownership.recordPath);
      }
      settle(exitCode, signal, null);
    });
  });
};
