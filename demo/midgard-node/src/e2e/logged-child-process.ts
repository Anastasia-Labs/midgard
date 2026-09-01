import { spawn } from "node:child_process";
import { createWriteStream, existsSync } from "node:fs";
import { mkdir } from "node:fs/promises";
import { dirname } from "node:path";

import {
  type ChildProcessCleanupResult,
  shouldSpawnDetachedProcessGroup,
  terminateChildProcessGroup,
} from "@/e2e/process-cleanup.js";
import {
  type OwnedProcessGroupSpec,
  removeOwnedProcessGroupRecord,
  writeOwnedProcessGroupRecord,
} from "@/e2e/process-ownership.js";

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
  readonly fileTermination: FileTerminationObservation | null;
  readonly stdout: string;
  readonly combinedOutput: string;
  readonly error: Error | null;
};

export type OutputTerminationSpec = {
  readonly marker: string;
  readonly additionalMarkers?: readonly string[];
  readonly occurrence?: number;
  readonly signal: NodeJS.Signals;
};

export type OutputTerminationObservation = {
  readonly marker: string;
  readonly occurrence: number;
  readonly signal: NodeJS.Signals;
  readonly at: string;
};

export type FileTerminationSpec = {
  readonly path: string;
  readonly signal: NodeJS.Signals;
  readonly pollIntervalMs?: number;
};

export type FileTerminationObservation = {
  readonly path: string;
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
  readonly terminateOnFile?: FileTerminationSpec;
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
  terminateOnFile,
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
  let fileTermination: FileTerminationObservation | null = null;
  let outputScanTail = "";
  const outputMarkerOccurrences = new Map<string, number>();

  const outputTerminationMarkers =
    terminateOnOutput === undefined
      ? []
      : [
          terminateOnOutput.marker,
          ...(terminateOnOutput.additionalMarkers ?? []),
        ];
  if (outputTerminationMarkers.some((marker) => marker.length === 0)) {
    throw new Error("terminateOnOutput markers must not be empty");
  }
  const terminateOnOccurrence = terminateOnOutput?.occurrence ?? 1;
  if (
    !Number.isSafeInteger(terminateOnOccurrence) ||
    terminateOnOccurrence <= 0
  ) {
    throw new Error("terminateOnOutput.occurrence must be a positive integer");
  }

  return await new Promise<LoggedChildProcessResult>((resolve) => {
    let settled = false;
    let filePoll: NodeJS.Timeout | undefined;
    const settle = (
      exitCode: number | null,
      signal: NodeJS.Signals | null,
      error: Error | null,
    ): void => {
      if (settled) {
        return;
      }
      settled = true;
      if (filePoll !== undefined) clearInterval(filePoll);
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
        fileTermination,
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

    if (terminateOnFile !== undefined) {
      const pollIntervalMs = Math.max(
        10,
        Math.floor(terminateOnFile.pollIntervalMs ?? 50),
      );
      filePoll = setInterval(() => {
        if (fileTermination !== null || !existsSync(terminateOnFile.path)) {
          return;
        }
        fileTermination = {
          path: terminateOnFile.path,
          signal: terminateOnFile.signal,
          at: coherentNow().at,
        };
        cleanup = terminateChildProcessGroup({
          pid,
          signal: terminateOnFile.signal,
          ownership,
        });
        log.write(
          JSON.stringify(cleanupEvent({ cleanup, at: fileTermination.at })) +
            "\n",
        );
      }, pollIntervalMs);
    }

    const appendOutput = (text: string): void => {
      combinedOutput += text;
      log.write(text);
      if (terminateOnOutput === undefined || outputTermination !== null) {
        return;
      }
      const searchable = outputScanTail + text;
      const matchedMarker = outputTerminationMarkers.find((marker) => {
        let found = 0;
        let fromIndex = 0;
        while (true) {
          const index = searchable.indexOf(marker, fromIndex);
          if (index < 0) break;
          found += 1;
          fromIndex = index + marker.length;
        }
        const observed = (outputMarkerOccurrences.get(marker) ?? 0) + found;
        outputMarkerOccurrences.set(marker, observed);
        return observed >= terminateOnOccurrence;
      });
      if (matchedMarker !== undefined) {
        outputTermination = {
          marker: matchedMarker,
          occurrence: outputMarkerOccurrences.get(matchedMarker) ?? 1,
          signal: terminateOnOutput.signal,
          at: coherentNow().at,
        };
        cleanup = terminateChildProcessGroup({
          pid,
          signal: terminateOnOutput.signal,
          ownership,
        });
        log.write(
          JSON.stringify(cleanupEvent({ cleanup, at: outputTermination.at })) +
            "\n",
        );
        return;
      }
      const retainedCharacters = Math.max(
        0,
        Math.max(...outputTerminationMarkers.map((marker) => marker.length)) -
          1,
      );
      outputScanTail = searchable.slice(-retainedCharacters);
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
